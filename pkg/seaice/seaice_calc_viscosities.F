C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_viscosities.F,v 1.19 2012/11/15 16:12:18 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
CStartOfInterface
      SUBROUTINE SEAICE_CALC_VISCOSITIES(
     I     e11, e22, e12, zMin, zMax, hEffM, press0,
     O     eta, etaZ, zeta, press,
     I     iStep, myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE  SEAICE_CALC_VISCOSITIES                      |
C     | o compute shear and bulk viscositites eta, zeta and the  |
C     |   corrected ice strength P                               |
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997)    |
C     |==========================================================|
C     | written by Martin Losch, Mar 2006                        |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     iStep  :: Sub-time-step number
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: My Thread Id. number
      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     strain rate tensor
      _RL e11   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
      _RL zMin  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL zMax  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL hEffM (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C
      _RL press0(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL press (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     bulk viscosity
      _RL  eta  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL  etaZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     shear viscosity
      _RL zeta  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CEndOfInterface

#if ( defined (SEAICE_CGRID) && defined (SEAICE_ALLOW_DYNAMICS) )
C     === Local variables ===
C     i,j,bi,bj - Loop counters
C     e11, e12, e22 - components of strain rate tensor
C     ecm2          - inverse of square of eccentricity of yield curve
      INTEGER i, j, bi, bj
      _RL ECM2, deltaC, deltaCreg, tmp
      _RL e12Csqr(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL e11Zsqr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL e22Zsqr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL e11e22Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pressZ (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
#ifdef SEAICE_ALLOW_TEM
      _RL etaMax, etaDen
#endif /* SEAICE_ALLOW_TEM */
      INTEGER k
      _RL zetaZ, deltaZ, deltaZreg, sumNorm
#ifdef SEAICE_ZETA_SMOOTHREG
      _RL argTmp
#endif /* SEAICE_ZETA_SMOOTHREG */
CEOP      

C--   FIRST SET UP BASIC CONSTANTS
      k=1
      ecm2=0. _d 0
      IF ( SEAICE_eccen .NE. 0. _d 0 ) ecm2=ONE/(SEAICE_eccen**2)
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     need to do this beforehand for easier vectorization after
C     TAFization
        IF ( SEAICEetaZmethod .LT. 2 ) THEN
         DO j=1-Oly+1,sNy+Oly-1
          DO i=1-Olx+1,sNx+Olx-1
           tmp = 0.25 *
     &          ( e12(I,J  ,bi,bj) + e12(I+1,J  ,bi,bj) 
     &          + e12(I,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
           e12Csqr(i,j) = tmp*tmp
          ENDDO
         ENDDO
        ELSEIF ( SEAICEetaZmethod .EQ. 2 ) THEN
         DO j=1-Oly+1,sNy+Oly-1
          DO i=1-Olx+1,sNx+Olx-1
           e12Csqr(i,j) = 0.25 * 
     &          ( e12(I,J  ,bi,bj)**2 + e12(I+1,J  ,bi,bj)**2 
     &          + e12(I,J+1,bi,bj)**2 + e12(I+1,J+1,bi,bj)**2 )
          ENDDO
         ENDDO
        ENDIF
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          deltaC = (e11(i,j,bi,bj)**2+e22(i,j,bi,bj)**2)*(ONE+ecm2)
     &         + 4. _d 0*ecm2*e12Csqr(i,j)
     &         + 2. _d 0*e11(i,j,bi,bj)*e22(i,j,bi,bj)*(ONE-ecm2)
#ifdef ALLOW_AUTODIFF_TAMC
C     avoid sqrt of 0
          IF ( deltaC .GT. 0. _d 0 ) deltaC = SQRT(deltaC)
#else
          deltaC    = SQRT(deltaC)
#endif /* ALLOW_AUTODIFF_TAMC */
          deltaCreg = MAX(deltaC,SEAICE_EPS)
C     "replacement pressure"
          zeta (I,J,bi,bj) = HALF*press0(I,J,bi,bj)/deltaCreg
C     put min and max viscosities in
#ifdef SEAICE_ZETA_SMOOTHREG
C     regularize zeta to zmax with a smooth tanh-function instead 
C     of a min(zeta,zmax). This improves convergence of iterative
C     solvers (Lemieux and Tremblay 2009, JGR). No effect on EVP
          argTmp = exp(-1. _d 0/(deltaCreg*SEAICE_zetaMaxFac))
          zeta (I,J,bi,bj) = ZMAX(I,J,bi,bj)
     &         *(1. _d 0 - argTmp)/(1. _d 0 + argTmp)
#else
          zeta (I,J,bi,bj) = MIN(ZMAX(I,J,bi,bj),zeta(I,J,bi,bj))
#endif /*  SEAICE_ZETA_SMOOTHREG */
          zeta (I,J,bi,bj) = MAX(ZMIN(I,J,bi,bj),zeta(I,J,bi,bj))
C     set viscosities to zero at hEffM flow pts
          zeta (I,J,bi,bj) = zeta(I,J,bi,bj)*HEFFM(I,J,bi,bj)
          eta  (I,J,bi,bj) = ECM2*zeta(I,J,bi,bj)
          press(I,J,bi,bj) = TWO *zeta(I,J,bi,bj)*deltaC
         ENDDO
        ENDDO
#ifdef SEAICE_ALLOW_TEM
        IF ( SEAICEuseTEM ) THEN
         DO j=1-Oly+1,sNy+Oly-1
          DO i=1-Olx+1,sNx+Olx-1
           etaDen = (e11(I,J,bi,bj)-e22(I,J,bi,bj))**2 
     &          + 4. _d 0*e12Csqr(i,j)
           etaDen = SQRT(MAX(SEAICE_EPS_SQ,etaDen))
           etaMax = ( 0.5 _d 0*press(I,J,bi,bj)-zeta(I,J,bi,bj)
     &          *( e11(I,J,bi,bj)+e22(I,J,bi,bj) ) 
     &          )/etaDen
           eta(I,J,bi,bj) = MIN(eta(I,J,bi,bj),etaMax)
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_ALLOW_TEM */
C     compute eta at Z-points
        IF ( SEAICEetaZmethod .eq. 0 ) THEN
C     This is the old and stupid way
         DO j=1-Oly+1,sNy+Oly-1
          DO i=1-Olx+1,sNx+Olx-1
           etaZ(I,J,bi,bj) = 
     &         ( eta (I,J  ,bi,bj)  + eta (I-1,J  ,bi,bj)
     &         + eta (I,J-1,bi,bj)  + eta (I-1,J-1,bi,bj) )
     &         / MAX(1.D0,maskC(I,J,  k,bi,bj)+maskC(I-1,J,  k,bi,bj)
     &         +          maskC(I,J-1,k,bi,bj)+maskC(I-1,J-1,k,bi,bj) )
          ENDDO
         ENDDO
        ELSE
         IF ( SEAICEetaZmethod .eq. 1 ) THEN
          DO j=1-Oly+1,sNy+Oly-1
           DO i=1-Olx+1,sNx+Olx-1
            sumNorm  = maskC(I,J,  k,bi,bj)+maskC(I-1,J,  k,bi,bj)
     &               + maskC(I,J-1,k,bi,bj)+maskC(I-1,J-1,k,bi,bj)
            IF ( sumNorm.GT.0. _d 0 ) sumNorm = 1. _d 0 / sumNorm
            pressZ(i,j)  = sumNorm * 
     &           ( press0(i,j,  bi,bj) + press0(i-1,j,  bi,bj)
     &           + press0(i,j-1,bi,bj) + press0(i-1,j-1,bi,bj) )
            e11Zsqr(i,j) =  ( sumNorm * 
     &           ( e11(i,j,  bi,bj) + e11(i-1,j,  bi,bj) 
     &           + e11(i,j-1,bi,bj) + e11(i-1,j-1,bi,bj) ) )**2
            e22Zsqr(i,j) = ( sumNorm *
     &           ( e22(i,j,  bi,bj) + e22(i-1,j,  bi,bj) 
     &           + e22(i,j-1,bi,bj) + e22(i-1,j-1,bi,bj) ) )**2
            e11e22Z(i,j)  = sumNorm * sumNorm *
     &           ( e11(i,  j,  bi,bj) + e11(i-1,j,  bi,bj)
     &           + e11(i,  j-1,bi,bj) + e11(i-1,j-1,bi,bj) ) *
     &           ( e22(i,  j,  bi,bj) + e22(i-1,j,  bi,bj)
     &           + e22(i  ,j-1,bi,bj) * e22(i-1,j-1,bi,bj) )
           ENDDO
          ENDDO
         ELSEIF ( SEAICEetaZmethod .eq. 2 ) THEN
          DO j=1-Oly+1,sNy+Oly-1
           DO i=1-Olx+1,sNx+Olx-1
            sumNorm  = maskC(I,J,  k,bi,bj)+maskC(I-1,J,  k,bi,bj)
     &               + maskC(I,J-1,k,bi,bj)+maskC(I-1,J-1,k,bi,bj)
            IF ( sumNorm.GT.0. _d 0 ) sumNorm = 1. _d 0 / sumNorm
            pressZ(i,j)  = sumNorm * 
     &           ( press0(i,j,  bi,bj) + press0(i-1,j,  bi,bj)
     &           + press0(i,j-1,bi,bj) + press0(i-1,j-1,bi,bj) )
            e11Zsqr(i,j) = sumNorm * 
     &           ( e11(i,j,  bi,bj)**2 + e11(i-1,j,  bi,bj)**2 
     &           + e11(i,j-1,bi,bj)**2 + e11(i-1,j-1,bi,bj)**2 )
            e22Zsqr(i,j) = sumNorm *
     &           ( e22(i,j,  bi,bj)**2 + e22(i-1,j,  bi,bj)**2 
     &           + e22(i,j-1,bi,bj)**2 + e22(i-1,j-1,bi,bj)**2 )
            e11e22Z(i,j)  = sumNorm *
     &           ( e11(i,  j,  bi,bj)*e22(i,  j,  bi,bj)
     &           + e11(i-1,j,  bi,bj)*e22(i-1,j,  bi,bj)
     &           + e11(i,  j-1,bi,bj)*e22(i  ,j-1,bi,bj)
     &           + e11(i-1,j-1,bi,bj)*e22(i-1,j-1,bi,bj) )
           ENDDO
          ENDDO
         ENDIF
         DO j=1-Oly+1,sNy+Oly-1
          DO i=1-Olx+1,sNx+Olx-1
          deltaZ  = (e11Zsqr(i,j)+e22Zsqr(i,j))*(ONE+ecm2)
     &         + 4. _d 0*ecm2*e12(i,j,bi,bj)**2
     &         + 2. _d 0*e11e22Z(i,j)*(ONE-ecm2)
#ifdef ALLOW_AUTODIFF_TAMC
C     avoid sqrt of 0
          IF ( deltaZ .GT. 0. _d 0 ) deltaZ = SQRT(deltaZ)
#else
          deltaZ    = SQRT(deltaZ)
#endif /* ALLOW_AUTODIFF_TAMC */
          deltaZreg = MAX(deltaZ,SEAICE_EPS)
C     "replacement pressure"
          zetaZ     = HALF*pressZ(i,j)/deltaZreg
C     put min and max viscosities in
#ifdef SEAICE_ZETA_SMOOTHREG
C     regularize zeta to zmax with a smooth tanh-function instead 
C     of a min(zeta,zmax). This improves convergence of iterative
C     solvers (Lemieux and Tremblay 2009, JGR). No effect on EVP
          argTmp = exp(-1. _d 0/(deltaZreg*SEAICE_zetaMaxFac))
          zetaZ  = ZMAX(I,J,bi,bj)*(1. _d 0-argTmp)/(1. _d 0+argTmp)
#else
          zetaZ  = MIN(ZMAX(I,J,bi,bj),zetaZ)
#endif /*  SEAICE_ZETA_SMOOTHREG */
          zetaZ  = MAX(ZMIN(I,J,bi,bj),zetaZ)
          etaZ(I,J,bi,bj) = ECM2*zetaZ
          ENDDO
         ENDDO
#ifdef SEAICE_ALLOW_TEM
        IF ( SEAICEuseTEM ) THEN
         STOP 'OOPS'
CML         DO j=1-Oly+1,sNy+Oly-1
CML          DO i=1-Olx+1,sNx+Olx-1
CML           etaDen = (e11Zsqr(i,j)+e22Zsqr(i,j)-2.*e11e22Z(i,j)) 
CML     &          + 4. _d 0*e12(I,J,bi,bj)**2
CML           etaDen = SQRT(MAX(SEAICE_EPS_SQ,etaDen))
CML           etaMax = ( 0.5 _d 0*press(I,J,bi,bj)-zeta(I,J,bi,bj)
CML     &          *( e11(I,J,bi,bj)+e22(I,J,bi,bj) ) 
CML     &          )/etaDen
CML           etaZ(I,J,bi,bj) = MIN(etaZ(I,J,bi,bj),etaMax)
CML          ENDDO
CML         ENDDO
        ENDIF
#endif /* SEAICE_ALLOW_TEM */
C     SEAICEetaZmethod
       ENDIF
C     free-slip means no lateral stress, which is best achieved masking
C     eta on vorticity(=Z)-points; from now on we only need to worry
C     about the no-slip boundary conditions
        IF (.NOT.SEAICE_no_slip) THEN
         DO J=1,sNy+1
          DO I=1,sNx+1
           etaZ(I,J,bi,bj) = etaZ(I,J,bi,bj)
     &          *maskC(I,J,  k,bi,bj)*maskC(I-1,J,  k,bi,bj)
     &          *maskC(I,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID */
      RETURN
      END
