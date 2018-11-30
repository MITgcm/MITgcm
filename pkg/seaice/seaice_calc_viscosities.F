#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
CStartOfInterface
      SUBROUTINE SEAICE_CALC_VISCOSITIES(
     I     e11, e22, e12, zMin, zMax, HEFFM, press0, tnsFac,
     O     eta, etaZ, zeta, zetaZ, press, deltaC,
     I     iStep, myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE  SEAICE_CALC_VISCOSITIES                      |
C     | o compute shear and bulk viscositites eta, zeta and the  |
C     |   corrected ice strength P                               |
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997)    |
C     *==========================================================*
C     | written by Martin Losch, Mar 2006                        |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

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
      _RL zMin  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zMax  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HEFFM (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
      _RL press0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL press (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
      _RL deltaC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     factor k to compute tensile strength from k*press0
      _RL tnsFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     bulk viscosity
      _RL  eta  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etaZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     shear viscosity
      _RL zeta  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEndOfInterface

#if ( defined (SEAICE_CGRID) && defined (SEAICE_ALLOW_DYNAMICS) )
C     === Local variables ===
C     i,j,bi,bj - Loop counters
C     e11, e12, e22 - components of strain rate tensor
C     recip_e2      - inverse of square of ratio of yield curve main axes
C     ep            - e11+e22 (abbreviation)
C     em            - e11-e22 (abbreviation)
      INTEGER i, j, bi, bj
      _RL recip_e2, deltaCreg, deltaCsq, deltaMinSq, tmp, ep, em
      _RL e12Csq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef SEAICE_ALLOW_TEM
      _RL etaMax, etaDen
#endif /* SEAICE_ALLOW_TEM */
      _RL sumNorm, maskZ
#ifdef SEAICE_ZETA_SMOOTHREG
      _RL argTmp
#endif /* SEAICE_ZETA_SMOOTHREG */
CEOP

C--   basic constants
      recip_e2=0. _d 0
      IF ( SEAICE_eccen .NE. 0. _d 0 ) recip_e2=ONE/(SEAICE_eccen**2)
      deltaMinSq = SEAICE_deltaMin**2
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          e12Csq(i,j) = 0. _d 0
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */
C     need to do this beforehand for easier vectorization after
C     TAFization
        IF ( SEAICEetaZmethod .EQ. 0 ) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           tmp = 0.25 *
     &          ( e12(I,J  ,bi,bj) + e12(I+1,J  ,bi,bj)
     &          + e12(I,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
           e12Csq(i,j) = tmp*tmp
          ENDDO
         ENDDO
        ELSEIF ( SEAICEetaZmethod .EQ. 3 ) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
C     area weighted average of the squares of e12 is more accurate
C     (and energy conserving)
           e12Csq(i,j) = 0.25 _d 0 * recip_rA(I,J,bi,bj) *
     &          ( rAz(I  ,J  ,bi,bj)*e12(I  ,J  ,bi,bj)**2
     &          + rAz(I+1,J  ,bi,bj)*e12(I+1,J  ,bi,bj)**2
     &          + rAz(I  ,J+1,bi,bj)*e12(I  ,J+1,bi,bj)**2
     &          + rAz(I+1,J+1,bi,bj)*e12(I+1,J+1,bi,bj)**2 )
          ENDDO
         ENDDO
        ENDIF
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)
          em = e11(i,j,bi,bj)-e22(i,j,bi,bj)
          deltaCsq = ep*ep+recip_e2*(em*em+4. _d 0*e12Csq(i,j))
CML The old formulation does not ensure that deltaC**2 is always positive,
CML but in case you need it to reproduce old results, here it is:
CML          deltaCsq = 
CML     &         (e11(i,j,bi,bj)**2+e22(i,j,bi,bj)**2)*(ONE+recip_e2)
CML     &         + 4. _d 0*recip_e2*e12Csq(i,j)
CML     &         + 2. _d 0*e11(i,j,bi,bj)*e22(i,j,bi,bj)*(ONE-recip_e2)
#ifdef ALLOW_AUTODIFF_TAMC
C     avoid sqrt of 0
          deltaC(I,J,bi,bj) = 0. _d 0
          IF ( deltaCsq .GT. 0. _d 0 ) 
     &         deltaC(I,J,bi,bj) = SQRT(deltaCsq)
#else
          deltaC(I,J,bi,bj) = SQRT(deltaCsq)
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef SEAICE_DELTA_SMOOTHREG
C     smooth regularization (without max-function) of delta for
C     better differentiability
          deltaCreg = SQRT(deltaCsq + deltaMinSq)
CML          deltaCreg = deltaC(I,J,bi,bj) + SEAICE_deltaMin
#else
          deltaCreg = MAX(deltaC(I,J,bi,bj),SEAICE_deltaMin)
#endif /* SEAICE_DELTA_SMOOTHREG */
#ifdef SEAICE_ZETA_SMOOTHREG
C     regularize zeta to zmax with a smooth tanh-function instead
C     of a min(zeta,zmax). This improves convergence of iterative
C     solvers (Lemieux and Tremblay 2009, JGR). No effect on EVP
          argTmp = exp(-1. _d 0/(deltaCreg*SEAICE_zetaMaxFac))
          zeta (I,J,bi,bj) = ZMAX(I,J,bi,bj)
     &         *(1. _d 0 - argTmp)/(1. _d 0 + argTmp)
#else
          zeta (I,J,bi,bj) = HALF*( press0(I,J,bi,bj) 
     &         * ( 1. _d 0 + tnsFac(I,J,bi,bj) )
     &         )/deltaCreg
C     put min and max viscosities in
          zeta (I,J,bi,bj) = MIN(ZMAX(I,J,bi,bj),zeta(I,J,bi,bj))
#endif /*  SEAICE_ZETA_SMOOTHREG */
          zeta (I,J,bi,bj) = MAX(ZMIN(I,J,bi,bj),zeta(I,J,bi,bj))
C     set viscosities to zero at HEFFM flow pts
          zeta (I,J,bi,bj) = zeta(I,J,bi,bj)*HEFFM(I,J,bi,bj)
          eta  (I,J,bi,bj) = recip_e2*zeta(I,J,bi,bj)
C     "replacement pressure"
          press(I,J,bi,bj) =
     &         ( press0(I,J,bi,bj)*( 1. _d 0 - SEAICEpressReplFac )
     &         + TWO*zeta(I,J,bi,bj)*deltaC(I,J,bi,bj)
     &         * SEAICEpressReplFac/( 1. _d 0 + tnsFac(I,J,bi,bj) )
     &         ) * ( 1. _d 0 - tnsFac(I,J,bi,bj) )
CML          press(I,J,bi,bj) = press0(I,J,bi,bj) *
CML     &         ( 1. _d 0 + SEAICEpressReplFac
CML     &                  * ( deltaC(I,J,bi,bj)/deltaCreg - 1. _d 0 )
CML     &         ) * ( 1. _d 0 - tnsFac(I,J,bi,bj) )
         ENDDO
        ENDDO
#ifdef SEAICE_ALLOW_TEM
        IF ( SEAICEuseTEM ) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           ep = e11(i,j,bi,bj)+e22(i,j,bi,bj)
           em = e11(i,j,bi,bj)-e22(i,j,bi,bj)
           etaDen = em*em + 4. _d 0 * e12Csq(i,j)
           etaDen = SQRT(MAX(deltaMinSq,etaDen))
           etaMax = ( 0.5 _d 0*press(I,J,bi,bj)-zeta(I,J,bi,bj)*ep
     &          )/etaDen
           eta(I,J,bi,bj) = MIN(eta(I,J,bi,bj),etaMax)
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_ALLOW_TEM */
C     compute eta at Z-points by simple averaging
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          sumNorm  = HEFFM(I,J,  bi,bj)+HEFFM(I-1,J,  bi,bj)
     &             + HEFFM(I,J-1,bi,bj)+HEFFM(I-1,J-1,bi,bj)
          IF ( sumNorm.GT.0. _d 0 ) sumNorm = 1. _d 0 / sumNorm
          etaZ(I,J,bi,bj) = sumNorm *
     &         ( eta (I,J  ,bi,bj)  + eta (I-1,J  ,bi,bj)
     &         + eta (I,J-1,bi,bj)  + eta (I-1,J-1,bi,bj) )
          zetaZ(I,J,bi,bj) = sumNorm *
     &         ( zeta(I,J  ,bi,bj)  + zeta(I-1,J  ,bi,bj)
     &         + zeta(I,J-1,bi,bj)  + zeta(I-1,J-1,bi,bj) )
         ENDDO
        ENDDO
C     free-slip means no lateral stress, which is best achieved by masking
C     eta on vorticity(=Z)-points; from now on we only need to worry
C     about the no-slip boundary conditions
        IF (.NOT.SEAICE_no_slip) THEN
         DO J=1-OLy+1,sNy+OLy-1
          DO I=1-OLx+1,sNx+OLx-1
           maskZ = HEFFM(I,J,  bi,bj)*HEFFM(I-1,J,  bi,bj)
     &          *  HEFFM(I,J-1,bi,bj)*HEFFM(I-1,J-1,bi,bj)
           etaZ (I,J,bi,bj) =  etaZ(I,J,bi,bj) * maskZ
           zetaZ(I,J,bi,bj) = zetaZ(I,J,bi,bj) * maskZ
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID */
      RETURN
      END
