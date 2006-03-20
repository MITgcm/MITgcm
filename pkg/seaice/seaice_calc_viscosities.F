C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_viscosities.F,v 1.3 2006/03/20 21:36:12 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_CALC_VISCOSITIES( 
     I     uFld, vFld, zMin, zMax, hEffM, press0,
     O     eta, zeta, press, 
#ifdef SEAICE_ALLOW_EVP
     O     seaice_div, seaice_tension, seaice_shear,
#endif /* SEAICE_ALLOW_EVP */
     I     myThid )
C     /==========================================================\
C     | SUBROUTINE  SEAICE_CALC_VISCOSITIES                      |
C     | o compute shear and bulk viscositites eta, zeta and the  |
C     |   ice strength P                                         |
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
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
      _RL uFld  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL vFld  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL zMin  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL zMax  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL hEffM (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL press0(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL press (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL  eta  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL zeta  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
#ifdef SEAICE_ALLOW_EVP
      _RL seaice_div    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaice_tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaice_shear  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_ALLOW_EVP */
CEndOfInterface

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS
C     === Local variables ===
C     i,j,bi,bj - Loop counters
C     e11, e12, e22 - components of strain rate tensor
C     ecm2          - inverse of square of eccentricity of yield curve
      INTEGER i, j, bi, bj
      _RL e11, e12, e22
      _RL ECM2, DELT1, DELT2
#ifdef SEAICE_ALLOW_EVP
      _RL delta (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

C--   FIRST SET UP BASIC CONSTANTS
      ecm2=0. _d 0
      IF ( SEAICE_eccen .NE. 0. _d 0 ) ecm2=ONE/(SEAICE_eccen**2)
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
C NOW EVALUATE STRAIN RATES
          e11= _recip_dxF(I,J,bi,bj) *
     &         (uFld(I+1,J,bi,bj)-uFld(I,J,bi,bj))
     &         -HALF*
     &         (vFld(I,J,bi,bj)+vFld(I,J+1,bi,bj))
     &         * _tanPhiAtU(I,J,bi,bj)*recip_rSphere
          e22= _recip_dyF(I,J,bi,bj) *
     &         (vFld(I,J+1,bi,bj)-vFld(I,J,bi,bj))
C     one metric term is missing
          e12=HALF*(
     &         (uFld(I,J+1,bi,bj)+uFld(I+1,J+1,bi,bj)
     &         -uFld(I,J-1,bi,bj)-uFld(I+1,J-1,bi,bj))
     &         * 1. _d 0 / (dyC(I,J,bi,bj) + dyC(I,J-1,bi,bj))
     &         +
     &         (vFld(I+1,J+1,bi,bj)+vFld(I+1,J,bi,bj)
     &         -vFld(I-1,J+1,bi,bj)-vFld(I-1,J,bi,bj))
     &         * 1. _d 0 / (dxC(I,J,bi,bj) + dxC(I-1,J,bi,bj))
     &         +HALF*
     &         (uFld(I,  J,  bi,bj)+uFld(I+1,J,  bi,bj))
     &         * _tanPhiAtU(I,J,bi,bj)*recip_rSphere)
C  NOW EVALUATE VISCOSITIES
          DELT1=(e11**2+e22**2)*(ONE+ECM2)
     &         +4. _d 0*ECM2*e12**2
     &         +TWO*e11*e22*(ONE-ECM2)
          IF ( DELT1 .LE. SEAICE_EPS_SQ ) THEN
             DELT2=SEAICE_EPS
          ELSE
             DELT2=SQRT(DELT1)
          ENDIF
          zeta(I,J,bi,bj)=HALF*PRESS0(I,J,bi,bj)/DELT2
C NOW PUT MIN AND MAX VISCOSITIES IN
          zeta(I,J,bi,bj)=MIN(ZMAX(I,J,bi,bj),zeta(I,J,bi,bj))
          zeta(I,J,bi,bj)=MAX(ZMIN(I,J,bi,bj),zeta(I,J,bi,bj))
C NOW SET VISCOSITIES TO ZERO AT HEFFMFLOW PTS 
          zeta (I,J,bi,bj) = zeta(I,J,bi,bj)*HEFFM(I,J,bi,bj) 
          eta  (I,J,bi,bj) = ECM2*zeta(I,J,bi,bj)
          press(I,J,bi,bj) = TWO*zeta(I,J,bi,bj)*DELT2
#ifdef SEAICE_ALLOW_EVP
          delta(I,J) = DELT2
          seaice_div    (I,J,bi,bj) = (e11+e22)/DELT2*hEffM(I,J,bi,bj)
          seaice_tension(I,J,bi,bj) = (e11-e22)/DELT2*hEffM(I,J,bi,bj)
C     recompute e12 on Z point so that seaice_shear and seaice_sigma12
C     are also defined on Z points (one half and two drop out)
          seaice_shear  (I,J,bi,bj) = (
     &         (uFld(I  ,J  ,bi,bj) * _dxC(I  ,J  ,bi,bj)
     &         -uFld(I  ,J-1,bi,bj) * _dxC(I  ,J-1,bi,bj)
     &         +vFld(I  ,J  ,bi,bj) * _dyC(I  ,J  ,bi,bj)
     &         -vFld(I-1,J  ,bi,bj) * _dyC(I-1,J  ,bi,bj))
     &         * recip_rAz(I,J,bi,bj)
     &         +
     &         0.25 _d 0 * (uFld(I,J,bi,bj)+uFld(I  ,J-1,bi,bj))
     &         * ( _tanPhiAtU(I,J,bi,bj) + _tanPhiAtU(I,J-1,bi,bj) )
     &         *recip_rSphere
     &         )
C     one metric term is missing
#endif /* SEAICE_ALLOW_EVP */
         ENDDO
        ENDDO
#ifdef SEAICE_ALLOW_EVP
        DO j=1,sNy
         DO i=1,sNx
          DELT2 = 0.25 * ( delta(I,J  ) + delta(I-1,J  ) 
     &         +           delta(I,J-1) + delta(I-1,J-1) )
          seaice_shear(I,J,bi,bj) = 
     &         seaice_shear(I,J,bi,bj)/DELT2
     &         *hEffM(I  ,J  ,bi,bj)*hEffM(I-1,J  ,bi,bj)
     &         *hEffM(I  ,J-1,bi,bj)*hEffM(I-1,J-1,bi,bj)
         ENDDO
        ENDDO
#endif /* SEAICE_ALLOW_EVP */
       ENDDO
      ENDDO

C--   Update overlap regions
      _EXCH_XY_R8(  eta, myThid)
      _EXCH_XY_R8( zeta, myThid)
      _EXCH_XY_R8(press, myThid)
#ifdef SEAICE_ALLOW_EVP
      _EXCH_XY_R8(seaice_div    , myThid)
      _EXCH_XY_R8(seaice_tension, myThid)
      _EXCH_XY_R8(seaice_shear  , myThid)
#endif /* SEAICE_ALLOW_EVP */
#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */
      RETURN
      END
