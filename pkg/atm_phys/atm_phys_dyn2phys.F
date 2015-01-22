C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_dyn2phys.F,v 1.2 2015/01/22 18:11:13 jmc Exp $
C $Name:  $

#include "ATM_PHYS_OPTIONS.h"

CBOP
C !ROUTINE: ATM_PHYS_DYN2PHYS

C !INTERFACE: ==========================================================
      SUBROUTINE ATM_PHYS_DYN2PHYS(
     O                    lat2d, pHalf3d, pFull3d,
     O                    zHalf3d, zFull3d,
     O                    t3d, q3d, u3d, v3d,
     I                    bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | S/R ATM_PHYS_DYN2PHYS
C     | o Get grid and dynamical fields (from main model common
C     |   blocks) and return them as argument to ATM_PHYS_DRIVER
C     *==========================================================*
C     \ev

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SURFACE.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj   :: Tile indices
C  myTime   :: Current time in simulation
C  myIter   :: Current time-step number
C  myThid   :: my Thread Id number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
C  lat2d    :: latitude of grid-cell center          [rad]
C pHalf3d   :: pressure at interface between 2 levels [Pa]
C pFull3d   :: pressure at level center               [Pa]
C zHalf3d   :: height of interface between 2 levels   [m]
C zFull3d   :: height of level center                 [m]
C  t3d      :: absolute temperature                   [K]
C  q3d      :: specific humidity                    [kg/kg]
C  u3d      :: wind speed, 1rst component (X-dir)    [m/s]
C  v3d      :: wind speed, 2nd  component (Y-dir)    [m/s]
      _RL lat2d   (sNx,sNy)
      _RL pHalf3d (sNx,sNy,Nr+1)
      _RL pFull3d (sNx,sNy,Nr)
      _RL zHalf3d (sNx,sNy,Nr+1)
      _RL zFull3d (sNx,sNy,Nr)
      _RL t3d     (sNx,sNy,Nr)
      _RL q3d     (sNx,sNy,Nr)
      _RL u3d     (sNx,sNy,Nr)
      _RL v3d     (sNx,sNy,Nr)

C !LOCAL VARIABLES: ====================================================
      _RL conv_theta2T
      INTEGER k, kc, ki, kp
c     INTEGER ioUnit
c     _RS     dummyRS(1)
c     CHARACTER*40 namFile
CEOP

C--   latitude and pressure levels
      lat2d(:,:) = yC(1:sNx,1:sNy,bi,bj)*deg2rad
#ifdef NONLIN_FRSURF
      IF ( nonlinFreeSurf.GT.0 ) THEN
       IF ( staggerTimeStep.AND.select_rStar.GT.0 ) THEN
         DO k=1,Nr
          kc = Nr-k+1
          pFull3d(:,:,k) = rF(Nr+1) + ( rC(kc) - rF(Nr+1) )
     &                               *rStarFacC(1:sNx,1:sNy,bi,bj)
         ENDDO
         DO k=1,Nr+1
          ki = Nr-k+2
          pHalf3d(:,:,k) = rF(Nr+1) + ( rF(ki) - rF(Nr+1) )
     &                               *rStarFacC(1:sNx,1:sNy,bi,bj)
         ENDDO
       ELSEIF ( select_rStar.GT.0 ) THEN
         DO k=1,Nr
          kc = Nr-k+1
          pFull3d(:,:,k) = rF(Nr+1) + ( rC(kc) - rF(Nr+1) )
     &                               *rStarFacNm1C(1:sNx,1:sNy,bi,bj)
         ENDDO
         DO k=1,Nr+1
          ki = Nr-k+2
          pHalf3d(:,:,k) = rF(Nr+1) + ( rF(ki) - rF(Nr+1) )
     &                               *rStarFacNm1C(1:sNx,1:sNy,bi,bj)
         ENDDO
       ELSE
         STOP 'ATM_PHYS_DYN2PHYS: misssing code - 1 -'
       ENDIF
      ELSE
#else /* ndef NONLIN_FRSURF */
      IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */
       DO k=1,Nr
        kc = Nr-k+1
        pFull3d(:,:,k) = rC(kc)
       ENDDO
       DO k=1,Nr+1
        ki = Nr-k+2
        pHalf3d(:,:,k) = rF(ki)
       ENDDO
      ENDIF

C--   level height and 3-D dynamical fields
      DO k=1,Nr
        kc = Nr-k+1
        zFull3d(:,:,k) = ( phiRef(2*kc)
     &                   + totPhiHyd(1:sNx,1:sNy,kc,bi,bj)
     &                   )*recip_gravity
        conv_theta2T = (rC(kc)/atm_po)**atm_kappa
        t3d(:,:,k) = theta(1:sNx,1:sNy,kc,bi,bj)*conv_theta2T
        q3d(:,:,k) = MAX( salt(1:sNx,1:sNy,kc,bi,bj), 0. _d 0 )
        u3d(:,:,k) = ( uVel(1:sNx,  1:sNy,kc,bi,bj)
     &               + uVel(2:sNx+1,1:sNy,kc,bi,bj) )*0.5 _d 0
        v3d(:,:,k) = ( vVel(1:sNx,1:sNy,  kc,bi,bj)
     &               + vVel(1:sNx,2:sNy+1,kc,bi,bj) )*0.5 _d 0
       IF ( nonlinFreeSurf.LE.0 ) THEN
        zFull3d(:,:,k) = zFull3d(:,:,k)
     &                 - Bo_surf(1:sNx,1:sNy,bi,bj)
     &                     *etaN(1:sNx,1:sNy,bi,bj)
     &                     *recip_gravity
       ENDIF
#ifdef NONLIN_FRSURF
       IF ( select_rStar.GE.1 ) THEN
          t3d(:,:,k) = t3d(:,:,k)*pStarFacK(1:sNx,1:sNy,bi,bj)
       ENDIF
#endif /* NONLIN_FRSURF */
      ENDDO
c       ioUnit = 0
c       WRITE(namFile,'(A,I10.10)') 'z1_Atm.', myIter
c       CALL MDS_WRITEVEC_LOC(
c    I                       namFile, writeBinaryPrec, ioUnit,
c    I                       'RL', sNx*sNy, zFull3d(1,1,Nr), dummyRS,
c    I                       bi, bj, 1, myIter, myThid )
      DO k=1,Nr+1
        ki = Nr-k+2
        zHalf3d(:,:,k) = phiRef(2*ki-1)*recip_gravity
      ENDDO
      DO k=1,Nr
        kc = Nr-k+1
        kp = MIN(kc+1,Nr)
        zHalf3d(:,:,k) = zHalf3d(:,:,k)
     &                 + ( totPhiHyd(1:sNx,1:sNy,kp,bi,bj)
     &                    +totPhiHyd(1:sNx,1:sNy,kc,bi,bj) )*0.5
     &                  *recip_gravity
       IF ( nonlinFreeSurf.LE.0 ) THEN
        zHalf3d(:,:,k) = zHalf3d(:,:,k)
     &                 - Bo_surf(1:sNx,1:sNy,bi,bj)
     &                     *etaN(1:sNx,1:sNy,bi,bj)
     &                     *recip_gravity
       ENDIF
      ENDDO

      RETURN
      END
