C $Header: /u/gcmpack/MITgcm/model/src/correction_step.F,v 1.26 2011/05/20 16:35:57 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE CORRECTION_STEP( bi, bj, iMin, iMax, jMin, jMax,
     I                       k, phiSurfX, phiSurfY,
     I                       myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CORRECTION_STEP
C     | o Corrects the horizontal flow fields with the surface
C     |   pressure (and Non-Hydrostatic pressure).
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi, bj              :: Tile indices
C     iMin,iMax,jMin,jMax :: Loop counters range
C     k                   :: Level index
C     phiSurfX, phiSurfY  :: Surface Potential gradient
C     myTime              :: Current time in simulation
C     myThid              :: my Thread Id number
      _RL     phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj
      INTEGER iMin,iMax,jMin,jMax
      INTEGER k
      _RL     myTime
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j          :: Loop counters
C     psFac, nhFac :: Scaling parameters for supressing gradients
      INTEGER i,j
      _RL     psFac, nhFac
CEOP

C     On/off scaling parameters (including anelastic & deep-model factors)
      psFac = pfFacMom*implicSurfPress
     &       *recip_deepFacC(k)*recip_rhoFacC(k)
      IF ( use3Dsolver ) THEN
        nhFac = pfFacMom*implicitNHPress
     &         *recip_deepFacC(k)*recip_rhoFacC(k)
      ELSE
        nhFac = 0.
      ENDIF

C     Step forward zonal velocity
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVel(i,j,k,bi,bj)=( gU(i,j,k,bi,bj)
     &       -deltaTmom*psFac*phiSurfX(i,j)
#ifdef ALLOW_NONHYDROSTATIC
     &       -deltaTmom*nhFac*_recip_dxC(i,j,bi,bj)
     &         *(phi_nh(i,j,k,bi,bj)-phi_nh(i-1,j,k,bi,bj))
#endif
     &                    )*_maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                     *maskInW(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

C     Step forward meridional velocity
      DO j=jMin,jMax
       DO i=iMin,iMax
        vVel(i,j,k,bi,bj)=( gV(i,j,k,bi,bj)
     &       -deltaTmom*psFac*phiSurfY(i,j)
#ifdef ALLOW_NONHYDROSTATIC
     &       -deltaTmom*nhFac*_recip_dyC(i,j,bi,bj)
     &         *(phi_nh(i,j,k,bi,bj)-phi_nh(i,j-1,k,bi,bj))
#endif
     &                    )*_maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                     *maskInS(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
