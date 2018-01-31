#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE CORRECTION_STEP( bi, bj, iMin, iMax, jMin, jMax,
     I                      phiSurfX, phiSurfY,
     I                      myTime, myIter, myThid )
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
C     phiSurfX, phiSurfY  :: Surface Potential gradient
C     myTime              :: Current time in simulation
C     myIter              :: Current iteration number in simulation
C     myThid              :: my Thread Id number
      _RL     phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi, bj
      INTEGER iMin, iMax, jMin, jMax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j         :: Loop counters
C     k            :: Level index
C     psFac, nhFac :: Scaling parameters for supressing gradients
C     gU_dpx       :: implicit part of pressure gradient tendency
C     gV_dpy       :: implicit part of pressure gradient tendency
      INTEGER i,j
      INTEGER k
      _RL     psFac, nhFac
      _RL     gU_dpx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gV_dpy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C--   Loop over all layers, top to bottom
      DO k=1,Nr

#ifdef ALLOW_SOLVE4_PS_AND_DRAG
       IF ( selectImplicitDrag.EQ.2 ) THEN

C     On/off scaling parameter
        psFac = pfFacMom*implicSurfPress

C     Pressure gradient tendency (zonal mom): Implicit part
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU_dpx(i,j) =
     &         -psFac*dU_psFacX(i,j,k,bi,bj)*phiSurfX(i,j)
c    &                  *_maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO

C     Pressure gradient tendency (merid mom): Implicit part
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV_dpy(i,j) =
     &         -psFac*dV_psFacY(i,j,k,bi,bj)*phiSurfY(i,j)
c    &                  *_maskS(i,j,k,bi,bj)
         ENDDO
        ENDDO

       ELSE
#endif /* ALLOW_SOLVE4_PS_AND_DRAG */

C     On/off scaling parameters (including anelastic & deep-model factors)
        psFac = pfFacMom*implicSurfPress
     &         *recip_deepFacC(k)*recip_rhoFacC(k)
        IF ( use3Dsolver ) THEN
         nhFac = pfFacMom*implicitNHPress
     &          *recip_deepFacC(k)*recip_rhoFacC(k)
        ELSE
         nhFac = 0.
        ENDIF

C     Pressure gradient tendency (zonal mom): Implicit part
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU_dpx(i,j) = -(
     &          psFac*phiSurfX(i,j)
#ifdef ALLOW_NONHYDROSTATIC
     &        + nhFac*_recip_dxC(i,j,bi,bj)
     &           *(phi_nh(i,j,k,bi,bj)-phi_nh(i-1,j,k,bi,bj))
#endif
     &                   )*_maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO

C     Pressure gradient tendency (merid mom): Implicit part
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV_dpy(i,j) = -(
     &          psFac*phiSurfY(i,j)
#ifdef ALLOW_NONHYDROSTATIC
     &        + nhFac*_recip_dyC(i,j,bi,bj)
     &           *(phi_nh(i,j,k,bi,bj)-phi_nh(i,j-1,k,bi,bj))
#endif
     &                   )*_maskS(i,j,k,bi,bj)
         ENDDO
        ENDDO

#ifdef ALLOW_SOLVE4_PS_AND_DRAG
       ENDIF
#endif /* ALLOW_SOLVE4_PS_AND_DRAG */

C     Update zonal velocity: add implicit pressure gradient tendency
       DO j=jMin,jMax
        DO i=iMin,iMax
          uVel(i,j,k,bi,bj)=( gU(i,j,k,bi,bj)
     &                      + deltaTMom*gU_dpx(i,j)
     &                      )*_maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                       *maskInW(i,j,bi,bj)
#endif
        ENDDO
       ENDDO

C     Update merid. velocity: add implicit pressure gradient tendency
       DO j=jMin,jMax
        DO i=iMin,iMax
          vVel(i,j,k,bi,bj)=( gV(i,j,k,bi,bj)
     &                      + deltaTMom*gV_dpy(i,j)
     &                      )*_maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                       *maskInS(i,j,bi,bj)
#endif
        ENDDO
       ENDDO

#ifdef ALLOW_DIAGNOSTICS
c      IF ( useDiagnostics ) THEN
c       CALL DIAGNOSTICS_FILL( gU_dpx,
c    &                         'UDIAG7  ', k, 1, 2, bi, bj, myThid )
c       CALL DIAGNOSTICS_FILL( gV_dpy,
c    &                         'UDIAG8  ', k, 1, 2, bi, bj, myThid )
c      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-    end of k loop
      ENDDO

      RETURN
      END
