#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#undef PRESERVE_C69C_OUTPUT

CBOP
C     !ROUTINE: MOMENTUM_CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE MOMENTUM_CORRECTION_STEP(
     O                    cg2d_x, cg3d_x,
     I                    myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MOMENTUM_CORRECTION_STEP
C     *==========================================================*
C     |1rst Part : Update U,V.
C     |
C     | The arrays used for time stepping are cycled.
C     | Momentum:
C     |           V(n) = Gv(n) - dt * grad Eta
C     |
C     |part1: update U,V
C     |  U*,V* (contained in gU,gV) have the surface
C     |     pressure gradient term added and the result stored
C     |     in U,V (contained in uVel, vVel)
C     |
C     |part2: Adjustments
C     |   o Filter  U,V (Shapiro Filter, Zonal_Filter)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#ifdef ALLOW_SHAP_FILT
# include "SHAP_FILT.h"
#endif
#ifdef ALLOW_ZONAL_FILT
# include "ZONAL_FILT.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     cg2d_x  :: Solution vector of the 2-D solver equation A.x=b
C     cg3d_x  :: Solution vector of the 3-D solver equation A.X=B
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id. number
      _RL cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_NONHYDROSTATIC
      _RL cg3d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#else /* ALLOW_NONHYDROSTATIC */
      _RL cg3d_x(1)
#endif /* ALLOW_NONHYDROSTATIC */
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      _RL phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        IF ( momStepping ) THEN
C--     Set up work arrays that need valid initial values
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            phiSurfX(i,j) = 0.
            phiSurfY(i,j) = 0.
           ENDDO
          ENDDO

C       Loop range: Gradients of Eta are evaluated so valid
C       range is all but first row and column in overlaps.
          iMin = 1-OLx+1
          iMax = sNx+OLx
          jMin = 1-OLy+1
          jMax = sNy+OLy

C-      Calculate gradient of surface Potential
#ifdef PRESERVE_C69C_OUTPUT
          CALL CALC_GRAD_PHI_SURF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         etaN,
     O         phiSurfX, phiSurfY,
     I         myThid )
#else
          DO j=jMin,jMax
           DO i=iMin,iMax
            phiSurfX(i,j) = _recip_dxC(i,j,bi,bj)
     &                    *( cg2d_x(i,j,bi,bj) - cg2d_x(i-1,j,bi,bj) )
           ENDDO
          ENDDO
          DO j=jMin,jMax
           DO i=iMin,iMax
            phiSurfY(i,j) = _recip_dyC(i,j,bi,bj)
     &                    *( cg2d_x(i,j,bi,bj) - cg2d_x(i,j-1,bi,bj) )
           ENDDO
          ENDDO
#endif

C-      Update velocity fields:  V(n) = V** - dt * grad Eta
          CALL CORRECTION_STEP(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         phiSurfX, phiSurfY, cg3d_x,
     I         myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_OBCS
        IF ( useOBCS ) THEN
          CALL OBCS_APPLY_UV( bi, bj, 0, uVel, vVel, myThid )
        ENDIF
#endif /* ALLOW_OBCS */

C--    End of 1rst bi,bj loop
       ENDDO
      ENDDO

C--- 2nd Part : Adjustment.

C--   Filter (and exchange)
#ifdef ALLOW_SHAP_FILT
      IF ( useSHAP_FILT ) THEN
       IF ( .NOT.shap_filt_uvStar ) THEN
        CALL TIMER_START('SHAP_FILT_UV       [MOM_CORR_STEP]',myThid)
        CALL SHAP_FILT_APPLY_UV( uVel, vVel, myTime, myIter, myThid )
        CALL TIMER_STOP ('SHAP_FILT_UV       [MOM_CORR_STEP]',myThid)
       ENDIF
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
      IF ( useZONAL_FILT ) THEN
       IF ( .NOT.zonal_filt_uvStar ) THEN
        CALL TIMER_START('ZONAL_FILT_UV      [MOM_CORR_STEP]',myThid)
        CALL ZONAL_FILT_APPLY_UV( uVel, vVel, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_UV      [MOM_CORR_STEP]',myThid)
       ENDIF
      ENDIF
#endif

C--   Try to fix restart (Pb at machine trucation level accumulating in wVel):
C     Apply EXCH to horiz velocity before integrating continuity (-> wVel)
      IF ( applyExchUV_early )
     &  CALL EXCH_UV_3D_RL( uVel, vVel, .TRUE., Nr, myThid )

      RETURN
      END
