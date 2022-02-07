#include "COST_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: COST_DEPTH
C     !INTERFACE:
      SUBROUTINE COST_DEPTH( myThid )

C     !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE COST_DEPTH                                    |
C     | o this routine computes the cost function for the tiles  |
C     |   of this processor                                      |
C     |==========================================================|
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     \==========================================================/
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "cost.h"
#ifdef ALLOW_GENARR2D_CONTROL
# include "CTRL_SIZE.h"
# include "CTRL_GENARR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_COST_DEPTH
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j
#ifdef ALLOW_GENARR2D_CONTROL
      INTEGER iarr
      INTEGER igen
      _RL xx_depth(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#else
      _RL dr_low
#endif
CEOP

#ifdef ALLOW_GENARR2D_CONTROL
      DO bj=myByLo(mythid),myByHi(mythid)
       DO bi=myBxLo(mythid),myBxHi(mythid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          xx_depth(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C--   use generic 2D control variable infrastructure to set xx_depth
      igen = 0
      DO iarr = 1, maxCtrlArr2D
       IF ( xx_genarr2d_weight(iarr).NE.' ' .AND.
     &      xx_genarr2d_file(iarr)(1:8).EQ.'xx_depth') igen = iarr
      ENDDO
      IF ( igen .GT. 0 ) THEN
       CALL CTRL_MAP_GENARR2D( xx_depth, igen, myThid )
      ENDIF
C--   Calculate cost function on tile of this instance
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF ( maskInC(i,j,bi,bj) .NE. 0. ) THEN
           objf_depth(bi,bj) = objf_depth(bi,bj)
     &          + 0.5 _d 0 * xx_depth(i,j,bi,bj)*xx_depth(i,j,bi,bj)
          ENDIF
         END DO
        END DO
       END DO
      END DO
#else
C--   Calculate cost function on tile of this instance
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF ( maskInC(i,j,bi,bj) .NE. 0. ) THEN
#ifdef ALLOW_DEPTH_CONTROL
C     This is just a test and does not make too much sense, because
C     R_low is recomputed after xx_r_low is set to "R_low +
C     xx_depth_file" so that dr_low is not the incremenent that would
C     make sense here.
           dr_low = xx_r_low(i,j,bi,bj) - R_low(i,j,bi,bj)
           objf_depth(bi,bj) = objf_depth(bi,bj)
     &          + 0.5 _d 0 *dr_low*dr_low
#endif
          ENDIF
         END DO
        END DO
       END DO
      END DO
#endif /* ALLOW_GENARR2D_CONTROL */
#endif /* ALLOW_COST_DEPTH */

      RETURN
      END
