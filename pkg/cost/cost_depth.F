#include "COST_OPTIONS.h"

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
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "cost.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_COST_DEPTH
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j
CEOP

C--   Calculate cost function on tile of this instance
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF ( maskC(i,j,1,bi,bj) .NE. 0. ) THEN
           objf_depth(bi,bj) = objf_depth(bi,bj) + R_low(i,j,bi,bj)
          ENDIF
         END DO
        END DO
       END DO
      END DO
#endif /* ALLOW_COST_DEPTH */

      RETURN
      END
