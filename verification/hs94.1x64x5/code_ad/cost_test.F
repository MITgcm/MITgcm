#include "COST_OPTIONS.h"

CBOP
C     !ROUTINE: COST_TEST
C     !INTERFACE:
      SUBROUTINE COST_TEST( myThid )
C     *==========================================================*
C     | SUBROUTINE COST_TEST
C     | o this routine computes the cost function for the tiles
C     |   of this processor
C     *==========================================================*
C     | Notes
C     *==========================================================*
C     !USES:
       IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "cost.h"

C !INPUT/OUTPUT PARAMETERS:
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

#ifdef ALLOW_COST_TEST
C     Abuse this routine as driver to call a free-format function and
C     pass all variables normally imported by include statements.
      CALL COST_TEST_LOCAL (
     I     sNx, sNy, nSx, nSy, OLx, OLy, Nr,
     I     myBxLo(myThid), myBxHi(myThid),
     I     myByLo(myThid), myByHi(myThid),
     I     myXGlobalLo, myYGlobalLo,
     I     theta,
     U     objf_test,
     I     myThid )
#endif  /* ALLOW_COST_TEST */

      RETURN
      END
