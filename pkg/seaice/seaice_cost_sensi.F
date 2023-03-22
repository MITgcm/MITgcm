#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_COST_SENSI( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE SEAICE_COST_SENSI
C     | o driver for seaice sensitivity cost functions
C     *==========================================================*

      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_COST
C     == Local variables

      CALL SEAICE_COST_TEST ( myTime, myIter, myThid )

      CALL SEAICE_COST_ACCUMULATE_MEAN (
     &    myTime, myIter, myThid)

#endif

      RETURN
      END
