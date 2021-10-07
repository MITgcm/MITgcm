#include "AUTODIFF_OPTIONS.h"

      SUBROUTINE AUTODIFF_INADMODE_UNSET( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE AUTODIFF_INADMODE_UNSET
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      RETURN
      END
