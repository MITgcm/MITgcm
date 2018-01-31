#include "EXF_OPTIONS.h"

      subroutine EXF_ADJOINT_SNAPSHOTS( iwhen, mytime, myiter, myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE EXF_ADJOINT_SNAPSHOTS                         |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer iwhen
      integer myThid
      integer myiter
      _RL     mytime

#ifdef ALLOW_AUTODIFF_MONITOR
      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
#endif

      end









