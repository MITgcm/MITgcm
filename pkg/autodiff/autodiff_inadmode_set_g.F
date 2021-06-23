#include "AUTODIFF_OPTIONS.h"

      SUBROUTINE G_AUTODIFF_INADMODE_SET( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE G_ADAUTODIFF_INADMODE_SET
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# ifdef ALLOW_DIAGNOSTICS
#  include "DIAGNOSTICS_SIZE.h"
#  include "DIAGNOSTICS.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#ifdef ALLOW_AUTODIFF_MONITOR
# ifdef ALLOW_DIAGNOSTICS
      LOGICAL modelEnd
C --- Write the fields out here
      IF ( useDiag4AdjOutp ) THEN
C --- Grab modelEnd, necessary input for diagnostics routines
       modelEnd = myTime.EQ.endTime .OR. myIter.EQ.nEndIter
       CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
       CALL DIAGNOSTICS_WRITE_ADJ( modelEnd, myTime, myIter, myThid )
       CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF
# endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_AUTODIFF_MONITOR */

      inAdmode = .FALSE.

      RETURN
      END
