#include "AUTODIFF_OPTIONS.h"

      SUBROUTINE G_AUTODIFF_INADMODE_UNSET( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE G_AUTODIFF_INADMODE_UNSET
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
      IF ( useDiag4AdjOutp ) THEN
        CALL DIAGNOSTICS_SWITCH_ONOFF( myTime, myIter, myThid )
      ENDIF
# endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_AUTODIFF_MONITOR */

      inAdMode = .FALSE.

      RETURN
      END
