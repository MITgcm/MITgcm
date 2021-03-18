#include "AUTODIFF_OPTIONS.h"

      SUBROUTINE G_AUTODIFF_INADMODE_UNSET( myThid )
C     *==========================================================*
C     | SUBROUTINE G_AUTODIFF_INADMODE_UNSET
C     *==========================================================*

      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"

C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid

      inAdMode = .FALSE.

      RETURN
      END
