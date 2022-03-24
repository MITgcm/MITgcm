#include "MY82_OPTIONS.h"

CBOP
C     !ROUTINE: MY82_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE MY82_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MY82_INIT_FIXED
C     | o Routine to initialize MY82 variables
C     |   that are kept fixed during the run.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES :
C     === Local variables ===
CEOP

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL MY82_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
