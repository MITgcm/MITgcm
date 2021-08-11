#include "PP81_OPTIONS.h"

CBOP
C     !ROUTINE: PP81_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE PP81_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PP81_INIT_FIXED
C     | o Routine to initialize PP81 variables
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
CEOP

C     !LOCAL VARIABLES :
C     === Local variables ===

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL PP81_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
