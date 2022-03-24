#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: ECCO_INIT_FIXED

C !INTERFACE:
      SUBROUTINE ECCO_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize ECCO variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialise ecco-specific cost function. This needs to preceed
C     the call ctrl_init in order to provide the weight files
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('ECCO_COST_INIT_FIXED',myThid)
#endif
      CALL ECCO_COST_INIT_FIXED( myThid )

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL ECCO_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
