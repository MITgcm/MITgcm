#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MYPACKAGE_INIT_FIXED

C !INTERFACE:
      SUBROUTINE MYPACKAGE_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize MYPACKAGE variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        CALL MYPACKAGE_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL MYPACKAGE_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
