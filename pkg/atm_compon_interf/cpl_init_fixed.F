#include "ATM_CPL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CPL_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE CPL_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Define MNC variables for use by the coupler.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC
C      IF (useMNC) THEN
C      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL CPL_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
