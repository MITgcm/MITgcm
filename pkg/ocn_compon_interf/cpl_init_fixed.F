#include "OCN_CPL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CPL_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE CPL_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialise non evolving (fixed) coupling interface package,
C     Oceanic version

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL CPL_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
