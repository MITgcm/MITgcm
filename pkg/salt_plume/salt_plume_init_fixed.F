C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_init_fixed.F,v 1.1 2007/11/28 00:18:17 dimitri Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C !ROUTINE: SALT_PLUME_INIT_FIXED

C !INTERFACE:
      SUBROUTINE SALT_PLUME_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize SALT_PLUME variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        CALL SALT_PLUME_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL SALT_PLUME_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
