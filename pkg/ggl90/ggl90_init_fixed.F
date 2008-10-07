C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_init_fixed.F,v 1.1 2008/10/07 19:32:07 dfer Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: GGL90_INIT_FIXED

C !INTERFACE:
      SUBROUTINE GGL90_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize GGL90 variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===

C#ifdef ALLOW_MNC
C      IF (useMNC) THEN
C        CALL GGL90_MNC_INIT( myThid )
C      ENDIF
C#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL GGL90_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
