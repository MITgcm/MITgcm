C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_init_fixed.F,v 1.2 2006/08/07 21:13:52 edhill Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_INIT_FIXED

C !INTERFACE:
      SUBROUTINE MYPACKAGE_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize MYPACKAGE variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===

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
