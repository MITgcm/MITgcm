C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_init_fixed.F,v 1.1 2006/06/28 21:26:21 heimbach Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

CBOP
C     !ROUTINE: MYPACKAGE_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE MYPACKAGE_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MYPACKAGE_INIT_FIXED
C     | o Routine to initialize MYPACKAGE variables 
C     |   that are kept fixed during the run.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
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
