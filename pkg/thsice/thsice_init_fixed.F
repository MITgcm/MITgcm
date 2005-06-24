C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_init_fixed.F,v 1.2 2005/06/24 04:36:54 edhill Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE THSICE_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THSICE_INIT_FIXED
C     | o Routine to initialize THSICE variables 
C     |   that are kept fixed during the run.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
c #include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===

#ifdef ALLOW_MNC
      IF ( useMNC ) THEN
        CALL THSICE_MNC_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL THSICE_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
