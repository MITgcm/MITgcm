C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_init_fixed.F,v 1.2 2010/01/22 00:51:54 dimitri Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

      SUBROUTINE ICEFRONT_INIT_FIXED( myThid )
C     *============================================================*
C     | SUBROUTINE ICEFRONT_INIT_FIXED
C     | o Routine to initialize ICEFRONT parameters and variables.
C     *============================================================*
C     | Initialize ICEFRONT parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEFRONT_INIT_FIXED
      INTEGER myThid

#ifdef ALLOW_ICEFRONT
C     === Local variables ===

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL ICEFRONT_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ICEFRONT */

      RETURN
      END
