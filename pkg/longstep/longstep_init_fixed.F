C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_init_fixed.F,v 1.1 2009/06/26 23:10:10 jahn Exp $
C $Name:  $

#include "LONGSTEP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE LONGSTEP_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize longstep constant

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_LONGSTEP

C     !LOCAL VARIABLES:

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL LONGSTEP_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
