C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_init_fixed.F,v 1.1 2011/08/06 03:13:22 dimitri Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_INIT_FIXED

C !INTERFACE:
      SUBROUTINE BBL_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize BBL variables that are kept fixed during the run.

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

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL BBL_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
