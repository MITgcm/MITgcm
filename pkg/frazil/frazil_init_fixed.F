C $Header: /u/gcmpack/MITgcm/pkg/frazil/frazil_init_fixed.F,v 1.2 2012/03/04 23:11:20 dimitri Exp $
C $Name:  $

#include "FRAZIL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: FRAZIL_INIT_FIXED

C !INTERFACE:
      SUBROUTINE FRAZIL_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize FRAZIL variables that are kept fixed during the run

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL FRAZIL_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
