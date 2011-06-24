C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_init_fixed.F,v 1.3 2011/06/24 01:24:07 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

CBOP
C     !ROUTINE: CHEAPAML_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE CHEAPAML_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CHEAPAML_INIT_FIXED
C     | o Routine to initialize CheapAML variables
C     |   that are kept fixed during the run.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Note: this should go in a separated S/R: KPP_MNC_INIT
#ifdef ALLOW_MNC
      IF (useMNC) THEN
      ENDIF
#endif /* ALLOW_MNC */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL CHEAPAML_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
