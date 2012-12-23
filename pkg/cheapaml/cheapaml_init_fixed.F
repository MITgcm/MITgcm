C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_init_fixed.F,v 1.6 2012/12/23 20:15:29 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

CBOP
C     !ROUTINE: CHEAPAML_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE CHEAPAML_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CHEAPAML_INIT_FIXED
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "CHEAPAML.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_MNC
c     IF (useMNC) THEN
c     ENDIF
#endif /* ALLOW_MNC */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL CHEAPAML_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      _BEGIN_MASTER( myThid )

C-    Initialise AB starting level
      cheapTairStartAB = nIter0
      cheapQairStartAB = nIter0
      cheapTracStartAB = nIter0

      _END_MASTER( myThid )

C-    Everyone else must wait for parameters to be set
      _BARRIER

      RETURN
      END
