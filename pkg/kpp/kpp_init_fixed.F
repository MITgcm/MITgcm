C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_init_fixed.F,v 1.1 2005/04/29 18:47:02 dimitri Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

CBOP
C     !ROUTINE: KPP_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE KPP_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KPP_INIT_FIXED
C     | o Routine to initialize GM/Redi variables 
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

C     !LOCAL VARIABLES:
C     === Local variables ===

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL KPP_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
