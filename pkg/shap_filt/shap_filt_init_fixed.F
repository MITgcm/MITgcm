C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_init_fixed.F,v 1.1 2005/02/15 01:51:24 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHAP_FILT_INIT_FIXED
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
        CALL SHAP_FILT_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
