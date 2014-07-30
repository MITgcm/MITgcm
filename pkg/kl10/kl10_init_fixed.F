C $Header: /u/gcmpack/MITgcm/pkg/kl10/kl10_init_fixed.F,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#include "KL10_OPTIONS.h"

CBOP
C     !ROUTINE: KL10_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE KL10_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KL10_INIT_FIXED
C     | o Routine to initialize KL10 variables
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

C     !LOCAL VARIABLES :
C     === Local variables ===
C
C JMK: not sure what this is.
CML#ifdef ALLOW_MNC
CML      IF (useMNC) THEN
CMLC       Define grid types for Mellor-Yamada (MY) variables
CML        CALL MNC_CW_ADD_VNAME('KLviscAr', 'Cen_xy_Hn__C__t',
CML     &       4,5, myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('KLviscAr','units','m^2/s',
CML     &       myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('KLviscAr','long_name',
CML     &       'MY_vertical_eddy_viscosity_coefficient', myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('KLviscAr',
CML     &       'coordinates','XC YC RC iter', myThid)
CML
CML        CALL MNC_CW_ADD_VNAME('KLdiffKr', 'Cen_xy_Hn__C__t',
CML     &       4,5, myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('KLdiffKr','units','m^2/s',
CML     &       myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('KLdiffKr','long_name',
CML     &       'MY_tracer_vertical_diffusion_coefficient',
CML     &       myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('KLdiffKr',
CML     &       'coordinates','XC YC RC iter', myThid)
CML#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL KL10_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
