C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_diagnostics_init.F,v 1.4 2013/02/06 23:52:19 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHAP_FILT_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for SHAP_FILT package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     ! FUNCTIONS:
      CHARACTER*16 DIAGS_MK_UNITS
      EXTERNAL     DIAGS_MK_UNITS

C     !LOCAL VARIABLES:
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagMate  :: diag. mate number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
      CHARACTER*10  trUnits

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnostics ) THEN

C-    Shapiro Filter tendency:

        diagName  = 'SHAP_dT '
        diagTitle = 'Temperature Tendency due to Shapiro Filter'
        diagUnits = 'K/s             '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        trUnits = 'kg/kg'
        IF ( fluidIsWater .OR. useAIM ) trUnits = 'g/kg'
        diagName  = 'SHAP_dS '
        diagTitle = 'Specific Humidity Tendency due to Shapiro Filter'
        diagUnits =  DIAGS_MK_UNITS( trUnits//'/s', myThid )
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SHAP_dU '
        diagTitle = 'Zonal Wind Tendency due to Shapiro Filter'
        diagUnits = 'm/s^2           '
        diagCode  = 'UUR     MR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'SHAP_dV '
        diagTitle = 'Meridional Wind Tendency due to Shapiro Filter'
        diagUnits = 'm/s^2           '
        diagCode  = 'VVR     MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'SHAP_dKE'
        diagTitle =
     &  'Kinetic Energy Tendency due to Shapiro Filter (Mass Weighted)'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SMr     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
