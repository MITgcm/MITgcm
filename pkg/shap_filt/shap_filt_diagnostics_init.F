C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_diagnostics_init.F,v 1.2 2006/08/29 17:03:16 dfer Exp $
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
#include "EEPARAMS.h"
c #include "SIZE.h"
c #include "PARAMS.h"
c #include "SHAP_FILT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnostics ) THEN

C-    Shapiro Filter tendency:

        diagName  = 'SHAP_dT '
        diagTitle = 'Temperature Tendency due to Shapiro Filter'
        diagUnits = 'K/s             '
        diagCode  = 'SM      MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SHAP_dS '
        diagTitle = 'Specific Humidity Tendency due to Shapiro Filter'
        diagUnits = 'g/kg/s          '
        diagCode  = 'SM      MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SHAP_dU '
        diagTitle = 'Zonal Wind Tendency due to Shapiro Filter'
        diagUnits = 'm/s^2           '
        WRITE(diagCode,'(A,I3.3,A)') 'UU   ', diagNum+2  ,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SHAP_dV '
        diagTitle = 'Meridional Wind Tendency due to Shapiro Filter'
        diagUnits = 'm/s^2           '
        WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SHAP_dKE'
        diagTitle = 
     &  'Kinetic Energy Tendency due to Shapiro Filter (Mass Weighted)'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM      MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
