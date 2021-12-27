#include "OPPS_OPTIONS.h"

      SUBROUTINE OPPS_INIT( myThid )
C     *==========================================================*
C     | SUBROUTINE OPPS_INIT                                     |
C     | o Routine to initialize OPPS parameters and variables.   |
C     *==========================================================*
C     | Initialize OPPS parameters and variables.                |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OPPS.h"

C     === Routine arguments ===
C     myThid :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES :
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic

#ifdef ALLOW_DIAGNOSTICS
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#endif
CEOP

C-----------------------------------------------------------------------
C Initialize OPPS variables
C-----------------------------------------------------------------------

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        diagName  = 'OPPScadj'
        diagTitle = 'OPPS Convective Adjustment Index [0-1] '
        diagUnits = 'fraction        '
        diagCode  = 'SMR     LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
