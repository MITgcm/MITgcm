#include "WETDRY_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE WETDRY_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics for pkg/wetdry

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
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

        diagName  = 'WDEVLIM '
        diagTitle = 'Evaporation removed on dry/limited columns (EmPmR)'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'WDdryC  '
        diagTitle = 'wetdry dry-column flag (1=at film floor)'
        diagUnits = '1               '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'WDdepth '
        diagTitle = 'Total water column depth (eta + H)'
        diagUnits = 'm               '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C-    U/V pair registered last: diagMate = diagNum + 2 relies on diagNum
C     holding the number of the previously registered diagnostic
        diagName  = 'WDmaskW '
        diagTitle = 'wetdry face mask at U point (1=open, 0=closed)'
        diagUnits = '1               '
        diagCode  = 'UU      L1      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'WDmaskS '
        diagTitle = 'wetdry face mask at V point (1=open, 0=closed)'
        diagUnits = '1               '
        diagCode  = 'VV      L1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )


#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
