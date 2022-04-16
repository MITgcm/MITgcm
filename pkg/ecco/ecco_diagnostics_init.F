#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ECCO_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE ECCO_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid    :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      CHARACTER*(16) DIAGS_MK_UNITS
      EXTERNAL DIAGS_MK_UNITS

C     !LOCAL VARIABLES:
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagMate  :: diag. mate number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
c     INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
      CHARACTER*2    rUnit2c

      IF ( usingPCoords ) THEN
        rUnit2c= 'Pa'
      ELSE
        rUnit2c= 'm '
      ENDIF

      diagName  = 'SSH     '
      diagTitle = 'Dynamic Sea Surface Height Anomaly'
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I        diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SSHIBC  '
      diagTitle =
     I  'Inverted Barometer (IB) Correction to Sea Surface Height'
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I        diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SSHNOIBC'
      diagTitle = 'Sea Surface Height Anomaly without IB '
     I          //'Correction (=SSH+SSHIBC)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I        diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'STERICHT'
      diagTitle = 'Steric Sea Surface Height Anomaly (=SSH-OBP)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I        diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OBP     '
      diagTitle = 'Ocean Bottom Pressure'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I        diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OBPGMAP '
      diagTitle = 'Ocean Bottom Pressure '
     I          //'Including Global Mean Atmospheric Pressure'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I        diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
