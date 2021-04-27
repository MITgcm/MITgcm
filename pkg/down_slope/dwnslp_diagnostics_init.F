#include "DWNSLP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: DWNSLP_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE DWNSLP_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
c#include "DWNSLP_PARAMS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
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
      CHARACTER*4   diagSufx
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
      INTEGER  DIAGS_GET_PARMS_I
      EXTERNAL DIAGS_GET_PARMS_I
#ifdef ALLOW_GENERIC_ADVDIFF
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnotics ) THEN

        diagNum   = DIAGS_GET_PARMS_I( 'LAST_DIAG_ID', myThid )

c       diagName  = 'RHOAnExt'
c       diagTitle = 'Density Anomaly (=Rho-rhoConst) + extended inland'
c       diagUnits = 'kg/m^3          '
c       diagCode  = 'SMR     MR      '
c       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'DSLPuFlw'
        diagTitle = 'Zonal  Down-Sloping Transport (u-component)'
        diagUnits = 'm^3/s           '
        diagCode  = 'UU      M1      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'DSLPvFlw'
        diagTitle = 'Merid. Down-Sloping Transport (v-component)'
        diagUnits = 'm^3/s           '
        diagCode  = 'VV      M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagSufx  = 'aaaa'
#ifdef ALLOW_GENERIC_ADVDIFF
        diagSufx  = GAD_DIAG_SUFX( GAD_TEMPERATURE, myThid )
#endif
        diagName  = 'DSLP'//diagSufx
        diagTitle = 'Pot.Temp. tendency from Down-Slope package'
        diagUnits = 'K/s             '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_GENERIC_ADVDIFF
        diagSufx  = GAD_DIAG_SUFX( GAD_SALINITY, myThid )
#endif
        diagName  = 'DSLP'//diagSufx
        diagTitle = 'Salinity  tendency from Down-Slope package'
        diagUnits = 'g/kg/s          '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
