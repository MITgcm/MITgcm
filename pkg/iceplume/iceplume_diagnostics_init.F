#include "ICEPLUME_OPTIONS.h"

C !INTERFACE:
      SUBROUTINE ICEPLUME_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "ICEPLUME.h"



C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP
#ifdef ALLOW_ICEPLUME
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

      diagName  = 'icefrntW'
      diagTitle = 'plume vertical velocity'
      diagUnits = 'm/s        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'icefrntT'
      diagTitle = 'plume temperature'
      diagUnits = 'Deg. C           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'icefrntS'
      diagTitle = 'plume salinity'
      diagUnits = 'g/kg       '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'icefrntR'
      diagTitle = 'plume thickness'
      diagUnits = 'm           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'icefrntM'
      diagTitle = 'plume melt rate'
      diagUnits = 'm d^-1          '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'icefrntA'
      diagTitle = 'average cell melt rate'
      diagUnits = 'm d^-1         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
      diagName  = 'PlumAngl'
      diagTitle = 'angle of FW plume'
      diagUnits = 'nondim         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PlumDist'
      diagTitle = 'dist of FW plume from wall'
      diagUnits = 'm              '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

      diagName  = 'IP_gTbg '
      diagTitle = 'Temperature tend from iceplumeBG, >0 incr ocn T'
      diagUnits = 'degC/s        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'IP_gSbg '
      diagTitle = 'Salinity tend from iceplumeBG, >0 incr ocn S'
      diagUnits = 'g/kg/s       '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'IP_gTplm'
      diagTitle = 'Temperature tend from plume, >0 incr ocn T'
      diagUnits = 'degC/s        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'IP_gSplm'
      diagTitle = 'Salinity tend from plume, >0 incr ocn S'
      diagUnits = 'g/kg/s       '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'IPmasspl'
      diagTitle = 'addMass from plume, >0 incr ocn mass'
      diagUnits = 'kg/s        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'IPmassbg'
      diagTitle = 'addMass from iceplumeBG, >0 incr ocn mass'
      diagUnits = 'kg/s        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ICEPLUME */

      RETURN
      END
