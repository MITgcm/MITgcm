#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: EXF_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE EXF_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXF_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for EXF package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "EXF_PARAM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
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
#ifdef ALLOW_AUTODIFF
      INTEGER       diagMate
#endif
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'EXFhs   '
      diagTitle = 'Sensible heat flux into ocean, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFhl   '
      diagTitle = 'Latent heat flux into ocean, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFlwnet'
      diagTitle = 'Net upward longwave radiation, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFswnet'
      diagTitle = 'Net upward shortwave radiation, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFlwdn '
      diagTitle = 'Downward longwave radiation, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFswdn '
      diagTitle = 'Downward shortwave radiation, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFqnet '
      diagTitle = 'Net upward heat flux (turb+rad), >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C--   surface fluxes:
      diagName  = 'EXFtaux '
      diagTitle = 'zonal surface wind stress, >0 increases uVel    '
      diagUnits = 'N/m^2           '
      IF ( stressIsOnCgrid ) THEN
       diagCode = 'UU      U1      '
      ELSE
       diagCode = 'UM      U1      '
      ENDIF
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFtauy '
      diagTitle = 'meridional surface wind stress, >0 increases vVel'
      diagUnits = 'N/m^2           '
      IF ( stressIsOnCgrid ) THEN
       diagCode = 'VV      U1      '
      ELSE
       diagCode = 'VM      U1      '
      ENDIF
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFuwind'
      diagTitle = 'zonal 10-m wind speed, >0 eastward'
      diagUnits = 'm/s             '
      diagCode  = 'UM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFvwind'
      diagTitle = 'meridional 10-m wind speed, >0 northward'
      diagUnits = 'm/s             '
      diagCode  = 'VM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFurelw'
      diagTitle = 'zonal relative 10-m wind speed, >0 eastward'
      diagUnits = 'm/s             '
      diagCode  = 'UM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFvrelw'
      diagTitle = 'meridional relative 10-m wind speed, >0 northward'
      diagUnits = 'm/s             '
      diagCode  = 'VM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFwspee'
      diagTitle = '10-m wind speed modulus ( >= 0 )'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFatemp'
      diagTitle = 'surface (2-m) air temperature'
      diagUnits = 'degK            '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFaqh  '
      diagTitle = 'surface (2-m) specific humidity'
      diagUnits = 'kg/kg           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFevap '
      diagTitle = 'evaporation, > 0 increases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFpreci'
      diagTitle = 'precipitation, > 0 decreases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFsnow '
      diagTitle = 'snow precipitation, > 0 decreases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFempmr'
      diagTitle = 'net upward freshwater flux, > 0 increases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFpress'
      diagTitle = 'atmospheric pressure field'
      diagUnits = 'N/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFroff '
      diagTitle = 'river runoff, > 0 decreases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFroft '
      diagTitle = 'river runoff temperature'
      diagUnits = 'deg C           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EXFsalfx'
      diagTitle = 'upward salt flux, > 0 decreases salinity'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_AUTODIFF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Adjoint state variables
      diagName  = 'ADJustrs'
      diagTitle = 'dJ/dustress: Senstivity to zonal surface wind stress'
      diagUnits = 'dJ/(N/m^2)      '
      IF ( stressIsOnCgrid ) THEN
       diagCode = 'UU A    U1      '
      ELSE
       diagCode = 'UM A    U1      '
      ENDIF
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'ADJvstrs'
      diagTitle = 'dJ/dvstrs: Sensitivity to merid. surface wind stress'
      diagUnits = 'dJ/(N/m^2)      '
      IF ( stressIsOnCgrid ) THEN
       diagCode = 'VV A    U1      '
      ELSE
       diagCode = 'VM A    U1      '
      ENDIF
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'ADJhflux'
      diagTitle = 'dJ/dhflux: Sensitivity to upward heat flux'
      diagUnits = 'dJ/(W/m^2)      '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJsflux'
      diagTitle = 'dJ/dhflux: Sensitivity to upward fresh water flux'
      diagUnits = 'dJ/(m/s)        '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJatemp'
      diagTitle = 'dJ/datemp: Sensitivity to atmos. surface temperature'
      diagUnits = 'dJ/K            '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJpreci'
      diagTitle = 'dJ/daqh: Sensitivity to precipitation'
      diagUnits = 'dJ/(m/s)        '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJroff '
      diagTitle = 'dJ/daqh: Sensitivity to river runoff'
      diagUnits = 'dJ/(m/s)        '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJswdn '
      diagTitle = 'dJ/dswdown: Sensitivity to downward SW radiation'
      diagUnits = 'dJ/(W/m^2)      '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJlwdn '
      diagTitle = 'dJ/dlwdown: Sensitivity to downward LW radiation'
      diagUnits = 'dJ/(W/m^2)      '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJuwind'
      diagTitle = 'dJ/duwind: Senstivity to zonal 10-m wind speed'
      diagUnits = 'dJ/d(m/s)       '
      diagCode  = 'UM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJvwind'
      diagTitle = 'dJ/dvwind: Senstivity to meridional 10-m wind speed'
      diagUnits = 'dJ/d(m/s)       '
      diagCode  = 'VM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJclsst'
      diagTitle = 'dJ/dclimsst: Sensitivity to restoring SST'
      diagUnits = 'dJ/K            '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJclsss'
      diagTitle = 'dJ/dclimsss: Sensitivity to restoring SSS'
      diagUnits = 'dJ/(g/kg)       '
      diagCode  = 'SM A    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_AUTODIFF */
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
