C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_diagnostics_init.F,v 1.3 2006/05/25 18:32:55 heimbach Exp $
C $Name:  $

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
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'EXFhs   '
      diagTitle = 'Sensible heat flux into ocean, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFhl   '
      diagTitle = 'Latent heat flux into ocean, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFlwnet'
      diagTitle = 'Net upward longwave radiation, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )
      
      diagName  = 'EXFswnet'
      diagTitle = 'Net upward shortwave radiation, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )
      
      diagName  = 'EXFlwdn '
      diagTitle = 'Downward longwave radiation, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )
      
      diagName  = 'EXFswdn '
      diagTitle = 'Downward shortwave radiation, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFqnet '
      diagTitle = 'Net upward heat flux (turb+rad), >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )
      
C--   surface fluxes:
      diagName  = 'EXFtaux '
      diagTitle = 'zonal surface wind stress, >0 increases uVel    '
      diagUnits = 'N/m^2           '
      diagCode  = 'SU      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFtauy '
      diagTitle = 'meridional surface wind stress, >0 increases vVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'SV      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFuwind'
      diagTitle = 'zonal 10-m wind speed, >0 increases uVel'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFvwind'
      diagTitle = 'meridional 10-m wind speed, >0 increases uVel'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFwspee'
      diagTitle = '10-m wind speed modulus ( >= 0 )'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFatemp'
      diagTitle = 'surface (2-m) air temperature'
      diagUnits = 'degK            '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFaqh  '
      diagTitle = 'surface (2-m) specific humidity'
      diagUnits = 'kg/kg           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFevap '
      diagTitle = 'evaporation, > 0 increases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFpreci'
      diagTitle = 'evaporation, > 0 decreases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFsnow '
      diagTitle = 'snow precipitation, > 0 decreases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFempmr'
      diagTitle = 'net upward freshwater flux, > 0 increases salinity'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EXFpress'
      diagTitle = 'atmospheric pressure field'
      diagUnits = 'N/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )


#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
