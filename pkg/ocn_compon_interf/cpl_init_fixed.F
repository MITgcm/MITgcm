#include "OCN_CPL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CPL_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE CPL_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Define MNC variables for use by the coupler.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC
      IF (useMNC) THEN

        CALL MNC_CW_ADD_VNAME('SLPtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SLPtave','long_name',
     &       'Atmospheric Sea-Level pressure', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SLPtave','units','N/m2', myThid)

        CALL MNC_CW_ADD_VNAME('HFtave', 'Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('HFtave','long_name',
     &       'Net surface heat-flux', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('HFtave','units','W/m2', myThid)

        CALL MNC_CW_ADD_VNAME('QSWtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('QSWtave','long_name',
     &       '', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('QSWtave','units','W/m2', myThid)

        CALL MNC_CW_ADD_VNAME('TXtave', 'Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('TXtave','long_name',
     &       'surface stress in X', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('TXtave','units','N/m2', myThid)

        CALL MNC_CW_ADD_VNAME('TYtave', 'Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('TYtave','long_name',
     &       'surface stress in Y', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('TYtave','units','N/m2', myThid)

        CALL MNC_CW_ADD_VNAME('FWtave', 'Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('FWtave','long_name',
     &       'Net fresh water flux', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('FWtave','note',
     &       'equals E-P-R', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('FWtave','units','kg/m^2/s', myThid)

        CALL MNC_CW_ADD_VNAME('SFxtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SFxtave','long_name',
     &       'salt flux from sea-ice', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SFxtave','units','g/m^2/s',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('SICtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SICtave','long_name',
     &       'sea-ice mass', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SICtave','units','kg/m^2',myThid)

        CALL MNC_CW_ADD_VNAME('MXLtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MXLtave','long_name',
     &       'Ocean mixed-layer depth', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MXLtave','units','m', myThid)

        CALL MNC_CW_ADD_VNAME('SSTtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SSTtave','long_name',
     &       'ocean surface temperature', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SSTtave','units','degC', myThid)

        CALL MNC_CW_ADD_VNAME('SSStave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SSStave','long_name',
     &       'ocean surface salinity', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SSStave','units','g/kg', myThid)

        CALL MNC_CW_ADD_VNAME('vSqtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('vSqtave','long_name',
     &       'ocean surface velocity square', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('vSqtave','units','m^2/s^2',myThid)

        CALL MNC_CW_ADD_VNAME('aCO2tave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('aCO2tave','long_name',
     &       'atm CO2 level', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('aCO2tave','units','parts',myThid)

        CALL MNC_CW_ADD_VNAME('sWSpdtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('sWSpdtave','long_name',
     &       'surface wind speed', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('sWSpdtave','units','m/s',myThid)

        CALL MNC_CW_ADD_VNAME('iceftave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('iceftave','long_name',
     &       'ocean seaice fraction', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('iceftave','units','non-dim',myThid)

        CALL MNC_CW_ADD_VNAME('fCO2tave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('fCO2tave','long_name',
     &       'ocean CO2 flux -> atm', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('fCO2tave','units','mol/m2/s',myThid)

      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL CPL_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
