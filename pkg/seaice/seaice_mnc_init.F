C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_mnc_init.F,v 1.1 2005/04/03 05:16:44 edhill Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_MNC_INIT
      
C     !INTERFACE:
      SUBROUTINE SEAICE_MNC_INIT( myThid )
      
C     !DESCRIPTION:
C     Initialize SEAICE variables within MNC

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_SEAICE
#ifdef ALLOW_MNC
      IF (useMNC) THEN

        CALL MNC_CW_ADD_VNAME('UWIND', 'U_xy_Hn__-__t', 3,4, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('UWIND','units','m/s', myThid)
                CALL MNC_CW_ADD_VATTR_TEXT('UWIND','description',
     &       'Surface (10m) zonal wind velocity', myThid)

        CALL MNC_CW_ADD_VNAME('VWIND', 'V_xy_Hn__-__t', 3,4, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('VWIND','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('VWIND','description',
     &       'Surface (10m) meridional wind velocity', myThid)

        CALL MNC_CW_ADD_VNAME('EVAP', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('EVAP','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('EVAP','description',
     &       'Evaporation', myThid)

        CALL MNC_CW_ADD_VNAME('PRECIP', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('PRECIP','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('PRECIP','description',
     &       'Precipitation', myThid)

        CALL MNC_CW_ADD_VNAME('RUNOFF', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('RUNOFF','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('RUNOFF','description',
     &       'River and glacier runoff', myThid)

        CALL MNC_CW_ADD_VNAME('ATEMP', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('ATEMP','units','deg K', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('ATEMP','description',
     &       'Surface (2m) air temperature', myThid)

        CALL MNC_CW_ADD_VNAME('AQH', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('AQH','units','kg/kg', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('AQH','description',
     &       'Surface (2m) specific humidity', myThid)

        CALL MNC_CW_ADD_VNAME('SWDOWN', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SWDOWN','units','W/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('SWDOWN','description',
     &       'Downward shortwave radiation', myThid)

        CALL MNC_CW_ADD_VNAME('LWDOWN', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('LWDOWN','units','W/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('LWDOWN','description',
     &       'Downward longwave radiation', myThid)

C       Time averages
        CALL MNC_CW_ADD_VNAME('UICEtave', 'U_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('UICEtave','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('UICEtave','description',
     &       'Time-averaged ', myThid)

        CALL MNC_CW_ADD_VNAME('VICEtave', 'V_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('VICEtave','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('VICEtave','description',
     &       'Time-averaged ', myThid)

        CALL MNC_CW_ADD_VNAME('FUtave', 'U_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('FUtave','units','N/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('FUtave','description',
     &       'Time-averaged zonal surface wind stress', myThid)

        CALL MNC_CW_ADD_VNAME('FVtave', 'V_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('FVtave','units','N/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('FVtave','description',
     &       'Time-averaged Meridional surface wind stress', myThid)

        CALL MNC_CW_ADD_VNAME('EmPmRtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('EmPmRtave','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('EmPmRtave','description',
     &       'Time-averaged net upward freshwater flux', myThid)

        CALL MNC_CW_ADD_VNAME('QNETtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('QNETtave','units','W/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('QNETtave','description',
     &       'Time-averaged net upward surface heat flux', myThid)

        CALL MNC_CW_ADD_VNAME('QSWtave', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('QSWtave','units','W/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('QSWtave','description',
     &       'Time-averaged net upward shortwave radiation', myThid)

        CALL MNC_CW_ADD_VNAME('HEFFtave', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('HEFFtave','units','m', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('HEFFtave','description',
     &       'Time-averaged effective ice thickness', myThid)

        CALL MNC_CW_ADD_VNAME('AREAtave', 'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('AREAtave','units','m^2/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('AREAtave','description',
     &       'Time-averaged fractional ice-covered area', myThid)

      ENDIF
#endif /*  ALLOW_MNC     */
#endif /*  ALLOW_SEAICE  */

      RETURN
      END

