C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_mnc_init.F,v 1.2 2005/08/10 19:18:52 edhill Exp $
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

        CALL MNC_CW_ADD_VNAME('si_UWIND','U_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_UWIND','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_UWIND','description',
     &       'Surface (10m) zonal wind velocity', myThid)

        CALL MNC_CW_ADD_VNAME('si_VWIND','V_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_VWIND','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_VWIND','description',
     &       'Surface (10m) meridional wind velocity', myThid)

        CALL MNC_CW_ADD_VNAME('si_EVAP','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_EVAP','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_EVAP','description',
     &       'Evaporation', myThid)

        CALL MNC_CW_ADD_VNAME('si_PRECIP','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_PRECIP','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_PRECIP','description',
     &       'Precipitation', myThid)

        CALL MNC_CW_ADD_VNAME('si_RUNOFF','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_RUNOFF','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_RUNOFF','description',
     &       'River and glacier runoff', myThid)

        CALL MNC_CW_ADD_VNAME('si_ATEMP','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_ATEMP','units','deg K', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_ATEMP','description',
     &       'Surface (2m) air temperature', myThid)

        CALL MNC_CW_ADD_VNAME('si_AQH','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_AQH','units','kg/kg', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_AQH','description',
     &       'Surface (2m) specific humidity', myThid)

        CALL MNC_CW_ADD_VNAME('si_SWDOWN','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_SWDOWN','units','W/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_SWDOWN','description',
     &       'Downward shortwave radiation', myThid)

        CALL MNC_CW_ADD_VNAME('si_LWDOWN','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_LWDOWN','units','W/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_LWDOWN','description',
     &       'Downward longwave radiation', myThid)

C       Time averages
        CALL MNC_CW_ADD_VNAME('si_UICEtave','U_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_UICEtave','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_UICEtave','description',
     &       'Time-averaged ', myThid)

        CALL MNC_CW_ADD_VNAME('si_VICEtave','V_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_VICEtave','units','m/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_VICEtave','description',
     &       'Time-averaged ', myThid)

        CALL MNC_CW_ADD_VNAME('si_FUtave','U_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_FUtave','units','N/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_FUtave','description',
     &       'Time-averaged zonal surface wind stress', myThid)

        CALL MNC_CW_ADD_VNAME('si_FVtave','V_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_FVtave','units','N/m^2',myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_FVtave','description',
     &       'Time-averaged Meridional surface wind stress', myThid)

        CALL MNC_CW_ADD_VNAME(
     &       'si_EmPmRtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT(
     &       'si_EmPmRtave','units','m/s',myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_EmPmRtave','description',
     &       'Time-averaged net upward freshwater flux', myThid)

        CALL MNC_CW_ADD_VNAME(
     &       'si_QNETtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT(
     &       'si_QNETtave','units','W/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_QNETtave','description',
     &       'Time-averaged net upward surface heat flux', myThid)

        CALL MNC_CW_ADD_VNAME(
     &       'si_QSWtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT(
     &       'si_QSWtave','units','W/m^2',myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_QSWtave','description',
     &       'Time-averaged net upward shortwave radiation',myThid)

        CALL MNC_CW_ADD_VNAME(
     &       'si_HEFFtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT(
     &       'si_HEFFtave','units','m', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_HEFFtave','description',
     &       'Time-averaged effective ice thickness',myThid)

        CALL MNC_CW_ADD_VNAME(
     &       'si_AREAtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT(
     &       'si_AREAtave','units','m^2/m^2',myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('si_AREAtave','description',
     &       'Time-averaged fractional ice-covered area',myThid)

      ENDIF
#endif /*  ALLOW_MNC     */
#endif /*  ALLOW_SEAICE  */

      RETURN
      END

