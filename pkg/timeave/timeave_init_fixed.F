#include "TIMEAVE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: TIMEAVE_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE TIMEAVE_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     At the end of average period, write the time-average
C     state-variables on file ; then reset for next period

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
c #include "GRID.h"
c #include "TIMEAVE_STATV.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_TIMEAVE

#ifdef ALLOW_MNC
      IF ( useMNC.AND.timeave_mnc ) THEN

      CALL MNC_CW_ADD_VNAME('ETAtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETAtave','long_name',
     &     'time-averaged free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETAtave','units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETAtave',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('Eta2tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta2tave','long_name',
     &     'time-averaged free-surface_r-anomaly squared', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta2tave','units','m^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta2tave',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('Ttave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ttave','units','degC', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ttave','long_name',
     &     'time-averaged potential_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ttave',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('Stave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Stave','long_name',
     &     'time-averaged salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Stave',
     &     'coordinates','XC YC RC iter', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Stave','description',
     &     'time-averaged salinity', myThid)

      CALL MNC_CW_ADD_VNAME('uVeltave', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('uVeltave','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('uVeltave',
     &     'coordinates','XU YU RC iter', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('uVeltave','description',
     &     'time-averaged zonal velocity', myThid)

      CALL MNC_CW_ADD_VNAME('vVeltave', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('vVeltave','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('vVeltave',
     &     'coordinates','XV YV RC iter', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('vVeltave','description',
     &     'time-averaged merid. velocity', myThid)

      CALL MNC_CW_ADD_VNAME('wVeltave', 'Cen_xy_Hn__L__t',4,5,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('wVeltave','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('vVeltave','description',
     &     'time-averaged vertical velocity', myThid)

      CALL MNC_CW_ADD_VNAME('UTtave', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UTtave','units','degC*m/s',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UTtave','description',
     &     'Zonal velocity and potential temp. t-ave product', myThid)

      CALL MNC_CW_ADD_VNAME('VTtave', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VTtave','units','degC*m/s',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VTtave','description',
     &     'Merid velocity and potential temp. t-ave product', myThid)

      CALL MNC_CW_ADD_VNAME('WTtave', 'Cen_xy_Hn__L__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('WTtave','units','degC*m/s',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('WTtave','description',
     &     'Vert. velocity and potential temp. t-ave product', myThid)

      CALL MNC_CW_ADD_VNAME('UStave', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UStave','units','g/kg*m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UStave','description',
     &     'Zonal velocity and Salinity t-ave product', myThid)

      CALL MNC_CW_ADD_VNAME('VStave', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VStave','units','g/kg*m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VStave','description',
     &     'Merid velocity and Salinity t-ave product', myThid)

      CALL MNC_CW_ADD_VNAME('WStave', 'Cen_xy_Hn__L__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('WStave','units','g/kg*m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('WStave','description',
     &     'Vert velocity and Salinity t-ave product', myThid)

      CALL MNC_CW_ADD_VNAME('TTtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('TTtave','units','degC^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('TTtave','description',
     &     'Potl Temp Squared t-ave', myThid)

      CALL MNC_CW_ADD_VNAME('UUtave', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UUtave','units','m^2/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UUtave','description',
     &     'Zonal Vel. Squared t-ave', myThid)

      CALL MNC_CW_ADD_VNAME('VVtave', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VVtave','units','m^2/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VVtave','description',
     &     'Merid Vel. Squared t-ave', myThid)

      CALL MNC_CW_ADD_VNAME('UVtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UVtave','units','m^2/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UVtave','description',
     &     'Horiz. Vel. Product t-ave', myThid)

      CALL MNC_CW_ADD_VNAME('KEtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('KEtave','units','m^2/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('KEtave','description',
     &     'Kinetic Energy t-ave', myThid)

      CALL MNC_CW_ADD_VNAME('Tdiftave', 'Cen_xy_Hn__C__t',4,5,myThid)

      CALL MNC_CW_ADD_VNAME('uZtave', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('uZtave','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('uZtave','description',
     &     'Zonal Vel. and Rel. Vorticity Product t-ave', myThid)

      CALL MNC_CW_ADD_VNAME('vZtave', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('vZtave','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('vZtave','description',
     &     'Merid Vel. and Rel. Vorticity Product t-ave', myThid)

      CALL MNC_CW_ADD_VNAME('PhHytave', 'Cen_xy_Hn__C__t',4,5,myThid)

      CALL MNC_CW_ADD_VNAME('PHLtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('PHL2tave', 'Cen_xy_Hn__-__t',3,4,myThid)

      CALL MNC_CW_ADD_VNAME('Convtave', 'Cen_xy_Hn__C__t',4,5,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Convtave','description',
     &     'Convection t-ave', myThid)

      CALL MNC_CW_ADD_VNAME('uFluxtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('vFluxtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('tFluxtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('sFluxtave', 'Cen_xy_Hn__-__t',3,4,myThid)

      CALL MNC_CW_ADD_VNAME('hUtave', 'U_xy_Hn__C__t',4,5,myThid)
      CALL MNC_CW_ADD_VNAME('hVtave', 'V_xy_Hn__C__t',4,5,myThid)
      CALL MNC_CW_ADD_VNAME('hFacCtave', 'Cen_xy_Hn__C__t',4,5,myThid)
      CALL MNC_CW_ADD_VNAME('hFacWtave', 'U_xy_Hn__C__t',4,5,myThid)
      CALL MNC_CW_ADD_VNAME('hFacStave', 'V_xy_Hn__C__t',4,5,myThid)

      ENDIF
#endif /* ALLOW_MNC */

#endif /* ALLOW_TIMEAVE */

      RETURN
      END
