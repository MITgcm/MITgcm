C $Header: /u/gcmpack/MITgcm/pkg/timeave/Attic/timeave_init.F,v 1.1 2004/09/10 12:19:31 edhill Exp $
C $Name:  $
#include "TIMEAVE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: TIMEAVE_INIT

C     !INTERFACE:
      SUBROUTINE TIMEAVE_INIT( myThid )

C     !DESCRIPTION:
C     At the end of average period, write the time-average
C     state-variables on file ; then reset for next period

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC        
#include "MNC_PARAMS.h"
#endif
#include "DYNVARS.h"
#include "GRID.h"
#include "TIMEAVE_STATV.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_TIMEAVE        

#ifdef ALLOW_MNC        
      IF (useMNC) THEN

      CALL MNC_CW_ADD_VNAME('ETAtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETAtave',1,'long_name',
     &     'time-averaged free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETAtave',1,'units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETAtave',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('ETA2tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETA2tave',1,'long_name',
     &     'time-averaged free-surface_r-anomaly squared', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETA2tave',1,'units','m^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('ETA2tave',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('Ttave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ttave',1,'units','degC', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ttave',1,'long_name',
     &     'time-averaged potential_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ttave',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('Stave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Stave',1,'long_name',
     &     'time-averaged salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Stave',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('uVeltave', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('uVeltave',1,'units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('uVeltave',1,
     &     'coordinates','XU YU RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('vVeltave', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('vVeltave',1,'units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('vVeltave',1,
     &     'coordinates','XV YV RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('wVeltave', 'Cen_xy_Hn__C__t',4,5,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('wVeltave',1,'units','m/s', myThid)

      CALL MNC_CW_ADD_VNAME('UTtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('UTtave',1,'units','degC*m/s',myThid)

      CALL MNC_CW_ADD_VNAME('VTtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VTtave',1,'units','degC*m/s',myThid)

      CALL MNC_CW_ADD_VNAME('WTtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('WTtave',1,'units','degC*m/s',myThid)

      CALL MNC_CW_ADD_VNAME('UStave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('VStave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('WStave', 'Cen_xy_Hn__C__t', 4,5, myThid)

      CALL MNC_CW_ADD_VNAME('TTtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('UUtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('VVtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('UVtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('KEtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('Tdiftave', 'Cen_xy_Hn__C__t',4,5,myThid)

      CALL MNC_CW_ADD_VNAME('uZtave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('vZtave', 'Cen_xy_Hn__C__t', 4,5, myThid)

      CALL MNC_CW_ADD_VNAME('PhHytave', 'Cen_xy_Hn__C__t',4,5,myThid)

      CALL MNC_CW_ADD_VNAME('PHLtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('PHL2tave', 'Cen_xy_Hn__-__t',3,4,myThid)

      CALL MNC_CW_ADD_VNAME('Convtave', 'Cen_xy_Hn__C__t',4,5,myThid)

      CALL MNC_CW_ADD_VNAME('uFluxtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('vFluxtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('tFluxtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('sFluxtave', 'Cen_xy_Hn__-__t',3,4,myThid)

      CALL MNC_CW_ADD_VNAME('hUtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('hVtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('hFacCtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('hFacWtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('hFacStave', 'Cen_xy_Hn__-__t',3,4,myThid)

      ENDIF
#endif /* ALLOW_MNC */

#endif /* ALLOW_TIMEAVE */

      RETURN
      END
