C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_mnc_io.F,v 1.8 2004/03/19 05:09:41 edhill Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

      SUBROUTINE INI_MNC_IO( myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      INTEGER myThid

#ifdef ALLOW_MNC
      IF (useMNC) THEN

C     Write the grid information
      CALL MNC_CW_WRITE_GRID_INFO('grid', myThid)

C     Create MNC definitions for DYNVARS.h variables
      CALL MNC_CW_ADD_VNAME('iter', '-_-_--__-__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('iter',1,
     &     'long_name','iteration_count', myThid)

      CALL MNC_CW_ADD_VNAME('model_time', '-_-_--__-__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('model_time',1,
     &     'long_name','Model Time', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('model_time',1,'units','s', myThid)

      CALL MNC_CW_ADD_VNAME('U', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('U',1,'units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('U',1,
     &     'coordinates','XU YU RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('V', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('V',1,'units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('V',1,
     &     'coordinates','XV YV RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('T', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',1,'units','degC', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',1,'long_name',
     &     'potential_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('S', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('S',1,'long_name',
     &     'salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('S',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('Unm1', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('Vnm1', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('Tnm1', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('Snm1', 'Cen_xy_Hn__C__t', 4,5, myThid)

      CALL MNC_CW_ADD_VNAME('Eta', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta',1,'long_name',
     &     'free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta',1,'units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('EtaNH', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaNH',1,'long_name',
     &     'NLFS_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaNH',1,'units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaNH',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('W', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W',1,'units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('totPhiHyd', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('phiHydLow', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('phi_nh', 'Cen_xy_Hn__C__t', 4,5, myThid)

      CALL MNC_CW_ADD_VNAME('tr1', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('tr1',1,
     &     'long_name','passive_tracer_1', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',1,
     &     'coordinates','XC YC RC iter', myThid)

C     Add coordinates
      CALL MNC_CW_ADD_VNAME('XC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XC',1,
     &     'units', 'degree_east', myThid)
      CALL MNC_CW_ADD_VNAME('XU', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XU',1,
     &     'units', 'degree_east', myThid)
      CALL MNC_CW_ADD_VNAME('XV', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XV',1,
     &     'units', 'degree_east', myThid)

      CALL MNC_CW_ADD_VNAME('YC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YC',1,
     &     'units', 'degree_north', myThid)
      CALL MNC_CW_ADD_VNAME('YU', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YU',1,
     &     'units', 'degree_north', myThid)
      CALL MNC_CW_ADD_VNAME('YV', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YV',1,
     &     'units', 'degree_north', myThid)

      CALL MNC_CW_ADD_VNAME('RC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RC',1,
     &     'units', 'm', myThid)
      CALL MNC_CW_ADD_VNAME('RF', '-_-_--__I__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RF',1,
     &     'units', 'm', myThid)

      CALL MNC_CW_RS_W_R('state',0,0,'XC',0,xC, myThid)
      CALL MNC_CW_RS_W_R('state',0,0,'XU',0,xG, myThid)
      CALL MNC_CW_RS_W_R('state',0,0,'XV',0,xG, myThid)

      CALL MNC_CW_RS_W_R('state',0,0,'YC',0,yC, myThid)
      CALL MNC_CW_RS_W_R('state',0,0,'YU',0,yG, myThid)
      CALL MNC_CW_RS_W_R('state',0,0,'YV',0,yG, myThid)

      CALL MNC_CW_RS_W_R('state',0,0,'RC',0,rC, myThid)
      CALL MNC_CW_RS_W_R('state',0,0,'RF',0,rF, myThid)

      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
