C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_mnc_io.F,v 1.1 2004/02/27 03:50:01 edhill Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

      SUBROUTINE INI_MNC_IO( myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

      INTEGER myThid

#ifdef ALLOW_MNC

C     Write the grid information
      CALL MNC_CW_WRITE_GRID_INFO(myThid, 'grid')

C     Create MNC definitions for DYNVARS.h variables
      CALL MNC_CW_ADD_VNAME(myThid, 'iter', '-_-_--__-__t', 0,0)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'iter',1,
     &     'long_name','iteration_count')

      CALL MNC_CW_ADD_VNAME(myThid, 'U', 'U_xy_Hn__C__t', 4,5)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'U',1,'units','m/s')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'U',1,
     &     'coordinates','XU YU RC iter')

      CALL MNC_CW_ADD_VNAME(myThid, 'V', 'V_xy_Hn__C__t', 4,5)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'V',1,'units','m/s')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'V',1,
     &     'coordinates','XV YV RC iter')

      CALL MNC_CW_ADD_VNAME(myThid, 'T', 'Cen_xy_Hn__C__t', 4,5)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'T',1,'units','degC')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'T',1,'long_name',
     &     'potential_temperature')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'T',1,
     &     'coordinates','XCen YCen RC iter')

      CALL MNC_CW_ADD_VNAME(myThid, 'S', 'Cen_xy_Hn__C__t', 4,5)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'S',1,'long_name',
     &     'salinity')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'S',1,
     &     'coordinates','XCen YCen RC iter')

      CALL MNC_CW_ADD_VNAME(myThid, 'Eta', 'Cen_xy_Hn__-__t', 3,4)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'Eta',1,'long_name',
     &     'free-surface_r-anomaly')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'Eta',1,
     &     'coordinates','XCen YCen RC iter')

      CALL MNC_CW_ADD_VNAME(myThid, 'W', 'Cen_xy_Hn__C__t', 4,5)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'W',1,'units','m/s')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'W',1,
     &     'coordinates','XCen YCen RC iter')

      CALL MNC_CW_ADD_VNAME(myThid, 'totPhiHyd', 'Cen_xy_Hn__C__t', 4,5)
      CALL MNC_CW_ADD_VNAME(myThid, 'phiHydLow', 'Cen_xy_Hn__-__t', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'phi_nh', 'Cen_xy_Hn__C__t', 4,5)

      CALL MNC_CW_ADD_VNAME(myThid, 'tr1', 'Cen_xy_Hn__C__t', 4,5)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'tr1',1,
     &     'long_name','passive_tracer_1')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'T',1,
     &     'coordinates','XCen YCen RC iter')

C     Add coordinates
      CALL MNC_CW_ADD_VNAME(myThid, 'XCen', 'Cen_xy_Hn__-__-', 3,4)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'XCen',1,
     &     'units', 'degree_east')
      CALL MNC_CW_ADD_VNAME(myThid, 'XU', 'U_xy_Hn__-__-', 3,4)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'XU',1,
     &     'units', 'degree_east')
      CALL MNC_CW_ADD_VNAME(myThid, 'XV', 'V_xy_Hn__-__-', 3,4)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'XV',1,
     &     'units', 'degree_east')

      CALL MNC_CW_ADD_VNAME(myThid, 'YCen', 'Cen_xy_Hn__-__-', 3,4)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'YCen',1,
     &     'units', 'degree_north')
      CALL MNC_CW_ADD_VNAME(myThid, 'YU', 'U_xy_Hn__-__-', 3,4)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'YU',1,
     &     'units', 'degree_north')
      CALL MNC_CW_ADD_VNAME(myThid, 'YV', 'V_xy_Hn__-__-', 3,4)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'YV',1,
     &     'units', 'degree_north')

      CALL MNC_CW_ADD_VNAME(myThid, 'RC', '-_-_--__C__-', 0,0)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'RC',1,
     &     'units', 'm')
      CALL MNC_CW_ADD_VNAME(myThid, 'RI', '-_-_--__I__-', 0,0)
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'RI',1,
     &     'units', 'm')

      CALL MNC_CW_RS_W_R(myThid,'state',0,0,'XCen',0,xC)
      CALL MNC_CW_RS_W_R(myThid,'state',0,0,'XU',0,xG)
      CALL MNC_CW_RS_W_R(myThid,'state',0,0,'XV',0,xG)

      CALL MNC_CW_RS_W_R(myThid,'state',0,0,'YCen',0,yC)
      CALL MNC_CW_RS_W_R(myThid,'state',0,0,'YU',0,yG)
      CALL MNC_CW_RS_W_R(myThid,'state',0,0,'YV',0,yG)

      CALL MNC_CW_RS_W_R(myThid,'state',0,0,'RC',0,rC)
      CALL MNC_CW_RS_W_R(myThid,'state',0,0,'RI',0,rF)

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
