C $Header: /u/gcmpack/MITgcm/model/src/ini_model_io.F,v 1.2 2004/07/02 17:43:04 jmc Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_MODEL_IO
C     !INTERFACE:
      SUBROUTINE INI_MODEL_IO( myThid )

C     !DESCRIPTION: \bv
C     ini_model_io() is where run-time/experiment specific data are passed
C     to any I/O packages ready that will be used for I/O of model state
C     variables.
C     This is specifically for setting up once only information such as
C     shape/size of variables, units, etc... and is only for state variables.
C     \ev

C     !CALLING SEQUENCE:
C     PACKAGES_INIT_FIXED
C       |
C       |-- MNC_INIT

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid
CEOP


C     Flags specific to RW and MDSIO
C     Set globalFiles flag for READ_WRITE_FLD package
      CALL SET_WRITE_GLOBAL_FLD( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
      CALL SET_WRITE_GLOBAL_REC( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
      CALL SET_WRITE_GLOBAL_PICKUP( globalFiles )

#ifdef ALLOW_MNC
      IF (useMNC) THEN

C     Define coordinates for all MNC files ---------------------------------
C     XC
      CALL MNC_CW_ADD_VNAME('XC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XC',1,'description',
     &     'X coordinate of cell center (T-P point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XC',1,
     &     'units', 'degree_east', myThid)

C     YC
      CALL MNC_CW_ADD_VNAME('YC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YC',1,'description',
     &     'Y coordinate of cell center (T-P point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YC',1,
     &     'units', 'degree_north', myThid)

C     XU
      CALL MNC_CW_ADD_VNAME('XU', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XU',1,'description',
     &     'X coordinate of U point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XU',1,
     &     'units', 'degree_east', myThid)

C     YU
      CALL MNC_CW_ADD_VNAME('YU', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YU',1,'description',
     &     'Y coordinate of U point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YU',1,
     &     'units', 'degree_north', myThid)

C     XV
      CALL MNC_CW_ADD_VNAME('XV', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XV',1,'description',
     &     'X coordinate of V point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XV',1,
     &     'units', 'degree_east', myThid)

C     YV
      CALL MNC_CW_ADD_VNAME('YV', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YV',1,'description',
     &     'Y coordinate of V point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YV',1,
     &     'units', 'degree_north', myThid)

C     XG
      CALL MNC_CW_ADD_VNAME('XG', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XG',1,'description',
     &     'X coordinate of cell corner (Vorticity point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XG',1,
     &     'units', 'degree_east', myThid)

C     YG
      CALL MNC_CW_ADD_VNAME('YG', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YG',1,'description',
     &     'Y coordinate of cell corner (Vorticity point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YG',1,
     &     'units', 'degree_north', myThid)

C     RC
      CALL MNC_CW_ADD_VNAME('RC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RC',1,'description',
     &     'R coordinate of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RC',1,
     &     'units', 'm', myThid)

C     RF
      CALL MNC_CW_ADD_VNAME('RF', '-_-_--__I__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RF',1,'description',
     &     'R coordinate of cell interface',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RF',1,
     &     'units', 'm', myThid)
C     End define coordinates for all MNC files ----------------------------


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

      CALL MNC_CW_ADD_VNAME('gUnm1', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gVnm1', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gTnm1', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gSnm1', 'Cen_xy_Hn__C__t', 4,5, myThid)

      CALL MNC_CW_ADD_VNAME('Eta', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta',1,'long_name',
     &     'free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta',1,'units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('EtaH', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH',1,'long_name',
     &     'column-thickness_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH',1,'units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('W', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W',1,'units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W',1,
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('phiHyd', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('phiHydLow', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('phi_nh', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gW', 'Cen_xy_Hn__C__t', 4,5, myThid)

      CALL MNC_CW_ADD_VNAME('tr1', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('tr1',1,
     &     'long_name','passive_tracer_1', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',1,
     &     'coordinates','XC YC RC iter', myThid)

C     Write coordinates to "state" file
      CALL MNC_CW_SET_UDIM('state', 0, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'XC',xC, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'YC',yC, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'XU',xG, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'YU',yC, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'XV',xC, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'YV',yG, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'XG',xG, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'YG',yG, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'RC',rC, myThid)
      CALL MNC_CW_RS_W('R','state',0,0,'RF',rF, myThid)

      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
