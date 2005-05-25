C $Header: /u/gcmpack/MITgcm/model/src/ini_model_io.F,v 1.19 2005/05/25 04:03:09 edhill Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_MODEL_IO

C     !INTERFACE:
      SUBROUTINE INI_MODEL_IO( myThid )

C     !DESCRIPTION: 
C     Pass specific setup data to any I/O packages necessary for I/O of
C     model state variables.  This is specifically for setting up (once
C     only!)  information such as shape/size of variables, units,
C     etc... and is primarily for state and snapshot variables.

C     !USES:
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
      CALL MNC_CW_ADD_VATTR_TEXT('XC','description',
     &     'X coordinate of cell center (T-P point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XC',
     &     'units', 'degree_east', myThid)

C     YC
      CALL MNC_CW_ADD_VNAME('YC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YC','description',
     &     'Y coordinate of cell center (T-P point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YC',
     &     'units', 'degree_north', myThid)

C     XU
      CALL MNC_CW_ADD_VNAME('XU', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XU','description',
     &     'X coordinate of U point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XU',
     &     'units', 'degree_east', myThid)

C     YU
      CALL MNC_CW_ADD_VNAME('YU', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YU','description',
     &     'Y coordinate of U point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YU',
     &     'units', 'degree_north', myThid)

C     XV
      CALL MNC_CW_ADD_VNAME('XV', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XV','description',
     &     'X coordinate of V point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XV',
     &     'units', 'degree_east', myThid)

C     YV
      CALL MNC_CW_ADD_VNAME('YV', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YV','description',
     &     'Y coordinate of V point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YV',
     &     'units', 'degree_north', myThid)

C     XG
      CALL MNC_CW_ADD_VNAME('XG', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XG','description',
     &     'X coordinate of cell corner (Vorticity point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('XG',
     &     'units', 'degree_east', myThid)

C     YG
      CALL MNC_CW_ADD_VNAME('YG', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YG','description',
     &     'Y coordinate of cell corner (Vorticity point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('YG',
     &     'units', 'degree_north', myThid)

C     RC
      CALL MNC_CW_ADD_VNAME('RC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RC','description',
     &     'R coordinate of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RC',
     &     'units', 'm', myThid)

C     RF
      CALL MNC_CW_ADD_VNAME('RF', '-_-_--__I__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RF','description',
     &     'R coordinate of cell interface',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RF',
     &     'units', 'm', myThid)

C     RL
      CALL MNC_CW_ADD_VNAME('RL', '-_-_--__L__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RL','description',
     &     'R coordinate of lower interface',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RL',
     &     'units', 'm', myThid)

C     RU
      CALL MNC_CW_ADD_VNAME('RU', '-_-_--__U__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RU','description',
     &     'R coordinate of upper interface',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('RU',
     &     'units', 'm', myThid)
C     End define coordinates for all MNC files ----------------------------


C     Create MNC definitions for DYNVARS.h variables
      CALL MNC_CW_ADD_VNAME('T', '-_-_--__-__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',
     &     'long_name','model_time', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',
     &     'units','s', myThid)

      CALL MNC_CW_ADD_VNAME('iter', '-_-_--__-__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('iter',
     &     'long_name','iteration_count', myThid)

      CALL MNC_CW_ADD_VNAME('model_time', '-_-_--__-__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('model_time',
     &     'long_name','Model Time', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('model_time','units','s', myThid)

      CALL MNC_CW_ADD_VNAME('U', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('U','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('U',
     &     'coordinates','XU YU RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('V', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('V','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('V',
     &     'coordinates','XV YV RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('Temp', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Temp','units','degC', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Temp','long_name',
     &     'potential_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Temp',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('S', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('S','long_name',
     &     'salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('S',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('gUnm1', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gVnm1', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gTnm1', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gSnm1', 'Cen_xy_Hn__C__t', 4,5, myThid)

      CALL MNC_CW_ADD_VNAME('Eta', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta','long_name',
     &     'free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta','units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta',
     &     'coordinates','XC YC iter', myThid)

      CALL MNC_CW_ADD_VNAME('EtaH', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH','long_name',
     &     'column-thickness_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH','units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH',
     &     'coordinates','XC YC iter', myThid)
      CALL MNC_CW_ADD_VNAME('dEtaHdt', 'Cen_xy_Hn__-__t', 3,4, myThid)

      CALL MNC_CW_ADD_VNAME('W', 'Cen_xy_Hn__L__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('phiHyd', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('phiHydLow', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('phi_nh', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gW', 'Cen_xy_Hn__L__t', 4,5, myThid)

C     Write coordinates to "state" file
C     CALL MNC_CW_SET_UDIM('state', 0, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'XC',xC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'YC',yC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'XU',xG, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'YU',yC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'XV',xC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'YV',yG, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'XG',xG, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'YG',yG, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'RC',rC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'RF',rF, myThid)

C     Define variables used in mom_vecinv
C     CALL MNC_CW_ADD_VNAME('fV', 'Cen_xy_Hn__-__t', 0,0, myThid)
C     CALL MNC_CW_ADD_VATTR_TEXT('','units','', myThid)
C     CALL MNC_CW_ADD_VATTR_TEXT('','long_name',
C     &     'potential_temperature', myThid)
      
      CALL MNC_CW_ADD_VNAME('fV', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('fU', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('zV', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('zU', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('KEx','Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('KEy','Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('Ds', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('Dt', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('Du', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('Dv', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('Z3', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('W3', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('KE', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('D',  'Cen_xy_Hn__C__t', 0,0, myThid)

C     Define variables from FFIELDS.h
      CALL MNC_CW_ADD_VNAME('fu', 'U_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fu','units','N/m^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fu','description',
     &     'Zonal surface wind stress', myThid)

      CALL MNC_CW_ADD_VNAME('fv', 'V_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fv','units','N/m^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fv','description',
     &     'Meridional surface wind stress', myThid)

      CALL MNC_CW_ADD_VNAME('Qnet', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Qnet','units','W/m^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Qnet','description',
     &     'Net upward surface heat flux (including shortwave)', 
     &     myThid)

      CALL MNC_CW_ADD_VNAME('Qsw', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Qsw','units','W/m^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Qsw','description',
     &     'Net upward shortwave radiation', myThid)

      CALL MNC_CW_ADD_VNAME('dQdt', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dQdt','units',
     &     'W/m^2/degrees', myThid)

      CALL MNC_CW_ADD_VNAME('EmPmR', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EmPmR','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EmPmR','description',
     &     'Net upward freshwater flux', myThid)

      CALL MNC_CW_ADD_VNAME('saltFlux', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('saltFlux','units',
     &     'psu.kg/m^2/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('saltFlux','description',
     &     'Net upward salt flux', myThid)

      CALL MNC_CW_ADD_VNAME('SST', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SST','units','deg C', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SST','description',
     &     ' Sea surface temperature for relaxation', myThid)

      CALL MNC_CW_ADD_VNAME('SSS', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SSS','units','psu', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SSS','description',
     &     'Sea surface salinity for relaxation', myThid)


#ifdef ALLOW_AUTODIFF_MONITOR
      CALL AUTODIFF_INI_MODEL_IO( myThid )
#endif

      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
