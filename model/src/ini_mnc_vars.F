C $Header: /u/gcmpack/MITgcm/model/src/ini_mnc_vars.F,v 1.10 2012/04/07 16:24:12 jmc Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_MNC_VARS

C     !INTERFACE:
      SUBROUTINE INI_MNC_VARS( myThid )

C     !DESCRIPTION:
C     Set up (once only) information such as shape/size of variables,
C     units, etc. primarily for state and snapshot variables.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC

C     !LOCAL VARIABLES:
#ifdef ALLOW_CAL
      INTEGER locDate(4)
      CHARACTER*(34) timeUnits
#endif /* ALLOW_CAL */

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
C     End define coordinates for all MNC files  ----------------------


C     Grid variables  ------------------------------------------------
      CALL MNC_CW_ADD_VNAME('drC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('drF', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('dxC', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyC', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxF', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyF', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxG', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyG', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxV', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyU', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rA',  'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAw', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAs', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAz', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('AngleCS','Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('AngleSN','Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('fCori', 'Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('fCoriG','Cor_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('R_low', 'Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('Ro_surf','Cen_xy_Hn__-__-',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('Depth', 'Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('HFacC', 'Cen_xy_Hn__C__-', 4,5,myThid)
      CALL MNC_CW_ADD_VNAME('HFacW', 'U_xy_Hn__C__-', 4,5,myThid)
      CALL MNC_CW_ADD_VNAME('HFacS', 'V_xy_Hn__C__-', 4,5,myThid)

      CALL MNC_CW_ADD_VATTR_TEXT('drC','description',
     &     'r cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('drF','description',
     &     'r cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxC','description',
     &     'x cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyC','description',
     &       'y cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxF','description',
     &     'x cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyF','description',
     &     'y cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxG','description',
     &     'x cell corner separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyG','description',
     &     'y cell corner separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxV','description',
     &     'x v-velocity separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyU','description',
     &     'y u-velocity separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rA','description',
     &     'r-face area at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAw','description',
     &     'r-face area at U point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAs','description',
     &     'r-face area at V point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAz','description',
     &     'r-face area at cell corner',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('AngleCS','description',
     &     'Cos of grid orientation angle at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('AngleSN','description',
     &       'Sin of grid orientation angle at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fCori','description',
     &     'Coriolis f at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fCoriG','description',
     &     'Coriolis f at cell corner',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('R_low','description',
     &     'base of fluid in r-units',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ro_surf','description',
     &     'surface reference (at rest) position',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Depth','description',
     &     'fluid thickness in r coordinates (at rest)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacC','description',
     &     'vertical fraction of open cell at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacW','description',
     &       'vertical fraction of open cell at West face', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacS','description',
     &     'vertical fraction of open cell at South face',myThid)


C     Create MNC definitions for DYNVARS.h variables  ----------------
      CALL MNC_CW_ADD_VNAME('T', '-_-_--__-__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',
     &     'long_name','model_time', myThid)
#ifdef ALLOW_CAL
      IF ( useCAL ) THEN
       CALL CAL_GETDATE( -1, startTime, locDate, myThid )
C     format: seconds since 1992-01-01, 00:00:00
       WRITE(timeUnits,'(A,I4.4,5(A,I2.2))')
     &      'seconds since ', locDate(1)/10000,       '-',
     &      (locDate(1) - locDate(1)/10000*10000)/100,'-',
     &      (locDate(1) - locDate(1)/100*100),        ', ',
     &       locDate(2)/10000,                        ':',
     &      (locDate(2) - locDate(2)/10000*10000)/100,':',
     &      (locDate(2) - locDate(2)/100*100)
       CALL MNC_CW_ADD_VATTR_TEXT('T',
     &      'units',timeUnits, myThid)
      ELSE
#else
      IF (.TRUE. ) THEN
#endif /* ALLOW_CAL */
       CALL MNC_CW_ADD_VATTR_TEXT('T',
     &     'units','s', myThid)
      ENDIF

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
      CALL MNC_CW_ADD_VNAME('gW',     'Cen_xy_Hn__L__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gWnm1',  'Cen_xy_Hn__L__t', 4,5, myThid)

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

      CALL MNC_CW_ADD_VNAME('fV', 'V_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fV','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fV','description',
     &     'Merid. Planetary Coriolis Acceleration (f x U)', myThid)

      CALL MNC_CW_ADD_VNAME('fU', 'U_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fU','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fU','description',
     &     'Zonal. Planetary Coriolis Acceleration (f x V)', myThid)

      CALL MNC_CW_ADD_VNAME('zV', 'V_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('zV','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('zV','description',
     &     'Merid. Relative Coriolis Acceleration (Zeta x U)', myThid)

      CALL MNC_CW_ADD_VNAME('zU', 'U_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('zU','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('zU','description',
     &     'Zonal. Relative Coriolis Acceleration (Zeta x V)', myThid)

      CALL MNC_CW_ADD_VNAME('KEx','U_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('KEx','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('KEx','description',
     &     'Kinetic Energy Gradient Zonal Acceleration', myThid)

      CALL MNC_CW_ADD_VNAME('KEy','V_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('KEx','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('KEx','description',
     &     'Kinetic Energy Gradient Merid. Acceleration', myThid)

      CALL MNC_CW_ADD_VNAME('Ds', 'Cor_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ds','units','1/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ds','description',
     &     'Horiz. Velocity Shear Strain Rate (u_y+v_x)', myThid)

      CALL MNC_CW_ADD_VNAME('Dt', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Dt','units','1/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Dt','description',
     &     'Horiz. Velocity Tension Rate (u_x-v_y)', myThid)

      CALL MNC_CW_ADD_VNAME('Du', 'U_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Du','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Du','description',
     &     'Explicit Viscous Zonal Acceleration', myThid)

      CALL MNC_CW_ADD_VNAME('Dv', 'V_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Dv','units','m/s^2', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Dv','description',
     &     'Explicit Viscous Merid. Acceleration', myThid)

      CALL MNC_CW_ADD_VNAME('Z3', 'Cor_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Z3','units','1/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Z3','description',
     &     'Vertical Relative Vorticity (v_x-u_y)', myThid)

      CALL MNC_CW_ADD_VNAME('W3', 'Cor_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W3','units','1/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W3','description',
     &     'Vertical Absolute Vorticity (v_x-u_y+f)', myThid)

      CALL MNC_CW_ADD_VNAME('KE', 'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('KE','units','1/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('KE','description',
     &     'Kinetic Energy of Horiz. Vel. (u^2+v^2)/2', myThid)

      CALL MNC_CW_ADD_VNAME('D',  'Cen_xy_Hn__C__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('D','units','1/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('D','description',
     &     'Divergence of Horiz. Vel. (u_x+v_y)', myThid)

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
      CALL MNC_CW_ADD_VATTR_TEXT('EmPmR','units','kg/m^2/s', myThid)
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


      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_MNC */

      RETURN
      END
