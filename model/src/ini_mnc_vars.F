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
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

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
      CALL MNC_CW_ADD_VNAME('xC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xC','description',
     &     'X coordinate of cell center (T-P point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xC',
     &     'units', 'degree_east', myThid)

C     YC
      CALL MNC_CW_ADD_VNAME('yC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yC','description',
     &     'Y coordinate of cell center (T-P point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yC',
     &     'units', 'degree_north', myThid)

C     XU
      CALL MNC_CW_ADD_VNAME('xU', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xU','description',
     &     'X coordinate of U point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xU',
     &     'units', 'degree_east', myThid)

C     YU
      CALL MNC_CW_ADD_VNAME('yU', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yU','description',
     &     'Y coordinate of U point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yU',
     &     'units', 'degree_north', myThid)

C     XV
      CALL MNC_CW_ADD_VNAME('xV', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xV','description',
     &     'X coordinate of V point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xV',
     &     'units', 'degree_east', myThid)

C     YV
      CALL MNC_CW_ADD_VNAME('yV', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yV','description',
     &     'Y coordinate of V point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yV',
     &     'units', 'degree_north', myThid)

C     XG
      CALL MNC_CW_ADD_VNAME('xG', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xG','description',
     &     'X coordinate of cell corner (Vorticity point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xG',
     &     'units', 'degree_east', myThid)

C     YG
      CALL MNC_CW_ADD_VNAME('yG', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yG','description',
     &     'Y coordinate of cell corner (Vorticity point)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yG',
     &     'units', 'degree_north', myThid)

C     RC
      CALL MNC_CW_ADD_VNAME('rC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rC','description',
     &     'R coordinate of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rC',
     &     'units', 'm', myThid)

C     RF
      CALL MNC_CW_ADD_VNAME('rF', '-_-_--__I__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rF','description',
     &     'R coordinate of cell interface',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rF',
     &     'units', 'm', myThid)

C     RL
      CALL MNC_CW_ADD_VNAME('rL', '-_-_--__L__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rL','description',
     &     'R coordinate of lower interface',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rL',
     &     'units', 'm', myThid)

C     RU
      CALL MNC_CW_ADD_VNAME('rU', '-_-_--__U__-', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rU','description',
     &     'R coordinate of upper interface',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rU',
     &     'units', 'm', myThid)
C     End define coordinates for all MNC files  ----------------------


C     Grid variables  ------------------------------------------------
      CALL MNC_CW_ADD_VNAME('drC', '-_-_--__I__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('drF', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('dxC', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyC', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxF', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyF', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxG', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyG', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxV', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyU', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAc',  'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAw', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAs', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAz', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('angleCS','Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('angleSN','Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('fCori', 'Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('fCoriG','Cor_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('rLowC', 'Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('rSurfC','Cen_xy_Hn__-__-',3,4,myThid)
      CALL MNC_CW_ADD_VNAME('Depth', 'Cen_xy_Hn__-__-', 3,4,myThid)
      CALL MNC_CW_ADD_VNAME('hFacC', 'Cen_xy_Hn__C__-', 4,5,myThid)
      CALL MNC_CW_ADD_VNAME('hFacW', 'U_xy_Hn__C__-', 4,5,myThid)
      CALL MNC_CW_ADD_VNAME('hFacS', 'V_xy_Hn__C__-', 4,5,myThid)

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
      CALL MNC_CW_ADD_VATTR_TEXT('rAc','description',
     &     'r-face area at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAw','description',
     &     'r-face area at U point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAs','description',
     &     'r-face area at V point',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAz','description',
     &     'r-face area at cell corner',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('angleCS','description',
     &     'Cos of grid orientation angle at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('angleSN','description',
     &       'Sin of grid orientation angle at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fCori','description',
     &     'Coriolis f at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fCoriG','description',
     &     'Coriolis f at cell corner',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rLowC','description',
     &     'base of fluid in r-units',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rSurfC','description',
     &     'surface reference (at rest) position',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Depth','description',
     &     'fluid thickness in r coordinates (at rest)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('hFacC','description',
     &     'vertical fraction of open cell at cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('hFacW','description',
     &       'vertical fraction of open cell at West face', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('hFacS','description',
     &     'vertical fraction of open cell at South face',myThid)


C     Create MNC definitions for DYNVARS.h variables  ----------------
      CALL MNC_CW_ADD_VNAME('T', '-_-_--__-__t', 0,0, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('T',
     &     'long_name','model_time', myThid)
#ifdef ALLOW_CAL
      IF ( useCAL ) THEN
       CALL CAL_GETDATE( -1, startTime, locDate, myThid )
C     format: seconds since 1992-01-01 00:00:00
       WRITE(timeUnits,'(A,I4.4,5(A,I2.2))')
     &      'seconds since ', locDate(1)/10000,       '-',
     &      (locDate(1) - locDate(1)/10000*10000)/100,'-',
     &      (locDate(1) - locDate(1)/100*100),        ' ',
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
     &     'coordinates','xU yU rC iter', myThid)

      CALL MNC_CW_ADD_VNAME('V', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('V','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('V',
     &     'coordinates','xV yV rC iter', myThid)

      CALL MNC_CW_ADD_VNAME('Temp', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Temp','units','degC', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Temp','long_name',
     &     'potential_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Temp',
     &     'coordinates','xC yC rC iter', myThid)

      CALL MNC_CW_ADD_VNAME('S', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('S','long_name',
     &     'salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('S',
     &     'coordinates','xC yC rC iter', myThid)

      CALL MNC_CW_ADD_VNAME('gUnm1', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gVnm1', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gTnm1', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gSnm1', 'Cen_xy_Hn__C__t', 4,5, myThid)

      CALL MNC_CW_ADD_VNAME('Eta', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta','long_name',
     &     'free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta','units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Eta',
     &     'coordinates','xC yC iter', myThid)

      CALL MNC_CW_ADD_VNAME('EtaH', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH','long_name',
     &     'column-thickness_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH','units','m', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EtaH',
     &     'coordinates','xC yC iter', myThid)
      CALL MNC_CW_ADD_VNAME('dEtaHdt', 'Cen_xy_Hn__-__t', 3,4, myThid)

      CALL MNC_CW_ADD_VNAME('W', 'Cen_xy_Hn__L__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W','units','m/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('W',
     &     'coordinates','xC yC rC iter', myThid)

      CALL MNC_CW_ADD_VNAME('phiHyd', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('phiHydLow', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('phi_nh', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gW',     'Cen_xy_Hn__L__t', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('gWnm1',  'Cen_xy_Hn__L__t', 4,5, myThid)

C     Write coordinates to "state" file
C     CALL MNC_CW_SET_UDIM('state', 0, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'xC',xC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'yC',yC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'xU',xG, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'yU',yC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'xV',xC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'yV',yG, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'xG',xG, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'yG',yG, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'RC',rC, myThid)
C     CALL MNC_CW_RS_W('R','state',0,0,'rF',rF, myThid)

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

      CALL MNC_CW_ADD_VNAME('EmPmR', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EmPmR','units','kg/m^2/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EmPmR','description',
     &     'Net upward freshwater flux', myThid)

      CALL MNC_CW_ADD_VNAME('saltFlux', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('saltFlux','units','g/m^2/s', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('saltFlux','description',
     &     'Net upward salt flux', myThid)

      CALL MNC_CW_ADD_VNAME('SST', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SST','units','deg C', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SST','description',
     &     ' Sea surface temperature for relaxation', myThid)

      CALL MNC_CW_ADD_VNAME('SSS', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SSS','units','g/kg', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SSS','description',
     &     'Sea surface salinity for relaxation', myThid)


      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_MNC */

      RETURN
      END
