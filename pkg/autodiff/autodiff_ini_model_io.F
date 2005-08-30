C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_ini_model_io.F,v 1.8 2005/08/30 22:28:50 heimbach Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: AUTODIFF_INI_MODEL_IO
C     !INTERFACE:
      SUBROUTINE AUTODIFF_INI_MODEL_IO( myThid )

C     !DESCRIPTION: \bv
C     autodiff_ini_model_io() is where run-time/experiment specific data are
C     passed to any I/O packages ready that will be used for I/O of model state
C     variables.
C     This is specifically for setting up once only information such as
C     shape/size of variables, units, etc... and is only for state variables.
C     \ev

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
#include "adcommon.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid
CEOP


#if (defined (ALLOW_MNC) && defined (ALLOW_AUTODIFF_MONITOR))

      IF (useMNC) THEN

      CALL MNC_CW_ADD_VNAME('adU', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU','long_name',
     &     'adjoint zonal velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU',
     &     'coordinates','XU YU RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adV', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV','long_name',
     &     'adjoint merid. velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV',
     &     'coordinates','XV YV RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adT', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT','units','[cost]/[degC]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT','long_name',
     &     'adjoint potential_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adS', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS','units','[cost]/[psu]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS','long_name',
     &     'adjoint salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adEta', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta','units','[cost]/[m]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta','long_name',
     &     'adjoint free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adW', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW','long_name',
     &     'adjoint vertical velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adQnet', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adQnet',
     &     'units','[cost]/[W/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adQnet','long_name',
     &     'adjoint net upward heat flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adQnet',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adEmpmr', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEmpmr',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEmpmr','long_name',
     &     'adjoint net upward freshwater flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEmpmr',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME(    'adFu', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFu',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFu','long_name',
     &     'adjoint zonal wind stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFu',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME(    'adFv', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFv',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFv','long_name',
     &     'adjoint zonal meridional stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFv',
     &     'coordinates','XC YC RC iter', myThid)

#ifdef ALLOW_SST0_CONTROL
      CALL MNC_CW_ADD_VNAME(    'adSST', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST',
     &     'units','[cost]/[degC]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST','long_name',
     &     'adjoint sea_surface_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST',
     &     'coordinates','XC YC RC iter', myThid)
#endif

#ifdef ALLOW_SSS0_CONTROL
      CALL MNC_CW_ADD_VNAME(    'adSSS', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS',
     &     'units','[cost]/[psu]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS','long_name',
     &     'adjoint sea_surface_salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS',
     &     'coordinates','XC YC RC iter', myThid)
#endif

#ifdef ALLOW_EXF
c
      CALL MNC_CW_ADD_VNAME('adustress', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adustress',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adustress','long_name',
     &     'adjoint zonal wind stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adustress',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('advstress', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advstress',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advstress','long_name',
     &     'adjoint meridional wind stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advstress',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adhflux', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhflux',
     &     'units','[cost]/[W/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhflux','long_name',
     &     'adjoint net upward heat flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhflux',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adsflux', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adsflux',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adsflux','long_name',
     &     'adjoint net upward freshwater flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adsflux',
     &     'coordinates','XC YC RC iter', myThid)
c
# ifdef ALLOW_ATM_TEMP
      CALL MNC_CW_ADD_VNAME('adatemp', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adatemp',
     &     'units','[cost]/[degK]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adatemp','long_name',
     &     'adjoint surface air temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adatemp',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adaqh', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adaqh',
     &     'units','[cost]/[kg/kg]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adaqh','long_name',
     &     'adjoint specific humidity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adaqh',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adprecip', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adprecip',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adprecip','long_name',
     &     'adjoint precipitation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adprecip',
     &     'coordinates','XC YC RC iter', myThid)
# endif

# ifdef ALLOW_ATM_WIND
      CALL MNC_CW_ADD_VNAME('aduwind', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduwind',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduwind','long_name',
     &     'adjoint zonal wind speed', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduwind',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('advwind', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advwind',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advwind','long_name',
     &     'adjoint meridional wind speed', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advwind',
     &     'coordinates','XC YC RC iter', myThid)
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
      CALL MNC_CW_ADD_VNAME('adswdown', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adswdown',
     &     'units','[cost]/[W/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adswdown','long_name',
     &     'adjoint downward shortwave radiation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adswdown',
     &     'coordinates','XC YC RC iter', myThid)
# endif
c
#endif

#ifdef ALLOW_SEAICE
c
      CALL MNC_CW_ADD_VNAME('adarea', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adarea',
     &     'units','[cost]/[m^2/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adarea','long_name',
     &     'adjoint fractional ice-covered area', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adarea',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adheff', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adheff',
     &     'units','[cost]/[m]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adheff','long_name',
     &     'adjoint effective ice thickness', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adheff',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adhsnow', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhsnow',
     &     'units','[cost]/[m]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhsnow','long_name',
     &     'adjoint snow thickness', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhsnow',
     &     'coordinates','XC YC RC iter', myThid)
c
# ifdef ALLOW_DYNAMICS
      CALL MNC_CW_ADD_VNAME('aduice', 'U_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduice',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduice','long_name',
     &     'adjoint zonal ice velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduice',
     &     'coordinates','XG YG RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('advice', 'V_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advice',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advice','long_name',
     &     'adjoint meridional ice velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advice',
     &     'coordinates','XG YG RC iter', myThid)
# endif
c
#endif

C     Write coordinates to "adstate" file
      CALL MNC_CW_SET_UDIM('adstate', 0, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'XC',xC, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'YC',yC, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'XU',xG, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'YU',yC, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'XV',xC, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'YV',yG, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'XG',xG, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'YG',yG, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'RC',rC, myThid)
      CALL MNC_CW_RS_W('R','adstate',0,0,'RF',rF, myThid)

      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
