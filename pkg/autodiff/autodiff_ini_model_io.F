C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_ini_model_io.F,v 1.3 2004/11/16 05:42:11 heimbach Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

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

C     Create MNC definitions for DYNVARS.h variables
c      CALL MNC_CW_ADD_VNAME('iter', '-_-_--__-__t', 0,0, myThid)
c      CALL MNC_CW_ADD_VATTR_TEXT('iter',
c     &     'long_name','iteration_count', myThid)

c      CALL MNC_CW_ADD_VNAME('model_time', '-_-_--__-__t', 0,0, myThid)
c      CALL MNC_CW_ADD_VATTR_TEXT('model_time',
c     &     'long_name','Model Time', myThid)
c      CALL MNC_CW_ADD_VATTR_TEXT('model_time','units','s', myThid)

      CALL MNC_CW_ADD_VNAME('adU', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU','long_name',
     &     'adj. zonal velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU',
     &     'coordinates','XU YU RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adV', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV','long_name',
     &     'adj. merid. velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV',
     &     'coordinates','XV YV RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adT', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT','units','[cost]/[degC]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT','long_name',
     &     'adj. potential_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adS', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS','units','[cost]/[psu]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS','long_name',
     &     'adj. salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adEta', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta','units','[cost]/[m]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta','long_name',
     &     'adj. free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adW', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW','long_name',
     &     'adj. vertical velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adSST', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST',
     &     'units','[cost]/[degC]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST','long_name',
     &     'adj. sea_surface_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adSSS', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS',
     &     'units','[cost]/[degC]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS','long_name',
     &     'adj. sea_surface_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS',
     &     'coordinates','XC YC RC iter', myThid)

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
