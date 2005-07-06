C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_mnc_init.F,v 1.2 2005/07/06 04:32:24 enderton Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_MNC_INIT

C     !INTERFACE:
      SUBROUTINE AIM_MNC_INIT( myThid )

C     !DESCRIPTION: 
C     Initialize MNC variables for AIM.

C     !USES:
      IMPLICIT NONE
#include "AIM_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AIM_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC
      
      CALL MNC_CW_ADD_VNAME('USTRtave', 'U_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('USTRtave','long_name','u-stress',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('USTRtave','units','Pa',myThid)

      CALL MNC_CW_ADD_VNAME('VSTRtave', 'V_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VSTRtave','long_name','v-stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VSTRtave','units','Pa', myThid)

      CALL MNC_CW_ADD_VNAME('TSRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('TSRtave','long_name','top-of-atm. shortwave radiation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('TSRtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('OLRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('OLRtave','long_name','outgoing longwave radiation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('OLRtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('SSRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SSRtave','long_name','surface shortwave radiation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SSRtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('SLRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SLRtave','long_name','surface longwave radiation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SLRtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('SHFtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SHFtave','long_name','sensible heat flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SHFtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('EVAPtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EVAPtave','long_name','evaporation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EVAPtave','units','g/(m2 s)', myThid)

      CALL MNC_CW_ADD_VNAME('PRECNVtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('PRECNVtave','long_name','convective precipitation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('PRECNVtave','units','g/(m2 s)', myThid)

      CALL MNC_CW_ADD_VNAME('PRECLStave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('PRECLStave','long_name','large-scale precipitation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('PRECLStave','units','g/(m2 s)', myThid)

      CALL MNC_CW_ADD_VNAME('CLOUDCtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CLOUDCtave','long_name','total cloud cover', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CLOUDCtave','units','fraction', myThid)

      CALL MNC_CW_ADD_VNAME('CLTOPtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CLTOPtave','long_name','normalized pressure at cloud top', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CLTOPtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('CBMFtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CBMFtave','long_name','cloud-base mass flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CBMFtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('DRAGtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('DRAGtave','long_name','surface Drag term (= Cd*Rho*|V|) (land+sea combined)', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('DRAGtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('aimV0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimV0tave','long_name','surface wind speed', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimV0tave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('aimT0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimT0tave','long_name','surface air absolute temp.', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimT0tave','units','K', myThid)

      CALL MNC_CW_ADD_VNAME('aimQ0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimQ0tave','long_name','surface air spec. humidity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimQ0tave','units','g/kg', myThid)

      CALL MNC_CW_ADD_VNAME('EnFxPrtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EnFxPrtave','long_name','energy flux associated with precip. (snow, rain temp)', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EnFxPrtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('albedotave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('albedotave','long_name','surface albedo', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('albedotave','units','fraction', myThid)

      CALL MNC_CW_ADD_VNAME('dTsurftave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dTsurftave','long_name','surf. Temp change from 1 iter to the next one (>0)', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dTsurftave','units','K', myThid)

      CALL MNC_CW_ADD_VNAME('aimRHtave', 'Cen_xy_Hn__C__t',4,5,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimRHtave','long_name','Relative Humidity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimRHtave','units','fraction', myThid)

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
