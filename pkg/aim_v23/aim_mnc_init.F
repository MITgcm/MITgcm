C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_mnc_init.F,v 1.5 2010/01/31 17:58:05 jmc Exp $
C $Name:  $

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
#ifdef ALLOW_MNC
#include "MNC_SIZE.h"
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC
C     == Local variables ==
      CHARACTER*55 ln_var
      INTEGER i,j, ndim
      INTEGER CW_MAX_LOC
      PARAMETER ( CW_MAX_LOC = 6 )
      CHARACTER*(MNC_MAX_CHAR) dn(CW_MAX_LOC)
      INTEGER dim(CW_MAX_LOC), ib(CW_MAX_LOC), ie(CW_MAX_LOC)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


      CALL MNC_CW_ADD_VNAME('USTRtave', 'U_xy_Hn__-__t',3,4,myThid)
      ln_var='u-stress'
      CALL MNC_CW_ADD_VATTR_TEXT('USTRtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('USTRtave','units','Pa',myThid)

      CALL MNC_CW_ADD_VNAME('VSTRtave', 'V_xy_Hn__-__t',3,4,myThid)
      ln_var='v-stress'
      CALL MNC_CW_ADD_VATTR_TEXT('VSTRtave','long_name',ln_var, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VSTRtave','units','Pa', myThid)

      CALL MNC_CW_ADD_VNAME('TSRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='top-of-atm. shortwave radiation'
      CALL MNC_CW_ADD_VATTR_TEXT('TSRtave','long_name',ln_var, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('TSRtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('OLRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='outgoing longwave radiation'
      CALL MNC_CW_ADD_VATTR_TEXT('OLRtave','long_name',ln_var, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('OLRtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('SSRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='surface shortwave radiation'
      CALL MNC_CW_ADD_VATTR_TEXT('SSRtave','long_name',ln_var, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SSRtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('SLRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='surface longwave radiation'
      CALL MNC_CW_ADD_VATTR_TEXT('SLRtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SLRtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('SHFtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='sensible heat flux'
      CALL MNC_CW_ADD_VATTR_TEXT('SHFtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SHFtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('EVAPtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='evaporation'
      CALL MNC_CW_ADD_VATTR_TEXT('EVAPtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EVAPtave','units','g/(m2 s)',myThid)

      CALL MNC_CW_ADD_VNAME('PRECNVtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='convective precipitation'
      CALL MNC_CW_ADD_VATTR_TEXT('PRECNVtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('PRECNVtave','units','g/(m2 s)',myThid)

      CALL MNC_CW_ADD_VNAME('PRECLStave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='large-scale precipitation'
      CALL MNC_CW_ADD_VATTR_TEXT('PRECLStave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('PRECLStave','units','g/(m2 s)',myThid)

      CALL MNC_CW_ADD_VNAME('CLOUDCtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='total cloud cover'
      CALL MNC_CW_ADD_VATTR_TEXT('CLOUDCtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CLOUDCtave','units','fraction',myThid)

      CALL MNC_CW_ADD_VNAME('CLTOPtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='normalized pressure at cloud top'
      CALL MNC_CW_ADD_VATTR_TEXT('CLTOPtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CLTOPtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('CBMFtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='cloud-base mass flux'
      CALL MNC_CW_ADD_VATTR_TEXT('CBMFtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CBMFtave','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('DRAGtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='surface Drag term (= Cd*Rho*|V|) (land+sea combined)'
      CALL MNC_CW_ADD_VATTR_TEXT('DRAGtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('DRAGtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('aimV0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='surface wind speed'
      CALL MNC_CW_ADD_VATTR_TEXT('aimV0tave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimV0tave','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aimT0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='surface air absolute temp.'
      CALL MNC_CW_ADD_VATTR_TEXT('aimT0tave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimT0tave','units','K',myThid)

      CALL MNC_CW_ADD_VNAME('aimQ0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='surface air spec. humidity'
      CALL MNC_CW_ADD_VATTR_TEXT('aimQ0tave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimQ0tave','units','g/kg',myThid)

      CALL MNC_CW_ADD_VNAME('EnFxPrtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='energy flux associated with precip. (snow, rain temp)'
      CALL MNC_CW_ADD_VATTR_TEXT('EnFxPrtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EnFxPrtave','units','W/m2', myThid)

      CALL MNC_CW_ADD_VNAME('albedotave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='surface albedo'
      CALL MNC_CW_ADD_VATTR_TEXT('albedotave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('albedotave','units','fraction',myThid)

      CALL MNC_CW_ADD_VNAME('dTsurftave', 'Cen_xy_Hn__-__t',3,4,myThid)
      ln_var='surf. Temp change from 1 iter to the next one (>0)'
      CALL MNC_CW_ADD_VATTR_TEXT('dTsurftave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dTsurftave','units','K', myThid)

      CALL MNC_CW_ADD_VNAME('aimRHtave', 'Cen_xy_Hn__C__t',4,5,myThid)
      ln_var='Relative Humidity'
      CALL MNC_CW_ADD_VATTR_TEXT('aimRHtave','long_name',ln_var,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimRHtave','units','fraction',myThid)

C     Non-halo-ed variables
      DO i = 1,CW_MAX_LOC
        DO j = 1,MNC_MAX_CHAR
          dn(i)(j:j) = ' '
        ENDDO
        dim(i) = 0
        ib(i) = 0
        ie(i) = 0
      ENDDO

      ndim = 1
      dn(ndim)(1:1) = 'X'
      dim(ndim) = sNx
      ib(ndim)  = 1
      ie(ndim)  = sNx
      ndim = ndim + 1
      dn(ndim)(1:1) = 'Y'
      dim(ndim) = sNy
      ib(ndim)  = 1
      ie(ndim)  = sNy
      ndim = ndim + 1
      dn(ndim)(1:1) = 'T'
      dim(ndim) = -1
      ib(ndim)  = 1
      ie(ndim)  = 1
      CALL MNC_CW_ADD_GNAME('AIM_xyt',  ndim, dim,dn,ib,ie, myThid)
      ndim = 3
      dn(ndim)(1:4) = 'Zaim'
      dim(ndim) = NLEV
      ib(ndim)  = 1
      ie(ndim)  = NLEV
      ndim = ndim + 1
      dn(ndim)(1:1) = 'T'
      dim(ndim) = -1
      ib(ndim)  = 1
      ie(ndim)  = 1
      CALL MNC_CW_ADD_GNAME('AIM_xyzt', ndim, dim,dn,ib,ie, myThid)


      CALL MNC_CW_ADD_VNAME('aim_DRAG0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_DRAG0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SPEED0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SPEED0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_TSR','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_TSR','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_OLR','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_OLR','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SSR0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SSR0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SLR0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SLR0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SHF0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SHF0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_EVAP0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_EVAP0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_PRECNV','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_PRECNV','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_PRECLS','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_PRECLS','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_CLOUDC','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_CLOUDC','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_CLTOP','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_CLTOP','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_CBMF','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_CBMF','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_T0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_T0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_Q0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_Q0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_EnPrec','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_EnPrec','units','J/g',myThid)

      CALL MNC_CW_ADD_VNAME('aim_alb10','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_alb10','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_dTsurf1','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_dTsurf1','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_TSWclr','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_TSWclr','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_OLWclr','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_OLWclr','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SSWclr','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SSWclr','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SLWclr','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SLWclr','units','---',myThid)

#endif /* ALLOW_MNC */

      RETURN
      END
