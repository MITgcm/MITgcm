C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_mnc_init.F,v 1.1 2005/06/23 20:02:50 edhill Exp $
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
      CALL MNC_CW_ADD_VATTR_TEXT('USTRtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('VSTRtave', 'V_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('VSTRtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('TSRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('TSRtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('OLRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('OLRtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('SSRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SSRtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('STRtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('STRtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('EVAPtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EVAPtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('SHFtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('SHFtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('PRECNVtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('PRECNVtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('PRECLStave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('PRECLStave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('CLOUDCtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CLOUDCtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('CLTOPtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CLTOPtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('CBMFtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('CBMFtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('DRAGtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('DRAGtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('aimV0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimV0tave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('aimT0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimT0tave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('aimQ0tave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimQ0tave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('EnFxPrtave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('EnFxPrtave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('albedotave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('albedotave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('dTsurftave', 'Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dTsurftave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('aimRHtave', 'Cen_xy_Hn__C__t',4,5,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aimRHtave','units','---', myThid)

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
