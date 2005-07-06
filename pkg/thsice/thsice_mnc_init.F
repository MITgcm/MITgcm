C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_mnc_init.F,v 1.3 2005/07/06 21:18:33 edhill Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_MNC_INIT

C     !INTERFACE:
      SUBROUTINE THSICE_MNC_INIT( myThid )

C     !DESCRIPTION: 
C     Initialize MNC variables for THSICE

C     !USES:
      IMPLICIT NONE
      
C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC

C     =====  Fields for the pickups and snapshot output =====
      CALL MNC_CW_ADD_VNAME('iceMask','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('iceMask','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('iceHeight','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('iceHeight','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('snowHeight','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('snowHeight','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('Tsrf','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Tsrf','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('Tice1','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Tice1','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('Tice2','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Tice2','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('Qice1','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Qice1','units','---', myThid)
      
      CALL MNC_CW_ADD_VNAME('Qice2','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Qice2','units','---', myThid)
      
      CALL MNC_CW_ADD_VNAME('snowAge','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('snowAge','units','---', myThid)
      
      CALL MNC_CW_ADD_VNAME('tOceMxL','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('tOceMxL','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('sOceMxL','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('sOceMxL','units','---', myThid)


C     =====  Fields for the tave output  =====
      CALL MNC_CW_ADD_VNAME('fract_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fract_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('iceH_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('iceH_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('snowH_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('snowH_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('Tsrf_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Tsrf','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('Tice1_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Tice1','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('Tice2_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Tice2','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('snowPr_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('snowPr_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('albedo_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('albedo_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('flx2oc_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('flx2oc_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('frw2oc_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('frw2oc_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('flxAtm_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('flxAtm_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('frwAtm_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('frwAtm_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('tMxL_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('tMxL_ave','units','---', myThid)

      CALL MNC_CW_ADD_VNAME('sMxL_ave','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('sMxL_ave','units','---', myThid)

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
