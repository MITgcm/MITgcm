C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_mnc_init.F,v 1.4 2007/07/31 23:01:58 ce107 Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIC_MNC_INIT

C     !INTERFACE:
      SUBROUTINE DIC_MNC_INIT( myThid )

C     !DESCRIPTION:
C     Initialize MNC for DIC

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
C     myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC
#ifdef ALLOW_MNC

      IF ( useMNC ) THEN

      CALL MNC_CW_ADD_VNAME(
     &     'dic_BIO_ave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_BIO_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_BIO_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_CAR_ave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_CAR_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_CAR_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_SUR_ave', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_SUR_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_SUR_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_SURO_ave', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_SURO_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_SURO_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_pH_ave', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_pH_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_pH_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_pCO2_ave', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_pCO2_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_pCO2_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_fluxCO2_ave', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_fluxCO2_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_fluxCO2_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_OmegaC_ave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_OmegaC_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_OmegaC_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_pflux_ave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_pflux_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_pflux_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_epflux_ave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_epflux_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_epflux_ave','description',
     &     '', myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'dic_cflux_ave', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT(
     &     'dic_cflux_ave','units','--', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dic_cflux_ave','description',
     &     '', myThid)


      ENDIF

#endif /* ALLOW_MNC */
#endif /* ALLOW_DIC */

      RETURN
      END
