#include "SHELFICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SHELFICE_MNC_INIT
      
C     !INTERFACE:
      SUBROUTINE SHELFICE_MNC_INIT( myThid )
      
C     !DESCRIPTION:
C     Initialize SHELFICE variables within MNC

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_SHELFICE
#ifdef ALLOW_MNC
      IF (useMNC) THEN

        CALL MNC_CW_ADD_VNAME('fwFlux','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('fwFlux','units','kg/m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('fwFlux','description',
     &       'Net fresh water flux into shelfice',myThid)

        CALL MNC_CW_ADD_VNAME('heatFlux','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('heatFlux','units','W/m^2',myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('heatFlux','description',
     &       'Net heat flux into shelfice',myThid)

C       Time averages

        CALL MNC_CW_ADD_VNAME(
     &       'fwFluxtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT(
     &       'fwFluxtave','units','kg/m^2/s',myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('fwFluxtave','description',
     &       'Time-averaged net freshwater flux into shelfice', myThid)

        CALL MNC_CW_ADD_VNAME(
     &       'heatFluxtave','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT(
     &       'heatFluxtave','units','W/m^2', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('heatFluxtave','description',
     &       'Time-averaged net heat flux into shelfice', myThid)

      ENDIF
#endif /*  ALLOW_MNC     */
#endif /*  ALLOW_SHELFICE  */

      RETURN
      END

