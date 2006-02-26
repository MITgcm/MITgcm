C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_init_fixed.F,v 1.2 2006/02/26 19:51:54 jmc Exp $
C $Name:  $

#include "CD_CODE_OPTIONS.h"

      SUBROUTINE CD_CODE_INIT_FIXED( myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
CEOP

#ifdef ALLOW_CD_CODE

#ifdef ALLOW_MNC
      IF (useMNC) THEN
C       Define grid types for CD_CODE variables
        CALL MNC_CW_ADD_VNAME('uVelD', 'Cen_xy_Hn__C__t',
     &       4,5, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('uVelD','units','m/s', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('uVelD','long_name',
     &       'CD_CODE_u_velocity', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('uVelD',
     &       'coordinates','XC YC RC iter', myThid)
        
        CALL MNC_CW_ADD_VNAME('vVelD', 'Cen_xy_Hn__C__t',
     &       4,5, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('vVelD','units','m/s', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('vVelD','long_name',
     &       'CD_CODE_v_velocity', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('vVelD',
     &       'coordinates','XC YC RC iter', myThid)
        
        CALL MNC_CW_ADD_VNAME('Unm1', 'Cen_xy_Hn__C__t',
     &       4,5, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Unm1','units','m/s', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Unm1','long_name',
     &       'CD_CODE_u_velocity_at_n-1', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Unm1',
     &       'coordinates','XC YC RC iter', myThid)
        
        CALL MNC_CW_ADD_VNAME('Vnm1', 'Cen_xy_Hn__C__t',
     &       4,5, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Vnm1','units','m/s', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Vnm1','long_name',
     &       'CD_CODE_u_velocity_at_n-1', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Vnm1',
     &       'coordinates','XC YC RC iter', myThid)
        
        CALL MNC_CW_ADD_VNAME('etaNm1', 'Cen_xy_Hn__-__t',
     &       3,4, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('etaNm1','units','m', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('etaNm1','long_name',
     &       'CD_CODE_ssh_at_n-1', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('etaNm1',
     &       'coordinates','XC YC RC iter', myThid)
      ENDIF
#endif /* ALLOW_MNC */

#endif /*  ALLOW_CD_CODE  */

      RETURN
      END
