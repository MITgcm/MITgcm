C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_ini_vars.F,v 1.5 2004/10/10 06:08:48 edhill Exp $
C $Name:  $

#include "CD_CODE_OPTIONS.h"

      SUBROUTINE CD_CODE_INI_VARS( myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CD_CODE_VARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec
CEOP

#ifdef ALLOW_CD_CODE

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           uNM1(I,J,K,bi,bj)=0. _d 0
           vNM1(I,J,K,bi,bj)=0. _d 0
           uVeld(I,J,K,bi,bj)=0. _d 0
           vVeld(I,J,K,bi,bj)=0. _d 0
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etaNm1(I,J,bi,bj)=0. _d 0
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      IF ( nIter0.NE.0 ) THEN
        CALL CD_CODE_READ_CHECKPOINT( nIter0, myThid )
      ENDIF

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
