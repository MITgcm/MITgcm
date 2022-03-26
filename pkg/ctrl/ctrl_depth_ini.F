#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_DEPTH_INI

C     !INTERFACE:
      SUBROUTINE CTRL_DEPTH_INI( mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE ctrl_depth_ini
c     | Add the depth part of the control vector to the model state
c     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#ifdef ALLOW_GENARR2D_CONTROL
# include "CTRL_GENARR.h"
#endif

C     !INPUT PARAMETERS:
      integer mythid

C     !OUTPUT PARAMETERS:

#ifdef ALLOW_DEPTH_CONTROL
C     !FUNCTIONS:

C     !LOCAL VARIABLES:

      INTEGER bi,bj
      INTEGER i,j
#ifdef ALLOW_GENARR2D_CONTROL
      INTEGER iarr
      INTEGER igen
#endif
      CHARACTER*(MAX_LEN_MBUF) msgbuf
CEOP

      WRITE(msgBuf,'(A,A)') 'CTRL_DEPTH_INI: ctrl update R_low, ',
     &     'adding the control vector.'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C     In contrast to other control variable starting with xx_, this
C     variable not an increment, but a full replacement of R_low, see
C     also GRID.h. This is necessary because TAF gets confused if we do
C     not do it like this.
      DO bj=myByLo(mythid),myByHi(mythid)
       DO bi=myBxLo(mythid),myBxHi(mythid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          xx_r_low(i,j,bi,bj) = R_low(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#ifdef ALLOW_GENARR2D_CONTROL
C--   use generic 2D control variable infrastructure to set xx_r_low
      igen = 0
      DO iarr = 1, maxCtrlArr2D
       IF ( xx_genarr2d_weight(iarr).NE.' ' .AND.
     &      xx_genarr2d_file(iarr)(1:8).EQ.'xx_depth') igen = iarr
      ENDDO
      IF ( igen .GT. 0 ) THEN
       CALL CTRL_MAP_GENARR2D( xx_r_low, igen, myThid )
      ENDIF
#else
      STOP 'ALLOW_DEPTH_CONTROL only works with ALLOW_GENARR2D_CONTROL'
#endif /* ALLOW_GENARR2D_CONTROL */
#endif /* ALLOW_DEPTH_CONTROL */

      return
      end
