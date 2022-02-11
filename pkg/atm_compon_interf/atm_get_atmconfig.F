#include "ATM_CPL_OPTIONS.h"
#ifdef ALLOW_AIM
# include "AIM_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: ATM_GET_ATMCONFIG

C !INTERFACE:
      SUBROUTINE ATM_GET_ATMCONFIG(
     O                              landMask,
     I                              myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ATM_GET_ATMCONFIG
C     | o get atmos. config (land/sea mask) from other pkgs
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#ifdef ALLOW_AIM
# include "AIM_SIZE.h"
#else
# include "SIZE.h"
#endif

#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
c#include "ATMIDS.h"
c#include "ATMCPL.h"
#ifdef ALLOW_AIM
# include "AIM_FFIELDS.h"
#endif /* ALLOW_AIM */

C !INPUT/OUTPUT PARAMETERS:
C     landMask :: land / sea mask (=1 : full land; =0 : full ocean grid cell)
C     myThid   :: Thread number for this instance of the routine
      _RL landMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C !LOCAL VARIABLES:
      INTEGER i,j,bi,bj
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   Initialise to zero (no land, full oceanic):
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           landMask(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

        IF ( useAtm_Phys ) THEN
C-    Since pkg/atm_phys does not currently handle land (but only swamp),
C     set landMask to an intermediate value (here half) to (by)pass the check
C     in ATM_CHECK_CPLCONFIG :
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           landMask(i,j,bi,bj) = 0.5 _d 0
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_AIM
        IF ( useAIM ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           landMask(i,j,bi,bj) = aim_landFr(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_AIM */

       ENDDO
      ENDDO

      RETURN
      END
