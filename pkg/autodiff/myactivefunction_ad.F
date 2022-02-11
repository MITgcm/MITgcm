#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !INTERFACE:
      SUBROUTINE admyActiveFunction(
     &     mytmp3d, myThid, admytmp3d, admyactivetmp )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      _RL     mytmp3d(sNx,sNy,Nr,nSx,nSy)
      _RL     admytmp3d(sNx,sNy,Nr,nSx,nSy)
      _RL     admyactivetmp
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

cph   Do nothing and nothing will be done

      RETURN
      END
