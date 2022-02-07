#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !INTERFACE:
       _RL FUNCTION myActiveFunction ( mytmp3d, myThid )
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
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

      myActiveFunction = 1.0

      RETURN
      END
