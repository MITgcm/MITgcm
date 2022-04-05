#include "MYPACKAGE_OPTIONS.h"

CBOP
C !ROUTINE: MYPACKAGE_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE MYPACKAGE_CALC_RHS(
     I                     myTime, myIter, myThid )

C !DESCRIPTION:
C Calculate custom tendency terms outside k-loop in DO_OCEANIC_PHYS

C !USES: ===============================================================
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
C  put following include within #ifdef in case this file is copied to local
C  code dir but pkg/mypackage is not compiled (missing file MYPACKAGE.h).
c#ifdef ALLOW_MYPACKAGE
c# include "MYPACKAGE.h"
c#endif

C !INPUT PARAMETERS: ===================================================
C myTime    :: Current time in simulation
C myIter    :: Current time-step number
C myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================

#ifdef ALLOW_MYPACKAGE
C !LOCAL VARIABLES: ====================================================
C bi,bj     :: Tile indices
      INTEGER bi, bj
CEOP

C--   Loops on tile indices bi,bj
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Calculate custom tendency terms ...

C--   end bi,bj loops.
       ENDDO
      ENDDO

#endif /* ALLOW_MYPACKAGE */

      RETURN
      END
