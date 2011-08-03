C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_calc_rhs.F,v 1.2 2011/08/03 00:31:39 jmc Exp $
C $Name:  $

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
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
c#include "MYPACKAGE.h"

C !INPUT PARAMETERS: ===================================================
C myTime    :: Current time in simulation
C myIter    :: Current time-step number
C myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================

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

      RETURN
      END
