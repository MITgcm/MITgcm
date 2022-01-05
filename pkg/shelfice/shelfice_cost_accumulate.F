#include "SHELFICE_OPTIONS.h"

      subroutine shelfice_cost_accumulate( myThid )
C     *==========================================================*
C     | subroutine shelfice_cost_accumulate                      |
C     | o accumulate mean state for cost evalualtion             |
C     *==========================================================*
C     |                                                          |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SHELFICE.h"
#include "SHELFICE_COST.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST
C     == Local variables
      integer bi, bj
      integer i, j

C--   Calculate cost function on tile of this instance
      do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
        do j=1,sNy
         do i=1,sNx
          cMeanSHIforT(i,j,bi,bj) = cMeanSHIforT(i,j,bi,bj)
     &         + shelficeHeatFlux(i,j,bi,bj)*deltaTClock
          cMeanSHIforS(i,j,bi,bj) = cMeanSHIforS(i,j,bi,bj)
     &         + shelficeFreshWaterFlux(i,j,bi,bj)*deltaTClock
         enddo
        enddo
       enddo
      enddo

#endif /* ALLOW_COST */

      RETURN
      END
