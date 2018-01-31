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
      _RL thetaRef
      _RL drLoc

      integer bi, bj
      integer i, j
c     integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
c     integer km1, kp1, klev, k

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
       do bi = itlo,ithi
ccc        do klev = 1, Nr
         do j=1,sNy
          do i=1,sNx
c--
         cMeanSHIforT(i,j,bi,bj) = cMeanSHIforT(i,j,bi,bj)
     &         + shelficeHeatFlux(i,j,bi,bj)*deltaTClock
         cMeanSHIforS(i,j,bi,bj) = cMeanSHIforS(i,j,bi,bj)
     &         + shelficeFreshWaterFlux(i,j,bi,bj)*deltaTClock
c--
          enddo
         enddo
ccc        enddo
       enddo
      enddo

#endif /* ALLOW_COST */

      RETURN
      END
