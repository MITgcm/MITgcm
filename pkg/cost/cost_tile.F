C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_tile.F,v 1.1 2001/07/13 13:37:45 heimbach Exp $

#include "CPP_OPTIONS.h"

      subroutine cost_tile( myThid )
C     /==========================================================\
C     | subroutine cost_tile                                     |
C     | o this routine computes the cost function for the tiles  |
C     |   of this processor                                      |
C     |==========================================================|
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     \==========================================================/
      implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST

      integer bi, bj

C--   TAMC need a stmt in front of a HPF directive

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_COST_TRACER
          CALL COST_TRACER( bi, bj, myThid )
#endif

        ENDDO
      ENDDO

#endif /* ALLOW_COST */

      RETURN
      END
