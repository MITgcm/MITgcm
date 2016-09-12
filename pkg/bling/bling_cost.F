C $Header: /u/gcmpack/MITgcm/pkg/bling/Attic/bling_cost.F,v 1.2 2016/09/12 20:00:28 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_COST (
     I           myTime,myIter,myThid)

C     =================================================================
C     | subroutine bling_cost
C     | o Define cost contribution from bling variables
C     =================================================================

      implicit none

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#include "BLING_COST.h"

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_BLING_COST

C     == Local variables ==
      INTEGER i, j, bi, bj, k, it
      _RL tmpcost
CEOP

c calculate costfunction

      tmpcost = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO i=1,sNx
          DO j=1,sNy
              tmpcost = tmpcost+FluxCO2(i,j,bi,bj)*rA(i,j,bi,bj)*
     &                        maskC(i,j,1,bi,bj)*dTtracerLev(1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_RL(tmpcost,myThid)

       totcost = totcost + tmpcost

       print *, 'QQ myiter, totcost = ', myiter, totcost

#endif /* ALLOW_BLING_COST */

      RETURN
      END
