C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_tracer.F,v 1.1 2001/07/13 13:37:45 heimbach Exp $

#include "CPP_OPTIONS.h"

      subroutine cost_tracer( bi, bj, myThid )
C     /==========================================================\
C     | subroutine cost_tracer                                   |
C     | o this routine computes the cost function for the tiles  |
C     |   of this processor                                      |
C     |==========================================================|
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "TR1.h"

#include "cost.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer bi, bj
      integer myThid

#ifdef ALLOW_COST_TRACER
C     == Local variables
      _RL thetaRef
      _RL locfc

      integer i, j, k
      integer ig, jg
ce    some reference temperature
      thetaRef = 24.0D0

      locfc = 0. _d 0

      k=1
      DO j=1,sNy
         DO i=1,sNx
            locfc = locfc + maskC(i,j,k,bi,bj)*
     &           lambdaTr1ClimRelax*Tr1(i,j,k,bi,bj)*
     &           rA(i,j,bi,bj)*drF(k)*deltaTtracer
         ENDDO
      ENDDO

cph(
      print *, 'COST TRACER vor ', objf_tracer(bi,bj)
cph)

      objf_tracer(bi,bj) = objf_tracer(bi,bj) + locfc

cph(
      print *, 'COST TRACER nach', objf_tracer(bi,bj)
cph)

#endif /* ALLOW_COST_TRACER */

      RETURN
      END
