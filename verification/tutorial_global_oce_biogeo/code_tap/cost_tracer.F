#include "COST_OPTIONS.h"

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
#ifdef ALLOW_COST_TRACER
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS_PARAMS.h"
#  include "PTRACERS_FIELDS.h"
# endif
#endif

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
#ifdef ALLOW_PTRACERS
            locfc = locfc + maskC(i,j,k,bi,bj)*
     &           ptracer(i,j,k,bi,bj,6)*
     &           rA(i,j,bi,bj)*drF(k)
#endif
         ENDDO
      ENDDO

      objf_tracer(bi,bj) = objf_tracer(bi,bj) + locfc

#ifdef ALLOW_PTRACERS
      print *, 'COST TRACER nach', objf_tracer(bi,bj),
     &     ptracer(83,33,1,1,1,1), ptracer(83,33,2,1,1,1)
#endif

#endif /* ALLOW_COST_TRACER */

      RETURN
      END
