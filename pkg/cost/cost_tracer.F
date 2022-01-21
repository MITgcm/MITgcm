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
#ifdef ALLOW_PTRACERS
C     == Local variables
      _RL locfc
      integer i, j, k

      locfc = 0. _d 0
      k=1
      DO j=1,sNy
         DO i=1,sNx
            locfc = locfc + hFacC(i,j,k,bi,bj)*
     &           lambdaTr1ClimRelax*ptracer(i,j,k,bi,bj,1)*
     &           rA(i,j,bi,bj)*drF(k)*dTtracerLev(k)
         ENDDO
      ENDDO

      objf_tracer(bi,bj) = objf_tracer(bi,bj) + locfc

#endif /* ALLOW_PTRACERS */
#endif /* ALLOW_COST_TRACER */

      RETURN
      END
