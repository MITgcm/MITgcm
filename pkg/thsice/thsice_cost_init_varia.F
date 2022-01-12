#include "THSICE_OPTIONS.h"

      SUBROUTINE THSICE_COST_INIT_VARIA( myThid )

c     ==================================================================
c     SUBROUTINE THSICE_COST_INIT_VARIA
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#ifdef ALLOW_COST
# include "THSICE_COST.h"
#endif

c     == routine arguments ==
      INTEGER myThid

#ifdef ALLOW_COST
c     == local variables ==
      INTEGER bi,bj

c     == end of interface ==

c--   Initialize the tiled cost function contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         objf_thsice(bi,bj) = 0. _d 0
         num_thsice(bi,bj)  = 0. _d 0
       ENDDO
      ENDDO

#endif /* ALLOW_COST */

      RETURN
      END
