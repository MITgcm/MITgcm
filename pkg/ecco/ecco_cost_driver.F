#include "ECCO_OPTIONS.h"

      subroutine ecco_cost_driver( myTime, myIter, myThid )

c     ==================================================================
c     SUBROUTINE ecco_cost_driver
c     ==================================================================
c
c     o Compute all ecco cost function contributions.
c
c     ==================================================================
c     SUBROUTINE ecco_cost_driver
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_TAPENADE
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==
      _RL     myTime
      integer myIter
      integer myThid

c--   Compute user defined cost function contributions (elaborate)
      call timer_start('cost_gencost_all    [ECCO SPIN-DOWN]', myThid)
      call cost_gencost_all( myIter, myTime, myThid )
      call timer_stop ('cost_gencost_all    [ECCO SPIN-DOWN]', myThid)

      RETURN
      END
