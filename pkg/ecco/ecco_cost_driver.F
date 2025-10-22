#include "ECCO_OPTIONS.h"
#ifdef ALLOW_TAPENADE
# ifdef ALLOW_CTRL
#  include "CTRL_OPTIONS.h"
# endif
#endif

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
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_GENARR.h"
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==
      _RL     myTime
      integer myIter
      integer myThid

#ifdef ALLOW_OBCS_COST_CONTRIBUTION
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_obcs',myThid)
# endif
      call timer_start('cost_obcs          [ECCO SPIN-DOWN]', myThid)
      call cost_obcs( myTime, myIter, myThid )
      call timer_stop ('cost_obcs          [ECCO SPIN-DOWN]', myThid)
#endif /* ALLOW_OBCS_COST_CONTRIBUTION */

c--   Compute user defined cost function contributions (elaborate)
      call timer_start('cost_gencost_all    [ECCO SPIN-DOWN]', myThid)
      call cost_gencost_all( myIter, myTime, myThid )
      call timer_stop ('cost_gencost_all    [ECCO SPIN-DOWN]', myThid)

      RETURN
      END
