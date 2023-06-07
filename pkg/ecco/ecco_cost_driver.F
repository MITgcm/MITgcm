#include "ECCO_OPTIONS.h"
#ifdef ALLOW_TAPENADE
# ifdef ALLOW_CTRL
#  include "CTRL_OPTIONS.h"
# endif
#endif

      subroutine ecco_cost_driver(  myTime, myIter, myThid  )

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
# include "ctrl.h"
# include "CTRL_GENARR.h"
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==
      integer myThid
      integer myIter
      _RL     myTime

#ifdef ALLOW_OBCS_COST_CONTRIBUTION
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_obcs',myThid)
#endif
      call timer_start('cost_obcs          [ECCO SPIN-DOWN]', myThid)
      call cost_obcs( myIter, myTime, myThid )
      call timer_stop ('cost_obcs          [ECCO SPIN-DOWN]', myThid)
#endif

c--   Compute user defined cost function contributions (elaborate)
      call timer_start('cost_gencost_all    [ECCO SPIN-DOWN]', myThid)
      call cost_gencost_all( myIter, myTime, myThid )
      call timer_stop ('cost_gencost_all    [ECCO SPIN-DOWN]', myThid)

#ifdef ALLOW_CTRL
      IF (useCTRL) THEN
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('ctrl_cost_driver',myThid)
#endif
      call timer_start('ctrl_cost_driver [ECCO SPIN-DOWN]', myThid)
      call ctrl_cost_driver( myThid )
      call timer_stop ('ctrl_cost_driver [ECCO SPIN-DOWN]', myThid)
      ENDIF
#endif

      RETURN
      END
