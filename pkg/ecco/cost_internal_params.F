#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine cost_internal_params( myIter, myTime, myThid )

c     ==================================================================
c     SUBROUTINE cost_internal_params
c     ==================================================================
c
c     o Evaluate cost function contributions of internal parameters adjustments.
c
c     started: Gael Forget 17-Aug-2007
c
c     ==================================================================
c     SUBROUTINE cost_internal_params
c     ==================================================================

      implicit none

c     == global variables ==

c     == routine arguments ==

      integer myIter
      _RL     myTime
      integer myThid

      RETURN
      END
