
#include "COST_CPPOPTIONS.h"


      subroutine cost_obcs(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_obcs
c     ==================================================================
c
c     o Evaluate cost function contributions for obc's
c
c     ==================================================================
c     SUBROUTINE cost_obcs
c     ==================================================================

      implicit none

c     == global variables ==

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == local variables ==


c     == end of interface ==

#ifdef ALLOW_OBCSN_COST_CONTRIBUTION
      call cost_obcsn ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_OBCSS_COST_CONTRIBUTION
      call cost_obcss ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_OBCSW_COST_CONTRIBUTION
      call cost_obcsw ( myiter, mytime, mythid )
#endif

#ifdef ALLOW_OBCSE_COST_CONTRIBUTION
      call cost_OBCSE ( myiter, mytime, mythid )
#endif

      end
