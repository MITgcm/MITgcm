
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
#include "SIZE.h"
#include "EEPARAMS.h"

#include "cal.h"
#include "ctrl.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == local variables ==
      integer startrec
      integer endrec

c     == end of interface ==

#if (defined (ALLOW_OBCSN_COST_CONTRIBUTION) && \
     defined (ALLOW_OBCSN_CONTROL))
cgg   North boundary contribution to cost function.
      startrec = ncvarrecstart(11 )
      endrec   = ncvarrecsend(11 )
      call cost_obcsn ( myiter, mytime,
     &                   startrec,endrec, mythid )
#endif
#if (defined (ALLOW_OBCSS_COST_CONTRIBUTION) && \
     defined (ALLOW_OBCSS_CONTROL))
cgg   South boundary contribution to cost function.
      startrec = ncvarrecstart(12 )
      endrec   = ncvarrecsend(12 )
      call cost_obcss ( myiter, mytime,
     &                   startrec,endrec, mythid )
#endif

#if (defined (ALLOW_OBCSW_COST_CONTRIBUTION) && \
     defined (ALLOW_OBCSW_CONTROL))
cgg   West boundary contribution to cost function.
      startrec = ncvarrecstart(13 )
      endrec   = ncvarrecsend(13 )
      call cost_obcsw ( myiter, mytime,
     &                   startrec,endrec, mythid )
#endif

#if (defined (ALLOW_OBCSE_COST_CONTRIBUTION) && \
     defined (ALLOW_OBCSE_CONTROL))
cgg   East boundary contribution to cost function.
      startrec = ncvarrecstart(14 )
      endrec   = ncvarrecsend(14 )
      call cost_obcse ( myiter, mytime,
     &                  startrec,endrec, mythid )
#endif

#ifdef OBCS_VOLFLUX_COST_CONTRIBUTION
      call cost_obcsvol ( myiter,mytime,
     &                    startrec, endrec, mythid)
#endif

Cgg ( Ageostrophic calculation uses *bar* fields not
cgg   calculated until the end of the run. 
cgg   Move to ecco_cost.hyd.
Cgg(#ifdef OBCS_AGEOS_COST_CONTRIBUTION
cgg      call cost_obcs_ageos ( myiter,mytime, mythid)
Cgg(#endif
      end
