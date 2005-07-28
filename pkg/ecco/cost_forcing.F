C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_forcing.F,v 1.4 2005/07/28 19:51:58 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_forcing(
     I                         myiter,
     I                         mytime,
     I                         mythid
     &                       )

c     ==================================================================
c     SUBROUTINE cost_forcing
c     ==================================================================
c
c     o Evaluate cost function contributions of surface flux forcing.
c       Now, these are heat flux, salt flux, zonal and meridional wind
c       stress.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_forcing
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"

#include "cal.h"
#include "ctrl.h"

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer startrec
      integer endrec

c     == end of interface ==

c--   Evaluate the individual cost function contributions.

#if (defined (ALLOW_HFLUX_COST_CONTRIBUTION) && \
     defined (ALLOW_HFLUX_CONTROL))
c--   Heat flux contribution to the cost function.
      startrec = ncvarrecstart(3)
      endrec   = ncvarrecsend(3)
      call cost_heatflux ( myiter, mytime,
     &                     startrec, endrec, mythid )
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION) && \
       defined (ALLOW_ATEMP_CONTROL))
c--   Atmos. temp. contribution to the cost function.
      startrec = ncvarrecstart(7)
      endrec   = ncvarrecsend(7)
      call cost_atemp ( myiter, mytime,
     &                  startrec, endrec, mythid )
#endif

#if (defined (ALLOW_SFLUX_COST_CONTRIBUTION) && \
     defined (ALLOW_SFLUX_CONTROL))
c--   Salt flux contribution to the cost function.
      startrec = ncvarrecstart(4)
      endrec   = ncvarrecsend(4)
      call cost_saltflux ( myiter, mytime,
     &                     startrec, endrec, mythid )
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION) && \
       defined (ALLOW_AQH_CONTROL))
c--   Specific humidity contribution to the cost function.
      startrec = ncvarrecstart(8)
      endrec   = ncvarrecsend(8)
      call cost_aqh ( myiter, mytime,
     &                startrec, endrec, mythid )
#endif

#if (defined (ALLOW_USTRESS_COST_CONTRIBUTION )&& \
     defined (ALLOW_USTRESS_CONTROL))
c--   Zonal wind stress contribution to the cost function.
      startrec = ncvarrecstart(5)
      endrec   = ncvarrecsend(5)
      call cost_zonstress( myiter, mytime,
     &                     startrec, endrec, mythid )
#elif (defined (ALLOW_UWIND_COST_CONTRIBUTION )&& \
       defined (ALLOW_UWIND_CONTROL))
c--   Zonal wind speed contribution to the cost function.
      startrec = ncvarrecstart(9)
      endrec   = ncvarrecsend(9)
      call cost_uwind( myiter, mytime,
     &                 startrec, endrec, mythid )
#endif

#if (defined (ALLOW_VSTRESS_COST_CONTRIBUTION) && \
     defined (ALLOW_VSTRESS_CONTROL))
c--   Meridional wind stress contribution to the cost function.
      startrec = ncvarrecstart(6)
      endrec   = ncvarrecsend(6)
      call cost_merstress( myiter, mytime,
     &                     startrec, endrec, mythid )
#elif (defined (ALLOW_VWIND_COST_CONTRIBUTION) && \
       defined (ALLOW_VWIND_CONTROL))
c--   Meridional wind speed contribution to the cost function.
      startrec = ncvarrecstart(10)
      endrec   = ncvarrecsend(10)
      call cost_vwind( myiter, mytime,
     &                     startrec, endrec, mythid )
#endif

#if (defined (ALLOW_PRECIP_COST_CONTRIBUTION) && \
       defined (ALLOW_PRECIP_CONTROL))
c--   Atmos. temp. contribution to the cost function.
      startrec = ncvarrecstart(32)
      endrec   = ncvarrecsend(32)
      call cost_precip ( myiter, mytime,
     &                  startrec, endrec, mythid )
#endif

#if (defined (ALLOW_SWFLUX_COST_CONTRIBUTION) && \
       defined (ALLOW_SWFLUX_CONTROL))
c--   Atmos. temp. contribution to the cost function.
      startrec = ncvarrecstart(33)
      endrec   = ncvarrecsend(33)
      call cost_swflux ( myiter, mytime,
     &                  startrec, endrec, mythid )
#endif

      end
