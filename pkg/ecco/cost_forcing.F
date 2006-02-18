C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_forcing.F,v 1.9 2006/02/18 01:13:07 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_forcing( myiter, mytime, mythid )

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
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "ecco_cost.h"

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
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_hflux_file, xx_hflux_dummy, wmean_hflux, whflux,
     &     num_hflux, num_hfluxm,
     &     objf_hflux, objf_hfluxm, objf_hfluxsmoo,
     &     maskC, mythid )
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION) && \
       defined (ALLOW_ATEMP_CONTROL))
c--   Atmos. temp. contribution to the cost function.
      startrec = ncvarrecstart(7)
      endrec   = ncvarrecsend(7)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_atemp_file, xx_atemp_dummy, wmean_atemp, watemp,
     &     num_atemp, num_atempm,
     &     objf_atemp, objf_atempm, objf_atempsmoo,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SFLUX_COST_CONTRIBUTION) && \
     defined (ALLOW_SFLUX_CONTROL))
c--   Salt flux contribution to the cost function.
      startrec = ncvarrecstart(4)
      endrec   = ncvarrecsend(4)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_sflux_file, xx_sflux_dummy, wmean_sflux, wsflux,
     &     num_sflux, num_sfluxm,
     &     objf_sflux, objf_sfluxm, objf_sfluxsmoo,
     &     maskC, mythid )
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION) && \
       defined (ALLOW_AQH_CONTROL))
c--   Specific humidity contribution to the cost function.
      startrec = ncvarrecstart(8)
      endrec   = ncvarrecsend(8)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_aqh_file, xx_aqh_dummy, wmean_aqh, waqh,
     &     num_aqh, num_aqhm,
     &     objf_aqh, objf_aqhm, objf_aqhsmoo,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_USTRESS_COST_CONTRIBUTION )&& \
     defined (ALLOW_USTRESS_CONTROL))
c--   Zonal wind stress contribution to the cost function.
      startrec = ncvarrecstart(5)
      endrec   = ncvarrecsend(5)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_tauu_file, xx_tauu_dummy, wmean_tau, wtauu,
     &     num_tauu, num_tauum,
     &     objf_tauu, objf_tauum, objf_tauusmoo,
     &     maskW, mythid )
#elif (defined (ALLOW_UWIND_COST_CONTRIBUTION )&& \
       defined (ALLOW_UWIND_CONTROL))
c--   Zonal wind speed contribution to the cost function.
      startrec = ncvarrecstart(9)
      endrec   = ncvarrecsend(9)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_uwind_file, xx_uwind_dummy, wmean_wind, wuwind,
     &     num_uwind, num_uwindm,
     &     objf_uwind, objf_uwindm, objf_uwindsmoo,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_VSTRESS_COST_CONTRIBUTION) && \
     defined (ALLOW_VSTRESS_CONTROL))
c--   Meridional wind stress contribution to the cost function.
      startrec = ncvarrecstart(6)
      endrec   = ncvarrecsend(6)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_tauv_file, xx_tauv_dummy, wmean_tau, wtauv,
     &     num_tauv, num_tauvm,
     &     objf_tauv, objf_tauvm, objf_tauvsmoo,
     &     maskS, mythid )
#elif (defined (ALLOW_VWIND_COST_CONTRIBUTION) && \
       defined (ALLOW_VWIND_CONTROL))
c--   Meridional wind speed contribution to the cost function.
      startrec = ncvarrecstart(10)
      endrec   = ncvarrecsend(10)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_vwind_file, xx_vwind_dummy, wmean_wind, wvwind,
     &     num_vwind, num_vwindm,
     &     objf_vwind, objf_vwindm, objf_vwindsmoo,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_PRECIP_COST_CONTRIBUTION) && \
       defined (ALLOW_PRECIP_CONTROL))
c--   Atmos. precip. contribution to the cost function.
      startrec = ncvarrecstart(32)
      endrec   = ncvarrecsend(32)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_precip_file, xx_precip_dummy, wmean_precip, wprecip,
     &     num_precip, num_precipm,
     &     objf_precip, objf_precipm, objf_precipsmoo,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SWFLUX_COST_CONTRIBUTION) && \
       defined (ALLOW_SWFLUX_CONTROL))
c--   Atmos. swflux. contribution to the cost function.
      startrec = ncvarrecstart(33)
      endrec   = ncvarrecsend(33)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_swflux_file, xx_swflux_dummy, wmean_swflux, wswflux,
     &     num_swflux, num_swfluxm,
     &     objf_swflux, objf_swfluxm, objf_swfluxsmoo,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SWDOWN_COST_CONTRIBUTION) && \
       defined (ALLOW_SWDOWN_CONTROL))
c--   Atmos. swdown. contribution to the cost function.
      startrec = ncvarrecstart(33)
      endrec   = ncvarrecsend(33)
      call cost_forcing_gen ( 
     &     myiter, mytime, startrec, endrec, 
     &     xx_swdown_file, xx_swdown_dummy, wmean_swdown, wswdown,
     &     num_swdown, num_swdownm,
     &     objf_swdown, objf_swdownm, objf_swdownsmoo,
     &     maskC, mythid )
#endif

c--   Compute cost function contribution of wind stress observations.  
#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
      call cost_mean_heatflux( myiter, mytime, mythid )
#endif

c--   Compute cost function contribution of wind stress observations.
#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
      call cost_mean_saltflux( myiter, mytime, mythid )
#endif

      end
