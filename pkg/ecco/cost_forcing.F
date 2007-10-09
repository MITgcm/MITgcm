C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_forcing.F,v 1.13 2007/10/09 00:02:50 jmc Exp $
C $Name:  $

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
     &     xx_hflux_file, xx_hflux_dummy, xx_hfluxperiod,
     &     wmean_hflux, whflux,
     &     num_hflux, num_hfluxm,
     &     objf_hflux, objf_hfluxm, objf_hfluxsmoo,
     &     xx_hflux_remo_intercept, xx_hflux_remo_slope,
     &     maskC, mythid )
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION) && \
       defined (ALLOW_ATEMP_CONTROL))
c--   Atmos. temp. contribution to the cost function.
      startrec = ncvarrecstart(7)
      endrec   = ncvarrecsend(7)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_atemp_file, xx_atemp_dummy, xx_atempperiod,
     &     wmean_atemp, watemp,
     &     num_atemp, num_atempm,
     &     objf_atemp, objf_atempm, objf_atempsmoo,
     &     xx_atemp_remo_intercept, xx_atemp_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SFLUX_COST_CONTRIBUTION) && \
     defined (ALLOW_SFLUX_CONTROL))
c--   Salt flux contribution to the cost function.
      startrec = ncvarrecstart(4)
      endrec   = ncvarrecsend(4)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_sflux_file, xx_sflux_dummy, xx_sfluxperiod,
     &     wmean_sflux, wsflux,
     &     num_sflux, num_sfluxm,
     &     objf_sflux, objf_sfluxm, objf_sfluxsmoo,
     &     xx_sflux_remo_intercept, xx_sflux_remo_slope,
     &     maskC, mythid )
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION) && \
       defined (ALLOW_AQH_CONTROL))
c--   Specific humidity contribution to the cost function.
      startrec = ncvarrecstart(8)
      endrec   = ncvarrecsend(8)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_aqh_file, xx_aqh_dummy, xx_aqhperiod,
     &     wmean_aqh, waqh,
     &     num_aqh, num_aqhm,
     &     objf_aqh, objf_aqhm, objf_aqhsmoo,
     &     xx_aqh_remo_intercept, xx_aqh_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_USTRESS_COST_CONTRIBUTION )&& \
     defined (ALLOW_USTRESS_CONTROL))
c--   Zonal wind stress contribution to the cost function.
      startrec = ncvarrecstart(5)
      endrec   = ncvarrecsend(5)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_tauu_file, xx_tauu_dummy, xx_tauuperiod,
     &     wmean_tau, wtauu,
     &     num_tauu, num_tauum,
     &     objf_tauu, objf_tauum, objf_tauusmoo,
     &     xx_tauu_remo_intercept, xx_tauu_remo_slope,
     &     maskW, mythid )
#elif (defined (ALLOW_UWIND_COST_CONTRIBUTION )&& \
       defined (ALLOW_UWIND_CONTROL))
c--   Zonal wind speed contribution to the cost function.
      startrec = ncvarrecstart(9)
      endrec   = ncvarrecsend(9)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_uwind_file, xx_uwind_dummy, xx_uwindperiod,
     &     wmean_wind, wuwind,
     &     num_uwind, num_uwindm,
     &     objf_uwind, objf_uwindm, objf_uwindsmoo,
     &     xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_VSTRESS_COST_CONTRIBUTION) && \
     defined (ALLOW_VSTRESS_CONTROL))
c--   Meridional wind stress contribution to the cost function.
      startrec = ncvarrecstart(6)
      endrec   = ncvarrecsend(6)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_tauv_file, xx_tauv_dummy, xx_tauvperiod,
     &     wmean_tau, wtauv,
     &     num_tauv, num_tauvm,
     &     objf_tauv, objf_tauvm, objf_tauvsmoo,
     &     xx_tauv_remo_intercept, xx_tauv_remo_slope,
     &     maskS, mythid )
#elif (defined (ALLOW_VWIND_COST_CONTRIBUTION) && \
       defined (ALLOW_VWIND_CONTROL))
c--   Meridional wind speed contribution to the cost function.
      startrec = ncvarrecstart(10)
      endrec   = ncvarrecsend(10)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_vwind_file, xx_vwind_dummy, xx_vwindperiod,
     &     wmean_wind, wvwind,
     &     num_vwind, num_vwindm,
     &     objf_vwind, objf_vwindm, objf_vwindsmoo,
     &     xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_PRECIP_COST_CONTRIBUTION) && \
       defined (ALLOW_PRECIP_CONTROL))
c--   Atmos. precip. contribution to the cost function.
      startrec = ncvarrecstart(32)
      endrec   = ncvarrecsend(32)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_precip_file, xx_precip_dummy, xx_precipperiod,
     &     wmean_precip, wprecip,
     &     num_precip, num_precipm,
     &     objf_precip, objf_precipm, objf_precipsmoo,
     &     xx_precip_remo_intercept, xx_precip_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SWFLUX_COST_CONTRIBUTION) && \
       defined (ALLOW_SWFLUX_CONTROL))
c--   Atmos. swflux. contribution to the cost function.
      startrec = ncvarrecstart(33)
      endrec   = ncvarrecsend(33)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_swflux_file, xx_swflux_dummy, xx_swfluxperiod,
     &     wmean_swflux, wswflux,
     &     num_swflux, num_swfluxm,
     &     objf_swflux, objf_swfluxm, objf_swfluxsmoo,
     &     xx_swflux_remo_intercept, xx_swflux_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SWDOWN_COST_CONTRIBUTION) && \
       defined (ALLOW_SWDOWN_CONTROL))
c--   Atmos. swdown. contribution to the cost function.
      startrec = ncvarrecstart(34)
      endrec   = ncvarrecsend(34)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_swdown_file, xx_swdown_dummy, xx_swdownperiod,
     &     wmean_swdown, wswdown,
     &     num_swdown, num_swdownm,
     &     objf_swdown, objf_swdownm, objf_swdownsmoo,
     &     xx_swdown_remo_intercept, xx_swdown_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_LWFLUX_COST_CONTRIBUTION) && \
       defined (ALLOW_LWFLUX_CONTROL))
c--   Atmos. lwflux. contribution to the cost function.
      startrec = ncvarrecstart(35)
      endrec   = ncvarrecsend(35)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_lwflux_file, xx_lwflux_dummy, xx_lwfluxperiod,
     &     wmean_lwflux, wlwflux,
     &     num_lwflux, num_lwfluxm,
     &     objf_lwflux, objf_lwfluxm, objf_lwfluxsmoo,
     &     xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_LWDOWN_COST_CONTRIBUTION) && \
       defined (ALLOW_LWDOWN_CONTROL))
c--   Atmos. lwdown. contribution to the cost function.
      startrec = ncvarrecstart(36)
      endrec   = ncvarrecsend(36)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_lwdown_file, xx_lwdown_dummy, xx_lwdownperiod,
     &     wmean_lwdown, wlwdown,
     &     num_lwdown, num_lwdownm,
     &     objf_lwdown, objf_lwdownm, objf_lwdownsmoo,
     &     xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_EVAP_COST_CONTRIBUTION) && \
       defined (ALLOW_EVAP_CONTROL))
c--   Atmos. evap. contribution to the cost function.
      startrec = ncvarrecstart(36)
      endrec   = ncvarrecsend(36)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_evap_file, xx_evap_dummy, xx_evapperiod,
     &     wmean_evap, wevap,
     &     num_evap, num_evapm,
     &     objf_evap, objf_evapm, objf_evapsmoo,
     &     xx_evap_remo_intercept, xx_evap_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SNOWPRECIP_COST_CONTRIBUTION) && \
       defined (ALLOW_SNOWPRECIP_CONTROL))
c--   Atmos. snowprecip. contribution to the cost function.
      startrec = ncvarrecstart(36)
      endrec   = ncvarrecsend(36)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_snowprecip_file, xx_snowprecip_dummy, xx_snowprecipperiod,
     &     wmean_snowprecip, wsnowprecip,
     &     num_snowprecip, num_snowprecipm,
     &     objf_snowprecip, objf_snowprecipm, objf_snowprecipsmoo,
     &     xx_snowprecip_remo_intercept, xx_snowprecip_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_APRESSURE_COST_CONTRIBUTION) && \
       defined (ALLOW_APRESSURE_CONTROL))
c--   Atmos. apressure. contribution to the cost function.
      startrec = ncvarrecstart(36)
      endrec   = ncvarrecsend(36)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_apressure_file, xx_apressure_dummy, xx_apressureperiod,
     &     wmean_apressure, wapressure,
     &     num_apressure, num_apressurem,
     &     objf_apressure, objf_apressurem, objf_apressuresmoo,
     &     xx_apressure_remo_intercept, xx_apressure_remo_slope,
     &     maskC, mythid )
#endif

#if (defined (ALLOW_RUNOFF_COST_CONTRIBUTION) && \
       defined (ALLOW_RUNOFF_CONTROL))
c--   Atmos. runoff. contribution to the cost function.
      startrec = ncvarrecstart(36)
      endrec   = ncvarrecsend(36)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_runoff_file, xx_runoff_dummy, xx_runoffperiod,
     &     wmean_runoff, wrunoff,
     &     num_runoff, num_runoffm,
     &     objf_runoff, objf_runoffm, objf_runoffsmoo,
     &     xx_runoff_remo_intercept, xx_runoff_remo_slope,
     &     maskC, mythid )
#endif

      end
