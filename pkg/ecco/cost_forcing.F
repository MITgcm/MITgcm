#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine cost_forcing( mythid )

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

#ifdef ECCO_CTRL_DEPRECATED
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

#if (defined (ALLOW_ECCO) && defined (ALLOW_CTRL)&& defined (ECCO_CTRL_DEPRECATED))

      integer startrec
      integer endrec

#ifndef ALLOW_EXF
      LOGICAL useAtmWind
      PARAMETER ( useAtmWind = .TRUE. )
#endif

      logical dodimensionalcost2D

c     == end of interface ==

      dodimensionalcost2D=.NOT.ctrlSmoothCorrel2D

c--   Evaluate the individual cost function contributions.

#if (defined (ALLOW_HFLUX_COST_CONTRIBUTION) && \
     defined (ALLOW_HFLUX_CONTROL))
c--   Heat flux contribution to the cost function.
      startrec = ncvarrecstart(3)
      endrec   = ncvarrecsend(3)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_hflux_file, xx_hflux_dummy, xx_hfluxperiod,
     &     whflux, dodimensionalcost2D,
     &     num_hflux,
     &     objf_hflux,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_hflux, num_hfluxm,
     &     objf_hfluxm, objf_hfluxsmoo,
     &     xx_hflux_remo_intercept, xx_hflux_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION) && \
       defined (ALLOW_ATEMP_CONTROL))
      if (.NOT.ctrlUseGen) then
c--   Atmos. temp. contribution to the cost function.
      startrec = ncvarrecstart(7)
      endrec   = ncvarrecsend(7)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_atemp_file, xx_atemp_dummy, xx_atempperiod,
     &     watemp, dodimensionalcost2D,
     &     num_atemp,
     &     objf_atemp,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_atemp, num_atempm,
     &     objf_atempm, objf_atempsmoo,
     &     xx_atemp_remo_intercept, xx_atemp_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
      endif
#endif

#if (defined (ALLOW_SFLUX_COST_CONTRIBUTION) && \
     defined (ALLOW_SFLUX_CONTROL))
c--   Salt flux contribution to the cost function.
      startrec = ncvarrecstart(4)
      endrec   = ncvarrecsend(4)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_sflux_file, xx_sflux_dummy, xx_sfluxperiod,
     &     wsflux, dodimensionalcost2D,
     &     num_sflux,
     &     objf_sflux,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_sflux, num_sfluxm,
     &     objf_sfluxm, objf_sfluxsmoo,
     &     xx_sflux_remo_intercept, xx_sflux_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION) && \
       defined (ALLOW_AQH_CONTROL))
      if (.NOT.ctrlUseGen) then
c--   Specific humidity contribution to the cost function.
      startrec = ncvarrecstart(8)
      endrec   = ncvarrecsend(8)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_aqh_file, xx_aqh_dummy, xx_aqhperiod,
     &     waqh, dodimensionalcost2D,
     &     num_aqh,
     &     objf_aqh,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_aqh, num_aqhm,
     &     objf_aqhm, objf_aqhsmoo,
     &     xx_aqh_remo_intercept, xx_aqh_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
      endif
#endif

#if (defined (ALLOW_USTRESS_COST_CONTRIBUTION )&& \
     defined (ALLOW_USTRESS_CONTROL))
      IF ( (.NOT.useAtmWind).AND.(.NOT.ctrlUseGen) ) THEN
c--   Zonal wind stress contribution to the cost function.
      startrec = ncvarrecstart(5)
      endrec   = ncvarrecsend(5)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_tauu_file, xx_tauu_dummy, xx_tauuperiod,
     &     wtauu, dodimensionalcost2D,
     &     num_tauu,
     &     objf_tauu,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_tau, num_tauum,
     &     objf_tauum, objf_tauusmoo,
     &     xx_tauu_remo_intercept, xx_tauu_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskW, mythid )
      ENDIF
#endif

#if (defined (ALLOW_UWIND_COST_CONTRIBUTION )&& \
     defined (ALLOW_UWIND_CONTROL))
      IF ( useAtmWind ) THEN
c--   Zonal wind speed contribution to the cost function.
      startrec = ncvarrecstart(9)
      endrec   = ncvarrecsend(9)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_uwind_file, xx_uwind_dummy, xx_uwindperiod,
     &     wuwind, dodimensionalcost2D,
     &     num_uwind,
     &     objf_uwind,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_wind, num_uwindm,
     &     objf_uwindm, objf_uwindsmoo,
     &     xx_uwind_remo_intercept, xx_uwind_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
      ENDIF
#endif

#if (defined (ALLOW_VSTRESS_COST_CONTRIBUTION) && \
     defined (ALLOW_VSTRESS_CONTROL))
      IF ( (.NOT.useAtmWind).AND.(.NOT.ctrlUseGen) ) THEN
c--   Meridional wind stress contribution to the cost function.
      startrec = ncvarrecstart(6)
      endrec   = ncvarrecsend(6)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_tauv_file, xx_tauv_dummy, xx_tauvperiod,
     &     wtauv, dodimensionalcost2D,
     &     num_tauv,
     &     objf_tauv,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_tau, num_tauvm,
     &     objf_tauvm, objf_tauvsmoo,
     &     xx_tauv_remo_intercept, xx_tauv_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskS, mythid )
      ENDIF
#endif

#if (defined (ALLOW_VWIND_COST_CONTRIBUTION) && \
     defined (ALLOW_VWIND_CONTROL))
      IF ( useAtmWind ) THEN
c--   Meridional wind speed contribution to the cost function.
      startrec = ncvarrecstart(10)
      endrec   = ncvarrecsend(10)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_vwind_file, xx_vwind_dummy, xx_vwindperiod,
     &     wvwind, dodimensionalcost2D,
     &     num_vwind,
     &     objf_vwind,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_wind, num_vwindm,
     &     objf_vwindm, objf_vwindsmoo,
     &     xx_vwind_remo_intercept, xx_vwind_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
      ENDIF
#endif

#if (defined (ALLOW_PRECIP_COST_CONTRIBUTION) && \
       defined (ALLOW_PRECIP_CONTROL))
      if (.NOT.ctrlUseGen) then
c--   Atmos. precip. contribution to the cost function.
      startrec = ncvarrecstart(32)
      endrec   = ncvarrecsend(32)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_precip_file, xx_precip_dummy, xx_precipperiod,
     &     wprecip, dodimensionalcost2D,
     &     num_precip,
     &     objf_precip,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_precip, num_precipm,
     &     objf_precipm, objf_precipsmoo,
     &     xx_precip_remo_intercept, xx_precip_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
      endif
#endif

#if (defined (ALLOW_SWFLUX_COST_CONTRIBUTION) && \
       defined (ALLOW_SWFLUX_CONTROL))
c--   Atmos. swflux. contribution to the cost function.
      startrec = ncvarrecstart(33)
      endrec   = ncvarrecsend(33)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_swflux_file, xx_swflux_dummy, xx_swfluxperiod,
     &     wswflux, dodimensionalcost2D,
     &     num_swflux,
     &     objf_swflux,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_swflux, num_swfluxm,
     &     objf_swfluxm, objf_swfluxsmoo,
     &     xx_swflux_remo_intercept, xx_swflux_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SWDOWN_COST_CONTRIBUTION) && \
       defined (ALLOW_SWDOWN_CONTROL))
      if (.NOT.ctrlUseGen) then
c--   Atmos. swdown. contribution to the cost function.
      startrec = ncvarrecstart(34)
      endrec   = ncvarrecsend(34)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_swdown_file, xx_swdown_dummy, xx_swdownperiod,
     &     wswdown, dodimensionalcost2D,
     &     num_swdown,
     &     objf_swdown,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_swdown, num_swdownm,
     &     objf_swdownm, objf_swdownsmoo,
     &     xx_swdown_remo_intercept, xx_swdown_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
      endif
#endif

#if (defined (ALLOW_LWFLUX_COST_CONTRIBUTION) && \
       defined (ALLOW_LWFLUX_CONTROL))
c--   Atmos. lwflux. contribution to the cost function.
      startrec = ncvarrecstart(35)
      endrec   = ncvarrecsend(35)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_lwflux_file, xx_lwflux_dummy, xx_lwfluxperiod,
     &     wlwflux, dodimensionalcost2D,
     &     num_lwflux,
     &     objf_lwflux,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_lwflux, num_lwfluxm,
     &     objf_lwfluxm, objf_lwfluxsmoo,
     &     xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
#endif

#if (defined (ALLOW_LWDOWN_COST_CONTRIBUTION) && \
       defined (ALLOW_LWDOWN_CONTROL))
      if (.NOT.ctrlUseGen) then
c--   Atmos. lwdown. contribution to the cost function.
      startrec = ncvarrecstart(36)
      endrec   = ncvarrecsend(36)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_lwdown_file, xx_lwdown_dummy, xx_lwdownperiod,
     &     wlwdown, dodimensionalcost2D,
     &     num_lwdown,
     &     objf_lwdown,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_lwdown, num_lwdownm,
     &     objf_lwdownm, objf_lwdownsmoo,
     &     xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
      endif
#endif

#if (defined (ALLOW_EVAP_COST_CONTRIBUTION) && \
       defined (ALLOW_EVAP_CONTROL))
c--   Atmos. evap. contribution to the cost function.
      startrec = ncvarrecstart(37)
      endrec   = ncvarrecsend(37)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_evap_file, xx_evap_dummy, xx_evapperiod,
     &     wevap, dodimensionalcost2D,
     &     num_evap,
     &     objf_evap,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_evap, num_evapm,
     &     objf_evapm, objf_evapsmoo,
     &     xx_evap_remo_intercept, xx_evap_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
#endif

#if (defined (ALLOW_SNOWPRECIP_COST_CONTRIBUTION) && \
       defined (ALLOW_SNOWPRECIP_CONTROL))
c--   Atmos. snowprecip. contribution to the cost function.
      startrec = ncvarrecstart(38)
      endrec   = ncvarrecsend(38)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_snowprecip_file, xx_snowprecip_dummy, xx_snowprecipperiod,
     &     wsnowprecip, dodimensionalcost2D,
     &     num_snowprecip,
     &     objf_snowprecip,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_snowprecip, num_snowprecipm,
     &     objf_snowprecipm, objf_snowprecipsmoo,
     &     xx_snowprecip_remo_intercept, xx_snowprecip_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
#endif

#if (defined (ALLOW_APRESSURE_COST_CONTRIBUTION) && \
       defined (ALLOW_APRESSURE_CONTROL))
c--   Atmos. apressure. contribution to the cost function.
      startrec = ncvarrecstart(39)
      endrec   = ncvarrecsend(39)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_apressure_file, xx_apressure_dummy, xx_apressureperiod,
     &     wapressure, dodimensionalcost2D,
     &     num_apressure,
     &     objf_apressure,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_apressure, num_apressurem,
     &     objf_apressurem, objf_apressuresmoo,
     &     xx_apressure_remo_intercept, xx_apressure_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
#endif

#if (defined (ALLOW_RUNOFF_COST_CONTRIBUTION) && \
       defined (ALLOW_RUNOFF_CONTROL))
c--   Atmos. runoff. contribution to the cost function.
      startrec = ncvarrecstart(40)
      endrec   = ncvarrecsend(40)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_runoff_file, xx_runoff_dummy, xx_runoffperiod,
     &     wrunoff, dodimensionalcost2D,
     &     num_runoff,
     &     objf_runoff,
#ifdef ECCO_CTRL_DEPRECATED
     &     wmean_runoff, num_runoffm,
     &     objf_runoffm, objf_runoffsmoo,
     &     xx_runoff_remo_intercept, xx_runoff_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     &     maskC, mythid )
#endif

#endif /* ECCO_CTRL_DEPRECATED */

      RETURN
      END
