C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_init.F,v 1.15 2007/04/16 23:27:21 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      subroutine exf_init( mythid )

c     ==================================================================
c     SUBROUTINE exf_init
c     ==================================================================
c
c     o This routine initialises the forcing
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-20000
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "cal.h"
#include "EXF.h"
#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer interp_method

c     == end of interface ==

      if ( useCubedSphereExchange
     &     .and. ustressperiod .eq. 0
     &     .and. ustressfile .NE. ' ' ) then
         stop 'CubedSphereExchange and ustressperiod=0 not supported'
      endif
      interp_method=12
      call exf_init_gen (
     &     ustressfile, ustressperiod, exf_inscal_ustress, ustressmask,
     &     ustressconst, ustress, ustress0, ustress1, 
#ifdef USE_EXF_INTERPOLATION
     &     ustress_lon0, ustress_lon_inc, 
     &     ustress_lat0, ustress_lat_inc,
     &     ustress_nlon, ustress_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      if ( useCubedSphereExchange
     &     .and. vstressperiod .eq. 0
     &     .and. vstressfile .NE. ' ' ) then
         stop 'CubedSphereExchange and vstressperiod=0 not supported'
      endif
      interp_method=22
      call exf_init_gen (
     &     vstressfile, vstressperiod, exf_inscal_vstress, vstressmask,
     &     vstressconst, vstress, vstress0, vstress1, 
#ifdef USE_EXF_INTERPOLATION
     &     vstress_lon0, vstress_lon_inc, 
     &     vstress_lat0, vstress_lat_inc,
     &     vstress_nlon, vstress_nlat, xC, yC, interp_method,
#endif
     &     mythid )

#ifdef ALLOW_ATM_WIND 

      if ( useCubedSphereExchange
     &     .and. uwindperiod .eq. 0
     &     .and. uwindfile .NE. ' ' ) then
         stop 'CubedSphereExchange and uwindperiod=0 not supported'
      endif
      interp_method=12
      call exf_init_gen (
     &     uwindfile, uwindperiod, exf_inscal_uwind, uwindmask,
     &     uwindconst, uwind, uwind0, uwind1, 
#ifdef USE_EXF_INTERPOLATION
     &     uwind_lon0, uwind_lon_inc, 
     &     uwind_lat0, uwind_lat_inc,
     &     uwind_nlon, uwind_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      if ( useCubedSphereExchange
     &     .and. vwindperiod .eq. 0
     &     .and. vwindfile .NE. ' ' ) then
         stop 'CubedSphereExchange and vwindperiod=0 not supported'
      endif
      interp_method=22
      call exf_init_gen (
     &     vwindfile, vwindperiod, exf_inscal_vwind, vwindmask,
     &     vwindconst, vwind, vwind0, vwind1, 
#ifdef USE_EXF_INTERPOLATION
     &     vwind_lon0, vwind_lon_inc, 
     &     vwind_lat0, vwind_lat_inc,
     &     vwind_nlon, vwind_nlat, xC, yC, interp_method,
#endif
     &     mythid )

#endif /* ALLOW_ATM_WIND */

      interp_method=1

      call exf_init_gen (
     &     wspeedfile, wspeedperiod, exf_inscal_wspeed, wspeedmask,
     &     wspeedconst, wspeed, wspeed0, wspeed1, 
#ifdef USE_EXF_INTERPOLATION
     &     wspeed_lon0, wspeed_lon_inc, 
     &     wspeed_lat0, wspeed_lat_inc,
     &     wspeed_nlon, wspeed_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      call exf_init_gen (
     &     hfluxfile, hfluxperiod, exf_inscal_hflux, hfluxmask,
     &     hfluxconst, hflux, hflux0, hflux1, 
#ifdef USE_EXF_INTERPOLATION
     &     hflux_lon0, hflux_lon_inc, 
     &     hflux_lat0, hflux_lat_inc,
     &     hflux_nlon, hflux_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      call exf_init_gen (
     &     sfluxfile, sfluxperiod, exf_inscal_sflux, sfluxmask,
     &     sfluxconst, sflux, sflux0, sflux1, 
#ifdef USE_EXF_INTERPOLATION
     &     sflux_lon0, sflux_lon_inc, 
     &     sflux_lat0, sflux_lat_inc,
     &     sflux_nlon, sflux_nlat, xC, yC, interp_method,
#endif
     &     mythid )

#ifdef ALLOW_ATM_TEMP

      call exf_init_gen (
     &     atempfile, atempperiod, exf_inscal_atemp, atempmask,
     &     atempconst, atemp, atemp0, atemp1, 
#ifdef USE_EXF_INTERPOLATION
     &     atemp_lon0, atemp_lon_inc, 
     &     atemp_lat0, atemp_lat_inc,
     &     atemp_nlon, atemp_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      call exf_init_gen (
     &     aqhfile, aqhperiod, exf_inscal_aqh, aqhmask,
     &     aqhconst, aqh, aqh0, aqh1, 
#ifdef USE_EXF_INTERPOLATION
     &     aqh_lon0, aqh_lon_inc, 
     &     aqh_lat0, aqh_lat_inc,
     &     aqh_nlon, aqh_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      call exf_init_gen (
     &     lwfluxfile, lwfluxperiod, exf_inscal_lwflux, lwfluxmask,
     &     lwfluxconst, lwflux, lwflux0, lwflux1, 
#ifdef USE_EXF_INTERPOLATION
     &     lwflux_lon0, lwflux_lon_inc, 
     &     lwflux_lat0, lwflux_lat_inc,
     &     lwflux_nlon, lwflux_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      call exf_init_gen (
     &     precipfile, precipperiod, exf_inscal_precip, precipmask,
     &     precipconst, precip, precip0, precip1, 
#ifdef USE_EXF_INTERPOLATION
     &     precip_lon0, precip_lon_inc, 
     &     precip_lat0, precip_lat_inc,
     &     precip_nlon, precip_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      call exf_init_gen (
     &     snowprecipfile, snowprecipperiod,
     &     exf_inscal_snowprecip, snowprecipmask,
     &     snowprecipconst, snowprecip, snowprecip0, snowprecip1, 
#ifdef USE_EXF_INTERPOLATION
     &     snowprecip_lon0, snowprecip_lon_inc, 
     &     snowprecip_lat0, snowprecip_lat_inc,
     &     snowprecip_nlon, snowprecip_nlat, xC, yC, interp_method,
#endif
     &     mythid )

#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      call exf_init_gen (
     &     swfluxfile, swfluxperiod,  exf_inscal_swflux, swfluxmask,
     &     swfluxconst, swflux, swflux0, swflux1, 
#ifdef USE_EXF_INTERPOLATION
     &     swflux_lon0, swflux_lon_inc, 
     &     swflux_lat0, swflux_lat_inc,
     &     swflux_nlon, swflux_nlat, xC, yC, interp_method,
#endif
     &     mythid )
#endif /* defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING) */

#if defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP)
      call exf_init_gen (
     &     evapfile, evapperiod, exf_inscal_evap, evapmask,
     &     evapconst, evap, evap0, evap1, 
#ifdef USE_EXF_INTERPOLATION
     &     evap_lon0, evap_lon_inc, 
     &     evap_lat0, evap_lat_inc,
     &     evap_nlon, evap_nlat, xC, yC, interp_method,
#endif
     &     mythid )
#endif /* defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP) */

#ifdef ALLOW_DOWNWARD_RADIATION

      call exf_init_gen (
     &     swdownfile, swdownperiod, exf_inscal_swdown, swdownmask,
     &     swdownconst, swdown, swdown0, swdown1, 
#ifdef USE_EXF_INTERPOLATION
     &     swdown_lon0, swdown_lon_inc, 
     &     swdown_lat0, swdown_lat_inc,
     &     swdown_nlon, swdown_nlat, xC, yC, interp_method,
#endif
     &     mythid )

      call exf_init_gen (
     &     lwdownfile, lwdownperiod, exf_inscal_lwdown, lwdownmask,
     &     lwdownconst, lwdown, lwdown0, lwdown1, 
#ifdef USE_EXF_INTERPOLATION
     &     lwdown_lon0, lwdown_lon_inc, 
     &     lwdown_lat0, lwdown_lat_inc,
     &     lwdown_nlon, lwdown_nlat, xC, yC, interp_method,
#endif
     &     mythid )

#endif /* ALLOW_DOWNWARD_RADIATION */

#ifdef ATMOSPHERIC_LOADING
      call exf_init_gen (
     &     apressurefile, apressureperiod,
     &     exf_inscal_apressure, apressuremask,
     &     apressureconst, apressure, apressure0, apressure1, 
#ifdef USE_EXF_INTERPOLATION
     &     apressure_lon0, apressure_lon_inc, 
     &     apressure_lat0, apressure_lat_inc,
     &     apressure_nlon, apressure_nlat, xC, yC, interp_method,
#endif
     &     mythid )
#endif /* ATMOSPHERIC_LOADING */

#if (defined (ALLOW_RUNOFF) || defined (ALLOW_SEAICE))
      call exf_init_gen (
     &     runofffile, runoffperiod, exf_inscal_runoff, runoffmask,
     &     runoffconst, runoff, runoff0, runoff1, 
#ifdef USE_EXF_INTERPOLATION
     &     runoff_lon0, runoff_lon_inc, 
     &     runoff_lat0, runoff_lat_inc,
     &     runoff_nlon, runoff_nlat, xC, yC, interp_method,
#endif
     &     mythid )
#endif /* defined (ALLOW_RUNOFF) || defined (ALLOW_SEAICE) */

c     Initialize climatological fields
      call exf_clim_init     ( mythid )

      end
