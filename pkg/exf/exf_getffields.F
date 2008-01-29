C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffields.F,v 1.40 2008/01/29 11:25:53 dimitri Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_getffields( mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_getffields
c     ==================================================================
c
c     o Read-in atmospheric state and/or surface fluxes from files.
c
c       heimbach@mit.edu, 23-May-2003 totally re-structured
c       5-Aug-2003: added USE_EXF_INTERPOLATION for arbitrary input grid
c
c     ==================================================================
c     SUBROUTINE exf_getffields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

#ifdef ALLOW_AUTODIFF
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer i, j, bi, bj, interp_method
      parameter(interp_method=1)

c     == end of interface ==

c--   read forcing fields from files and temporal interpolation

c     Zonal and meridional wind stress.
#ifdef USE_EXF_INTERPOLATION
      call exf_set_uv(
     &     ustressfile, ustressstartdate, ustressperiod,
     &     exf_inscal_ustress, ustress, ustress0, ustress1, ustressmask,
     &     ustress_lon0, ustress_lon_inc, ustress_lat0, ustress_lat_inc,
     &     ustress_nlon, ustress_nlat,
     &     ustress_exfremo_intercept, ustress_exfremo_slope,
     &     vstressfile, vstressstartdate, vstressperiod,
     &     exf_inscal_vstress, vstress, vstress0, vstress1, vstressmask,
     &     vstress_lon0, vstress_lon_inc, vstress_lat0, vstress_lat_inc,
     &     vstress_nlon, vstress_nlat,
     &     vstress_exfremo_intercept, vstress_exfremo_slope,
     &     mytime, myiter, mythid )
#else /* ifndef USE_EXF_INTERPOLATION */
      call exf_set_gen( 
     &     ustressfile, ustressstartdate, ustressperiod,
     &     exf_inscal_ustress,
     &     ustress_exfremo_intercept, ustress_exfremo_slope,
     &     ustress, ustress0, ustress1, ustressmask,
     &     mytime, myiter, mythid )
      call exf_set_gen(
     &     vstressfile, vstressstartdate, vstressperiod,
     &     exf_inscal_vstress,
     &     vstress_exfremo_intercept, vstress_exfremo_slope,
     &     vstress, vstress0, vstress1, vstressmask,
     &     mytime, myiter, mythid )
#endif /* USE_EXF_INTERPOLATION */

c--   wind speed
      call exf_set_gen( 
     &     wspeedfile, wspeedstartdate, wspeedperiod, 
     &     exf_inscal_wspeed,
     &     wspeed_exfremo_intercept, wspeed_exfremo_slope,
     &     wspeed, wspeed0, wspeed1, wspeedmask, 
#ifdef USE_EXF_INTERPOLATION
     &     wspeed_lon0, wspeed_lon_inc, 
     &     wspeed_lat0, wspeed_lat_inc,
     &     wspeed_nlon, wspeed_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

#ifdef ALLOW_ATM_WIND

c     Zonal and meridional wind.
#ifdef USE_EXF_INTERPOLATION
      call exf_set_uv(
     &     uwindfile, uwindstartdate, uwindperiod,
     &     exf_inscal_uwind, uwind, uwind0, uwind1, uwindmask,
     &     uwind_lon0, uwind_lon_inc, uwind_lat0, uwind_lat_inc,
     &     uwind_nlon, uwind_nlat,
     &     uwind_exfremo_intercept, uwind_exfremo_slope,
     &     vwindfile, vwindstartdate, vwindperiod,
     &     exf_inscal_vwind, vwind, vwind0, vwind1, vwindmask,
     &     vwind_lon0, vwind_lon_inc, vwind_lat0, vwind_lat_inc,
     &     vwind_nlon, vwind_nlat,
     &     vwind_exfremo_intercept, vwind_exfremo_slope,
     &     mytime, myiter, mythid )
#else /* ifndef USE_EXF_INTERPOLATION */
      call exf_set_gen(
     &     uwindfile, uwindstartdate, uwindperiod,
     &     exf_inscal_uwind,
     &     uwind_exfremo_intercept, uwind_exfremo_slope,
     &     uwind, uwind0, uwind1, uwindmask,
     &     mytime, myiter, mythid )
      call exf_set_gen(
     &     vwindfile, vwindstartdate, vwindperiod,
     &     exf_inscal_vwind,
     &     vwind_exfremo_intercept, vwind_exfremo_slope,
     &     vwind, vwind0, vwind1, vwindmask,
     &     mytime, myiter, mythid )
#endif /* USE_EXF_INTERPOLATION */

      if (useRelativeWind) then
C     Subtract UVEL and VVEL from UWIND and VWIND.
       do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
         do j = 1,sny
          do i = 1,snx
           uwind(i,j,bi,bj) = uwind(i,j,bi,bj) - 0.5 _d 0 *
     &          (uVel(i,j,1,bi,bj)+uVel(i+1,j,1,bi,bj))
           vwind(i,j,bi,bj) = vwind(i,j,bi,bj) - 0.5 _d 0 *
     &          (vVel(i,j,1,bi,bj)+vVel(i,j+1,1,bi,bj))
          enddo
         enddo
        enddo
       enddo
      endif

#endif /* ALLOW_ATM_WIND */

c     Atmospheric heat flux.
      call exf_set_gen  (
     &     hfluxfile, hfluxstartdate, hfluxperiod, 
     &     exf_inscal_hflux,
     &     hflux_exfremo_intercept, hflux_exfremo_slope,
     &     hflux, hflux0, hflux1, hfluxmask, 
#ifdef USE_EXF_INTERPOLATION
     &     hflux_lon0, hflux_lon_inc, hflux_lat0, hflux_lat_inc,
     &     hflux_nlon, hflux_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

c     Salt flux.
      call exf_set_gen  (
     &     sfluxfile, sfluxstartdate, sfluxperiod, 
     &     exf_inscal_sflux,
     &     sflux_exfremo_intercept, sflux_exfremo_slope,
     &     sflux, sflux0, sflux1, sfluxmask, 
#ifdef USE_EXF_INTERPOLATION
     &     sflux_lon0, sflux_lon_inc, sflux_lat0, sflux_lat_inc,
     &     sflux_nlon, sflux_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

#ifdef ALLOW_ATM_TEMP

c     Atmospheric temperature.
      call exf_set_gen( 
     &     atempfile, atempstartdate, atempperiod, 
     &     exf_inscal_atemp,
     &     atemp_exfremo_intercept, atemp_exfremo_slope,
     &     atemp, atemp0, atemp1, atempmask, 
#ifdef USE_EXF_INTERPOLATION
     &     atemp_lon0, atemp_lon_inc, atemp_lat0, atemp_lat_inc,
     &     atemp_nlon, atemp_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )
      do bj = mybylo(mythid),mybyhi(mythid)
       do bi = mybxlo(mythid),mybxhi(mythid)
        do j = 1,sny
         do i = 1,snx
          atemp(i,j,bi,bj) = atemp(i,j,bi,bj) + exf_offset_atemp
         enddo
        enddo
       enddo
      enddo

c     Atmospheric humidity.
      call exf_set_gen( 
     &     aqhfile, aqhstartdate, aqhperiod, 
     &     exf_inscal_aqh,
     &     aqh_exfremo_intercept, aqh_exfremo_slope,
     &     aqh, aqh0, aqh1, aqhmask, 
#ifdef USE_EXF_INTERPOLATION
     &     aqh_lon0, aqh_lon_inc, aqh_lat0, aqh_lat_inc,
     &     aqh_nlon, aqh_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

c     Net long wave radiative flux.
      call exf_set_gen( 
     &     lwfluxfile, lwfluxstartdate, lwfluxperiod, 
     &     exf_inscal_lwflux,
     &     lwflux_exfremo_intercept, lwflux_exfremo_slope,
     &     lwflux, lwflux0, lwflux1, lwfluxmask, 
#ifdef USE_EXF_INTERPOLATION
     &     lwflux_lon0, lwflux_lon_inc, lwflux_lat0, lwflux_lat_inc,
     &     lwflux_nlon, lwflux_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

c     Precipitation.
      call exf_set_gen( 
     &     precipfile, precipstartdate, precipperiod, 
     &     exf_inscal_precip,
     &     precip_exfremo_intercept, precip_exfremo_slope,
     &     precip, precip0, precip1, precipmask, 
#ifdef USE_EXF_INTERPOLATION
     &     precip_lon0, precip_lon_inc, precip_lat0, precip_lat_inc,
     &     precip_nlon, precip_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

c     Snow.
      call exf_set_gen( 
     &     snowprecipfile, snowprecipstartdate, snowprecipperiod, 
     &     exf_inscal_snowprecip,
     &     snowprecip_exfremo_intercept, snowprecip_exfremo_slope,
     &     snowprecip, snowprecip0, snowprecip1, snowprecipmask, 
#ifdef USE_EXF_INTERPOLATION
     &     snowprecip_lon0, snowprecip_lon_inc, 
     &     snowprecip_lat0, snowprecip_lat_inc,
     &     snowprecip_nlon, snowprecip_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
c     Net short wave radiative flux.
      call exf_set_gen  (
     &     swfluxfile, swfluxstartdate, swfluxperiod, 
     &     exf_inscal_swflux,
     &     swflux_exfremo_intercept, swflux_exfremo_slope,
     &     swflux, swflux0, swflux1, swfluxmask, 
#ifdef USE_EXF_INTERPOLATION
     &     swflux_lon0, swflux_lon_inc, swflux_lat0, swflux_lat_inc,
     &     swflux_nlon, swflux_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )
#endif

#ifdef EXF_READ_EVAP
c     Evaporation
      call exf_set_gen  (
     &     evapfile, evapstartdate, evapperiod, 
     &     exf_inscal_evap,
     &     evap_exfremo_intercept, evap_exfremo_slope,
     &     evap, evap0, evap1, evapmask, 
#ifdef USE_EXF_INTERPOLATION
     &     evap_lon0, evap_lon_inc, evap_lat0, evap_lat_inc,
     &     evap_nlon, evap_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_DOWNWARD_RADIATION

c     Downward shortwave radiation.
      call exf_set_gen  (
     &     swdownfile, swdownstartdate, swdownperiod, 
     &     exf_inscal_swdown,
     &     swdown_exfremo_intercept, swdown_exfremo_slope,
     &     swdown, swdown0, swdown1, swdownmask, 
#ifdef USE_EXF_INTERPOLATION
     &     swdown_lon0, swdown_lon_inc, swdown_lat0, swdown_lat_inc,
     &     swdown_nlon, swdown_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

c     Downward longwave radiation.
      call exf_set_gen  (
     &     lwdownfile, lwdownstartdate, lwdownperiod, 
     &     exf_inscal_lwdown,
     &     lwdown_exfremo_intercept, lwdown_exfremo_slope,
     &     lwdown, lwdown0, lwdown1, lwdownmask, 
#ifdef USE_EXF_INTERPOLATION
     &     lwdown_lon0, lwdown_lon_inc, lwdown_lat0, lwdown_lat_inc,
     &     lwdown_nlon, lwdown_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )

#endif

#ifdef ATMOSPHERIC_LOADING
c     Atmos. pressure forcing
      call exf_set_gen  (
     &     apressurefile, apressurestartdate, apressureperiod, 
     &     exf_inscal_apressure,
     &     apressure_exfremo_intercept, apressure_exfremo_slope,
     &     apressure, apressure0, apressure1, apressuremask, 
#ifdef USE_EXF_INTERPOLATION
     &     apressure_lon0, apressure_lon_inc, apressure_lat0,
     &     apressure_lat_inc, apressure_nlon, apressure_nlat, xC, yC,
     &     interp_method,
#endif
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_RUNOFF
c     Runoff
      call exf_set_gen  (
     &     runofffile, runoffstartdate, runoffperiod, 
     &     exf_inscal_runoff,
     &     runoff_exfremo_intercept, runoff_exfremo_slope,
     &     runoff, runoff0, runoff1, runoffmask, 
#ifdef USE_EXF_INTERPOLATION
     &     runoff_lon0, runoff_lon_inc, runoff_lat0,
     &     runoff_lat_inc, runoff_nlon, runoff_nlat, xC, yC,
     &     interp_method,
#endif
     &     mytime, myiter, mythid )
#endif

c-- Control variables for atmos. state

#ifdef ALLOW_ATEMP_CONTROL
      call ctrl_get_gen (
     &     xx_atemp_file, xx_atempstartdate, xx_atempperiod,
     &     maskc, atemp, xx_atemp0, xx_atemp1, xx_atemp_dummy,
     &     xx_atemp_remo_intercept, xx_atemp_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_AQH_CONTROL
      call ctrl_get_gen (
     &     xx_aqh_file, xx_aqhstartdate, xx_aqhperiod,
     &     maskc, aqh, xx_aqh0, xx_aqh1, xx_aqh_dummy,
     &     xx_aqh_remo_intercept, xx_aqh_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_PRECIP_CONTROL
      call ctrl_get_gen (
     &     xx_precip_file, xx_precipstartdate, xx_precipperiod,
     &     maskc, precip, xx_precip0, xx_precip1, xx_precip_dummy,
     &     xx_precip_remo_intercept, xx_precip_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_SWFLUX_CONTROL
      call ctrl_get_gen (
     &     xx_swflux_file, xx_swfluxstartdate, xx_swfluxperiod,
     &     maskc, swflux, xx_swflux0, xx_swflux1, xx_swflux_dummy,
     &     xx_swflux_remo_intercept, xx_swflux_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_SWDOWN_CONTROL
      call ctrl_get_gen (
     &     xx_swdown_file, xx_swdownstartdate, xx_swdownperiod,
     &     maskc, swdown, xx_swdown0, xx_swdown1, xx_swdown_dummy,
     &     xx_swdown_remo_intercept, xx_swdown_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_LWFLUX_CONTROL
      call ctrl_get_gen (
     &     xx_lwflux_file, xx_lwfluxstartdate, xx_lwfluxperiod,
     &     maskc, lwflux, xx_lwflux0, xx_lwflux1, xx_lwflux_dummy,
     &     xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_LWDOWN_CONTROL
      call ctrl_get_gen (
     &     xx_lwdown_file, xx_lwdownstartdate, xx_lwdownperiod,
     &     maskc, lwdown, xx_lwdown0, xx_lwdown1, xx_lwdown_dummy,
     &     xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_EVAP_CONTROL
      call ctrl_get_gen (
     &     xx_evap_file, xx_evapstartdate, xx_evapperiod,
     &     maskc, evap, xx_evap0, xx_evap1, xx_evap_dummy,
     &     xx_evap_remo_intercept, xx_evap_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_SNOWPRECIP_CONTROL
      call ctrl_get_gen (
     &     xx_snowprecip_file, xx_snowprecipstartdate, 
     &     xx_snowprecipperiod,
     &     maskc, snowprecip, xx_snowprecip0, xx_snowprecip1, 
     &     xx_snowprecip_dummy,
     &     xx_snowprecip_remo_intercept, xx_snowprecip_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_APRESSURE_CONTROL
      call ctrl_get_gen (
     &     xx_apressure_file, xx_apressurestartdate, 
     &     xx_apressureperiod,
     &     maskc, apressure, xx_apressure0, xx_apressure1, 
     &     xx_apressure_dummy,
     &     xx_apressure_remo_intercept, xx_apressure_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_UWIND_CONTROL
      call ctrl_get_gen (
     &     xx_uwind_file, xx_uwindstartdate, xx_uwindperiod,
     &     maskc, uwind, xx_uwind0, xx_uwind1, xx_uwind_dummy,
     &     xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &     mytime, myiter, mythid )
#endif /* ALLOW_UWIND_CONTROL */

#ifdef ALLOW_VWIND_CONTROL
      call ctrl_get_gen (
     &     xx_vwind_file, xx_vwindstartdate, xx_vwindperiod,
     &     maskc, vwind, xx_vwind0, xx_vwind1, xx_vwind_dummy,
     &     xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &     mytime, myiter, mythid )
#endif /* ALLOW_VWIND_CONTROL */

cdm transferred from exf_init_runoff.F
cdm functionality needs to be checked before turning on
cdm #ifdef ALLOW_RUNOFF_CONTROL
cdm       call ctrl_get_gen ( 
cdm      &     xx_runoff_file, xx_runoffstartdate, xx_runoffperiod,
cdm      &     maskc, runoff, xx_runoff0, xx_runoff1, xx_runoff_dummy,
cdm      &     xx_runoff_remo_intercept, xx_runoff_remo_slope,
cdm      &     0., 0., mythid )
cdm #endif

      end
