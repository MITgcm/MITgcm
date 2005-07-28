C
C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffields.F,v 1.27 2005/07/28 13:53:31 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_getffields( mycurrenttime, mycurrentiter, mythid )

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

#include "exf_param.h"
#include "exf_fields.h"
#include "exf_constants.h"

#ifdef ALLOW_AUTODIFF
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==

      integer mythid
      integer mycurrentiter
      _RL     mycurrenttime

c     == local variables ==

      integer i, j, bi, bj, interp_method
      parameter(interp_method=1)

c     == end of interface ==

c--   read forcing fields from files and temporal interpolation

cph-exf-print      print *, 'ph-exf --------- ----------------------------------'

c     Zonal and meridional wind stress.
#ifdef USE_EXF_INTERPOLATION
      call exf_set_uv(
     &     ustressfile, ustressstartdate, ustressperiod,
     &     ustressstartdate1, ustressstartdate2,
     &     exf_inscal_ustress, ustress, ustress0, ustress1, ustressmask,
     &     ustress_lon0, ustress_lon_inc, ustress_lat0, ustress_lat_inc,
     &     ustress_nlon, ustress_nlat,
     &     vstressfile, vstressstartdate, vstressperiod,
     &     vstressstartdate1, vstressstartdate2,
     &     exf_inscal_vstress, vstress, vstress0, vstress1, vstressmask,
     &     vstress_lon0, vstress_lon_inc, vstress_lat0, vstress_lat_inc,
     &     vstress_nlon, vstress_nlat,
     &     mycurrenttime, mycurrentiter, mythid )
#else /* ifndef USE_EXF_INTERPOLATION */
      call exf_set_gen( 
     &     ustressfile, ustressstartdate, ustressperiod,
     &     ustressstartdate1, ustressstartdate2,
     &     exf_inscal_ustress,
     &     ustress, ustress0, ustress1, ustressmask,
     &     mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen(
     &     vstressfile, vstressstartdate, vstressperiod,
     &     ustressstartdate1, ustressstartdate2,
     &     exf_inscal_vstress,
     &     vstress, vstress0, vstress1, vstressmask,
     &     mycurrenttime, mycurrentiter, mythid )
#endif /* USE_EXF_INTERPOLATION */

#ifdef ALLOW_ATM_WIND

c     Zonal and meridional wind.
#ifdef USE_EXF_INTERPOLATION
      call exf_set_uv(
     &     uwindfile, uwindstartdate, uwindperiod,
     &     uwindstartdate1, uwindstartdate2,
     &     exf_inscal_uwind, uwind, uwind0, uwind1, uwindmask,
     &     uwind_lon0, uwind_lon_inc, uwind_lat0, uwind_lat_inc,
     &     uwind_nlon, uwind_nlat,
     &     vwindfile, vwindstartdate, vwindperiod,
     &     vwindstartdate1, vwindstartdate2,
     &     exf_inscal_vwind, vwind, vwind0, vwind1, vwindmask,
     &     vwind_lon0, vwind_lon_inc, vwind_lat0, vwind_lat_inc,
     &     vwind_nlon, vwind_nlat,
     &     mycurrenttime, mycurrentiter, mythid )
#else /* ifndef USE_EXF_INTERPOLATION */
      call exf_set_gen(
     &     uwindfile, uwindstartdate, uwindperiod,
     &     uwindstartdate1, uwindstartdate2,
     &     exf_inscal_uwind,
     &     uwind, uwind0, uwind1, uwindmask,
     &     mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen(
     &     vwindfile, vwindstartdate, vwindperiod,
     &     vwindstartdate1, vwindstartdate2,
     &     exf_inscal_vwind,
     &     vwind, vwind0, vwind1, vwindmask,
     &     mycurrenttime, mycurrentiter, mythid )
#endif /* USE_EXF_INTERPOLATION */

#ifdef ALLOW_UWIND_CONTROL
      call ctrl_get_gen (
     &     xx_uwind_file, xx_uwindstartdate, xx_uwindperiod,
     &     maskc, uwind, xx_uwind0, xx_uwind1, xx_uwind_dummy,
     &     mycurrenttime, mycurrentiter, mythid )
#endif /* ALLOW_UWIND_CONTROL */

#ifdef ALLOW_VWIND_CONTROL
      call ctrl_get_gen (
     &     xx_vwind_file, xx_vwindstartdate, xx_vwindperiod,
     &     maskc, vwind, xx_vwind0, xx_vwind1, xx_vwind_dummy,
     &     mycurrenttime, mycurrentiter, mythid )
#endif /* ALLOW_VWIND_CONTROL */

#endif /* ALLOW_ATM_WIND */

c     Atmospheric heat flux.
      call exf_set_gen  ( 
     &     hfluxfile, hfluxstartdate, hfluxperiod, 
     &     hfluxstartdate1, hfluxstartdate2,
     &     exf_inscal_hflux,
     &     hflux, hflux0, hflux1, hfluxmask, 
#ifdef USE_EXF_INTERPOLATION
     &     hflux_lon0, hflux_lon_inc, hflux_lat0, hflux_lat_inc,
     &     hflux_nlon, hflux_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )

c     Salt flux.
      call exf_set_gen  ( 
     &     sfluxfile, sfluxstartdate, sfluxperiod, 
     &     sfluxstartdate1, sfluxstartdate2,
     &     exf_inscal_sflux,
     &     sflux, sflux0, sflux1, sfluxmask, 
#ifdef USE_EXF_INTERPOLATION
     &     sflux_lon0, sflux_lon_inc, sflux_lat0, sflux_lat_inc,
     &     sflux_nlon, sflux_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )

#ifdef ALLOW_ATM_TEMP

c     Atmospheric temperature.
      call exf_set_gen( 
     &     atempfile, atempstartdate, atempperiod, 
     &     atempstartdate1, atempstartdate2,
     &     exf_inscal_atemp,
     &     atemp, atemp0, atemp1, atempmask, 
#ifdef USE_EXF_INTERPOLATION
     &     atemp_lon0, atemp_lon_inc, atemp_lat0, atemp_lat_inc,
     &     atemp_nlon, atemp_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )
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
     &     aqhstartdate1, aqhstartdate2,
     &     exf_inscal_aqh,
     &     aqh, aqh0, aqh1, aqhmask, 
#ifdef USE_EXF_INTERPOLATION
     &     aqh_lon0, aqh_lon_inc, aqh_lat0, aqh_lat_inc,
     &     aqh_nlon, aqh_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )

c     Net long wave radiative flux.
      call exf_set_gen( 
     &     lwfluxfile, lwfluxstartdate, lwfluxperiod, 
     &     lwfluxstartdate1, lwfluxstartdate2,
     &     exf_inscal_lwflux,
     &     lwflux, lwflux0, lwflux1, lwfluxmask, 
#ifdef USE_EXF_INTERPOLATION
     &     lwflux_lon0, lwflux_lon_inc, lwflux_lat0, lwflux_lat_inc,
     &     lwflux_nlon, lwflux_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )

c     Precipitation.
      call exf_set_gen( 
     &     precipfile, precipstartdate, precipperiod, 
     &     precipstartdate1, precipstartdate2,
     &     exf_inscal_precip,
     &     precip, precip0, precip1, precipmask, 
#ifdef USE_EXF_INTERPOLATION
     &     precip_lon0, precip_lon_inc, precip_lat0, precip_lat_inc,
     &     precip_nlon, precip_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )

#ifdef ALLOW_ATEMP_CONTROL
      call ctrl_get_gen ( 
     &     xx_atemp_file, xx_atempstartdate, xx_atempperiod,
     &     maskc, atemp, xx_atemp0, xx_atemp1, xx_atemp_dummy,
     &     mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_AQH_CONTROL
      call ctrl_get_gen ( 
     &     xx_aqh_file, xx_aqhstartdate, xx_aqhperiod,
     &     maskc, aqh, xx_aqh0, xx_aqh1, xx_aqh_dummy,
     &     mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_PRECIP_CONTROL
      call ctrl_get_gen ( 
     &     xx_precip_file, xx_precipstartdate, xx_precipperiod,
     &     maskc, precip, xx_precip0, xx_precip1, xx_precip_dummy,
     &     mycurrenttime, mycurrentiter, mythid )
#endif

#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
c     Net short wave radiative flux.
      call exf_set_gen  ( 
     &     swfluxfile, swfluxstartdate, swfluxperiod, 
     &     swfluxstartdate1, swfluxstartdate2,
     &     exf_inscal_swflux,
     &     swflux, swflux0, swflux1, swfluxmask, 
#ifdef USE_EXF_INTERPOLATION
     &     swflux_lon0, swflux_lon_inc, swflux_lat0, swflux_lat_inc,
     &     swflux_nlon, swflux_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef EXF_READ_EVAP
c     Evaporation
      call exf_set_gen  ( 
     &     evapfile, evapstartdate, evapperiod, 
     &     evapstartdate1, evapstartdate2,
     &     exf_inscal_evap,
     &     evap, evap0, evap1, evapmask, 
#ifdef USE_EXF_INTERPOLATION
     &     evap_lon0, evap_lon_inc, evap_lat0, evap_lat_inc,
     &     evap_nlon, evap_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_DOWNWARD_RADIATION

c     Downward shortwave radiation.
      call exf_set_gen  ( 
     &     swdownfile, swdownstartdate, swdownperiod, 
     &     swdownstartdate1, swdownstartdate2,
     &     exf_inscal_swdown,
     &     swdown, swdown0, swdown1, swdownmask, 
#ifdef USE_EXF_INTERPOLATION
     &     swdown_lon0, swdown_lon_inc, swdown_lat0, swdown_lat_inc,
     &     swdown_nlon, swdown_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )

c     Downward longwave radiation.
      call exf_set_gen  ( 
     &     lwdownfile, lwdownstartdate, lwdownperiod, 
     &     lwdownstartdate1, lwdownstartdate2,
     &     exf_inscal_lwdown,
     &     lwdown, lwdown0, lwdown1, lwdownmask, 
#ifdef USE_EXF_INTERPOLATION
     &     lwdown_lon0, lwdown_lon_inc, lwdown_lat0, lwdown_lat_inc,
     &     lwdown_nlon, lwdown_nlat, xC, yC, interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )

#endif

#ifdef ATMOSPHERIC_LOADING
c     Atmos. pressure forcing
      call exf_set_gen  ( 
     &     apressurefile, apressurestartdate, apressureperiod, 
     &     apressurestartdate1, apressurestartdate2,
     &     exf_inscal_apressure,
     &     apressure, apressure0, apressure1, apressuremask, 
#ifdef USE_EXF_INTERPOLATION
     &     apressure_lon0, apressure_lon_inc, apressure_lat0,
     &     apressure_lat_inc, apressure_nlon, apressure_nlat, xC, yC,
     &     interp_method,
#endif
     &     mycurrenttime, mycurrentiter, mythid )
#endif

      end
