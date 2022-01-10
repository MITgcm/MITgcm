#include "EXF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

      SUBROUTINE EXF_GETFFIELDS( myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE exf_getffields
C     ==================================================================
C
C     o Read-in atmospheric state and/or surface fluxes from files.
C
C       heimbach@mit.edu, 23-May-2003 totally re-structured
C       5-Aug-2003: added USE_EXF_INTERPOLATION for arbitrary input grid
C
C     ==================================================================
C     SUBROUTINE exf_getffields
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"
#include "EXF_FIELDS.h"

#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# ifdef ALLOW_GENTIM2D_CONTROL
#  include "CTRL_GENARR.h"
# endif
#endif
#if (defined (ALLOW_ECCO) && defined (ECCO_CTRL_DEPRECATED))
#  include "ecco_cost.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == local variables ==
      INTEGER i, j, bi, bj
#ifdef ALLOW_CTRL
# if (!defined (ALLOW_ECCO) || defined (ECCO_CTRL_DEPRECATED))
      _RS mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# endif
#endif
#ifdef ALLOW_ROTATE_UV_CONTROLS
      _RL     tmpUE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpVN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpUX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmpVY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#if (defined (ALLOW_CTRL) && \
     defined (ALLOW_GENTIM2D_CONTROL))
      INTEGER iarr
#endif

C     == end of interface ==

C--   read forcing fields from files and temporal interpolation

C-    Zonal and meridional wind stress.
      IF ( .NOT.useAtmWind ) THEN
       CALL EXF_SET_UV(
     I     'ustress', ustressfile, ustressmask,
     I     ustressStartTime, ustressperiod, ustressRepCycle,
     I     exf_inscal_ustress,
     I     ustress_exfremo_intercept, ustress_exfremo_slope,
     U     ustress, ustress0, ustress1,
     I     'vstress', vstressfile, vstressmask,
     I     vstressStartTime, vstressperiod, vstressRepCycle,
     I     exf_inscal_vstress,
     I     vstress_exfremo_intercept, vstress_exfremo_slope,
     U     vstress, vstress0, vstress1,
#ifdef USE_EXF_INTERPOLATION
     I     ustress_lon0, ustress_lon_inc, ustress_lat0, ustress_lat_inc,
     I     ustress_nlon, ustress_nlat, ustress_interpMethod,
     I     vstress_lon0, vstress_lon_inc, vstress_lat0, vstress_lat_inc,
     I     vstress_nlon, vstress_nlat, vstress_interpMethod,
     I     uvInterp_stress,
#endif /* USE_EXF_INTERPOLATION */
     I     myTime, myIter, myThid )
      ELSE
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           ustress(i,j,bi,bj) = 0. _d 0
           vstress(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C-    Wind speed
      CALL EXF_SET_FLD(
     I     'wspeed', wspeedfile, wspeedmask,
     I     wspeedStartTime, wspeedperiod, wspeedRepCycle,
     I     exf_inscal_wspeed,
     I     wspeed_exfremo_intercept, wspeed_exfremo_slope,
     U     wspeed, wspeed0, wspeed1,
#ifdef USE_EXF_INTERPOLATION
     I     wspeed_lon0, wspeed_lon_inc,
     I     wspeed_lat0, wspeed_lat_inc,
     I     wspeed_nlon, wspeed_nlat, xC, yC, wspeed_interpMethod,
#endif
     I     myTime, myIter, myThid )

C-    Zonal and meridional wind.
      IF ( useAtmWind ) THEN
       CALL EXF_SET_UV(
     I     'uwind', uwindfile, uwindmask,
     I     uwindStartTime, uwindperiod, uwindRepCycle,
     I     exf_inscal_uwind,
     I     uwind_exfremo_intercept, uwind_exfremo_slope,
     U     uwind, uwind0, uwind1,
     I     'vwind', vwindfile, vwindmask,
     I     vwindStartTime, vwindperiod, vwindRepCycle,
     I     exf_inscal_vwind,
     I     vwind_exfremo_intercept, vwind_exfremo_slope,
     U     vwind, vwind0, vwind1,
#ifdef USE_EXF_INTERPOLATION
     I     uwind_lon0, uwind_lon_inc, uwind_lat0, uwind_lat_inc,
     I     uwind_nlon, uwind_nlat, uwind_interpMethod,
     I     vwind_lon0, vwind_lon_inc, vwind_lat0, vwind_lat_inc,
     I     vwind_nlon, vwind_nlat, vwind_interpMethod, uvInterp_wind,
#endif /* USE_EXF_INTERPOLATION */
     I     myTime, myIter, myThid )

      ELSE
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uwind(i,j,bi,bj) = 0. _d 0
           vwind(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C-    Atmospheric heat flux.
      CALL EXF_SET_FLD(
     I     'hflux', hfluxfile, hfluxmask,
     I     hfluxStartTime, hfluxperiod, hfluxRepCycle,
     I     exf_inscal_hflux,
     I     hflux_exfremo_intercept, hflux_exfremo_slope,
     U     hflux, hflux0, hflux1,
#ifdef USE_EXF_INTERPOLATION
     I     hflux_lon0, hflux_lon_inc, hflux_lat0, hflux_lat_inc,
     I     hflux_nlon, hflux_nlat, xC, yC, hflux_interpMethod,
#endif
     I     myTime, myIter, myThid )

C-    Freshwater flux.
      CALL EXF_SET_FLD(
     I     'sflux', sfluxfile, sfluxmask,
     I     sfluxStartTime, sfluxperiod, sfluxRepCycle,
     I     exf_inscal_sflux,
     I     sflux_exfremo_intercept, sflux_exfremo_slope,
     U     sflux, sflux0, sflux1,
#ifdef USE_EXF_INTERPOLATION
     I     sflux_lon0, sflux_lon_inc, sflux_lat0, sflux_lat_inc,
     I     sflux_nlon, sflux_nlat, xC, yC, sflux_interpMethod,
#endif
     I     myTime, myIter, myThid )

#ifdef ALLOW_ATM_TEMP

C-    Atmospheric temperature.
      CALL EXF_SET_FLD(
     I     'atemp', atempfile, atempmask,
     I     atempStartTime, atempperiod, atempRepCycle,
     I     exf_inscal_atemp,
     I     atemp_exfremo_intercept, atemp_exfremo_slope,
     U     atemp, atemp0, atemp1,
#ifdef USE_EXF_INTERPOLATION
     I     atemp_lon0, atemp_lon_inc, atemp_lat0, atemp_lat_inc,
     I     atemp_nlon, atemp_nlat, xC, yC, atemp_interpMethod,
#endif
     I     myTime, myIter, myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
          atemp(i,j,bi,bj) = atemp(i,j,bi,bj) + exf_offset_atemp
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-    Atmospheric humidity.
      CALL EXF_SET_FLD(
     I     'aqh', aqhfile, aqhmask,
     I     aqhStartTime, aqhperiod, aqhRepCycle,
     I     exf_inscal_aqh,
     I     aqh_exfremo_intercept, aqh_exfremo_slope,
     U     aqh, aqh0, aqh1,
#ifdef USE_EXF_INTERPOLATION
     I     aqh_lon0, aqh_lon_inc, aqh_lat0, aqh_lat_inc,
     I     aqh_nlon, aqh_nlat, xC, yC, aqh_interpMethod,
#endif
     I     myTime, myIter, myThid )

# ifdef ALLOW_READ_TURBFLUXES

C-    Sensible Heat flux
      CALL EXF_SET_FLD(
     I     'hs', hs_file, hs_mask,
     I     hs_StartTime, hs_period, hs_RepCycle,
     I     exf_inscal_hs,
     I     hs_exfremo_intercept, hs_exfremo_slope,
     U     hs, hs0, hs1,
#  ifdef USE_EXF_INTERPOLATION
     I     hs_lon0, hs_lon_inc, hs_lat0, hs_lat_inc,
     I     hs_nlon, hs_nlat, xC, yC, hs_interpMethod,
#  endif
     I     myTime, myIter, myThid )

C-    Latent Heat flux
      CALL EXF_SET_FLD(
     I     'hl', hl_file, hl_mask,
     I     hl_StartTime, hl_period, hl_RepCycle,
     I     exf_inscal_hl,
     I     hl_exfremo_intercept, hl_exfremo_slope,
     U     hl, hl0, hl1,
#  ifdef USE_EXF_INTERPOLATION
     I     hl_lon0, hl_lon_inc, hl_lat0, hl_lat_inc,
     I     hl_nlon, hl_nlat, xC, yC, hl_interpMethod,
#  endif
     I     myTime, myIter, myThid )

# endif /* ALLOW_READ_TURBFLUXES */

C-    Net long wave radiative flux.
      CALL EXF_SET_FLD(
     I     'lwflux', lwfluxfile, lwfluxmask,
     I     lwfluxStartTime, lwfluxperiod, lwfluxRepCycle,
     I     exf_inscal_lwflux,
     I     lwflux_exfremo_intercept, lwflux_exfremo_slope,
     U     lwflux, lwflux0, lwflux1,
#ifdef USE_EXF_INTERPOLATION
     I     lwflux_lon0, lwflux_lon_inc, lwflux_lat0, lwflux_lat_inc,
     I     lwflux_nlon, lwflux_nlat, xC, yC, lwflux_interpMethod,
#endif
     I     myTime, myIter, myThid )

#ifdef EXF_READ_EVAP
C-    Evaporation
      CALL EXF_SET_FLD(
     I     'evap', evapfile, evapmask,
     I     evapStartTime, evapperiod, evapRepCycle,
     I     exf_inscal_evap,
     I     evap_exfremo_intercept, evap_exfremo_slope,
     U     evap, evap0, evap1,
#ifdef USE_EXF_INTERPOLATION
     I     evap_lon0, evap_lon_inc, evap_lat0, evap_lat_inc,
     I     evap_nlon, evap_nlat, xC, yC, evap_interpMethod,
#endif
     I     myTime, myIter, myThid )
#endif /* EXF_READ_EVAP */

C-    Precipitation.
      CALL EXF_SET_FLD(
     I     'precip', precipfile, precipmask,
     I     precipStartTime, precipperiod, precipRepCycle,
     I     exf_inscal_precip,
     I     precip_exfremo_intercept, precip_exfremo_slope,
     U     precip, precip0, precip1,
#ifdef USE_EXF_INTERPOLATION
     I     precip_lon0, precip_lon_inc, precip_lat0, precip_lat_inc,
     I     precip_nlon, precip_nlat, xC, yC, precip_interpMethod,
#endif
     I     myTime, myIter, myThid )

C-    Snow.
      CALL EXF_SET_FLD(
     I     'snowprecip', snowprecipfile, snowprecipmask,
     I     snowprecipStartTime, snowprecipperiod, snowprecipRepCycle,
     I     exf_inscal_snowprecip,
     I     snowprecip_exfremo_intercept, snowprecip_exfremo_slope,
     U     snowprecip, snowprecip0, snowprecip1,
#ifdef USE_EXF_INTERPOLATION
     I     snowprecip_lon0, snowprecip_lon_inc,
     I     snowprecip_lat0, snowprecip_lat_inc,
     I     snowprecip_nlon, snowprecip_nlat, xC, yC,
     I     snowprecip_interpMethod,
#endif
     I     myTime, myIter, myThid )
C     Take care of case where total precip is not defined
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_ATM_TEMP
CADJ STORE precip      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE snowprecip  = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif
      IF ( snowPrecipFile .NE. ' ' ) THEN
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           precip(i,j,bi,bj) =
     &          MAX( precip(i,j,bi,bj), snowPrecip(i,j,bi,bj) )
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
C-    Net short wave radiative flux.
      CALL EXF_SET_FLD(
     I     'swflux', swfluxfile, swfluxmask,
     I     swfluxStartTime, swfluxperiod, swfluxRepCycle,
     I     exf_inscal_swflux,
     I     swflux_exfremo_intercept, swflux_exfremo_slope,
     U     swflux, swflux0, swflux1,
#ifdef USE_EXF_INTERPOLATION
     I     swflux_lon0, swflux_lon_inc, swflux_lat0, swflux_lat_inc,
     I     swflux_nlon, swflux_nlat, xC, yC, swflux_interpMethod,
#endif
     I     myTime, myIter, myThid )
#endif

#ifdef ALLOW_DOWNWARD_RADIATION

C-    Downward shortwave radiation.
      CALL EXF_SET_FLD(
     I     'swdown', swdownfile, swdownmask,
     I     swdownStartTime, swdownperiod, swdownRepCycle,
     I     exf_inscal_swdown,
     I     swdown_exfremo_intercept, swdown_exfremo_slope,
     U     swdown, swdown0, swdown1,
#ifdef USE_EXF_INTERPOLATION
     I     swdown_lon0, swdown_lon_inc, swdown_lat0, swdown_lat_inc,
     I     swdown_nlon, swdown_nlat, xC, yC, swdown_interpMethod,
#endif
     I     myTime, myIter, myThid )

C-    Downward longwave radiation.
      CALL EXF_SET_FLD(
     I     'lwdown', lwdownfile, lwdownmask,
     I     lwdownStartTime, lwdownperiod, lwdownRepCycle,
     I     exf_inscal_lwdown,
     I     lwdown_exfremo_intercept, lwdown_exfremo_slope,
     U     lwdown, lwdown0, lwdown1,
#ifdef USE_EXF_INTERPOLATION
     I     lwdown_lon0, lwdown_lon_inc, lwdown_lat0, lwdown_lat_inc,
     I     lwdown_nlon, lwdown_nlat, xC, yC, lwdown_interpMethod,
#endif
     I     myTime, myIter, myThid )

#endif /* ALLOW_DOWNWARD_RADIATION */

#ifdef ATMOSPHERIC_LOADING
C-    Atmos. pressure forcing
      CALL EXF_SET_FLD(
     I     'apressure', apressurefile, apressuremask,
     I     apressureStartTime, apressureperiod, apressureRepCycle,
     I     exf_inscal_apressure,
     I     apressure_exfremo_intercept, apressure_exfremo_slope,
     U     apressure, apressure0, apressure1,
#ifdef USE_EXF_INTERPOLATION
     I     apressure_lon0, apressure_lon_inc,
     I     apressure_lat0, apressure_lat_inc,
     I     apressure_nlon,apressure_nlat,xC,yC, apressure_interpMethod,
#endif
     I     myTime, myIter, myThid )
#endif

#ifdef EXF_ALLOW_TIDES
C-    Tidal geopotential
      CALL EXF_SET_FLD(
     I     'tidePot', tidePotFile, tidePotMask,
     I     tidePotStartTime, tidePotPeriod, tidePotRepCycle,
     I     exf_inscal_tidePot,
     I     tidePot_exfremo_intercept, tidePot_exfremo_slope,
     U     tidePot, tidePot0, tidePot1,
#ifdef USE_EXF_INTERPOLATION
     I     tidePot_lon0, tidePot_lon_inc,
     I     tidePot_lat0, tidePot_lat_inc,
     I     tidePot_nlon, tidePot_nlat, xC, yC, tidePot_interpMethod,
#endif
     I     myTime, myIter, myThid )
#endif /* EXF_ALLOW_TIDES */

#ifdef EXF_SEAICE_FRACTION
C-    fractional ice-covered area mask
      CALL EXF_SET_FLD(
     I     'areamask', areamaskfile, areamaskmask,
     I     areamaskStartTime, areamaskperiod, areamaskRepCycle,
     I     exf_inscal_areamask,
     I     areamask_exfremo_intercept, areamask_exfremo_slope,
     U     areamask, areamask0, areamask1,
#ifdef USE_EXF_INTERPOLATION
     I     areamask_lon0, areamask_lon_inc,
     I     areamask_lat0, areamask_lat_inc,
     I     areamask_nlon, areamask_nlat, xC, yC, areamask_interpMethod,
#endif
     I     myTime, myIter, myThid )
#endif

#ifdef ALLOW_RUNOFF
C-    Runoff
      CALL EXF_SET_FLD(
     I     'runoff', runofffile, runoffmask,
     I     runoffStartTime, runoffperiod, runoffRepCycle,
     I     exf_inscal_runoff,
     I     runoff_exfremo_intercept, runoff_exfremo_slope,
     U     runoff, runoff0, runoff1,
#ifdef USE_EXF_INTERPOLATION
     I     runoff_lon0, runoff_lon_inc, runoff_lat0, runoff_lat_inc,
     I     runoff_nlon, runoff_nlat, xC, yC, runoff_interpMethod,
#endif
     I     myTime, myIter, myThid )
#endif /* ALLOW_RUNOFF */

#ifdef ALLOW_RUNOFTEMP
C-    Runoff temperature
      CALL EXF_SET_FLD(
     I     'runoftemp', runoftempfile, runoffmask,
     I     runoffStartTime, runoffperiod, runoffRepCycle,
     I     exf_inscal_runoftemp,
     I     runoftemp_exfremo_intercept, runoftemp_exfremo_slope,
     U     runoftemp, runoftemp0, runoftemp1,
#ifdef USE_EXF_INTERPOLATION
     I     runoff_lon0, runoff_lon_inc, runoff_lat0, runoff_lat_inc,
     I     runoff_nlon, runoff_nlat, xC, yC, runoff_interpMethod,
#endif
     I     myTime, myIter, myThid )
#endif /* ALLOW_RUNOFTEMP */

#ifdef ALLOW_SALTFLX
C-    Salt flux
      CALL EXF_SET_FLD(
     I     'saltflx', saltflxfile, saltflxmask,
     I     saltflxStartTime, saltflxperiod, saltflxRepCycle,
     I     exf_inscal_saltflx,
     I     saltflx_exfremo_intercept, saltflx_exfremo_slope,
     U     saltflx, saltflx0, saltflx1,
#ifdef USE_EXF_INTERPOLATION
     I     saltflx_lon0, saltflx_lon_inc,
     I     saltflx_lat0, saltflx_lat_inc,
     I     saltflx_nlon, saltflx_nlat, xC, yC, saltflx_interpMethod,
#endif
     I     myTime, myIter, myThid )
#endif

#ifdef ALLOW_ROTATE_UV_CONTROLS
      IF ( useCTRL ) THEN
        DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
           DO i = 1-OLx,sNx+OLx
             tmpUE(i,j,bi,bj) = 0. _d 0
             tmpVN(i,j,bi,bj) = 0. _d 0
             tmpUX(i,j,bi,bj) = 0. _d 0
             tmpVY(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif

# if (!defined (ALLOW_ECCO) || defined (ECCO_CTRL_DEPRECATED))

C-- Control variables for atmos. state
#ifdef ALLOW_CTRL
      IF (.NOT.ctrlUseGen) THEN

#ifdef ALLOW_ATEMP_CONTROL
      CALL CTRL_GET_MASK2D( xx_atemp_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_atemp_file, xx_atempstartdate, xx_atempperiod,
     &     mask2D, atemp, xx_atemp0, xx_atemp1, xx_atemp_dummy,
     &     xx_atemp_remo_intercept, xx_atemp_remo_slope,
     &     watemp, myTime, myIter, myThid )
#endif

#ifdef ALLOW_AQH_CONTROL
      CALL CTRL_GET_MASK2D( xx_aqh_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_aqh_file, xx_aqhstartdate, xx_aqhperiod,
     &     mask2D, aqh, xx_aqh0, xx_aqh1, xx_aqh_dummy,
     &     xx_aqh_remo_intercept, xx_aqh_remo_slope,
     &     waqh, myTime, myIter, myThid )
#endif

#ifdef ALLOW_PRECIP_CONTROL
      CALL CTRL_GET_MASK2D( xx_precip_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_precip_file, xx_precipstartdate, xx_precipperiod,
     &     mask2D, precip, xx_precip0, xx_precip1, xx_precip_dummy,
     &     xx_precip_remo_intercept, xx_precip_remo_slope,
     &     wprecip, myTime, myIter, myThid )
#endif

#ifdef ALLOW_SWDOWN_CONTROL
      CALL CTRL_GET_MASK2D( xx_swdown_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_swdown_file, xx_swdownstartdate, xx_swdownperiod,
     &     mask2D, swdown, xx_swdown0, xx_swdown1, xx_swdown_dummy,
     &     xx_swdown_remo_intercept, xx_swdown_remo_slope,
     &     wswdown, myTime, myIter, myThid )
#endif

#ifdef ALLOW_LWDOWN_CONTROL
      CALL CTRL_GET_MASK2D( xx_lwdown_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_lwdown_file, xx_lwdownstartdate, xx_lwdownperiod,
     &     mask2D, lwdown, xx_lwdown0, xx_lwdown1, xx_lwdown_dummy,
     &     xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     &     wlwdown, myTime, myIter, myThid )
#endif

      ENDIF !if (.NOT.ctrlUseGen) then

#ifdef ALLOW_SWFLUX_CONTROL
      CALL CTRL_GET_MASK2D( xx_swflux_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_swflux_file, xx_swfluxstartdate, xx_swfluxperiod,
     &     mask2D, swflux, xx_swflux0, xx_swflux1, xx_swflux_dummy,
     &     xx_swflux_remo_intercept, xx_swflux_remo_slope,
     &     wswflux, myTime, myIter, myThid )
#endif

#ifdef ALLOW_LWFLUX_CONTROL
      CALL CTRL_GET_MASK2D( xx_lwflux_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_lwflux_file, xx_lwfluxstartdate, xx_lwfluxperiod,
     &     mask2D, lwflux, xx_lwflux0, xx_lwflux1, xx_lwflux_dummy,
     &     xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     &     wswflux, myTime, myIter, myThid )
#endif

#ifdef ALLOW_EVAP_CONTROL
      CALL CTRL_GET_MASK2D( xx_evap_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_evap_file, xx_evapstartdate, xx_evapperiod,
     &     mask2D, evap, xx_evap0, xx_evap1, xx_evap_dummy,
     &     xx_evap_remo_intercept, xx_evap_remo_slope,
     &     wevap, myTime, myIter, myThid )
#endif

#ifdef ALLOW_SNOWPRECIP_CONTROL
      CALL CTRL_GET_MASK2D( xx_snowprecip_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_snowprecip_file, xx_snowprecipstartdate,
     &     xx_snowprecipperiod,
     &     mask2D, snowprecip, xx_snowprecip0, xx_snowprecip1,
     &     xx_snowprecip_dummy,
     &     xx_snowprecip_remo_intercept, xx_snowprecip_remo_slope,
     &     wsnowprecip, myTime, myIter, myThid )
#endif

#ifdef ALLOW_APRESSURE_CONTROL
      CALL CTRL_GET_MASK2D( xx_apressure_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_apressure_file, xx_apressurestartdate,
     &     xx_apressureperiod,
     &     mask2D, apressure, xx_apressure0, xx_apressure1,
     &     xx_apressure_dummy,
     &     xx_apressure_remo_intercept, xx_apressure_remo_slope,
     &     wapressure, myTime, myIter, myThid )
#endif

      IF ( useAtmWind ) THEN
#ifndef ALLOW_ROTATE_UV_CONTROLS

#ifdef ALLOW_UWIND_CONTROL
      CALL CTRL_GET_MASK2D( xx_uwind_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_uwind_file, xx_uwindstartdate, xx_uwindperiod,
     &     mask2D, uwind, xx_uwind0, xx_uwind1, xx_uwind_dummy,
     &     xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &     wuwind, myTime, myIter, myThid )
#endif /* ALLOW_UWIND_CONTROL */

#ifdef ALLOW_VWIND_CONTROL
      CALL CTRL_GET_MASK2D( xx_vwind_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_vwind_file, xx_vwindstartdate, xx_vwindperiod,
     &     mask2D, vwind, xx_vwind0, xx_vwind1, xx_vwind_dummy,
     &     xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &     wvwind, myTime, myIter, myThid )
#endif /* ALLOW_VWIND_CONTROL */

#else

#if defined(ALLOW_UWIND_CONTROL) && defined(ALLOW_VWIND_CONTROL)
      CALL CTRL_GET_MASK2D( xx_uwind_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_uwind_file, xx_uwindstartdate, xx_uwindperiod,
     &     mask2D, tmpUE, xx_uwind0, xx_uwind1, xx_uwind_dummy,
     &     xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &     wuwind, myTime, myIter, myThid )

      CALL CTRL_GET_MASK2D( xx_vwind_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_vwind_file, xx_vwindstartdate, xx_vwindperiod,
     &     mask2D, tmpVN, xx_vwind0, xx_vwind1, xx_vwind_dummy,
     &     xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &     wvwind, myTime, myIter, myThid )

      CALL ROTATE_UV2EN_RL(tmpUX,tmpVY,tmpUE,tmpVN,
     &     .FALSE.,.FALSE.,.TRUE.,1,myThid)

        DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
          DO j = 1,sNy
           DO i = 1,sNx
             uwind(i,j,bi,bj)=uwind(i,j,bi,bj)+tmpUX(i,j,bi,bj)
             vwind(i,j,bi,bj)=vwind(i,j,bi,bj)+tmpVY(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#endif

#endif /* ALLOW_ROTATE_UV_CONTROLS */
      ENDIF

#ifdef ALLOW_ATM_MEAN_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
# ifdef ALLOW_ATEMP_CONTROL
          atemp(i,j,bi,bj) =atemp(i,j,bi,bj) +xx_atemp_mean(i,j,bi,bj)
# endif
# ifdef ALLOW_AQH_CONTROL
          aqh(i,j,bi,bj)   =aqh(i,j,bi,bj)   +xx_aqh_mean(i,j,bi,bj)
# endif
# ifdef ALLOW_PRECIP_CONTROL
          precip(i,j,bi,bj)=precip(i,j,bi,bj)+xx_precip_mean(i,j,bi,bj)
# endif
# ifdef ALLOW_SWDOWN_CONTROL
          swdown(i,j,bi,bj)=swdown(i,j,bi,bj)+xx_swdown_mean(i,j,bi,bj)
# endif
# ifdef ALLOW_UWIND_CONTROL
          uwind(i,j,bi,bj) =uwind(i,j,bi,bj) +xx_uwind_mean(i,j,bi,bj)
# endif
# ifdef ALLOW_VWIND_CONTROL
          vwind(i,j,bi,bj) =vwind(i,j,bi,bj) +xx_vwind_mean(i,j,bi,bj)
# endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_ATM_MEAN_CONTROL */

cdm transferred from exf_init_runoff.F
cdm functionality needs to be checked before turning on
cdm #ifdef ALLOW_RUNOFF_CONTROL
cdm       CALL CTRL_GET_GEN (
cdm      &     xx_runoff_file, xx_runoffstartdate, xx_runoffperiod,
cdm      &     runoff, xx_runoff0, xx_runoff1, xx_runoff_dummy,
cdm      &     xx_runoff_remo_intercept, xx_runoff_remo_slope,
cdm      &     wrunoff, 0., 0., myThid )
cdm #endif

#endif /* ALLOW_CTRL */

#endif /* undef ALLOW_ECCO) || def ECCO_CTRL_DEPRECATED */

#if (defined (ALLOW_CTRL) && defined (ALLOW_GENTIM2D_CONTROL))
      IF ( useCTRL.AND.ctrlUseGen ) THEN
       DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
          DO iarr = 1, maxCtrlTim2D
#ifdef ALLOW_ATM_TEMP
           IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_atemp')
     &       atemp(i,j,bi,bj)=atemp(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:6).EQ.'xx_aqh')
     &       aqh(i,j,bi,bj)=aqh(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:9).EQ.'xx_precip')
     &       precip(i,j,bi,bj)=precip(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:13).EQ.'xx_snowprecip')
     &       snowprecip(i,j,bi,bj)=snowprecip(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:9).EQ.'xx_lwflux')
     &       lwflux(i,j,bi,bj)=lwflux(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#endif
#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
           IF (xx_gentim2d_file(iarr)(1:9).EQ.'xx_swflux')
     &       swflux(i,j,bi,bj)=swflux(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
           IF (xx_gentim2d_file(iarr)(1:9).EQ.'xx_swdown')
     &       swdown(i,j,bi,bj)=swdown(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:9).EQ.'xx_lwdown')
     &       lwdown(i,j,bi,bj)=lwdown(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#endif
#ifdef ALLOW_RUNOFF
           IF (xx_gentim2d_file(iarr)(1:9).EQ.'xx_runoff')
     &       runoff(i,j,bi,bj)=runoff(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#endif
#ifdef EXF_READ_EVAP
           IF (xx_gentim2d_file(iarr)(1:7).EQ.'xx_evap')
     &       evap(i,j,bi,bj)=evap(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#endif
#ifdef ATMOSPHERIC_LOADING
           IF (xx_gentim2d_file(iarr)(1:12).EQ.'xx_apressure')
     &       apressure(i,j,bi,bj)=apressure(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#endif
#ifdef EXF_SEAICE_FRACTION
           IF (xx_gentim2d_file(iarr)(1:11).EQ.'xx_areamask')
     &       areamask(i,j,bi,bj)=areamask(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#endif
#ifndef ALLOW_ROTATE_UV_CONTROLS
           IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_uwind')
     &       uwind(i,j,bi,bj)=uwind(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_vwind')
     &       vwind(i,j,bi,bj)=vwind(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#else
           IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_uwind')
     &       tmpUE(i,j,bi,bj)=tmpUE(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           IF (xx_gentim2d_file(iarr)(1:8).EQ.'xx_vwind')
     &       tmpVN(i,j,bi,bj)=tmpVN(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ENDDO
#ifdef ALLOW_ROTATE_UV_CONTROLS
       CALL ROTATE_UV2EN_RL(tmpUX,tmpVY,tmpUE,tmpVN,
     &      .FALSE.,.FALSE.,.TRUE.,1,myThid)

       DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
          DO j = 1,sNy
           DO i = 1,sNx
             uwind(i,j,bi,bj)=uwind(i,j,bi,bj)+tmpUX(i,j,bi,bj)
             vwind(i,j,bi,bj)=vwind(i,j,bi,bj)+tmpVY(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
       ENDDO
#endif /* ALLOW_ROTATE_UV_CONTROLS */

      ENDIF !if (ctrlUseGen) then
#endif

      RETURN
      END
