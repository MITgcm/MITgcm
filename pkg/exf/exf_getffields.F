C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffields.F,v 1.58 2014/11/02 21:23:30 gforget Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
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
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

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

C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == local variables ==
      INTEGER i, j, bi, bj
#ifdef ALLOW_ROTATE_UV_CONTROLS
      _RL     tmpUE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nsx,nsy)
      _RL     tmpVN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nsx,nsy)
      _RL     tmpUX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nsx,nsy)
      _RL     tmpVY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nsx,nsy)
#endif
#if (defined (ALLOW_CTRL) && \
     defined (ALLOW_GENTIM2D_CONTROL))
      INTEGER iarr
#endif

C     == end of interface ==

C--   read forcing fields from files and temporal interpolation

C     Zonal and meridional wind stress.
      IF ( .NOT.useAtmWind ) THEN
      CALL EXF_SET_UV(
     I     ustressfile, ustressstartdate, ustressperiod,
     I     exf_inscal_ustress,
     I     ustress_exfremo_intercept, ustress_exfremo_slope,
     U     ustress, ustress0, ustress1, ustressmask,
     I     vstressfile, vstressstartdate, vstressperiod,
     I     exf_inscal_vstress,
     I     vstress_exfremo_intercept, vstress_exfremo_slope,
     U     vstress, vstress0, vstress1, vstressmask,
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
        DO bi = myBxLo(myThid),mybxhi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           ustress(i,j,bi,bj) = 0. _d 0
           vstress(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   wind speed
      CALL EXF_SET_GEN(
     &     wspeedfile, wspeedstartdate, wspeedperiod,
     &     exf_inscal_wspeed,
     &     wspeed_exfremo_intercept, wspeed_exfremo_slope,
     &     wspeed, wspeed0, wspeed1, wspeedmask,
#ifdef USE_EXF_INTERPOLATION
     &     wspeed_lon0, wspeed_lon_inc,
     &     wspeed_lat0, wspeed_lat_inc,
     &     wspeed_nlon, wspeed_nlat, xC, yC, wspeed_interpMethod,
#endif
     &     myTime, myIter, myThid )

C     Zonal and meridional wind.
      IF ( useAtmWind ) THEN
      CALL EXF_SET_UV(
     I     uwindfile, uwindstartdate, uwindperiod,
     I     exf_inscal_uwind,
     I     uwind_exfremo_intercept, uwind_exfremo_slope,
     U     uwind, uwind0, uwind1, uwindmask,
     I     vwindfile, vwindstartdate, vwindperiod,
     I     exf_inscal_vwind,
     I     vwind_exfremo_intercept, vwind_exfremo_slope,
     U     vwind, vwind0, vwind1, vwindmask,
#ifdef USE_EXF_INTERPOLATION
     I     uwind_lon0, uwind_lon_inc, uwind_lat0, uwind_lat_inc,
     I     uwind_nlon, uwind_nlat, uwind_interpMethod,
     I     vwind_lon0, vwind_lon_inc, vwind_lat0, vwind_lat_inc,
     I     vwind_nlon, vwind_nlat, vwind_interpMethod, uvInterp_wind,
#endif /* USE_EXF_INTERPOLATION */
     I     myTime, myIter, myThid )

      if (useRelativeWind) then
C     Subtract UVEL and VVEL from UWIND and VWIND.
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),mybxhi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           uwind(i,j,bi,bj) = uwind(i,j,bi,bj) - 0.5 _d 0 *
     &          (uVel(i,j,1,bi,bj)+uVel(i+1,j,1,bi,bj))
           vwind(i,j,bi,bj) = vwind(i,j,bi,bj) - 0.5 _d 0 *
     &          (vVel(i,j,1,bi,bj)+vVel(i,j+1,1,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      endif

      ELSE
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),mybxhi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uwind(i,j,bi,bj) = 0. _d 0
           vwind(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C     Atmospheric heat flux.
      CALL EXF_SET_GEN  (
     &     hfluxfile, hfluxstartdate, hfluxperiod,
     &     exf_inscal_hflux,
     &     hflux_exfremo_intercept, hflux_exfremo_slope,
     &     hflux, hflux0, hflux1, hfluxmask,
#ifdef USE_EXF_INTERPOLATION
     &     hflux_lon0, hflux_lon_inc, hflux_lat0, hflux_lat_inc,
     &     hflux_nlon, hflux_nlat, xC, yC, hflux_interpMethod,
#endif
     &     myTime, myIter, myThid )

C     Salt flux.
      CALL EXF_SET_GEN  (
     &     sfluxfile, sfluxstartdate, sfluxperiod,
     &     exf_inscal_sflux,
     &     sflux_exfremo_intercept, sflux_exfremo_slope,
     &     sflux, sflux0, sflux1, sfluxmask,
#ifdef USE_EXF_INTERPOLATION
     &     sflux_lon0, sflux_lon_inc, sflux_lat0, sflux_lat_inc,
     &     sflux_nlon, sflux_nlat, xC, yC, sflux_interpMethod,
#endif
     &     myTime, myIter, myThid )

#ifdef ALLOW_ATM_TEMP

C     Atmospheric temperature.
      CALL EXF_SET_GEN(
     &     atempfile, atempstartdate, atempperiod,
     &     exf_inscal_atemp,
     &     atemp_exfremo_intercept, atemp_exfremo_slope,
     &     atemp, atemp0, atemp1, atempmask,
#ifdef USE_EXF_INTERPOLATION
     &     atemp_lon0, atemp_lon_inc, atemp_lat0, atemp_lat_inc,
     &     atemp_nlon, atemp_nlat, xC, yC, atemp_interpMethod,
#endif
     &     myTime, myIter, myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),mybxhi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
          atemp(i,j,bi,bj) = atemp(i,j,bi,bj) + exf_offset_atemp
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Atmospheric humidity.
      CALL EXF_SET_GEN(
     &     aqhfile, aqhstartdate, aqhperiod,
     &     exf_inscal_aqh,
     &     aqh_exfremo_intercept, aqh_exfremo_slope,
     &     aqh, aqh0, aqh1, aqhmask,
#ifdef USE_EXF_INTERPOLATION
     &     aqh_lon0, aqh_lon_inc, aqh_lat0, aqh_lat_inc,
     &     aqh_nlon, aqh_nlat, xC, yC, aqh_interpMethod,
#endif
     &     myTime, myIter, myThid )

C     Net long wave radiative flux.
      CALL EXF_SET_GEN(
     &     lwfluxfile, lwfluxstartdate, lwfluxperiod,
     &     exf_inscal_lwflux,
     &     lwflux_exfremo_intercept, lwflux_exfremo_slope,
     &     lwflux, lwflux0, lwflux1, lwfluxmask,
#ifdef USE_EXF_INTERPOLATION
     &     lwflux_lon0, lwflux_lon_inc, lwflux_lat0, lwflux_lat_inc,
     &     lwflux_nlon, lwflux_nlat, xC, yC, lwflux_interpMethod,
#endif
     &     myTime, myIter, myThid )

C     Precipitation.
      CALL EXF_SET_GEN(
     &     precipfile, precipstartdate, precipperiod,
     &     exf_inscal_precip,
     &     precip_exfremo_intercept, precip_exfremo_slope,
     &     precip, precip0, precip1, precipmask,
#ifdef USE_EXF_INTERPOLATION
     &     precip_lon0, precip_lon_inc, precip_lat0, precip_lat_inc,
     &     precip_nlon, precip_nlat, xC, yC, precip_interpMethod,
#endif
     &     myTime, myIter, myThid )

C     Snow.
      CALL EXF_SET_GEN(
     &     snowprecipfile, snowprecipstartdate, snowprecipperiod,
     &     exf_inscal_snowprecip,
     &     snowprecip_exfremo_intercept, snowprecip_exfremo_slope,
     &     snowprecip, snowprecip0, snowprecip1, snowprecipmask,
#ifdef USE_EXF_INTERPOLATION
     &     snowprecip_lon0, snowprecip_lon_inc,
     &     snowprecip_lat0, snowprecip_lat_inc,
     &     snowprecip_nlon, snowprecip_nlat, xC, yC,
     &     snowprecip_interpMethod,
#endif
     &     myTime, myIter, myThid )
C     Take care of case where total precip is not defined
      IF ( snowPrecipFile .NE. ' ' ) THEN
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),mybxhi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           precip(i,j,bi,bj) =
     &          max( precip(i,j,bi,bj), snowPrecip(i,j,bi,bj) )
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
C     Net short wave radiative flux.
      CALL EXF_SET_GEN  (
     &     swfluxfile, swfluxstartdate, swfluxperiod,
     &     exf_inscal_swflux,
     &     swflux_exfremo_intercept, swflux_exfremo_slope,
     &     swflux, swflux0, swflux1, swfluxmask,
#ifdef USE_EXF_INTERPOLATION
     &     swflux_lon0, swflux_lon_inc, swflux_lat0, swflux_lat_inc,
     &     swflux_nlon, swflux_nlat, xC, yC, swflux_interpMethod,
#endif
     &     myTime, myIter, myThid )
#endif

#ifdef EXF_READ_EVAP
C     Evaporation
      CALL EXF_SET_GEN  (
     &     evapfile, evapstartdate, evapperiod,
     &     exf_inscal_evap,
     &     evap_exfremo_intercept, evap_exfremo_slope,
     &     evap, evap0, evap1, evapmask,
#ifdef USE_EXF_INTERPOLATION
     &     evap_lon0, evap_lon_inc, evap_lat0, evap_lat_inc,
     &     evap_nlon, evap_nlat, xC, yC, evap_interpMethod,
#endif
     &     myTime, myIter, myThid )
#endif

#ifdef ALLOW_DOWNWARD_RADIATION

C     Downward shortwave radiation.
      CALL EXF_SET_GEN  (
     &     swdownfile, swdownstartdate, swdownperiod,
     &     exf_inscal_swdown,
     &     swdown_exfremo_intercept, swdown_exfremo_slope,
     &     swdown, swdown0, swdown1, swdownmask,
#ifdef USE_EXF_INTERPOLATION
     &     swdown_lon0, swdown_lon_inc, swdown_lat0, swdown_lat_inc,
     &     swdown_nlon, swdown_nlat, xC, yC, swdown_interpMethod,
#endif
     &     myTime, myIter, myThid )

C     Downward longwave radiation.
      CALL EXF_SET_GEN  (
     &     lwdownfile, lwdownstartdate, lwdownperiod,
     &     exf_inscal_lwdown,
     &     lwdown_exfremo_intercept, lwdown_exfremo_slope,
     &     lwdown, lwdown0, lwdown1, lwdownmask,
#ifdef USE_EXF_INTERPOLATION
     &     lwdown_lon0, lwdown_lon_inc, lwdown_lat0, lwdown_lat_inc,
     &     lwdown_nlon, lwdown_nlat, xC, yC, lwdown_interpMethod,
#endif
     &     myTime, myIter, myThid )

#endif

#ifdef ATMOSPHERIC_LOADING
C     Atmos. pressure forcing
      CALL EXF_SET_GEN  (
     &     apressurefile, apressurestartdate, apressureperiod,
     &     exf_inscal_apressure,
     &     apressure_exfremo_intercept, apressure_exfremo_slope,
     &     apressure, apressure0, apressure1, apressuremask,
#ifdef USE_EXF_INTERPOLATION
     &     apressure_lon0, apressure_lon_inc,
     &     apressure_lat0, apressure_lat_inc,
     &     apressure_nlon,apressure_nlat,xC,yC, apressure_interpMethod,
#endif
     &     myTime, myIter, myThid )
#endif

#ifdef EXF_SEAICE_FRACTION
C     fractional ice-covered area MASK
      CALL EXF_SET_GEN  (
     &     areamaskfile, areamaskstartdate, areamaskperiod,
     &     exf_inscal_areamask,
     &     areamask_exfremo_intercept, areamask_exfremo_slope,
     &     areamask, areamask0, areamask1, areamaskmask,
#ifdef USE_EXF_INTERPOLATION
     &     areamask_lon0, areamask_lon_inc,
     &     areamask_lat0, areamask_lat_inc,
     &     areamask_nlon, areamask_nlat, xC, yC, areamask_interpMethod,
#endif
     &     myTime, myIter, myThid )
#endif

#ifdef ALLOW_RUNOFF
C     Runoff
      CALL EXF_SET_GEN  (
     &     runofffile, runoffstartdate, runoffperiod,
     &     exf_inscal_runoff,
     &     runoff_exfremo_intercept, runoff_exfremo_slope,
     &     runoff, runoff0, runoff1, runoffmask,
#ifdef USE_EXF_INTERPOLATION
     &     runoff_lon0, runoff_lon_inc, runoff_lat0, runoff_lat_inc,
     &     runoff_nlon, runoff_nlat, xC, yC, runoff_interpMethod,
#endif
     &     myTime, myIter, myThid )
#endif /* ALLOW_RUNOFF */

#ifdef ALLOW_RUNOFTEMP
C     Runoff temperature
      CALL EXF_SET_GEN  (
     &     runoftempfile, runoffstartdate, runoffperiod,
     &     exf_inscal_runoftemp,
     &     runoftemp_exfremo_intercept, runoftemp_exfremo_slope,
     &     runoftemp, runoftemp0, runoftemp1, runoffmask,
#ifdef USE_EXF_INTERPOLATION
     &     runoff_lon0, runoff_lon_inc, runoff_lat0, runoff_lat_inc,
     &     runoff_nlon, runoff_nlat, xC, yC, runoff_interpMethod,
#endif
     &     myTime, myIter, myThid )
#endif /* ALLOW_RUNOFTEMP */

# if (!defined (ALLOW_ECCO) || defined (ECCO_CTRL_DEPRECATED))

C-- Control variables for atmos. state
#ifdef ALLOW_CTRL
      if (.NOT.ctrlUseGen) then

#ifdef ALLOW_ATEMP_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_atemp_file, xx_atempstartdate, xx_atempperiod,
     &     maskc, atemp, xx_atemp0, xx_atemp1, xx_atemp_dummy,
     &     xx_atemp_remo_intercept, xx_atemp_remo_slope,
     &     watemp, myTime, myIter, myThid )
#endif

#ifdef ALLOW_AQH_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_aqh_file, xx_aqhstartdate, xx_aqhperiod,
     &     maskc, aqh, xx_aqh0, xx_aqh1, xx_aqh_dummy,
     &     xx_aqh_remo_intercept, xx_aqh_remo_slope,
     &     waqh, myTime, myIter, myThid )
#endif

#ifdef ALLOW_PRECIP_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_precip_file, xx_precipstartdate, xx_precipperiod,
     &     maskc, precip, xx_precip0, xx_precip1, xx_precip_dummy,
     &     xx_precip_remo_intercept, xx_precip_remo_slope,
     &     wprecip, myTime, myIter, myThid )
#endif

#ifdef ALLOW_SWDOWN_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_swdown_file, xx_swdownstartdate, xx_swdownperiod,
     &     maskc, swdown, xx_swdown0, xx_swdown1, xx_swdown_dummy,
     &     xx_swdown_remo_intercept, xx_swdown_remo_slope,
     &     wswdown, myTime, myIter, myThid )
#endif

#ifdef ALLOW_LWDOWN_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_lwdown_file, xx_lwdownstartdate, xx_lwdownperiod,
     &     maskc, lwdown, xx_lwdown0, xx_lwdown1, xx_lwdown_dummy,
     &     xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     &     wlwdown, myTime, myIter, myThid )
#endif

      endif !if (.NOT.ctrlUseGen) then

#ifdef ALLOW_SWFLUX_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_swflux_file, xx_swfluxstartdate, xx_swfluxperiod,
     &     maskc, swflux, xx_swflux0, xx_swflux1, xx_swflux_dummy,
     &     xx_swflux_remo_intercept, xx_swflux_remo_slope,
     &     wswflux, myTime, myIter, myThid )
#endif

#ifdef ALLOW_LWFLUX_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_lwflux_file, xx_lwfluxstartdate, xx_lwfluxperiod,
     &     maskc, lwflux, xx_lwflux0, xx_lwflux1, xx_lwflux_dummy,
     &     xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     &     wswflux, myTime, myIter, myThid )
#endif

#ifdef ALLOW_EVAP_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_evap_file, xx_evapstartdate, xx_evapperiod,
     &     maskc, evap, xx_evap0, xx_evap1, xx_evap_dummy,
     &     xx_evap_remo_intercept, xx_evap_remo_slope,
     &     wevap, myTime, myIter, myThid )
#endif

#ifdef ALLOW_SNOWPRECIP_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_snowprecip_file, xx_snowprecipstartdate,
     &     xx_snowprecipperiod,
     &     maskc, snowprecip, xx_snowprecip0, xx_snowprecip1,
     &     xx_snowprecip_dummy,
     &     xx_snowprecip_remo_intercept, xx_snowprecip_remo_slope,
     &     wsnowprecip, myTime, myIter, myThid )
#endif

#ifdef ALLOW_APRESSURE_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_apressure_file, xx_apressurestartdate,
     &     xx_apressureperiod,
     &     maskc, apressure, xx_apressure0, xx_apressure1,
     &     xx_apressure_dummy,
     &     xx_apressure_remo_intercept, xx_apressure_remo_slope,
     &     wapressure, myTime, myIter, myThid )
#endif

      IF ( useAtmWind ) THEN
#ifndef ALLOW_ROTATE_UV_CONTROLS

#ifdef ALLOW_UWIND_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_uwind_file, xx_uwindstartdate, xx_uwindperiod,
     &     maskc, uwind, xx_uwind0, xx_uwind1, xx_uwind_dummy,
     &     xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &     wuwind, myTime, myIter, myThid )
#endif /* ALLOW_UWIND_CONTROL */

#ifdef ALLOW_VWIND_CONTROL
      CALL CTRL_GET_GEN (
     &     xx_vwind_file, xx_vwindstartdate, xx_vwindperiod,
     &     maskc, vwind, xx_vwind0, xx_vwind1, xx_vwind_dummy,
     &     xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &     wvwind, myTime, myIter, myThid )
#endif /* ALLOW_VWIND_CONTROL */

#else

#if defined(ALLOW_UWIND_CONTROL) && defined(ALLOW_VWIND_CONTROL)
        DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),mybxhi(myThid)
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

      CALL CTRL_GET_GEN (
     &     xx_uwind_file, xx_uwindstartdate, xx_uwindperiod,
     &     maskc, tmpUE, xx_uwind0, xx_uwind1, xx_uwind_dummy,
     &     xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &     wuwind, myTime, myIter, myThid )

      CALL CTRL_GET_GEN (
     &     xx_vwind_file, xx_vwindstartdate, xx_vwindperiod,
     &     maskc, tmpVN, xx_vwind0, xx_vwind1, xx_vwind_dummy,
     &     xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &     wvwind, myTime, myIter, myThid )

      CALL ROTATE_UV2EN_RL(tmpUX,tmpVY,tmpUE,tmpVN,
     &     .FALSE.,.FALSE.,.TRUE.,1,myThid)

        DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),mybxhi(myThid)
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
       DO bi = myBxLo(myThid),mybxhi(myThid)
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
cdm      &     maskc, runoff, xx_runoff0, xx_runoff1, xx_runoff_dummy,
cdm      &     xx_runoff_remo_intercept, xx_runoff_remo_slope,
cdm      &     wrunoff, 0., 0., myThid )
cdm #endif

#endif /* ALLOW_CTRL */

#endif /* undef ALLOW_ECCO) || def ECCO_CTRL_DEPRECATED */

#if (defined (ALLOW_CTRL) && defined (ALLOW_GENTIM2D_CONTROL))
      if ( useCTRL.AND.ctrlUseGen ) then
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),mybxhi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
           do iarr = 1, maxCtrlTim2D
           if (xx_gentim2d_file(iarr).EQ.'xx_atemp')
     &       atemp(i,j,bi,bj)=atemp(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           if (xx_gentim2d_file(iarr).EQ.'xx_aqh')
     &       aqh(i,j,bi,bj)=aqh(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           if (xx_gentim2d_file(iarr).EQ.'xx_swdown')
     &       swdown(i,j,bi,bj)=swdown(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           if (xx_gentim2d_file(iarr).EQ.'xx_lwdown')
     &       lwdown(i,j,bi,bj)=lwdown(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           if (xx_gentim2d_file(iarr).EQ.'xx_precip')
     &       precip(i,j,bi,bj)=precip(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           enddo
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      endif !if (ctrlUseGen) then
#endif

      RETURN
      END
