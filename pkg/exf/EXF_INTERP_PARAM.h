CBOP
C     !ROUTINE: EXF_INTERP_PARAM.h
C     !INTERFACE:
C     include "EXF_INTERP_PARAM.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | EXF_INTERP_PARAM.h
C     | o Hold parameters used for interpolation of
C     |   EXF surface forcing fileds
C     *==========================================================*
C     *==========================================================*
C     \ev
CEOP

#ifdef USE_EXF_INTERPOLATION
C-- Interpolation parameters (for each input field):
C   {inputField}_lon0    :: longitude of the 1rst point (South-East corner)
C   {inputField}_lon_inc :: longitude increment (uniform)
C   {inputField}_lat0    :: latitude  of the 1rst point (South-East corner)
C   {inputField}_lat_inc :: latitude  increment (vector, fct of latitude only)
C   {inputField}_nlon    :: input filed 1rst dim, longitudinal direction
C   {inputField}_nlat    :: input filed 2nd  dim, latitudinal  direction
C   {inputField}_interpMethod :: interpolation method: =0 : no interpolation ;
C                             :: =1,11,21 : bilinear ; =2,12,22 : bicubic ;
C                             :: =1,2 for tracer ; =11,12 for U ; =21,22 for V.
C-  Global parameters (for all fields):
C    exf_output_interp   :: output directly interpolation result (before
C                           rescaling, rotation or time-interp)
C-  Internal parameters, for 2 components vector field:
C    uvInterp_stress     :: interpolate wind-stress u & v components together
C    uvInterp_wind       :: interpolate wind        u & v components together
C    uvInterp_climstr    :: interpolate clim stress u & v components together
C-  used to set default input-grid:
C    inp_lon0            :: default {inputField}_lon0
C    inp_dLon            :: default {inputField}_lon_inc
C    inp_lat0            :: default {inputField}_lat0
C    inp_dLat            :: default {inputField}_lat_inc
C    inp_gNx             :: default {inputField}_nlon
C    inp_gNy             :: default {inputField}_nlat
C----
      _RL inp_lon0, inp_dLon
      _RL inp_lat0, inp_dLat(MAX_LAT_INC)
      INTEGER inp_gNx, inp_gNy
      _RL ustress_lon0, ustress_lon_inc
      _RL ustress_lat0, ustress_lat_inc(MAX_LAT_INC)
      INTEGER ustress_nlon, ustress_nlat, ustress_interpMethod
      _RL vstress_lon0, vstress_lon_inc
      _RL vstress_lat0, vstress_lat_inc(MAX_LAT_INC)
      INTEGER vstress_nlon, vstress_nlat, vstress_interpMethod
      _RL hflux_lon0, hflux_lon_inc
      _RL hflux_lat0, hflux_lat_inc(MAX_LAT_INC)
      INTEGER hflux_nlon, hflux_nlat, hflux_interpMethod
      _RL sflux_lon0, sflux_lon_inc
      _RL sflux_lat0, sflux_lat_inc(MAX_LAT_INC)
      INTEGER sflux_nlon, sflux_nlat, sflux_interpMethod
      _RL swflux_lon0, swflux_lon_inc
      _RL swflux_lat0, swflux_lat_inc(MAX_LAT_INC)
      INTEGER swflux_nlon, swflux_nlat, swflux_interpMethod
      _RL runoff_lon0, runoff_lon_inc
      _RL runoff_lat0, runoff_lat_inc(MAX_LAT_INC)
      INTEGER runoff_nlon, runoff_nlat, runoff_interpMethod
      _RL saltflx_lon0, saltflx_lon_inc
      _RL saltflx_lat0, saltflx_lat_inc(MAX_LAT_INC)
      INTEGER saltflx_nlon, saltflx_nlat, saltflx_interpMethod
      _RL atemp_lon0, atemp_lon_inc
      _RL atemp_lat0, atemp_lat_inc(MAX_LAT_INC)
      INTEGER atemp_nlon, atemp_nlat, atemp_interpMethod
      _RL aqh_lon0, aqh_lon_inc
      _RL aqh_lat0, aqh_lat_inc(MAX_LAT_INC)
      INTEGER aqh_nlon, aqh_nlat, aqh_interpMethod
      _RL hs_lon0, hs_lon_inc
      _RL hs_lat0, hs_lat_inc(MAX_LAT_INC)
      INTEGER hs_nlon, hs_nlat, hs_interpMethod
      _RL hl_lon0, hl_lon_inc
      _RL hl_lat0, hl_lat_inc(MAX_LAT_INC)
      INTEGER hl_nlon, hl_nlat, hl_interpMethod
      _RL evap_lon0, evap_lon_inc
      _RL evap_lat0, evap_lat_inc(MAX_LAT_INC)
      INTEGER evap_nlon, evap_nlat, evap_interpMethod
      _RL precip_lon0, precip_lon_inc
      _RL precip_lat0, precip_lat_inc(MAX_LAT_INC)
      INTEGER precip_nlon, precip_nlat, precip_interpMethod
      _RL snowprecip_lon0, snowprecip_lon_inc
      _RL snowprecip_lat0, snowprecip_lat_inc(MAX_LAT_INC)
      INTEGER snowprecip_nlon, snowprecip_nlat, snowprecip_interpMethod
      _RL uwind_lon0, uwind_lon_inc
      _RL uwind_lat0, uwind_lat_inc(MAX_LAT_INC)
      INTEGER uwind_nlon, uwind_nlat, uwind_interpMethod
      _RL vwind_lon0, vwind_lon_inc
      _RL vwind_lat0, vwind_lat_inc(MAX_LAT_INC)
      INTEGER vwind_nlon, vwind_nlat, vwind_interpMethod
      _RL wspeed_lon0, wspeed_lon_inc
      _RL wspeed_lat0, wspeed_lat_inc(MAX_LAT_INC)
      INTEGER wspeed_nlon, wspeed_nlat, wspeed_interpMethod
      _RL lwflux_lon0, lwflux_lon_inc
      _RL lwflux_lat0, lwflux_lat_inc(MAX_LAT_INC)
      INTEGER lwflux_nlon, lwflux_nlat, lwflux_interpMethod
      _RL swdown_lon0, swdown_lon_inc
      _RL swdown_lat0, swdown_lat_inc(MAX_LAT_INC)
      INTEGER swdown_nlon, swdown_nlat, swdown_interpMethod
      _RL lwdown_lon0, lwdown_lon_inc
      _RL lwdown_lat0, lwdown_lat_inc(MAX_LAT_INC)
      INTEGER lwdown_nlon, lwdown_nlat, lwdown_interpMethod
      _RL apressure_lon0,apressure_lon_inc
      _RL apressure_lat0,apressure_lat_inc(MAX_LAT_INC)
      INTEGER apressure_nlon,apressure_nlat,apressure_interpMethod
      _RL tidePot_lon0,tidePot_lon_inc
      _RL tidePot_lat0,tidePot_lat_inc(MAX_LAT_INC)
      INTEGER tidePot_nlon,tidePot_nlat,tidePot_interpMethod
      _RL areamask_lon0,areamask_lon_inc
      _RL areamask_lat0,areamask_lat_inc(MAX_LAT_INC)
      INTEGER areamask_nlon,areamask_nlat,areamask_interpMethod

      LOGICAL exf_output_interp
      LOGICAL uvInterp_stress
      LOGICAL uvInterp_wind
      LOGICAL uvInterp_climstr
      COMMON /EXF_INTERPOLATION_L/
     & exf_output_interp,
     & uvInterp_stress, uvInterp_wind, uvInterp_climstr

      COMMON /EXF_INTERPOLATION_RL/
     & inp_lon0, inp_dLon, inp_lat0, inp_dLat,
     & ustress_lon0, ustress_lon_inc,
     & ustress_lat0, ustress_lat_inc,
     & vstress_lon0, vstress_lon_inc,
     & vstress_lat0, vstress_lat_inc,
     & hflux_lon0, hflux_lon_inc,
     & hflux_lat0, hflux_lat_inc,
     & sflux_lon0, sflux_lon_inc,
     & sflux_lat0, sflux_lat_inc,
     & swflux_lon0, swflux_lon_inc,
     & swflux_lat0, swflux_lat_inc,
     & runoff_lon0, runoff_lon_inc,
     & runoff_lat0, runoff_lat_inc,
     & saltflx_lon0, saltflx_lon_inc,
     & saltflx_lat0, saltflx_lat_inc,
     & atemp_lon0, atemp_lon_inc,
     & atemp_lat0, atemp_lat_inc,
     & aqh_lon0, aqh_lon_inc,
     & aqh_lat0, aqh_lat_inc,
     & hs_lon0, hs_lon_inc,
     & hs_lat0, hs_lat_inc,
     & hl_lon0, hl_lon_inc,
     & hl_lat0, hl_lat_inc,
     & evap_lon0, evap_lon_inc,
     & evap_lat0, evap_lat_inc,
     & precip_lon0, precip_lon_inc,
     & precip_lat0, precip_lat_inc,
     & snowprecip_lon0, snowprecip_lon_inc,
     & snowprecip_lat0, snowprecip_lat_inc,
     & uwind_lon0, uwind_lon_inc,
     & uwind_lat0, uwind_lat_inc,
     & vwind_lon0, vwind_lon_inc,
     & vwind_lat0, vwind_lat_inc,
     & wspeed_lon0, wspeed_lon_inc,
     & wspeed_lat0, wspeed_lat_inc,
     & lwflux_lon0, lwflux_lon_inc,
     & lwflux_lat0, lwflux_lat_inc,
     & swdown_lon0, swdown_lon_inc,
     & swdown_lat0, swdown_lat_inc,
     & lwdown_lon0, lwdown_lon_inc,
     & lwdown_lat0, lwdown_lat_inc,
     & apressure_lon0, apressure_lon_inc,
     & apressure_lat0, apressure_lat_inc,
     & tidePot_lon0, tidePot_lon_inc,
     & tidePot_lat0, tidePot_lat_inc,
     & areamask_lon0, areamask_lon_inc,
     & areamask_lat0, areamask_lat_inc

      COMMON /EXF_INTERPOLATION_I/
     & inp_gNx, inp_gNy,
     & ustress_nlon, ustress_nlat, ustress_interpMethod,
     & vstress_nlon, vstress_nlat, vstress_interpMethod,
     & hflux_nlon, hflux_nlat, hflux_interpMethod,
     & sflux_nlon, sflux_nlat, sflux_interpMethod,
     & swflux_nlon, swflux_nlat, swflux_interpMethod,
     & runoff_nlon, runoff_nlat, runoff_interpMethod,
     & saltflx_nlon, saltflx_nlat, saltflx_interpMethod,
     & atemp_nlon, atemp_nlat, atemp_interpMethod,
     & aqh_nlon, aqh_nlat, aqh_interpMethod,
     & hs_nlon, hs_nlat, hs_interpMethod,
     & hl_nlon, hl_nlat, hl_interpMethod,
     & evap_nlon, evap_nlat, evap_interpMethod,
     & precip_nlon, precip_nlat, precip_interpMethod,
     & snowprecip_nlon, snowprecip_nlat, snowprecip_interpMethod,
     & uwind_nlon, uwind_nlat, uwind_interpMethod,
     & vwind_nlon, vwind_nlat, vwind_interpMethod,
     & wspeed_nlon, wspeed_nlat, wspeed_interpMethod,
     & lwflux_nlon, lwflux_nlat, lwflux_interpMethod,
     & swdown_nlon, swdown_nlat, swdown_interpMethod,
     & lwdown_nlon, lwdown_nlat, lwdown_interpMethod,
     & apressure_nlon, apressure_nlat, apressure_interpMethod,
     & tidePot_nlon, tidePot_nlat, tidePot_interpMethod,
     & areamask_nlon, areamask_nlat, areamask_interpMethod

      _RL climsst_lon0, climsst_lon_inc
      _RL climsst_lat0, climsst_lat_inc(MAX_LAT_INC)
      INTEGER climsst_nlon, climsst_nlat, climsst_interpMethod
      _RL climsss_lon0, climsss_lon_inc
      _RL climsss_lat0, climsss_lat_inc(MAX_LAT_INC)
      INTEGER climsss_nlon, climsss_nlat, climsss_interpMethod
      _RL climustr_lon0, climustr_lon_inc
      _RL climustr_lat0, climustr_lat_inc(MAX_LAT_INC)
      INTEGER climustr_nlon, climustr_nlat, climustr_interpMethod
      _RL climvstr_lon0, climvstr_lon_inc
      _RL climvstr_lat0, climvstr_lat_inc(MAX_LAT_INC)
      INTEGER climvstr_nlon, climvstr_nlat, climvstr_interpMethod

      COMMON /EXF_CLIM_INTERPOLATION/
     & climsst_lon0, climsst_lon_inc,
     & climsst_lat0, climsst_lat_inc,
     & climsss_lon0, climsss_lon_inc,
     & climsss_lat0, climsss_lat_inc,
     & climustr_lon0, climustr_lon_inc,
     & climustr_lat0, climustr_lat_inc,
     & climvstr_lon0, climvstr_lon_inc,
     & climvstr_lat0, climvstr_lat_inc,
     & climsst_nlon, climsst_nlat, climsst_interpMethod,
     & climsss_nlon, climsss_nlat, climsss_interpMethod,
     & climustr_nlon, climustr_nlat, climustr_interpMethod,
     & climvstr_nlon, climvstr_nlat, climvstr_interpMethod

#endif /* USE_EXF_INTERPOLATION */
