CBOP
C     !ROUTINE: BLING_EXF.h
C     !INTERFACE:
C #include BLING_EXF.h

C     !DESCRIPTION:
C Contains parameters for reading BGC forcing
C with pkg/exf routines

C Surface silica forcing parameters for exf
      COMMON/bling_forcing_silica_c/
     &    bling_silicaMask
      COMMON/bling_forcing_silica_i/
     &    bling_silicaStartDate1, bling_silicaStartDate2
      COMMON/bling_forcing_silica_r/
     &    bling_silicaStartTime,
     &    bling_silicaPeriod, bling_silicaRepCycle,
     &    bling_silica_exfremo_intercept, 
     &    bling_silica_exfremo_slope,
     &    bling_inscal_silica, bling_outscal_silica
      CHARACTER*1 bling_silicaMask
      INTEGER bling_silicaStartDate1
      INTEGER bling_silicaStartDate2
      _RL bling_silicaStartTime
      _RL bling_silicaPeriod
      _RL bling_silicaRepCycle
      _RL bling_silica_exfremo_intercept
      _RL bling_silica_exfremo_slope
      _RL bling_inscal_silica
      _RL bling_outscal_silica

C Iron dust forcing parameters for exf
      COMMON/bling_forcing_iron_c/
     &    bling_ironMask
      COMMON/bling_forcing_iron_i/
     &    bling_ironStartDate1, bling_ironStartDate2
      COMMON/bling_forcing_iron_r/
     &    bling_ironStartTime,
     &    bling_ironPeriod, bling_ironRepCycle,
     &    bling_iron_exfremo_intercept, bling_iron_exfremo_slope,
     &    bling_inscal_iron, bling_outscal_iron
      CHARACTER*1 bling_ironMask
      INTEGER bling_ironStartDate1
      INTEGER bling_ironStartDate2
      _RL bling_ironStartTime
      _RL bling_ironPeriod
      _RL bling_ironRepCycle
      _RL bling_iron_exfremo_intercept
      _RL bling_iron_exfremo_slope
      _RL bling_inscal_iron
      _RL bling_outscal_iron

C Ice forcing parameters for exf
      COMMON/bling_forcing_ice_c/
     &    bling_iceMask
      COMMON/bling_forcing_ice_i/
     &    bling_iceStartDate1, bling_iceStartDate2
      COMMON/bling_forcing_ice_r/
     &    bling_iceStartTime,
     &    bling_icePeriod, bling_iceRepCycle,
     &    bling_ice_exfremo_intercept, bling_ice_exfremo_slope,
     &    bling_inscal_ice, bling_outscal_ice
      CHARACTER*1 bling_iceMask
      INTEGER bling_iceStartDate1
      INTEGER bling_iceStartDate2
      _RL bling_iceStartTime
      _RL bling_icePeriod
      _RL bling_iceRepCycle
      _RL bling_ice_exfremo_intercept
      _RL bling_ice_exfremo_slope
      _RL bling_inscal_ice
      _RL bling_outscal_ice

C Wind forcing parameters for exf
      COMMON/bling_forcing_wind_c/
     &    bling_windMask
      COMMON/bling_forcing_wind_i/
     &    bling_windStartDate1, bling_windStartDate2
      COMMON/bling_forcing_wind_r/
     &    bling_windStartTime,
     &    bling_windPeriod, bling_windRepCycle,
     &    bling_wind_exfremo_intercept, bling_wind_exfremo_slope,
     &    bling_inscal_wind, bling_outscal_wind
      CHARACTER*1 bling_windMask
      INTEGER bling_windStartDate1
      INTEGER bling_windStartDate2
      _RL bling_windStartTime
      _RL bling_windPeriod
      _RL bling_windRepCycle
      _RL bling_wind_exfremo_intercept
      _RL bling_wind_exfremo_slope
      _RL bling_inscal_wind
      _RL bling_outscal_wind

C Atmos pCO2 forcing parameters for exf
      COMMON/bling_forcing_apco2_c/
     &    bling_apco2Mask
      COMMON/bling_forcing_apco2_i/
     &    bling_apco2StartDate1, bling_apco2StartDate2
      COMMON/bling_forcing_apco2_r/
     &    bling_apco2StartTime,
     &    bling_apco2Period, bling_apco2RepCycle,
     &    bling_apco2_exfremo_intercept, 
     &    bling_apco2_exfremo_slope,
     &    bling_inscal_apco2, bling_outscal_apco2
      CHARACTER*1 bling_apco2Mask
      INTEGER bling_apco2StartDate1
      INTEGER bling_apco2StartDate2
      _RL bling_apco2StartTime
      _RL bling_apco2Period
      _RL bling_apco2RepCycle
      _RL bling_apco2_exfremo_intercept
      _RL bling_apco2_exfremo_slope
      _RL bling_inscal_apco2
      _RL bling_outscal_apco2

C Atmos pressure forcing parameters for exf
      COMMON/bling_forcing_apress_c/
     &    bling_apressMask
      COMMON/bling_forcing_apress_i/
     &    bling_apressStartDate1, bling_apressStartDate2
      COMMON/bling_forcing_apress_r/
     &    bling_apressStartTime,
     &    bling_apressPeriod, bling_apressRepCycle,
     &    bling_apress_exfremo_intercept, 
     &    bling_apress_exfremo_slope,
     &    bling_inscal_apress, bling_outscal_apress
      CHARACTER*1 bling_apressMask
      INTEGER bling_apressStartDate1
      INTEGER bling_apressStartDate2
      _RL bling_apressStartTime
      _RL bling_apressPeriod
      _RL bling_apressRepCycle
      _RL bling_apress_exfremo_intercept
      _RL bling_apress_exfremo_slope
      _RL bling_inscal_apress
      _RL bling_outscal_apress

C ==========================================================
C   EXF interpolation needs
C ==========================================================
#ifdef ALLOW_EXF
#ifdef USE_EXF_INTERPOLATION
      _RL bling_apco2_lon0, bling_apco2_lon_inc
      _RL bling_apco2_lat0, bling_apco2_lat_inc(MAX_LAT_INC)
      _RL bling_silica_lon0, bling_silica_lon_inc
      _RL bling_silica_lat0, bling_silica_lat_inc(MAX_LAT_INC)
      _RL bling_iron_lon0, bling_iron_lon_inc
      _RL bling_iron_lat0, bling_iron_lat_inc(MAX_LAT_INC)
      _RL bling_apress_lon0, bling_apress_lon_inc
      _RL bling_apress_lat0, bling_apress_lat_inc(MAX_LAT_INC)
      _RL bling_wind_lon0, bling_wind_lon_inc
      _RL bling_wind_lat0, bling_wind_lat_inc(MAX_LAT_INC)
      _RL bling_ice_lon0, bling_ice_lon_inc
      _RL bling_ice_lat0, bling_ice_lat_inc(MAX_LAT_INC)
      INTEGER bling_apco2_nlon, bling_apco2_nlat
      INTEGER bling_apco2_interpMethod
      INTEGER bling_silica_nlon, bling_silica_nlat
      INTEGER bling_silica_interpMethod
      INTEGER bling_iron_nlon, bling_iron_nlat, bling_iron_interpMethod
      INTEGER bling_apress_nlon, bling_apress_nlat
      INTEGER bling_apress_interpMethod
      INTEGER bling_wind_nlon, bling_wind_nlat, bling_wind_interpMethod
      INTEGER bling_ice_nlon, bling_ice_nlat, bling_ice_interpMethod

      COMMON /BLG_EXF_INTERPOLATION/
     &        bling_apco2_lon0, bling_apco2_lon_inc,
     &        bling_apco2_lat0, bling_apco2_lat_inc,
     &        bling_silica_lon0, bling_silica_lon_inc,
     &        bling_silica_lat0, bling_silica_lat_inc,
     &        bling_iron_lon0, bling_iron_lon_inc,
     &        bling_iron_lat0, bling_iron_lat_inc,
     &        bling_apress_lon0, bling_apress_lon_inc,
     &        bling_apress_lat0, bling_apress_lat_inc,
     &        bling_wind_lon0, bling_wind_lon_inc,
     &        bling_wind_lat0, bling_wind_lat_inc,
     &        bling_ice_lon0, bling_ice_lon_inc,
     &        bling_ice_lat0, bling_ice_lat_inc,
     &        bling_apco2_nlon, bling_apco2_nlat,
     &        bling_apco2_interpMethod,
     &        bling_silica_nlon, bling_silica_nlat,
     &        bling_silica_interpMethod,
     &        bling_iron_nlon, bling_iron_nlat, bling_iron_interpMethod,
     &        bling_apress_nlon, bling_apress_nlat,
     &        bling_apress_interpMethod,
     &        bling_wind_nlon, bling_wind_nlat, bling_wind_interpMethod,
     &        bling_ice_nlon, bling_ice_nlat, bling_ice_interpMethod
#endif
#endif

