#ifdef ALLOW_GCHEM

CBOP
C     !ROUTINE: GCHEM_EXF.h
C     !INTERFACE:
C #include GCHEM_EXF.h

C     !DESCRIPTION:
C Contains fields and parameters for reading
C BGC forcing with pkg/exf routines, to be
C used with pkg/dic or pkg/bling (and maybe pkg/darwin).

C Requires: EXF_OPTIONS.h
C Requires: SIZE.h

      _RL GCHEM_pCO2
      PARAMETER( GCHEM_pCO2=380.5D-6 )

C Surface silica forcing parameters for exf

      COMMON/gchem_forcing_silica_c/
     &    silicamask
      COMMON/gchem_forcing_silica_i/
     &    silicastartdate1, silicastartdate2
      COMMON/gchem_forcing_silica_r/
     &    silicaStartTime,
     &    silicaperiod, silicaRepCycle, silicaconst,
     &    silica_exfremo_intercept, silica_exfremo_slope,
     &    gchem_inscal_silica
      CHARACTER*1 silicamask
      INTEGER silicastartdate1
      INTEGER silicastartdate2
      _RL silicaStartTime
      _RL silicaperiod
      _RL silicaRepCycle
      _RL silicaconst
      _RL silica_exfremo_intercept
      _RL silica_exfremo_slope
      _RL gchem_inscal_silica
#ifdef USE_EXF_INTERPOLATION
      COMMON/gchem_interp_Si_i/
     &    silica_nlon, silica_nlat, silica_interpMethod
      COMMON/gchem_interp_Si_r/
     &    silica_lon0, silica_lat0, silica_lon_inc,
     &    silica_lat_inc
      INTEGER silica_interpMethod, silica_nlon, silica_nlat
      _RL silica_lon0
      _RL silica_lat0
      _RL silica_lon_inc
      _RL silica_lat_inc(MAX_LAT_INC)
#endif

C PAR forcing parameters for exf

      COMMON/gchem_forcing_PAR_c/
     &    PARmask
      COMMON/gchem_forcing_PAR_i/
     &    PARstartdate1, PARstartdate2
      COMMON/gchem_forcing_PAR_r/
     &    PARStartTime,
     &    PARperiod, PARRepCycle, PARconst,
     &    PAR_exfremo_intercept, PAR_exfremo_slope,
     &    gchem_inscal_PAR
      _RL PARStartTime
      CHARACTER*1 PARmask
      INTEGER PARstartdate1
      INTEGER PARstartdate2
      _RL PARperiod
      _RL PARRepCycle
      _RL PARconst
      _RL PAR_exfremo_intercept
      _RL PAR_exfremo_slope
      _RL gchem_inscal_PAR
#ifdef USE_EXF_INTERPOLATION
      COMMON/gchem_interp_PAR_i/
     &    PAR_nlon, PAR_nlat, PAR_interpMethod
      COMMON/gchem_interp_PAR_r/
     &    PAR_lon0, PAR_lat0, PAR_lon_inc,
     &    PAR_lat_inc
      INTEGER PAR_interpMethod, PAR_nlon, PAR_nlat
      _RL  PAR_lon0
      _RL  PAR_lat0
      _RL  PAR_lon_inc
      _RL  PAR_lat_inc(MAX_LAT_INC)
#endif

C Iron dust forcing parameters for exf

      COMMON/gchem_forcing_iron_c/
     &    ironmask
      COMMON/gchem_forcing_iron_i/
     &    ironstartdate1, ironstartdate2
      COMMON/gchem_forcing_iron_r/
     &    ironStartTime,
     &    ironperiod, ironRepCycle, ironconst,
     &    iron_exfremo_intercept, iron_exfremo_slope,
     &    gchem_inscal_iron
      CHARACTER*1 ironmask
      INTEGER ironstartdate1
      INTEGER ironstartdate2
      _RL ironStartTime
      _RL ironperiod
      _RL ironRepCycle
      _RL ironconst
      _RL iron_exfremo_intercept
      _RL iron_exfremo_slope
      _RL gchem_inscal_iron
#ifdef USE_EXF_INTERPOLATION
      COMMON/gchem_interp_iron_i/
     &    iron_nlon, iron_nlat, iron_interpMethod
      COMMON/gchem_interp_iron_r/
     &    iron_lon0, iron_lat0, iron_lon_inc,
     &    iron_lat_inc
      INTEGER iron_interpMethod, iron_nlon, iron_nlat
      _RL iron_lon0
      _RL iron_lat0
      _RL iron_lon_inc
      _RL iron_lat_inc(MAX_LAT_INC)
#endif

C Ice forcing parameters for exf

      COMMON/gchem_forcing_ice_c/
     &    icemask
      COMMON/gchem_forcing_ice_i/
     &    icestartdate1, icestartdate2
      COMMON/gchem_forcing_ice_r/
     &    iceStartTime,
     &    iceperiod, iceRepCycle, iceconst,
     &    ice_exfremo_intercept, ice_exfremo_slope,
     &    gchem_inscal_ice
      CHARACTER*1 icemask
      INTEGER icestartdate1
      INTEGER icestartdate2
      _RL iceStartTime
      _RL iceperiod
      _RL iceRepCycle
      _RL iceconst
      _RL ice_exfremo_intercept
      _RL ice_exfremo_slope
      _RL gchem_inscal_ice
#ifdef USE_EXF_INTERPOLATION
      COMMON/gchem_interp_ice_i/
     &    ice_nlon, ice_nlat, ice_interpMethod
      COMMON/gchem_interp_ice_r/
     &    ice_lon0, ice_lat0, ice_lon_inc,
     &    ice_lat_inc
      INTEGER ice_interpMethod, ice_nlon, ice_nlat
      _RL ice_lon0
      _RL ice_lat0
      _RL ice_lon_inc
      _RL ice_lat_inc(MAX_LAT_INC)
#endif

C Wind forcing parameters for exf

      COMMON/gchem_forcing_wind_c/
     &    windmask
      COMMON/gchem_forcing_wind_i/
     &    windstartdate1, windstartdate2
      COMMON/gchem_forcing_wind_r/
     &    windStartTime,
     &    windperiod, windRepCycle, windconst,
     &    wind_exfremo_intercept, wind_exfremo_slope,
     &    gchem_inscal_wind
      CHARACTER*1 windmask
      INTEGER windstartdate1
      INTEGER windstartdate2
      _RL windStartTime
      _RL windperiod
      _RL windRepCycle
      _RL windconst
      _RL wind_exfremo_intercept
      _RL wind_exfremo_slope
      _RL gchem_inscal_wind
#ifdef USE_EXF_INTERPOLATION
      COMMON/gchem_interp_wind_i/
     &    wind_nlon, wind_nlat, wind_interpMethod
      COMMON/gchem_interp_wind_r/
     &    wind_lon0, wind_lat0, wind_lon_inc,
     &    wind_lat_inc
      INTEGER wind_interpMethod, wind_nlon, wind_nlat
      _RL wind_lon0
      _RL wind_lat0
      _RL wind_lon_inc
      _RL wind_lat_inc(MAX_LAT_INC)
#endif

C Atmos pCO2 forcing parameters for exf

      COMMON/gchem_forcing_apCO2_c/
     &    apCO2mask
      COMMON/gchem_forcing_apCO2_i/
     &    apCO2startdate1, apCO2startdate2
      COMMON/gchem_forcing_apCO2_r/
     &    apCO2StartTime,
     &    apCO2period, apCO2RepCycle, apCO2const,
     &    apCO2_exfremo_intercept, apCO2_exfremo_slope,
     &    gchem_inscal_apCO2
      CHARACTER*1 apCO2mask
      INTEGER apCO2startdate1
      INTEGER apCO2startdate2
      _RL apCO2StartTime
      _RL apCO2period
      _RL apCO2RepCycle
      _RL apCO2const
      _RL apCO2_exfremo_intercept
      _RL apCO2_exfremo_slope
      _RL gchem_inscal_apCO2
#ifdef USE_EXF_INTERPOLATION
      COMMON/gchem_interp_pCO2_i/
     &    apCO2_nlon, apCO2_nlat, apCO2_interpMethod
      COMMON/gchem_interp_pCO2_r/
     &    apCO2_lon0, apCO2_lat0, apCO2_lon_inc,
     &    apCO2_lat_inc
      INTEGER apCO2_interpMethod, apCO2_nlon, apCO2_nlat
      _RL apCO2_lon0
      _RL apCO2_lat0
      _RL apCO2_lon_inc
      _RL apCO2_lat_inc(MAX_LAT_INC)
#endif

C Atmos pressure forcing parameters for exf

      COMMON/gchem_forcing_apres_c/
     &    apresmask
      COMMON/gchem_forcing_apres_i/
     &    apresstartdate1, apresstartdate2
      COMMON/gchem_forcing_apres_r/
     &    apresStartTime,
     &    apresperiod, apresRepCycle, apresconst,
     &    apres_exfremo_intercept, apres_exfremo_slope,
     &    gchem_inscal_apres
      CHARACTER*1 apresmask
      INTEGER apresstartdate1
      INTEGER apresstartdate2
      _RL apresStartTime
      _RL apresperiod
      _RL apresRepCycle
      _RL apresconst
      _RL apres_exfremo_intercept
      _RL apres_exfremo_slope
      _RL gchem_inscal_apres
#ifdef USE_EXF_INTERPOLATION
      COMMON/gchem_interp_pCO2_i/
     &    apres_nlon, apres_nlat, apres_interpMethod
      COMMON/gchem_interp_pCO2_r/
     &    apres_lon0, apres_lat0, apres_lon_inc,
     &    apres_lat_inc
      INTEGER apres_interpMethod, apres_nlon, apres_nlat
      _RL apres_lon0
      _RL apres_lat0
      _RL apres_lon_inc
      _RL apres_lat_inc(MAX_LAT_INC)
#endif

#endif /* ALLOW_GCHEM */
