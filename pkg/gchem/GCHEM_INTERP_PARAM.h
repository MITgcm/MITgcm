CBOP
C     !ROUTINE: GCHEM_INTERP_PARAM.h
C     !INTERFACE:
C #include GCHEM_INTERP_PARAM.h

C     !DESCRIPTION:
C Contains fields and parameters for interpolating
C BGC forcing with pkg/exf interpolation routines, to be
C used with pkg/dic or pkg/bling (and maybe pkg/darwin).

C Requires: EXF_INTERP_SIZE.h

C Surface silica forcing parameters for exf

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

C PAR forcing parameters for exf

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

C Iron dust forcing parameters for exf

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

C Ice forcing parameters for exf

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

C Wind forcing parameters for exf

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

C Atmos pCO2 forcing parameters for exf

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

C Atmos pressure forcing parameters for exf

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
