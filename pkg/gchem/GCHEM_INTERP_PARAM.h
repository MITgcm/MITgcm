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
     &    Si_nlon, Si_nlat, Si_interpMethod
      COMMON/gchem_interp_Si_r/
     &    Si_lon0, Si_lat0, Si_lon_inc,
     &    Si_lat_inc
      INTEGER Si_interpMethod, Si_nlon, Si_nlat
      _RL Si_lon0
      _RL Si_lat0
      _RL Si_lon_inc
      _RL Si_lat_inc(MAX_LAT_INC)

C PAR forcing parameters for exf

      COMMON/gchem_interp_PAR_i/
     &    par_nlon, par_nlat, par_interpMethod
      COMMON/gchem_interp_PAR_r/
     &    par_lon0, par_lat0, par_lon_inc,
     &    par_lat_inc
      INTEGER par_interpMethod, par_nlon, par_nlat
      _RL  par_lon0
      _RL  par_lat0
      _RL  par_lon_inc
      _RL  par_lat_inc(MAX_LAT_INC)

C Iron dust forcing parameters for exf

      COMMON/gchem_interp_iron_i/
     &    Fe_nlon, Fe_nlat, Fe_interpMethod
      COMMON/gchem_interp_iron_r/
     &    Fe_lon0, Fe_lat0, Fe_lon_inc,
     &    Fe_lat_inc
      INTEGER Fe_interpMethod, Fe_nlon, Fe_nlat
      _RL Fe_lon0
      _RL Fe_lat0
      _RL Fe_lon_inc
      _RL Fe_lat_inc(MAX_LAT_INC)

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
     &    apco2_nlon, apco2_nlat, apco2_interpMethod
      COMMON/gchem_interp_pCO2_r/
     &    apco2_lon0, apco2_lat0, apco2_lon_inc,
     &    apco2_lat_inc
      INTEGER apco2_interpMethod, apco2_nlon, apco2_nlat
      _RL apco2_lon0
      _RL apco2_lat0
      _RL apco2_lon_inc
      _RL apco2_lat_inc(MAX_LAT_INC)

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
