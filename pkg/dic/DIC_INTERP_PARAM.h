CBOP
C     !ROUTINE: DIC_INTERP_PARAM.h
C     !INTERFACE:
C #include DIC_INTERP_PARAM.h

C     !DESCRIPTION:
C Contains fields and parameters for interpolating
C BGC forcing with pkg/exf interpolation routines.

C Requires: EXF_INTERP_SIZE.h

C Surface silica forcing parameters for exf

      COMMON/dic_interp_Silicate_i/
     &    Silicate_nlon, Silicate_nlat, Silicate_interpMethod
      COMMON/dic_interp_Silicate_r/
     &    Silicate_lon0, Silicate_lat0, Silicate_lon_inc,
     &    Silicate_lat_inc
      INTEGER Silicate_interpMethod, Silicate_nlon, Silicate_nlat
      _RL Silicate_lon0
      _RL Silicate_lat0
      _RL Silicate_lon_inc
      _RL Silicate_lat_inc(MAX_LAT_INC)

C Three dimensional silica forcing parameters for exf (3D interp)
#ifdef DIC_3D_SILICA
      COMMON/dic_interp_SilicateDeep_i/
     &    SilicateDeep_nlon, SilicateDeep_nlat, SilicateDeep_interpMethod, SilicateDeep_nzin
      COMMON/dic_interp_SilicateDeep_r/
     &    SilicateDeep_lon0, SilicateDeep_lat0, SilicateDeep_lon_inc,
     &    SilicateDeep_lat_inc, SilicateDeep_dzin
      INTEGER SilicateDeep_interpMethod, SilicateDeep_nlon, SilicateDeep_nlat
      INTEGER SilicateDeep_nzin
      _RL SilicateDeep_lon0
      _RL SilicateDeep_lat0
      _RL SilicateDeep_lon_inc
      _RL SilicateDeep_lat_inc(MAX_LAT_INC)
      _RL SilicateDeep_dzin(MAX_LEV_IN)
#endif /* DIC_3D_SILICA */

C PAR forcing parameters for exf
      COMMON/dic_interp_PAR_i/
     &    par_nlon, par_nlat, par_interpMethod
      COMMON/dic_interp_PAR_r/
     &    par_lon0, par_lat0, par_lon_inc,
     &    par_lat_inc
      INTEGER par_interpMethod, par_nlon, par_nlat
      _RL  par_lon0
      _RL  par_lat0
      _RL  par_lon_inc
      _RL  par_lat_inc(MAX_LAT_INC)

C Iron dust forcing parameters for exf
      COMMON/dic_interp_iron_i/
     &    iron_nlon, iron_nlat, iron_interpMethod
      COMMON/dic_interp_iron_r/
     &    iron_lon0, iron_lat0, iron_lon_inc,
     &    iron_lat_inc
      INTEGER iron_interpMethod, iron_nlon, iron_nlat
      _RL iron_lon0
      _RL iron_lat0
      _RL iron_lon_inc
      _RL iron_lat_inc(MAX_LAT_INC)

C Ice forcing parameters for exf
      COMMON/dic_interp_ice_i/
     &    ice_nlon, ice_nlat, ice_interpMethod
      COMMON/dic_interp_ice_r/
     &    ice_lon0, ice_lat0, ice_lon_inc,
     &    ice_lat_inc
      INTEGER ice_interpMethod, ice_nlon, ice_nlat
      _RL ice_lon0
      _RL ice_lat0
      _RL ice_lon_inc
      _RL ice_lat_inc(MAX_LAT_INC)

C Wind forcing parameters for exf
      COMMON/dic_interp_wind_i/
     &    wind_nlon, wind_nlat, wind_interpMethod
      COMMON/dic_interp_wind_r/
     &    wind_lon0, wind_lat0, wind_lon_inc,
     &    wind_lat_inc
      INTEGER wind_interpMethod, wind_nlon, wind_nlat
      _RL wind_lon0
      _RL wind_lat0
      _RL wind_lon_inc
      _RL wind_lat_inc(MAX_LAT_INC)

C Atmos pCO2 forcing parameters for exf
      COMMON/dic_interp_atmpCO2_i/
     &    atmospCO2_nlon, atmospCO2_nlat, atmospCO2_interpMethod
      COMMON/dic_interp_atmpCO2_r/
     &    atmospCO2_lon0, atmospCO2_lat0, atmospCO2_lon_inc,
     &    atmospCO2_lat_inc
      INTEGER atmospCO2_interpMethod, atmospCO2_nlon, atmospCO2_nlat
      _RL atmospCO2_lon0
      _RL atmospCO2_lat0
      _RL atmospCO2_lon_inc
      _RL atmospCO2_lat_inc(MAX_LAT_INC)

C Atmos pressure forcing parameters for exf
      COMMON/dic_interp_atmosp_i/
     &    atmosp_nlon, atmosp_nlat, atmosp_interpMethod
      COMMON/dic_interp_atmosp_r/
     &    atmosp_lon0, atmosp_lat0, atmosp_lon_inc,
     &    atmosp_lat_inc
      INTEGER atmosp_interpMethod, atmosp_nlon, atmosp_nlat
      _RL atmosp_lon0
      _RL atmosp_lat0
      _RL atmosp_lon_inc
      _RL atmosp_lat_inc(MAX_LAT_INC)

C Chlorophyll forcing parameters for exf
      COMMON/dic_interp_chl_i/
     &    chl_nlon, chl_nlat, chl_interpMethod
      COMMON/dic_interp_chl_r/
     &    chl_lon0, chl_lat0, chl_lon_inc,
     &    chl_lat_inc
      INTEGER chl_interpMethod, chl_nlon, chl_nlat
      _RL chl_lon0
      _RL chl_lat0
      _RL chl_lon_inc
      _RL chl_lat_inc(MAX_LAT_INC)

C SST forcing parameters for exf
      COMMON/dic_interp_SST_i/
     &    SST_nlon, SST_nlat, SST_interpMethod
      COMMON/dic_interp_SST_r/
     &    SST_lon0, SST_lat0, SST_lon_inc,
     &    SST_lat_inc
      INTEGER SST_interpMethod, SST_nlon, SST_nlat
      _RL SST_lon0
      _RL SST_lat0
      _RL SST_lon_inc
      _RL SST_lat_inc(MAX_LAT_INC)

C SSS forcing parameters for exf
      COMMON/dic_interp_SSS_i/
     &    SSS_nlon, SSS_nlat, SSS_interpMethod
      COMMON/dic_interp_SSS_r/
     &    SSS_lon0, SSS_lat0, SSS_lon_inc,
     &    SSS_lat_inc  
      INTEGER SSS_interpMethod, SSS_nlon, SSS_nlat
      _RL SSS_lon0
      _RL SSS_lat0
      _RL SSS_lon_inc
      _RL SSS_lat_inc(MAX_LAT_INC)