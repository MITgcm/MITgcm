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

C--   COMMON /GCHEM_FIELDS_C/
      COMMON /GCHEM_FIELDS_C/
     &    exf_silicafile,
     &    exf_PARfile,
     &    exf_ironfile,
     &    exf_apCO2file

      CHARACTER*128 exf_silicafile
      CHARACTER*128 exf_PARfile
      CHARACTER*128 exf_ironfile
      CHARACTER*128 exf_apCO2file

C--   COMMON /GCHEM_FIELDS_R/
      COMMON /GCHEM_FIELDS_R/
     &    gSi0, gSi1, gchemSi,
     &    gPAR0, gPAR1, gchemPAR,
     &    giron0, giron1, gchemFe,
     &    apCO20, apCO21, gchemapCO2

      _RL gSi0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gSi1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemSi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gPAR0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gPAR1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemPAR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL giron0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL giron1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL apCO20(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL apCO21(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemapCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C Surface silica forcing parameters for exf

      _RL silicaStartTime

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
      _RL  silica_lon0
      _RL  silica_lat0
      _RL  silica_lon_inc
      _RL  silica_lat_inc(MAX_LAT_INC)
#endif

C PAR forcing parameters for exf

      _RL PARStartTime

      COMMON/gchem_forcing_PAR_c/
     &    PARmask
      COMMON/gchem_forcing_PAR_i/
     &    PARstartdate1, PARstartdate2
      COMMON/gchem_forcing_PAR_r/
     &    PARStartTime,
     &    PARperiod, PARRepCycle, PARconst,
     &    PAR_exfremo_intercept, PAR_exfremo_slope,
     &    gchem_inscal_PAR
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

C Iron forcing parameters for exf

      _RL ironStartTime

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
      _RL  iron_lon0
      _RL  iron_lat0
      _RL  iron_lon_inc
      _RL  iron_lat_inc(MAX_LAT_INC)
#endif

C Atmos pCO2 forcing parameters for exf

      _RL apCO2StartTime

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
      _RL  apCO2_lon0
      _RL  apCO2_lat0
      _RL  apCO2_lon_inc
      _RL  apCO2_lat_inc(MAX_LAT_INC)
#endif

#endif /* ALLOW_GCHEM */

