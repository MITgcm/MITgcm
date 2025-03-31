CBOP
C     !ROUTINE: GCHEM_EXF.h
C     !INTERFACE:
C #include GCHEM_EXF.h

C     !DESCRIPTION:
C Contains and parameters for reading BGC forcing
C with pkg/exf routines, to be used with
C pkg/dic or pkg/bling (and maybe pkg/darwin).

C Surface silica forcing parameters for exf

      COMMON/gchem_forcing_silica_c/
     &    silicamask
      COMMON/gchem_forcing_silica_i/
     &    silicastartdate1, silicastartdate2
      COMMON/gchem_forcing_silica_r/
     &    silicaStartTime,
     &    silicaperiod, silicaRepCycle,
     &    silica_exfremo_intercept, silica_exfremo_slope,
     &    gchem_inscal_silica
      CHARACTER*1 silicamask
      INTEGER silicastartdate1
      INTEGER silicastartdate2
      _RL silicaStartTime
      _RL silicaperiod
      _RL silicaRepCycle
      _RL silica_exfremo_intercept
      _RL silica_exfremo_slope
      _RL gchem_inscal_silica

C PAR forcing parameters for exf

      COMMON/gchem_forcing_PAR_c/
     &    PARmask
      COMMON/gchem_forcing_PAR_i/
     &    PARstartdate1, PARstartdate2
      COMMON/gchem_forcing_PAR_r/
     &    PARStartTime,
     &    PARperiod, PARRepCycle,
     &    PAR_exfremo_intercept, PAR_exfremo_slope,
     &    gchem_inscal_PAR
      _RL PARStartTime
      CHARACTER*1 PARmask
      INTEGER PARstartdate1
      INTEGER PARstartdate2
      _RL PARperiod
      _RL PARRepCycle
      _RL PAR_exfremo_intercept
      _RL PAR_exfremo_slope
      _RL gchem_inscal_PAR

C Iron dust forcing parameters for exf

      COMMON/gchem_forcing_iron_c/
     &    ironmask
      COMMON/gchem_forcing_iron_i/
     &    ironstartdate1, ironstartdate2
      COMMON/gchem_forcing_iron_r/
     &    ironStartTime,
     &    ironperiod, ironRepCycle,
     &    iron_exfremo_intercept, iron_exfremo_slope,
     &    gchem_inscal_iron
      CHARACTER*1 ironmask
      INTEGER ironstartdate1
      INTEGER ironstartdate2
      _RL ironStartTime
      _RL ironperiod
      _RL ironRepCycle
      _RL iron_exfremo_intercept
      _RL iron_exfremo_slope
      _RL gchem_inscal_iron

C Ice forcing parameters for exf

      COMMON/gchem_forcing_ice_c/
     &    icemask
      COMMON/gchem_forcing_ice_i/
     &    icestartdate1, icestartdate2
      COMMON/gchem_forcing_ice_r/
     &    iceStartTime,
     &    iceperiod, iceRepCycle,
     &    ice_exfremo_intercept, ice_exfremo_slope,
     &    gchem_inscal_ice
      CHARACTER*1 icemask
      INTEGER icestartdate1
      INTEGER icestartdate2
      _RL iceStartTime
      _RL iceperiod
      _RL iceRepCycle
      _RL ice_exfremo_intercept
      _RL ice_exfremo_slope
      _RL gchem_inscal_ice

C Wind forcing parameters for exf

      COMMON/gchem_forcing_wind_c/
     &    windmask
      COMMON/gchem_forcing_wind_i/
     &    windstartdate1, windstartdate2
      COMMON/gchem_forcing_wind_r/
     &    windStartTime,
     &    windperiod, windRepCycle,
     &    wind_exfremo_intercept, wind_exfremo_slope,
     &    gchem_inscal_wind
      CHARACTER*1 windmask
      INTEGER windstartdate1
      INTEGER windstartdate2
      _RL windStartTime
      _RL windperiod
      _RL windRepCycle
      _RL wind_exfremo_intercept
      _RL wind_exfremo_slope
      _RL gchem_inscal_wind

C Atmos pCO2 forcing parameters for exf

      COMMON/gchem_forcing_apCO2_c/
     &    apCO2mask
      COMMON/gchem_forcing_apCO2_i/
     &    apCO2startdate1, apCO2startdate2
      COMMON/gchem_forcing_apCO2_r/
     &    apCO2StartTime,
     &    apCO2period, apCO2RepCycle,
     &    apCO2_exfremo_intercept, apCO2_exfremo_slope,
     &    gchem_inscal_apCO2
      CHARACTER*1 apCO2mask
      INTEGER apCO2startdate1
      INTEGER apCO2startdate2
      _RL apCO2StartTime
      _RL apCO2period
      _RL apCO2RepCycle
      _RL apCO2_exfremo_intercept
      _RL apCO2_exfremo_slope
      _RL gchem_inscal_apCO2

C Atmos pressure forcing parameters for exf

      COMMON/gchem_forcing_apres_c/
     &    apresmask
      COMMON/gchem_forcing_apres_i/
     &    apresstartdate1, apresstartdate2
      COMMON/gchem_forcing_apres_r/
     &    apresStartTime,
     &    apresperiod, apresRepCycle,
     &    apres_exfremo_intercept, apres_exfremo_slope,
     &    gchem_inscal_apres
      CHARACTER*1 apresmask
      INTEGER apresstartdate1
      INTEGER apresstartdate2
      _RL apresStartTime
      _RL apresperiod
      _RL apresRepCycle
      _RL apres_exfremo_intercept
      _RL apres_exfremo_slope
      _RL gchem_inscal_apres
