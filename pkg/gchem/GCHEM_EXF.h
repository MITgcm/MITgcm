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
     &    gchem_SiMask
      COMMON/gchem_forcing_silica_i/
     &    gchem_SiStartDate1, gchem_SiStartDate2
      COMMON/gchem_forcing_silica_r/
     &    gchem_SiStartTime,
     &    gchem_SiPeriod, gchem_SiRepCycle,
     &    gchem_Si_exfremo_intercept, gchem_Si_exfremo_slope,
     &    gchem_inscal_Si
      CHARACTER*1 gchem_SiMask
      INTEGER gchem_SiStartDate1
      INTEGER gchem_SiStartDate2
      _RL gchem_SiStartTime
      _RL gchem_SiPeriod
      _RL gchem_SiRepCycle
      _RL gchem_Si_exfremo_intercept
      _RL gchem_Si_exfremo_slope
      _RL gchem_inscal_Si

C PAR forcing parameters for exf

      COMMON/gchem_forcing_PAR_c/
     &    gchem_parMask
      COMMON/gchem_forcing_PAR_i/
     &    gchem_parStartDate1, gchem_parStartDate2
      COMMON/gchem_forcing_PAR_r/
     &    gchem_parStartTime,
     &    gchem_parPeriod, gchem_parRepCycle,
     &    gchem_par_exfremo_intercept, gchem_par_exfremo_slope,
     &    gchem_inscal_par
      _RL gchem_parStartTime
      CHARACTER*1 gchem_parMask
      INTEGER gchem_parStartDate1
      INTEGER gchem_parStartDate2
      _RL gchem_parPeriod
      _RL gchem_parRepCycle
      _RL gchem_par_exfremo_intercept
      _RL gchem_par_exfremo_slope
      _RL gchem_inscal_par

C Iron dust forcing parameters for exf

      COMMON/gchem_forcing_iron_c/
     &    gchem_FeMask
      COMMON/gchem_forcing_iron_i/
     &    gchem_FeStartDate1, gchem_FeStartDate2
      COMMON/gchem_forcing_iron_r/
     &    gchem_FeStartTime,
     &    gchem_FePeriod, gchem_FeRepCycle,
     &    gchem_Fe_exfremo_intercept, gchem_Fe_exfremo_slope,
     &    gchem_inscal_Fe
      CHARACTER*1 gchem_FeMask
      INTEGER gchem_FeStartDate1
      INTEGER gchem_FeStartDate2
      _RL gchem_FeStartTime
      _RL gchem_FePeriod
      _RL gchem_FeRepCycle
      _RL gchem_Fe_exfremo_intercept
      _RL gchem_Fe_exfremo_slope
      _RL gchem_inscal_Fe

C Ice forcing parameters for exf

      COMMON/gchem_forcing_ice_c/
     &    gchem_iceMask
      COMMON/gchem_forcing_ice_i/
     &    gchem_iceStartDate1, gchem_iceStartDate2
      COMMON/gchem_forcing_ice_r/
     &    gchem_iceStartTime,
     &    gchem_icePeriod, gchem_iceRepCycle,
     &    gchem_ice_exfremo_intercept, gchem_ice_exfremo_slope,
     &    gchem_inscal_ice
      CHARACTER*1 gchem_iceMask
      INTEGER gchem_iceStartDate1
      INTEGER gchem_iceStartDate2
      _RL gchem_iceStartTime
      _RL gchem_icePeriod
      _RL gchem_iceRepCycle
      _RL gchem_ice_exfremo_intercept
      _RL gchem_ice_exfremo_slope
      _RL gchem_inscal_ice

C Wind forcing parameters for exf

      COMMON/gchem_forcing_wind_c/
     &    gchem_windMask
      COMMON/gchem_forcing_wind_i/
     &    gchem_windStartDate1, gchem_windStartDate2
      COMMON/gchem_forcing_wind_r/
     &    gchem_windStartTime,
     &    gchem_windPeriod, gchem_windRepCycle,
     &    gchem_wind_exfremo_intercept, gchem_wind_exfremo_slope,
     &    gchem_inscal_wind
      CHARACTER*1 gchem_windMask
      INTEGER gchem_windStartDate1
      INTEGER gchem_windStartDate2
      _RL gchem_windStartTime
      _RL gchem_windPeriod
      _RL gchem_windRepCycle
      _RL gchem_wind_exfremo_intercept
      _RL gchem_wind_exfremo_slope
      _RL gchem_inscal_wind

C Atmos pCO2 forcing parameters for exf

      COMMON/gchem_forcing_apCO2_c/
     &    gchem_apco2Mask
      COMMON/gchem_forcing_apCO2_i/
     &    gchem_apco2StartDate1, gchem_apco2StartDate2
      COMMON/gchem_forcing_apCO2_r/
     &    gchem_apco2StartTime,
     &    gchem_apco2Period, gchem_apco2RepCycle,
     &    gchem_apco2_exfremo_intercept, gchem_apco2_exfremo_slope,
     &    gchem_inscal_apco2
      CHARACTER*1 gchem_apco2Mask
      INTEGER gchem_apco2StartDate1
      INTEGER gchem_apco2StartDate2
      _RL gchem_apco2StartTime
      _RL gchem_apco2Period
      _RL gchem_apco2RepCycle
      _RL gchem_apco2_exfremo_intercept
      _RL gchem_apco2_exfremo_slope
      _RL gchem_inscal_apco2

C Atmos pressure forcing parameters for exf

      COMMON/gchem_forcing_apres_c/
     &    gchem_apresMask
      COMMON/gchem_forcing_apres_i/
     &    gchem_apresStartDate1, gchem_apresStartDate2
      COMMON/gchem_forcing_apres_r/
     &    gchem_apresStartTime,
     &    gchem_apresPeriod, gchem_apresRepCycle,
     &    gchem_apres_exfremo_intercept, gchem_apres_exfremo_slope,
     &    gchem_inscal_apres
      CHARACTER*1 gchem_apresMask
      INTEGER gchem_apresStartDate1
      INTEGER gchem_apresStartDate2
      _RL gchem_apresStartTime
      _RL gchem_apresPeriod
      _RL gchem_apresRepCycle
      _RL gchem_apres_exfremo_intercept
      _RL gchem_apres_exfremo_slope
      _RL gchem_inscal_apres
