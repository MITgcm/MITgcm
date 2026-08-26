CBOP
C     !ROUTINE: DIC_EXF.h
C     !INTERFACE:
C #include DIC_EXF.h

C     !DESCRIPTION:
C Contains parameters for reading BGC forcing
C with pkg/exf routines

C Surface silica forcing parameters for exf
      COMMON/dic_forcing_silica_c/
     &    dic_silicaSurfMask
      COMMON/dic_forcing_silica_i/
     &    dic_silicaSurfStartDate1, dic_silicaSurfStartDate2
      COMMON/dic_forcing_silica_r/
     &    dic_silicaSurfStartTime,
     &    dic_silicaSurfPeriod, dic_silicaSurfRepCycle,
     &    dic_silicaSurf_exfremo_intercept, 
     &    dic_silicaSurf_exfremo_slope,
     &    dic_inscal_silicaSurf
      CHARACTER*1 dic_silicaSurfMask
      INTEGER dic_silicaSurfStartDate1
      INTEGER dic_silicaSurfStartDate2
      _RL dic_silicaSurfStartTime
      _RL dic_silicaSurfPeriod
      _RL dic_silicaSurfRepCycle
      _RL dic_silicaSurf_exfremo_intercept
      _RL dic_silicaSurf_exfremo_slope
      _RL dic_inscal_silicaSurf

      COMMON/dic_forcing_silicaDeep_c/
     &    dic_silicaDeepMask
      COMMON/dic_forcing_silicaDeep_i/
     &    dic_silicaDeepStartDate1, dic_silicaDeepStartDate2
      COMMON/dic_forcing_silicaDeep_r/
     &    dic_silicaDeepStartTime,
     &    dic_silicaDeepPeriod, dic_silicaDeepRepCycle,
     &    dic_silicaDeep_exfremo_intercept, 
     &    dic_silicaDeep_exfremo_slope,
     &    dic_inscal_silicaDeep
      CHARACTER*1 dic_silicaDeepMask
      INTEGER dic_silicaDeepStartDate1
      INTEGER dic_silicaDeepStartDate2
      _RL dic_silicaDeepStartTime
      _RL dic_silicaDeepPeriod
      _RL dic_silicaDeepRepCycle
      _RL dic_silicaDeep_exfremo_intercept
      _RL dic_silicaDeep_exfremo_slope
      _RL dic_inscal_silicaDeep

C PAR forcing parameters for exf
      COMMON/dic_forcing_PAR_c/
     &    dic_parMask
      COMMON/dic_forcing_PAR_i/
     &    dic_parStartDate1, dic_parStartDate2
      COMMON/dic_forcing_PAR_r/
     &    dic_parStartTime,
     &    dic_parPeriod, dic_parRepCycle,
     &    dic_par_exfremo_intercept, dic_par_exfremo_slope,
     &    dic_inscal_par
      _RL dic_parStartTime
      CHARACTER*1 dic_parMask
      INTEGER dic_parStartDate1
      INTEGER dic_parStartDate2
      _RL dic_parPeriod
      _RL dic_parRepCycle
      _RL dic_par_exfremo_intercept
      _RL dic_par_exfremo_slope
      _RL dic_inscal_par

C Iron dust forcing parameters for exf
      COMMON/dic_forcing_iron_c/
     &    dic_ironMask
      COMMON/dic_forcing_iron_i/
     &    dic_ironStartDate1, dic_ironStartDate2
      COMMON/dic_forcing_iron_r/
     &    dic_ironStartTime,
     &    dic_ironPeriod, dic_ironRepCycle,
     &    dic_iron_exfremo_intercept, dic_iron_exfremo_slope,
     &    dic_inscal_iron
      CHARACTER*1 dic_ironMask
      INTEGER dic_ironStartDate1
      INTEGER dic_ironStartDate2
      _RL dic_ironStartTime
      _RL dic_ironPeriod
      _RL dic_ironRepCycle
      _RL dic_iron_exfremo_intercept
      _RL dic_iron_exfremo_slope
      _RL dic_inscal_iron

C Ice forcing parameters for exf
      COMMON/dic_forcing_ice_c/
     &    dic_iceMask
      COMMON/dic_forcing_ice_i/
     &    dic_iceStartDate1, dic_iceStartDate2
      COMMON/dic_forcing_ice_r/
     &    dic_iceStartTime,
     &    dic_icePeriod, dic_iceRepCycle,
     &    dic_ice_exfremo_intercept, dic_ice_exfremo_slope,
     &    dic_inscal_ice
      CHARACTER*1 dic_iceMask
      INTEGER dic_iceStartDate1
      INTEGER dic_iceStartDate2
      _RL dic_iceStartTime
      _RL dic_icePeriod
      _RL dic_iceRepCycle
      _RL dic_ice_exfremo_intercept
      _RL dic_ice_exfremo_slope
      _RL dic_inscal_ice

C Wind forcing parameters for exf
      COMMON/dic_forcing_wind_c/
     &    dic_windMask
      COMMON/dic_forcing_wind_i/
     &    dic_windStartDate1, dic_windStartDate2
      COMMON/dic_forcing_wind_r/
     &    dic_windStartTime,
     &    dic_windPeriod, dic_windRepCycle,
     &    dic_wind_exfremo_intercept, dic_wind_exfremo_slope,
     &    dic_inscal_wind
      CHARACTER*1 dic_windMask
      INTEGER dic_windStartDate1
      INTEGER dic_windStartDate2
      _RL dic_windStartTime
      _RL dic_windPeriod
      _RL dic_windRepCycle
      _RL dic_wind_exfremo_intercept
      _RL dic_wind_exfremo_slope
      _RL dic_inscal_wind

C Atmos pCO2 forcing parameters for exf
      COMMON/dic_forcing_atmospCO2_c/
     &    dic_atmospCO2Mask
      COMMON/dic_forcing_atmospCO2_i/
     &    dic_atmospCO2StartDate1, dic_atmospCO2StartDate2
      COMMON/dic_forcing_atmospCO2_r/
     &    dic_atmospCO2StartTime,
     &    dic_atmospCO2Period, dic_atmospCO2RepCycle,
     &    dic_atmospCO2_exfremo_intercept, 
     &    dic_atmospCO2_exfremo_slope,
     &    dic_inscal_atmospCO2
      CHARACTER*1 dic_atmospCO2Mask
      INTEGER dic_atmospCO2StartDate1
      INTEGER dic_atmospCO2StartDate2
      _RL dic_atmospCO2StartTime
      _RL dic_atmospCO2Period
      _RL dic_atmospCO2RepCycle
      _RL dic_atmospCO2_exfremo_intercept
      _RL dic_atmospCO2_exfremo_slope
      _RL dic_inscal_atmospCO2

C Atmos pressure forcing parameters for exf
      COMMON/dic_forcing_atmosp_c/
     &    dic_atmospMask
      COMMON/dic_forcing_atmosp_i/
     &    dic_atmospStartDate1, dic_atmospStartDate2
      COMMON/dic_forcing_atmosp_r/
     &    dic_atmospStartTime,
     &    dic_atmospPeriod, dic_atmospRepCycle,
     &    dic_atmosp_exfremo_intercept, 
     &    dic_atmosp_exfremo_slope,
     &    dic_inscal_atmosp
      CHARACTER*1 dic_atmospMask
      INTEGER dic_atmospStartDate1
      INTEGER dic_atmospStartDate2
      _RL dic_atmospStartTime
      _RL dic_atmospPeriod
      _RL dic_atmospRepCycle
      _RL dic_atmosp_exfremo_intercept
      _RL dic_atmosp_exfremo_slope
      _RL dic_inscal_atmosp

C Chlorophyll forcing parameters for exf
      COMMON/dic_forcing_chl_c/
     &    dic_chlMask
      COMMON/dic_forcing_chl_i/
     &    dic_chlStartDate1, dic_chlStartDate2
      COMMON/dic_forcing_chl_r/
     &    dic_chlStartTime,
     &    dic_chlPeriod, dic_chlRepCycle,
     &    dic_chl_exfremo_intercept, dic_chl_exfremo_slope,
     &    dic_inscal_chl
      CHARACTER*1 dic_chlMask
      INTEGER dic_chlStartDate1
      INTEGER dic_chlStartDate2
      _RL dic_chlStartTime
      _RL dic_chlPeriod
      _RL dic_chlRepCycle
      _RL dic_chl_exfremo_intercept
      _RL dic_chl_exfremo_slope
      _RL dic_inscal_chl

C SST forcing parameters for exf
      COMMON/dic_forcing_SST_c/
     &    dic_SSTMask
      COMMON/dic_forcing_SST_i/
     &    dic_SSTStartDate1, dic_SSTStartDate2
      COMMON/dic_forcing_SST_r/
     &    dic_SSTStartTime,
     &    dic_SSTPeriod, dic_SSTRepCycle,
     &    dic_SST_exfremo_intercept, dic_SST_exfremo_slope,
     &    dic_inscal_SST
      CHARACTER*1 dic_SSTMask
      INTEGER dic_SSTStartDate1
      INTEGER dic_SSTStartDate2
      _RL dic_SSTStartTime
      _RL dic_SSTPeriod
      _RL dic_SSTRepCycle
      _RL dic_SST_exfremo_intercept
      _RL dic_SST_exfremo_slope
      _RL dic_inscal_SST

C SSS forcing parameters for exf
      COMMON/dic_forcing_SSS_c/
     &    dic_SSSMask
      COMMON/dic_forcing_SSS_i/
     &    dic_SSSStartDate1, dic_SSSStartDate2
      COMMON/dic_forcing_SSS_r/
     &    dic_SSSStartTime,
     &    dic_SSSPeriod, dic_SSSRepCycle,
     &    dic_SSS_exfremo_intercept, dic_SSS_exfremo_slope,
     &    dic_inscal_SSS
      CHARACTER*1 dic_SSSMask
      INTEGER dic_SSSStartDate1
      INTEGER dic_SSSStartDate2
      _RL dic_SSSStartTime
      _RL dic_SSSPeriod
      _RL dic_SSSRepCycle
      _RL dic_SSS_exfremo_intercept
      _RL dic_SSS_exfremo_slope
      _RL dic_inscal_SSS
      