#ifdef ALLOW_AIM
C     *==========================================================*
C     | AIM_PARAMS.h
C     | o Header file for AIM package parameters
C     |   e.g.: output/input file & parameters;
C     |         forcing & interface parameters;
C     *==========================================================*

C--   COMMON /AIM_PARM_L/ Logical valued parameters for AIM
C     aim_useFMsurfBC :: select surface B.C. from Franco Molteni
C     aim_useMMsurfFc :: select Monthly Mean surface forcing (e.g., NCEP)
C     aim_surfPotTemp :: surf.Temp input file is in Pot.Temp (aim_useMMsurfFc)
C     aim_energPrecip :: account for energy of precipitation (snow & rain temp)
C     aim_splitSIOsFx :: compute separately Sea-Ice & Ocean surf. Flux
C                 (also land SW & LW) ; default=F as in original version
C     aim_clrSkyDiag  :: compute clear-sky radiation for diagnostics
      COMMON /AIM_PARM_L/
     &     aim_useFMsurfBC, aim_useMMsurfFc, aim_surfPotTemp,
     &     aim_energPrecip, aim_splitSIOsFx, aim_clrSkyDiag,
     &     aim_snapshot_mnc, aim_snapshot_mdsio,
     &     aim_pickup_write_mnc, aim_pickup_write_mdsio,
     &     aim_pickup_read_mnc, aim_pickup_read_mdsio
      LOGICAL aim_useFMsurfBC
      LOGICAL aim_useMMsurfFc
      LOGICAL aim_surfPotTemp
      LOGICAL aim_energPrecip
      LOGICAL aim_splitSIOsFx
      LOGICAL aim_clrSkyDiag
      LOGICAL
     &     aim_snapshot_mnc, aim_snapshot_mdsio,
     &     aim_pickup_write_mnc, aim_pickup_write_mdsio,
     &     aim_pickup_read_mnc, aim_pickup_read_mdsio

C--   COMMON /AIM_PARM_C/ Character valued parameters for AIM
C     aim_LandFile :: file name for Land fraction
C     aim_MMsufx   :: sufix for all Monthly Mean surface forcing files
C     aim_albFile  :: file name for Albedo input file   (F.M. surfBC)
C     aim_vegFile  :: file name for vegetation fraction (F.M. surfBC)
C     aim_sstFile  :: file name for  Sea.Surf.Temp      (F.M. surfBC)
C     aim_lstFile  :: file name for Land.Surf.Temp      (F.M. surfBC)
C     aim_oiceFile :: file name for Sea Ice fraction    (F.M. surfBC)
C     aim_snowFile :: file name for Snow depth          (F.M. surfBC)
C     aim_swcFile  :: file name for Soil Water content  (F.M. surfBC)
C     aim_qfxFile :: file name for ocean q-flux
      COMMON /AIM_PARM_C/
     &  aim_LandFile, aim_MMsufx,
     &  aim_albFile, aim_vegFile,
     &  aim_sstFile, aim_lstFile, aim_oiceFile, aim_snowFile,
     &  aim_swcFile, aim_qfxFile
      CHARACTER*(MAX_LEN_FNAM) aim_LandFile
      CHARACTER*(MAX_LEN_FNAM) aim_MMsufx
      CHARACTER*(MAX_LEN_FNAM) aim_albFile
      CHARACTER*(MAX_LEN_FNAM) aim_vegFile
      CHARACTER*(MAX_LEN_FNAM) aim_sstFile
      CHARACTER*(MAX_LEN_FNAM) aim_lstFile
      CHARACTER*(MAX_LEN_FNAM) aim_oiceFile
      CHARACTER*(MAX_LEN_FNAM) aim_snowFile
      CHARACTER*(MAX_LEN_FNAM) aim_swcFile
      CHARACTER*(MAX_LEN_FNAM) aim_qfxFile

C--   COMMON /AIM_PARM_I/ Integer valued parameters for AIM
C     aim_surfForc_NppCycle :: Number of time period per Cycle (e.g. 12)
C     aim_MMsufxLength :: Length of sufix (Monthly Mean surf. forcing files)
C     aim_select_pCO2  :: select AIM CO2 formulation:
C                      :: =0 (default): prescribed absorp. in CO2 band (=ABLCO2)
C                      :: =1: absorption fct of prescribed pCO2 (aim_fixed_pCO2)
C                      :: =2: same as 0 with passive,  well-mixed pCO2 atm box
C                      :: =3: same as 1 with evolving, well-mixed pCO2 atm-box
C     aim_selectOceAlbedo :: select ocean albedo scheme:  =0: constant (default)
C                         :: =1 : function of Lat, Briegleb et al. 1986

      COMMON /AIM_PARM_I/
     &  aim_surfForc_NppCycle,
     &  aim_MMsufxLength,
     &  aim_select_pCO2,
     &  aim_selectOceAlbedo
      INTEGER aim_surfForc_NppCycle
      INTEGER aim_MMsufxLength
      INTEGER aim_select_pCO2
      INTEGER aim_selectOceAlbedo

C--   COMMON /AIM_PARM_R/ "Real" valued parameters for AIM
C     aim_surfForc_TimePeriod :: Length of forcing time period (e.g. 1 month)
C     aim_surfForc_TransRatio :: transition ratio from one month to the next
C     aim_dragStrato :: stratospheric-drag damping time scale (s)
C     aim_abs_pCO2 :: pCO2 dependence coeff. of CO2 band LW absortion
C     aim_ref_pCO2 :: reference pCO2 concentration (not in namelist)
C     aim_diagFreq :: Frequency^-1 for diagnostic output (s)
C     aim_tendFreq :: Frequency^-1 for tendencies output (s)
      COMMON /AIM_PARM_R/
     &  aim_surfForc_TimePeriod, aim_surfForc_TransRatio,
     &  aim_dragStrato, aim_abs_pCO2, aim_ref_pCO2,
     &  aim_diagFreq, aim_tendFreq
      _RL aim_surfForc_TimePeriod, aim_surfForc_TransRatio
      _RL aim_dragStrato
      _RL aim_abs_pCO2, aim_ref_pCO2
      _RL aim_diagFreq
      _RL aim_tendFreq

C--   COMMON /INSOLATION/
C      OBLIQ  :: Obliquity (in degree) used with ALLOW_INSOLATION
      COMMON /INSOLATION/ OBLIQ
      _RL OBLIQ

#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
