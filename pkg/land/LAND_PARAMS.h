C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_PARAMS.h,v 1.8 2007/02/02 14:47:04 jmc Exp $
C $Name:  $

#ifdef ALLOW_LAND
C     *==========================================================*
C     | LAND_PARAMS.h
C     | o Header file for LAND package parameters:
C     |   - basic parameter ( I/O frequency, etc ...)
C     |   - physical constants
C     |   - vertical discretization
C     *==========================================================*

C--   COMMON /LAND_PAR_L/: logical parameters
C     land_calc_grT  :: step forward ground Temperature
C     land_calc_grW  :: step forward soil moiture
C     land_impl_grT  :: solve ground Temperature implicitly
C     land_calc_snow :: step forward snow thickness
C     land_calc_alb  :: compute albedo of snow over land
C     land_oldPickup :: restart from an old pickup (= before checkpoint 52l_pre)
      COMMON /LAND_PAR_L/
     &    land_calc_grT, land_calc_grW,
     &    land_impl_grT, land_calc_snow,
     &    land_calc_alb, land_oldPickup,
     &    land_timeave_mnc, land_snapshot_mnc, land_mon_mnc,
     &    land_pickup_write_mnc, land_pickup_read_mnc,
     &    land_timeave_mdsio, land_snapshot_mdsio, land_mon_stdio,
     &    land_pickup_write_mdsio
      LOGICAL land_calc_grT
      LOGICAL land_calc_grW
      LOGICAL land_impl_grT
      LOGICAL land_calc_snow
      LOGICAL land_calc_alb
      LOGICAL land_oldPickup
      LOGICAL
     &    land_timeave_mnc, land_snapshot_mnc, land_mon_mnc,
     &    land_pickup_write_mnc, land_pickup_read_mnc,
     &    land_timeave_mdsio, land_snapshot_mdsio, land_mon_stdio,
     &    land_pickup_write_mdsio

C--   COMMON /LAND_PAR_C/: Character valued parameters
C     land_grT_iniFile  :: File containing initial ground Temp.
C     land_grW_iniFile  :: File containing initial ground Water.
C     land_snow_iniFile :: File containing initial snow thickness.
      COMMON /LAND_PAR_C/
     &    land_grT_iniFile, land_grW_iniFile, land_snow_iniFile
      CHARACTER*(MAX_LEN_FNAM) land_grT_iniFile
      CHARACTER*(MAX_LEN_FNAM) land_grW_iniFile
      CHARACTER*(MAX_LEN_FNAM) land_snow_iniFile

C--   COMMON /LAND_PAR_R/: real-type parameters
C     land_deltaT     :: land model time-step
C     land_taveFreq   :: Frequency^-1 for time-Aver. output (s)
C     land_diagFreq   :: Frequency^-1 for diagnostic output (s)
C     land_monFreq    :: Frequency^-1 for monitor    output (s)
C     land_grdLambda  :: Thermal conductivity of the ground (W/m/K)
C     land_heatCs     :: Heat capacity of dry soil (J/m3/K)
C     land_CpWater    :: Heat capacity of water    (J/kg/K)
C     land_wTauDiff   :: soil moisture diffusion time scale (s)
C     land_waterCap   :: field capacity per meter of soil (1)
C     land_fractRunOff:: fraction of water in excess which run-off (1)
C     land_rhoSnow    :: density of snow (kg/m3)
C     land_rhoLiqW    :: density of liquid water (kg/m3)
C     land_Lfreez     :: Latent heat of freezing (J/kg)
C     recip_Lfreez    :: reciprol of Latent heat (kg/J)
C     land_hMaxSnow   :: Maximum snow-thickness  (m)
C     diffKsnow       :: thermal conductivity of snow (W/m/K)
C     timeSnowAge     :: snow aging time scale   (s)
C     hNewSnowAge     :: new snow thickness that refreshes snow-age (by 1/e)
C     albColdSnow     :: albedo of cold (=dry) new snow (Tsfc < tempSnowAlbL)
C     albWarmSnow     :: albedo of warm (=wet) new snow (Tsfc = 0)
C     tempSnowAlbL    :: temperature transition from ColdSnow to WarmSnow Alb. (oC)
C     albOldSnow      :: albedo of old snow (snowAge > 35.d)
C     hAlbSnow        :: snow thickness for albedo transition: snow/ground

      COMMON /LAND_PAR_R/
     &    land_deltaT, land_taveFreq, land_diagFreq, land_monFreq,
     &    land_grdLambda, land_heatCs, land_CpWater,
     &    land_wTauDiff, land_waterCap, land_fractRunOff,
     &    land_rhoLiqW,
     &    land_rhoSnow, land_Lfreez, recip_Lfreez,
     &    land_hMaxSnow, diffKsnow, timeSnowAge, hNewSnowAge,
     &    albColdSnow, albWarmSnow, tempSnowAlbL,
     &    albOldSnow, hAlbSnow

      _RL land_deltaT, land_taveFreq, land_diagFreq, land_monFreq
      _RL land_grdLambda, land_heatCs, land_CpWater
      _RL land_wTauDiff, land_waterCap, land_fractRunOff
      _RL land_rhoLiqW
      _RL land_rhoSnow, land_Lfreez, recip_Lfreez
      _RL land_hMaxSnow, diffKsnow, timeSnowAge, hNewSnowAge
      _RL albColdSnow, albWarmSnow, tempSnowAlbL
      _RL albOldSnow, hAlbSnow

C--   COMMON /LAND_GRID_R/: layer dependent parameters
C     land_dzF        :: layer thickness
C     land_rec_dzC    :: reciprol vertical spacing (from center to center)
      COMMON /LAND_GRID_R/
     &    land_dzF, land_rec_dzC

      _RL land_dzF(land_nLev), land_rec_dzC(land_nLev)

#endif /* ALLOW_LAND */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***

