C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_PARAMS.h,v 1.2 2003/07/31 18:22:10 jmc Exp $
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
C     land_calc_grT :: step forward ground Temperature
C     land_calc_grW :: step forward soil moiture
      COMMON /LAND_PAR_L/
     &  land_calc_grT, land_calc_grW
      LOGICAL land_calc_grT
      LOGICAL land_calc_grW

C--   COMMON /LAND_PAR_C/: Character valued parameters
C     land_grT_iniFile :: File containing initial ground Temp.
C     land_grW_iniFile :: File containing initial ground Water.
      COMMON /LAND_PAR_C/ 
     &    land_grT_iniFile, land_grW_iniFile
      CHARACTER*(MAX_LEN_FNAM) land_grT_iniFile
      CHARACTER*(MAX_LEN_FNAM) land_grW_iniFile

C--   COMMON /LAND_PAR_R/: real-type parameters
C     land_deltaT     :: land model time-step
C     land_taveFreq   :: Frequency^-1 for time-Aver. output (s)
C     land_diagFreq   :: Frequency^-1 for diagnostic output (s)
C     land_grdLambda  :: Thermal conductivity of the ground (W.m-1.K-1)
C     land_heatCs     :: Heat capacity of dry soil (J.m-3.k_1)
C     land_heatCw     :: Heat capacity of water    (J.m-3.k_1)
C     land_wTauDiff   :: soil moisture diffusion time scale (s)
C     land_waterCap   :: field capacity per meter of soil (1)
C     land_fractRunOff:: fraction of water in excess which run-off (1)
      COMMON /LAND_PAR_R/ 
     &    land_deltaT, land_taveFreq, land_diagFreq,
     &    land_grdLambda, land_heatCs, land_heatCw,
     &    land_wTauDiff, land_waterCap, land_fractRunOff

      _RL land_deltaT, land_taveFreq, land_diagFreq
      _RL land_grdLambda, land_heatCs, land_heatCw
      _RL land_wTauDiff, land_waterCap, land_fractRunOff

C--   COMMON /LAND_GRID_R/: layer dependent parameters
C     land_dzF        :: layer thickness
C     land_rec_dzC    :: reciprol vertical spacing (from center to center)
      COMMON /LAND_GRID_R/ 
     &    land_dzF, land_rec_dzC

      _RL land_dzF(land_nLev), land_rec_dzC(land_nLev)

#endif /* ALLOW_LAND */ 
