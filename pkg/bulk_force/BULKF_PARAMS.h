C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/BULKF_PARAMS.h,v 1.2 2006/01/22 15:51:34 jmc Exp $
C $Name:  $

#ifdef ALLOW_BULK_FORCE
C     *==========================================================*
C     | BULK_PARAMS.h
C     | o Header file for BULK_FORC package parameters:
C     |   - basic parameter ( I/O frequency, etc ...)
C     |   - physical constants (not used in therm_seaice pkg)
C     *==========================================================*

C--   COMMON / BULK_PHYS_CONST / physical constants
C.. densities
C     rhoa      ::  density of air (kg/m^3)
C     rhofw     ::  density of fresh water (kg/m^3)
C.. specific heats
C     cpair     ::  specific heat of air (J/kg/K)
C     cpwv      ::  specific heat of water vapour (J/kg/KC)
C .. latent heat
C     Lvap      ::  latent heat of vaporizn at 0 C (J/kg)
C     Lfresh    ::  latent heat of melting of pure ice (J/kg)
C .. melting
C     Tf0kel    ::  Freezing temp of fresh ice in Kelvin = 273.15
C .. wind drag
C    drag_[n]   ::  n = 1,2,3 coefficients used to evaluate drag coefficient
C .. constants
C     stefan    ::  Stefan-Boltzmann constant (W/m^2 K^4)
C     xkar      ::  Von Karman constant  QQ units?
C     Rvap      ::  gas constant for H2O vapor (J/kg/K)
C.. Miscellaneous
C     p0        ::  surface pressure (mb)
C.. Combinations used for efficiency
C    Qcoef      ::  another constant for latent heat flux
C .. for bulk formula
C     humid_fac ::  const. for the evaluation of the virtual temp.
C     saltsat   ::  reduction of sat. vapor pressure over salt water
C     gamma_blk ::  adiabatic lapse rate
C .. for Vince bulk formula QQ check units
C    Lvap_ice   ::  latent heat from sublimation
C    Rgas       ::  gas constant for dry air
C    Sha        ::
C .. emissivities
C    atm_emissivity   ::
C    ocean_emissivity ::
C    snow_emissivity  ::
C    ice_emissivity   ::
C .. for BULKF_FORMULA_AIM
C      FWIND0 :: ratio of near-sfc wind to lowest-level wind
C      CHS    :: heat exchange coefficient over sea
C      VGUST  :: wind speed for sub-grid-scale gusts
C      DTHETA :: Potential temp. gradient for stability correction
C      dTstab :: potential temp. increment for stability function derivative
C      FSTAB  :: Amplitude of stability correction (fraction)
C .. Albedo
C    ocean_albedo :: ocean surface albedo [0-1]
	
      _RL  rhoa
      _RL  rhosw
      _RL  rhofw
      _RL  cpair
      _RL  cpwv
      _RL  Lvap
      _RL  Lfresh
      _RL  Tf0kel
      _RL  cdrag_1, cdrag_2, cdrag_3
      _RL  stefan
      _RL  xkar
      _RL  Rvap
      _RL  p0
      _RL  Qcoef
      _RL  humid_fac
      _RL  saltsat
      _RL  gamma_blk
      _RL  Lvap_ice
      _RL  Rgas
      _RL  Sha
      _RL  atm_emissivity
      _RL  ocean_emissivity
      _RL  snow_emissivity
      _RL  ice_emissivity
#ifdef ALLOW_FORMULA_AIM
      _RL  FWIND0, CHS, VGUST, DTHETA, dTstab, FSTAB
#endif
      _RL  ocean_albedo

      COMMON / BULK_PHYS_CONST /
     &  rhoa, rhosw, rhofw,
     &  cpair, cpwv,
     &  Lvap, Lfresh,
     &  Tf0kel,
     &  cdrag_1, cdrag_2, cdrag_3,
     &  stefan, xkar, Rvap,
     &  p0, Qcoef,
     &  humid_fac, saltsat, gamma_blk,
     &  Lvap_ice, Rgas, Sha,
     &  atm_emissivity, ocean_emissivity,
     &  snow_emissivity, ice_emissivity,
#ifdef ALLOW_FORMULA_AIM
     &  FWIND0, CHS, VGUST, DTHETA, dTstab, FSTAB,
#endif
     &  ocean_albedo

C--   COMMON / BULK_PAR_R / real parameter

      _RL  blk_taveFreq

      COMMON / BULK_PAR_R /
     &       blk_taveFreq

C--   COMMON / BULK_PAR_L / Logical parameters
C .. for BULKF_FORMULA_AIM
C    calcWindStress     :: True to calculate Wind-Stress from surface wind
C    useFluxFormula_AIM :: set to T when using AIM flux formula rather
C                          than the default formula (LANL)

      LOGICAL calcWindStress
      LOGICAL useFluxFormula_AIM

      COMMON / BULK_PAR_L /
     &       calcWindStress,
     &       useFluxFormula_AIM

C--   COMMON / BULK_PAR_C / Character string parameters
      CHARACTER*(MAX_LEN_FNAM) AirTempFile
      CHARACTER*(MAX_LEN_FNAM) AirHumidityFile
      CHARACTER*(MAX_LEN_FNAM) RainFile
      CHARACTER*(MAX_LEN_FNAM) SolarFile
      CHARACTER*(MAX_LEN_FNAM) LongwaveFile
      CHARACTER*(MAX_LEN_FNAM) UWindFile
      CHARACTER*(MAX_LEN_FNAM) VWindFile
      CHARACTER*(MAX_LEN_FNAM) WSpeedFile
      CHARACTER*(MAX_LEN_FNAM) RunoffFile
      CHARACTER*(MAX_LEN_FNAM) QnetFile
      CHARACTER*(MAX_LEN_FNAM) EmPFile
      CHARACTER*(MAX_LEN_FNAM) CloudFile
      CHARACTER*(MAX_LEN_FNAM) SnowFile
      CHARACTER*(MAX_LEN_FNAM) airPotTempFile

      COMMON / BULK_PAR_C /
     &       AirTempFile, AirHumidityFile, RainFile,
     &       SolarFile, LongwaveFile, UWindFile, VWindFile,
     &       WSpeedFile, RunoffFile,
     &       QnetFile, EmPFile, CloudFile, SnowFile,
     &       airPotTempFile

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /*ALLOW_BULK_FORCE*/
