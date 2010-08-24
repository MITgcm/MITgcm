C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/BULKF_PARAMS.h,v 1.5 2010/08/24 13:56:02 jmc Exp $
C $Name:  $

#ifdef ALLOW_BULK_FORCE
C     *==========================================================*
C     | BULK_PARAMS.h
C     | o Header file for BULK_FORCE package parameters:
C     |   - basic parameter ( I/O frequency, etc ...)
C     |   - physical constants
C     *==========================================================*

C--   COMMON / BULK_PHYS_CONST / physical constants
C.. densities
C     rhoA      ::  density of air [kg/m^3]
C     rhoFW     ::  density of fresh water [kg/m^3]
C     rhoSW     ::  density of sea water [kg/m^3]
C.. specific heats
C     cpAir     ::  specific heat of air [J/kg/K]
C     cpwv      ::  specific heat of water vapour [J/kg/K]
C.. latent heat
C     Lvap      ::  latent heat of vaporization at 0.oC [J/kg]
C     Lfresh    ::  latent heat of melting of pure ice [J/kg]
C     Lvap_ice  ::  latent heat from sublimation [J/kg]
C.. constants
C     Tf0kel    ::  Freezing temp of fresh water in Kelvin = 273.15
C     Rgas      ::  gas constant for dry air   [J/kg/K]
C     Rvap      ::  gas constant for H2O vapor [J/kg/K]
C     xkar      ::  von Karman constant  [-]
C     stefan    ::  Stefan-Boltzmann constant [W/m^2/K^4]
C.. for transfer coefficient
C     zref      :: reference height [m] for transfer coefficient
C     zwd       :: height [m] of near-surface wind-speed input data
C     zth       :: height [m] of near-surface air-temp. & air-humid. input
C     cDrag_[n] ::  n = 1,2,3 coefficients used to evaluate drag coefficient
C     cStantonS,U :: coefficients used to evaluate Stanton number (for
C                 sensib. Heat Flx), under Stable / Unstable stratification
C     cDalton   :: coefficient used to evaluate Dalton number (for Evap)
C.. for bulk formula
C     umin      :: minimum wind speed used in bulk-formulae [m/s]
C     humid_fac :: dry-air - water-vapor molecular mass ratio (minus one)
C                    (used to calculate virtual temp.)
C     saltQsFac :: reduction of sat. vapor pressure over salty water
C     gamma_blk :: adiabatic lapse rate
C.. for Long-Wave Radiation
C     atm_emissivity   ::
C     ocean_emissivity ::
C     snow_emissivity  ::
C     ice_emissivity   ::
C.. for BULKF_FORMULA_AIM
C      FWIND0 :: ratio of near-sfc wind to lowest-level wind
C      CHS    :: heat exchange coefficient over sea
C      VGUST  :: wind speed for sub-grid-scale gusts
C      DTHETA :: Potential temp. gradient for stability correction
C      dTstab :: potential temp. increment for stability function derivative
C      FSTAB  :: Amplitude of stability correction (fraction)
C.. Albedo
C     ocean_albedo :: ocean surface albedo [0-1]
      _RL  rhoA
c     _RL  rhoSW
      _RL  rhoFW
      _RL  cpAir
c     _RL  cpwv
      _RL  Lvap
      _RL  Lfresh
c     _RL  Lvap_ice
      _RL  Tf0kel
      _RL  Rgas
c     _RL  Rvap
      _RL  xkar
      _RL  stefan
      _RL  zref, zwd, zth
      _RL  cDrag_1, cDrag_2, cDrag_3
      _RL  cStantonS, cStantonU
      _RL  cDalton
      _RL  umin
      _RL  humid_fac
      _RL  saltQsFac
      _RL  gamma_blk
      _RL  atm_emissivity
      _RL  ocean_emissivity
      _RL  snow_emissivity
      _RL  ice_emissivity
#ifdef ALLOW_FORMULA_AIM
      _RL  FWIND0, CHS, VGUST, DTHETA, dTstab, FSTAB
#endif
      _RL  ocean_albedo
      COMMON / BULK_PHYS_CONST /
     &  rhoA, rhoFW,
     &  cpAir, Lvap, Lfresh,
     &  Tf0kel, Rgas,
     &  xkar, stefan,
     &  zref, zwd, zth,
     &  cDrag_1, cDrag_2, cDrag_3,
     &  cStantonS, cStantonU, cDalton,
     &  umin, humid_fac, saltQsFac, gamma_blk,
     &  atm_emissivity, ocean_emissivity,
     &  snow_emissivity, ice_emissivity,
#ifdef ALLOW_FORMULA_AIM
     &  FWIND0, CHS, VGUST, DTHETA, dTstab, FSTAB,
#endif
     &  ocean_albedo

C--   COMMON / BULK_PAR_I / Integer parameters
C     blk_nIter :: Number of iterations to find turbulent transfer coeff.

      INTEGER blk_nIter
      COMMON / BULK_PAR_I /
     &       blk_nIter

C--   COMMON / BULK_PAR_R / real parameter
C     blk_taveFreq :: time-average output frequency [s]

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
      LOGICAL useQnetch, useEmPch
      COMMON / BULK_PAR_L /
     &       calcWindStress,
     &       useFluxFormula_AIM,
     &       useQnetch, useEmPch

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
