C $Header: /u/gcmpack/MITgcm/pkg/thsice/THSICE_PARAMS.h,v 1.9 2007/04/09 17:44:13 jscott Exp $
C $Name:  $

#ifdef ALLOW_THSICE
C     *==========================================================*
C     | THSICE_PARAMS.h
C     | o Header file for Therm_SeaIce package parameters:
C     |   - basic parameter ( I/O frequency, etc ...)
C     |   - physical constants (used in therm_SeaIce pkg)
C     *==========================================================*

C----------------------------------------------------------------------------
C.. Common blocks for almost everything that the sea ice model passes around.
C----------------------------------------------------------------------------

C--   COMMON / THSICE_PHYSPAR_R / physical (real) parameter
C.. densities
C     rhos      ::   density of snow (kg/m^3)
C     rhoi      ::   density of ice (kg/m^3)
C     rhosw     ::   density of seawater (kg/m^3)
C     rhofw     ::   density of fresh water (kg/m^3)
C     rhoiw     ::   ocean-ice density difference (kg/m^3)
C.. specific heats
C     cpice     ::   specific heat of fresh ice (J/kg/K)
C     cpwater   ::   specific heat of water (J/kg/K)
C .. thermal conductivity. QQ check units
C     kice      ::   thermal conductivity of pure ice (W/m/K)
C     ksnow     ::   thermal conductivity of snow (W/m/K)
C .. heat transfer coefficient
C     transcoef ::   transfer coef between ice and water (unitless)
C .. latent heat
C     Lfresh    ::   latent heat of melting of pure ice (J/kg)
C .. Enthalpy
C     qsnow     ::   snow enthalpy (J/kg)
C .. Albedo
C     albColdSnow :: albedo of cold (=dry) new snow (Tsfc < tempSnowAlb)
C     albWarmSnow :: albedo of warm (=wet) new snow (Tsfc = 0)
C     tempSnowAlb :: temperature transition from ColdSnow to WarmSnow Alb. (oC)
C     albOldSnow  :: albedo of old snow (snowAge > 35.d)
C     albIceMax   :: max albedo of bare ice (thick ice)
C     albIceMin   :: minimum ice albedo (very thin ice)
C     hAlbIce     :: ice thickness for albedo transition: thin/thick ice albedo
C     hAlbSnow    :: snow thickness for albedo transition: snow/ice albedo
C     hNewSnowAge :: new snow thickness that refresh the snow-age (by 1/e)
C     snowAgTime  :: snow aging time scale (s)
C .. Solar parameters
C     i0        ::   fraction of penetrating solar rad
C     ksolar    ::   bulk solar abs coeff of sea ice (m-1)
C .. Salinity
C     saltice   ::   salinity of ice (o/oo)
C     S_winton  ::   winton salinity of ice (o/oo)
C     mu_Tf     ::   Tf:brine salinity ratio (C/ppt)
C .. melting
C     Tf0kel    ::   Freezing temp of fresh ice in Kelvin = 273.15
C     Tmlt1     ::    melting temp; depends on S (C)
C .. Min/Max
C     himin     ::   minimum thickness for ice (m)
C     Terrmax   ::   temperature convergence precision (C)
C     hiMax     ::   Maximum thickness for ice  (m)
C     hsMax     ::   Maximum thickness for snow (m)
C .. for fractional ice
C     iceMaskmax :: maximum Ice fraction (=1 for no fractional ice)
C     iceMaskmin :: mimimum Ice fraction (=1 for no fractional ice)
C     himin0     :: minimum ice height   (=himin for no fractional ice)
C     frac_energy:: fraction of energy going to melting (=0 for no fract. ice)
C     hihig      :: ice height above which freezing only occurs over open ocean
C                                        (=large for no fractional ice)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      COMMON / THSICE_PHYSPAR_R /
     &  rhos, rhoi, rhosw, rhofw, rhoiw,
     &  cpice, cpwater,
     &  kice, ksnow,
     &  transcoef, Lfresh, qsnow,
     &  albColdSnow, albWarmSnow, tempSnowAlb,
     &  albOldSnow, hNewSnowAge, snowAgTime,
     &  albIceMax, albIceMin, hAlbIce, hAlbSnow,
     &  i0, ksolar,
     &  saltice, S_winton, mu_Tf,
     &  Tf0kel, Tmlt1,
     &  himin, Terrmax, hiMax, hsMax,
     &  iceMaskmax, iceMaskmin, himin0,
     &  frac_energy, hihig

      _RL  rhos
      _RL  rhoi
      _RL  rhosw
      _RL  rhofw
      _RL  rhoiw
      _RL  cpice
      _RL  cpwater
      _RL  kice
      _RL  ksnow
      _RL  transcoef
      _RL  Lfresh
      _RL  qsnow
      _RL  albColdSnow
      _RL  albWarmSnow
      _RL  tempSnowAlb
      _RL  albOldSnow
      _RL  hNewSnowAge
      _RL  snowAgTime
      _RL  albIceMax
      _RL  albIceMin
      _RL  hAlbIce
      _RL  hAlbSnow
      _RL  i0
      _RL  ksolar
      _RL  saltice
      _RL  S_winton
      _RL  mu_Tf
      _RL  Tf0kel
      _RL  Tmlt1
      _RL  himin
      _RL  Terrmax
      _RL  hiMax
      _RL  hsMax
      _RL iceMaskmax
      _RL iceMaskmin
      _RL himin0
      _RL frac_energy
      _RL hihig

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   COMMON / THSICE_PAR_L / ice model (logical) parameters
C     stepFwd_oceMxL        :: step forward mixed-layer T & S (slab-ocean)
C     thSIce_tave_mdsio     :: write TimeAverage output using MDSIO
C     thSIce_snapshot_mdsio :: write snap-shot output   using MDSIO
C     thSIce_mon_stdio      :: write monitor to std-outp
C     thSIce_tave_mnc       :: write TimeAverage output using MNC
C     thSIce_snapshot_mnc   :: write snap-shot output   using MNC
C     thSIce_mon_mnc        :: write monitor to netcdf file
C     thSIce_pickup_read_mnc    :: pickup read w/ MNC
C     thSIce_pickup_write_mnc   :: pickup write w/ MNC
C     thSIce_pickup_write_mdsio :: pickup write w/ MDSIO
      COMMON / THSICE_PAR_L /
     &     stepFwd_oceMxL,
     &     thSIce_tave_mdsio, thSIce_snapshot_mdsio, thSIce_mon_stdio,
     &     thSIce_tave_mnc,   thSIce_snapshot_mnc,   thSIce_mon_mnc,
     &     thSIce_pickup_read_mnc,
     &     thSIce_pickup_write_mdsio,
     &     thSIce_pickup_write_mnc

      LOGICAL stepFwd_oceMxL
      LOGICAL thSIce_tave_mdsio, thSIce_snapshot_mdsio, thSIce_mon_stdio
      LOGICAL thSIce_tave_mnc,   thSIce_snapshot_mnc,   thSIce_mon_mnc
      LOGICAL thSIce_pickup_read_mnc
      LOGICAL thSIce_pickup_write_mdsio
      LOGICAL thSIce_pickup_write_mnc

C--   COMMON / THSICE_PAR_I / ice model (integer) parameters
C     startIceModel   :: =1 : start ice model at nIter0 ; =0 : use pickup files
C                     :: -1 : start from a small pickup (without Mix.Layer)
C     nitMaxTsf       :: maximum Nb of iter to find Surface Temp (Trsf)
C     thSIceAdvScheme :: thSIce Advection scheme selector
      COMMON / THSICE_PAR_I /
     &  startIceModel, nitMaxTsf, thSIceAdvScheme

      INTEGER startIceModel
      INTEGER nitMaxTsf
      INTEGER thSIceAdvScheme

C--   COMMON / THSICE_PAR_R / ice model (real) parameters
C     thSIce_deltaT   :: ice model time-step, seaice thicken/extend [s]
C     thSIce_dtTemp   :: ice model time-step, solve4temp [s]
C     ocean_deltaT    :: ocean mixed-layer time-step [s]
C     tauRelax_MxL    :: Relaxation time scale for MixLayer T & S [s]
C     hMxL_default    :: default value for ocean MixLayer thickness [m]
C     sMxL_default    :: default value for salinity in MixLayer [psu]
C     vMxL_default    :: default value for ocean current velocity in MxL [m/s]
C     thSIce_diffK    :: thickness (horizontal) diffusivity [m^2/s]
C     stressReduction :: reduction factor for wind-stress under sea-ice [0-1]
C     thSIce_taveFreq :: Frequency^-1 for time-Aver. output [s]
C     thSIce_diagFreq :: Frequency^-1 for diagnostic output [s]
C     thSIce_monFreq  :: Frequency^-1 for monitor    output [s]
      COMMON / THSICE_PAR_R /
     &  thSIce_deltaT,  thSIce_dtTemp, ocean_deltaT, tauRelax_MxL,
     &  hMxL_default,  sMxL_default, vMxL_default,
     &  thSIce_diffK,  stressReduction,
     &  thSIce_taveFreq, thSIce_diagFreq, thSIce_monFreq

      _RL  thSIce_deltaT, thSIce_dtTemp, ocean_deltaT, tauRelax_MxL
      _RL  hMxL_default, sMxL_default, vMxL_default
      _RL  thSIce_diffK,  stressReduction
      _RL  thSIce_taveFreq, thSIce_diagFreq, thSIce_monFreq

C--   COMMON / THSICE_PAR_C / ice model (character) parameters
C     thSIceFract_InitFile :: File name for initial ice fraction
C     thSIceThick_InitFile :: File name for initial ice thickness
C     thSIceSnowH_InitFile :: File name for initial snow thickness
C     thSIceSnowA_InitFile :: File name for initial snow Age
C     thSIceEnthp_InitFile :: File name for initial ice enthalpy
C     thSIceTsurf_InitFile :: File name for initial surf. temp
      COMMON / THSICE_PAR_C /
     &  thSIceFract_InitFile,
     &  thSIceThick_InitFile,
     &  thSIceSnowH_InitFile,
     &  thSIceSnowA_InitFile,
     &  thSIceEnthp_InitFile,
     &  thSIceTsurf_InitFile
      CHARACTER*(MAX_LEN_FNAM) thSIceFract_InitFile
      CHARACTER*(MAX_LEN_FNAM) thSIceThick_InitFile
      CHARACTER*(MAX_LEN_FNAM) thSIceSnowH_InitFile
      CHARACTER*(MAX_LEN_FNAM) thSIceSnowA_InitFile
      CHARACTER*(MAX_LEN_FNAM) thSIceEnthp_InitFile
      CHARACTER*(MAX_LEN_FNAM) thSIceTsurf_InitFile

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_THSICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
