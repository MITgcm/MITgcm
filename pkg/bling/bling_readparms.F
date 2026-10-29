#include "BLING_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
      SUBROUTINE BLING_READPARMS( myThid )

C     *========================================================*
C     | subroutine bling_readparms
C     | o Initialise and read parameters for BLING model
C     *========================================================*

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BLING_VARS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
# ifdef USE_EXF_INTERPOLATION
c#  ifdef ALLOW_EXCH2
c#   include "W2_EXCH2_SIZE.h"
c#   include "W2_EXCH2_TOPOLOGY.h"
c#  endif /* ALLOW_EXCH2 */
#  include "SET_GRID.h"
#  include "EXF_INTERP_SIZE.h"
#  include "EXF_INTERP_PARAM.h"
# endif /* USE_EXF_INTERPOLATION */
# include "BLING_EXF.h"
#endif /* ALLOW_EXF */

C     === Routine arguments ===
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING

C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     errCount  :: error counter
C     iUnit     :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
      INTEGER iUnit
      _RL secperday
#ifdef ALLOW_EXF
C     Local helper parameters that can be set in case we want to use
C       pkg/exf machinery for bling fields with bling_useEXF=T but useEXF=F 
C.      (physical model not using pkg/exf)
      INTEGER bling_exf_iprec, bling_exf_debugLev
      LOGICAL bling_useExfYearlyFields, bling_twoDigitYear
      LOGICAL prtBlkLn, addBlkLn
# ifdef USE_EXF_INTERPOLATION
      LOGICAL bling_exf_output_interp
      INTEGER j
# endif
#endif

C ==========================================================
C   Abiotic parameters
C  selectBTconst :: estimates borate concentration from salinity
C     selectBTconst=1 for the default formulation of UppstrÃ¶1974) i.e.
C     the same as S/R CARBON_COEFFS
C     selectBTconst=2 for the new formulation from Lee et al (2010)
C  selectFTconst :: estimates fluoride concentration from salinity
C     selectFTconst=1 for the default formulation of Riley (1965) i.e.
C     the same as S/R CARBON_COEFFS
C     selectFTconst=2 for the new formulation from Culkin (1965)
C  selectHFconst :: sets the first dissociation constant for hydrogen
C  fluoride:
C     selectHFconst=1 for the default  Dickson and Riley (1979) i.e. the
C     same as S/R CARBON_COEFFS
C     selectHFconst=2 for the formulation of Perez and Fraga (1987)
C  selectK1K2const :: sets the first and second dissociation constants
C  of carbonic acid:
C     selectK1K2const=1 for the default formulation of Millero (1995)
C     with data from Mehrbach et al. (1973),
C          i.e. the same as S/R CARBON_COEFFS
C     selectK1K2const=2 for the formulation of Roy et al. (1993)
C     selectK1K2const=3 for the "combination" formulation of Millero
C     (1995)
C     selectK1K2const=4 for the formulation of Luecker et al. (2000)
C     selectK1K2const=5 for the formulation of Millero (2010, Mar. Fresh
C     Wat. Res.)
C     selectK1K2const=6 for the formulation of Waters, Millero, Woosley
C     (2014, Mar. Chem.)
C  selectPHsolver  :: sets the pH solver to use
C     selectPHsolver=0 for Follows et al., (2006)
C     selectPHsolver=1 for Munhoven (2013) solvesaphe general
C     selectPHsolver=2 for Munhoven (2013) solvesaphe sec
C     selectPHsolver=3 for Munhoven (2013) solvesaphe fast
C ==========================================================

      NAMELIST /ABIOTIC_PARMS/
     &                      bling_debugLevel,
     &                      permil,
     &                      Pa2Atm,
     &                      epsln
#ifdef CARBONCHEM_SOLVESAPHE
     & ,selectBTconst,selectFTconst,
     &  selectHFconst,selectK1K2const,
     &  selectPHsolver
#endif

C ==========================================================
C   BLING parameters
C ==========================================================

      NAMELIST /BIOTIC_PARMS/
     &                     CtoN,
     &                     CtoP,
     &                     NtoP,
     &                     HtoC,
     &                     NO3toN,
     &                     O2toN,
     &                     O2toP,
     &                     CatoN,
     &                     CatoP,
     &                     masstoN,
#ifndef USE_BLING_V1
     &                     Pc_0_diaz,
     &                     alpha_photo,
     &                     gamma_DON,
     &                     k_Fe_diaz,
     &                     k_NO3,
     &                     k_PtoN,
     &                     k_FetoN,
     &                     k_NO3_sm,
     &                     k_NO3_lg,
     &                     k_PO4_sm,
     &                     k_PO4_lg,
     &                     k_Fe_sm,
     &                     k_Fe_lg,
     &                     PtoN_min,
     &                     PtoN_max,
     &                     FetoN_min,
     &                     FetoN_max,
     &                     kappa_eppley_diaz,
     &                     phi_dvm,
     &                     sigma_dvm,
#ifdef USE_SIBLING
     &                     k_Si,
     &                     gamma_Si_0,
     &                     kappa_remin_Si,
     &                     wsink_Si,
     &                     SitoN_uptake_min,
     &                     SitoN_uptake_max,
     &                     SitoN_uptake_scale,
     &                     SitoN_uptake_exp,
     &                     q_SitoN_diss,
#endif
#else
     &                     alpha_max,
     &                     alpha_min,
     &                     gamma_biomass,
     &                     k_FetoP,
     &                     FetoP_max,
     &                     Fe_lim_min,
#endif
     &                     pivotal,
     &                     Pc_0,
     &                     lambda_0,
     &                     resp_frac,
     &                     chl_min,
     &                     theta_Fe_max_hi,
     &                     theta_Fe_max_lo,
     &                     gamma_irr_mem,
     &                     gamma_DOP,
     &                     gamma_POM,
     &                     k_Fe,
     &                     k_O2,
     &                     k_PO4,
     &                     kFe_eq_lig_max,
     &                     kFe_eq_lig_min,
     &                     kFe_eq_lig_Femin,
     &                     kFe_eq_lig_irr,
     &                     kFe_org,
     &                     kFe_inorg,
     &                     FetoC_sed,
     &                     remin_min,
     &                     oxic_min,
     &                     ligand,
     &                     kappa_eppley,
     &                     kappa_remin,
     &                     ca_remin_depth,
     &                     phi_DOM,
     &                     phi_sm,
     &                     phi_lg,
     &                     wsink0z,
     &                     wsink0,
     &                     wsinkacc,
     &                     parfrac,
     &                     alpfe,
     &                     k0,
     &                     MLmix_max,
     &                     chlsat_locTimWindow,
     &                     river_conc_po4,
     &                     river_dom_to_nut

      NAMELIST /BLING_FORCING/
     &          bling_useEXF,
     &          bling_windFile, bling_apressFile, bling_iceFile,
     &          bling_ironFile, bling_silicaFile,
     &          bling_psmFile, bling_plgFile, bling_PdiazFile,
     &          bling_forcingPeriod, bling_forcingCycle,
     &          bling_Pc_2dFile, bling_Pc_2d_diazFile,
     &          bling_k0_2dFile,
     &          bling_alpha_photo2dFile,bling_phi_DOM2dFile,
     &          bling_k_Fe2dFile, bling_k_Fe_diaz2dFile,
     &          bling_gamma_POM2dFile, bling_wsink0_2dFile,
     &          bling_phi_sm2dFile,bling_phi_lg2dFile,
     &          bling_pCO2,
     &          river_conc_po4, river_dom_to_nut
#ifdef ALLOW_EXF
     &        , bling_exf_iprec, bling_exf_debugLev,
     &          bling_useExfYearlyFields, bling_twoDigitYear,
     &          bling_apco2File, bling_apco2StartDate1,
     &          bling_apco2StartDate2,
     &          bling_apco2RepCycle, bling_apco2Period,
     &          bling_apco2StartTime,
     &          bling_inscal_apco2, bling_outscal_apco2,
     &          bling_apco2Const,
     &          bling_apco2_exfremo_intercept,
     &          bling_apco2_exfremo_slope,
     &          bling_silicaStartDate1, bling_silicaStartDate2,
     &          bling_silicaRepCycle, bling_silicaPeriod,
     &          bling_silicaStartTime,
     &          bling_inscal_silica, bling_outscal_silica,
     &          bling_silicaConst,
     &          bling_silica_exfremo_intercept,
     &          bling_silica_exfremo_slope,
     &          bling_ironStartDate1, bling_ironStartDate2,
     &          bling_ironRepCycle, bling_ironPeriod,
     &          bling_ironStartTime,
     &          bling_inscal_iron, bling_outscal_iron, bling_ironConst,
     &          bling_iron_exfremo_intercept, bling_iron_exfremo_slope,
     &          bling_windStartDate1, bling_windStartDate2,
     &          bling_windRepCycle, bling_windPeriod,
     &          bling_windStartTime,
     &          bling_inscal_wind, bling_outscal_wind,
     &          bling_windConst,
     &          bling_wind_exfremo_intercept,
     &          bling_wind_exfremo_slope,
     &          bling_iceStartDate1, bling_iceStartDate2,
     &          bling_iceRepCycle, bling_icePeriod,
     &          bling_iceStartTime,
     &          bling_inscal_ice, bling_outscal_ice,
     &          bling_iceConst,
     &          bling_ice_exfremo_intercept,
     &          bling_ice_exfremo_slope,
     &          bling_apressStartDate1, bling_apressStartDate2,
     &          bling_apressRepCycle, bling_apressPeriod,
     &          bling_apressStartTime,
     &          bling_inscal_apress, bling_outscal_apress,
     &          bling_apressConst,
     &          bling_apress_exfremo_intercept,
     &          bling_apress_exfremo_slope
#ifdef USE_EXF_INTERPOLATION
     &         ,bling_apco2_lon0, bling_apco2_lon_inc,
     &          bling_apco2_lat0, bling_apco2_lat_inc,
     &          bling_apco2_nlon, bling_apco2_nlat,
     &          bling_apco2_interpMethod,
     &          bling_silica_lon0, bling_silica_lon_inc, 
     &          bling_silica_lat0, bling_silica_lat_inc,
     &          bling_silica_nlon, bling_silica_nlat,
     &          bling_silica_interpMethod,
     &          bling_iron_lon0, bling_iron_lon_inc,
     &          bling_iron_lat0, bling_iron_lat_inc,
     &          bling_iron_nlon, bling_iron_nlat,
     &          bling_iron_interpMethod,
     &          bling_wind_lon0, bling_wind_lon_inc,
     &          bling_wind_lat0, bling_wind_lat_inc,
     &          bling_wind_nlon, bling_wind_nlat,
     &          bling_wind_interpMethod,
     &          bling_ice_lon0, bling_ice_lon_inc,
     &          bling_ice_lat0, bling_ice_lat_inc,
     &          bling_ice_nlon, bling_ice_nlat,
     &          bling_ice_interpMethod,
     &          bling_apress_lon0, bling_apress_lon_inc,
     &          bling_apress_lat0, bling_apress_lat_inc,
     &          bling_apress_nlon, bling_apress_nlat,
     &          bling_apress_interpMethod
#endif /* USE_EXF_INTERPOLATION */
#endif /* ALLOW_EXF */

C ==========================================================
C   secperday        :: Seconds in a day = 24*60*60
C   permil           :: Set carbon mol/m3 <---> mol/kg conversion factor
C                       Default permil = 1024.5 kg/m3
C   Pa2Atm           :: Conversion factor for atmospheric pressure from EXF
C                       1 Atm = 1.01325e5 Pa = 1013.25 mb
C   epsln            :: A very small number
C   CtoN             :: Carbon to nitrogen ratio in organic matter
C   CtoP             :: Carbon to phosphorus ratio in organic matter
C   NtoP             :: Nitrogen to phosphorus ratio in organic matter
C   HtoC             :: Reduced hydrogen to carbon ratio in organic matter
C   NO3toN           :: Nitrate to organic nitrogen ratio for denitrification
C   O2toN            :: Oxygen to nitrogen for biological activity
C   O2toP            :: Oxygen to phosphorus for biological activity
C   CatoN            :: Calcium to nitrogen uptake by small phyto
C   CatoP            :: Calcium to phosphorus uptake by small phyto
C   masstoN          :: Organic matter mass to nitrogen ratio
C   pivotal          :: Pivotal phytoplankton biomass, for grazing law
C                       [mol P m-3  or  mol N m-3]
C   Pc_0             :: Maximum phytoplankton carbon-specific growth rate at 0C
C   Pc_0_diaz        :: Maximum diazotroph carbon-specific growth rate at 0C
C   lambda_0         :: Total mortality rate constant
C   resp_frac        :: Fraction of production that is respired
C   chl_min          :: Minimum chl concentration allowed (for numerical stability)
C   alpha_photo      :: Quantum yield under low light, Fe-replete
C                       [g C g Chl-1 m2 W-1 s-1]
C   alpha_max        :: Quantum yield under low light, abundant iron
C   alpha_min        :: Quantum yield under low light, iron limited
C   theta_Fe_max_hi  :: Maximum Chl:C ratio when iron-replete
C   theta_Fe_max_lo  :: Maximum Chl:C ratio when iron-limited
C   gamma_irr_mem    :: Photoadaptation timescale
C   gamma_DON        :: Dissolved organic nitrogen decay timescale
C   gamma_DOP        :: Dissolved organic phosphorus decay timescale
C   gamma_POM        :: Particulate Organic Matter decay timescale
C   gamma_biomass    :: Biomass adjustment time scale
C   gamma_Si_0       :: Opal dissolution timescale
C   k_Fe             :: Iron half-saturation concentration
C   k_Fe_diaz        :: Iron half-saturation concentration for diazotrophs
C   k_O2             :: Oxygen half-saturation concentration for aerobic respiration
C   k_PO4            :: Phosphate half-saturation concentration
C   k_NO3            :: Nitrate half-saturation concentration
C   k_Si             :: Silicate half-saturation concentration
C   k_PtoN           :: Phosphate half-saturation concentration for uptake ratio
C   k_FetoN          :: Iron half-saturation concentration for uptake ratio
C   k_FetoP          :: Iron half-saturation concentration for uptake ratio
C   kFe_eq_lig_max   :: Maximum light-dependent iron ligand stability constant
C   kFe_eq_lig_min   :: Minimum light-dependent iron ligand stability constant
C   kFe_eq_lig_Femin :: Low-iron threshold for ligand stability constant
C   kFe_eq_lig_irr   :: Irradiance scaling for iron ligand stability constant
C   kFe_org          :: Iron scavenging, 1st order
C   kFe_inorg        :: Iron scavenging, 2nd order
C   PtoN_max         :: Phosphorus to Nitrogen uptake ratio maximum
C   PtoN_min         :: Phosphorus to Nitrogen uptake ratio minimum
C   FetoN_max        :: Iron to Nitrogen uptake ratio maximum
C   FetoN_min        :: Iron to Nitrogen uptake ratio minimum
C   FetoC_sed        :: Iron released per organic carbon delivery to sediments
C   FetoP_max        :: Iron to Phosphorus uptake ratio maximum
C   Fe_lim_min       :: Minimum iron limitation
C   SitoN_uptake_max :: Silica to Nitrogen uptake ratio maximum
C   SitoN_uptake_min :: Silica to Nitrogen uptake ratio minimum
C   SitoN_uptake_scale:: Scale factor for Silica to Nitrogen uptake ratio
C   SitoN_uptake_exp :: Exponent for Silica to Nitrogen uptake ratio
C   remin_min        :: Minimum remineralization under anoxia
C   oxic_min         :: Anaerobic respiration threshold
C   ligand           :: Iron ligand concentration
C   kappa_eppley     :: Temperature dependence of growth
C   kappa_eppley_diaz:: Temperature dependence of growth for diazotrophs
C   kappa_remin      :: Temperature dependence for particle fractionation
C   kappa_remin_Si   :: Temperature dependence for silica
C   ca_remin_depth   :: CaCO3 dissolution length scale (subject to omega)
C   q_SitoN_diss     :: Stoichiometric role in opal dissolution, following TOPAZ
C   phi_DOM          :: Fraction of uptake/consumption that becomes DOM
C   phi_sm           :: Fraction of small phytoplankton biomass converted to detritus
C   phi_lg           :: Fraction of large phytoplankton biomass converted to detritus
C   phi_dvm          :: Fraction of particate production exported by animals
C   sigma_dvm        :: Standard deviation of vertical migrator resting depth range
C   wsink0           :: Sinking rate at surface
C   wsink0z          :: Depth to which sinking rate remains constant
C   wsinkacc         :: Sinking rate acceleration with depth
C   wsink_si         :: Opal sinking velocity
C   parfrac          :: Fraction of Qsw available for photosynthesis
C   alpfe            :: Solubility of aeolian iron
C   k0               :: Light attentuation coefficient
C   MLmix_max        :: Maximum depth over which light and biomass can be mixed
C   river_conc_po4   :: Phosphate concentration in river runoff
C   river_dom_to_nut :: DOM to PO4 ratio in river runoff
C   chlsat_locTimWindow(1:2) :: local-time window (in h) for
C                       satellite-equivalent chlorophyll diagnostic (and cost)

      _BEGIN_MASTER(myThid)
      errCount = 0

C ==========================================================
C     Default values

      secperday            = 86400. _d 0
      permil               = 1. _d 0 / 1024.5 _d 0
      Pa2Atm               = 1.01325 _d 5
      epsln                = 1. _d -30
#ifdef CARBONCHEM_SOLVESAPHE
       selectBTconst   = 1
       selectFTconst   = 1
       selectHFconst   = 1
       selectK1K2const = 1
       selectPHsolver  = 0
#endif

      CtoN                 = 6.75 _d 0
      CtoP                 = 106. _d 0
      NtoP                 = 16. _d 0
      HtoC                 = 48. _d 0 / 106. _d 0
      NO3toN               = CtoN * (1. _d 0 + 0.25 _d 0 * HtoC)
     &                        * 0.8 _d 0 + 0.6 _d 0
      O2toN                = CtoN * (1. _d 0 + 0.25 _d 0 * HtoC)
     &                        + 2. _d 0
      O2toP                = 150. _d 0
      CatoN                = CtoN * 0.015 _d 0
      CatoP                = 106.0 _d 0 * 0.015 _d 0
      masstoN              = CtoN * 12.001 _d 0
#ifndef USE_BLING_V1
      pivotal              = 1.9 _d -3 / 1028. _d 0 / CtoN / permil
      Pc_0                 = 1.7 _d -5
      Pc_0_diaz            = 0.01 _d -5
      alpha_photo          = 0.7 _d -5 * 2.77 _d 18 / 6.022 _d 17
      gamma_DON            = 0.25 _d 0 / (365.25 _d 0 * secperday)
      gamma_DOP            = 0.5 _d 0 / (365.25 _d 0 * secperday)
      PtoN_min             = 1. / 28.
      PtoN_max             = 1. / 9.
      FetoN_min            = 2. _d -6 * 6.75
      FetoN_max            = 25. _d -6 * 6.75
      k_Fe                 = 1.6 _d -10 / permil
      k_Fe_diaz            = 7. _d -10 / permil
      k_NO3                = 2. _d -6 / permil
      k_PO4                = 1. _d -8 / permil
      k_NO3_sm             = 2.0 _d -6 / permil
      k_NO3_lg             = 6.0 _d -6 / permil
      k_PO4_sm             = 1.0 _d -8 / permil
      k_PO4_lg             = 3.0 _d -8 / permil
      k_Fe_sm              = 1.6 _d -10 / permil
      k_Fe_lg              = 4.8 _d -10 / permil
      k_PtoN               = 1.5 _d -6 / permil
      k_FetoN              = 8. _d -10 / permil
      remin_min            = 0.15 _d 0
      kappa_eppley_diaz    = 0.18 _d 0
      sigma_dvm            = 40.0 _d 0
      phi_dvm              = 0.2 _d 0
#ifdef USE_SIBLING
      k_Si                 = 2.0 _d -6 / permil
      gamma_Si_0           = 0.05 / secperday
      kappa_remin_Si       = 0.075
      wsink_Si             = 100. / secperday
      SitoN_uptake_min     = 4
      SitoN_uptake_max     = 1
      SitoN_uptake_scale   = 0.6
      SitoN_uptake_exp     = 3.887
      q_SitoN_diss         = 1.0 _d 0
#endif
#else
      pivotal              = 1.9 _d -3 / 1028. _d 0 / CtoP / permil
      Pc_0                 = 1 _d -5
      alpha_max            = 1.6 _d -5 * 2.77 _d 18 / 6.022 _d 17
      alpha_min            = 0.4 _d -5 * 2.77 _d 18 / 6.022 _d 17
      gamma_biomass        = 0.5 _d 0 / secperday
      gamma_DOP            = 0.25 _d 0 / (365.25 _d 0 * secperday)
      k_Fe                 = 8. _d -10 / permil
      k_PO4                = 1. _d -7 / permil
      k_FetoP              = 7. _d -6 * CtoP
      FetoP_max            = 28. _d -6 * CtoP
      Fe_lim_min           = 0. _d 0
      remin_min            = 0.3 _d 0
#endif
      lambda_0             = 0.19 _d 0 / secperday
      resp_frac            = 0. _d 0
      chl_min              = 1. _d -5
      theta_Fe_max_hi      = 0.04 _d 0
      theta_Fe_max_lo      = 0.01 _d 0
      gamma_irr_mem        = 1. _d 0 / secperday
      gamma_POM            = 0.12 _d 0 / secperday
      k_O2                 = 20. _d -6 / permil
      kFe_eq_lig_max       = 8.0 _d 10 * permil
      kFe_eq_lig_min       = 8.0 _d 9 * permil
      kFe_eq_lig_Femin     = 0.05 _d -9 / permil
      kFe_eq_lig_irr       = 0.1 _d 0
      kFe_org              = 0.5 _d 0 / secperday * permil**(0.58)
      kFe_inorg            = 1. _d 3 / secperday * permil**(0.5)
      FetoC_sed            = 1. _d -4
      oxic_min             = 1. _d -6 / permil
      Ligand               = 1. _d -9 / permil
      kappa_eppley         = 0.063 _d 0
      kappa_remin          = -0.032 _d 0
      ca_remin_depth       = 1343. _d 0
      phi_DOM              = 0.1 _d 0
      phi_sm               = 0.18 _d 0
      phi_lg               = 1. _d 0
      wsink0               = 16. _d 0 / secperday
      wsink0z              = 80. _d 0
      wsinkacc             = 0.05 _d 0 / secperday
      parfrac              = 0.4 _d 0
      alpfe                = 0.01 _d 0
      k0                   = 0.04 _d 0
      MLmix_max            = 200.0 _d 0
      chlsat_locTimWindow(1) = 12.0 _d 0
      chlsat_locTimWindow(2) = 13.5 _d 0

      bling_debugLevel       = 0
      bling_useEXF           = .FALSE.
      bling_apco2File        = ' '
      bling_windFile         = ' '
      bling_apressFile       = ' '
      bling_iceFile          = ' '
      bling_ironFile         = ' '
      bling_silicaFile       = ' '
      bling_psmFile          = ' '
      bling_plgFile          = ' '
      bling_pdiazFile        = ' '
      bling_Pc_2dFile        = ' '
      bling_Pc_2d_diazFile   = ' '
      bling_k0_2dFile        = ' '
      bling_alpha_photo2dFile= ' '
      bling_k_Fe2dFile       = ' '
      bling_k_Fe_diaz2dFile  = ' '
      bling_gamma_POM2dFile  = ' '
      bling_wsink0_2dFile    = ' '
      bling_phi_DOM2dFile    = ' '
      bling_phi_sm2dFile     = ' '
      bling_phi_lg2dFile     = ' '

      bling_pCO2             = 278. _d -6
      river_conc_po4         = 0.007 _d 0
      river_dom_to_nut       = 0. _d 0

C     some sensible defaults
      bling_iceConst        = 0.      _d 0
      bling_apressConst     = 1.01325 _d 5
      bling_silicaConst     = 7.6838  _d -3
C     bling_apco2Const defaults to bling_pCO2, but bling_pCO2 is itself
C     read from the BLING_FORCING namelist below: leave it unset here and
C     resolve the default after the namelist has been read.
      bling_apco2Const      = UNSET_RL
      bling_windConst       = 5.      _d 0
      bling_ironConst       = 1.      _d -11

C     default periodic forcing to same as for physics
      bling_forcingPeriod = externForcingPeriod
      bling_forcingCycle  = externForcingCycle

#ifdef ALLOW_EXF
C     If useEXF=F, s/r exf_readparms is not called and exf-parameters
C     are not initialiased properly. The following few exf-related
C     parameters need to have a defined value to be used in s/r
C     exf_set_fld.
       bling_exf_iprec           = 32
       bling_useExfYearlyFields  = .FALSE.
       bling_twoDigitYear        = .FALSE.
       bling_exf_debugLev        = debugLevel

C     repeatPeriod is needed now as default for bling_*RepCycle
       IF ( .NOT. useEXF ) repeatPeriod = bling_ForcingCycle

      bling_apco2StartDate1         = 0
      bling_apco2StartDate2         = 0
      bling_apco2StartTime          = UNSET_RL
      bling_apco2Period             = 0.0 _d 0
      bling_apco2RepCycle           = repeatPeriod
      bling_apco2_exfremo_intercept = 0.0 _d 0
      bling_apco2_exfremo_slope     = 0.0 _d 0
      bling_inscal_apco2            =  1. _d 0
      bling_outscal_apco2           =  1. _d 0
      bling_silicaStartDate1        = 0
      bling_silicaStartDate2        = 0
      bling_silicaStartTime         = UNSET_RL
      bling_silicaPeriod            = 0.0 _d 0
      bling_silicaRepCycle          = repeatPeriod
      bling_silica_exfremo_intercept= 0.0 _d 0
      bling_silica_exfremo_slope    = 0.0 _d 0
      bling_inscal_silica           =  1. _d 0
      bling_outscal_silica          =  1. _d 0
      bling_ironStartDate1          = 0
      bling_ironStartDate2          = 0
      bling_ironStartTime           = UNSET_RL
      bling_ironPeriod              = 0.0 _d 0
      bling_ironRepCycle            = repeatPeriod
      bling_iron_exfremo_intercept  = 0.0 _d 0
      bling_iron_exfremo_slope      = 0.0 _d 0
      bling_inscal_iron             =  1. _d 0
      bling_outscal_iron            =  1. _d 0
      bling_apressStartDate1        = 0
      bling_apressStartDate2        = 0
      bling_apressStartTime         = UNSET_RL
      bling_apressPeriod            = 0.0 _d 0
      bling_apressRepCycle          = repeatPeriod
      bling_apress_exfremo_intercept= 0.0 _d 0
      bling_apress_exfremo_slope    = 0.0 _d 0
      bling_inscal_apress           =  1. _d 0
      bling_outscal_apress          =  1. _d 0
      bling_windStartDate1          = 0
      bling_windStartDate2          = 0
      bling_windStartTime           = UNSET_RL
      bling_windPeriod              = 0.0 _d 0
      bling_windRepCycle            = repeatPeriod
      bling_wind_exfremo_intercept  = 0.0 _d 0
      bling_wind_exfremo_slope      = 0.0 _d 0
      bling_inscal_wind             =  1. _d 0
      bling_outscal_wind            =  1. _d 0
      bling_iceStartDate1           = 0
      bling_iceStartDate2           = 0
      bling_iceStartTime            = UNSET_RL
      bling_icePeriod               = 0.0 _d 0
      bling_iceRepCycle             = repeatPeriod
      bling_ice_exfremo_intercept   = 0.0 _d 0
      bling_ice_exfremo_slope       = 0.0 _d 0
      bling_inscal_ice              =  1. _d 0
      bling_outscal_ice             =  1. _d 0

#ifdef USE_EXF_INTERPOLATION

       IF ( .NOT. useEXF ) THEN
C     This is done in s/r exf_init_interp only if useEXF=T
# ifdef ALLOW_EXCH2
        inp_gNx = exch2_mydNx(1)
        inp_gNy = exch2_mydNy(1)
# else /* ALLOW_EXCH2 */
        inp_gNx = Nx
        inp_gNy = Ny
# endif /* ALLOW_EXCH2 */

        inp_lon0 = xgOrigin + delX(1)*halfRL
        inp_lat0 = ygOrigin + delY(1)*halfRL

        inp_dLon = delX(1)
        DO j=1,MAX_LAT_INC
         IF (j.LT.inp_gNy) THEN
           inp_dLat(j) = (delY(j) + delY(j+1))*halfRL
         ELSE
           inp_dLat(j) = 0.
         ENDIF
        ENDDO
       ENDIF

      bling_apco2_lon0          = inp_lon0
      bling_apco2_lat0          = inp_lat0
      bling_apco2_nlon          = inp_gNx
      bling_apco2_nlat          = inp_gNy
      bling_apco2_lon_inc       = inp_dLon
      bling_apco2_interpMethod  = 1
      bling_silica_lon0         = inp_lon0
      bling_silica_lat0         = inp_lat0
      bling_silica_nlon         = inp_gNx
      bling_silica_nlat         = inp_gNy
      bling_silica_lon_inc      = inp_dLon
      bling_silica_interpMethod = 1
      bling_iron_lon0           = inp_lon0
      bling_iron_lat0           = inp_lat0
      bling_iron_nlon           = inp_gNx
      bling_iron_nlat           = inp_gNy
      bling_iron_lon_inc        = inp_dLon
      bling_iron_interpMethod   = 1
      bling_wind_lon0           = inp_lon0
      bling_wind_lat0           = inp_lat0
      bling_wind_nlon           = inp_gNx
      bling_wind_nlat           = inp_gNy
      bling_wind_lon_inc        = inp_dLon
      bling_wind_interpMethod   = 1
      bling_apress_lon0         = inp_lon0
      bling_apress_lat0         = inp_lat0
      bling_apress_nlon         = inp_gNx
      bling_apress_nlat         = inp_gNy
      bling_apress_lon_inc      = inp_dLon
      bling_apress_interpMethod = 1
      bling_ice_lon0            = inp_lon0
      bling_ice_lat0            = inp_lat0
      bling_ice_nlon            = inp_gNx
      bling_ice_nlat            = inp_gNy
      bling_ice_lon_inc         = inp_dLon
      bling_ice_interpMethod    = 1

      DO j=1,MAX_LAT_INC
        bling_apco2_lat_inc(j)  = inp_dLat(j)
        bling_silica_lat_inc(j) = inp_dLat(j)
        bling_iron_lat_inc(j)   = inp_dLat(j)
        bling_apress_lat_inc(j) = inp_dLat(j)
        bling_wind_lat_inc(j)   = inp_dLat(j)
        bling_ice_lat_inc(j)    = inp_dLat(j)
      ENDDO

       bling_exf_output_interp = .FALSE.

       IF ( .NOT. useEXF ) THEN
        exf_output_interp      = bling_exf_output_interp
       ENDIF

#endif /* USE_EXF_INTERPOLATION */
#endif /* ALLOW_EXF */

      WRITE(msgBuf,'(A)') ' BLING_READPARMS: opening data.bling'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data.bling', 'BLING_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Abiotic parameters
      READ(UNIT=iUnit,NML=ABIOTIC_PARMS)

C-    BLING parameters
      READ(UNIT=iUnit,NML=BIOTIC_PARMS)

C-    forcing filenames and parameters
      READ(UNIT=iUnit,NML=BLING_FORCING)
      IF ( bling_apco2Const .EQ. UNSET_RL )
     &     bling_apco2Const = bling_pCO2

#ifdef ALLOW_EXF
C     pass the exf-related parameters read from data.bling to pkg/exf
C     (only if pkg/exf is not used; otherwise set in data.exf)
      IF ( .NOT. useEXF ) THEN
        exf_iprec          = bling_exf_iprec
        useExfYearlyFields = bling_useExfYearlyFields
        twoDigitYear       = bling_twoDigitYear
        exf_debugLev       = bling_exf_debugLev
      ENDIF
#else /* ALLOW_EXF */
      IF ( bling_useEXF ) THEN
        WRITE(msgBuf,'(2A)') 'BLING_READPARMS: ',
     &   'bling_useEXF=.TRUE. requires pkg/exf (ALLOW_EXF undefined)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_EXF */

      WRITE(msgBuf,'(A)')
     &   ' BLING_READPARMS: finished reading data.BLING'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      iUnit = standardMessageUnit
C Record the solver to calculate pH and evaluate surface ocean pCO2
#ifdef CARBONCHEM_SOLVESAPHE
      IF ( selectPHsolver.GT.0 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Munhoven (2013) Solvesaphe for pH/pCO2'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectPHsolver.EQ.0 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Follows et al. (2006) for pH/pCO2'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
#endif

C Record which carbonate coefficients are used and which pH/pCO2 solver
#ifdef CARBONCHEM_SOLVESAPHE
      WRITE(msgBuf,'(A)')
     &  'Using Munhoven (2013) Solvesaphe carbon coefficients'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
C Munhoven (2013)'s "Solvesaphe" coefficients have several options:
      IF ( selectK1K2const.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Millero (1995)/Mehrbach K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Roy et al. (1993) K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.3 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Millero (1995) "consensus" K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.4 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Luecker et al. (2000) K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.5 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Millero et al. (2010) K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.6 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Waters et al. (2014) K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
      IF ( selectHFconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Dickson and Riley (1979) KF coefficient'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectHFconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Perez and Fraga (1987) KF coefficient'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
      IF ( selectBTconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     & 'Using Uppstrom (1974) BT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectBTconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Lee et al (2010) BT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
      IF ( selectFTconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Riley (1965) FT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectBTconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Culkin (1965) FT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
#else
C OCMIP2 Coefficients from S/R CARBON_COEFFS in CARBON_CHEM.F
      WRITE(msgBuf,'(A)')
     &  'Using Millero (1995)/Mehrbach K1 and K2 coefficients'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)')
     &  'Using Dickson and Riley (1979) KF coefficient'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)')
     &  'Using Uppstrom (1974) BT estimation from salinity'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)')
     &  'Using Riley (1965) FT estimation from salinity'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
#endif

#ifdef CARBONCHEM_SOLVESAPHE
      IF ( selectHFconst.GT.2 .OR. selectBTconst.GT.2
     & .OR. selectFTconst.GT.2 ) THEN
        WRITE(msgBuf,'(A)')
     &    'BLING_READPARMS: selectHF, selectBT or selectFT > 2'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_READPARMS: solvesaphe_const error'
      ELSEIF ( selectK1K2const.GT.6 ) THEN
        WRITE(msgBuf,'(A)')
     &    'BLING_READPARMS: selectK1K2const > 6 '
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_READPARMS: solvesaphe_const error'
      ELSEIF ( selectPHsolver.GT.3 ) THEN
        WRITE(msgBuf,'(A)')
     &    'BLING_READPARMS: selectPHsolver > 3 '
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_READPARMS: select pH solver error'
      ENDIF
#endif

#ifdef ALLOW_EXF
      IF ( bling_useEXF ) THEN
C--   Print general parameters:
        WRITE(msgBuf,'(A)') ' bling_useEXF general parameters:'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(A)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
#ifdef USE_EXF_INTERPOLATION
        WRITE(msgBuf,'(A)')
     &  '// USE_EXF_INTERPOLATION:              defined'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
#else
          WRITE(msgBuf,'(A)')
     &  '// USE_EXF_INTERPOLATION:          NOT defined'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
#endif
C--   For each data set used the summary prints the calendar data
C     and the corresponding file from which the data will be read.
      prtBlkLn = .FALSE.
      addBlkLn = .TRUE.

C--   bling wind speed
        IF ( bling_windFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'wind speed',
     I         bling_windFile, 
     I         bling_windRepCycle, 
     I         bling_windPeriod,
     I         bling_windStartTime, 
     I         useExfYearlyFields,
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'blingWind',
     I         bling_wind_lon0, 
     I         bling_wind_lon_inc, 
     I         bling_wind_lat0, 
     I         bling_wind_lat_inc,
     I         bling_wind_nlon, 
     I         bling_wind_nlat, 
     I         bling_wind_interpMethod, 
     I         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C--   bling atmospheric pressure
        IF ( bling_apressFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'atmospheric pressure',
     I         bling_apressFile, 
     I         bling_apressRepCycle, 
     I         bling_apressPeriod,
     I         bling_apressStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'blingApress',
     I         bling_apress_lon0, 
     I         bling_apress_lon_inc, 
     I         bling_apress_lat0, 
     I         bling_apress_lat_inc,
     I         bling_apress_nlon, 
     I         bling_apress_nlat, 
     I         bling_apress_interpMethod, 
     I         myThid )
#endif  
         prtBlkLn = .TRUE.
        ENDIF 

C--   bling surface silica
        IF ( bling_silicaFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'surface silica',
     I         bling_silicaFile, 
     I         bling_silicaRepCycle, 
     I         bling_silicaPeriod,
     I         bling_silicaStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'blingSilica',
     I         bling_silica_lon0, 
     I         bling_silica_lon_inc, 
     I         bling_silica_lat0, 
     I         bling_silica_lat_inc,
     I         bling_silica_nlon, 
     I         bling_silica_nlat, 
     I         bling_silica_interpMethod, 
     I         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C--   bling iron forcing
        IF ( bling_ironFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'aeolian iron flux',
     I         bling_ironFile, 
     I         bling_ironRepCycle, 
     I         bling_ironPeriod,
     I         bling_ironStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'InputFe',
     I         bling_iron_lon0, 
     I         bling_iron_lon_inc, 
     I         bling_iron_lat0, 
     I         bling_iron_lat_inc,
     I         bling_iron_nlon, 
     I         bling_iron_nlat, 
     I         bling_iron_interpMethod, 
     I         myThid )
#endif   
          prtBlkLn = .TRUE.
      ENDIF

C--   bling atmospheric CO2 forcing
        IF ( bling_apco2File .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'atmospheric CO2',
     I         bling_apco2File, 
     I         bling_apco2RepCycle, 
     I         bling_apco2Period,
     I         bling_apco2StartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid)
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'blingAtmosCO2',
     I         bling_apco2_lon0, 
     I         bling_apco2_lon_inc, 
     I         bling_apco2_lat0, 
     I         bling_apco2_lat_inc,
     I         bling_apco2_nlon, 
     I         bling_apco2_nlat, 
     I         bling_apco2_interpMethod, 
     I         myThid )
#endif  
         prtBlkLn = .TRUE.
        ENDIF

C--  bling sea ice forcing
        IF ( bling_iceFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'sea ice fraction',
     I         bling_iceFile, 
     I         bling_iceRepCycle, 
     I         bling_icePeriod,
     I         bling_iceStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid ) 
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'blingIce',
     I         bling_ice_lon0, 
     I         bling_ice_lon_inc, 
     I         bling_ice_lat0, 
     I         bling_ice_lat_inc,
     I         bling_ice_nlon, 
     I         bling_ice_nlat, 
     I         bling_ice_interpMethod, 
     I         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C bling_useEXF
      ENDIF

C     bling_useEXF and useExfYearlyFields
      IF ( bling_useEXF .AND. useExfYearlyFields ) THEN
       IF ( bling_ironFile.NE.' ' .AND.
     &      bling_ironRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'BLING_READPARMS: The use of ',
     &   'useExfYearlyFields AND bling_ironRepCycle ',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( bling_silicaFile.NE.' ' .AND.
     &      bling_silicaRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'BLING_READPARMS: The use of ',
     &   'useExfYearlyFields AND bling_silicaRepCycle ',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( bling_apco2File.NE.' ' .AND.
     &      bling_apco2RepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'BLING_READPARMS: The use of ',
     &   'useExfYearlyFields AND bling_apco2RepCycle ',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( bling_windFile.NE.' ' .AND.
     &      bling_windRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'BLING_READPARMS: The use of ',
     &   'useExfYearlyFields AND bling_windRepCycle ',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( bling_iceFile.NE.' ' .AND.
     &      bling_iceRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'BLING_READPARMS: The use of ',
     &   'useExfYearlyFields AND bling_iceRepCycle ',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( bling_apressFile.NE.' ' .AND.
     &      bling_apressRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'BLING_READPARMS: The use of ',
     &   'useExfYearlyFields AND bling_apressRepCycle ',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
C     useEXF and useExfYearlyFields
#endif /* ALLOW_EXF */

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'BLING_READPARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R BLING_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_BLING */

      RETURN
      END
