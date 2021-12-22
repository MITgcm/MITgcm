#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_READPARMS
C     !INTERFACE:
      SUBROUTINE SEAICE_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_READPARMS
C     | o Routine to read in file data.seaice
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif /* ALLOW_GENERIC_ADVDIFF */
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_COST
# include "SEAICE_COST.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_CONSTANTS.h"
#endif /* ALLOW_EXF */
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid     :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL chkFlag
      INTEGER iUnit
      INTEGER l
#ifdef ALLOW_SITRACER
      INTEGER iTracer
#endif
#ifdef ALLOW_COST
      INTEGER locDate(4)
#endif /* ALLOW_COST */
      INTEGER nRetired, nError
      _RL tmp

C-    Old parameters (to be retired one day):
      _RL SEAICE_availHeatTaper
      _RL SEAICE_gamma_t, SEAICE_gamma_t_frz, SEAICE_availHeatFracFrz

C-    Retired parameters:
C     MAX_TICE          :: maximum ice temperature   (deg C)
C     LAD               :: time stepping used for sea-ice advection:
C                          1 = LEAPFROG,  2 = BACKWARD EULER.
C     SEAICE_freeze     :: FREEZING TEMP. OF SEA WATER
      _RL SEAICE_sensHeat, SEAICE_latentWater, SEAICE_latentIce
      _RL SEAICE_salinity, SIsalFRAC, SIsal0
      _RL SEAICE_lhSublim, SEAICE_freeze, MAX_HEFF
      _RL areaMin, areaMax, A22, hiceMin, MAX_TICE
      LOGICAL SEAICEadvAge
      INTEGER SEAICEadvSchAge, LAD, SEAICEturbFluxFormula
      INTEGER NPSEUDOTIMESTEPS, SOLV_MAX_ITERS
      INTEGER SEAICEnewtonIterMax, SEAICEkrylovIterMax
      _RL JFNKgamma_nonlin
      _RL SEAICEdiffKhAge
      CHARACTER*(MAX_LEN_MBUF) IceAgeFile, IceAgeTrFile(4)
      _RL SEAICE_abEps
      LOGICAL SEAICEuseAB2

C--   SEAICE parameters
      NAMELIST /SEAICE_PARM01/
     & SEAICEuseDYNAMICS, SEAICEuseFREEDRIFT, SEAICEuseStrImpCpl,
     & SEAICEuseMCS, SEAICEuseMCE, SEAICEuseTD, SEAICEusePL,
     & SEAICEuseTEM, SEAICEuseMetricTerms, SEAICEuseTilt,
     & useHB87stressCoupling, SEAICEupdateOceanStress,
     & usePW79thermodynamics, useMaykutSatVapPoly, SEAICEuseFlooding,
     & SEAICErestoreUnderIce, SEAICE_growMeltByConv,
     & SEAICE_salinityTracer, SEAICE_ageTracer,
     & SEAICEadvHeff, SEAICEadvArea, SEAICEadvSnow,
     & SEAICEadvSalt, SEAICEadvAge, SEAICEaddSnowMass,
     & SEAICEmomAdvection, SEAICEselectKEscheme, SEAICEselectVortScheme,
     & SEAICEhighOrderVorticity, SEAICEupwindVorticity,
     & SEAICEuseAbsVorticity, SEAICEuseJamartMomAdv,
     & SEAICE_clipVelocities, SEAICE_maskRHS,
     & SEAICE_no_slip, SEAICE_2ndOrderBC,
     & SEAICEetaZmethod, LAD, IMAX_TICE, postSolvTempIter,
     & SEAICEuseFluxForm, SEAICEadvScheme, SEAICEadvSchArea,
     & SEAICEadvSchHeff, SEAICEadvSchSnow,
     & SEAICEadvSchSalt, SEAICEadvSchAge,
     & SEAICEdiffKhHeff, SEAICEdiffKhSnow, SEAICEdiffKhArea,
     & SEAICEdiffKhSalt, SEAICEdiffKhAge, DIFF1,
     & SEAICE_deltaTtherm, SEAICE_deltaTdyn,
     & SEAICE_LSRrelaxU, SEAICE_LSRrelaxV,
     & SOLV_MAX_ITERS, SOLV_NCHECK, NPSEUDOTIMESTEPS,
     & LSR_ERROR, LSR_mixIniGuess, SEAICEuseMultiTileSolver,
     & SEAICE_deltaTevp, SEAICE_elasticParm, SEAICE_evpTauRelax,
     & SEAICE_evpDampC, SEAICEnEVPstarSteps,
     & SEAICE_evpAlpha, SEAICE_evpBeta,
     & SEAICEaEVPcoeff, SEAICEaEVPcStar, SEAICEaEVPalphaMin,
     & SEAICE_zetaMin, SEAICE_zetaMaxFac, SEAICEusePicardAsPrecon,
     & SEAICEuseKrylov, SEAICEuseJFNK,
     & SEAICEnonLinIterMax, SEAICElinearIterMax, SEAICEnonLinTol,
     & SEAICEnewtonIterMax, SEAICEkrylovIterMax, JFNKgamma_nonlin,
     & SEAICEpreconNL_Iter, SEAICEpreconLinIter,
     & SEAICE_JFNK_lsIter, SEAICE_JFNK_lsLmax, SEAICE_JFNK_lsGamma,
     & SEAICE_JFNK_tolIter, JFNKres_t,JFNKres_tFac,
     & JFNKgamma_lin_min,JFNKgamma_lin_max,
     & SEAICE_JFNKepsilon, SEAICE_OLx, SEAICE_OLy,
     & SEAICE_JFNKphi, SEAICE_JFNKalpha, SEAICEuseIMEX, SEAICEuseBDF2,
     & SEAICEuseLinRemapITD,
     & useHibler79IceStrength, SEAICEpartFunc, SEAICEredistFunc,
     & SEAICEridgingIterMax, SEAICEsimpleRidging, SEAICEsnowFracRidge,
     & SEAICEgStar, SEAICEhStar, SEAICEaStar, SEAICEshearParm,
     & SEAICEmuRidging, SEAICEmaxRaft, SEAICE_cf,
     & SEAICEuseAB2, SEAICE_abEps,
     & SEAICEpresH0, SEAICEpresPow0, SEAICEpresPow1,
     & SEAICE_initialHEFF, SEAICEturbFluxFormula,
     & SEAICE_areaGainFormula, SEAICE_areaLossFormula,
     & SEAICE_doOpenWaterGrowth, SEAICE_doOpenWaterMelt,
     & SEAICE_rhoAir, SEAICE_rhoIce, SEAICE_rhoSnow, ICE2WATR,
     & SEAICE_cpAir, SEAICEscaleSurfStress,
     & SEAICE_drag, SEAICE_waterDrag, SEAICEdWatMin, SEAICE_dryIceAlb,
     & SEAICE_wetIceAlb, SEAICE_drySnowAlb, SEAICE_wetSnowAlb, HO,
     & SEAICE_drag_south, SEAICE_waterDrag_south,
     & SEAICE_dryIceAlb_south, SEAICE_wetIceAlb_south,
     & SEAICE_drySnowAlb_south, SEAICE_wetSnowAlb_south, HO_south,
     & SEAICE_cBasalStar, SEAICEbasalDragU0, SEAICEbasalDragK1,
     & SEAICEbasalDragK2, SEAICE_wetAlbTemp, SEAICE_waterAlbedo,
     & SEAICE_strength, SEAICE_cStar, SEAICE_eccen,
     & SEAICE_eccfr, SEAICEtdMU, SEAICEmcMu,
     & SEAICEpressReplFac, SEAICE_tensilFac, SEAICE_tensilDepth,
     & SEAICE_lhFusion, SEAICE_lhEvap, SEAICE_dalton,
     & SEAICE_sensHeat, SEAICE_latentWater, SEAICE_latentIce,
     & SEAICE_salinity, SIsalFRAC, SIsal0,
     & areaMin, areaMax, A22, hiceMin,
     & SEAICE_iceConduct, SEAICE_snowConduct,
     & SEAICE_emissivity, SEAICE_ice_emiss, SEAICE_snow_emiss,
     & SEAICE_snowThick, SEAICE_shortwave, SEAICE_freeze, OCEAN_drag,
     & SEAICE_tempFrz0, SEAICE_dTempFrz_dS, SEAICE_salt0,
     & SEAICE_saltFrac, SEAICEstressFactor, SEAICE_availHeatTaper,
     & SEAICE_mcPheePiston, SEAICE_frazilFrac, SEAICE_mcPheeTaper,
     & SEAICE_mcPheeStepFunc, SEAICE_gamma_t, SEAICE_gamma_t_frz,
     & SEAICE_availHeatFrac, SEAICE_availHeatFracFrz, SEAICE_PDF,
     & AreaFile, HeffFile, uIceFile, vIceFile, HsnowFile, HsaltFile,
     & SEAICEheatConsFix, SEAICE_multDim, SEAICE_useMultDimSnow,
     & SEAICE_deltaMin, SEAICE_area_reg, SEAICE_hice_reg,
     & SEAICE_area_floor, SEAICE_area_max, SEAICE_tauAreaObsRelax,
     & SEAICE_airTurnAngle, SEAICE_waterTurnAngle,
     & MAX_HEFF, MIN_ATEMP, MIN_LWDOWN, MAX_TICE, MIN_TICE,
     & SEAICE_EPS, SEAICE_EPS_SQ,
     & SEAICEwriteState, SEAICEuseEVPpickup,
     & SEAICEuseEVPstar, SEAICEuseEVPrev,
     & SEAICE_monFreq, SEAICE_dumpFreq, SEAICE_taveFreq,
     & SEAICE_tave_mnc, SEAICE_dump_mnc, SEAICE_mon_mnc,
#ifdef SEAICE_ITD
     & Hlimit_c1, Hlimit_c2, Hlimit_c3, Hlimit,
#endif
     & SEAICE_debugPointI, SEAICE_debugPointJ,
     & SINegFac

#ifdef ALLOW_COST
      NAMELIST /SEAICE_PARM02/
     &          mult_ice_export, mult_ice, cost_ice_flag,
     &          costIceStart1, costIceStart2,
     &          costIceEnd1, costIceEnd2,
     &          cost_ice_flag,
     &          SEAICE_clamp_salt, SEAICE_clamp_theta,
     &          SEAICE_cutoff_area, SEAICE_cutoff_heff,
     &          mult_smrsst, smrsstbarfile,
     &          mult_smrsss, smrsssbarfile,
     &          mult_smrarea, smrareabarfile, smrareadatfile,
     &          wsmrarea0, wmean_smrarea, smrarea_errfile,
     &          smrareastartdate1, smrareastartdate2, smrareaperiod
#endif

#ifdef ALLOW_SITRACER
      NAMELIST /SEAICE_PARM03/
     &  SItrFile, SItrName, SItrNameLong, SItrUnit, SItrMate,
     &  SItrFromOcean0, SItrFromOceanFrac, SItrFromFlood0,
     &  SItrFromFloodFrac, SItrExpand0,
     &  IceAgeTrFile, SItrNumInUse
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useSEAICE ) THEN
C-    pkg SEAICE is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.seaice is found
         CALL PACKAGES_UNUSED_MSG( 'useSEAICE', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   set default sea ice parameters
#ifdef SEAICE_ALLOW_DYNAMICS
      SEAICEuseDYNAMICS  = .TRUE.
#else
      SEAICEuseDYNAMICS  = .FALSE.
#endif
      SEAICEadjMODE      = 0
      SEAICEuseFREEDRIFT = .FALSE.
      SEAICEuseStrImpCpl = .FALSE.
      SEAICEuseTilt      = .TRUE.
      SEAICEheatConsFix  = .FALSE.
      SEAICEuseTEM       = .FALSE.
      SEAICEuseMCS       = .FALSE.
      SEAICEuseMCE       = .FALSE.
      SEAICEuseTD        = .FALSE.
      SEAICEusePL        = .FALSE.
      SEAICEuseMetricTerms = .TRUE.
      SEAICEuseEVPpickup = .TRUE.
      SEAICEuseEVPstar   = .TRUE.
      SEAICEuseEVPrev    = .TRUE.
      SEAICErestoreUnderIce = .FALSE.
      SEAICE_growMeltByConv = .FALSE.
      SEAICE_salinityTracer = .FALSE.
      SEAICE_ageTracer      = .FALSE.
      useHB87stressCoupling = .FALSE.
      SEAICEupdateOceanStress = .TRUE.
      SEAICEscaleSurfStress = .TRUE.
      SEAICEaddSnowMass     = .TRUE.
      usePW79thermodynamics = .TRUE.
C     start of ridging parameters
      useHibler79IceStrength= .TRUE.
      SEAICEsimpleRidging   = .TRUE.
C     The range of this proportionality constant is 2 to 17
      SEAICE_cf             = 17. _d 0
C     By default use partition function of Thorndyke et al. (1975) ...
      SEAICEpartFunc        = 0
C     and redistribution function of Hibler (1980)
      SEAICEredistFunc      = 0
      SEAICEridgingIterMax  = 10
C     This parameter is not well constraint (Lipscomb et al. 2007)
      SEAICEshearParm       = 0.5 _d 0
C     Thorndyke et al. (1975)
      SEAICEgStar           = 0.15 _d 0
C     suggested by Hibler (1980), Flato and Hibler (1995)
C     SEAICEhStar           = 100. _d 0
C     but this value of 25 seems to give thinner ridges in better agreement
C     with observations (according to Lipscomb et al 2007)
      SEAICEhStar           =  25. _d 0
C     according to Lipscomb et al. (2007) these values for aStar and mu
C     are approximately equivalent to gStar=0.15 (aStar = gStar/3) for
C     SEAICEpartFunc = 1 ...
      SEAICEaStar           = 0.05 _d 0
C     ... and hStar=25 for SEAICEredistFunc = 1
C     Libscomb et al. (2007): mu =  3,  4,  5,   6
C     correspond to        hStar = 25, 50, 75, 100
      SEAICEmuRidging       = 3. _d 0
      SEAICEmaxRaft         = 1. _d 0
      SEAICEsnowFracRidge   = 0.5 _d 0
      SEAICEuseLinRemapITD  = .TRUE.
C     end ridging parampeters
      useMaykutSatVapPoly = .FALSE.
      SEAICEuseFluxForm  = .TRUE.
      SEAICEadvHeff      = .TRUE.
      SEAICEadvArea      = .TRUE.
      SEAICEadvSnow      = .TRUE.
#ifdef SEAICE_VARIABLE_SALINITY
      SEAICEadvSalt      = .TRUE.
#else
      SEAICEadvSalt      = .FALSE.
#endif
      SEAICEmomAdvection       = .FALSE.
      SEAICEselectKEscheme     = 1
      SEAICEselectVortScheme   = 2
      SEAICEhighOrderVorticity = .FALSE.
      SEAICEupwindVorticity    = .FALSE.
      SEAICEuseAbsVorticity    = .FALSE.
      SEAICEuseJamartMomAdv    = .FALSE.
      SEAICEuseFlooding  = .TRUE.
      SEAICE_no_slip     = .FALSE.
      SEAICE_2ndOrderBC  = .FALSE.
      SEAICE_clipVelocities = .FALSE.
      SEAICE_maskRHS     = .FALSE.
      SEAICEetaZmethod   = 3
      SEAICEadvScheme    = 77
      SEAICEadvSchArea   = UNSET_I
      SEAICEadvSchHeff   = UNSET_I
      SEAICEadvSchSnow   = UNSET_I
      SEAICEadvSchSalt   = UNSET_I
      SEAICEdiffKhArea   = UNSET_RL
      SEAICEdiffKhHeff   = UNSET_RL
      SEAICEdiffKhSnow   = UNSET_RL
      SEAICEdiffKhSalt   = UNSET_RL
      DIFF1      = UNSET_RL
C--   old DIFF1 default:
c     DIFF1      = .004 _d 0
      SEAICE_deltaTtherm = dTtracerLev(1)
      SEAICE_deltaTdyn   = dTtracerLev(1)
      SEAICE_deltaTevp   = UNSET_RL
      SEAICEuseBDF2      = .FALSE.
      SEAICEuseKrylov    = .FALSE.
C     JFNK stuff
      SEAICEuseJFNK       = .FALSE.
      SEAICEuseIMEX       = .FALSE.
      SEAICE_JFNK_lsIter  = UNSET_I
      SEAICE_JFNK_lsLmax  = 4
      SEAICE_JFNK_lsGamma = 0.5 _d 0
      SEAICE_JFNK_tolIter = 100
C     This should be the default for both JFNK and for LSR but in order
C     to not jeopardize any existing results, and because it does not yet
C     work for llc/cubed-sphere topologies, we require that the user knows
C     what he/she is doing when turning this on.
      SEAICE_OLx          = OLx-2
      SEAICE_OLy          = OLy-2
      JFNKgamma_nonlin    = 1. _d -05
      JFNKgamma_lin_min   = 0.10 _d 0
      JFNKgamma_lin_max   = 0.99 _d 0
      JFNKres_t           = UNSET_RL
      JFNKres_tFac        = UNSET_RL
      SEAICE_JFNKepsilon  = 1. _d -06
C     factor for inexact Newton forcing, .gt. 0 and .le. 1
      SEAICE_JFNKphi      = 1. _d 0
C     exponent for inexact Newton forcing .gt. 1 and .le. 2
      SEAICE_JFNKalpha    = 1. _d 0
C     Hunke, JCP, 2001 use 615 kg/m^2 for this, but does not recommend using it
      SEAICE_evpDampC    = -1. _d 0
      SEAICE_zetaMin     = 0. _d 0
      SEAICE_zetaMaxFac  = 2.5 _d 8
      SEAICEpresH0       = 1. _d 0
      SEAICEpresPow0     = 1
      SEAICEpresPow1     = 1
      SEAICE_evpTauRelax = -1. _d 0
      SEAICE_elasticParm = 0.33333333333333333333333333 _d 0
      SEAICE_evpAlpha    = UNSET_RL
      SEAICE_evpBeta     = UNSET_RL
      SEAICEnEVPstarSteps = UNSET_I
      SEAICEaEVPcoeff    = UNSET_RL
      SEAICEaEVPcStar    = UNSET_RL
      SEAICEaEVPalphaMin = UNSET_RL
      SEAICE_initialHEFF = ZERO
#ifdef SEAICE_ITD
C     Coefficients used to calculate sea ice thickness category limits
C     after Lipscomb et al. (2001, JGR), Equ. 22
C     choose between
C      - original parameters of Lipscomb et al. (2001):
C        c1=3.0/N, c2=15*c1, c3=3.0
C      - and a higher resolution of thin end of ITD:
C        c1=1.5/N, c2=42*c1, c3=3.3
      DO l = 0, nITD
       Hlimit(l) = UNSET_RL
      ENDDO
      Hlimit_c1          = 3.0
      Hlimit_c2          = 15.
      Hlimit_c3          = 3.0
#endif
      SEAICE_rhoIce      = 0.91   _d +03
      SEAICE_rhoSnow     = 330.   _d 0
      ICE2WATR           = UNSET_RL
      SEAICE_drag        = 0.001  _d 0
      OCEAN_drag         = 0.001  _d 0
      SEAICE_waterDrag   = 0.0055 _d 0
      SEAICEdWatMin      = 0.25   _d 0
      SEAICE_dryIceAlb   = 0.75   _d 0
      SEAICE_wetIceAlb   = 0.66   _d 0
      SEAICE_drySnowAlb  = 0.84   _d 0
      SEAICE_wetSnowAlb  = 0.7    _d 0
      HO                 = 0.5    _d 0
      SEAICE_drag_south       = UNSET_RL
      SEAICE_waterDrag_south  = UNSET_RL
      SEAICE_dryIceAlb_south  = UNSET_RL
      SEAICE_wetIceAlb_south  = UNSET_RL
      SEAICE_drySnowAlb_south = UNSET_RL
      SEAICE_wetSnowAlb_south = UNSET_RL
      HO_south                = UNSET_RL
C     basal drag parameters following Lemieux et al. (2015)
      SEAICE_cBasalStar = UNSET_RL
      SEAICEbasalDragU0 =  5. _d -05
      SEAICEbasalDragK1 =  8. _d 0
      SEAICEbasalDragK2 =  0. _d 0
C     Lemieux et al. (2015) recommend: SEAICEbasalDragK2 = 15. _d 0
C
      SEAICE_wetAlbTemp  = -1. _d -3
#ifdef SEAICE_EXTERNAL_FLUXES
      SEAICE_waterAlbedo = UNSET_RL
#else /* if undef SEAICE_EXTERNAL_FLUXES */
      SEAICE_waterAlbedo = 0.1    _d +00
#endif /* SEAICE_EXTERNAL_FLUXES */
      SEAICE_strength    = 2.75   _d +04
      SEAICE_cStar       = 20.    _d 0
      SEAICEpressReplFac = 1.     _d 0
      SEAICE_eccen       = 2.     _d 0
      SEAICE_eccfr       = UNSET_RL
      SEAICE_tensilFac   = 0.     _d 0
      SEAICE_tensilDepth = 0.     _d 0
      SEAICEtdMU         = 1.     _d 0
      SEAICEmcMu         = 1.     _d 0
C     coefficients for flux computations/bulk formulae
      SEAICE_dalton      = 1.75   _d -03
#ifdef ALLOW_EXF
      IF ( useEXF ) THEN
C     Use parameters that have already been set in data.exf
C     to be consistent
       SEAICE_rhoAir     = atmrho
       SEAICE_cpAir      = atmcp
       SEAICE_lhEvap     = flamb
       SEAICE_lhFusion   = flami
       SEAICE_boltzmann  = stefanBoltzmann
       SEAICE_emissivity = ocean_emissivity
       SEAICE_ice_emiss  = ice_emissivity
       SEAICE_snow_emiss = snow_emissivity
      ELSE
#else
      IF ( .TRUE. ) THEN
#endif /* ALLOW_EXF */
       SEAICE_rhoAir     = 1.3    _d 0
       SEAICE_cpAir      = 1004.  _d 0
       SEAICE_lhEvap     = 2.50   _d 6
       SEAICE_lhFusion   = 3.34   _d 5
       SEAICE_boltzmann  = 5.670  _d -08
C     old default value of 0.97001763668430343479
       SEAICE_emissivity = 5.5    _d -08/5.670 _d -08
       SEAICE_ice_emiss  = SEAICE_emissivity
       SEAICE_snow_emiss = SEAICE_emissivity
      ENDIF
      SEAICE_iceConduct  = 2.1656 _d +00
      SEAICE_snowConduct = 3.1    _d -01
      SEAICE_snowThick   = 0.15   _d 0
      SEAICE_shortwave   = 0.30   _d 0
      SEAICE_salt0       = 0.0    _d 0
      SEAICE_saltFrac    = 0.0    _d 0
#ifdef SEAICE_ITD
C in case defined(SEAICE_ITD) MULTDIM = nITD (see SEAICE_SIZE.h)
c      SEAICE_multDim     = MULTDIM
C the switch MULTICATEGORY (and with it parameter MULTDIM) has been retired
C  and SEAICE_multDim is now a runtime parameter;
C  in case SEAICE_multDim is given in data.seaice it needs to be overwritten
C  after PARM01 was read (see below)
      SEAICE_multDim     = nITD
      SEAICE_PDF(1)     = 1. _d 0
      DO l=2,nITD
       SEAICE_PDF(l)     = 0. _d 0
      ENDDO
#else
      SEAICE_multDim     = 1
      DO l=1,nITD
       SEAICE_PDF(l)     = UNSET_RL
      ENDDO
#endif
      SEAICE_useMultDimSnow = .TRUE.
C     default to be set later (ocean-seaice turbulent flux coeff):
      SEAICE_mcPheeStepFunc = .FALSE.
      SEAICE_mcPheeTaper    = UNSET_RL
      SEAICE_availHeatTaper = UNSET_RL
      SEAICE_mcPheePiston   = UNSET_RL
      SEAICE_frazilFrac     = UNSET_RL
      SEAICE_gamma_t        = UNSET_RL
      SEAICE_gamma_t_frz    = UNSET_RL
      SEAICE_availHeatFrac  = UNSET_RL
      SEAICE_availHeatFracFrz = UNSET_RL
      SEAICE_doOpenWaterGrowth=.TRUE.
      SEAICE_doOpenWaterMelt=.FALSE.
      SEAICE_areaLossFormula=1
      SEAICE_areaGainFormula=1
      SEAICE_tempFrz0    = 0.0901 _d 0
      SEAICE_dTempFrz_dS = -0.0575 _d 0
C     old default for constant freezing point
c     SEAICE_tempFrz0    = -1.96 _d 0
c     SEAICE_dTempFrz_dS = 0. _d 0
      SEAICEstressFactor = 1.     _d 0
      SEAICE_tauAreaObsRelax = -999. _d 0
      AreaFile   = ' '
      HsnowFile  = ' '
      HsaltFile  = ' '
      HeffFile   = ' '
      uIceFile   = ' '
      vIceFile   = ' '
      IMAX_TICE  = 10
      postSolvTempIter = 2
C     LSR parameters
      SEAICEuseLSR = .TRUE.
      SEAICEusePicardAsPrecon = .FALSE.
      SEAICE_LSRrelaxU = 0.95 _d 0
      SEAICE_LSRrelaxV = 0.95 _d 0
      SOLV_NCHECK= 2
      SEAICEnonLinIterMax = UNSET_I
      SEAICElinearIterMax = UNSET_I
      SEAICEpreconNL_Iter =  0
      SEAICEpreconLinIter = 10
#ifdef SEAICE_ALLOW_FREEDRIFT
      LSR_mixIniGuess =  0
#else
      LSR_mixIniGuess = -1
#endif
      LSR_ERROR  = 0.00001    _d 0
      SEAICEuseMultiTileSolver = .FALSE.

      SEAICE_area_floor = siEPS
      SEAICE_area_reg   = siEPS
      SEAICE_hice_reg   = 0.05 _d 0
      SEAICE_area_max   = 1.00 _d 0

      SEAICE_airTurnAngle   = 0.0 _d 0
      SEAICE_waterTurnAngle = 0.0 _d 0
      MIN_ATEMP         = -50.    _d 0
      MIN_LWDOWN        = 60.     _d 0
      MIN_TICE          = -50.    _d 0
      SEAICE_deltaMin   = UNSET_RL
      SEAICE_EPS        = 1.      _d -10
      SEAICE_EPS_SQ     = -99999.

      SEAICEwriteState  = .FALSE.
      SEAICE_monFreq    = monitorFreq
      SEAICE_dumpFreq   = dumpFreq
      SEAICE_taveFreq   = taveFreq
#ifdef ALLOW_MNC
      SEAICE_tave_mnc = timeave_mnc
      SEAICE_dump_mnc = snapshot_mnc
      SEAICE_mon_mnc  = monitor_mnc
#else
      SEAICE_tave_mnc = .FALSE.
      SEAICE_dump_mnc = .FALSE.
      SEAICE_mon_mnc  = .FALSE.
#endif
      SEAICE_debugPointI = UNSET_I
      SEAICE_debugPointJ = UNSET_I
      SINegFac = 1. _d 0
C-    Retired parameters:
c     LAD        = 2
      LAD        = UNSET_I
      NPSEUDOTIMESTEPS   = UNSET_I
      SOLV_MAX_ITERS     = UNSET_I
      SEAICEnewtonIterMax= UNSET_I
      SEAICEkrylovIterMax= UNSET_I
      JFNKgamma_nonlin   = UNSET_RL
c     SEAICE_sensHeat    = 1.75 _d -03 * 1004 * 1.3
c     SEAICE_sensHeat    = 2.284  _d +00
      SEAICE_sensHeat    = UNSET_RL
c     SEAICE_latentWater = 1.75 _d -03 * 2.500 _d 06 * 1.3
c     SEAICE_latentWater = 5.6875 _d +03
      SEAICE_latentWater = UNSET_RL
c     SEAICE_latentIce   = 1.75 _d -03 * 2.834 _d 06 * 1.3
c     SEAICE_latentIce   = 6.4474 _d +03
      SEAICE_latentIce   = UNSET_RL
      SEAICE_salinity    = UNSET_RL
      SIsalFRAC          = UNSET_RL
      SIsal0             = UNSET_RL
      IceAgeFile         = ' '
c     MAX_TICE           = 30.     _d 0
      MAX_TICE           = UNSET_RL
      areaMin            = UNSET_RL
      hiceMin            = UNSET_RL
      A22                = UNSET_RL
      areaMax            = UNSET_RL
      SEAICE_lhSublim    = UNSET_RL
      SEAICEadvAge       = .TRUE.
      SEAICEadvSchAge    = UNSET_I
      SEAICEdiffKhAge    = UNSET_RL
      IceAgeTrFile(1)    = ' '
      IceAgeTrFile(2)    = ' '
      IceAgeTrFile(3)    = ' '
      IceAgeTrFile(4)    = ' '
      SEAICEturbFluxFormula =UNSET_I
      SEAICE_freeze      = UNSET_RL
      MAX_HEFF           = UNSET_RL
      SEAICEuseAB2       = .FALSE.
      SEAICE_abEps       = UNSET_RL
C-    end retired parameters

#ifdef ALLOW_COST
      locDate(1) = 0
      locDate(2) = 0
# ifdef ALLOW_CAL
      CALL CAL_GETDATE( -1, startTime, locDate, myThid )
# endif /* ALLOW_CAL */
      mult_ice_export   =  0. _d 0
      mult_ice          =  0. _d 0
      costIceStart1     = locDate(1)
      costIceStart2     = locDate(2)
      costIceEnd1       =  0
      costIceEnd2       =  0
      cost_ice_flag     =  1
      SEAICE_clamp_salt  = 27.5   _d 0
      SEAICE_clamp_theta = 0.0001 _d 0
      SEAICE_cutoff_area = 0.0001 _d 0
      SEAICE_cutoff_heff = 0. _d 0
c
      mult_smrsst       =  0. _d 0
      mult_smrsss       =  0. _d 0
      mult_smrarea      =  0. _d 0
      wsmrarea0         =  0.5 _d 0
      wmean_smrarea     =  0.5 _d 0
      smrsstbarfile     =  'smrsstbar'
      smrsssbarfile     =  'smrsssbar'
      smrareabarfile    =  'smrareabar'
      smrareadatfile    =  ' '
      smrarea_errfile   =  ' '
      smrareastartdate1 = locDate(1)
      smrareastartdate2 = locDate(2)
#endif /* ALLOW_COST */

#ifdef ALLOW_SITRACER
      SItrNumInUse=SItrMaxNum
      DO iTracer = 1, SItrMaxNum
       SItrFile(iTracer) = ' '
       SItrName(iTracer) = ' '
       SItrNameLong(iTracer) = ' '
       SItrUnit(iTracer) = ' '
       SItrMate(iTracer) = 'HEFF'
       SItrFromOcean0(iTracer)    = ZERO
       SItrFromOceanFrac(iTracer) = ZERO
       SItrFromFlood0(iTracer)    = ZERO
       SItrFromFloodFrac(iTracer) = ZERO
       SItrExpand0(iTracer)       = ZERO
      ENDDO
#endif /* ALLOW_SITRACER */
      nRetired = 0
      nError   = 0

C     Open and read the data.seaice file
      WRITE(msgBuf,'(A)')
     &' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)') ' SEAICE_READPARMS: opening data.seaice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.seaice', 'SEAICE_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Read settings from model parameter file "data.seaice".
      READ(UNIT=iUnit,NML=SEAICE_PARM01)

#ifdef ALLOW_COST
      READ(UNIT=iUnit,NML=SEAICE_PARM02)
#endif /* ALLOW_COST */

#ifdef ALLOW_SITRACER
      READ(UNIT=iUnit,NML=SEAICE_PARM03)
#endif /* ALLOW_SITRACER */

#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

#ifdef SEAICE_ITD
C SEAICE_multDim has become a runtime parameter but if SEAICE_ITD is defined
C  it needs to equal nITD because of shared code (mostly in seaice_growth.F).
C nITD is set in SEAICE_SIZE.h
      SEAICE_multDim     = nITD
#endif

      WRITE(msgBuf,'(A)')
     &     ' SEAICE_READPARMS: finished reading data.seaice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)

C--   Set default values (if not specified in data.seaice namelist)

C--   Default for regularizing Delta to remain backward compatible
      IF ( SEAICE_deltaMin .EQ. UNSET_RL ) SEAICE_deltaMin = SEAICE_EPS
C--   Default is to have a normal flow rule if SEAICE_eccfr is not set
      IF (SEAICE_eccfr .EQ. UNSET_RL ) SEAICE_eccfr = SEAICE_eccen

C--   If no PDF was prescribed use the default uniform pdf
      tmp = SEAICE_multDim
      DO l = 1, SEAICE_multDim
       IF (SEAICE_PDF(l).EQ.UNSET_RL) SEAICE_PDF(l) = ONE/tmp
      ENDDO
      DO l = SEAICE_multDim+1, nITD
       IF (SEAICE_PDF(l).EQ.UNSET_RL) SEAICE_PDF(l) = 0. _d 0
      ENDDO

      IF (ICE2WATR.EQ.UNSET_RL) ICE2WATR = SEAICE_rhoIce*recip_rhoConst
      IF (SEAICE_drag_south       .EQ. UNSET_RL)
     &    SEAICE_drag_south       = SEAICE_drag
      IF (SEAICE_waterDrag_south  .EQ. UNSET_RL)
     &    SEAICE_waterDrag_south  = SEAICE_waterDrag
      IF (SEAICE_dryIceAlb_south  .EQ. UNSET_RL)
     &    SEAICE_dryIceAlb_south  = SEAICE_dryIceAlb
      IF (SEAICE_wetIceAlb_south  .EQ. UNSET_RL)
     &    SEAICE_wetIceAlb_south  = SEAICE_wetIceAlb
      IF (SEAICE_drySnowAlb_south .EQ. UNSET_RL)
     &    SEAICE_drySnowAlb_south = SEAICE_drySnowAlb
      IF (SEAICE_wetSnowAlb_south .EQ. UNSET_RL)
     &    SEAICE_wetSnowAlb_south = SEAICE_wetSnowAlb
      IF (HO_south                .EQ. UNSET_RL)
     &    HO_south                = HO
C     Basal drag parameter
      IF (SEAICE_cBasalStar .EQ. UNSET_RL)
     &     SEAICE_cBasalStar = SEAICE_cStar

C     Check that requested time step size is supported.  The combination
C     below is the only one that is supported at this time.  Does not
C     mean that something fancier will not work, just that it has not
C     yet been tried nor thought through.
      IF ( SEAICE_deltaTtherm .NE. dTtracerLev(1)     .OR.
     &     SEAICE_deltaTdyn   .LT. SEAICE_deltaTtherm .OR.
     &     (SEAICE_deltaTdyn/SEAICE_deltaTtherm) .NE.
     &     INT(SEAICE_deltaTdyn/SEAICE_deltaTtherm) ) THEN
         WRITE(msgBuf,'(A)')
     &        'Unsupported combination of SEAICE_deltaTtherm,'
         CALL PRINT_ERROR( msgBuf , myThid)
         WRITE(msgBuf,'(A)')
     &        ' SEAICE_deltaTdyn, and dTtracerLev(1)'
         CALL PRINT_ERROR( msgBuf , myThid)
         nError = nError + 1
      ENDIF
      SEAICEuseEVP = .FALSE.
#ifdef SEAICE_ALLOW_EVP
C     There are three ways to turn on EVP
C     1. original EVP (Hunke, 2001)
      IF ( SEAICE_deltaTevp .NE. UNSET_RL ) SEAICEuseEVP = .TRUE.
C     2. modified EVP (Lemieux et al., 2012) or revised EVP (Bouillon
C     et al., 2014) by setting alpha and beta
      IF ( SEAICE_evpAlpha  .NE. UNSET_RL
     &  .OR. SEAICE_evpBeta .NE. UNSET_RL ) SEAICEuseEVP = .TRUE.
C     3. adaptive EVP
      IF ( SEAICEaEVPcoeff  .NE. UNSET_RL ) SEAICEuseEVP = .TRUE.
C     if EVP is turned on, a couple of parameters need to be computed
      IF ( SEAICEuseEVP ) THEN
       IF (    (SEAICE_deltaTdyn/SEAICE_deltaTevp) .NE.
     &      INT(SEAICE_deltaTdyn/SEAICE_deltaTevp) .AND.
     &      .NOT. (SEAICEuseEVPstar.OR.SEAICEuseEVPrev) ) THEN
        WRITE(msgBuf,'(A)')
     &       'SEAICE_deltaTevp must be a factor of SEAICE_deltaTdyn.'
        CALL PRINT_ERROR( msgBuf , myThid)
        nError = nError + 1
       ENDIF
       IF ( SEAICE_elasticParm .LE. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A)')
     &       'SEAICE_elasticParm must greater than 0.'
        CALL PRINT_ERROR( msgBuf , myThid)
        nError = nError + 1
       ENDIF
       IF ( SEAICE_evpTauRelax .LE. 0. _d 0 )
     &      SEAICE_evpTauRelax = SEAICE_deltaTdyn*SEAICE_elasticParm
C     determine number of internal steps
       IF ( SEAICEnEVPstarSteps.EQ.UNSET_I ) THEN
        IF ( SEAICE_deltaTevp.EQ.UNSET_RL ) THEN
         WRITE(msgBuf,'(A,A)') 'S/R SEAICE_readparms: Either ',
     &        'SEAICEnEVPstarSteps or SEAICE_deltaTevp need to be set.'
         CALL PRINT_ERROR( msgBuf , myThid)
         nError = nError + 1
        ELSE
         SEAICEnEVPstarSteps = INT(SEAICE_deltaTdyn/SEAICE_deltaTevp)
        ENDIF
       ENDIF
C     default: evpAlpha = evpBeta
       IF ( SEAICE_evpAlpha .NE. UNSET_RL .AND.
     &  SEAICE_evpBeta .EQ. UNSET_RL ) SEAICE_evpBeta = SEAICE_evpAlpha
       IF ( SEAICE_evpBeta .NE. UNSET_RL .AND.
     & SEAICE_evpAlpha .EQ. UNSET_RL ) SEAICE_evpAlpha = SEAICE_evpBeta
C     derive other parameters
       IF ( SEAICE_evpBeta .EQ. UNSET_RL ) THEN
        SEAICE_evpBeta   = SEAICE_deltaTdyn/SEAICE_deltaTevp
       ELSE
        SEAICE_deltaTevp = SEAICE_deltaTdyn/SEAICE_evpBeta
       ENDIF
       IF ( SEAICE_evpAlpha .EQ. UNSET_RL ) THEN
        SEAICE_evpAlpha = 2. _d 0 * SEAICE_evpTauRelax/SEAICE_deltaTevp
       ELSE
        SEAICE_evpTauRelax = 0.5 _d 0 *SEAICE_evpAlpha*SEAICE_deltaTevp
       ENDIF
C     this turns on adaptive EVP
       IF ( SEAICEaEVPcoeff .NE. UNSET_RL ) THEN
        IF ( SEAICEaEVPcStar  .EQ.UNSET_RL) SEAICEaEVPcStar   =4. _d 0
        IF (SEAICEaEVPalphaMin.EQ.UNSET_RL) SEAICEaEVPalphaMin=5. _d 0
C     requires EVP* to work well, so make sure we set it here (commented out
C     for now, but these values are the default values now)
CML        SEAICEuseEVPstar   = .TRUE.
CML        SEAICEuseEVPrev    = .TRUE.
C     For adaptive EVP we do not need constant parameters alpha and
C     beta, because they are computed dynamically. Reset them to
C     undefined here, so that we know if something funny is going on.
        SEAICE_evpAlpha     = UNSET_RL
        SEAICE_evpBeta      = UNSET_RL
       ENDIF
C     Check if all parameters are set.
      ENDIF
#endif /* SEAICE_ALLOW_EVP */

#ifdef SEAICE_ALLOW_FREEDRIFT
      IF ( SEAICEuseFREEDRIFT ) SEAICEuseEVP = .FALSE.
      IF ( SEAICEuseFREEDRIFT ) THEN
        WRITE(msgBuf,'(A,A)')
     &       'WARNING FROM S/R SEAICE_READPARMS:',
     &       ' switch seaice from LSR or EVP to "free drift"'
      CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      ENDIF
#endif /* SEAICE_ALLOW_FREEDRIFT */

#ifndef SEAICE_ITD
      IF ( .NOT.useHibler79IceStrength ) THEN
       useHibler79IceStrength = .TRUE.
       WRITE(msgBuf,'(A,A)')
     &      'WARNING FROM S/R SEAICE_READPARMS:',
     &      ' resetting useHibler79IceStrength = .TRUE., because'
      CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
       WRITE(msgBuf,'(A,A)')
     &      'WARNING FROM S/R SEAICE_READPARMS:',
     &      ' SEAICE_ITD is not defined.'
      CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      ENDIF
#endif /* SEAICE_ITD */

C     reset default SEAICEuseLSR according to parameters from namelist
      SEAICEuseLSR = .NOT.SEAICEuseFREEDRIFT .AND. .NOT.SEAICEuseEVP
     &     .AND. .NOT.SEAICEuseJFNK .AND. .NOT. SEAICEuseKrylov
C     allow SEAICEuseLSR = .TRUE. if used as a preconditioner for non-linear
C     JFNK problem (and Krylov solver is not used)
      IF ( SEAICEuseJFNK .AND. SEAICEusePicardAsPrecon .AND.
     &     .NOT. SEAICEuseKrylov ) SEAICEuseLSR = .TRUE.
      IF ( SEAICEuseJFNK .AND. .NOT. SEAICEusePicardAsPrecon )
     &     SEAICEuseKrylov = .FALSE.

C     Set different defaults for different solvers
      IF ( SEAICEnonLinIterMax .EQ. UNSET_I ) THEN
C     two nonlinear iterations correspond to the original modified
C     Euler time stepping scheme of Zhang+Hibler (1997)
       IF ( SEAICEuseLSR ) SEAICEnonLinIterMax = 2
       IF ( SEAICEuseJFNK.OR.SEAICEuseKrylov ) SEAICEnonLinIterMax = 10
      ENDIF
C     Make sure that we have least two pseudo time steps for Picard-LSR
      IF ( SEAICEuseLSR .AND. .NOT. SEAICEusePicardAsPrecon )
     &     SEAICEnonLinIterMax = MAX(SEAICEnonLinIterMax,2)

C-    different defaults for different linear solvers
      IF ( SEAICElinearIterMax .EQ. UNSET_I ) THEN
C     maximum number of LSOR steps in default Picard solver
C     (=previous default for retired SOLV_MAX_ITERS)
#ifdef ALLOW_AUTODIFF
       SEAICElinearIterMax = SOLV_MAX_FIXED
#else
       SEAICElinearIterMax = 1500
#endif
C     the maximum number of Krylov dimensions of 50 is hard coded in
C     S/R SEAICE_FGMRES, so that more than 50 linear iterations will
C     restart GMRES
       IF ( SEAICEuseJFNK.OR.SEAICEuseKrylov ) SEAICElinearIterMax = 10
      ENDIF

C     Turn line search with JFNK solver off by default by making this
C     number much larger than the maximum allowed Newton iterations
      IF ( SEAICE_JFNK_lsIter .EQ. UNSET_I )
     &     SEAICE_JFNK_lsIter  = 2*SEAICEnewtonIterMax

C     2nd order boundary conditions only possible for no_slip,
C     and EVP, JFNK, and Krylov solvers
      IF ( .NOT. SEAICE_no_slip ) SEAICE_2ndOrderBC = .FALSE.
      IF ( SEAICEuseLSR ) SEAICE_2ndOrderBC = .FALSE.

C     2nd order boundary conditions require one more row of overlap for the additive Schwartz method
      IF ( SEAICE_2ndOrderBC ) THEN
       SEAICE_OLx = OLx-3
       SEAICE_OLy = OLy-3
      ENDIF

C-    The old ways of specifying mcPheeTaper, mcPheePiston & frazilFrac:
C     a) prevent multiple specification of the same coeff;
C     b) if specified, then try to recover old way of setting & default.
      IF ( SEAICE_mcPheeTaper .EQ. UNSET_RL ) THEN
       IF ( SEAICE_availHeatTaper.EQ.UNSET_RL ) THEN
         SEAICE_mcPheeTaper = 0.0 _d 0
       ELSE
         SEAICE_mcPheeTaper = SEAICE_availHeatTaper
       ENDIF
      ELSEIF ( SEAICE_availHeatTaper.NE.UNSET_RL ) THEN
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: Cannot specify ',
     &    'both SEAICE_mcPheeTaper & SEAICE_availHeatTaper'
         CALL PRINT_ERROR( msgBuf , myThid)
         nError = nError + 1
      ENDIF

C-    set SEAICE_frazilFrac if not yet done
      IF ( SEAICE_gamma_t_frz .NE. UNSET_RL ) THEN
       IF ( SEAICE_frazilFrac .EQ. UNSET_RL ) THEN
         SEAICE_frazilFrac = SEAICE_deltaTtherm/SEAICE_gamma_t_frz
       ELSE
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: Cannot specify ',
     &    'both SEAICE_frazilFrac & SEAICE_gamma_t_frz'
         CALL PRINT_ERROR( msgBuf , myThid)
         nError = nError + 1
       ENDIF
      ENDIF
      IF ( SEAICE_availHeatFracFrz.NE.UNSET_RL ) THEN
       IF ( SEAICE_frazilFrac .EQ. UNSET_RL ) THEN
         SEAICE_frazilFrac = SEAICE_availHeatFracFrz
       ELSE
        IF ( SEAICE_gamma_t_frz .EQ. UNSET_RL ) THEN
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: Cannot specify ',
     &    'both SEAICE_frazilFrac  & SEAICE_availHeatFracFrz'
        ELSE
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: Cannot specify ',
     &    'both SEAICE_gamma_t_frz & SEAICE_availHeatFracFrz'
        ENDIF
        CALL PRINT_ERROR( msgBuf , myThid)
        nError = nError + 1
       ENDIF
      ENDIF
C     the default for SEAICE_gamma_t_frz use to be SEAICE_gamma_t:
      IF ( SEAICE_gamma_t .NE. UNSET_RL .AND.
     &     SEAICE_frazilFrac .EQ. UNSET_RL ) THEN
         SEAICE_frazilFrac = SEAICE_deltaTtherm/SEAICE_gamma_t
      ENDIF
C     the default for SEAICE_availHeatFracFrz use to be SEAICE_availHeatFrac:
      IF ( SEAICE_availHeatFrac.NE.UNSET_RL .AND.
     &     SEAICE_frazilFrac .EQ. UNSET_RL ) THEN
         SEAICE_frazilFrac = SEAICE_availHeatFrac
      ENDIF
      IF ( SEAICE_frazilFrac .EQ. UNSET_RL ) THEN
         SEAICE_frazilFrac = 1. _d 0
      ENDIF

C-    start by setting SEAICE_availHeatFrac (used in seaice_init_fixed.F
C     to set SEAICE_mcPheePiston once drF is known)
      IF ( SEAICE_gamma_t .NE. UNSET_RL ) THEN
       IF ( SEAICE_availHeatFrac.EQ.UNSET_RL ) THEN
         SEAICE_availHeatFrac = SEAICE_deltaTtherm/SEAICE_gamma_t
       ELSE
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: Cannot specify ',
     &    'both SEAICE_gamma_t & SEAICE_availHeatFrac'
         CALL PRINT_ERROR( msgBuf , myThid)
         nError = nError + 1
       ENDIF
      ENDIF
      IF ( SEAICE_mcPheePiston .NE. UNSET_RL .AND.
     &     SEAICE_availHeatFrac.NE. UNSET_RL ) THEN
        IF ( SEAICE_gamma_t .EQ. UNSET_RL ) THEN
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: Cannot specify ',
     &    'both SEAICE_mcPheePiston & SEAICE_availHeatFrac'
        ELSE
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: Cannot specify ',
     &    'both SEAICE_mcPheePiston & SEAICE_gamma_t'
        ENDIF
        CALL PRINT_ERROR( msgBuf , myThid)
        nError = nError + 1
      ENDIF

      IF ( useThSice ) THEN
C     If the thsice package with the Winton thermodynamics is used
C     is does not make sense to have the following parameters defined,
C     so we reset them here
       usePW79thermodynamics = .FALSE.
       SEAICEadvHeff         = .FALSE.
       SEAICEadvArea         = .FALSE.
       SEAICEadvSnow         = .FALSE.
       SEAICEadvSalt         = .FALSE.
      ENDIF
C     Set advection schemes to some sensible values if not done in data.seaice
      IF ( SEAICEadvSchArea .EQ. UNSET_I )
     &     SEAICEadvSchArea = SEAICEadvSchHeff
      IF ( SEAICEadvSchArea .EQ. UNSET_I )
     &     SEAICEadvSchArea = SEAICEadvScheme
      IF ( SEAICEadvScheme .NE. SEAICEadvSchArea )
     &     SEAICEadvScheme  = SEAICEadvSchArea
      IF ( SEAICEadvSchHeff .EQ. UNSET_I )
     &     SEAICEadvSchHeff = SEAICEadvSchArea
      IF ( SEAICEadvSchSnow .EQ. UNSET_I )
     &     SEAICEadvSchSnow = SEAICEadvSchHeff
      IF ( SEAICEadvSchSalt .EQ. UNSET_I )
     &     SEAICEadvSchSalt = SEAICEadvSchHeff
C     Set diffusivity to some sensible values if not done in data.seaice
      IF ( SEAICEdiffKhArea .EQ. UNSET_RL )
     &     SEAICEdiffKhArea = SEAICEdiffKhHeff
      IF ( SEAICEdiffKhArea .EQ. UNSET_RL )
     &     SEAICEdiffKhArea = 0. _d 0
      IF ( SEAICEdiffKhHeff .EQ. UNSET_RL )
     &     SEAICEdiffKhHeff = SEAICEdiffKhArea
      IF ( SEAICEdiffKhSnow .EQ. UNSET_RL )
     &     SEAICEdiffKhSnow = SEAICEdiffKhHeff
      IF ( SEAICEdiffKhSalt .EQ. UNSET_RL )
     &     SEAICEdiffKhSalt = SEAICEdiffKhHeff
      IF ( SEAICE_EPS_SQ .EQ. -99999. )
     &     SEAICE_EPS_SQ = SEAICE_EPS * SEAICE_EPS

#ifdef ALLOW_GENERIC_ADVDIFF
      SEAICEmultiDimAdvection = .TRUE.
      IF ( SEAICEadvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.SEAICEadvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.SEAICEadvScheme.EQ.ENUM_CENTERED_4TH ) THEN
       SEAICEmultiDimAdvection = .FALSE.
      ENDIF
#else
      SEAICEmultiDimAdvection = .FALSE.
#endif /* ALLOW_GENERIC_ADVDIFF */

C-    Retired parameters
      IF ( SEAICEnewtonIterMax .NE. UNSET_I ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICEnewtonIterMax" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R SEAICE_READPARMS: use "SEAICEnonLinIterMax" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICEkrylovIterMax .NE. UNSET_I ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICEkrylovIterMax" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R SEAICE_READPARMS: use "SEAICElinearIterMax" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( NPSEUDOTIMESTEPS    .NE. UNSET_I ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "NPSEUDOTIMESTEPS" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R SEAICE_READPARMS: use "SEAICEnonLinIterMax" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SOLV_MAX_ITERS .NE. UNSET_I ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SOLV_MAX_ITERS" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R SEAICE_READPARMS: use "SEAICElinearIterMax" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( JFNKgamma_nonlin   .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "JFNKgamma_nonlin" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R SEAICE_READPARMS: use "SEAICEnonLinTol" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_sensHeat    .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_sensHeat" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: set "SEAICE_cpAir", ',
     &  '"SEAICE_dalton", and "SEAICE_rhoAir" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_latentWater .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_latentWater" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: set "SEAICE_lhEvap", ',
     &  '"SEAICE_dalton", and "SEAICE_rhoAir" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_latentIce   .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_latentIce" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: set "SEAICE_lhFusion", ',
     &  '"SEAICE_dalton", and "SEAICE_rhoAir" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_freeze .NE. UNSET_RL ) THEN
       WRITE(msgBuf,'(A,A)')'S/R SEAICE_READPARMS: ',
     &  '"SEAICE_freeze" no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')'S/R SEAICE_READPARMS: ',
     &  'set instead "SEAICE_tempFrz0" and "SEAICE_dTempFrz_dS"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_salinity   .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R SEAICE_READPARMS: ',
     &  '"SEAICE_salinity" is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R SEAICE_READPARMS: ',
     &  'set "SEAICE_saltFrac" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SIsalFrac .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R SEAICE_READPARMS: ',
     &  '"SIsalFrac" is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R SEAICE_READPARMS: ',
     &  'set "SEAICE_saltFrac" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SIsal0 .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R SEAICE_READPARMS: ',
     &  '"SIsal0" is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R SEAICE_READPARMS: ',
     &  'set "SEAICE_salt0" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( IceAgeFile .NE. ' ' ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "IceAgeFile" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"IceAgeTrFile(SEAICE_num)" array '
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( areaMax .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "areaMax" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"SEAICE_area_max"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( areaMin .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "areaMin" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"SEAICE_area_reg" for regularization and ',
     &  '"SEAICE_area_floor" setting a lower bound'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF (SEAICE_lhSublim .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_lhSublim" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: specify ',
     &  '"SEAICE_lhFusion" and "SEAICE_lhEvap" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( A22 .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "A22" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"SEAICE_area_reg" for regularization'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( LAD .NE. UNSET_I ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R SEAICE_READPARMS: "LAD" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'always use modified Euler step ',
     &  '(LAD==2) since Leap frog code (LAD==1) is gone.'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( MAX_TICE .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "MAX_TICE" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( hiceMin .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "hiceMin" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"SEAICE_hice_reg" for regularization'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT. SEAICEadvAge ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICEadvAge" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: since ALLOW_SITRACER ',
     &  'replaced and extended SEAICE_AGE'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICEadvSchAge .NE. UNSET_I ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICEadvSchAge" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: since ALLOW_SITRACER ',
     &  'replaced and extended SEAICE_AGE'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICEdiffKhAge .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICEdiffKhAge" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: since ALLOW_SITRACER ',
     &  'replaced and extended SEAICE_AGE'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( ( IceAgeTrFile(1) .NE. ' ' ).OR.
     &     ( IceAgeTrFile(2) .NE. ' ' ).OR.
     &     ( IceAgeTrFile(3) .NE. ' ' ).OR.
     &     ( IceAgeTrFile(4) .NE. ' ' ) ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "IceAgeTrFile" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: since ALLOW_SITRACER ',
     &  'replaced and extended SEAICE_AGE'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICEturbFluxFormula .NE. UNSET_I ) THEN
       WRITE(msgBuf,'(A,A)')'S/R SEAICE_READPARMS: ',
     &  '"SEAICEturbFluxFormula" no longer allowed in "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')'S/R SEAICE_READPARMS: ',
     &  ' Set instead "SEAICE_mcPheePiston" and "SEAICE_frazilFrac"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( MAX_HEFF .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "MAX_HEFF" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICEuseAB2 ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICEuseAB2" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_abEps .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_abEps" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: ',
     &  'Error reading parameter file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,I3,A)') 'S/R SEAICE_READPARMS: ', nRetired,
     &      ' out of date parameters were found in the namelist'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

      IF ( nError .GT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'S/R SEAICE_READPARMS: ',
     &  'Error reading parameter file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,I3,A)') 'S/R SEAICE_READPARMS: ', nError,
     &  ' parameters values are inconsistent or incomplete'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

      IF ( nRetired .GT. 0 .OR. nError .GT. 0 ) THEN
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF

C--   Now set-up any remaining parameters that result from other params

C-    convert SEAICE_doOpenWaterGrowth/Melt logical switch to numerical
C     facOpenGrow/facOpenMelt
      facOpenGrow = 0. _d 0
      facOpenMelt = 0. _d 0
      IF (SEAICE_doOpenWaterGrowth) facOpenGrow = 1. _d 0
      IF (SEAICE_doOpenWaterMelt)   facOpenMelt = 1. _d 0

C-    Set Output type flags :
      SEAICE_tave_mdsio = .TRUE.
      SEAICE_dump_mdsio = .TRUE.
      SEAICE_mon_stdio  = .TRUE.
#ifdef ALLOW_MNC
      IF (useMNC) THEN
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_tave_mnc ) SEAICE_tave_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_dump_mnc ) SEAICE_dump_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_mon_mnc  ) SEAICE_mon_stdio  = .FALSE.
      ENDIF
#endif

C-    store value of logical flag which might be changed in AD mode
#ifdef ALLOW_AUTODIFF
      SEAICEuseFREEDRIFTinFwdMode = SEAICEuseFREEDRIFT
      SEAICEuseDYNAMICSinFwdMode  = SEAICEuseDYNAMICS
#endif /* ALLOW_AUTODIFF */

C     Check the consitency of a few parameters
      IF ( SEAICE_emissivity .LT. 1. _d -04 ) THEN
       WRITE(msgBuf,'(2A)')
     &      'SEAICE_emissivity is no longer emissivity*(boltzmann ',
     &      'constant) but really an emissivity.'
       CALL PRINT_ERROR( msgBuf , myThid)
       WRITE(msgBuf,'(2A)')
     &      'Typical values are near 1 ',
     &      '(default is 5.5/5.67=0.9700176...).'
       CALL PRINT_ERROR( msgBuf , myThid)
       WRITE(msgBuf,'(A,E13.6,A)')
     &      'Please change SEAICE_emissivity in data.seaice to ',
     &      SEAICE_emissivity, '/5.67e-8.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF

C--   Since the default of SEAICE_waterDrag has changed, issue a warning
C     in case of large values
      chkFlag = .FALSE.
      IF ( SEAICE_waterDrag .GT. 1. _d 0 ) THEN
       WRITE(msgBuf,'(A,A,F5.2)') '** WARNING ** SEAICE_READPARMS: ',
     &      'SEAICE_waterDrag = ', SEAICE_waterDrag
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       chkFlag = .TRUE.
      ENDIF
      IF ( SEAICE_waterDrag_South .GT. 1. _d 0 ) THEN
       WRITE(msgBuf,'(A,A,F5.2)') '** WARNING ** SEAICE_READPARMS: ',
     &      'SEAICE_waterDrag_South = ', SEAICE_waterDrag_South
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       chkFlag = .TRUE.
      ENDIF
      IF ( chkFlag ) THEN
       WRITE(msgBuf,'(3A)') '** WARNING ** SEAICE_READPARMS: ',
     &      'That is 3 orders of magnitude larger',
     &      ' than the default of 5.5e-3.'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &      'Are you maybe using an old (pre Jun2018) data.seaice?'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( DIFF1 .EQ. UNSET_RL ) THEN
        DIFF1 = 0. _d 0
        chkFlag = .FALSE.
        IF ( SEAICEadvScheme.EQ.2 ) THEN
C--   Since DIFF1 default value has been changed (2011/05/29), issue a warning
C     in case using centered avection scheme without any diffusion:
         IF ( SEAICEadvHeff .AND. SEAICEdiffKhHeff .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for HEFF  without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( SEAICEadvArea .AND. SEAICEdiffKhArea .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for AREA  without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( SEAICEadvSnow .AND. SEAICEdiffKhSnow .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for HSNOW without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( SEAICEadvSalt .AND. SEAICEdiffKhSalt .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for HSALT without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( chkFlag ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &      'since DIFF1 is set to 0 (= new DIFF1 default value)'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDIF
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
