#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SET_DEFAULTS
C     !INTERFACE:
      SUBROUTINE SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRdefault,
     I   myThid )

C     !DESCRIPTION:
C     Routine to set model "parameter defaults".

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
#endif /* ALLOW_EXCH2 */
#include "SET_GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id. Number
      INTEGER myThid
      _RL viscArDefault
      _RL diffKrTDefault
      _RL diffKrSDefault
      _RL hFacMinDrDefault
      _RL delRDefault(Nr)

C     !LOCAL VARIABLES:
C     i, j, k :: Loop counters
      INTEGER i, j, k
CEOP

C--   Grid parameters
C-    Vertical gridding
      delRFile            = ' '
      delRcFile           = ' '
      hybSigmFile         = ' '
      seaLev_Z            = UNSET_RL
      top_Pres            = UNSET_RL
      rSigmaBnd           = UNSET_RL
      selectSigmaCoord    = 0
      DO k=1,Nr
       delRdefault(k)     = 0.
      ENDDO
      DO k=1,Nr+1
       delRc(k)           = UNSET_RL
      ENDDO
      useMin4hFacEdges    = .FALSE.
C-    vertical profile
      tRefFile            = ' '
      sRefFile            = ' '
      rhoRefFile          = ' '
      gravityFile         = ' '
      thetaConst          = UNSET_RL
      DO k=1,Nr
       tRef(k)            = UNSET_RL
       sRef(k)            = UNSET_RL
      ENDDO

C-    Horizontal gridding
      delXFile            = ' '
      delYFile            = ' '
      horizGridFile       = ' '
      deepAtmosphere      = .FALSE.
      xgOrigin            = UNSET_RL
      ygOrigin            = UNSET_RL
      DO i=1,grid_maxNx
       delX(i)            = UNSET_RL
      ENDDO
      DO j=1,grid_maxNy
       delY(j)            = UNSET_RL
      ENDDO
C     In cartesian coords distances are in metres
      usingCartesianGrid  = .FALSE.
C     In spherical polar distances are in degrees
      usingSphericalPolarGrid = .FALSE.
      rSphere             = UNSET_RL
C     General curvilinear coordinate system
      usingCurvilinearGrid= .FALSE.
      radius_fromHorizGrid= UNSET_RL
      hasWetCSCorners     = .FALSE.
C     General cylindrical coordinate system
      usingCylindricalGrid= .FALSE.
C     Coriolis map:
      selectCoriMap       = -1
      use3dCoriolis       = .TRUE.
C     grid rotation
      rotateGrid          = .FALSE.
      phiEuler            = 0. _d 0
      thetaEuler          = 0. _d 0
      psiEuler            = 0. _d 0

C--   Set default "physical" parameters
      nh_Am2              = 1. _d 0
      gravity             = 9.81 _d 0
      gBaro               = UNSET_RL
      surf_pRef           = 101325. _d 0
      rhoNil              = 999.8 _d 0
      rhoConst            = UNSET_RL
C-- jmc : the default is to set rhoConstFresh to rhoConst (=rhoNil by default)
C         (so that the default produces same results as before)
c     rhoConstFresh       = 999.8 _d 0
      rhoConstFresh       = UNSET_RL
      convertFW2Salt      = UNSET_RL
      f0                  = 1. _d -4
      beta                = 1. _d -11
      fPrime              = 0. _d 0
C-    Earth rotation period is 86400*365.25/366.25 (use to be 1.day)
      rotationPeriod      = 86164. _d 0
      omega               = UNSET_RL
C-    viscosity and diffusivity default value:
      viscAh              = 0. _d 3
      smag3D_coeff        = 1. _d -2
      smag3D_diffCoeff    = 0. _d 0
      viscAhGrid          = 0. _d 0
      viscAhGridMin       = 0. _d 0
      viscAhGridMax       = 1. _d 21
      viscAhMax           = 1. _d 21
      viscAhReMax         = 0. _d 0
      viscC2leith         = 0. _d 0
      viscC2leithD        = 0. _d 0
      viscC2LeithQG       = 0. _d 0
      viscC2smag          = 0. _d 0
      viscArDefault       = 0. _d -3
      viscFacAdj          = 1. _d 0
      no_slip_sides       = .TRUE.
      no_slip_bottom      = .TRUE.
      bottomVisc_pCell    = .FALSE.
      sideDragFactor      = 2. _d 0
      bottomDragLinear    = 0.
      bottomDragQuadratic = 0.
      selectBotDragQuadr  = -1
      viscA4              = 0. _d 11
      viscA4Grid          = 0. _d 0
      viscA4GridMax       = 1. _d 21
      viscA4GridMin       = 0. _d 0
      viscA4Max           = 1. _d 21
      viscA4ReMax         = 0. _d 0
      viscC4leith         = 0. _d 0
      viscC4leithD        = 0. _d 0
      viscC4smag          = 0. _d 0
      DO k=1,Nr
       viscArNr(k)        = UNSET_RL
      ENDDO
      cosPower            = 0.
      diffKhT             = 0. _d 3
      diffKhS             = 0. _d 3
      diffK4T             = 0. _d 11
      diffK4S             = 0. _d 11
      diffKrTDefault      = 0. _d -3
      diffKrSDefault      = 0. _d -3
      diffKrBL79surf      = 0. _d 0
      diffKrBL79deep      = 0. _d 0
      diffKrBL79scl       = 200. _d 0
      diffKrBL79Ho        = -2000. _d 0
      BL79LatVary         = 30.
      diffKrBLEQsurf      = UNSET_RL
      diffKrBLEQdeep      = UNSET_RL
      diffKrBLEQscl       = UNSET_RL
      diffKrBLEQHo        = UNSET_RL
      DO k=1,Nr
       diffKrNrT(k)       = UNSET_RL
       diffKrNrS(k)       = UNSET_RL
       diffKr4T(k)        = 0.
       diffKr4S(k)        = 0.
      ENDDO
      HeatCapacity_Cp     = 3994. _d 0
      eosType             = 'LINEAR'
      buoyancyRelation    = 'OCEANIC'
      selectP_inEOS_Zc    = UNSET_I
      smoothAbsFuncRange  = 0. _d 0
      sIceLoadFac         = 1. _d 0
      hFacMin             = 1. _d 0
      hFacMinDrDefault    = 0. _d 0
      implicitIntGravWave = .FALSE.
      staggerTimeStep     = .FALSE.
      applyExchUV_early   = .FALSE.
      doResetHFactors     = .FALSE.
      momViscosity        = .TRUE.
      momAdvection        = .TRUE.
      momForcing          = .TRUE.
      momTidalForcing     = .TRUE.
      useCoriolis         = .TRUE.
      momPressureForcing  = .TRUE.
      momStepping         = .TRUE.
      vectorInvariantMomentum = .FALSE.
      tempStepping        = .TRUE.
      tempAdvection       = .TRUE.
      tempForcing         = .TRUE.
      temp_stayPositive   = .FALSE.
      saltStepping        = .TRUE.
      saltAdvection       = .TRUE.
      saltForcing         = .TRUE.
      salt_stayPositive   = .FALSE.
      addFrictionHeating  = .FALSE.
      metricTerms         = .TRUE.
      useNHMTerms         = .FALSE.
      useSmag3D           = .FALSE.
      useFullLeith        = .FALSE.
      useAreaViscLength   = .FALSE.
      useStrainTensionVisc= .FALSE.
      implicitDiffusion   = .FALSE.
      implicitViscosity   = .FALSE.
      selectImplicitDrag  = 0
      momImplVertAdv      = .FALSE.
      tempImplVertAdv     = .FALSE.
      saltImplVertAdv     = .FALSE.
      nonHydrostatic      = .FALSE.
      quasiHydrostatic    = .FALSE.
      globalFiles         = .FALSE.
      useSingleCpuIO      = .FALSE.
      useSingleCpuInput   = .FALSE.
      allowFreezing       = .FALSE.
      ivdc_kappa          = 0. _d 0
      hMixCriteria        = -.8 _d 0
      dRhoSmall           = 1. _d -6
      hMixSmooth          = 0. _d 0
      usePickupBeforeC54    = .FALSE.
      tempAdvScheme       = 2
      saltAdvScheme       = 2
      multiDimAdvection   = .TRUE.
      useMultiDimAdvec    = .FALSE.
      useCDscheme         = .FALSE.
      selectCoriScheme    = UNSET_I
      selectVortScheme    = UNSET_I
      useJamartMomAdv     = .FALSE.
      upwindVorticity     = .FALSE.
      highOrderVorticity  = .FALSE.
      useAbsVorticity     = .FALSE.
      upwindShear         = .FALSE.
      selectKEscheme      = 0
      IF ( debugMode ) THEN
        debugLevel        = debLevD
      ELSE
        debugLevel        = debLevB
#ifdef ALLOW_AUTODIFF
        debugLevel        = debLevA
#endif
      ENDIF

C--   Set (free)surface-related parameters
      implicitFreeSurface = .FALSE.
      rigidLid            = .FALSE.
      implicSurfPress     = 1. _d 0
      implicDiv2DFlow     = 1. _d 0
      exactConserv        = .FALSE.
      linFSConserveTr     = .FALSE.
      uniformLin_PhiSurf  = .TRUE.
      nonlinFreeSurf      = 0
      hFacInf             = 0.2 _d 0
      hFacSup             = 2.0 _d 0
      select_rStar        = 0
      selectNHfreeSurf    = 0
      selectAddFluid      = 0
      useRealFreshWaterFlux = .FALSE.
      temp_EvPrRn = UNSET_RL
      salt_EvPrRn = 0.
      temp_addMass = UNSET_RL
      salt_addMass = UNSET_RL
      selectBalanceEmPmR  = UNSET_I
      balanceQnet         = .FALSE.
      balancePrintMean    = .FALSE.
      balanceThetaClimRelax = .FALSE.
      balanceSaltClimRelax  = .FALSE.

C--   Atmospheric physical parameters (e.g.: EOS)
      celsius2K = 273.15 _d 0
      atm_Po =  1. _d 5
      atm_Cp = 1004. _d 0
      atm_Rd = UNSET_RL
      atm_kappa = 2. _d 0 / 7. _d 0
      atm_Rq = 0. _d 0
      integr_GeoPot = 2
      selectFindRoSurf = 0

C--   Elliptic solver parameters
      cg2dMaxIters       = 150
      cg2dMinItersNSA    = 0
      cg2dTargetResidual = 1. _d -7
      cg2dTargetResWunit = -1.
      cg2dUseMinResSol   = UNSET_I
      cg2dpcOffDFac      = 0.51 _d 0
      cg2dPreCondFreq    = 1
      cg3dMaxIters       = 150
      cg3dTargetResidual = 1. _d -7
      useNSACGSolver     = .FALSE.
      useSRCGSolver      = .FALSE.

C--   Time stepping parameters
      deltaT            = 0. _d 0
      deltaTMom         = 0. _d 0
      deltaTFreeSurf    = 0. _d 0
      DO k=1,Nr
        dTtracerLev(k)  = 0. _d 0
      ENDDO
      baseTime          = 0. _d 0
      nIter0            = -1
      startTime         = UNSET_RL
      nTimeSteps        = 0
      nTimeSteps_l2     = 0
      nEndIter          = 0
      endTime           = 0. _d 0
      momForcingOutAB   = UNSET_I
      tracForcingOutAB  = UNSET_I
      momDissip_In_AB   = .TRUE.
      doAB_onGtGs       = .TRUE.
      abEps             = 0.01 _d 0
#ifdef ALLOW_ADAMSBASHFORTH_3
      alph_AB           = 0.5 _d 0
      beta_AB           = 5. _d 0 / 12. _d 0
      startFromPickupAB2= .FALSE.
#else
      alph_AB           = UNSET_RL
      beta_AB           = UNSET_RL
      startFromPickupAB2= .TRUE.
#endif
      cAdjFreq          =  0. _d 0
      tauCD             =  0. _d 0
      tauThetaClimRelax =  0. _d 0
      tauSaltClimRelax  =  0. _d 0
      periodicExternalForcing = .FALSE.
      externForcingPeriod     = 0.
      externForcingCycle      = 0.
      tCylIn            = 0.
      tCylOut           = 20.
C-    I/O params:
      pickupSuff        = ' '
      pickupStrictlyMatch = .TRUE.
      pChkPtFreq        = deltaT*0
      chkPtFreq         = deltaT*0
      outputTypesInclusive = .FALSE.
      pickup_read_mdsio = .TRUE.
      pickup_write_mdsio= .TRUE.
      pickup_write_immed= .FALSE.
      writePickupAtEnd  = .TRUE.
      dumpFreq          = deltaT*0
      adjDumpFreq       = deltaT*0
      diagFreq          = deltaT*0
      dumpInitAndLast   = .TRUE.
      snapshot_mdsio    = .TRUE.
      monitorFreq       = -1.
      adjMonitorFreq    = 0.
      monitorSelect     = UNSET_I
      monitor_stdio     = .TRUE.
      taveFreq          = deltaT*0
      timeave_mdsio     = .TRUE.
      tave_lastIter     = 0.5 _d 0
      readBinaryPrec    = precFloat32
      writeBinaryPrec   = precFloat32
      writeStatePrec    = precFloat64
      rwSuffixType      = 0

C--   Input files
      bathyFile       = ' '
      topoFile        = ' '
      addWwallFile    = ' '
      addSwallFile    = ' '
      hydrogSaltFile  = ' '
      hydrogThetaFile = ' '
      maskIniTemp     = .TRUE.
      maskIniSalt     = .TRUE.
      checkIniTemp    = .TRUE.
      checkIniSalt    = .TRUE.
      diffKrFile      = ' '
      viscAhDfile     = ' '
      viscAhZfile     = ' '
      viscA4Dfile     = ' '
      viscA4Zfile     = ' '
      zonalWindFile   = ' '
      meridWindFile   = ' '
      thetaClimFile   = ' '
      saltClimFile    = ' '
      EmPmRfile       = ' '
      saltFluxFile    = ' '
      surfQfile       = ' '
      surfQnetFile    = ' '
      surfQswFile     = ' '
      uVelInitFile    = ' '
      vVelInitFile    = ' '
      pSurfInitFile   = ' '
      pLoadFile       = ' '
      geoPotAnomFile  = ' '
      addMassFile     = ' '
      eddyPsiXFile    = ' '
      eddyPsiYFile    = ' '
      geothermalFile  = ' '
      lambdaThetaFile = ' '
      lambdaSaltFile  = ' '
      wghtBalanceFile = ' '
      mdsioLocalDir   = ' '
      adTapeDir       = ' '
      the_run_name    = ' '

      RETURN
      END
