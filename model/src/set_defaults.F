C $Header: /u/gcmpack/MITgcm/model/src/set_defaults.F,v 1.110 2006/02/07 11:47:48 mlosch Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SET_DEFAULTS
C     !INTERFACE:
      SUBROUTINE SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRdefault, rkFacDefault,
     I   myThid )

C     !DESCRIPTION:
C     Routine to set model "parameter defaults".

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
Cml#include "EOS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Number of this instance of INI_PARMS
      INTEGER myThid
      _RL viscArDefault
      _RL diffKrTDefault
      _RL diffKrSDefault
      _RL hFacMinDrDefault
      _RL delRDefault(Nr)
      _RS rkFacDefault

C     !LOCAL VARIABLES:
C     K, I, J - Loop counters
      INTEGER K, I, J
CEOP

C--   Grid parameters
C     Vertical gridding
      rkFacDefault         = 1.D0
      horiVertRatio        = 1.D0
      Ro_SeaLevel = 0.
      DO k=1,Nr
       delRdefault(k) = 0.
      ENDDO
      DO k=1,Nr+1
       delRc(k) = UNSET_RL
      ENDDO

C     Horizontal gridding
      delXFile        = ' '
      delYFile        = ' '
      horizGridFile   = ' '
C     In cartesian coords distances are in metres
      usingCartesianGrid = .FALSE.
      DO i=1,Nx
       delX(i) = UNSET_RL
      ENDDO
      DO j=1,Ny
       delY(j) = UNSET_RL
      ENDDO
C     In spherical polar distances are in degrees
      usingSphericalPolarGrid = .FALSE.
      phiMin               = 0.0
      thetaMin             = 0.
      rSphere              = 6370. * 1.D3
C     General curvilinear coordinate system
      usingCurvilinearGrid = .FALSE.
C     General cylindrical coordinate system
      usingCylindricalGrid = .FALSE.
C     Coriolis map:
      useConstantF  = .FALSE.
      useBetaPlaneF = .FALSE.
      useSphereF    = .FALSE.


C--   Set default "physical" parameters
      nh_Am2              = 1.D0
      gravity             = 9.81D0
      rhoNil              = 999.8D0
C-- jmc : the default is to set rhoConstFresh to rhoConst (=rhoNil by default)
C         (so that the default produces same results as before)
c     rhoConstFresh       = 999.8D0
      f0                  = 1.D-4
      beta                = 1.D-11
C-    Always use 1 day in the past but should be 86164 (=86400*365.25/366.25)
      rotationPeriod      = 86400. _d 0
      viscAh              = 0.D3
      viscAhGrid          = 0.D0
      viscAhGridMin       = 0.D0
      viscAhGridMax       = 1.D21
      viscAhMax           = 1.D21
      viscAhReMax         = 0.D0
      viscC2leith         = 0.D0
      viscC2leithD        = 0.D0
      viscC2smag          = 0.D0
      diffKhT             = 0.D3
      diffKhS             = 0.D3
      viscArDefault       = 0.D-3
      no_slip_sides       = .TRUE.
      no_slip_bottom      = .TRUE.
      sideDragFactor      = 2. _d 0
      bottomDragLinear    = 0.
      bottomDragQuadratic = 0.
      diffKrTDefault      = 0.D-3
      diffKrSDefault      = 0.D-3
      diffKrBL79surf      = 0.D0
      diffKrBL79deep      = 0.D0
      diffKrBL79scl       = 200.D0
      diffKrBL79Ho        = -2000.D0
      viscA4              = 0.D11
      viscA4Grid          = 0.D0
      viscA4GridMax       = 1.D21
      viscA4GridMin       = 0.D0
      viscA4Max           = 1.D21
      viscA4ReMax         = 0.D0
      viscC4leith         = 0.D0
      viscC4leithD        = 0.D0
      viscC4smag          = 0.D0
      diffK4T             = 0.D11
      diffK4S             = 0.D11
      cosPower            = 0.
      HeatCapacity_Cp     = 3994.D0
Cml      tAlpha              = 2.D-4
Cml      sBeta               = 7.4D-4
      eosType             = 'LINEAR'
      buoyancyRelation    = 'OCEANIC'
      hFacMin             = 1.D0
      hFacMinDrDefault    = 0.D0
      implicitIntGravWave = .FALSE.
      staggerTimeStep     = .FALSE.
      momViscosity        = .TRUE.
      momAdvection        = .TRUE.
      momForcing          = .TRUE.
      useCoriolis         = .TRUE.
      momPressureForcing  = .TRUE.
      momStepping         = .TRUE.
      vectorInvariantMomentum = .FALSE.
      tempStepping        = .TRUE.
      tempAdvection       = .TRUE.
      tempForcing         = .TRUE.
      saltStepping        = .TRUE.
      saltAdvection       = .TRUE.
      saltForcing         = .TRUE.
      metricTerms         = .TRUE.
      useNHMTerms         = .FALSE.
      useFullLeith        = .FALSE.
      useAreaViscLength   = .FALSE.
      useStrainTensionVisc = .FALSE.
      implicitDiffusion   = .FALSE.
      implicitViscosity   = .FALSE.
      momImplVertAdv      = .FALSE.
      tempImplVertAdv     = .FALSE.
      saltImplVertAdv     = .FALSE.
      nonHydrostatic      = .FALSE.
      quasiHydrostatic    = .FALSE.
      globalFiles         = .FALSE.
      useSingleCpuIO      = .FALSE.
      allowFreezing       = .FALSE.
      useOldFreezing      = .FALSE.
      ivdc_kappa          = 0.D0
      usePickupBeforeC35    = .FALSE.
      usePickupBeforeC54    = .FALSE.
      debugMode             = .FALSE.
      tempAdvScheme       = 2
      saltAdvScheme       = 2
      multiDimAdvection   = .TRUE.
      useCDscheme         = .FALSE.
      useEnergyConservingCoriolis = .FALSE.
      useJamartWetPoints  = .FALSE.
      useJamartMomAdv     = .FALSE.
      SadournyCoriolis    = .FALSE.
      upwindVorticity     = .FALSE.
      highOrderVorticity  = .FALSE.
      useAbsVorticity     = .FALSE.
      upwindShear         = .FALSE.
      selectKEscheme      = 0
      debugLevel          = debLevA
      inAdMode            = .FALSE.
      inAdExact           = .TRUE.

C--   Set (free)surface-related parameters
      implicitFreeSurface = .FALSE.
      rigidLid            = .FALSE.
      implicSurfPress     = 1.D0
      implicDiv2DFlow     = 1.D0
      exactConserv        = .FALSE.
      uniformLin_PhiSurf  = .TRUE.
      nonlinFreeSurf      = 0
      hFacInf             = 0.2 _d 0
      hFacSup             = 2.0 _d 0
      select_rStar        = 0
      useRealFreshWaterFlux = .FALSE.
      temp_EvPrRn = UNSET_RL
      salt_EvPrRn = 0.

C--   Atmospheric physical parameters (e.g.: EOS)
      celsius2K = 273.16 _d 0
      atm_Po =  1. _d 5
      atm_Cp = 1004. _d 0
      atm_Rd = UNSET_RL
      atm_kappa = 2. _d 0 / 7. _d 0
      atm_Rq = 0. _d 0
      integr_GeoPot = 2
      selectFindRoSurf = 0

C--   Elliptic solver parameters
      cg2dMaxIters       = 150
      cg2dTargetResidual = 1.D-7
      cg2dTargetResWunit = -1.
      cg2dChkResFreq     = 1
      cg2dpcOffDFac      = 0.51D0
      cg2dPreCondFreq    = 1
      cg3dMaxIters       = 150
      cg3dTargetResidual = 1.D-7
      cg3dChkResFreq     = 1

C--   Time stepping parameters
      deltaT            = 0. _d 0
      deltaTmom         = 0. _d 0
      deltaTfreesurf    = 0. _d 0
      DO k=1,Nr
        dTtracerLev(k)  = 0. _d 0
      ENDDO
      baseTime          = 0. _d 0
      nIter0            = 0
      startTime         = deltaT*float(nIter0)
      pickupSuff        = ' '
      nTimeSteps        = 0
      nEndIter          = nIter0+nTimeSteps
      endTime           = deltaT*float(nEndIter)
      forcing_In_AB     = .TRUE.
      doAB_onGtGs       = .TRUE.
      abEps             = 0.01
#ifdef ALLOW_ADAMSBASHFORTH_3
      alph_AB           = 0.5 _d 0
      beta_AB           = 5. _d 0 / 12. _d 0
      startFromPickupAB2= .FALSE.
#else
      alph_AB           = UNSET_RL
      beta_AB           = UNSET_RL
      startFromPickupAB2= .TRUE.
#endif
      pchkPtFreq        = deltaT*0
      chkPtFreq         = deltaT*0
      outputTypesInclusive = .FALSE.
      pickup_read_mdsio = .TRUE.
      pickup_write_mdsio= .TRUE.
      pickup_write_immed= .FALSE.
      dumpFreq          = deltaT*0
      adjDumpFreq       = deltaT*0
      diagFreq          = deltaT*0
      dumpInitAndLast   = .TRUE.
      snapshot_mdsio    = .TRUE.
      monitorFreq       = -1.
      adjMonitorFreq    = 0.
      monitor_stdio     = .TRUE.
      taveFreq          = deltaT*0
      timeave_mdsio     = .TRUE.
      tave_lastIter     = 0.5 _d 0
      writeStatePrec    = precFloat64
      writeBinaryPrec   = precFloat32
      readBinaryPrec    = precFloat32
      nCheckLev         = 1
      checkPtSuff(1)    = 'ckptA'
      checkPtSuff(2)    = 'ckptB'
      cAdjFreq          =  0.D0
      tauCD             =  0.D0
      tauThetaClimRelax =  0.D0
      tauSaltClimRelax  =  0.D0
      tauThetaClimRelax3Dim =  0.D0
      tauSaltClimRelax3Dim  =  0.D0
      tauTr1ClimRelax   =  0.D0
      periodicExternalForcing = .FALSE.
      externForcingPeriod     = 0.
      externForcingCycle      = 0.
      tCylIn             = 0.
      tCylOut            = 20.
      calendarDumps     = .FALSE.

C--   Input files
      bathyFile       = ' '
      topoFile        = ' '
      shelfIceFile    = ' '
      hydrogSaltFile  = ' '
      hydrogThetaFile = ' '
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
      dQdTFile        = ' '
      ploadFile       = ' '
      eddyTauxFile    = ' '
      eddyTauyFile    = ' '
      lambdaThetaFile = ' '
      lambdaSaltFile  = ' '
      mdsioLocalDir   = ' '

C
      RETURN
      END
