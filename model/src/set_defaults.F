C $Header: /u/gcmpack/MITgcm/model/src/set_defaults.F,v 1.59 2004/03/04 04:00:12 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: SET_DEFAULTS
C     !INTERFACE:
      SUBROUTINE SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRdefault, rkFacDefault,
     I   myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SET_DEFAULTS                                   
C     | o Routine to set model "parameters"                       
C     *==========================================================*
C     | Notes:                                                    
C     | ======                                                    
C     | The present version of this routine is a place-holder.    
C     | A production version needs to handle parameters from an   
C     | external file and possibly reading in some initial field  
C     | values.                                                   
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
Cml#include "EOS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Number of this instance of INI_PARMS
      INTEGER myThid
      _RL viscArDefault
      _RL diffKrTDefault
      _RL diffKrSDefault
      _RL hFacMinDrDefault
      _RL delRDefault(Nr)
      _RS rkFacDefault

C     !LOCAL VARIABLES:
C     === Local variables ===
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

C--   Set default "physical" parameters
      DO K =1,Nr
       tRef(K) = 30.D0 - FLOAT( K )
Cml       sRef(K) = 35.D0
      ENDDO
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
      viscAhMax           = 1.D21
      viscAstrain         = 0.D3
      viscAtension        = 0.D3
      diffKhT             = 0.D3
      diffKhS             = 0.D3
      viscArDefault       = 0.D-3
      no_slip_sides       = .TRUE.
      no_slip_bottom      = .TRUE.
      diffKrTDefault      = 0.D-3
      diffKrSDefault      = 0.D-3
      viscA4              = 0.D11
      viscA4Grid          = 0.D0
      viscA4Max           = 1.D21
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
      tr1Stepping         = .FALSE.
      metricTerms         = .TRUE.
      useNHMTerms         = .FALSE.
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
      groundAtK1          = .FALSE.
      bottomDragLinear    = 0.
      bottomDragQuadratic = 0.
      usePickupBeforeC35    = .FALSE.
      debugMode             = .FALSE.
      readPickupWithTracer  = .FALSE.
      writePickupWithTracer = .FALSE.
      tempAdvScheme       = 2
      saltAdvScheme       = 2
      tracerAdvScheme     = 2
      multiDimAdvection   = .TRUE.
      useCDscheme         = .FALSE.
      useEnergyConservingCoriolis = .FALSE.
      useJamartWetPoints  = .FALSE.
      debugLevel          = debLevA

C--   Set (free)surface-related parameters
      implicitFreeSurface = .TRUE.
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
      trac_EvPrRn = UNSET_RL

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
      cg3dMaxIters       = 150
      cg3dTargetResidual = 1.D-7
      cg3dChkResFreq     = 1

C--   Time stepping parameters
      deltaT            = 0.
      nIter0            = 0
      startTime         = deltaT*float(nIter0)
      pickupSuff        = ' '
      nTimeSteps        = 0
      nEndIter          = nIter0+nTimeSteps
      endTime           = deltaT*float(nEndIter)
      forcing_In_AB     = .TRUE.
      abEps             = 0.01
      pchkPtFreq        = deltaT*0
      chkPtFreq         = deltaT*0
      dumpFreq          = deltaT*0
      adjDumpFreq       = deltaT*0
      diagFreq          = deltaT*0
      monitorFreq       = -1.
      taveFreq          = deltaT*0
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
      tauTr1ClimRelax   =  0.D0
      periodicExternalForcing = .FALSE.
      externForcingPeriod     = 0.
      externForcingCycle      = 0.

C--   Input files
      bathyFile       = ' '
      topoFile        = ' '
      hydrogSaltFile  = ' '
      hydrogThetaFile = ' '
      zonalWindFile   = ' '
      meridWindFile   = ' '
      thetaClimFile   = ' '
      saltClimFile    = ' '
      EmPmRfile       = ' '
      surfQfile       = ' '
      surfQswfile     = ' '
      uVelInitFile    = ' '
      vVelInitFile    = ' '
      pSurfInitFile   = ' '
      dQdTFile        = ' '
      ploadFile       = ' '
      mdsioLocalDir   = ' '

C
      RETURN
      END
