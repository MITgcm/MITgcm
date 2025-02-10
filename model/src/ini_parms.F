#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_EXCH2
# include "W2_OPTIONS.h"
#endif /* ALLOW_EXCH2 */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: INI_PARMS
C !INTERFACE:
      SUBROUTINE INI_PARMS( myThid )

C !DESCRIPTION:
C     Routine to load model "parameters" from parameter file "data"

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */
#include "EOS.h"
C- need GRID.h to save, in rF(1), half retired param Ro_SeaLevel value:
#include "GRID.h"
#include "SET_GRID.h"

C !INPUT/OUTPUT PARAMETERS:
C   myThid :: Number of this instance of INI_PARMS
      INTEGER myThid

C !LOCAL VARIABLES:
C   dxSpacing, dySpacing :: Default spacing in X and Y.
C                        :: Units are that of coordinate system
C                        :: i.e. cartesian => metres ; s. polar => degrees
C   deltaTtracer  :: Timestep for tracer equations ( s )
C   forcing_In_AB :: flag to put all forcings (Temp,Salt,Tracers,Momentum)
C                 :: contribution in (or out of) Adams-Bashforth time stepping.
C   goptCount  :: Used to count the nuber of grid options (only one is allowed!)
C   msgBuf     :: Informational/error message buffer
C   errIO      :: IO error flag
C   errCount   :: error counter (inconsitent params and other errors)
C   iUnit      :: Work variable for IO unit number
C   k, i, j    :: Loop counters
C   xxxDefault :: Default value for variable xxx
      _RL  dxSpacing
      _RL  dySpacing
      _RL deltaTtracer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL forcing_In_AB
      INTEGER goptCount
      INTEGER gridNx, gridNy
      INTEGER k, i, j, iUnit
      INTEGER errIO, errCount
C   Default values for variables which have vertical coordinate system
C   dependency.
      _RL viscArDefault
      _RL diffKrTDefault
      _RL diffKrSDefault
      _RL hFacMinDrDefault
      _RL delRDefault(Nr)
C-- Variables used to select between different coordinate systems:
C   zCoordInputData :: The vertical coordinate system in the rest of the  model
C   pCoordInputData :: is written in terms of r. In the model "data" file,
C   rCoordInputData :: input params can be in terms of z, p or r.
C   coordsSet       :: e.g. delZ or delP or delR for the vertical grid spacing.
C                   :: The following rules apply:
C                   :: All parameters must use the same vertical coordinate
C                   :: system.  e.g. delZ and viscAz is legal
C                   ::           but delZ and viscAr is an error.
C                   :: Similarly specifying delZ and delP is an error.
C                   :: zCoord..., pCoord..., rCoord... are used to flag when
C                   :: z, p or r are used.
C                   :: coordsSet counts how many vertical coordinates have been
C                   :: used to specify variables. coordsSet > 1 is an error.
C   vertSetCount    :: to count number of vertical array elements which are set.
C   specifiedDiffKrT :: internal flag, true if any diffK[r,z,p,Nr]T is specified
C   specifiedDiffKrS :: internal flag, true if any diffK[r,z,p,Nr]S is specified

      LOGICAL zCoordInputData
      LOGICAL pCoordInputData
      LOGICAL rCoordInputData
      INTEGER coordsSet
      INTEGER vertSetCount
      LOGICAL specifiedDiffKrT, specifiedDiffKrS

C-- Variables which have vertical coordinate system dependency.
C   delZ    :: Vertical grid spacing ( m  ).
C   delP    :: Vertical grid spacing ( Pa ).
C   viscAz  :: Eddy viscosity coeff. for mixing of momentum vertically (m^2/s)
C   viscAp  :: Eddy viscosity coeff. for mixing of momentum vertically (Pa^2/s)
C   diffKzT :: Laplacian diffusion coeff. for mixing of heat vertically (m^2/s)
C   diffKpT :: Laplacian diffusion coeff. for mixing of heat vertically (Pa^2/s)
C   diffKzS :: Laplacian diffusion coeff. for mixing of salt vertically (m^2/s)
C   diffKpS :: Laplacian diffusion coeff. for mixing of salt vertically (Pa^2/s)
      _RL delZ(Nr)
      _RL delP(Nr)
      _RL viscAz
      _RL viscAp
      _RL viscAr
      _RL diffKzT
      _RL diffKpT
      _RL diffKrT
      _RL diffKzS
      _RL diffKpS
      _RL diffKrS

C-- Retired main data file parameters. Kept here to trap use of old data files.
C   nRetired :: Counter used to trap gracefully namelists containing "retired"
C            :: parameters. These are parameters that are either no-longer used
C            :: or that have moved to a different input file and/or namelist.
C Namelist PARM01:
C   useConstantF  :: Coriolis coeff set to f0     (replaced by selectCoriMap=0)
C   useBetaPlaneF :: Coriolis coeff = f0 + beta.y (replaced by selectCoriMap=1)
C   useSphereF    :: Coriolis = 2.omega.sin(phi)  (replaced by selectCoriMap=2)
C   useJamartWetPoints :: for backward compat. (replaced by selectCoriScheme=1)
C                      :: Use wet-point method for Coriolis (Jamart & Ozer 1986)
C   SadournyCoriolis :: for backward compat. (replaced by selectVortScheme=2)
C   use3dCoriolis :: for backward compat. (=F,T same as select3dCoriScheme=0,1)
C   metricTerms   :: for backward compat. (=F,T same as selectMetricTerms=0,1)
C   tracerAdvScheme :: tracer advection scheme (old passive tracer code)
C   trac_EvPrRn :: tracer conc. in Rain & Evap (old passive tracer code)
C   saltDiffusion :: diffusion of salinity    on/off (flag not used)
C   tempDiffusion :: diffusion of temperature on/off (flag not used)
C   zonal_filt_lat :: Moved to package "zonal_filt"
C   gravitySign  :: direction of gravity relative to R direction
C                :: (removed from namelist and set according to z/p coordinate)
C   viscAstrain  :: replaced by standard viscosity coeff & useStrainTensionVisc
C   viscAtension :: replaced by standard viscosity coeff & useStrainTensionVisc
C   useAnisotropicViscAgridMax :: Changed to be default behavior.  Can
C                   use old method by setting useAreaViscLength=.true.
C   usePickupBeforeC35 :: to restart from old-pickup files (generated with code
C                 from before checkpoint-35, Feb 08, 2001): disabled (Jan 2007)
C   debugMode    :: to print debug msg. now read from parameter file eedata
C   allowInteriorFreezing :: Allow water at depth to freeze
C                            and rise to the surface (replaced by pkg/frazil)
C   useOldFreezing :: use the old version (before checkpoint52a_pre, 2003-11-12)
C   balanceEmPmR   :: for backward compat. (replaced by selectBalanceEmPmR=1),
C                  :: substract global mean of EmPmR at every time step
C   writeStatePrec :: Precision used for writing model state (not used)
C Namelist PARM02:
C   cg2dChkResFreq :: Frequency with which to check 2-D con. grad solver
C                       residual (was never coded)
C   cg3dChkResFreq :: Frequency with which to check 3-D con. grad solver
C                       residual (was never coded)
C Namelist PARM03:
C   tauThetaClimRelax3Dim :: replaced by pkg/rbcs (3.D Relaxation B.Cs)
C   tauSaltClimRelax3Dim  :: replaced by pkg/rbcs (3.D Relaxation B.Cs)
C   calendarDumps  :: moved to package "cal" (calendar)
C Namelist PARM04:
C   Ro_SeaLevel :: origin of the vertical R-coords axis ;
C               :: replaced by top_Pres or seaLev_Z setting
C   groundAtK1 :: put the surface(k=1) at the ground (replaced by usingPCoords)
C   rkFac      :: removed from namelist ; replaced by -rkSign
C   thetaMin   :: unfortunate variable name ; replaced by xgOrigin
C   phiMin     :: unfortunate variable name ; replaced by ygOrigin
C Namelist PARM05:
C   shelfIceFile :: File containing the topography of the shelfice draught
C                   (replaced by SHELFICEtopoFile in SHELFICE.h)
C   dQdTfile     :: File containing thermal relaxation coefficient
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      INTEGER nRetired
      LOGICAL useConstantF, useBetaPlaneF, useSphereF
      LOGICAL useJamartWetPoints, useEnergyConservingCoriolis
      LOGICAL SadournyCoriolis
      LOGICAL use3dCoriolis, metricTerms
      LOGICAL tempDiffusion, saltDiffusion
      INTEGER tracerAdvScheme
      _RL trac_EvPrRn
      _RL zonal_filt_lat
c     _RL gravitySign
      _RL viscAstrain, viscAtension
      LOGICAL useAnisotropicViscAgridMax
      LOGICAL usePickupBeforeC35
      LOGICAL saveDebugMode
      LOGICAL allowInteriorFreezing, useOldFreezing
      LOGICAL balanceEmPmR
      INTEGER writeStatePrec
C-
      INTEGER cg2dChkResFreq, cg3dChkResFreq
C-
      _RL tauThetaClimRelax3Dim, tauSaltClimRelax3Dim
      LOGICAL calendarDumps
C-
      LOGICAL groundAtK1
      _RL Ro_SeaLevel
      _RL rkFac
      _RL thetaMin, phiMin
C-
      CHARACTER*(MAX_LEN_FNAM) shelfIceFile
      CHARACTER*(MAX_LEN_FNAM) dQdTfile

C--   Continuous equation parameters
      NAMELIST /PARM01/
     & gravitySign, nh_Am2,
     & gravity, gBaro, gravityFile, rhonil, tAlpha, sBeta,
     & selectCoriMap, f0, beta, fPrime, omega, rotationPeriod,
     & viscAh, viscAhW, viscAhMax,
     & viscAhGrid, viscAhGridMax, viscAhGridMin,
     & viscC2leith, viscC4leith, smag3D_coeff, useSmag3D,
     & useFullLeith, useAnisotropicViscAgridMax, useStrainTensionVisc,
     & useAreaViscLength, viscC2leithD, viscC4leithD, viscC2LeithQG,
     & viscC2smag, viscC4smag, viscAhD, viscAhZ, viscA4D, viscA4Z,
     & viscA4, viscA4W,
     & viscA4Max, viscA4Grid, viscA4GridMax, viscA4GridMin,
     & viscA4ReMax, viscAhReMax,
     & cosPower, viscAstrain, viscAtension,
     & viscAz, diffKzT, diffKzS, viscAp, diffKpT, diffKpS,
     & viscAr, diffKrT, diffKrS, viscArNr, diffKrNrT, diffKrNrS,
     & diffKhT, diffK4T, diffKhS, diffK4S, smag3D_diffCoeff,
     & diffKr4T, diffKr4S, BL79LatVary,
     & diffKrBL79surf, diffKrBL79deep, diffKrBL79scl, diffKrBL79Ho,
     & diffKrBLEQsurf, diffKrBLEQdeep, diffKrBLEQscl, diffKrBLEQHo,
     & surf_pRef, tRef, sRef, tRefFile, sRefFile, rhoRefFile,
     & eosType, selectP_inEOS_Zc, integr_GeoPot, selectFindRoSurf,
     & HeatCapacity_Cp, celsius2K, atm_Cp, atm_Rd, atm_Rq, atm_Po,
     & no_slip_sides, sideDragFactor, no_slip_bottom, bottomVisc_pCell,
     & bottomDragLinear, bottomDragQuadratic,  zRoughBot,
     & selectBotDragQuadr,
     & momPressureForcing, momForcing, momTidalForcing,
     & momViscosity, momAdvection, vectorInvariantMomentum,
     & useConstantF, useBetaPlaneF, useSphereF, useCoriolis,
     & use3dCoriolis, select3dCoriScheme, selectCoriScheme,
     & useCDscheme, useJamartWetPoints, useEnergyConservingCoriolis,
     & useAbsVorticity, selectVortScheme, SadournyCoriolis,
     & useJamartMomAdv, upwindVorticity, highOrderVorticity,
     & upwindShear, selectKEscheme,
     & selectMetricTerms, metricTerms, useNHMTerms, addFrictionHeating,
     & tempDiffusion, tempAdvection, tempForcing, temp_stayPositive,
     & saltDiffusion, saltAdvection, saltForcing, salt_stayPositive,
     & implicSurfPress, implicDiv2DFlow, implicitNHPress,
     & implicitFreeSurface, rigidLid, freeSurfFac,
     & hFacMin, hFacMinDz, hFacMinDp, hFacMinDr,
     & exactConserv, linFSConserveTr, uniformLin_PhiSurf,
     & nonlinFreeSurf, hFacInf, hFacSup, select_rStar,
     & nonHydrostatic, selectNHfreeSurf, quasiHydrostatic,
     & implicitIntGravWave, staggerTimeStep, doResetHFactors,
     & tempStepping, saltStepping, momStepping,
     & implicitDiffusion, implicitViscosity, selectImplicitDrag,
     & tempImplVertAdv, saltImplVertAdv, momImplVertAdv,
     & rhoConst, thetaConst, rhoConstFresh, buoyancyRelation,
     & allowFreezing, allowInteriorFreezing, useOldFreezing, ivdc_kappa,
     & hMixCriteria, dRhoSmall, hMixSmooth,
     & tempAdvScheme, tempVertAdvScheme, tracerAdvScheme,
     & saltAdvScheme, saltVertAdvScheme, multiDimAdvection,
     & selectAddFluid, useRealFreshWaterFlux, convertFW2Salt,
     & temp_EvPrRn, salt_EvPrRn, trac_EvPrRn,
     & temp_addMass, salt_addMass, zonal_filt_lat,
     & smoothAbsFuncRange, sIceLoadFac,
     & selectBalanceEmPmR, balanceEmPmR, balanceQnet, balancePrintMean,
     & balanceThetaClimRelax, balanceSaltClimRelax,
     & readBinaryPrec, writeBinaryPrec, writeStatePrec, globalFiles,
     & useSingleCpuIO, useSingleCpuInput, usePickupBeforeC54,
     & usePickupBeforeC35, debugMode, debugLevel, plotLevel

C--   Elliptic solver parameters
      NAMELIST /PARM02/
     & cg2dMaxIters, cg2dMinItersNSA, cg2dChkResFreq, cg2dUseMinResSol,
     & cg2dTargetResidual, cg2dTargetResWunit,
     & cg2dpcOffDFac, cg2dPreCondFreq,
     & cg3dMaxIters, cg3dChkResFreq,
     & cg3dTargetResidual, cg3dTargetResWunit,
     & useNSACGSolver, useSRCGSolver, printResidualFreq

C--   Time stepping parammeters
      NAMELIST /PARM03/
     & nIter0, nTimeSteps, nTimeSteps_l2, nEndIter,
     & baseTime, startTime, endTime,
     & deltaT, deltaTClock, deltaTMom,
     & deltaTtracer, dTtracerLev, deltaTFreeSurf,
     & forcing_In_AB, momForcingOutAB, tracForcingOutAB,
     & momDissip_In_AB, doAB_onGtGs,
     & abEps, alph_AB, beta_AB, startFromPickupAB2, applyExchUV_early,
     & tauCD, rCD, epsAB_CD, cAdjFreq,
     & chkPtFreq, pChkPtFreq, pickupSuff, pickupStrictlyMatch,
     & writePickupAtEnd,
     & dumpFreq, dumpInitAndLast, adjDumpFreq, taveFreq, tave_lastIter,
     & diagFreq, monitorFreq, adjMonitorFreq, monitorSelect,
     & outputTypesInclusive, rwSuffixType,
     & tauThetaClimRelax, tauSaltClimRelax, latBandClimRelax,
     & tauThetaClimRelax3Dim, tauSaltClimRelax3Dim,
     & periodicExternalForcing, externForcingPeriod, externForcingCycle,
     & calendarDumps

C--   Gridding parameters
      NAMELIST /PARM04/
     & usingCartesianGrid, usingCylindricalGrid,
     & usingSphericalPolarGrid, usingCurvilinearGrid,
     & xgOrigin, ygOrigin, dxSpacing, dySpacing,
     & delX, delY, delXFile, delYFile, horizGridFile,
     & phiEuler, thetaEuler, psiEuler,
     & rSphere, radius_fromHorizGrid, deepAtmosphere, seaLev_Z,
     & top_Pres, delZ, delP, delR, delRc, delRFile, delRcFile,
     & useMin4hFacEdges, interViscAr_pCell, interDiffKr_pCell,
     & pCellMix_select, pCellMix_maxFac, pCellMix_delR,
     & pCellMix_viscAr, pCellMix_diffKr,
     & selectSigmaCoord, rSigmaBnd, hybSigmFile,
     & Ro_SeaLevel, rkFac, groundAtK1, thetaMin, phiMin

C--   Input files
      NAMELIST /PARM05/
     & bathyFile, topoFile, addWwallFile, addSwallFile, shelfIceFile,
     & diffKrFile, viscAhDfile, viscAhZfile, viscA4Dfile, viscA4Zfile,
     & hydrogThetaFile, hydrogSaltFile,
     & maskIniTemp, maskIniSalt, checkIniTemp, checkIniSalt,
     & zonalWindFile, meridWindFile,
     & thetaClimFile, saltClimFile,
     & surfQfile, surfQnetFile, surfQswFile, EmPmRfile, saltFluxFile,
     & uVelInitFile, vVelInitFile, pSurfInitFile,
     & dQdTFile, ploadFile, geoPotAnomFile, addMassFile,
     & tCylIn, tCylOut,
     & eddyPsiXFile, eddyPsiYFile, geothermalFile,
     & lambdaThetaFile, lambdaSaltFile, wghtBalanceFile,
     & mdsioLocalDir, adTapeDir,
     & the_run_name
CEOP

#ifdef ALLOW_EXCH2
      gridNx = exch2_mydNx(1)
      gridNy = exch2_mydNy(1)
#else /* ALLOW_EXCH2 */
      gridNx = Nx
      gridNy = Ny
#endif /* ALLOW_EXCH2 */

      _BEGIN_MASTER(myThid)

C     Defaults values for input parameters
      CALL SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRDefault,
     I   myThid )

      useJamartWetPoints = .FALSE.
      useEnergyConservingCoriolis = .FALSE.
      SadournyCoriolis = .FALSE.
      use3dCoriolis   = .TRUE.
      metricTerms     = .TRUE.
      balanceEmPmR    = .FALSE.

C--   Initialise "which vertical coordinate system used" flags.
      zCoordInputData = .FALSE.
      pCoordInputData = .FALSE.
      rCoordInputData = .FALSE.
      coordsSet       = 0

C--   Initialise retired parameters to unlikely value
      nRetired = 0
      useConstantF    = .FALSE.
      useBetaPlaneF   = .FALSE.
      useSphereF      = .TRUE.
      tempDiffusion   = .TRUE.
      saltDiffusion   = .TRUE.
      tracerAdvScheme = UNSET_I
      trac_EvPrRn     = UNSET_RL
      zonal_filt_lat  = UNSET_RL
      gravitySign     = UNSET_RL
      viscAstrain     = UNSET_RL
      viscAtension    = UNSET_RL
      useAnisotropicViscAgridMax=.TRUE.
      usePickupBeforeC35    = .FALSE.
      saveDebugMode   = debugMode
      allowInteriorFreezing = .FALSE.
      useOldFreezing  = .FALSE.
      writeStatePrec  = UNSET_I
      cg2dChkResFreq  = UNSET_I
      cg3dChkResFreq  = UNSET_I
      tauThetaClimRelax3Dim = UNSET_RL
      tauSaltClimRelax3Dim  = UNSET_RL
      calendarDumps   = .FALSE.
      Ro_SeaLevel     = UNSET_RL
      rkFac           = UNSET_RL
      groundAtK1      = .FALSE.
      thetaMin        = UNSET_RL
      phiMin          = UNSET_RL
      shelfIceFile    = ' '
      dQdTFile        = ' '

C--   Open the parameter file
      WRITE(msgBuf,'(A)')
     &     ' INI_PARMS: opening model parameter file "data"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data', 'INI_PARMS',
     O                          iUnit, myThid )

C--   Read settings from iUnit (= a copy of model parameter file "data").
      errIO = 0
      errCount = 0

C--   Set default "physical" parameters
      viscAhW  = UNSET_RL
      viscA4W  = UNSET_RL
      viscAhD  = UNSET_RL
      viscAhZ  = UNSET_RL
      viscA4D  = UNSET_RL
      viscA4Z  = UNSET_RL
      viscAz   = UNSET_RL
      viscAp   = UNSET_RL
      viscAr   = UNSET_RL
      diffKzT  = UNSET_RL
      diffKpT  = UNSET_RL
      diffKrT  = UNSET_RL
      diffKzS  = UNSET_RL
      diffKpS  = UNSET_RL
      diffKrS  = UNSET_RL
      hFacMinDr         = UNSET_RL
      hFacMinDz         = UNSET_RL
      hFacMinDp         = UNSET_RL
      tAlpha            = UNSET_RL
      sBeta             = UNSET_RL
      implicitNHPress   = UNSET_RL
      tempVertAdvScheme = 0
      saltVertAdvScheme = 0
      plotLevel         = UNSET_I
C--   z,p,r coord input switching.
      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM01) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM01'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM01 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

C-    set the type of vertical coordinate and type of fluid
C     according to buoyancyRelation
      usingPCoords    = .FALSE.
      usingZCoords    = .FALSE.
      fluidIsAir      = .FALSE.
      fluidIsWater    = .FALSE.
      IF ( buoyancyRelation.EQ.'ATMOSPHERIC' ) THEN
        usingPCoords = .TRUE.
        fluidIsAir   = .TRUE.
      ELSEIF ( buoyancyRelation.EQ.'OCEANICP') THEN
        usingPCoords  = .TRUE.
        fluidIsWater  = .TRUE.
      ELSEIF ( buoyancyRelation.EQ.'OCEANIC' ) THEN
        usingZCoords  = .TRUE.
        fluidIsWater  = .TRUE.
      ELSE
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS:',
     &      ' Bad value of buoyancyRelation '
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( .NOT.rigidLid .AND.
     &     .NOT.implicitFreeSurface ) THEN
C-     No barotropic solver selected => use implicitFreeSurface as default
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: No request for barotropic solver'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: => Use implicitFreeSurface as default'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
        implicitFreeSurface = .TRUE.
      ENDIF
      IF ( implicitFreeSurface ) freeSurfFac = 1. _d 0
      IF ( rigidLid            ) freeSurfFac = 0. _d 0
      IF ( gBaro .EQ. UNSET_RL ) gBaro=gravity
      IF ( rhoConst .EQ. UNSET_RL ) rhoConst=rhoNil
      IF ( rhoConstFresh .EQ. UNSET_RL ) rhoConstFresh=rhoConst
      IF ( implicitNHPress.EQ.UNSET_RL )
     &     implicitNHPress = implicSurfPress
      IF ( omega .EQ. UNSET_RL ) THEN
        omega = 0. _d 0
        IF ( rotationPeriod .NE. 0. _d 0 )
     &  omega = 2. _d 0 * PI / rotationPeriod
      ELSEIF ( omega .EQ. 0. _d 0 ) THEN
        rotationPeriod = 0. _d 0
      ELSE
        rotationPeriod = 2. _d 0 * PI / omega
      ENDIF
      IF ( atm_Rd .EQ. UNSET_RL ) THEN
        atm_Rd = atm_Cp * atm_kappa
      ELSE
        atm_kappa = atm_Rd / atm_Cp
      ENDIF
C--   Non-hydrostatic/quasi-hydrostatic
      IF (nonHydrostatic.AND.quasiHydrostatic) THEN
       WRITE(msgBuf,'(A)')
     &   'Illegal: both nonHydrostatic = quasiHydrostatic = TRUE'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
C--  Advection and Forcing for Temp and salt
      IF (tempVertAdvScheme.EQ.0) tempVertAdvScheme = tempAdvScheme
      IF (saltVertAdvScheme.EQ.0) saltVertAdvScheme = saltAdvScheme
C--  horizontal viscosity (acting on Divergence or Vorticity)
      IF ( viscAhD .EQ. UNSET_RL ) viscAhD = viscAh
      IF ( viscAhZ .EQ. UNSET_RL ) viscAhZ = viscAh
      IF ( viscA4D .EQ. UNSET_RL ) viscA4D = viscA4
      IF ( viscA4Z .EQ. UNSET_RL ) viscA4Z = viscA4
C--  horizontal viscosity for vertical momentum
      IF ( viscAhW .EQ. UNSET_RL ) viscAhW = viscAhD
      IF ( viscA4W .EQ. UNSET_RL ) viscA4W = viscA4D
C--   z,p,r coord input switching.
      IF ( viscAz .NE. UNSET_RL ) zCoordInputData = .TRUE.
      IF ( viscAp .NE. UNSET_RL ) pCoordInputData = .TRUE.
      IF ( viscAr .NE. UNSET_RL ) rCoordInputData = .TRUE.
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscAz
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscAp
      vertSetCount = 0
      DO k=1,Nr
        IF ( viscArNr(k).NE.UNSET_RL ) vertSetCount = vertSetCount + 1
      ENDDO
      IF ( vertSetCount.GT.0 .AND. vertSetCount.LT.Nr ) THEN
        WRITE(msgBuf,'(A,2(I5,A))') 'S/R INI_PARMS: Partial setting (',
     &        vertSetCount, ' /', Nr, ') of viscArNr is not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( viscAr .EQ. UNSET_RL ) THEN
        viscAr = viscArDefault
      ELSEIF ( vertSetCount.GT.0 ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: Cannot set both ',
     &     'viscArNr and viscAr (or Ap,Az) in param file data'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( vertSetCount.EQ.0 ) THEN
        DO k=1,Nr
          viscArNr(k) = viscAr
        ENDDO
      ENDIF
#ifdef ALLOW_MOM_COMMON
C-    set default scheme for quadratic bottom-drag
      IF ( selectBotDragQuadr.EQ.-1 .AND. bottomDragQuadratic.NE.0. )
     &  selectBotDragQuadr = 0
      IF ( selectBotDragQuadr.EQ.-1 .AND. zRoughBot.NE.0. )
     &  selectBotDragQuadr = 0
#endif /* ALLOW_MOM_COMMON */

      IF ( smag3D_diffCoeff.NE.zeroRL .AND. .NOT.useSmag3D ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &    'will not use "smag3D_diffCoeff" without useSmag3D'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &    '==> reset "smag3D_diffCoeff" to zero'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       smag3D_diffCoeff = zeroRL
      ENDIF
      IF ( diffKzT .NE. UNSET_RL ) zCoordInputData  = .TRUE.
      IF ( diffKpT .NE. UNSET_RL ) pCoordInputData  = .TRUE.
      IF ( diffKrT .NE. UNSET_RL ) rCoordInputData  = .TRUE.
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKzT
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKpT
      vertSetCount = 0
      DO k=1,Nr
        IF ( diffKrNrT(k).NE.UNSET_RL ) vertSetCount = vertSetCount + 1
      ENDDO
      IF ( vertSetCount.GT.0 .AND. vertSetCount.LT.Nr ) THEN
        WRITE(msgBuf,'(A,2(I5,A))') 'S/R INI_PARMS: Partial setting (',
     &        vertSetCount, ' /', Nr, ') of diffKrNrT is not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      specifiedDiffKrT = vertSetCount.EQ.Nr
      IF ( diffKrT .EQ. UNSET_RL ) THEN
        diffKrT = diffKrTDefault
      ELSEIF ( vertSetCount.GT.0 ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: Cannot set both ',
     &     'diffKrNrT and diffKrT (or Kp,Kz) in param file data'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ELSE
        specifiedDiffKrT = .TRUE.
      ENDIF
      IF ( vertSetCount.EQ.0 ) THEN
        DO k=1,Nr
          diffKrNrT(k) = diffKrT
        ENDDO
      ENDIF

      IF ( diffKzS .NE. UNSET_RL ) zCoordInputData  = .TRUE.
      IF ( diffKpS .NE. UNSET_RL ) pCoordInputData  = .TRUE.
      IF ( diffKrS .NE. UNSET_RL ) rCoordInputData  = .TRUE.
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKzS
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKpS
      vertSetCount = 0
      DO k=1,Nr
        IF ( diffKrNrS(k).NE.UNSET_RL ) vertSetCount = vertSetCount + 1
      ENDDO
      IF ( vertSetCount.GT.0 .AND. vertSetCount.LT.Nr ) THEN
        WRITE(msgBuf,'(A,2(I5,A))') 'S/R INI_PARMS: Partial setting (',
     &        vertSetCount, ' /', Nr, ') of diffKrNrS is not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( vertSetCount.EQ.Nr ) THEN
        specifiedDiffKrS = .TRUE.
        IF ( diffKrS.NE.UNSET_RL ) THEN
         WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: Cannot set both ',
     &     'diffKrNrS and diffKrS (or Kp,Kz) in param file data'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
      ELSEIF ( diffKrS.NE.UNSET_RL ) THEN
        specifiedDiffKrS = .TRUE.
        DO k=1,Nr
          diffKrNrS(k) = diffKrS
        ENDDO
      ELSE
        specifiedDiffKrS = .FALSE.
        diffKrS = diffKrSDefault
C-    use temp diffusivity as default salt diffusivity:
        DO k=1,Nr
          diffKrNrS(k) = diffKrNrT(k)
        ENDDO
      ENDIF

      IF (diffKrBLEQsurf .EQ. UNSET_RL) diffKrBLEQsurf = diffKrBL79surf
      IF (diffKrBLEQdeep .EQ. UNSET_RL) diffKrBLEQdeep = diffKrBL79deep
      IF (diffKrBLEQscl  .EQ. UNSET_RL) diffKrBLEQscl  = diffKrBL79scl
      IF (diffKrBLEQHo   .EQ. UNSET_RL) diffKrBLEQHo   = diffKrBL79Ho

      IF ( hFacMinDz .NE. UNSET_RL ) zCoordInputData = .TRUE.
      IF ( hFacMinDp .NE. UNSET_RL ) pCoordInputData = .TRUE.
      IF ( hFacMinDr .NE. UNSET_RL ) rCoordInputData = .TRUE.
      IF ( hFacMinDr .EQ. UNSET_RL ) hFacMinDr       = hFacMinDz
      IF ( hFacMinDr .EQ. UNSET_RL ) hFacMinDr       = hFacMinDp
      IF ( hFacMinDr .EQ. UNSET_RL ) hFacMinDr       = hFacMinDrDefault

      IF (convertFW2Salt.EQ.UNSET_RL) THEN
        convertFW2Salt = 35.
        IF (useRealFreshWaterFlux) convertFW2Salt=-1
        IF ( selectAddFluid.GE.1 ) convertFW2Salt=-1
      ENDIF

C--   for backward compatibility :
      IF ( selectCoriScheme.EQ.UNSET_I ) THEN
        selectCoriScheme = 0
        IF ( useJamartWetPoints ) selectCoriScheme = 1
        IF ( useEnergyConservingCoriolis .AND.
     &       .NOT.vectorInvariantMomentum )
     &    selectCoriScheme = selectCoriScheme + 2
      ENDIF
      IF ( vectorInvariantMomentum ) THEN
        IF ( useJamartWetPoints .AND. selectCoriScheme.NE.1 ) THEN
          WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectCoriScheme=', selectCoriScheme,
     &     ' conflicts with "useJamartWetPoints"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ELSE
        IF ( useEnergyConservingCoriolis
     &       .AND. selectCoriScheme.LT.2 ) THEN
          WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectCoriScheme=', selectCoriScheme,
     &     ' conflicts with "useEnergyConservingCoriolis"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
        IF ( useJamartWetPoints .AND. selectCoriScheme.NE.1
     &                          .AND. selectCoriScheme.NE.3 ) THEN
          WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectCoriScheme=', selectCoriScheme,
     &     ' conflicts with "useJamartWetPoints"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDIF
      IF ( SadournyCoriolis ) THEN
C--   for backward compatibility :
        IF ( selectVortScheme.EQ.UNSET_I ) selectVortScheme = 2
        IF ( selectVortScheme.NE.2 ) THEN
          WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectVortScheme=', selectVortScheme,
     &     ' conflicts with "SadournyCoriolis"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDIF
      IF ( select3dCoriScheme.EQ.UNSET_I ) THEN
C--   for backward compatibility :
        select3dCoriScheme = 0
        IF ( use3dCoriolis ) select3dCoriScheme = 1
      ELSEIF ( select3dCoriScheme.NE.0 .AND. .NOT.use3dCoriolis ) THEN
        WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: select3dCoriScheme=', select3dCoriScheme,
     &     ' conflicts with "use3dCoriolis=F"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectMetricTerms.EQ.UNSET_I ) THEN
C--   for backward compatibility :
        selectMetricTerms = 0
        IF ( metricTerms ) selectMetricTerms = 1
      ELSEIF ( selectMetricTerms.NE.0 .AND. .NOT.metricTerms ) THEN
        WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectMetricTerms=', selectMetricTerms,
     &     ' conflicts with "metricTerms=F"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectBalanceEmPmR.EQ.UNSET_I ) THEN
        selectBalanceEmPmR = 0
        IF ( balanceEmPmR ) selectBalanceEmPmR = 1
      ELSEIF ( selectBalanceEmPmR.NE.1 .AND. balanceEmPmR ) THEN
        WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectBalanceEmPmR=', selectBalanceEmPmR,
     &     ' conflicts with "balanceEmPmR"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( ivdc_kappa.NE.zeroRL .AND. .NOT.implicitDiffusion ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: To use ivdc_kappa you must enable implicit',
     &  ' vertical diffusion.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      coordsSet = 0
      IF ( zCoordInputData ) coordsSet = coordsSet + 1
      IF ( pCoordInputData ) coordsSet = coordsSet + 1
      IF ( rCoordInputData ) coordsSet = coordsSet + 1
      IF ( coordsSet .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Cannot mix z, p and r in the input data.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( rhoConst .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: rhoConst must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
       recip_rhoConst = 1. _d 0
      ELSE
       recip_rhoConst = 1. _d 0 / rhoConst
      ENDIF
      IF ( eosType.EQ.'LINEAR' .AND. rhoNil.LE.0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: rhoNil must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( HeatCapacity_Cp .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: HeatCapacity_Cp must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( gravity .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: gravity must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
       recip_gravity = 1. _d 0
      ELSE
       recip_gravity = 1. _d 0 / gravity
      ENDIF

C-    set default printResidualFreq according to debugLevel
      printResidualFreq = -1
      IF ( debugLevel.GE.debLevE ) printResidualFreq = 1
      IF ( plotLevel.EQ.UNSET_I ) plotLevel = debugLevel

C-    set useSingleCpuInput=.TRUE. if useSingleCpuIO==.TRUE.
      IF ( useSingleCpuIO ) useSingleCpuInput=.TRUE.

C     Check for retired parameters still being used
      nRetired = 0
      IF ( useConstantF ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "useConstantF" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: set "selectCoriMap"',
     &  ' [0,1,2,3] to impose a setting over grid default'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( useBetaPlaneF ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "useBetaPlaneF" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: set "selectCoriMap"',
     &  ' [0,1,2,3] to impose a setting over grid default'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT. useSphereF ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "useSphereF" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: set "selectCoriMap"',
     &  ' [0,1,2,3] to impose a setting over grid default'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( zonal_filt_lat .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: Paramater "zonal_filt_lat" is',
     &  ' no longer allowed in file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: Paramater "zonal_filt_lat" is',
     &  ' now read from file "data.zonfilt".'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( gravitySign .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "gravitySign" is set according to vertical ',
     &  ' coordinate and is no longer allowed in file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( tracerAdvScheme .NE. UNSET_I ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tracerAdvScheme" ',
     &  '(old passive tracer code) is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( trac_EvPrRn .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "trac_EvPrRn" ',
     &  '(old passive tracer code) is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT. tempDiffusion ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tempDiffusion" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: to turn off diffusion',
     &  ' => set diffusivity to zero'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT. saltDiffusion ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "saltDiffusion" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: to turn off diffusion',
     &  ' => set diffusivity to zero'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( viscAstrain .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "viscAstrain" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: to use Strain & Tension',
     &  ' formulation => set useStrainTensionVisc to TRUE'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( viscAtension .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "viscAtension" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: to use Strain & Tension',
     &  ' formulation => set useStrainTensionVisc to TRUE'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT.useAnisotropicViscAgridMax ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "useAnisotropicViscAgridMax" ',
     &  'is not allowed in "data" substitute useAreaViscLength=true'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( usePickupBeforeC35 ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "usePickupBeforeC35" ',
     &  'is no longer supported & not longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( debugMode.NEQV.saveDebugMode ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "debugMode" has been moved to "eedata"',
     &  ' and is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( allowInteriorFreezing ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "allowInteriorFreezing" has been replaced',
     &  ' by pkg/frazil and is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( useOldFreezing ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "useOldFreezing" ',
     &  'is no longer supported & not longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( writeStatePrec .NE. UNSET_I ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "writeStatePrec" ',
     &  '(un-used) is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C--   Elliptic solver parameters
      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM02) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM02'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM02 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
C     Check for retired parameters still being used
      IF ( cg2dChkResFreq .NE. UNSET_I ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: unused "cg2dChkResFreq"',
     &  ' is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( cg3dChkResFreq .NE. UNSET_I ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: unused "cg3dChkResFreq"',
     &  ' is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C--   Time stepping parameters
      rCD               = -1. _d 0
      epsAB_CD          = UNSET_RL
      latBandClimRelax  = UNSET_RL
      deltaTtracer      = 0. _d 0
      forcing_In_AB     = .TRUE.
      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM03'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM03) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM03'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM03 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
C     Check for retired parameters still being used
      IF ( tauThetaClimRelax3Dim .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tauThetaClimRelax3Dim" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: 3-dim. relaxation code',
     & ' has moved to separate pkg/rbcs.'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( tauSaltClimRelax3Dim .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tauSaltClimRelax3Dim" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: 3-dim. relaxation code',
     & ' has moved to separate pkg/rbcs.'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( calendarDumps ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "calendarDumps" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: calendarDumps',
     & ' has moved to "data.cal"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C     Process "timestepping" params
C     o Time step size
      IF ( deltaTtracer .NE. dTtracerLev(1) .AND.
     &     deltaTtracer .NE. 0. .AND. dTtracerLev(1) .NE. 0. ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: deltaTtracer & dTtracerLev(1) not equal'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ELSEIF ( dTtracerLev(1) .NE. 0. ) THEN
        deltaTtracer = dTtracerLev(1)
      ENDIF
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTClock
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTtracer
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTMom
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTFreeSurf
      IF ( deltaT       .EQ. 0. ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: ',
     &   'need to specify in file "data", namelist "PARM03"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: ',
     &   ' a model timestep (in s) deltaT or deltaTClock= ?'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
        deltaT = 1.
      ENDIF
      IF ( deltaTMom    .EQ. 0. ) deltaTMom    = deltaT
      IF ( deltaTtracer .EQ. 0. ) deltaTtracer = deltaT
      IF ( deltaTClock  .EQ. 0. ) deltaTClock  = deltaT
      DO k=1,Nr
       IF (dTtracerLev(k).EQ.0.) dTtracerLev(k)= deltaTtracer
      ENDDO
C Note that this line should set deltaFreesurf=deltaTtracer
C but this would change a lot of existing set-ups so we are
C obliged to set the default inappropriately.
C Be advised that when using asynchronous time stepping
C it is better to set deltaTreesurf=deltaTtracer
      IF ( deltaTFreeSurf .EQ. 0. ) deltaTFreeSurf  = deltaTMom
      IF ( periodicExternalForcing ) THEN
       IF ( externForcingCycle*externForcingPeriod .EQ. 0. ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle,externForcingPeriod =0'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSEIF ( INT(externForcingCycle/externForcingPeriod) .NE.
     &              externForcingCycle/externForcingPeriod ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle <> N*externForcingPeriod'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSEIF ( externForcingCycle.LT.externForcingPeriod ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle < externForcingPeriod'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( externForcingPeriod.LT.deltaTClock ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingPeriod < deltaTClock'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
C     o Adams-Bashforth time stepping:
      IF ( momForcingOutAB  .EQ. UNSET_I ) THEN
        momForcingOutAB  = 1
        IF ( forcing_In_AB ) momForcingOutAB  = 0
      ENDIF
      IF ( tracForcingOutAB .EQ. UNSET_I ) THEN
        tracForcingOutAB = 1
        IF ( forcing_In_AB ) tracForcingOutAB = 0
      ENDIF
C     o Convection frequency
      IF ( cAdjFreq .LT. 0. ) THEN
       cAdjFreq = deltaTClock
      ENDIF
      IF ( ivdc_kappa .NE. 0. .AND. cAdjFreq .NE. 0. ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: You have enabled both ivdc_kappa and',
     &  ' convective adjustment.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF (useCDscheme) THEN
C     o CD coupling (CD scheme):
        IF ( tauCD .EQ. 0. _d 0 ) tauCD = deltaTMom
        IF ( rCD .LT. 0. ) rCD = 1. _d 0 - deltaTMom/tauCD
        IF ( epsAB_CD .EQ. UNSET_RL ) epsAB_CD = abEps
      ENDIF

      IF ( startTime.EQ.UNSET_RL .AND. nIter0.EQ.-1 ) THEN
C     o set default value for start time & nIter0
        startTime = baseTime
        nIter0 = 0
      ELSEIF ( startTime.EQ.UNSET_RL ) THEN
C     o set start time from nIter0
         startTime = baseTime + deltaTClock*DFLOAT(nIter0)
      ELSEIF ( nIter0.EQ.-1 ) THEN
C     o set nIter0 from start time
         nIter0 = NINT( (startTime-baseTime)/deltaTClock )
      ELSEIF ( baseTime.EQ.0. ) THEN
C     o set base time from the 2 others
         baseTime = startTime - deltaTClock*DFLOAT(nIter0)
      ENDIF

      nTimeSteps_l2 = 4
C     o nTimeSteps 1
      IF ( nTimeSteps .EQ. 0 .AND. nEndIter .NE. 0 )
     &     nTimeSteps = nEndIter-nIter0
C     o nTimeSteps 2
      IF ( nTimeSteps .EQ. 0 .AND. endTime .NE. 0. )
     &     nTimeSteps = NINT((endTime-startTime)/deltaTClock)
C     o nEndIter 1
      IF ( nEndIter .EQ. 0 .AND. nTimeSteps .NE. 0 )
     &     nEndIter = nIter0+nTimeSteps
C     o nEndIter 2
      IF ( nEndIter .EQ. 0 .AND. endTime .NE. 0. )
     &     nEndIter = NINT((endTime-baseTime)/deltaTClock)
C     o End Time 1
      IF ( endTime .EQ. 0. .AND. nTimeSteps .NE. 0 )
     &     endTime = startTime + deltaTClock*DFLOAT(nTimeSteps)
C     o End Time 2
      IF ( endTime .EQ. 0. .AND. nEndIter .NE. 0 )
     &     endTime = baseTime + deltaTClock*DFLOAT(nEndIter)

C     o Consistent?
      IF ( startTime .NE. baseTime+deltaTClock*DFLOAT(nIter0) ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: startTime, baseTime and nIter0 are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: Perhaps more than two were set at once'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( nEndIter .NE. nIter0+nTimeSteps ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: nIter0, nTimeSteps and nEndIter are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: Perhaps more than two were set at once'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( nTimeSteps .NE. NINT((endTime-startTime)/deltaTClock)
     &   ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: both endTime and nTimeSteps have been set'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: but are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

C     o Monitor (should also add CPP flag for monitor?)
      IF (monitorFreq.LT.0.) THEN
       monitorFreq=0.
       IF (dumpFreq.NE.0.) monitorFreq=dumpFreq
       IF (diagFreq.NE.0..AND.diagFreq.LT.monitorFreq)
     &         monitorFreq=diagFreq
       IF (taveFreq.NE.0..AND.taveFreq.LT.monitorFreq)
     &         monitorFreq=taveFreq
       IF (chkPtFreq.NE.0..AND.chkPtFreq.LT.monitorFreq)
     &         monitorFreq=chkPtFreq
       IF (pChkPtFreq.NE.0..AND.pChkPtFreq.LT.monitorFreq)
     &         monitorFreq=pChkPtFreq
       IF (monitorFreq.EQ.0.) monitorFreq=deltaTClock
      ENDIF
      IF ( monitorSelect.EQ.UNSET_I ) THEN
        monitorSelect = 2
        IF ( fluidIsWater ) monitorSelect = 3
      ENDIF

C--   Grid parameters
C     In cartesian coords distances are in metres
      DO k =1,Nr
       delZ(k) = UNSET_RL
       delP(k) = UNSET_RL
       delR(k) = UNSET_RL
      ENDDO
C     In spherical polar distances are in degrees
      dxSpacing = UNSET_RL
      dySpacing = UNSET_RL
C-    pCell-Mix  parameters:
      interViscAr_pCell = .FALSE.
      interDiffKr_pCell = .FALSE.
      pCellMix_select = 0
      pCellMix_maxFac = 1. _d 4
      pCellMix_delR = 0.
      DO k=1,Nr
        pCellMix_viscAr(k) = viscArNr(k)
        pCellMix_diffKr(k) = diffKrNrT(k)
      ENDDO

      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM04'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM04) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM04'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM04 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

C     Check for retired parameters still being used
      IF ( Ro_SeaLevel .NE. UNSET_RL ) THEN
c      nRetired = nRetired+1
       IF ( usingPCoords ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &   '"Ro_SeaLevel" (P @ bottom) depreciated (backward compat'
       ELSEIF ( usingZCoords ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &   '"Ro_SeaLevel" (Z @ top) depreciated (backward compat'
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       IF ( usingPCoords ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &   ' only). To set vert. axis, use instead "top_Pres".'
       ELSEIF ( usingZCoords ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &   ' only). To set vert. axis, use instead "seaLev_Z".'
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( rkFac .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "rkFac" has been replaced by -rkSign ',
     &  ' and is no longer allowed in file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( groundAtK1 ) THEN
c      nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "groundAtK1" is set according to vertical ',
     &  ' coordinate and is no longer allowed in file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( thetaMin .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "thetaMin" no longer allowed,',
     &  ' has been replaced by "xgOrigin"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( phiMin .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "phiMin" no longer allowed,',
     &  ' has been replaced by "ygOrigin"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C     X coordinate : Check for multiple definitions
      goptCount = 0
      IF ( delX(1)   .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( dxSpacing .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( delXFile  .NE. ' ' )      goptCount = goptCount + 1
      IF ( goptCount.GT.1 ) THEN
       WRITE(msgBuf,'(A,A)') 'Too many specifications for delX:',
     &   'Specify only one of delX, dxSpacing or delXfile'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( dxSpacing .NE. UNSET_RL ) THEN
       DO i=1,gridNx
        delX(i) = dxSpacing
       ENDDO
      ENDIF
C     Y coordinate : Check for multiple definitions
      goptCount = 0
      IF ( delY(1)   .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( dySpacing .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( delYFile  .NE. ' ' )      goptCount = goptCount + 1
      IF ( goptCount.GT.1 ) THEN
       WRITE(msgBuf,'(A,A)') 'Too many specifications for delY:',
     &   'Specify only one of delY, dySpacing or delYfile'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( dySpacing .NE. UNSET_RL ) THEN
       DO j=1,gridNy
        delY(j) = dySpacing
       ENDDO
      ENDIF

C--   Check for conflicting grid definitions.
      goptCount = 0
      IF ( usingCartesianGrid )      goptCount = goptCount+1
      IF ( usingSphericalPolarGrid ) goptCount = goptCount+1
      IF ( usingCurvilinearGrid )    goptCount = goptCount+1
      IF ( usingCylindricalGrid )    goptCount = goptCount+1
      IF ( goptCount .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: More than one coordinate system requested'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( goptCount .LT. 1 ) THEN
C-     No horizontal grid is specified => use Cartesian grid as default:
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: No horizontal grid requested'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: => Use Cartesian Grid as default'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       usingCartesianGrid = .TRUE.
      ENDIF
C-    Radius of the Planet:
      IF ( rSphere.EQ.UNSET_RL ) THEN
        IF ( usingCurvilinearGrid .AND.
     &       radius_fromHorizGrid.NE.UNSET_RL ) THEN
           rSphere = radius_fromHorizGrid
        ELSE
           rSphere = 6370. _d 3
        ENDIF
      ENDIF
      IF ( radius_fromHorizGrid.EQ.UNSET_RL ) THEN
           radius_fromHorizGrid = rSphere
      ENDIF
      IF ( rSphere .NE. 0. ) THEN
       recip_rSphere = 1. _d 0/rSphere
      ELSE
       recip_rSphere = 0.
      ENDIF
C--   Grid rotation
      IF ( phiEuler .NE. 0. _d 0 .OR. thetaEuler .NE. 0. _d 0
     &     .OR. psiEuler .NE. 0. _d 0 ) rotateGrid = .TRUE.

C--   Default vertical axis origin
      IF ( Ro_SeaLevel .NE. UNSET_RL ) THEN
       IF ( usingPCoords .AND. top_Pres.NE.UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: ',
     &       'Cannot set both "Ro_SeaLevel" and "top_Pres"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( usingZCoords .AND. seaLev_Z.NE.UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: ',
     &       'Cannot set both "Ro_SeaLevel" and "seaLev_Z"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       rF(1) = Ro_SeaLevel
      ELSE
       rF(1) = UNSET_RS
      ENDIF
      IF ( top_Pres.EQ.UNSET_RL ) top_Pres = 0.
      IF ( seaLev_Z.EQ.UNSET_RL ) seaLev_Z = 0.
C--   Default origin for  X & Y axis (longitude & latitude origin):
      IF ( xgOrigin .EQ. UNSET_RL ) xgOrigin = 0.
      IF ( ygOrigin .EQ. UNSET_RL ) THEN
        IF ( usingSphericalPolarGrid )  THEN
          ygOrigin = 0.
        ELSEIF ( usingCartesianGrid )   THEN
          ygOrigin = 0.
        ELSEIF ( usingCylindricalGrid ) THEN
          ygOrigin = 0.
        ELSEIF ( usingCurvilinearGrid ) THEN
          ygOrigin = 0.
        ELSE
          WRITE(msgBuf,'(A)')
     &    'S/R INI_PARMS: found no coordinate system to set ygOrigin'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDIF

C--   set cell Center depth and put Interface at the middle between 2 C
      setCenterDr = .FALSE.
      DO k=1,Nr+1
       IF ( delRc(k).EQ.UNSET_RL ) THEN
        IF ( setCenterDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delRc at k =', k
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ELSE
        IF ( k.EQ.1 ) setCenterDr = .TRUE.
        IF ( .NOT.setCenterDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delRc at k <', k
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
      ENDDO
      IF ( setCenterDr ) rCoordInputData = .TRUE.
C--   p, z, r coord parameters
      setInterFDr = .FALSE.
      DO k = 1, Nr
       IF ( delZ(k) .NE. UNSET_RL ) zCoordInputData = .TRUE.
       IF ( delP(k) .NE. UNSET_RL ) pCoordInputData = .TRUE.
       IF ( delR(k) .NE. UNSET_RL ) rCoordInputData = .TRUE.
       IF ( delR(k) .EQ. UNSET_RL ) delR(k) = delZ(k)
       IF ( delR(k) .EQ. UNSET_RL ) delR(k) = delP(k)
       IF ( delR(k) .EQ. UNSET_RL ) THEN
        IF ( setInterFDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delZ/delP/delR at k =', k
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ELSE
        IF ( k.EQ.1 ) setInterFDr = .TRUE.
        IF ( .NOT.setInterFDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delZ/delP/delR at k <', k
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
      ENDDO
C     Check for multiple coordinate systems
      coordsSet = 0
      IF ( zCoordInputData ) coordsSet = coordsSet + 1
      IF ( pCoordInputData ) coordsSet = coordsSet + 1
      IF ( rCoordInputData ) coordsSet = coordsSet + 1
      IF ( coordsSet .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Cannot mix z, p and r in the input data.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
C-    Check for double definition (file & namelist)
      IF ( delRcFile.NE.' ' ) THEN
        IF ( setCenterDr ) THEN
          WRITE(msgBuf,'(A)')
     &         'S/R INI_PARMS: Cannot set both delRc and delRcFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
        setCenterDr = .TRUE.
      ENDIF
      IF ( delRFile.NE.' ' ) THEN
        IF ( setInterFDr ) THEN
          WRITE(msgBuf,'(A)')
     &         'S/R INI_PARMS: Cannot set both delR and delRFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
        setInterFDr = .TRUE.
      ENDIF

C--   Input files
      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM05'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM05) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM05'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM05 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
C     Check for retired parameters still being used
      IF ( shelfIceFile .NE. ' ' ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "shelfIceFile" is not allowed in "data", ',
     &  'substitute "SHELFICEtopoFile" in data.shelfice'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( dQdTFile .NE. ' ' ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "dQdTFile" has been retired from file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
C     Check if two conflicting I/O directories have been specified
      IF (mdsioLocalDir .NE. ' ' .AND. adTapeDir .NE. ' ') THEN
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: mdsioLocalDir and adTapeDir cannot be'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: specified at the same time'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

C     Check for relaxation term:
       IF ( (tauThetaClimRelax.GT.0.).AND.
     &      (thetaClimFile.EQ.' ') ) THEN
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: tauThetaClimRelax > 0 but'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: thetaClimFile is undefined'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
       ENDIF
       IF ( (tauSaltClimRelax.GT.0.).AND.
     &      (saltClimFile.EQ.' ') ) THEN
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: tauSaltClimRelax > 0 but'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: saltClimFile is undefined'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
       ENDIF
C     Check vertical diffusivity setting:
#ifdef ALLOW_3D_DIFFKR
       IF ( specifiedDiffKrT ) THEN
         WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: Ignores diffKr',
     &   'T (or Kp,Kz) setting in file "data" with ALLOW_3D_DIFFKR'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( specifiedDiffKrS .AND. diffKrFile.NE.' ' ) THEN
         WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: Ignores diffKr',
     &   'S (or Kp,Kz) setting in file "data" and uses diffKrFile'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
       ENDIF
#endif

C--   Set Units conversion factor required to incorporate
C     surface forcing into z-p isomorphic equations:
C     mass2rUnit: from mass per unit area [kg/m2] to r-coordinate
C     rUnit2mass: from r-coordinate to mass per unit area [kg/m2]
      IF ( usingPCoords ) THEN
         mass2rUnit = gravity
         rUnit2mass = recip_gravity
      ELSE
         mass2rUnit = recip_rhoConst
         rUnit2mass = rhoConst
      ENDIF

C--   For backward compatibility, set temp_addMass and salt_addMass
C     to temp_EvPrRn and salt_EvPrRn if not set in parameter file "data"
      IF (temp_addMass .EQ. UNSET_RL) temp_addMass = temp_EvPrRn
      IF (salt_addMass .EQ. UNSET_RL) salt_addMass = salt_EvPrRn

C--   Make a choice (if unset) regarding using CG2D solver min.-residual sol.
C      for simple set-up (cartesian grid + flat bottom), default is to
C      use the solver minimum residual solution (cg2dUseMinResSol=1).
      IF ( cg2dUseMinResSol.EQ.UNSET_I ) THEN
        cg2dUseMinResSol = 0
        IF ( topoFile.EQ.' ' .AND. bathyFile.EQ.' '
     &       .AND. usingCartesianGrid ) cg2dUseMinResSol = 1
      ENDIF

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      WRITE(msgBuf,'(A)') ' INI_PARMS: finished reading file "data"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C--   Check whether any retired parameters were found.
      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading parameter file "data":'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(I4,A)') nRetired,
     &      ' out of date parameters were found in the namelist'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
C--   Stop if any error was found (including retired params):
      IF ( errCount .GE. 1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'S/R INI_PARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
