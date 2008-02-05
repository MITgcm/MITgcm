C $Header: /u/gcmpack/MITgcm/model/src/ini_parms.F,v 1.205 2008/02/05 13:32:49 mlosch Exp $
C $Name:  $

c #include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: INI_PARMS
C     !INTERFACE:
      SUBROUTINE INI_PARMS( myThid )

C     !DESCRIPTION:
C     Routine to load model "parameters" from parameter file "data"

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: Number of this instance of INI_PARMS
      INTEGER myThid

C     !LOCAL VARIABLES:
C     dxSpacing, dySpacing :: Default spacing in X and Y.
C                          :: Units are that of coordinate system
C                          :: i.e. cartesian => metres
C                          ::       s. polar => degrees
C     deltaTtracer  :: Timestep for tracer equations ( s )
C     forcing_In_AB :: flag to put all forcings (Temp,Salt,Tracers,Momentum)
C                   :: contribution in (or out of) Adams-Bashforth time stepping.
C     goptCount  :: Used to count the nuber of grid options (only one is allowed!)
C     msgBuf     :: Informational/error meesage buffer
C     errIO      :: IO error flag
C     iUnit      :: Work variable for IO unit number
C     record     :: Work variable for IO buffer
C     K, I, J    :: Loop counters
C     xxxDefault :: Default value for variable xxx
      _RL  dxSpacing
      _RL  dySpacing
      _RL deltaTtracer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
      LOGICAL forcing_In_AB
      INTEGER goptCount
      INTEGER K, i, j, IL, iUnit
      INTEGER errIO
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
C     Default values for variables which have vertical coordinate system
C     dependency.
      _RL viscArDefault
      _RL diffKrTDefault
      _RL diffKrSDefault
      _RL hFacMinDrDefault
      _RL delRDefault(Nr)
      _RS rkFacDefault
C     zCoordInputData :: Variables used to select between different coordinate systems.
C     pCoordInputData :: The vertical coordinate system in the rest of the model is
C     rCoordInputData :: written in terms of r. In the model "data" file input data
C     coordsSet       :: can be interms of z, p or r.
C                     :: e.g. delZ or delP or delR for the vertical grid spacing.
C                     :: The following rules apply:
C                     :: All parameters must use the same vertical coordinate system.
C                     ::  e.g. delZ and viscAz is legal but
C                     ::       delZ and viscAr is an error.
C                     :: Similarly specifying delZ and delP is an error.
C                     :: zCoord..., pCoord..., rCoord... are used to flag when
C                     :: z, p or r are used.
C                     :: coordsSet counts how many vertical coordinate systems have
C                     :: been used to specify variables. coordsSet > 1 is an error.
C

      LOGICAL zCoordInputData
      LOGICAL pCoordInputData
      LOGICAL rCoordInputData
      INTEGER coordsSet
      LOGICAL diffKrSet

C     Variables which have vertical coordinate system dependency.
C     delZ    :: Vertical grid spacing ( m  ).
C     delP    :: Vertical grid spacing ( Pa ).
C     viscAz  :: Eddy viscosity coeff. for mixing of momentum vertically ( m^2/s )
C     viscAp  :: Eddy viscosity coeff. for mixing of momentum vertically ( Pa^2/s )
C     diffKzT :: Laplacian diffusion coeff. for mixing of heat vertically ( m^2/s )
C     diffKpT :: Laplacian diffusion coeff. for mixing of heat vertically ( Pa^2/s )
C     diffKzS :: Laplacian diffusion coeff. for mixing of salt vertically ( m^2/s )
C     diffKpS :: Laplacian diffusion coeff. for mixing of salt vertically ( Pa^2/s )
      _RL delZ(Nr)
      _RL delP(Nr)
      _RL viscAz
      _RL viscAp
      _RL diffKzT
      _RL diffKpT
      _RL diffKrT
      _RL diffKzS
      _RL diffKpS
      _RL diffKrS

C     Retired main data file parameters. Kept here to trap use of old data files.
C Namelist PARM01:
C     tracerAdvScheme :: tracer advection scheme (old passive tracer code)
C     trac_EvPrRn :: tracer conc. in Rain & Evap (old passive tracer code)
C     saltDiffusion :: diffusion of salinity    on/off (flag not used)
C     tempDiffusion :: diffusion of temperature on/off (flag not used)
C     zonal_filt_lat :: Moved to package "zonal_filt"
C     gravitySign :: direction of gravity relative to R direction
C                 :: (removed from namelist and set according to z/p coordinate)
C     viscAstrain  :: replaced by standard viscosity coeff & useStrainTensionVisc
C     viscAtension :: replaced by standard viscosity coeff & useStrainTensionVisc
C     useAnisotropicViscAgridMax :: Changed to be default behavior.  Can
C                 use old method by setting useAreaViscLength=.true.
C     usePickupBeforeC35 :: to restart from old-pickup files (generated with code
C                   from before checkpoint-35, Feb 08, 2001): disabled (Jan 2007)
C Namelist PARM03:
C     tauThetaClimRelax3Dim :: replaced by pkg/rbcs (3.D Relaxation B.Cs)
C     tauSaltClimRelax3Dim  :: replaced by pkg/rbcs (3.D Relaxation B.Cs)
C     calendarDumps  :: moved to package "cal" (calendar)
C Namelist PARM04:
C     groundAtK1 :: put the surface(k=1) at the ground (replaced by usingPCoords)
C     rkFac      :: removed from namelist ; replaced by -rkSign
C     nRetired :: Counter used to trap gracefully namelists containing "retired"
C              :: parameters. These are parameters that are either no-longer used
C                 or that have moved to a different input file and/or namelist.
      LOGICAL tempDiffusion, saltDiffusion
      INTEGER tracerAdvScheme
      _RL trac_EvPrRn
      _RL zonal_filt_lat, gravitySign
      _RL viscAstrain, viscAtension
      LOGICAL useAnisotropicViscAgridMax
      LOGICAL usePickupBeforeC35
C-
      _RL tauThetaClimRelax3Dim, tauSaltClimRelax3Dim
      LOGICAL calendarDumps
C-
      LOGICAL groundAtK1
      _RL rkFac
      INTEGER nRetired

C--   Continuous equation parameters
      NAMELIST /PARM01/
     & gravitySign, nh_Am2,
     & gravity, gBaro, rhonil, tAlpha, sBeta,
     & f0, beta, omega, rotationPeriod,
     & viscAh, viscAhW, viscAhMax,
     & viscAhGrid, viscAhGridMax, viscAhGridMin,
     & viscC2leith, viscC4leith,
     & useFullLeith, useAnisotropicViscAgridMax, useStrainTensionVisc,
     & useAreaViscLength,
     & viscC2leithD, viscC4leithD, viscC2Smag, viscC4Smag,
     & viscAhD, viscAhZ, viscA4D, viscA4Z,
     & viscA4, viscA4W,
     & viscA4Max, viscA4Grid, viscA4GridMax, viscA4GridMin,
     & viscA4ReMax, viscAhReMax,
     & viscAz,  cosPower, viscAstrain, viscAtension,
     & diffKhT, diffKzT, diffK4T,
     & diffKhS, diffKzS, diffK4S,
     & tRef, sRef, tRefFile, sRefFile, rhoRefFile,
     & eosType, integr_GeoPot, selectFindRoSurf,
     & atm_Cp, atm_Rd, atm_Rq,
     & no_slip_sides, sideDragFactor,
     & no_slip_bottom, bottomDragLinear, bottomDragQuadratic,
     & momViscosity,  momAdvection, momForcing, useCoriolis,
     & useConstantF, useBetaPlaneF, useSphereF, use3dCoriolis,
     & momPressureForcing, metricTerms, vectorInvariantMomentum,
     & tempDiffusion, tempAdvection, tempForcing,
     & saltDiffusion, saltAdvection, saltForcing,
     & implicSurfPress, implicDiv2DFlow,
     & implicitFreeSurface, rigidLid, freeSurfFac, hFacMin, hFacMinDz,
     & exactConserv, linFSConserveTr, uniformLin_PhiSurf,
     & nonlinFreeSurf, hFacInf, hFacSup, select_rStar,
     & implicitIntGravWave, staggerTimeStep,
     & tempStepping, saltStepping, momStepping,
     & implicitDiffusion, implicitViscosity,
     & tempImplVertAdv, saltImplVertAdv, momImplVertAdv,
     & viscAr, diffKrT, diffKrS, diffKrNrT, diffKrNrS, hFacMinDr,
     & viscAp, diffKpT, diffKpS, hFacMinDp,
     & diffKrBL79surf, diffKrBL79deep, diffKrBL79scl, diffKrBL79Ho,
     & BL79LatVary,
     & diffKrBLEQsurf, diffKrBLEQdeep, diffKrBLEQscl, diffKrBLEQHo,
     & rhoConst, rhoConstFresh, buoyancyRelation, HeatCapacity_Cp,
     & writeBinaryPrec, readBinaryPrec, writeStatePrec,
     & nonHydrostatic, quasiHydrostatic, globalFiles, useSingleCpuIO,
     & allowFreezing, useOldFreezing, ivdc_kappa, hMixCriteria,
     & usePickupBeforeC35, usePickupBeforeC54, debugMode, debugLevel,
     & tempAdvScheme, tempVertAdvScheme,
     & saltAdvScheme, saltVertAdvScheme, tracerAdvScheme,
     & multiDimAdvection, useEnergyConservingCoriolis,
     & useCDscheme, useJamartWetPoints, useJamartMomAdv, useNHMTerms,
     & SadournyCoriolis, upwindVorticity, highOrderVorticity,
     & useAbsVorticity, upwindShear, selectKEscheme,
     & useRealFreshWaterFlux, convertFW2Salt,
     & temp_EvPrRn, salt_EvPrRn, trac_EvPrRn,
     & zonal_filt_lat,
     & inAdExact, smoothAbsFuncRange,
     & balanceEmPmR, balanceQnet, balancePrintMean

C--   Elliptic solver parameters
      NAMELIST /PARM02/
     & cg2dMaxIters, cg2dChkResFreq, cg2dTargetResidual,
     & cg2dTargetResWunit, cg2dpcOffDFac, cg2dPreCondFreq,
     & cg3dMaxIters, cg3dChkResFreq, cg3dTargetResidual

C--   Time stepping parammeters
      NAMELIST /PARM03/
     & nIter0, nTimeSteps, nEndIter,
     & baseTime, startTime, endTime,
     & deltaT, deltaTClock, deltaTmom,
     & deltaTtracer, dTtracerLev, deltaTfreesurf,
     & forcing_In_AB, momForcingOutAB, tracForcingOutAB,
     & momDissip_In_AB, doAB_onGtGs,
     & abEps, alph_AB, beta_AB, startFromPickupAB2,
     & tauCD, rCD, cAdjFreq,
     & chkPtFreq, pChkPtFreq, pickupSuff, pickupStrictlyMatch,
     & dumpFreq, dumpInitAndLast, adjDumpFreq, taveFreq, tave_lastIter,
     & diagFreq, monitorFreq, adjMonitorFreq, monitorSelect,
     & outputTypesInclusive,
     & tauThetaClimRelax, tauSaltClimRelax, latBandClimRelax,
     & tauThetaClimRelax3Dim, tauSaltClimRelax3Dim, tauTr1ClimRelax,
     & periodicExternalForcing, externForcingPeriod, externForcingCycle,
     & calendarDumps

C--   Gridding parameters
      NAMELIST /PARM04/
     & usingCartesianGrid, usingCylindricalGrid,
     & dxSpacing, dySpacing, delX, delY, delXFile, delYFile,
     & usingSphericalPolarGrid, phiMin, thetaMin, rSphere,
     & usingCurvilinearGrid, horizGridFile, deepAtmosphere,
     & Ro_SeaLevel, delZ, delP, delR, delRc, delRFile, delRcFile,
     & rkFac, groundAtK1,
     & phiEuler, thetaEuler, psiEuler

C--   Input files
      NAMELIST /PARM05/
     & bathyFile, topoFile, shelfIceFile,
     & hydrogThetaFile, hydrogSaltFile, diffKrFile,
     & zonalWindFile, meridWindFile,
     & thetaClimFile, saltClimFile,
     & surfQfile, surfQnetFile, surfQswFile, EmPmRfile, saltFluxFile,
     & lambdaThetaFile, lambdaSaltFile,
     & uVelInitFile, vVelInitFile, pSurfInitFile,
     & dQdTFile, ploadFile,tCylIn,tCylOut,
     & eddyTauxFile, eddyTauyFile,
     & mdsioLocalDir,
     & the_run_name
CEOP

C
      _BEGIN_MASTER(myThid)

C     Defaults values for input parameters
      CALL SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRDefault, rkFacDefault,
     I   myThid )

C--   Initialise "which vertical coordinate system used" flags.
      zCoordInputData = .FALSE.
      pCoordInputData = .FALSE.
      rCoordInputData = .FALSE.
      coordsSet       = 0

C--   Initialise retired parameters to unlikely value
      nRetired = 0
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
      tauThetaClimRelax3Dim = UNSET_RL
      tauSaltClimRelax3Dim  = UNSET_RL
      calendarDumps   = .FALSE.
      rkFac           = UNSET_RL
      groundAtK1      = .FALSE.

C--   Open the parameter file
#ifdef TARGET_BGL
      OPEN(UNIT=scrUnit1,FILE='scratch1',STATUS='UNKNOWN')
      OPEN(UNIT=scrUnit2,FILE='scratch2',STATUS='UNKNOWN')
#else
      OPEN(UNIT=scrUnit1,STATUS='SCRATCH')
      OPEN(UNIT=scrUnit2,STATUS='SCRATCH')
#endif
      OPEN(UNIT=modelDataUnit,FILE='data',STATUS='OLD',
     &     IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Unable to open model parameter'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

      DO WHILE ( .TRUE. )
       READ(modelDataUnit,FMT='(A)',END=1001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       IF ( RECORD(1:1) .NE. commentCharacter ) THEN
        CALL NML_SET_TERMINATOR( RECORD )
        WRITE(UNIT=scrUnit1,FMT='(A)') RECORD(:IL)
       ENDIF
       WRITE(UNIT=scrUnit2,FMT='(A)') RECORD(:IL)
      ENDDO
 1001 CONTINUE
      CLOSE(modelDataUnit)

C--   Report contents of model parameter file
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// Model parameter file "data"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      iUnit = scrUnit2
      REWIND(iUnit)
      DO WHILE ( .TRUE. )
       READ(UNIT=iUnit,FMT='(A)',END=2001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       WRITE(msgBuf,'(A,A)') '>',RECORD(:IL)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDDO
 2001 CONTINUE
      CLOSE(iUnit)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )


C--   Read settings from model parameter file "data".
      iUnit = scrUnit1
      REWIND(iUnit)

C--   Set default "physical" parameters
      viscAhW  = UNSET_RL
      viscA4W  = UNSET_RL
      viscAhD  = UNSET_RL
      viscAhZ  = UNSET_RL
      viscA4D  = UNSET_RL
      viscA4Z  = UNSET_RL
      viscAz   = UNSET_RL
      viscAr   = UNSET_RL
      viscAp   = UNSET_RL
      diffKzT  = UNSET_RL
      diffKpT  = UNSET_RL
      diffKrT  = UNSET_RL
      diffKzS  = UNSET_RL
      diffKpS  = UNSET_RL
      diffKrS  = UNSET_RL
      DO k=1,Nr
       diffKrNrT(k) = UNSET_RL
       diffKrNrS(k) = UNSET_RL
       tRef(k)      = UNSET_RL
       sRef(k)      = UNSET_RL
      ENDDO
      gBaro    = UNSET_RL
      rhoConst = UNSET_RL
      omega    = UNSET_RL
      hFacMinDr           = UNSET_RL
      hFacMinDz           = UNSET_RL
      hFacMinDp           = UNSET_RL
      rhoConstFresh  = UNSET_RL
      convertFW2Salt = UNSET_RL
      tAlpha              = UNSET_RL
      sBeta               = UNSET_RL
      tempVertAdvScheme = 0
      saltVertAdvScheme = 0
C--   z,p,r coord input switching.
      WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; starts to read PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM01) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM01'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; read PARM01 : OK'
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
        STOP 'ABNORMAL END: S/R INI_PARMS'
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
      IF ( implicitFreeSurface ) freeSurfFac = 1.D0
      IF ( rigidLid            ) freeSurfFac = 0.D0
      IF ( gBaro .EQ. UNSET_RL ) gBaro=gravity
      IF ( rhoConst .EQ. UNSET_RL ) rhoConst=rhoNil
      IF ( rhoConstFresh .EQ. UNSET_RL ) rhoConstFresh=rhoConst
      IF ( omega .EQ. UNSET_RL ) THEN
        omega = 0. _d 0
        IF ( rotationPeriod .NE. 0. _d 0 )
     &  omega = 2.D0 * PI / rotationPeriod
      ELSEIF ( omega .EQ. 0. _d 0 ) THEN
        rotationPeriod = 0. _d 0
      ELSE
        rotationPeriod = 2.D0 * PI / omega
      ENDIF
      IF (atm_Rd .EQ. UNSET_RL) THEN
        atm_Rd = atm_Cp * atm_kappa
      ELSE
        atm_kappa = atm_Rd / atm_Cp
      ENDIF
C--   Non-hydrostatic/quasi-hydrostatic
      IF (nonHydrostatic.AND.quasiHydrostatic) THEN
       WRITE(msgBuf,'(A)')
     &   'Illegal: both nonHydrostatic = quasiHydrostatic = TRUE'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
C--  Advection and Forcing for Temp and salt
      IF (tempVertAdvScheme.EQ.0) tempVertAdvScheme = tempAdvScheme
      IF (saltVertAdvScheme.EQ.0) saltVertAdvScheme = saltAdvScheme
C--  horizontal viscosity for vertical moments
      IF ( viscAhW .EQ. UNSET_RL ) viscAhW = viscAh
      IF ( viscA4W .EQ. UNSET_RL ) viscA4W = viscA4
C--  horizontal viscosity (acting on Divergence or Vorticity)
      IF ( viscAhD .EQ. UNSET_RL ) viscAhD = viscAh
      IF ( viscAhZ .EQ. UNSET_RL ) viscAhZ = viscAh
      IF ( viscA4D .EQ. UNSET_RL ) viscA4D = viscA4
      IF ( viscA4Z .EQ. UNSET_RL ) viscA4Z = viscA4
C--   z,p,r coord input switching.
      IF ( viscAz .NE. UNSET_RL ) zCoordInputData = .TRUE.
      IF ( viscAp .NE. UNSET_RL ) pCoordInputData = .TRUE.
      IF ( viscAr .NE. UNSET_RL ) rCoordInputData = .TRUE.
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscAz
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscAp
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscArDefault

      IF ( diffKzT .NE. UNSET_RL ) zCoordInputData  = .TRUE.
      IF ( diffKpT .NE. UNSET_RL ) pCoordInputData  = .TRUE.
      IF ( diffKrT .NE. UNSET_RL ) rCoordInputData  = .TRUE.
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKzT
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKpT
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKrTDefault
      diffKrSet = .TRUE.
      DO k=1,Nr
        IF ( diffKrNrT(k).EQ. UNSET_RL ) diffKrSet = .FALSE.
      ENDDO
      IF ( .NOT.diffKrSet ) THEN
        DO k=1,Nr
         diffKrNrT(k) = diffKrT
        ENDDO
      ELSEIF ( diffKrT.NE.diffKrTDefault ) THEN
       WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: Cannot set both ',
     &    'diffKrNrT and diffKrT (or Kp,Kz) in input file data'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

      IF ( diffKzS .NE. UNSET_RL ) zCoordInputData  = .TRUE.
      IF ( diffKpS .NE. UNSET_RL ) pCoordInputData  = .TRUE.
      IF ( diffKrS .NE. UNSET_RL ) rCoordInputData  = .TRUE.
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKzS
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKpS
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKrSDefault
      diffKrSet = .TRUE.
      DO k=1,Nr
        IF ( diffKrNrS(k).EQ. UNSET_RL ) diffKrSet = .FALSE.
      ENDDO
      IF ( .NOT.diffKrSet ) THEN
        DO k=1,Nr
         diffKrNrS(k) = diffKrS
        ENDDO
      ELSEIF ( diffKrS.NE.diffKrSDefault ) THEN
       WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: Cannot set both ',
     &    'diffKrNrS and diffKrS (or Kp,Kz) in input file data'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
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
      ENDIF

      IF ( ivdc_kappa .NE. 0. .AND. .NOT. implicitDiffusion ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: To use ivdc_kappa you must enable implicit',
     &  ' vertical diffusion.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

      coordsSet = 0
      IF ( zCoordInputData ) coordsSet = coordsSet + 1
      IF ( pCoordInputData ) coordsSet = coordsSet + 1
      IF ( rCoordInputData ) coordsSet = coordsSet + 1
      IF ( coordsSet .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Cannot mix z, p and r in the input data.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
      IF ( rhoConst .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: rhoConst must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_rhoConst = 1.D0 / rhoConst
      ENDIF
      IF ( rhoNil .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: rhoNil must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_rhoNil = 1.D0 / rhoNil
      ENDIF
      IF ( HeatCapacity_Cp .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: HeatCapacity_Cp must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_Cp = 1.D0 / HeatCapacity_Cp
      ENDIF
      IF ( gravity .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: gravity must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_gravity = 1.D0 / gravity
      ENDIF
C     This flags are now passed to RW and MDSIO packages in ini_model_io.F
C     Set globalFiles flag for READ_WRITE_FLD package
c     CALL SET_WRITE_GLOBAL_FLD( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
c     CALL SET_WRITE_GLOBAL_REC( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
c     CALL SET_WRITE_GLOBAL_PICKUP( globalFiles )

C     Check for retired parameters still being used
      nRetired = 0
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

C--   Elliptic solver parameters
      WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; starts to read PARM02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM02) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'parameter file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM02'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; read PARM02 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

C--   Time stepping parameters
      rCD               = -1.D0
      latBandClimRelax  = UNSET_RL
      deltaTtracer      = 0. _d 0
      forcing_In_AB     = .TRUE.
      WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; starts to read PARM03'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM03) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM03'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; read PARM03 : OK'
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
        STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSEIF ( dTtracerLev(1) .NE. 0. ) THEN
        deltaTtracer = dTtracerLev(1)
      ENDIF
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTClock
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTtracer
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTmom
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTfreesurf
      IF ( deltaTmom    .EQ. 0. ) deltaTmom    = deltaT
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
      IF ( deltaTfreesurf .EQ. 0. ) deltaTfreesurf  = deltaTmom
      IF ( periodicExternalForcing ) THEN
       IF ( externForcingCycle*externForcingPeriod .EQ. 0. ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle,externForcingPeriod =0'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_PARMS'
       ENDIF
       IF ( INT(externForcingCycle/externForcingPeriod) .NE.
     &          externForcingCycle/externForcingPeriod ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle <> N*externForcingPeriod'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_PARMS'
       ENDIF
       IF ( externForcingCycle.lt.externForcingPeriod ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle < externForcingPeriod'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_PARMS'
       ENDIF
       IF ( externForcingPeriod.lt.deltaTclock ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingPeriod < deltaTclock'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_PARMS'
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
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
      IF (useCDscheme) THEN
C     o CD coupling (CD scheme):
        IF ( tauCD .EQ. 0.D0 ) tauCD = deltaTmom
        IF ( rCD .LT. 0. ) rCD = 1. _d 0 - deltaTMom/tauCD
      ENDIF
C     o Temperature climatology relaxation time scale
      IF ( tauThetaClimRelax .EQ. 0.D0 ) THEN
       doThetaClimRelax     = .FALSE.
      ELSE
       doThetaClimRelax     = .TRUE.
      ENDIF
C     o Salinity climatology relaxation time scale
      IF ( tauSaltClimRelax .EQ. 0.D0 ) THEN
       doSaltClimRelax     = .FALSE.
      ELSE
       doSaltClimRelax     = .TRUE.
      ENDIF
C     o Tracer 1 climatology relaxation time scale
      IF ( tauTr1ClimRelax .EQ. 0.D0 ) THEN
       doTr1ClimRelax     = .FALSE.
       lambdaTr1ClimRelax = 0.D0
      ELSE
       doTr1ClimRelax     = .TRUE.
       lambdaTr1ClimRelax = 1./tauTr1ClimRelax
      ENDIF

C     o Base time
      IF ( nIter0.NE.0 .AND. startTime.NE.0. .AND. baseTime.EQ.0. )
     &   baseTime = startTime - deltaTClock*float(nIter0)
C     o Start time
      IF ( nIter0 .NE. 0 .AND. startTime .EQ. 0. )
     &   startTime = baseTime + deltaTClock*float(nIter0)
C     o nIter0
      IF ( nIter0 .EQ. 0 .AND. startTime .NE. baseTime )
     &   nIter0 = NINT( (startTime-baseTime)/deltaTClock )

C     o nTimeSteps 1
      IF ( nTimeSteps .EQ. 0 .AND. nEndIter .NE. 0 )
     &     nTimeSteps = nEndIter-nIter0
C     o nTimeSteps 2
      IF ( nTimeSteps .EQ. 0 .AND. endTime .NE. 0. )
     &     nTimeSteps = NINT((endTime-startTime)/deltaTclock)
C     o nEndIter 1
      IF ( nEndIter .EQ. 0 .AND. nTimeSteps .NE. 0 )
     &     nEndIter = nIter0+nTimeSteps
C     o nEndIter 2
      IF ( nEndIter .EQ. 0 .AND. endTime .NE. 0. )
     &     nEndIter = NINT((endTime-baseTime)/deltaTclock)
C     o End Time 1
      IF ( endTime .EQ. 0. .AND. nTimeSteps .NE. 0 )
     &     endTime = startTime + deltaTClock*float(nTimeSteps)
C     o End Time 2
      IF ( endTime .EQ. 0. .AND. nEndIter .NE. 0 )
     &     endTime = baseTime + deltaTClock*float(nEndIter)

C     o Consistent?
      IF ( startTime .NE. baseTime+deltaTClock*float(nIter0) ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: startTime, baseTime and nIter0 are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: Perhaps more than two were set at once'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
      IF ( nEndIter .NE. nIter0+nTimeSteps ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: nIter0, nTimeSteps and nEndIter are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: Perhaps more than two were set at once'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
      IF ( nTimeSteps .NE. NINT((endTime-startTime)/deltaTClock)
     &   ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: both endTime and nTimeSteps have been set'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: but are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
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
       IF (monitorFreq.EQ.0.) monitorFreq=deltaTclock
      ENDIF
      IF ( monitorSelect.EQ.UNSET_I ) THEN
        monitorSelect = 2
        IF ( fluidIsWater ) monitorSelect = 3
      ENDIF

C--   Grid parameters
C     In cartesian coords distances are in metres
      DO K =1,Nr
       delZ(K) = UNSET_RL
       delP(K) = UNSET_RL
       delR(K) = UNSET_RL
      ENDDO
C     In spherical polar distances are in degrees
      dxSpacing = UNSET_RL
      dySpacing = UNSET_RL
      WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; starts to read PARM04'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM04,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM04'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; read PARM04 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

C     Check for retired parameters still being used
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

C     X coordinate : Check for multiple definitions
      goptCount = 0
      IF ( delX(1)   .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( dxSpacing .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( delXFile  .NE. ' ' )      goptCount = goptCount + 1
      IF ( goptCount.GT.1 ) THEN
       WRITE(msgBuf,'(A,A)') 'Too many specifications for delX:',
     &   'Specify only one of delX, dxSpacing or delXfile'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
      IF ( dxSpacing .NE. UNSET_RL ) THEN
       DO i=1,Nx
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
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
      IF ( dySpacing .NE. UNSET_RL ) THEN
       DO j=1,Ny
        delY(j) = dySpacing
       ENDDO
      ENDIF
C
      IF ( rSphere .NE. 0. ) THEN
       recip_rSphere = 1. _d 0/rSphere
      ELSE
       recip_rSphere = 0.
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
       STOP 'ABNORMAL END: S/R INI_PARMS'
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
C--   Make metric term & Coriolis settings consistent with underlying grid.
      IF ( usingCartesianGrid ) THEN
       metricTerms   = .FALSE.
       useNHMTerms   = .FALSE.
       useBetaPlaneF = .TRUE.
      ENDIF
      IF ( usingCylindricalGrid ) THEN
       useNHMTerms   = .FALSE.
       useBetaPlaneF = .TRUE.
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; Cylinder OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( usingCurvilinearGrid ) THEN
       metricTerms   = .FALSE.
      ENDIF
      IF ( useConstantF ) useBetaPlaneF = .FALSE.
      IF ( useConstantF )  useSphereF   = .FALSE.
      IF ( useBetaPlaneF ) useSphereF   = .FALSE.
      IF ( usingCartesianGrid .OR. usingCylindricalGrid
     &     .OR. .NOT.(nonHydrostatic.OR.quasiHydrostatic) )
     &                    use3dCoriolis = .FALSE.

C--   Grid rotation
      IF ( phiEuler .NE. 0. _d 0 .OR. thetaEuler .NE. 0. _d 0
     &     .OR. psiEuler .NE. 0. _d 0 ) rotateGrid = .TRUE.

C--   Set default for latitude-band where relaxation to climatology applies
C     note: done later (once domain size is known) if using CartesianGrid
      IF ( latBandClimRelax .EQ. UNSET_RL) THEN
        IF ( usingSphericalPolarGrid ) latBandClimRelax= 180. _d 0
        IF ( usingCurvilinearGrid )    latBandClimRelax= 180. _d 0
      ENDIF
C--   set cell Center depth and put Interface at the middle between 2 C
      setCenterDr = .FALSE.
      DO K=1,Nr+1
       IF ( delRc(K).EQ.UNSET_RL ) THEN
        IF ( setCenterDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delRc at K =', K
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_PARMS'
        ENDIF
       ELSE
        IF ( k.EQ.1 ) setCenterDr = .TRUE.
        IF ( .NOT.setCenterDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delRc at K <', K
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_PARMS'
        ENDIF
       ENDIF
      ENDDO
      IF ( setCenterDr ) rCoordInputData = .TRUE.
C--   p, z, r coord parameters
      setInterFDr = .FALSE.
      DO K = 1, Nr
       IF ( delZ(K) .NE. UNSET_RL ) zCoordInputData = .TRUE.
       IF ( delP(K) .NE. UNSET_RL ) pCoordInputData = .TRUE.
       IF ( delR(K) .NE. UNSET_RL ) rCoordInputData = .TRUE.
       IF ( delR(K) .EQ. UNSET_RL ) delR(K) = delZ(K)
       IF ( delR(K) .EQ. UNSET_RL ) delR(K) = delP(K)
       IF ( delR(K) .EQ. UNSET_RL ) THEN
        IF ( setInterFDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delZ/delP/delR at K =', K
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_PARMS'
        ENDIF
       ELSE
        IF ( k.EQ.1 ) setInterFDr = .TRUE.
        IF ( .NOT.setInterFDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delZ/delP/delR at K <', K
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_PARMS'
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
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
C-    Check for double definition (file & namelist)
      IF ( delRcFile.NE.' ' ) THEN
        IF ( setCenterDr ) THEN
          WRITE(msgBuf,'(A)')
     &         'S/R INI_PARMS: Cannot set both delRc and delRcFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R INI_PARMS'
        ENDIF
        setCenterDr = .TRUE.
      ENDIF
      IF ( delRFile.NE.' ' ) THEN
        IF ( setInterFDr ) THEN
          WRITE(msgBuf,'(A)')
     &         'S/R INI_PARMS: Cannot set both delR and delRFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R INI_PARMS'
        ENDIF
        setInterFDr = .TRUE.
      ENDIF
c     IF ( setInterFDr .AND. setCenterDr ) THEN
c       WRITE(msgBuf,'(2A)') 'S/R INI_PARMS:',
c    &    ' Cannot specify both delRc and delZ/delP/delR'
c       CALL PRINT_ERROR( msgBuf, myThid )
c       STOP 'ABNORMAL END: S/R INI_PARMS'
c     ENDIF

C--   Input files
      WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; starts to read PARM05'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM05) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM05'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; read PARM05 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

C--   Set Units conversion factor required to incorporate
C     surface forcing into z-p isomorphic equations:
C     mass2rUnit: from mass per unit area [kg/m2] to r-coordinate (z:=1/rho;p:=g)
C     rUnit2mass: from r-coordinate to mass per unit area [kg/m2] (z:=rho;p:=1/g)
      IF ( usingPCoords ) THEN
         mass2rUnit = gravity
         rUnit2mass = recip_gravity
      ELSE
         mass2rUnit = recip_rhoConst
         rUnit2mass = rhoConst
      ENDIF

c-- gradually replacing debugMode by debugLevel
      IF ( debugMode ) debugLevel = debLevB
      IF ( debugLevel .GE. debLevB ) debugMode = .TRUE.

c-- flag for approximate adjoint
      IF ( inAdExact ) THEN
       inAdTrue  = .FALSE.
       inAdFALSE = .FALSE.
      ELSE
       inAdTrue  = .TRUE.
       inAdFALSE = .FALSE.
      ENDIF
C
      CLOSE(iUnit)

C--   Check whether any retired parameters were found.
C--   Stop if they were
      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'Error reading parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'some out of date parameters were found in the namelist'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER
C
      RETURN
      END
