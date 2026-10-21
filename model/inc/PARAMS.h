!BOP
! !ROUTINE: PARAMS.h
! !INTERFACE:
! #include PARAMS.h

! !DESCRIPTION:
! Header file defining model "parameters".  The values from the
! model standard input file are stored into the variables held
! here. Notes describing the parameters can also be found here.

!EOP

!--   Contants
! Useful physical values
      Real(kind=8) :: PI
      PARAMETER ( PI    = 3.14159265358979323844D0   )
      Real(kind=8) :: deg2rad
      PARAMETER ( deg2rad = 2.D0*PI/360.D0           )

!--   COMMON /PARM_C/ Character valued parameters used by the model.
! buoyancyRelation :: Flag used to indicate which relation to use to
!                     get buoyancy.
! eosType         :: choose the equation of state:
!                    LINEAR, POLY3, UNESCO, JMD95Z, JMD95P, MDJWF, IDEALGAS
! pickupSuff      :: force to start from pickup files (even if nIter0=0)
!                    and read pickup files with this suffix (max 10 Char.)
! mdsioLocalDir   :: read-write tiled file from/to this directory name
!                    (+ 4 digits Processor-Rank) instead of current dir.
! adTapeDir       :: read-write checkpointing tape files from/to this
!                    directory name instead of current dir. Conflicts
!                    mdsioLocalDir, so only one of the two can be set.
! tRefFile      :: File containing reference Potential Temperat.  tRef (1.D)
! sRefFile      :: File containing reference salinity/spec.humid. sRef (1.D)
! rhoRefFile    :: File containing reference density profile rhoRef (1.D)
! gravityFile   :: File containing gravity vertical profile (1.D)
! delRFile      :: File containing vertical grid spacing delR  (1.D array)
! delRcFile     :: File containing vertical grid spacing delRc (1.D array)
! hybSigmFile   :: File containing hybrid-sigma vertical coord. coeff. (2x 1.D)
! delXFile      :: File containing X-spacing grid definition (1.D array)
! delYFile      :: File containing Y-spacing grid definition (1.D array)
! horizGridFile :: File containing horizontal-grid definition
!                    (only when using curvilinear_grid)
! bathyFile       :: File containing bathymetry. If not defined bathymetry
!                    is taken from inline function.
! topoFile        :: File containing the topography of the surface (unit=m)
!                    (mainly used for the atmosphere = ground height).
! addWwallFile    :: File containing 2-D additional Western  cell-edge wall
! addSwallFile    :: File containing 2-D additional Southern cell-edge wall
!                    (e.g., to add "thin-wall" where it is =1)
! hydrogThetaFile :: File containing initial hydrographic data (3-D)
!                    for potential temperature.
! hydrogSaltFile  :: File containing initial hydrographic data (3-D)
!                    for salinity.
! diffKrFile      :: File containing 3D specification of vertical diffusivity
! viscAhDfile     :: File containing 3D specification of horizontal viscosity
! viscAhZfile     :: File containing 3D specification of horizontal viscosity
! viscA4Dfile     :: File containing 3D specification of horizontal viscosity
! viscA4Zfile     :: File containing 3D specification of horizontal viscosity
! zonalWindFile   :: File containing zonal wind data
! meridWindFile   :: File containing meridional wind data
! thetaClimFile   :: File containing surface theta climataology used
!                    in relaxation term -lambda(theta-theta*)
! saltClimFile    :: File containing surface salt climataology used
!                    in relaxation term -lambda(salt-salt*)
! surfQfile       :: File containing surface heat flux, excluding SW
!                    (old version, kept for backward compatibility)
! surfQnetFile    :: File containing surface net heat flux
! surfQswFile     :: File containing surface shortwave radiation
! EmPmRfile       :: File containing surface fresh water flux
!       NOTE: for backward compatibility EmPmRfile is specified in
!             m/s when using external_fields_load.F.  It is converted
!             to kg/m2/s by multiplying by rhoConstFresh.
! saltFluxFile    :: File containing surface salt flux
! pLoadFile       :: File containing pressure loading
! geoPotAnomFile  :: File containing constant geopotential anomaly due to
!                    density structure
! addMassFile     :: File containing source/sink of fluid in the interior
! eddyPsiXFile    :: File containing zonal Eddy streamfunction data
! eddyPsiYFile    :: File containing meridional Eddy streamfunction data
! geothermalFile  :: File containing geothermal heat flux
! lambdaThetaFile :: File containing SST relaxation coefficient
! lambdaSaltFile  :: File containing SSS relaxation coefficient
! wghtBalanceFile :: File containing weight used in balancing net EmPmR
! the_run_name    :: string identifying the name of the model "run"
      COMMON /PARM_C/                                                             &
     &      buoyancyRelation, eosType,                                            &
     &      pickupSuff, mdsioLocalDir, adTapeDir,                                 &
     &      tRefFile, sRefFile, rhoRefFile, gravityFile,                          &
     &      delRFile, delRcFile, hybSigmFile,                                     &
     &      delXFile, delYFile, horizGridFile,                                    &
     &      bathyFile, topoFile, addWwallFile, addSwallFile,                      &
     &      viscAhDfile, viscAhZfile,                                             &
     &      viscA4Dfile, viscA4Zfile,                                             &
     &      hydrogThetaFile, hydrogSaltFile, diffKrFile,                          &
     &      zonalWindFile, meridWindFile, thetaClimFile,                          &
     &      saltClimFile,                                                         &
     &      EmPmRfile, saltFluxFile,                                              &
     &      surfQfile, surfQnetFile, surfQswFile,                                 &
     &      uVelInitFile, vVelInitFile, pSurfInitFile,                            &
     &      pLoadFile, geoPotAnomFile, addMassFile,                               &
     &      eddyPsiXFile, eddyPsiYFile, geothermalFile,                           &
     &      lambdaThetaFile, lambdaSaltFile, wghtBalanceFile,                     &
     &      the_run_name
      CHARACTER*(MAX_LEN_FNAM) buoyancyRelation
      CHARACTER(len=6) :: eosType
      CHARACTER(len=10) :: pickupSuff
      CHARACTER*(MAX_LEN_FNAM) mdsioLocalDir
      CHARACTER*(MAX_LEN_FNAM) adTapeDir
      CHARACTER*(MAX_LEN_FNAM) tRefFile
      CHARACTER*(MAX_LEN_FNAM) sRefFile
      CHARACTER*(MAX_LEN_FNAM) rhoRefFile
      CHARACTER*(MAX_LEN_FNAM) gravityFile
      CHARACTER*(MAX_LEN_FNAM) delRFile
      CHARACTER*(MAX_LEN_FNAM) delRcFile
      CHARACTER*(MAX_LEN_FNAM) hybSigmFile
      CHARACTER*(MAX_LEN_FNAM) delXFile
      CHARACTER*(MAX_LEN_FNAM) delYFile
      CHARACTER*(MAX_LEN_FNAM) horizGridFile
      CHARACTER*(MAX_LEN_FNAM) bathyFile, topoFile
      CHARACTER*(MAX_LEN_FNAM) addWwallFile, addSwallFile
      CHARACTER*(MAX_LEN_FNAM) hydrogThetaFile, hydrogSaltFile
      CHARACTER*(MAX_LEN_FNAM) diffKrFile
      CHARACTER*(MAX_LEN_FNAM) viscAhDfile
      CHARACTER*(MAX_LEN_FNAM) viscAhZfile
      CHARACTER*(MAX_LEN_FNAM) viscA4Dfile
      CHARACTER*(MAX_LEN_FNAM) viscA4Zfile
      CHARACTER*(MAX_LEN_FNAM) zonalWindFile
      CHARACTER*(MAX_LEN_FNAM) meridWindFile
      CHARACTER*(MAX_LEN_FNAM) thetaClimFile
      CHARACTER*(MAX_LEN_FNAM) saltClimFile
      CHARACTER*(MAX_LEN_FNAM) surfQfile
      CHARACTER*(MAX_LEN_FNAM) surfQnetFile
      CHARACTER*(MAX_LEN_FNAM) surfQswFile
      CHARACTER*(MAX_LEN_FNAM) EmPmRfile
      CHARACTER*(MAX_LEN_FNAM) saltFluxFile
      CHARACTER*(MAX_LEN_FNAM) uVelInitFile
      CHARACTER*(MAX_LEN_FNAM) vVelInitFile
      CHARACTER*(MAX_LEN_FNAM) pSurfInitFile
      CHARACTER*(MAX_LEN_FNAM) pLoadFile
      CHARACTER*(MAX_LEN_FNAM) geoPotAnomFile
      CHARACTER*(MAX_LEN_FNAM) addMassFile
      CHARACTER*(MAX_LEN_FNAM) eddyPsiXFile
      CHARACTER*(MAX_LEN_FNAM) eddyPsiYFile
      CHARACTER*(MAX_LEN_FNAM) geothermalFile
      CHARACTER*(MAX_LEN_FNAM) lambdaThetaFile
      CHARACTER*(MAX_LEN_FNAM) lambdaSaltFile
      CHARACTER*(MAX_LEN_FNAM) wghtBalanceFile
      CHARACTER*(MAX_LEN_PREC/2) the_run_name

!--   COMMON /PARM_I/ Integer valued parameters used by the model.
! cg2dMaxIters        :: Maximum number of iterations in the
!                        two-dimensional con. grad solver.
! cg2dMinItersNSA     :: Minimum number of iterations in the
!                        not-self-adjoint version (cg2d_nsa.F) of the
!                        two-dimensional con. grad solver (default = 0).
! cg2dPreCondFreq     :: Frequency for updating cg2d preconditioner
!                        (non-linear free-surf.)
! cg2dUseMinResSol    :: =0 : use last-iteration/converged solution
!                        =1 : use solver minimum-residual solution
! cg3dMaxIters        :: Maximum number of iterations in the
!                        three-dimensional con. grad solver.
! printResidualFreq   :: Frequency for printing residual in CG iterations
! nIter0              :: Start time-step number of for this run
! nTimeSteps          :: Number of timesteps to execute
! nTimeSteps_l2       :: Number of inner timesteps to execute per timestep
! selectCoriMap       :: select setting of Coriolis parameter map:
!                       =0 f-Plane (Constant Coriolis, = f0)
!                       =1 Beta-Plane Coriolis (= f0 + beta.y)
!                       =2 Spherical Coriolis (= 2.omega.sin(phi))
!                       =3 Read Coriolis 2-d fields from files.
! selectSigmaCoord    :: option related to sigma vertical coordinate
! nonlinFreeSurf      :: option related to non-linear free surface
!                       =0 Linear free surface ; >0 Non-linear
! select_rStar        :: option related to r* vertical coordinate
!                       =0 (default) use r coord. ; > 0 use r*
! selectNHfreeSurf    :: option for Non-Hydrostatic (free-)Surface formulation:
!                       =0 (default) hydrostatic surf. ; > 0 add NH effects.
! selectP_inEOS_Zc    :: select which pressure to use in EOS (for z-coords)
!                       =0: simply: -g*rhoConst*z
!                       =1: use pRef = integral{-g*rho(Tref,Sref,pRef)*dz}
!                       =2: use hydrostatic dynamical pressure
!                       =3: use full (Hyd+NH) dynamical pressure
! selectAddFluid      :: option to add mass source/sink of fluid in the interior
!                        (3-D generalisation of oceanic real-fresh water flux)
!                       =0 off ; =1 add fluid ; =-1 virtual flux (no mass added)
! selectBalanceEmPmR  :: option to balance net surface fresh-water flux:
!                       =0 off ; =1 uniform correction ; = 2 weighted correction
! selectImplicitDrag  :: select Implicit treatment of bottom/top drag
!                       = 0: fully explicit
!                       = 1: implicit on provisional velocity
!                            (i.e., before grad.Eta increment)
!                       = 2: fully implicit (combined with Impl Surf.Press)
! selectPenetratingSW :: select treatment of penetrating shortwave radiation
!                        (requires to define SHORTWAVE_HEATING):
!                       = 0: no shortwave penetration
!                       = 1: constant in time and horizontally uniform
!                            fraction of shortwave penetration (default)
!                       = 2: constant in time, but non-uniform fraction of
!                            shortwave penetration (not yet coded)
!                       > 2: time varying fraction of shortwave penetration
!                            according to external function (e.g. BGC model,
!                            not yet coded)
! momForcingOutAB     :: =1: take momentum forcing contribution
!                        out of (=0: in) Adams-Bashforth time stepping.
! tracForcingOutAB    :: =1: take tracer (Temp,Salt,pTracers) forcing contribution
!                        out of (=0: in) Adams-Bashforth time stepping.
! tempAdvScheme       :: Temp. Horiz.Advection scheme selector
! tempVertAdvScheme   :: Temp. Vert. Advection scheme selector
! saltAdvScheme       :: Salt. Horiz.advection scheme selector
! saltVertAdvScheme   :: Salt. Vert. Advection scheme selector
! selectKEscheme      :: Kinetic Energy scheme selector (Vector Inv.)
! selectVortScheme    :: Scheme selector for Vorticity term (Vector Inv.)
!     selectMetricTerms   :: Scheme selector for Metric terms (Flux-Form)
! selectCoriScheme    :: Scheme selector for Coriolis term
! select3dCoriScheme  :: Scheme selector for 3-D Coriolis (in Omega.cos Phi)
! selectBotDragQuadr  :: quadratic bottom drag discretisation option:
!                       =0: average KE from grid center to U & V location
!                       =1: use local velocity norm @ U & V location
!                       =2: same with wet-point averaging of other component
! pCellMix_select     :: select option to enhance mixing near surface & bottom
!                        unit digit: near bottom ; tens digit: near surface
!                        with digit =0 : disable ;
!                       = 1 : increases mixing linearly with recip_hFac
!                       = 2,3,4 : increases mixing by recip_hFac^(2,3,4)
! readBinaryPrec      :: Precision used for reading binary files
! writeBinaryPrec     :: Precision used for writing binary files
! rwSuffixType        :: controls the format of the mds file suffix.
!                      =0 (default): use iteration number (myIter, I10.10);
!                      =1: 100*myTime (100th sec); =2: myTime (seconds);
!                      =3: myTime/360 (10th of hr); =4: myTime/3600 (hours).
! monitorSelect       :: select group of variables to monitor
!                        =1 : dynvars ; =2 : + vort ; =3 : + surface
!-    debugLevel          :: controls printing of algorithm intermediate results
!                        and statistics ; higher -> more writing
!-    plotLevel           :: controls printing of field maps ; higher -> more flds

      COMMON /PARM_I/                                                             &
     &      cg2dMaxIters, cg2dMinItersNSA,                                        &
     &      cg2dPreCondFreq, cg2dUseMinResSol,                                    &
     &      cg3dMaxIters, printResidualFreq,                                      &
     &      nIter0, nTimeSteps, nTimeSteps_l2, nEndIter,                          &
     &      selectCoriMap,                                                        &
     &      selectSigmaCoord,                                                     &
     &      nonlinFreeSurf, select_rStar,                                         &
     &      selectNHfreeSurf, selectP_inEOS_Zc,                                   &
     &      selectAddFluid, selectBalanceEmPmR, selectImplicitDrag,               &
     &      momForcingOutAB, tracForcingOutAB,                                    &
     &      tempAdvScheme, tempVertAdvScheme,                                     &
     &      saltAdvScheme, saltVertAdvScheme,                                     &
     &      selectKEscheme, selectVortScheme, selectMetricTerms,                  &
     &      selectCoriScheme, select3dCoriScheme,                                 &
     &      selectBotDragQuadr, selectPenetratingSW, pCellMix_select,             &
     &      readBinaryPrec, writeBinaryPrec,                                      &
     &      rwSuffixType, monitorSelect, debugLevel, plotLevel
      INTEGER :: cg2dMaxIters
      INTEGER :: cg2dMinItersNSA
      INTEGER :: cg2dPreCondFreq
      INTEGER :: cg2dUseMinResSol
      INTEGER :: cg3dMaxIters
      INTEGER :: printResidualFreq
      INTEGER :: nIter0
      INTEGER :: nTimeSteps
      INTEGER :: nTimeSteps_l2
      INTEGER :: nEndIter
      INTEGER :: selectCoriMap
      INTEGER :: selectSigmaCoord
      INTEGER :: nonlinFreeSurf
      INTEGER :: select_rStar
      INTEGER :: selectNHfreeSurf
      INTEGER :: selectP_inEOS_Zc
      INTEGER :: selectAddFluid
      INTEGER :: selectBalanceEmPmR
      INTEGER :: selectImplicitDrag
      INTEGER :: momForcingOutAB, tracForcingOutAB
      INTEGER :: tempAdvScheme, tempVertAdvScheme
      INTEGER :: saltAdvScheme, saltVertAdvScheme
      INTEGER :: selectKEscheme
      INTEGER :: selectVortScheme
      INTEGER :: selectMetricTerms
      INTEGER :: selectCoriScheme
      INTEGER :: select3dCoriScheme
      INTEGER :: selectBotDragQuadr
      INTEGER :: selectPenetratingSW
      INTEGER :: pCellMix_select
      INTEGER :: readBinaryPrec
      INTEGER :: writeBinaryPrec
      INTEGER :: rwSuffixType
      INTEGER :: monitorSelect
      INTEGER :: debugLevel
      INTEGER :: plotLevel

!--   COMMON /PARM_L/ Logical valued parameters used by the model.
!- Coordinate + Grid params:
! fluidIsAir       :: Set to indicate that the fluid major constituent
!                     is air
! fluidIsWater     :: Set to indicate that the fluid major constituent
!                     is water
! usingPCoords     :: Set to indicate that we are working in a pressure
!                     type coordinate (p or p*).
! usingZCoords     :: Set to indicate that we are working in a height
!                     type coordinate (z or z*)
! usingCartesianGrid :: If TRUE grid generation will be in a cartesian
!                       coordinate frame.
! usingSphericalPolarGrid :: If TRUE grid generation will be in a
!                            spherical polar frame.
! rotateGrid      :: rotate grid coordinates to geographical coordinates
!                    according to Euler angles phiEuler, thetaEuler, psiEuler
! usingCylindricalGrid :: If TRUE grid generation will be Cylindrical
! usingCurvilinearGrid :: If TRUE, use a curvilinear grid (to be provided)
! hasWetCSCorners :: domain contains CS-type corners where dynamics is solved
! deepAtmosphere :: deep model (drop the shallow-atmosphere approximation)
! setInterFDr    :: set Interface depth (put cell-Center at the middle)
! setCenterDr    :: set cell-Center depth (put Interface at the middle)
! useMin4hFacEdges :: set hFacW,hFacS as minimum of adjacent hFacC factor
! interViscAr_pCell :: account for partial-cell in interior vert. viscosity
! interDiffKr_pCell :: account for partial-cell in interior vert. diffusion
!- Momentum params:
! no_slip_sides  :: Impose "no-slip" at lateral boundaries.
! no_slip_bottom :: Impose "no-slip" at bottom boundary.
! bottomVisc_pCell :: account for partial-cell in bottom visc. (no-slip BC)
! useSmag3D      :: Use isotropic 3-D Smagorinsky
! useFullLeith   :: Set to true to use full Leith viscosity(may be unstable
!                   on irregular grids)
! useStrainTensionVisc:: Set to true to use Strain-Tension viscous terms
! useAreaViscLength :: Set to true to use old scaling for viscous lengths,
!                      e.g., L2=Raz.  May be preferable for cube sphere.
! momViscosity  :: Flag which turns momentum friction terms on and off.
! momAdvection  :: Flag which turns advection of momentum on and off.
! momForcing    :: Flag which turns external forcing of momentum on and off.
! momTidalForcing    :: Flag which turns tidal forcing on and off.
! momPressureForcing :: Flag which turns pressure term in momentum equation
!                      on and off.
! useNHMTerms   :: If TRUE use non-hydrostatic metric terms.
! useCoriolis   :: Flag which turns the coriolis terms on and off.
! useCDscheme   :: use CD-scheme to calculate Coriolis terms.
! vectorInvariantMomentum :: use Vector-Invariant form (mom_vecinv package)
!                            (default = F = use mom_fluxform package)
! useJamartMomAdv :: Use wet-point method for V.I. non-linear term
! upwindVorticity :: bias interpolation of vorticity in the Coriolis term
! highOrderVorticity :: use 3rd/4th order interp. of vorticity (V.I., advection)
! useAbsVorticity :: work with f+zeta in Coriolis terms
! upwindShear     :: use 1rst order upwind interp. (V.I., vertical advection)
! momStepping    :: Turns momentum equation time-stepping off
! calc_wVelocity :: Turns vertical velocity calculation off
!- Temp. & Salt params:
! tempStepping   :: Turns temperature equation time-stepping on/off
! saltStepping   :: Turns salinity equation time-stepping on/off
! addFrictionHeating :: account for frictional heating
! temp_stayPositive :: use Smolarkiewicz Hack to ensure Temp stays positive
! salt_stayPositive :: use Smolarkiewicz Hack to ensure Salt stays positive
! tempAdvection  :: Flag which turns advection of temperature on and off.
! tempVertDiff4  :: use vertical bi-harmonic diffusion for temperature
! tempIsActiveTr :: Pot.Temp. is a dynamically active tracer
! tempForcing    :: Flag which turns external forcing of temperature on/off
! saltAdvection  :: Flag which turns advection of salinity on and off.
! saltVertDiff4  :: use vertical bi-harmonic diffusion for salinity
! saltIsActiveTr :: Salinity  is a dynamically active tracer
! saltForcing    :: Flag which turns external forcing of salinity on/off
! maskIniTemp    :: apply mask to initial Pot.Temp.
! maskIniSalt    :: apply mask to initial salinity
! checkIniTemp   :: check for points with identically zero initial Pot.Temp.
! checkIniSalt   :: check for points with identically zero initial salinity
!- Pressure solver related parameters (PARM02)
! useNSACGSolver :: Set to true to use "not self-adjoint" conjugate
!                   gradient solver that stores the iteration history
!                   for an iterative adjoint as accuate as possible
! useSRCGSolver  :: Set to true to use conjugate gradient
!                   solver with single reduction (only one call of
!                   s/r mpi_allreduce), default is false
!- Time-stepping & free-surface params:
! rigidLid            :: Set to true to use rigid lid
! implicitFreeSurface :: Set to true to use implicit free surface
! uniformLin_PhiSurf  :: Set to true to use a uniform Bo_surf in the
!                        linear relation Phi_surf = Bo_surf*eta
! uniformFreeSurfLev  :: TRUE if free-surface level-index is uniform (=1)
! exactConserv        :: Set to true to conserve exactly the total Volume
! linFSConserveTr     :: Set to true to correct source/sink of tracer
!                        at the surface due to Linear Free Surface
! useRealFreshWaterFlux :: if True (=Natural BCS), treats P+R-E flux
!                     as a real Fresh Water (=> changes the Sea Level)
!                     if F, converts P+R-E to salt flux (no SL effect)
! storePhiHyd4Phys :: store hydrostatic potential for use in Physics/EOS
!                     this requires specific code for restart & exchange
! quasiHydrostatic :: Using non-hydrostatic terms in hydrostatic algorithm
! nonHydrostatic   :: Using non-hydrostatic algorithm
! use3Dsolver      :: set to true to use 3-D pressure solver
! implicitIntGravWave :: treat Internal Gravity Wave implicitly
! staggerTimeStep   :: enable a Stagger time stepping U,V (& W) then T,S
! applyExchUV_early :: Apply EXCH to U,V earlier, just before integr_continuity
! doResetHFactors   :: Do reset thickness factors @ beginning of each time-step
! implicitDiffusion :: Turns implicit vertical diffusion on
! implicitViscosity :: Turns implicit vertical viscosity on
! tempImplVertAdv   :: Turns on implicit vertical advection for Temperature
! saltImplVertAdv   :: Turns on implicit vertical advection for Salinity
! momImplVertAdv    :: Turns on implicit vertical advection for Momentum
! multiDimAdvection :: Flag that enable multi-dimension advection
! useMultiDimAdvec  :: True if multi-dim advection is used at least once
! momDissip_In_AB   :: if False, put Dissipation tendency contribution
!                      out off Adams-Bashforth time stepping.
! doAB_onGtGs       :: if the Adams-Bashforth time stepping is used, always
!                      apply AB on tracer tendencies (rather than on Tracer)
!- Other forcing params -
! balanceQnet     :: substract global mean of Qnet at every time step
! balancePrintMean:: print substracted global means to STDOUT
! doThetaClimRelax :: Set true if relaxation to temperature
!                    climatology is required.
! doSaltClimRelax  :: Set true if relaxation to salinity
!                    climatology is required.
! balanceThetaClimRelax :: substract global mean effect at every time step
! balanceSaltClimRelax :: substract global mean effect at every time step
! allowFreezing  :: Allows surface water to freeze and form ice
! periodicExternalForcing :: Set true if forcing is time-dependant
!- I/O parameters -
! globalFiles    :: Selects between "global" and "tiled" files.
!                   On some platforms with MPI, option globalFiles is either
!                   slow or does not work. Use useSingleCpuIO instead.
! useSingleCpuIO :: moved to EEPARAMS.h
! pickupStrictlyMatch :: check and stop if pickup-file do not stricly match
! startFromPickupAB2 :: with AB-3 code, start from an AB-2 pickup
! usePickupBeforeC54 :: start from old-pickup files, generated with code from
!                       before checkpoint-54a, Jul 06, 2004.
! pickup_write_mdsio :: use mdsio to write pickups
! pickup_read_mdsio  :: use mdsio to read  pickups
! pickup_write_immed :: echo the pickup immediately (for conversion)
! writePickupAtEnd   :: write pickup at the last timestep
! snapshot_mdsio     :: use mdsio for "snapshot" (dumpfreq/diagfreq) output
! monitor_stdio      :: use stdio for monitor output
! dumpInitAndLast :: dumps model state to files at Initial (nIter0)
!                    & Last iteration, in addition multiple of dumpFreq iter.

      COMMON /PARM_L/                                                             &
     &      fluidIsAir, fluidIsWater,                                             &
     &      usingPCoords, usingZCoords,                                           &
     &      usingCartesianGrid, usingSphericalPolarGrid, rotateGrid,              &
     &      usingCylindricalGrid, usingCurvilinearGrid, hasWetCSCorners,          &
     &      deepAtmosphere, setInterFDr, setCenterDr, useMin4hFacEdges,           &
     &      interViscAr_pCell, interDiffKr_pCell,                                 &
     &      no_slip_sides, no_slip_bottom, bottomVisc_pCell, useSmag3D,           &
     &      useFullLeith, useStrainTensionVisc, useAreaViscLength,                &
     &      momViscosity, momAdvection, momForcing, momTidalForcing,              &
     &      momPressureForcing, useNHMTerms,                                      &
     &      useCoriolis, useCDscheme, vectorInvariantMomentum,                    &
     &      useJamartMomAdv, upwindVorticity, highOrderVorticity,                 &
     &      useAbsVorticity, upwindShear,                                         &
     &      momStepping, calc_wVelocity, tempStepping, saltStepping,              &
     &      addFrictionHeating, temp_stayPositive, salt_stayPositive,             &
     &      tempAdvection, tempVertDiff4, tempIsActiveTr, tempForcing,            &
     &      saltAdvection, saltVertDiff4, saltIsActiveTr, saltForcing,            &
     &      maskIniTemp, maskIniSalt, checkIniTemp, checkIniSalt,                 &
     &      useNSACGSolver, useSRCGSolver,                                        &
     &      rigidLid, implicitFreeSurface,                                        &
     &      uniformLin_PhiSurf, uniformFreeSurfLev,                               &
     &      exactConserv, linFSConserveTr, useRealFreshWaterFlux,                 &
     &      storePhiHyd4Phys, quasiHydrostatic, nonHydrostatic,                   &
     &      use3Dsolver, implicitIntGravWave, staggerTimeStep,                    &
     &      applyExchUV_early, doResetHFactors,                                   &
     &      implicitDiffusion, implicitViscosity,                                 &
     &      tempImplVertAdv, saltImplVertAdv, momImplVertAdv,                     &
     &      multiDimAdvection, useMultiDimAdvec,                                  &
     &      momDissip_In_AB, doAB_onGtGs,                                         &
     &      balanceQnet, balancePrintMean,                                        &
     &      balanceThetaClimRelax, balanceSaltClimRelax,                          &
     &      doThetaClimRelax, doSaltClimRelax,                                    &
     &      allowFreezing,                                                        &
     &      periodicExternalForcing,                                              &
     &      globalFiles,                                                          &
     &      pickupStrictlyMatch, usePickupBeforeC54, startFromPickupAB2,          &
     &      pickup_read_mdsio, pickup_write_mdsio, pickup_write_immed,            &
     &      writePickupAtEnd,                                                     &
     &      snapshot_mdsio, monitor_stdio,                                        &
     &      outputTypesInclusive, dumpInitAndLast

      LOGICAL :: fluidIsAir
      LOGICAL :: fluidIsWater
      LOGICAL :: usingPCoords
      LOGICAL :: usingZCoords
      LOGICAL :: usingCartesianGrid
      LOGICAL :: usingSphericalPolarGrid, rotateGrid
      LOGICAL :: usingCylindricalGrid
      LOGICAL :: usingCurvilinearGrid, hasWetCSCorners
      LOGICAL :: deepAtmosphere
      LOGICAL :: setInterFDr
      LOGICAL :: setCenterDr
      LOGICAL :: useMin4hFacEdges
      LOGICAL :: interViscAr_pCell
      LOGICAL :: interDiffKr_pCell

      LOGICAL :: no_slip_sides
      LOGICAL :: no_slip_bottom
      LOGICAL :: bottomVisc_pCell
      LOGICAL :: useSmag3D
      LOGICAL :: useFullLeith
      LOGICAL :: useStrainTensionVisc
      LOGICAL :: useAreaViscLength
      LOGICAL :: momViscosity
      LOGICAL :: momAdvection
      LOGICAL :: momForcing
      LOGICAL :: momTidalForcing
      LOGICAL :: momPressureForcing
      LOGICAL :: useNHMTerms

      LOGICAL :: useCoriolis
      LOGICAL :: useCDscheme
      LOGICAL :: vectorInvariantMomentum
      LOGICAL :: useJamartMomAdv
      LOGICAL :: upwindVorticity
      LOGICAL :: highOrderVorticity
      LOGICAL :: useAbsVorticity
      LOGICAL :: upwindShear
      LOGICAL :: momStepping
      LOGICAL :: calc_wVelocity
      LOGICAL :: tempStepping
      LOGICAL :: saltStepping
      LOGICAL :: addFrictionHeating
      LOGICAL :: temp_stayPositive
      LOGICAL :: salt_stayPositive
      LOGICAL :: tempAdvection
      LOGICAL :: tempVertDiff4
      LOGICAL :: tempIsActiveTr
      LOGICAL :: tempForcing
      LOGICAL :: saltAdvection
      LOGICAL :: saltVertDiff4
      LOGICAL :: saltIsActiveTr
      LOGICAL :: saltForcing
      LOGICAL :: maskIniTemp
      LOGICAL :: maskIniSalt
      LOGICAL :: checkIniTemp
      LOGICAL :: checkIniSalt
      LOGICAL :: useNSACGSolver
      LOGICAL :: useSRCGSolver
      LOGICAL :: rigidLid
      LOGICAL :: implicitFreeSurface
      LOGICAL :: uniformLin_PhiSurf
      LOGICAL :: uniformFreeSurfLev
      LOGICAL :: exactConserv
      LOGICAL :: linFSConserveTr
      LOGICAL :: useRealFreshWaterFlux
      LOGICAL :: storePhiHyd4Phys
      LOGICAL :: quasiHydrostatic
      LOGICAL :: nonHydrostatic
      LOGICAL :: use3Dsolver
      LOGICAL :: implicitIntGravWave
      LOGICAL :: staggerTimeStep
      LOGICAL :: applyExchUV_early
      LOGICAL :: doResetHFactors
      LOGICAL :: implicitDiffusion
      LOGICAL :: implicitViscosity
      LOGICAL :: tempImplVertAdv
      LOGICAL :: saltImplVertAdv
      LOGICAL :: momImplVertAdv
      LOGICAL :: multiDimAdvection
      LOGICAL :: useMultiDimAdvec
      LOGICAL :: momDissip_In_AB
      LOGICAL :: doAB_onGtGs
      LOGICAL :: balanceQnet
      LOGICAL :: balancePrintMean
      LOGICAL :: doThetaClimRelax
      LOGICAL :: doSaltClimRelax
      LOGICAL :: balanceThetaClimRelax
      LOGICAL :: balanceSaltClimRelax
      LOGICAL :: allowFreezing
      LOGICAL :: periodicExternalForcing
      LOGICAL :: globalFiles
      LOGICAL :: pickupStrictlyMatch
      LOGICAL :: usePickupBeforeC54
      LOGICAL :: startFromPickupAB2
      LOGICAL :: pickup_read_mdsio, pickup_write_mdsio
      LOGICAL :: pickup_write_immed, writePickupAtEnd
      LOGICAL :: snapshot_mdsio, monitor_stdio
      LOGICAL :: outputTypesInclusive
      LOGICAL :: dumpInitAndLast

!--   COMMON /PARM_R/ "Real" valued parameters used by the model.
! cg2dTargetResidual
!      :: Target residual for cg2d solver ; no unit (RHS normalisation)
! cg2dTargetResWunit
!      :: Target residual for cg2d solver ; W unit (No RHS normalisation)
! cg3dTargetResidual
!      :: Target residual for cg3d solver ; no unit (RHS normalisation)
! cg3dTargetResWunit
!      :: Target residual for cg3d solver ; W unit (No RHS normalisation)
! cg2dpcOffDFac :: Averaging weight for preconditioner off-diagonal.
! Note. 20th May 1998
!       I made a weird discovery! In the model paper we argue
!       for the form of the preconditioner used here ( see
!       A Finite-volume, Incompressible Navier-Stokes Model
!       ...., Marshall et. al ). The algebra gives a simple
!       0.5 factor for the averaging of ac and aCw to get a
!       symmettric pre-conditioner. By using a factor of 0.51
!       i.e. scaling the off-diagonal terms in the
!       preconditioner down slightly I managed to get the
!       number of iterations for convergence in a test case to
!       drop form 192 -> 134! Need to investigate this further!
!       For now I have introduced a parameter cg2dpcOffDFac which
!       defaults to 0.51 but can be set at runtime.
! delR      :: Vertical grid spacing ( units of r ).
! delRc     :: Vertical grid spacing between cell centers (r unit).
! delX      :: Separation between cell faces (m) or (deg), depending
! delY         on input flags. Note: moved to header file SET_GRID.h
! xgOrigin   :: Origin of the X-axis (Cartesian Grid) / Longitude of Western
!            :: most cell face (Lat-Lon grid) (Note: this is an "inert"
!            :: parameter but it makes geographical references simple.)
! ygOrigin   :: Origin of the Y-axis (Cartesian Grid) / Latitude of Southern
!            :: most face (Lat-Lon grid).
! rSphere    :: Radius of sphere for a spherical polar grid ( m ).
! recip_rSphere :: Reciprocal radius of sphere ( m^-1 ).
! radius_fromHorizGrid :: sphere Radius of input horiz. grid (Curvilinear Grid)
! seaLev_Z   :: the reference height of sea-level (usually zero)
! top_Pres   :: pressure (P-Coords) or reference pressure (Z-Coords) at the top
! rSigmaBnd  :: vertical position (in r-unit) of r/sigma transition (Hybrid-Sigma)
! gravity    :: Acceleration due to constant gravity ( m/s^2 )
! recip_gravity :: Reciprocal gravity acceleration ( s^2/m )
! gBaro      :: Accel. due to gravity used in barotropic equation ( m/s^2 )
! gravFacC   :: gravity factor (vs surf. gravity) vert. profile at cell-Center
! gravFacF   :: gravity factor (vs surf. gravity) vert. profile at cell-interF
! rhoNil     :: Reference density for the linear equation of state
! rhoConst   :: Vertically constant reference density (Boussinesq)
! rho1Ref    :: reference vertical profile for density (anelastic)
! rhoFacC    :: normalized (by rhoConst) reference density at cell-Center
! rhoFacF    :: normalized (by rhoConst) reference density at cell-interFace
! rhoConstFresh :: Constant reference density for fresh water (rain)
! thetaConst :: Constant reference for potential temperature
! tRef       :: reference vertical profile for potential temperature
! sRef       :: reference vertical profile for salinity/specific humidity
! rhoRef     :: density vertical profile from (tRef,sRef) [kg/m^3]
! dBdrRef    :: vertical gradient of reference buoyancy  [(m/s/r)^2]:
!            :: z-coord: = N^2_ref = Brunt-Vaissala frequency [s^-2]
!            :: p-coord: = -(d.alpha/dp)_ref          [(m^2.s/kg)^2]
! surf_pRef  :: surface reference pressure ( Pa )
! pRef4EOS   :: reference pressure used in EOS (case selectP_inEOS_Zc=1)
! phiRef     :: reference potential (press/rho, geopot) profile (m^2/s^2)
! rVel2wUnit :: units conversion factor (Non-Hydrostatic code),
!            :: from r-coordinate vertical velocity to vertical velocity [m/s].
!            :: z-coord: = 1 ; p-coord: wSpeed [m/s] = rVel [Pa/s] * rVel2wUnit
! wUnit2rVel :: units conversion factor (Non-Hydrostatic code),
!            :: from vertical velocity [m/s] to r-coordinate vertical velocity.
!            :: z-coord: = 1 ; p-coord: rVel [Pa/s] = wSpeed [m/s] * wUnit2rVel
! rUnit2z    :: units conversion factor (for ocean in P-coord, only fct of k),
!            :: from r-coordinate to z [m] (at level center):
!            :: z-coord: = 1 ; p-coord: dz [m] = dr [Pa] * rUnit2z
! z2rUnit    :: units conversion factor (for ocean in P-coord, only fct of k),
!            :: from z [m] to r-coordinate (at level center):
!            :: z-coord: = 1 ; p-coord: dr [Pa] = dz [m] * z2rUnit
! mass2rUnit :: units conversion factor (surface forcing),
!            :: from mass per unit area [kg/m2] to vertical r-coordinate unit.
!            :: z-coord: = 1/rhoConst ( [kg/m2] / rho = [m] ) ;
!            :: p-coord: = gravity    ( [kg/m2] *  g = [Pa] ) ;
! rUnit2mass :: units conversion factor (surface forcing),
!            :: from vertical r-coordinate unit to mass per unit area [kg/m2].
!            :: z-coord: = rhoConst  ( [m] * rho = [kg/m2] ) ;
!            :: p-coord: = 1/gravity ( [Pa] /  g = [kg/m2] ) ;
! sIceLoadFac:: factor to scale (and turn off) sIceLoad (sea-ice loading)
!               default = 1
! f0         :: Reference coriolis parameter ( 1/s )
!               ( Southern edge f for beta plane )
! beta       :: df/dy ( s^-1.m^-1 )
! fPrime     :: Second Coriolis parameter ( 1/s ), related to Y-component
!               of rotation (reference value = 2.Omega.Cos(Phi))
! omega      :: Angular velocity ( rad/s )
! rotationPeriod :: Rotation period (s) (= 2.pi/omega)
! viscArNr   :: vertical profile of Eddy viscosity coeff.
!               for vertical mixing of momentum ( units of r^2/s )
! viscAh     :: Eddy viscosity coeff. for mixing of
!               momentum laterally ( m^2/s )
! viscAhW    :: Eddy viscosity coeff. for mixing of vertical
!               momentum laterally, no effect for hydrostatic
!               model, defaults to viscAhD if unset ( m^2/s )
!               Not used if variable horiz. viscosity is used.
! viscA4     :: Biharmonic viscosity coeff. for mixing of
!               momentum laterally ( m^4/s )
! viscA4W    :: Biharmonic viscosity coeff. for mixing of vertical
!               momentum laterally, no effect for hydrostatic
!               model, defaults to viscA4D if unset ( m^2/s )
!               Not used if variable horiz. viscosity is used.
! viscAhD    :: Eddy viscosity coeff. for mixing of momentum laterally
!               (act on Divergence part) ( m^2/s )
! viscAhZ    :: Eddy viscosity coeff. for mixing of momentum laterally
!               (act on Vorticity  part) ( m^2/s )
! viscA4D    :: Biharmonic viscosity coeff. for mixing of momentum laterally
!               (act on Divergence part) ( m^4/s )
! viscA4Z    :: Biharmonic viscosity coeff. for mixing of momentum laterally
!               (act on Vorticity  part) ( m^4/s )
! smag3D_coeff     :: Isotropic 3-D Smagorinsky viscosity coefficient (-)
! smag3D_diffCoeff :: Isotropic 3-D Smagorinsky diffusivity coefficient (-)
! viscC2leith  :: Leith non-dimensional viscosity factor (grad(vort))
! viscC2leithD :: Modified Leith non-dimensional visc. factor (grad(div))
! viscC2LeithQG:: QG Leith non-dimensional viscosity factor
! viscC4leith  :: Leith non-dimensional viscosity factor (grad(vort))
! viscC4leithD :: Modified Leith non-dimensional viscosity factor (grad(div))
!     viscC2smag   :: Smagorinsky non-dimensional viscosity factor (harmonic)
! viscC4smag   :: Smagorinsky non-dimensional viscosity factor (biharmonic)
! viscAhMax    :: Maximum eddy viscosity coeff. for mixing of
!                momentum laterally ( m^2/s )
! viscAhReMax  :: Maximum gridscale Reynolds number for eddy viscosity
!                 coeff. for mixing of momentum laterally (non-dim)
! viscAhGrid   :: non-dimensional grid-size dependent viscosity
! viscAhGridMax:: maximum and minimum harmonic viscosity coefficients ...
! viscAhGridMin::  in terms of non-dimensional grid-size dependent visc.
! viscA4Max    :: Maximum biharmonic viscosity coeff. for mixing of
!                 momentum laterally ( m^4/s )
! viscA4ReMax  :: Maximum Gridscale Reynolds number for
!                 biharmonic viscosity coeff. momentum laterally (non-dim)
! viscA4Grid   :: non-dimensional grid-size dependent bi-harmonic viscosity
! viscA4GridMax:: maximum and minimum biharmonic viscosity coefficients ...
! viscA4GridMin::  in terms of non-dimensional grid-size dependent viscosity
! diffKhT   :: Laplacian diffusion coeff. for mixing of
!             heat laterally ( m^2/s )
! diffK4T   :: Biharmonic diffusion coeff. for mixing of
!             heat laterally ( m^4/s )
! diffKrNrT :: vertical profile of Laplacian diffusion coeff.
!             for mixing of heat vertically ( units of r^2/s )
! diffKr4T  :: vertical profile of Biharmonic diffusion coeff.
!             for mixing of heat vertically ( units of r^4/s )
! diffKhS  ::  Laplacian diffusion coeff. for mixing of
!             salt laterally ( m^2/s )
! diffK4S   :: Biharmonic diffusion coeff. for mixing of
!             salt laterally ( m^4/s )
! diffKrNrS :: vertical profile of Laplacian diffusion coeff.
!             for mixing of salt vertically ( units of r^2/s ),
! diffKr4S  :: vertical profile of Biharmonic diffusion coeff.
!             for mixing of salt vertically ( units of r^4/s )
! diffKrBL79surf :: T/S surface diffusivity (m^2/s) Bryan and Lewis, 1979
! diffKrBL79deep :: T/S deep diffusivity (m^2/s) Bryan and Lewis, 1979
! diffKrBL79scl  :: depth scale for arctan fn (m) Bryan and Lewis, 1979
! diffKrBL79Ho   :: depth offset for arctan fn (m) Bryan and Lewis, 1979
! BL79LatVary    :: polarwise of this latitude diffKrBL79 is applied with
!                   gradual transition to diffKrBLEQ towards Equator
! diffKrBLEQsurf :: same as diffKrBL79surf but at Equator
! diffKrBLEQdeep :: same as diffKrBL79deep but at Equator
! diffKrBLEQscl  :: same as diffKrBL79scl but at Equator
! diffKrBLEQHo   :: same as diffKrBL79Ho but at Equator
! pCellMix_maxFac :: maximum enhanced mixing factor for thin partial-cell
! pCellMix_delR   :: thickness criteria   for too thin partial-cell
! pCellMix_viscAr :: vertical viscosity   for too thin partial-cell
! pCellMix_diffKr :: vertical diffusivity for too thin partial-cell
! deltaT    :: Default timestep ( s )
! deltaTClock  :: Timestep used as model "clock". This determines the
!                IO frequencies and is used in tagging output. It can
!                be totally different to the dynamical time. Typically
!                it will be the deep-water timestep for accelerated runs.
!                Frequency of checkpointing and dumping of the model state
!                are referenced to this clock. ( s )
! deltaTMom    :: Timestep for momemtum equations ( s )
! dTtracerLev  :: Timestep for tracer equations ( s ), function of level k
! deltaTFreeSurf :: Timestep for free-surface equation ( s )
! freeSurfFac  :: Parameter to turn implicit free surface term on or off
!                 freeSurFac = 1. uses implicit free surface
!                 freeSurFac = 0. uses rigid lid
! abEps        :: Adams-Bashforth-2 stabilizing weight
! alph_AB      :: Adams-Bashforth-3 primary factor
! beta_AB      :: Adams-Bashforth-3 secondary factor
! implicSurfPress :: parameter of the Crank-Nickelson time stepping :
!                 Implicit part of Surface Pressure Gradient ( 0-1 )
! implicDiv2DFlow :: parameter of the Crank-Nickelson time stepping :
!                 Implicit part of barotropic flow Divergence ( 0-1 )
! implicitNHPress :: parameter of the Crank-Nickelson time stepping :
!                 Implicit part of Non-Hydrostatic Pressure Gradient ( 0-1 )
!     hFacMin      :: Minimum fraction size of a cell (affects hFacC etc...)
! hFacMinDz    :: Minimum dimensional size of a cell (affects hFacC etc..., m)
! hFacMinDp    :: Minimum dimensional size of a cell (affects hFacC etc..., Pa)
! hFacMinDr    :: Minimum dimensional size of a cell (-> hFacC etc..., r units)
! hFacInf      :: Threshold (inf and sup) for fraction size of surface cell
! hFacSup          that control vanishing and creating levels
! tauCD         :: CD scheme coupling timescale ( s )
! rCD           :: CD scheme normalised coupling parameter (= 1 - deltaT/tauCD)
! epsAB_CD      :: Adams-Bashforth-2 stabilizing weight used in CD scheme
! baseTime      :: model base time (time origin) = time @ iteration zero
! startTime     :: Starting time for this integration ( s ).
! endTime       :: Ending time for this integration ( s ).
! chkPtFreq     :: Frequency of rolling check pointing ( s ).
! pChkPtFreq    :: Frequency of permanent check pointing ( s ).
! dumpFreq      :: Frequency with which model state is written to
!                  post-processing files ( s ).
! diagFreq      :: Frequency with which model writes diagnostic output
!                  of intermediate quantities.
! afFacMom      :: Advection of momentum term multiplication factor
! vfFacMom      :: Momentum viscosity term    multiplication factor
! pfFacMom      :: Momentum pressure forcing  multiplication factor
! cfFacMom      :: Coriolis term              multiplication factor
! foFacMom      :: Momentum forcing           multiplication factor
! mtFacMom      :: Metric terms               multiplication factor
! cosPower      :: Power of cosine of latitude to multiply viscosity
! cAdjFreq      :: Frequency of convective adjustment
!
! tauThetaClimRelax :: Relaxation to climatology time scale ( s ).
! tauSaltClimRelax :: Relaxation to climatology time scale ( s ).
! latBandClimRelax :: latitude band where Relaxation to Clim. is applied,
!                     i.e. where |yC| <= latBandClimRelax
! externForcingPeriod :: Is the period of which forcing varies (eg. 1 month)
! externForcingCycle :: Is the repeat time of the forcing (eg. 1 year)
!                      (note: externForcingCycle must be an integer
!                       number times externForcingPeriod)
! convertFW2Salt :: salinity, used to convert Fresh-Water Flux to Salt Flux
!                   (use model surface (local) value if set to -1)
! temp_EvPrRn :: temperature of Rain & Evap.
! salt_EvPrRn :: salinity of Rain & Evap.
! temp_addMass :: temperature of addMass field
! salt_addMass :: salinity of addMass field
!    (notes: a) tracer content of Rain/Evap only used if both
!                 NonLin_FrSurf & useRealFreshWater are set.
!            b) use model surface (local) value if set to UNSET_RL)
! hMixCriteria:: criteria for mixed-layer diagnostic
! dRhoSmall   :: parameter for mixed-layer diagnostic
! hMixSmooth  :: Smoothing parameter for mixed-layer diag
!                (default=0: no smoothing)
! ivdc_kappa  :: implicit vertical diffusivity for convection [m^2/s]
! sideDragFactor     :: side-drag scaling factor (used only if no_slip_sides)
!                       (default=2: full drag ; =1: gives half-slip BC)
! bottomDragLinear    :: Linear    bottom-drag coefficient (units of [r]/s)
! bottomDragQuadratic :: Quadratic bottom-drag coefficient (units of [r]/m)
!           (if using zcoordinate, units becomes linear: m/s, quadratic: [-])
! zRoughBot :: roughness length for quadratic bottom friction coefficient
!              (in m, typical values are order 0.01 m)
! smoothAbsFuncRange :: 1/2 of interval around zero, for which FORTRAN ABS
!                       is to be replace by a smoother function
!                       (affects myabs, mymin, mymax)
! nh_Am2        :: scales non-hydrostatic terms and changes internal scales
!                  (i.e. allows convection at different Rayleigh numbers)
! tCylIn        :: Temperature of the cylinder inner boundary
! tCylOut       :: Temperature of the cylinder outer boundary
! phiEuler      :: Euler angle, rotation about original z-axis
! thetaEuler    :: Euler angle, rotation about new x-axis
! psiEuler      :: Euler angle, rotation about new z-axis
      COMMON /PARM_R/ cg2dTargetResidual, cg2dTargetResWunit,                     &
     &      cg2dpcOffDFac, cg3dTargetResidual, cg3dTargetResWunit,                &
     &      delR, delRc, xgOrigin, ygOrigin, rSphere, recip_rSphere,              &
     &      radius_fromHorizGrid, seaLev_Z, top_Pres, rSigmaBnd,                  &
     &      deltaT, deltaTMom, dTtracerLev, deltaTFreeSurf, deltaTClock,          &
     &      abEps, alph_AB, beta_AB,                                              &
     &      f0, beta, fPrime, omega, rotationPeriod,                              &
     &      viscFacAdj, viscAh, viscAhW, smag3D_coeff, smag3D_diffCoeff,          &
     &      viscAhMax, viscAhGrid, viscAhGridMax, viscAhGridMin,                  &
     &      viscC2leith, viscC2leithD, viscC2LeithQG,                             &
     &      viscC2smag, viscC4smag,                                               &
     &      viscAhD, viscAhZ, viscA4D, viscA4Z,                                   &
     &      viscA4, viscA4W, viscA4Max,                                           &
     &      viscA4Grid, viscA4GridMax, viscA4GridMin,                             &
     &      viscAhReMax, viscA4ReMax,                                             &
     &      viscC4leith, viscC4leithD, viscArNr,                                  &
     &      diffKhT, diffK4T, diffKrNrT, diffKr4T,                                &
     &      diffKhS, diffK4S, diffKrNrS, diffKr4S,                                &
     &      diffKrBL79surf, diffKrBL79deep, diffKrBL79scl, diffKrBL79Ho,          &
     &      BL79LatVary,                                                          &
     &      diffKrBLEQsurf, diffKrBLEQdeep, diffKrBLEQscl, diffKrBLEQHo,          &
     &      pCellMix_maxFac, pCellMix_delR, pCellMix_viscAr, pCellMix_diffKr,     &
     &      tauCD, rCD, epsAB_CD,                                                 &
     &      freeSurfFac, implicSurfPress, implicDiv2DFlow, implicitNHPress,       &
     &      hFacMin, hFacMinDz, hFacInf, hFacSup,                                 &
     &      gravity, recip_gravity, gBaro,                                        &
     &      gravFacC, recip_gravFacC, gravFacF, recip_gravFacF,                   &
     &      rhoNil, rhoConst, recip_rhoConst, rho1Ref,                            &
     &      rhoFacC, recip_rhoFacC, rhoFacF, recip_rhoFacF, rhoConstFresh,        &
     &      thetaConst, tRef, sRef, rhoRef, dBdrRef,                              &
     &      surf_pRef, pRef4EOS, phiRef,                                          &
     &      rVel2wUnit, wUnit2rVel, rUnit2z, z2rUnit, mass2rUnit, rUnit2mass,     &
     &      baseTime, startTime, endTime,                                         &
     &      chkPtFreq, pChkPtFreq, dumpFreq, adjDumpFreq,                         &
     &      diagFreq, monitorFreq, adjMonitorFreq,                                &
     &      afFacMom, vfFacMom, pfFacMom, cfFacMom, foFacMom, mtFacMom,           &
     &      cosPower, cAdjFreq,                                                   &
     &      tauThetaClimRelax, tauSaltClimRelax, latBandClimRelax,                &
     &      externForcingCycle, externForcingPeriod,                              &
     &      convertFW2Salt, temp_EvPrRn, salt_EvPrRn,                             &
     &      temp_addMass, salt_addMass, hFacMinDr, hFacMinDp,                     &
     &      ivdc_kappa, hMixCriteria, dRhoSmall, hMixSmooth,                      &
     &      sideDragFactor, bottomDragLinear, bottomDragQuadratic,                &
     &      zRoughBot, nh_Am2, smoothAbsFuncRange, sIceLoadFac,                   &
     &      tCylIn, tCylOut,                                                      &
     &      phiEuler, thetaEuler, psiEuler

      _RL cg2dTargetResidual
      _RL cg2dTargetResWunit
      _RL cg3dTargetResidual
      _RL cg3dTargetResWunit
      _RL cg2dpcOffDFac
      _RL delR(Nr)
      _RL delRc(Nr+1)
      _RL xgOrigin
      _RL ygOrigin
      _RL rSphere
      _RL recip_rSphere
      _RL radius_fromHorizGrid
      _RL seaLev_Z
      _RL top_Pres
      _RL rSigmaBnd
      _RL deltaT
      _RL deltaTClock
      _RL deltaTMom
      _RL dTtracerLev(Nr)
      _RL deltaTFreeSurf
      _RL abEps, alph_AB, beta_AB
      _RL f0
      _RL beta
      _RL fPrime
      _RL omega
      _RL rotationPeriod
      _RL freeSurfFac
      _RL implicSurfPress
      _RL implicDiv2DFlow
      _RL implicitNHPress
      _RL hFacMin
      _RL hFacMinDz
      _RL hFacMinDp
      _RL hFacMinDr
      _RL hFacInf
      _RL hFacSup
      _RL viscArNr(Nr)
      _RL viscFacAdj
      _RL viscAh
      _RL viscAhW
      _RL viscAhD
      _RL viscAhZ
      _RL smag3D_coeff, smag3D_diffCoeff
      _RL viscAhMax
      _RL viscAhReMax
      _RL viscAhGrid, viscAhGridMax, viscAhGridMin
      _RL viscC2leith
      _RL viscC2leithD
      _RL viscC2LeithQG
      _RL viscC2smag
      _RL viscA4
      _RL viscA4W
      _RL viscA4D
      _RL viscA4Z
      _RL viscA4Max
      _RL viscA4ReMax
      _RL viscA4Grid, viscA4GridMax, viscA4GridMin
      _RL viscC4leith
      _RL viscC4leithD
      _RL viscC4smag
      _RL diffKhT
      _RL diffK4T
      _RL diffKrNrT(Nr)
      _RL diffKr4T(Nr)
      _RL diffKhS
      _RL diffK4S
      _RL diffKrNrS(Nr)
      _RL diffKr4S(Nr)
      _RL diffKrBL79surf
      _RL diffKrBL79deep
      _RL diffKrBL79scl
      _RL diffKrBL79Ho
      _RL BL79LatVary
      _RL diffKrBLEQsurf
      _RL diffKrBLEQdeep
      _RL diffKrBLEQscl
      _RL diffKrBLEQHo
      _RL pCellMix_maxFac
      _RL pCellMix_delR
      _RL pCellMix_viscAr(Nr)
      _RL pCellMix_diffKr(Nr)
      _RL tauCD, rCD, epsAB_CD
      _RL gravity,       recip_gravity
      _RL gBaro
      _RL gravFacC(Nr),   recip_gravFacC(Nr)
      _RL gravFacF(Nr+1), recip_gravFacF(Nr+1)
      _RL rhoNil
      _RL rhoConst,      recip_rhoConst
      _RL rho1Ref(Nr)
      _RL rhoFacC(Nr),   recip_rhoFacC(Nr)
      _RL rhoFacF(Nr+1), recip_rhoFacF(Nr+1)
      _RL rhoConstFresh
      _RL thetaConst
      _RL tRef(Nr)
      _RL sRef(Nr)
      _RL rhoRef(Nr)
      _RL dBdrRef(Nr)
      _RL surf_pRef, pRef4EOS(Nr)
      _RL phiRef(2*Nr+1)
      _RL rVel2wUnit(Nr+1), wUnit2rVel(Nr+1)
      _RL rUnit2z(Nr), z2rUnit(Nr)
      _RL mass2rUnit, rUnit2mass
      _RL baseTime
      _RL startTime
      _RL endTime
      _RL chkPtFreq
      _RL pChkPtFreq
      _RL dumpFreq
      _RL adjDumpFreq
      _RL diagFreq
      _RL monitorFreq
      _RL adjMonitorFreq
      _RL afFacMom
      _RL vfFacMom
      _RL pfFacMom
      _RL cfFacMom
      _RL foFacMom
      _RL mtFacMom
      _RL cosPower
      _RL cAdjFreq
      _RL tauThetaClimRelax
      _RL tauSaltClimRelax
      _RL latBandClimRelax
      _RL externForcingCycle
      _RL externForcingPeriod
      _RL convertFW2Salt
      _RL temp_EvPrRn
      _RL salt_EvPrRn
      _RL temp_addMass
      _RL salt_addMass
      _RL ivdc_kappa
      _RL hMixCriteria
      _RL dRhoSmall
      _RL hMixSmooth
      _RL sideDragFactor
      _RL bottomDragLinear
      _RL bottomDragQuadratic
      _RL zRoughBot
      _RL smoothAbsFuncRange
      _RL sIceLoadFac
      _RL nh_Am2
      _RL tCylIn, tCylOut
      _RL phiEuler, thetaEuler, psiEuler

!--   COMMON /PARM_A/ Thermodynamics constants ?
      COMMON /PARM_A/ HeatCapacity_Cp
      _RL HeatCapacity_Cp

!--   COMMON /PARM_ATM/ Atmospheric physical parameters (Ideal Gas EOS, ...)
! celsius2K :: convert centigrade (Celsius) degree to Kelvin
! atm_Po    :: standard reference pressure
! atm_Cp    :: specific heat (Cp) of the (dry) air at constant pressure
! atm_Rd    :: gas constant for dry air
! atm_kappa :: kappa = R/Cp (R: constant of Ideal Gas EOS)
! atm_Rq    :: water vapour specific volume anomaly relative to dry air
!              (e.g. typical value = (29/18 -1) 10^-3 with q [g/kg])
! integr_GeoPot :: option to select the way we integrate the geopotential
!                 (still a subject of discussions ...)
! selectFindRoSurf :: select the way surf. ref. pressure (=Ro_surf) is
!         derived from the orography. Implemented: 0,1 (see INI_P_GROUND)
      COMMON /PARM_ATM/                                                           &
     &      celsius2K,                                                            &
     &      atm_Cp, atm_Rd, atm_kappa, atm_Rq, atm_Po,                            &
     &      integr_GeoPot, selectFindRoSurf
      _RL celsius2K
      _RL atm_Po, atm_Cp, atm_Rd, atm_kappa, atm_Rq
      INTEGER :: integr_GeoPot, selectFindRoSurf

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
!-- Logical flags for selecting packages
      LOGICAL :: useGAD
      LOGICAL :: useOBCS
      LOGICAL :: useSHAP_FILT
      LOGICAL :: useZONAL_FILT
      LOGICAL :: useOPPS
      LOGICAL :: usePP81
      LOGICAL :: useKL10
      LOGICAL :: useMY82
      LOGICAL :: useGGL90
      LOGICAL :: useKPP
      LOGICAL :: useGMRedi
      LOGICAL :: useDOWN_SLOPE
      LOGICAL :: useBBL
      LOGICAL :: useCAL
      LOGICAL :: useEXF
      LOGICAL :: useBulkForce
      LOGICAL :: useEBM
      LOGICAL :: useCheapAML
      LOGICAL :: useAUTODIFF
      LOGICAL :: useGrdchk
      LOGICAL :: useSMOOTH
      LOGICAL :: usePROFILES
      LOGICAL :: useOBSFIT
      LOGICAL :: useECCO
      LOGICAL :: useCTRL
      LOGICAL :: useSBO
      LOGICAL :: useFLT
      LOGICAL :: usePTRACERS
      LOGICAL :: useGCHEM
      LOGICAL :: useRBCS
      LOGICAL :: useOffLine
      LOGICAL :: useMATRIX
      LOGICAL :: useFRAZIL
      LOGICAL :: useSEAICE
      LOGICAL :: useSALT_PLUME
      LOGICAL :: useShelfIce
      LOGICAL :: useSTIC
      LOGICAL :: useStreamIce
      LOGICAL :: useICEFRONT
      LOGICAL :: useThSIce
      LOGICAL :: useLand
      LOGICAL :: useATM2d
      LOGICAL :: useAIM
      LOGICAL :: useAtm_Phys
      LOGICAL :: useFizhi
      LOGICAL :: useGridAlt
      LOGICAL :: useDiagnostics
      LOGICAL :: useREGRID
      LOGICAL :: useLayers
      LOGICAL :: useMNC
      LOGICAL :: useRunClock
      LOGICAL :: useEMBED_FILES
      LOGICAL :: useMYPACKAGE
      COMMON /PARM_PACKAGES/                                                      &
     &      useGAD, useOBCS, useSHAP_FILT, useZONAL_FILT,                         &
     &      useOPPS, usePP81, useKL10, useMY82, useGGL90, useKPP,                 &
     &      useGMRedi, useBBL, useDOWN_SLOPE,                                     &
     &      useCAL, useEXF, useBulkForce, useEBM, useCheapAML,                    &
     &      useGrdchk, useSMOOTH, usePROFILES, useOBSFIT,                         &
     &      useECCO, useCTRL,                                                     &
     &      useSBO, useFLT, useAUTODIFF,                                          &
     &      usePTRACERS, useGCHEM, useRBCS, useOffLine, useMATRIX,                &
     &      useFRAZIL, useSEAICE, useSALT_PLUME, useShelfIce, useSTIC,            &
     &      useStreamIce, useICEFRONT, useThSIce, useLand,                        &
     &      useATM2d, useAIM, useAtm_Phys, useFizhi, useGridAlt,                  &
     &      useDiagnostics, useREGRID, useLayers, useMNC,                         &
     &      useRunClock, useEMBED_FILES,                                          &
     &      useMYPACKAGE

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
