C $Header: /u/gcmpack/MITgcm/model/inc/PARAMS.h,v 1.203 2007/08/23 19:07:32 jmc Exp $
C $Name:  $
C

CBOP
C     !ROUTINE: PARAMS.h
C     !INTERFACE:
C     #include PARAMS.h

C     !DESCRIPTION:
C     Header file defining model "parameters".  The values from the
C     model standard input file are stored into the variables held
C     here. Notes describing the parameters can also be found here.

CEOP

C     Macros for special grid options
#include "PARAMS_MACROS.h"

C--   Contants
C     Useful physical values
      Real*8 PI
      PARAMETER ( PI    = 3.14159265358979323844D0   )
      Real*8 deg2rad
      PARAMETER ( deg2rad = 2.D0*PI/360.D0           )

C     Symbolic values
C     precXXXX :: Used to indicate what precision to use for
C                dumping model state.
      INTEGER precFloat32
      PARAMETER ( precFloat32 = 32 )
      INTEGER precFloat64
      PARAMETER ( precFloat64 = 64 )
C     UNSET_xxx :: Used to indicate variables that have not been given a value
      Real*8 UNSET_FLOAT8
      PARAMETER ( UNSET_FLOAT8 = 1.234567D5 )
      Real*4 UNSET_FLOAT4
      PARAMETER ( UNSET_FLOAT4 = 1.234567E5 )
      _RL    UNSET_RL
      PARAMETER ( UNSET_RL     = 1.234567D5 )
      _RS    UNSET_RS
      PARAMETER ( UNSET_RS     = 1.234567E5 )
      INTEGER UNSET_I
      PARAMETER ( UNSET_I      = 123456789  )

C     Checkpoint data
      INTEGER maxNoChkptLev
      PARAMETER ( maxNoChkptLev = 2 )

C--   COMMON /PARM_C/ Character valued parameters used by the model.
C     checkPtSuff :: List of checkpoint file suffices
C     tRefFile      :: File containing reference Potential Temperat.  tRef (1.D)
C     sRefFile      :: File containing reference salinity/spec.humid. sRef (1.D)
C     rhoRefFile    :: File containing reference density profile rhoRef (1.D)
C     delRFile      :: File containing vertical grid spacing delR  (1.D array)
C     delRcFile     :: File containing vertical grid spacing delRc (1.D array)
C     delXFile      :: File containing X-spacing grid definition (1.D array)
C     delYFile      :: File containing Y-spacing grid definition (1.D array)
C     horizGridFile :: File containing horizontal-grid definition
C                        (only when using curvilinear_grid)
C     bathyFile   :: File containing bathymetry. If not defined bathymetry
C                   is taken from inline function.
C     topoFile    :: File containing the topography of the surface (unit=m)
C                   (mainly used for the atmosphere = ground height).
C     shelfIceFile:: File containing the topography of the shelfice draught
C                    (unit=m)
C     hydrogThetaFile :: File containing initial hydrographic data for potential
C                       temperature.
C     hydrogSaltFile  :: File containing initial hydrographic data for salinity.
C     diffKrFile      :: File containing 3D specification of vertical diffusivity
C     zonalWindFile   :: File containing zonal wind data
C     meridWindFile   :: File containing meridional wind data
C     thetaClimFile   :: File containing theta climataology used
C                       in relaxation term -lambda(theta-theta*)
C     saltClimFile    :: File containing salt climataology used
C                       in relaxation term -lambda(salt-salt*)
C     surfQfile       :: File containing surface heat flux, excluding SW
C                        (old version, kept for backward compatibility)
C     surfQnetFile    :: File containing surface net heat flux
C     surfQswFile     :: File containing surface shortwave radiation
C     dQdTfile        :: File containing thermal relaxation coefficient
C     EmPmRfile       :: File containing surface fresh water flux
C     saltFluxFile    :: File containing surface salt flux
C     pLoadFile       :: File containing pressure loading
C     eddyTauxFile    :: File containing zonal Eddy stress data
C     eddyTauyFile    :: File containing meridional Eddy stress data
C     buoyancyRelation :: Flag used to indicate which relation to use to
C                        get buoyancy.
C     eosType         :: choose the equation of state:
C                        LINEAR, POLY3, UNESCO, JMD95Z, JMD95P, MDJWF, IDEALGAS
C     the_run_name    :: string identifying the name of the model "run"
      COMMON /PARM_C/ checkPtSuff,
     &                tRefFile, sRefFile, rhoRefFile,
     &                delRFile, delRcFile,
     &                delXFile, delYFile, horizGridFile,
     &                bathyFile, topoFile, shelfIceFile,
     &                hydrogThetaFile, hydrogSaltFile, diffKrFile,
     &                zonalWindFile, meridWindFile, thetaClimFile,
     &                saltClimFile, buoyancyRelation,
     &                EmPmRfile, saltFluxFile,
     &                surfQfile, surfQnetFile, surfQswFile,
     &                lambdaThetaFile, lambdaSaltFile,
     &                uVelInitFile, vVelInitFile, pSurfInitFile,
     &                dQdTfile, ploadFile,
     &                eddyTauxFile, eddyTauyFile,
     &                eosType, pickupSuff,
     &                mdsioLocalDir,
     &                the_run_name
      CHARACTER*(5) checkPtSuff(maxNoChkptLev)
      CHARACTER*(MAX_LEN_FNAM) tRefFile
      CHARACTER*(MAX_LEN_FNAM) sRefFile
      CHARACTER*(MAX_LEN_FNAM) rhoRefFile
      CHARACTER*(MAX_LEN_FNAM) delRFile
      CHARACTER*(MAX_LEN_FNAM) delRcFile
      CHARACTER*(MAX_LEN_FNAM) delXFile
      CHARACTER*(MAX_LEN_FNAM) delYFile
      CHARACTER*(MAX_LEN_FNAM) horizGridFile
      CHARACTER*(MAX_LEN_FNAM) bathyFile, topoFile, shelfIceFile
      CHARACTER*(MAX_LEN_FNAM) hydrogThetaFile, hydrogSaltFile
      CHARACTER*(MAX_LEN_FNAM) diffKrFile
      CHARACTER*(MAX_LEN_FNAM) zonalWindFile
      CHARACTER*(MAX_LEN_FNAM) meridWindFile
      CHARACTER*(MAX_LEN_FNAM) thetaClimFile
      CHARACTER*(MAX_LEN_FNAM) saltClimFile
      CHARACTER*(MAX_LEN_FNAM) surfQfile
      CHARACTER*(MAX_LEN_FNAM) surfQnetFile
      CHARACTER*(MAX_LEN_FNAM) surfQswFile
      CHARACTER*(MAX_LEN_FNAM) EmPmRfile
      CHARACTER*(MAX_LEN_FNAM) saltFluxFile
      CHARACTER*(MAX_LEN_FNAM) buoyancyRelation
      CHARACTER*(MAX_LEN_FNAM) uVelInitFile
      CHARACTER*(MAX_LEN_FNAM) vVelInitFile
      CHARACTER*(MAX_LEN_FNAM) pSurfInitFile
      CHARACTER*(MAX_LEN_FNAM) dQdTfile
      CHARACTER*(MAX_LEN_FNAM) ploadFile
      CHARACTER*(MAX_LEN_FNAM) eddyTauxFile
      CHARACTER*(MAX_LEN_FNAM) eddyTauyFile
      CHARACTER*(MAX_LEN_FNAM) lambdaThetaFile
      CHARACTER*(MAX_LEN_FNAM) lambdaSaltFile
      CHARACTER*(MAX_LEN_FNAM) mdsioLocalDir
      CHARACTER*(MAX_LEN_PREC/2) the_run_name
      CHARACTER*(6) eosType
      CHARACTER*(10) pickupSuff

C--   COMMON /PARM_I/ Integer valued parameters used by the model.
C     cg2dMaxIters        :: Maximum number of iterations in the
C                           two-dimensional con. grad solver.
C     cg2dChkResFreq      :: Frequency with which to check residual
C                           in con. grad solver.
C     cg2dPreCondFreq     :: Frequency for updating cg2d preconditioner
C                            (non-linear free-surf.)
C     cg3dMaxIters        :: Maximum number of iterations in the
C                           three-dimensional con. grad solver.
C     cg3dChkResFreq      :: Frequency with which to check residual
C                           in con. grad solver.
C     nIter0              :: Start time-step number of for this run
C     nTimeSteps          :: Number of timesteps to execute
C     numStepsPerPickup   :: For offline setup. Frequency of pickup
C                           of flow fields.
C     writeStatePrec      :: Precision used for writing model state.
C     writeBinaryPrec     :: Precision used for writing binary files
C     readBinaryPrec      :: Precision used for reading binary files
C     nCheckLev           :: Holds current checkpoint level
C     nonlinFreeSurf      :: option related to non-linear free surface
C                           =0 Linear free surface ; >0 Non-linear
C     select_rStar        :: option related to r* vertical coordinate
C                           =0 (default) use r coord. ; > 0 use r*
C     momForcingOutAB     :: =1: take momentum forcing contribution
C                           out of (=0: in) Adams-Bashforth time stepping.
C     tracForcingOutAB    :: =1: take tracer (Temp,Salt,pTracers) forcing contribution
C                           out of (=0: in) Adams-Bashforth time stepping.
C     tempAdvScheme       :: Temp. Horiz.Advection scheme selector
C     tempVertAdvScheme   :: Temp. Vert. Advection scheme selector
C     saltAdvScheme       :: Salt. Horiz.advection scheme selector
C     saltVertAdvScheme   :: Salt. Vert. Advection scheme selector
C     selectKEscheme      :: Kinetic Energy scheme selector (Vector Inv.)
C     debugLevel          :: debug level selector: higher -> more writing

      COMMON /PARM_I/
     &        cg2dMaxIters,
     &        cg2dChkResFreq, cg2dPreCondFreq,
     &        cg3dMaxIters,
     &        cg3dChkResFreq,
     &        nIter0, nTimeSteps, nEndIter,
     &        numStepsPerPickup,
     &        writeStatePrec, nCheckLev,
     &        writeBinaryPrec, readBinaryPrec,
     &        nonlinFreeSurf, select_rStar,
     &        momForcingOutAB, tracForcingOutAB,
     &        tempAdvScheme, tempVertAdvScheme,
     &        saltAdvScheme, saltVertAdvScheme,
     &        selectKEscheme,
     &        debugLevel
      INTEGER cg2dMaxIters
      INTEGER cg2dChkResFreq
      INTEGER cg2dPreCondFreq
      INTEGER cg3dMaxIters
      INTEGER cg3dChkResFreq
      INTEGER nIter0
      INTEGER nTimeSteps
      INTEGER nEndIter
      INTEGER numStepsPerPickup
      INTEGER writeStatePrec
      INTEGER writeBinaryPrec
      INTEGER readBinaryPrec
      INTEGER nCheckLev
      INTEGER nonlinFreeSurf
      INTEGER select_rStar
      INTEGER momForcingOutAB, tracForcingOutAB
      INTEGER tempAdvScheme, tempVertAdvScheme
      INTEGER saltAdvScheme, saltVertAdvScheme
      INTEGER selectKEscheme
      INTEGER debugLevel

C
      INTEGER debLevZero
      PARAMETER(debLevZero=0)
      INTEGER debLevA
      PARAMETER(debLevA=1)
      INTEGER debLevB
      PARAMETER(debLevB=2)

C--   COMMON /PARM_L/ Logical valued parameters used by the model.
C     usingCartesianGrid :: If TRUE grid generation will be in a cartesian
C                          coordinate frame.
C     usingSphericalPolarGrid :: If TRUE grid generation will be in a 
C                               spherical polar frame.
C     usingCylindricalGrid :: If TRUE grid generation will be Cylindrical
C     usingCurvilinearGrid :: If TRUE, use a curvilinear grid (to be provided)
C     deepAtmosphere :: deep model (drop the shallow-atmosphere approximation)
C     no_slip_sides :: Impose "no-slip" at lateral boundaries.
C     no_slip_bottom :: Impose "no-slip" at bottom boundary.
C     momViscosity  :: Flag which turns momentum friction terms on and off.
C     momAdvection  :: Flag which turns advection of momentum on and off.
C     momForcing    :: Flag which turns external forcing of momentum on
C                     and off.
C     momPressureForcing :: Flag which turns pressure term in momentum equation
C                          on and off.
C     metricTerms   :: Flag which turns metric terms on or off.
C     useNHMTerms   :: If TRUE use non-hydrostatic metric terms.
C     useCoriolis   :: Flag which turns the coriolis terms on and off.
C     tempAdvection :: Flag which turns advection of temperature on
C                     and off.
C     tempForcing   :: Flag which turns external forcing of temperature on
C                     and off.
C     saltAdvection :: Flag which turns advection of salinity on 
C                     and off.
C     saltForcing   :: Flag which turns external forcing of salinity on
C                     and off.
C     useRealFreshWaterFlux :: if True (=Natural BCS), treats P+R-E flux 
C                         as a real Fresh Water (=> changes the Sea Level)
C                         if F, converts P+R-E to salt flux (no SL effect)
C     useFullLeith   :: Set to true to use full Leith viscosity(may be unstable
C                       on irregular grids)
C     useAreaViscLength :: Set to true to use old scaling for viscous
C              lengths, e.g., L2=Raz.  May be preferable for cube sphere.
C     useStrainTensionVisc:: Set to true to use Strain-Tension viscous terms
C     rigidLid            :: Set to true to use rigid lid
C     implicitFreeSurface :: Set to true to use implicit free surface
C     exactConserv        :: Set to true to conserve exactly the total Volume
C     linFSConserveTr     :: Set to true to correct source/sink of tracer
C                            at the surface due to Linear Free Surface 
C     uniformLin_PhiSurf  :: Set to true to use a uniform Bo_surf in the
C                           linear relation Phi_surf = Bo_surf*eta
C     use3Dsolver   :: set to true to use 3-D pressure solver
C     implicitIntGravWave :: treat Internal Gravity Wave implicitly
C     staggerTimeStep :: enable a Stagger time stepping T,S Rho then U,V
C     momStepping   :: Turns momentum equation time-stepping off
C     tempStepping  :: Turns temperature equation time-stepping off
C     saltStepping  :: Turns salinity equation time-stepping off
C     useConstantF  :: Coriolis parameter set to f0
C     useBetaPlaneF :: Coriolis parameter set to f0 + beta.y
C     useSphereF    :: Coriolis parameter set to 2.omega.sin(phi)
C     use3dCoriolis :: Turns the 3-D coriolis terms (in Omega.cos Phi) on - off
C     useCDscheme   :: use CD-scheme to calculate Coriolis terms.
C     useJamartWetPoints :: Use wet-point method for Coriolis (Jamart and Ozer, 1986)
C     useJamartMomAdv :: Use wet-point method for V.I. non-linear term
C     SadournyCoriolis :: use the enstrophy conserving scheme by Sadourny
C     upwindVorticity :: bias interpolation of vorticity in the Coriolis term
C     highOrderVorticity :: use 3rd/4th order interp. of vorticity (V.I., advection)
C     upwindShear        :: use 1rst order upwind interp. (V.I., vertical advection)
C     useAbsVorticity :: work with f+zeta in Coriolis terms
C     implicitDiffusion :: Turns implicit vertical diffusion on
C     implicitViscosity :: Turns implicit vertical viscosity on
C     tempImplVertAdv :: Turns on implicit vertical advection for Temperature
C     saltImplVertAdv :: Turns on implicit vertical advection for Salinity
C     momImplVertAdv  :: Turns on implicit vertical advection for Momentum
C     multiDimAdvection :: Flag that enable multi-dimension advection
C     useMultiDimAdvec  :: True if multi-dim advection is used at least once
C     momDissip_In_AB   :: if False, put Dissipation tendency contribution
C                          out off Adams-Bashforth time stepping.
C     doAB_onGtGs       :: if the Adams-Bashforth time stepping is used, always
C                          apply AB on tracer tendencies (rather than on Tracer)
C     startFromPickupAB2 :: with AB-3 code, start from an AB-2 pickup
C     usePickupBeforeC54 :: start from old-pickup files, generated with code from
C                           before checkpoint-54a, Jul 06, 2004.
C     doThetaClimRelax :: Set true if relaxation to temperature
C                        climatology is required.
C     doSaltClimRelax  :: Set true if relaxation to salinity
C                        climatology is required.
C     periodicExternalForcing :: Set true if forcing is time-dependant
C     usingPCoords     :: Set to indicate that we are working in a pressure
C                        type coordinate (p or p*).
C     usingZCoords     :: Set to indicate that we are working in a height
C                        type coordinate (z or z*)
C     fluidIsAir       :: Set to indicate that the fluid major constituent
C                        is air
C     fluidIsWater     :: Set to indicate that the fluid major constituent
C                        is water
C     useDynP_inEos_Zc :: use the dynamical pressure in EOS (with Z-coord.)
C                         this requires specific code for restart & exchange
C     setInterFDr    :: set Interface depth (put cell-Center at the middle)
C     setCenterDr    :: set cell-Center depth (put Interface at the middle)
C     nonHydrostatic :: Using non-hydrostatic terms
C     quasiHydrostatic :: Using non-hydrostatic terms in hydrostatic algorithm
C     globalFiles    :: Selects between "global" and "tiled" files
C     useSingleCpuIO :: On SGI platforms, option globalFiles is either
C                       slow (f77) or does not work (f90).  When
C                       useSingleCpuIO is set, mdsio_writefield.F
C                       outputs from master mpi process only.
C     allowFreezing  :: Allows surface water to freeze and form ice
C     useOldFreezing :: use the old version (before checkpoint52a_pre, 2003-11-12)
C     pickup_write_mdsio :: use mdsio to write pickups
C     pickup_read_mdsio  :: use mdsio to read  pickups
C     pickup_write_immed :: echo the pickup immediately (for conversion)
C     timeave_mdsio      :: use mdsio for timeave output
C     snapshot_mdsio     :: use mdsio for "snapshot" (dumpfreq/diagfreq) output
C     monitor_stdio      :: use stdio for monitor output
C     dumpInitAndLast :: dumps model state to files at Initial (nIter0) 
C                        & Last iteration, in addition multiple of dumpFreq iter.
C     balanceEmPmR    :: substract global mean of EmPmR at every time step
C     balanceQnet     :: substract global mean of Qnet at every time step
C     balancePrintMean:: print substracted global means to STDOUT

      COMMON /PARM_L/ usingCartesianGrid, usingSphericalPolarGrid,
     & usingCurvilinearGrid, usingCylindricalGrid,
     & deepAtmosphere, setInterFDr, setCenterDr,
     & no_slip_sides,no_slip_bottom,
     & momViscosity, momAdvection, momForcing, useCoriolis,
     & momPressureForcing, vectorInvariantMomentum,
     & tempAdvection, tempForcing,
     & saltAdvection, saltForcing,
     & useRealFreshWaterFlux,
     & useFullLeith, useStrainTensionVisc,
     & useAreaViscLength,
     & rigidLid, implicitFreeSurface, exactConserv, linFSConserveTr,
     & uniformLin_PhiSurf,
     & use3Dsolver, implicitIntGravWave, staggerTimeStep,
     & momStepping, tempStepping, saltStepping,
     & metricTerms, useNHMTerms,
     & useConstantF, useBetaPlaneF, useSphereF, use3dCoriolis,
     & useCDscheme,
     & useEnergyConservingCoriolis, useJamartWetPoints, useJamartMomAdv,
     & SadournyCoriolis, upwindVorticity, highOrderVorticity,
     & useAbsVorticity, upwindShear,
     & implicitDiffusion, implicitViscosity,
     & tempImplVertAdv, saltImplVertAdv, momImplVertAdv,
     & multiDimAdvection, useMultiDimAdvec,
     & momDissip_In_AB, doAB_onGtGs,
     & doThetaClimRelax, doSaltClimRelax, doTr1ClimRelax,
     & periodicExternalForcing,
     & fluidIsAir, fluidIsWater,
     & usingPCoords, usingZCoords, useDynP_inEos_Zc,
     & nonHydrostatic, quasiHydrostatic, globalFiles, useSingleCpuIO,
     & allowFreezing, useOldFreezing,
     & usePickupBeforeC54, startFromPickupAB2,
     & pickup_read_mdsio, pickup_write_mdsio, pickup_write_immed,
     & timeave_mdsio, snapshot_mdsio, monitor_stdio,
     & outputTypesInclusive, dumpInitAndLast, debugMode,
     & inAdMode, inAdTrue, inAdFalse, inAdExact,
     & balanceEmPmR, balanceQnet, balancePrintMean

      LOGICAL usingCartesianGrid
      LOGICAL usingSphericalPolarGrid
      LOGICAL usingCylindricalGrid
      LOGICAL usingCurvilinearGrid
      LOGICAL deepAtmosphere
      LOGICAL setInterFDr
      LOGICAL setCenterDr
      LOGICAL useNHMTerms
      LOGICAL no_slip_sides
      LOGICAL no_slip_bottom
      LOGICAL momViscosity
      LOGICAL momAdvection
      LOGICAL momForcing
      LOGICAL momPressureForcing
      LOGICAL useCoriolis
      LOGICAL vectorInvariantMomentum
      LOGICAL tempAdvection
      LOGICAL tempForcing
      LOGICAL saltAdvection
      LOGICAL saltForcing
      LOGICAL useRealFreshWaterFlux
      LOGICAL useFullLeith
      LOGICAL useStrainTensionVisc
      LOGICAL useAreaViscLength
      LOGICAL rigidLid
      LOGICAL implicitFreeSurface
      LOGICAL exactConserv
      LOGICAL linFSConserveTr
      LOGICAL uniformLin_PhiSurf
      LOGICAL use3Dsolver
      LOGICAL implicitIntGravWave
      LOGICAL staggerTimeStep
      LOGICAL momStepping
      LOGICAL tempStepping
      LOGICAL saltStepping
      LOGICAL metricTerms
      LOGICAL useConstantF
      LOGICAL useBetaPlaneF
      LOGICAL useSphereF
      LOGICAL use3dCoriolis
      LOGICAL useCDscheme
      LOGICAL useEnergyConservingCoriolis
      LOGICAL useJamartWetPoints
      LOGICAL useJamartMomAdv
      LOGICAL SadournyCoriolis
      LOGICAL upwindVorticity
      LOGICAL highOrderVorticity
      LOGICAL useAbsVorticity
      LOGICAL upwindShear
      LOGICAL implicitDiffusion
      LOGICAL implicitViscosity
      LOGICAL tempImplVertAdv
      LOGICAL saltImplVertAdv
      LOGICAL momImplVertAdv
      LOGICAL multiDimAdvection
      LOGICAL useMultiDimAdvec
      LOGICAL momDissip_In_AB
      LOGICAL doAB_onGtGs
      LOGICAL doThetaClimRelax
      LOGICAL doSaltClimRelax
      LOGICAL doTr1ClimRelax
      LOGICAL periodicExternalForcing
      LOGICAL fluidIsAir
      LOGICAL fluidIsWater
      LOGICAL usingPCoords
      LOGICAL usingZCoords
      LOGICAL useDynP_inEos_Zc
      LOGICAL nonHydrostatic
      LOGICAL quasiHydrostatic
      LOGICAL globalFiles
      LOGICAL useSingleCpuIO
      LOGICAL allowFreezing
      LOGICAL useOldFreezing
      LOGICAL usePickupBeforeC54
      LOGICAL startFromPickupAB2
      LOGICAL dumpInitAndLast
      LOGICAL debugMode
      LOGICAL pickup_read_mdsio, pickup_write_mdsio
      LOGICAL pickup_write_immed
      LOGICAL timeave_mdsio, snapshot_mdsio, monitor_stdio
      LOGICAL outputTypesInclusive
      LOGICAL inAdMode, inAdTrue, inAdFalse, inAdExact

      LOGICAL balanceEmPmR
      LOGICAL balanceQnet
      LOGICAL balancePrintMean

C--   COMMON /PARM_R/ "Real" valued parameters used by the model.
C     cg2dTargetResidual
C          :: Target residual for cg2d solver; no unit (RHS normalisation)
C     cg2dTargetResWunit
C          :: Target residual for cg2d solver; W unit (No RHS normalisation)
C     cg3dTargetResidual
C               :: Target residual for cg3d solver.
C     cg2dpcOffDFac :: Averaging weight for preconditioner off-diagonal.
C     Note. 20th May 1998
C           I made a weird discovery! In the model paper we argue
C           for the form of the preconditioner used here ( see
C           A Finite-volume, Incompressible Navier-Stokes Model
C           ...., Marshall et. al ). The algebra gives a simple
C           0.5 factor for the averaging of ac and aCw to get a
C           symmettric pre-conditioner. By using a factor of 0.51
C           i.e. scaling the off-diagonal terms in the
C           preconditioner down slightly I managed to get the
C           number of iterations for convergence in a test case to
C           drop form 192 -> 134! Need to investigate this further!
C           For now I have introduced a parameter cg2dpcOffDFac which
C           defaults to 0.51 but can be set at runtime.
C     delR      :: Vertical grid spacing ( units of r ).
C     delRc     :: Vertical grid spacing between cell centers (r unit).
C     delX      :: Separation between cell faces (m) or (deg), depending
C     delY        on input flags.
C     gravity   :: Accel. due to gravity ( m/s^2 )
C     recip_gravity and its inverse
C     gBaro     :: Accel. due to gravity used in barotropic equation ( m/s^2 )
C     rhoNil    :: Reference density for the linear equation of state
C     rhoConst  :: Vertically constant reference density
C     rhoFacC   :: normalized (by rhoConst) reference density at cell-Center
C     rhoFacF   :: normalized (by rhoConst) reference density at cell-interFace
C     rhoConstFresh :: Constant reference density for fresh water (rain)
C     tRef      :: reference vertical profile for potential temperature
C     sRef      :: reference vertical profile for salinity/specific humidity
C     phiRef    :: reference potential (pressure/rho, geopotential) profile
C     dBdrRef   :: vertical gradient of reference boyancy  [(m/s/r)^2)]:
C               :: z-coord: = N^2_ref = Brunt-Vaissala frequency [s^-2]
C               :: p-coord: = -(d.alpha/dp)_ref          [(m^2.s/kg)^2]
C     rVel2wUnit :: units conversion factor (Non-Hydrostatic code),
C                :: from r-coordinate vertical velocity to vertical velocity [m/s].
C                :: z-coord: = 1 ; p-coord: wSpeed [m/s] = rVel [Pa/s] * rVel2wUnit
C     wUnit2rVel :: units conversion factor (Non-Hydrostatic code),
C                :: from vertical velocity [m/s] to r-coordinate vertical velocity.
C                :: z-coord: = 1 ; p-coord: rVel [Pa/s] = wSpeed [m/s] * wUnit2rVel
C     mass2rUnit :: units conversion factor (surface forcing),
C                :: from mass per unit area [kg/m2] to vertical r-coordinate unit.
C                :: z-coord: = 1/rhoConst ( [kg/m2] / rho = [m] ) ;
C                :: p-coord: = gravity    ( [kg/m2] *  g = [Pa] ) ;
C     rUnit2mass :: units conversion factor (surface forcing),
C                :: from vertical r-coordinate unit to mass per unit area [kg/m2].
C                :: z-coord: = rhoConst  ( [m] * rho = [kg/m2] ) ;
C                :: p-coord: = 1/gravity ( [Pa] /  g = [kg/m2] ) ;
C     phiMin    :: Latitude of southern most cell face.
C     thetaMin  :: Longitude of western most cell face (this
C                 is an "inert" parameter but it is included
C                 to make geographical references simple.)
C     rSphere   :: Radius of sphere for a spherical polar grid ( m ).
C     recip_rSphere  :: Reciprocal radius of sphere ( m ).
C     f0        :: Reference coriolis parameter ( 1/s )
C                 ( Southern edge f for beta plane )
C     beta      :: df/dy ( s^-1.m^-1 )
C     omega     :: Angular velocity ( rad/s )
C     rotationPeriod :: Rotation period (s) (= 2.pi/omega)
C     viscAh    :: Eddy viscosity coeff. for mixing of
C                 momentum laterally ( m^2/s )
C     viscAhW   :: Eddy viscosity coeff. for mixing of vertical
C                 momentum laterally, no effect for hydrostatic
C                 model, defaults to viscAh if unset ( m^2/s )
C                 Not used if variable horiz. viscosity is used.
C     viscAr    :: Eddy viscosity coeff. for mixing of
C                 momentum vertically ( units of r^2/s )
C     viscA4    :: Biharmonic viscosity coeff. for mixing of
C                 momentum laterally ( m^4/s )
C     viscA4W   :: Biharmonic viscosity coeff. for mixing of vertical
C                 momentum laterally, no effect for hydrostatic
C                 model, defaults to viscA4 if unset ( m^2/s )
C                 Not used if variable horiz. viscosity is used.
C     viscAhD   :: Eddy viscosity coeff. for mixing of momentum laterally
C                  (act on Divergence part) ( m^2/s )
C     viscAhZ   :: Eddy viscosity coeff. for mixing of momentum laterally
C                  (act on Vorticity  part) ( m^2/s )
C     viscA4D   :: Biharmonic viscosity coeff. for mixing of momentum laterally
C                  (act on Divergence part) ( m^4/s )
C     viscA4Z   :: Biharmonic viscosity coeff. for mixing of momentum laterally
C                  (act on Vorticity  part) ( m^4/s )
C     viscC2leith :: Leith non-dimensional viscosity factor (grad(vort))
C     viscC2leithD :: Modified Leith non-dimensional visc. factor (grad(div))
C     viscC2smag  :: Smagorinsky non-dimensional viscosity factor (harmonic)
C     viscC4smag  :: Smagorinsky non-dimensional viscosity factor (biharmonic)
C     viscAhMax :: Maximum eddy viscosity coeff. for mixing of
C                   momentum laterally ( m^2/s )
C     viscAhReMax :: Maximum gridscale Reynolds number for eddy viscosity 
C                   coeff. for mixing of momentum laterally (non-dim)
C     viscAhGridMax:: maximum and minimum harmonic viscosity coefficients ...
C     viscAhGridMin::  in terms of non-dimensional grid-size dependent visc.
C     viscA4Max :: Maximum biharmonic viscosity coeff. for mixing of
C                 momentum laterally ( m^4/s )
C     viscA4ReMax :: Maximum Gridscale Reynolds number for 
C                  biharmonic viscosity coeff. momentum laterally (non-dim)
C     viscAhGrid:: non-dimensional grid-size dependent viscosity
C     viscA4Grid:: non-dimensional grid-size dependent bi-harmonic viscosity
C     viscA4GridMax:: maximum and minimum biharmonic viscosity coefficients ...
C     viscA4GridMin::  in terms of non-dimensional grid-size dependent viscosity
C     viscC4leith :: Leith non-dimensional viscosity factor (grad(vort))
C     viscC4leithD :: Modified Leith non-dimensional viscosity factor (grad(div))
C     diffKhT   :: Laplacian diffusion coeff. for mixing of
C                 heat laterally ( m^2/s )
C     diffKrNrT :: vertical profile of Laplacian diffusion coeff. 
C                 for mixing of heat vertically ( units of r^2/s )
C     diffK4T   :: Biharmonic diffusion coeff. for mixing of
C                 heat laterally ( m^4/s )
C     diffKhS  ::  Laplacian diffusion coeff. for mixing of
C                 salt laterally ( m^2/s )
C     diffKrNrS :: vertical profile of Laplacian diffusion coeff. 
C                 for mixing of salt vertically ( units of r^2/s ), 
C     diffK4S   :: Biharmonic diffusion coeff. for mixing of
C                 salt laterally ( m^4/s )
C     diffKrBL79surf :: T/S surface diffusivity (m^2/s) Bryan and Lewis, 1979
C     diffKrBL79deep :: T/S deep diffusivity (m^2/s) Bryan and Lewis, 1979
C     diffKrBL79scl  :: depth scale for arctan fn (m) Bryan and Lewis, 1979
C     diffKrBL79Ho   :: depth offset for arctan fn (m) Bryan and Lewis, 1979
C     BL79LatVary    :: polarwise of this latitude diffKrBL79 is applied with
C                       gradual transition to diffKrBLEQ towards Equator
C     diffKrBLEQsurf :: same as diffKrBL79surf but at Equator
C     diffKrBLEQdeep :: same as diffKrBL79deep but at Equator
C     diffKrBLEQscl  :: same as diffKrBL79scl but at Equator
C     diffKrBLEQHo   :: same as diffKrBL79Ho but at Equator
C     deltaT    :: Default timestep ( s )
C     deltaTClock  :: Timestep used as model "clock". This determines the
C                    IO frequencies and is used in tagging output. It can
C                    be totally different to the dynamical time. Typically
C                    it will be the deep-water timestep for accelerated runs.
C                    Frequency of checkpointing and dumping of the model state
C                    are referenced to this clock. ( s )
C     deltaTMom    :: Timestep for momemtum equations ( s )
C     dTtracerLev  :: Timestep for tracer equations ( s ), function of level k
C     deltaTfreesurf :: Timestep for free-surface equation ( s )
C     freesurfFac  :: Parameter to turn implicit free surface term on or off
C                    freesurfac = 1. uses implicit free surface
C                    freesurfac = 0. uses rigid lid
C     abEps        :: Adams-Bashforth-2 stabilizing weight
C     alph_AB      :: Adams-Bashforth-3 primary factor
C     beta_AB      :: Adams-Bashforth-3 secondary factor
C     implicSurfPress :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of Surface Pressure Gradient ( 0-1 )
C     implicDiv2Dflow :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of barotropic flow Divergence ( 0-1 )
C     hFacMin      :: Minimum fraction size of a cell (affects hFacC etc...)
C     hFacMinDz    :: Minimum dimesional size of a cell (affects hFacC etc..., m)
C     hFacMinDp    :: Minimum dimesional size of a cell (affects hFacC etc..., Pa)
C     hFacMinDr    :: Minimum dimesional size of a cell (affects hFacC etc..., units of r)
C     hFacInf      :: Threshold (inf and sup) for fraction size of surface cell
C     hFacSup        that control vanishing and creating levels
C     tauCD        :: CD scheme coupling timescale ( 1/s )
C     rCD          :: CD scheme normalised coupling parameter ( 0-1 )
C     baseTime      :: model base time (time origin) = time @ iteration zero
C     startTime     :: Starting time for this integration ( s ).
C     endTime       :: Ending time for this integration ( s ).
C     chkPtFreq     :: Frequency of rolling check pointing ( s ).
C     pChkPtFreq    :: Frequency of permanent check pointing ( s ).
C     dumpFreq      :: Frequency with which model state is written to
C                     post-processing files ( s ).
C     diagFreq      :: Frequency with which model writes diagnostic output
C                     of intermediate quantities.
C     afFacMom      :: Advection of momentum term tracer parameter
C     vfFacMom      :: Momentum viscosity tracer parameter
C     pfFacMom      :: Momentum pressure forcing tracer parameter
C     cfFacMom      :: Coriolis term tracer parameter
C     foFacMom      :: Momentum forcing tracer parameter
C     mtFacMom      :: Metric terms tracer parameter
C     cosPower      :: Power of cosine of latitude to multiply viscosity
C     cAdjFreq      :: Frequency of convective adjustment
C
C     taveFreq      :: Frequency with which time-averaged model state 
C                      is written to post-processing files ( s ).
C     tave_lastIter :: (for state variable only) fraction of the last time 
C                      step (of each taveFreq period) put in the time average. 
C                      (fraction for 1rst iter = 1 - tave_lastIter)
C     tauThetaClimRelax :: Relaxation to climatology time scale ( s ).
C     tauSaltClimRelax :: Relaxation to climatology time scale ( s ).
C     latBandClimRelax :: latitude band where Relaxation to Clim. is applied,
C                         i.e. where |yC| <= latBandClimRelax
C     externForcingPeriod :: Is the period of which forcing varies (eg. 1 month)
C     externForcingCycle :: Is the repeat time of the forcing (eg. 1 year)
C                          (note: externForcingCycle must be an integer
C                           number times externForcingPeriod)
C     convertFW2Salt :: salinity, used to convert Fresh-Water Flux to Salt Flux
C                       (use model surface (local) value if set to -1)
C     temp_EvPrRn :: temperature of Rain & Evap. 
C     salt_EvPrRn :: salinity of Rain & Evap.
C        (notes: a) tracer content of Rain/Evap only used if both 
C                     NonLin_FrSurf & useRealFreshWater are set.
C                b) use model surface (local) value if set to UNSET_RL)
C     hMixCrit    :: criteria for mixed-layer diagnostic
C     ivdc_kappa  :: implicit vertical diffusivity for convection [m^2/s]
C     Ro_SeaLevel :: standard position of Sea-Level in "R" coordinate, used as
C                    starting value (k=1) for vertical coordinate (rf(1)=Ro_SeaLevel)
C     sideDragFactor     :: side-drag scaling factor (used only if no_slip_sides) 
C                           (default=2: full drag ; =1: gives half-slip BC)
C     bottomDragLinear    :: Linear    bottom-drag coefficient (units of [r]/s)
C     bottomDragQuadratic :: Quadratic bottom-drag coefficient (units of [r]/m)
C               (if using zcoordinate, units becomes linear: m/s, quadratic: [-])
C     smoothAbsFuncRange :: 1/2 of interval around zero, for which FORTRAN ABS
C                           is to be replace by a smoother function
C                           (affects myabs, mymin, mymax)
C     nh_Am2        :: scales the non-hydrostatic terms and changes internal scales
C                      (i.e. allows convection at different Rayleigh numbers)
      COMMON /PARM_R/ cg2dTargetResidual, cg2dTargetResWunit,
     & cg2dpcOffDFac, cg3dTargetResidual,
     & delR, delRc, delX, delY,
     & deltaT, deltaTmom, dTtracerLev, deltaTfreesurf, deltaTClock,
     & abEps, alph_AB, beta_AB,
     & phiMin, thetaMin, rSphere, recip_RSphere, f0, beta,
     & viscAh, viscAhW, viscAhMax,
     & viscAhGrid, viscAhGridMax, viscAhGridMin,
     & viscC2leith, viscC2leithD,
     & viscC2smag, viscC4smag,
     & viscAhD, viscAhZ, viscA4D, viscA4Z,
     & viscA4, viscA4W,
     & viscA4Max, viscA4Grid, viscA4GridMax, viscA4GridMin,
     & viscAhRemax, viscA4Remax,
     & viscC4leith, viscC4leithD, viscAr,
     & diffKhT, diffK4T, diffKrNrT,
     & diffKhS, diffK4S, diffKrNrS,
     & diffKrBL79surf, diffKrBL79deep, diffKrBL79scl, diffKrBL79Ho,
     & BL79LatVary,
     & diffKrBLEQsurf, diffKrBLEQdeep, diffKrBLEQscl, diffKrBLEQHo,
     & delT, tauCD, rCD, freeSurfFac, implicSurfPress, implicDiv2Dflow,
     & hFacMin, hFacMinDz, hFacInf, hFacSup,
     & gravity, recip_gravity, gBaro,
     & rhonil, recip_rhonil, rhoConst, recip_rhoConst,
     & rhoFacC, recip_rhoFacC, rhoFacF, recip_rhoFacF,
     & rhoConstFresh, convertEmP2rUnit, tRef, sRef, phiRef, dBdrRef,
     & rVel2wUnit, wUnit2rVel, mass2rUnit, rUnit2mass,
     & baseTime, startTime, endTime,
     & chkPtFreq, pChkPtFreq, dumpFreq, adjDumpFreq,
     & diagFreq, taveFreq, tave_lastIter, monitorFreq, adjMonitorFreq,
     & afFacMom, vfFacMom, pfFacMom, cfFacMom, foFacMom, mtFacMom,
     & cosPower, cAdjFreq, omega, rotationPeriod,
     & tauThetaClimRelax, tauSaltClimRelax,
     & tauTr1ClimRelax, lambdaTr1ClimRelax, latBandClimRelax,
     & externForcingCycle, externForcingPeriod,
     & convertFW2Salt, temp_EvPrRn, salt_EvPrRn,
     & hFacMinDr, hFacMinDp,
     & ivdc_kappa, hMixCriteria, Ro_SeaLevel,
     & sideDragFactor, bottomDragLinear, bottomDragQuadratic, nh_Am2,
     & smoothAbsFuncRange,
     & tCylIn, tCylOut

      _RL cg2dTargetResidual
      _RL cg2dTargetResWunit
      _RL cg3dTargetResidual
      _RL cg2dpcOffDFac
      _RL delR(Nr)
      _RL delRc(Nr+1)
      _RL delX(Nx)
      _RL delY(Ny)
      _RL deltaT
      _RL deltaTClock
      _RL deltaTmom
      _RL dTtracerLev(Nr)
      _RL deltaTfreesurf
      _RL abEps, alph_AB, beta_AB
      _RL phiMin
      _RL thetaMin
      _RL rSphere
      _RL recip_rSphere
      _RL f0
      _RL freeSurfFac
      _RL implicSurfPress
      _RL implicDiv2Dflow
      _RL hFacMin
      _RL hFacMinDz
      _RL hFacMinDp
      _RL hFacMinDr
      _RL hFacInf
      _RL hFacSup
      _RL beta
      _RL viscAh
      _RL viscAhW
      _RL viscAhD
      _RL viscAhZ
      _RL viscAhMax
      _RL viscAhReMax
      _RL viscAhGrid
      _RL viscAhGridMax
      _RL viscAhGridMin
      _RL viscC2leith
      _RL viscC2leithD
      _RL viscC2smag
      _RL viscC4smag
      _RL viscAr
      _RL viscA4
      _RL viscA4W
      _RL viscA4D
      _RL viscA4Z
      _RL viscA4Max
      _RL viscA4ReMax
      _RL viscA4Grid, viscA4GridMax, viscA4GridMin
      _RL viscC4leith
      _RL viscC4leithD
      _RL diffKhT
      _RL diffKrNrT(Nr)
      _RL diffK4T
      _RL diffKhS
      _RL diffKrNrS(Nr)
      _RL diffK4S
      _RL diffKrBL79surf
      _RL diffKrBL79deep
      _RL diffKrBL79scl
      _RL diffKrBL79Ho
      _RL BL79LatVary
      _RL diffKrBLEQsurf
      _RL diffKrBLEQdeep
      _RL diffKrBLEQscl
      _RL diffKrBLEQHo
      _RL delt
      _RL tauCD
      _RL rCD
      _RL gravity
      _RL recip_gravity
      _RL gBaro
      _RL rhonil,        recip_rhonil
      _RL rhoConst,      recip_rhoConst
      _RL rhoFacC(Nr),   recip_rhoFacC(Nr)
      _RL rhoFacF(Nr+1), recip_rhoFacF(Nr+1)
      _RL rhoConstFresh
      _RL convertEmP2rUnit
      _RL tRef(Nr)
      _RL sRef(Nr)
      _RL phiRef(2*Nr+1)
      _RL dBdrRef(Nr)
      _RL rVel2wUnit(Nr+1), wUnit2rVel(Nr+1)
      _RL mass2rUnit, rUnit2mass
      _RL baseTime
      _RL startTime
      _RL endTime
      _RL chkPtFreq
      _RL pChkPtFreq
      _RL dumpFreq
      _RL adjDumpFreq
      _RL diagFreq
      _RL taveFreq
      _RL tave_lastIter
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
      _RL omega
      _RL rotationPeriod
      _RL tauThetaClimRelax
      _RL tauSaltClimRelax
      _RL tauTr1ClimRelax
      _RL lambdaTr1ClimRelax
      _RL latBandClimRelax
      _RL externForcingCycle
      _RL externForcingPeriod
      _RL convertFW2Salt
      _RL temp_EvPrRn
      _RL salt_EvPrRn
      _RL ivdc_kappa
      _RL hMixCriteria
      _RL Ro_SeaLevel
      _RL sideDragFactor
      _RL bottomDragLinear
      _RL bottomDragQuadratic
      _RL smoothAbsFuncRange
      _RL nh_Am2
      _RL tCylIn
      _RL tCylOut

C--   COMMON /PARM_A/ Thermodynamics constants ?
      COMMON /PARM_A/ HeatCapacity_Cp,recip_Cp
      _RL HeatCapacity_Cp
      _RL recip_Cp

C--   COMMON /PARM_ATM/ Atmospheric physical parameters (Ideal Gas EOS, ...)
C     celsius2K :: convert centigrade (Celsius) degree to Kelvin
C     atm_Po    :: standard reference pressure
C     atm_Cp    :: specific heat (Cp) of the (dry) air at constant pressure
C     atm_Rd    :: gas constant for dry air
C     atm_kappa :: kappa = R/Cp (R: constant of Ideal Gas EOS)
C     atm_Rq    :: water vapour specific volume anomaly relative to dry air
C                  (e.g. typical value = (29/18 -1) 10^-3 with q [g/kg])
C     integr_GeoPot :: option to select the way we integrate the geopotential
C                     (still a subject of discussions ...) 
C     selectFindRoSurf :: select the way surf. ref. pressure (=Ro_surf) is
C             derived from the orography. Implemented: 0,1 (see INI_P_GROUND)
      COMMON /PARM_ATM/
     &            celsius2K,
     &            atm_Cp, atm_Rd, atm_kappa, atm_Rq, atm_Po,
     &            integr_GeoPot, selectFindRoSurf
      _RL celsius2K
      _RL atm_Po, atm_Cp, atm_Rd, atm_kappa, atm_Rq
      INTEGER integr_GeoPot, selectFindRoSurf

C Logical flags for selecting packages
      LOGICAL useOPPS
      LOGICAL usePP81
      LOGICAL useMY82
      LOGICAL useGGL90
      LOGICAL useKPP
      LOGICAL useGAD
      LOGICAL useGMRedi
      LOGICAL useOBCS
      LOGICAL useAIM
      LOGICAL useLand
      LOGICAL useCAL
      LOGICAL useEXF
      LOGICAL useEBM
      LOGICAL useGrdchk
      LOGICAL useECCO
      LOGICAL useSHAP_FILT
      LOGICAL useZONAL_FILT
      LOGICAL useFLT
      LOGICAL usePTRACERS
      LOGICAL useGCHEM
      LOGICAL useRBCS
      LOGICAL useOffLine
      LOGICAL useMATRIX
      LOGICAL useSBO
      LOGICAL useSEAICE
      LOGICAL useShelfIce
      LOGICAL useThSIce
      LOGICAL useATM2d
      LOGICAL useBulkForce
      LOGICAL usefizhi
      LOGICAL usegridalt
      LOGICAL useDiagnostics
      LOGICAL useMNC
      LOGICAL useREGRID
      LOGICAL useRunClock
      LOGICAL useEMBED_FILES
      LOGICAL useMYPACKAGE
      COMMON /PARM_PACKAGES/
     &        useOPPS, usePP81, useMY82, useGGL90, useKPP,
     &        useGAD, useGMRedi, useOBCS, useAIM, useLand,
     &        useCAL, useEXF, useEBM, useGrdchk, useECCO,
     &        useSHAP_FILT, useZONAL_FILT, useFLT,
     &        usePTRACERS, useGCHEM, useRBCS, useOffLine, useMATRIX,
     &        useSBO, useSEAICE, useShelfIce,
     &        useThSIce, useATM2D, useBulkForce,
     &        usefizhi, usegridalt, useDiagnostics, useMNC, useREGRID,
     &        useRunClock, useEMBED_FILES, useMYPACKAGE

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
