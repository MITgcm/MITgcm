C $Header: /u/gcmpack/MITgcm/model/inc/PARAMS.h,v 1.109 2004/05/14 21:08:27 heimbach Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: PARAMS.h
C    !INTERFACE:
C    include PARAMS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | PARAMS.h                                                  
C     | o Header file defining model "parameters".                
C     *==========================================================*
C     | The values from the model standard input file are         
C     | stored into the variables held here. Notes describing     
C     | the parameters can also be found here.                    
C     *==========================================================*
C     \ev
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
C     bathyFile   :: File containing bathymetry. If not defined bathymetry
C                   is taken from inline function.
C     topoFile    :: File containing the topography of the surface (unit=m)
C                   (mainly used for the atmosphere = ground height).
C     hydrogThetaFile :: File containing initial hydrographic data for potential
C                       temperature.
C     hydrogSaltFile  :: File containing initial hydrographic data for salinity.
C     zonalWindFile   :: File containing zonal wind data
C     meridWindFile   :: File containing meridional wind data
C     thetaClimFile   :: File containing theta climataology used
C                       in relaxation term -lambda(theta-theta*)
C     saltClimFile    :: File containing salt climataology used
C                       in relaxation term -lambda(salt-salt*)
C     surfQfile       :: File containing surface heat flux
C     surfQswfile     :: File containing surface shortwave radiation
C     dQdTfile        :: File containing thermal relaxation coefficient
C     EmPmRfile       :: File containing surface fresh water flux
C     pLoadFile       :: File containing pressure loading
C     buoyancyRelation :: Flag used to indicate which relation to use to
C                        get buoyancy.
C     eosType         :: choose the equation of state:
C                        LINEAR, POLY3, UNESCO, JMD95Z, JMD95P, MDJWF, IDEALGAS
      COMMON /PARM_C/ checkPtSuff,
     &                bathyFile, topoFile,
     &                hydrogThetaFile, hydrogSaltFile,
     &                zonalWindFile, meridWindFile, thetaClimFile,
     &                saltClimFile, buoyancyRelation,
     &                EmPmRfile, surfQfile, surfQswfile,
     &                uVelInitFile, vVelInitFile, pSurfInitFile,
     &                dQdTfile, ploadFile,
     &                eosType, pickupSuff,
     &                mdsioLocalDir
      CHARACTER*(5) checkPtSuff(maxNoChkptLev)
      CHARACTER*(MAX_LEN_FNAM) bathyFile, topoFile
      CHARACTER*(MAX_LEN_FNAM) hydrogThetaFile
      CHARACTER*(MAX_LEN_FNAM) hydrogSaltFile
      CHARACTER*(MAX_LEN_FNAM) zonalWindFile
      CHARACTER*(MAX_LEN_FNAM) meridWindFile
      CHARACTER*(MAX_LEN_FNAM) thetaClimFile
      CHARACTER*(MAX_LEN_FNAM) saltClimFile
      CHARACTER*(MAX_LEN_FNAM) surfQfile
      CHARACTER*(MAX_LEN_FNAM) surfQswfile
      CHARACTER*(MAX_LEN_FNAM) EmPmRfile
      CHARACTER*(MAX_LEN_FNAM) buoyancyRelation
      CHARACTER*(MAX_LEN_FNAM) uVelInitFile
      CHARACTER*(MAX_LEN_FNAM) vVelInitFile
      CHARACTER*(MAX_LEN_FNAM) pSurfInitFile
      CHARACTER*(MAX_LEN_FNAM) dQdTfile
      CHARACTER*(MAX_LEN_FNAM) ploadFile
      CHARACTER*(MAX_LEN_FNAM) mdsioLocalDir
      CHARACTER*(6) eosType
      CHARACTER*(10) pickupSuff

C--   COMMON /PARM_I/ Integer valued parameters used by the model.
C     cg2dMaxIters        :: Maximum number of iterations in the
C                           two-dimensional con. grad solver.
C     cg2dChkResFreq      :: Frequency with which to check residual
C                           in con. grad solver.
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

      COMMON /PARM_I/
     &        cg2dMaxIters,
     &        cg2dChkResFreq,
     &        cg3dMaxIters,
     &        cg3dChkResFreq,
     &        nIter0, nTimeSteps, nEndIter,
     &        numStepsPerPickup,
     &        writeStatePrec, nCheckLev,
     &        writeBinaryPrec, readBinaryPrec,
     &        nonlinFreeSurf, select_rStar,
     &        tempAdvScheme, saltAdvScheme, tracerAdvScheme,
     &        debugLevel
      INTEGER cg2dMaxIters
      INTEGER cg2dChkResFreq
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
      INTEGER tempAdvScheme
      INTEGER saltAdvScheme
      INTEGER tracerAdvScheme
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
C     no_slip_sides :: Impose "no-slip" at lateral boundaries.
C     no_slip_bottom :: Impose "no-slip" at bottom boundary.
C     staggerTimeStep :: enable a Stagger time stepping T,S Rho then U,V
C     momViscosity  :: Flag which turns momentum friction terms on and off.
C     momAdvection  :: Flag which turns advection of momentum on and off.
C     momForcing    :: Flag which turns external forcing of momentum on
C                     and off.
C     momPressureForcing :: Flag which turns pressure term in momentum equation
C                          on and off.
C     metricTerms   :: Flag which turns metric terms on or off.
C     usingSphericalPolarMTerms :: If TRUE use spherical polar metric terms.
C     useNHMTerms :: If TRUE use non-hydrostatic metric terms.
C     useCoriolis   :: Flag which turns the coriolis terms on and off.
C     tempDiffusion :: Flag which turns diffusion of temperature on
C                     and off.
C     tempAdvection :: Flag which turns advection of temperature on
C                     and off.
C     tempForcing   :: Flag which turns external forcing of temperature on
C                     and off.
C     saltDiffusion :: Flag which turns diffusion of salinity on
C                     and off.
C     saltAdvection :: Flag which turns advection of salinity on 
C                     and off.
C     saltForcing   :: Flag which turns external forcing of salinity on
C                     and off.
C     useRealFreshWaterFlux :: if True (=Natural BCS), treats P+R-E flux 
C                         as a real Fresh Water (=> changes the Sea Level)
C                         if F, converts P+R-E to salt flux (no SL effect)
C     rigidLid            :: Set to true to use rigid lid
C     implicitFreeSurface :: Set to true to use implcit free surface
C     exactConserv        :: Set to true to conserve exactly the total Volume
C     uniformLin_PhiSurf  :: Set to true to use a uniform Bo_surf in the
C                           linear relation Phi_surf = Bo_surf*eta
C     momStepping   :: Turns momentum equation time-stepping off
C     tempStepping  :: Turns temperature equation time-stepping off
C     saltStepping  :: Turns salinity equation time-stepping off
C     tr1Stepping   :: Turns passive tracer 1 time-stepping on/off
C     useConstantF  :: Coriolis parameter set to f0
C     useBetaPlaneF :: Coriolis parameter set to f0 + beta.y
C     useSphereF    :: Coriolis parameter set to 2.omega.sin(phi)
C     useCDscheme   :: use CD-scheme to calculate Coriolis terms.
C     useJamartWetPoints :: Use wet-point method for Coriolis (Jamart and Ozer, 1986)
C     implicitDiffusion :: Turns implicit vertical diffusion on
C     implicitViscosity :: Turns implicit vertical viscosity on
C     tempImplVertAdv :: Turns on implicit vertical advection for Temperature
C     saltImplVertAdv :: Turns on implicit vertical advection for Salinity
C     momImplVertAdv  :: Turns on implicit vertical advection for Momentum
C     multiDimAdvection :: Flag that enable multi-dimension advection
C     forcing_In_AB :: if False, put forcing (Temp,Salt,Tracers) contribution
C                      out off Adams-Bashforth time stepping.
C     doThetaClimRelax :: Set true if relaxation to temperature
C                        climatology is required.
C     doSaltClimRelax  :: Set true if relaxation to salinity
C                        climatology is required.
C     periodicExternalForcing :: Set true if forcing is time-dependant
C     usingPCoords     :: Set to indicate that we are working in pressure
C                        coords. (jmc: is it still used ?)
C     usingZCoords     :: Set to indicate that we are working in height
C                        coords. (jmc: is it still used ?)
C     useDynP_inEos_Zc :: use the dynamical pressure in EOS (with Z-coord.)
C                         this requires specific code for restart & exchange
C     setCenterDr    :: set cell Center depth and put Interface at the middle
C     nonHydrostatic :: Using non-hydrostatic terms
C     quasiHydrostatic :: Using non-hydrostatic terms in hydrostatic algorithm
C     globalFiles    :: Selects between "global" and "tiled" files
C     useSingleCpuIO :: On SGI platforms, option globalFiles is either
C                       slow (f77) or does not work (f90).  When
C                       useSingleCpuIO is set, mdsio_writefield.F
C                       outputs from master mpi process only.
C     allowFreezing  :: Allows surface water to freeze and form ice
C     useOldFreezing :: use the old version (before checkpoint52a_pre, 2003-11-12)
C     groundAtK1  :: put the surface(k=1) at the Lower Boundary (=ground)
      COMMON /PARM_L/ usingCartesianGrid, usingSphericalPolarGrid,
     & usingCurvilinearGrid,
     & no_slip_sides,no_slip_bottom,
     & staggerTimeStep,
     & momViscosity, momAdvection, momForcing, useCoriolis, 
     & momPressureForcing, vectorInvariantMomentum,
     & tempDiffusion, tempAdvection, tempForcing,
     & saltDiffusion, saltAdvection, saltForcing,
     & useRealFreshWaterFlux,
     & rigidLid, implicitFreeSurface, exactConserv, uniformLin_PhiSurf,
     & momStepping, tempStepping, saltStepping, tr1Stepping,
     & metricTerms, usingSphericalPolarMTerms, useNHMTerms,
     & useConstantF, useBetaPlaneF, useSphereF,
     & useCDscheme,
     & useEnergyConservingCoriolis, useJamartWetPoints,
     & implicitDiffusion, implicitViscosity,
     & tempImplVertAdv, saltImplVertAdv, momImplVertAdv,
     & multiDimAdvection, forcing_In_AB,
     & doThetaClimRelax, doSaltClimRelax, doTr1ClimRelax, 
     & periodicExternalForcing, 
     & usingPCoords, usingZCoords, useDynP_inEos_Zc, setCenterDr,
     & nonHydrostatic, quasiHydrostatic, globalFiles, useSingleCpuIO,
     & allowFreezing, useOldFreezing, groundAtK1,
     & usePickupBeforeC35, debugMode,
     & readPickupWithTracer, writePickupWithTracer
      LOGICAL usingCartesianGrid
      LOGICAL usingSphericalPolarGrid
      LOGICAL usingCurvilinearGrid
      LOGICAL usingSphericalPolarMTerms
      LOGICAL useNHMTerms
      LOGICAL no_slip_sides
      LOGICAL no_slip_bottom
      LOGICAL staggerTimeStep
      LOGICAL momViscosity
      LOGICAL momAdvection
      LOGICAL momForcing
      LOGICAL momPressureForcing
      LOGICAL useCoriolis
      LOGICAL vectorInvariantMomentum
      LOGICAL tempDiffusion
      LOGICAL tempAdvection
      LOGICAL tempForcing
      LOGICAL saltDiffusion
      LOGICAL saltAdvection
      LOGICAL saltForcing
      LOGICAL useRealFreshWaterFlux
      LOGICAL rigidLid
      LOGICAL implicitFreeSurface
      LOGICAL exactConserv
      LOGICAL uniformLin_PhiSurf
      LOGICAL momStepping
      LOGICAL tempStepping
      LOGICAL saltStepping
      LOGICAL tr1Stepping
      LOGICAL metricTerms
      LOGICAL useConstantF
      LOGICAL useBetaPlaneF
      LOGICAL useSphereF
      LOGICAL useCDscheme
      LOGICAL useEnergyConservingCoriolis
      LOGICAL useJamartWetPoints
      LOGICAL implicitDiffusion
      LOGICAL implicitViscosity
      LOGICAL tempImplVertAdv 
      LOGICAL saltImplVertAdv 
      LOGICAL momImplVertAdv
      LOGICAL multiDimAdvection
      LOGICAL forcing_In_AB
      LOGICAL doThetaClimRelax
      LOGICAL doSaltClimRelax
      LOGICAL doTr1ClimRelax
      LOGICAL periodicExternalForcing
      LOGICAL usingPCoords
      LOGICAL usingZCoords
      LOGICAL useDynP_inEos_Zc
      LOGICAL setCenterDr
      LOGICAL nonHydrostatic
      LOGICAL quasiHydrostatic
      LOGICAL globalFiles
      LOGICAL useSingleCpuIO
      LOGICAL allowFreezing
      LOGICAL useOldFreezing
      LOGICAL groundAtK1
      LOGICAL usePickupBeforeC35
      LOGICAL debugMode
      LOGICAL readPickupWithTracer
      LOGICAL writePickupWithTracer

C--   COMMON /PARM_R/ "Real" valued parameters used by the model.
C     gg2dTargetResidual
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
C     rhoConstFresh :: Constant reference density for fresh water (rain)
C     startTime :: Start time for model ( s )
C     phiMin    :: Latitude of southern most cell face.
C     thetaMin  :: Longitude of western most cell face (this
C                 is an "inert" parameter but it is included
C                 to make geographical references simple.)
C     rSphere   :: Radius of sphere for a spherical polar grid ( m ).
C     recip_RSphere  :: Reciprocal radius of sphere ( m ).
C     f0        :: Reference coriolis parameter ( 1/s )
C                 ( Southern edge f for beta plane )
C     beta      :: df/dy ( s^-1.m^-1 )
C     omega     :: Angular velocity ( rad/s )
C     rotationPeriod :: Rotation period (s) (= 2.pi/omega)
C     viscAh    :: Eddy viscosity coeff. for mixing of
C                 momentum laterally ( m^2/s )
C     viscAr    :: Eddy viscosity coeff. for mixing of
C                 momentum vertically ( units of r^2/s )
C     viscA4    :: Biharmonic viscosity coeff. for mixing of
C                 momentum laterally ( m^4/s )
C     viscAhMax :: Maximum eddy viscosity coeff. for mixing of
C                 momentum laterally ( m^2/s )
C     viscA4Max :: Maximum biharmonic viscosity coeff. for mixing of
C                 momentum laterally ( m^4/s )
C     viscAhGrid:: non-dimensional grid-size dependent viscosity
C     viscA4Grid:: non-dimensional grid-size dependent bi-harmonic viscosity
C     diffKhT   :: Laplacian diffusion coeff. for mixing of
C                 heat laterally ( m^2/s )
C     diffKrT   :: Laplacian diffusion coeff. for mixing of
C                 heat vertically ( units of r^2/s )
C     diffK4T   :: Biharmonic diffusion coeff. for mixing of
C                 heat laterally ( m^4/s )
C     diffKhS  ::  Laplacian diffusion coeff. for mixing of
C                 salt laterally ( m^2/s )
C     diffKrS   :: Laplacian diffusion coeff. for mixing of
C                 salt vertically ( units of r^2/s )
C     diffK4S   :: Biharmonic diffusion coeff. for mixing of
C                 salt laterally ( m^4/s )
C     deltaT    :: Default timestep ( s )
C     deltaTClock  :: Timestep used as model "clock". This determines the
C                    IO frequencies and is used in tagging output. It can
C                    be totally different to the dynamical time. Typically
C                    it will be the deep-water timestep for accelerated runs.
C                    Frequency of checkpointing and dumping of the model state
C                    are referenced to this clock. ( s )
C     deltaTMom    :: Timestep for momemtum equations ( s )
C     deltaTtracer :: Timestep for tracer equations ( s )
C     deltaTfreesurf :: Timestep for free-surface equation ( s )
C     freesurfFac  :: Parameter to turn implicit free surface term on or off
C                    freesurfac = 1. uses implicit free surface
C                    freesurfac = 0. uses rigid lid
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
C     lambdaThetaClimRelax :: Inverse time scale for relaxation ( 1/s ).
C     tauSaltClimRelax :: Relaxation to climatology time scale ( s ).
C     lambdaSaltClimRelax :: Inverse time scale for relaxation ( 1/s ).
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
C     trac_EvPrRn :: tracer concentration in Rain & Evap.
C        (notes: a) tracer content of Rain/Evap only used if both 
C                     NonLin_FrSurf & useRealFreshWater are set.
C                b) use model surface (local) value if set to UNSET_RL)
C     horiVertRatio      :: Ratio on units in vertical to units in horizontal.
C     recip_horiVertRatio  ( 1 if horiz in m and vertical in m ).
C                          ( g*rho if horiz in m and vertical in Pa ).
C     Ro_SeaLevel        :: standard position of Sea-Level in "R" coordinate, used as
C                          starting value (k=1) for vertical coordinate (rf(1)=Ro_SeaLevel)
C     bottomDragLinear   :: Drag coefficient built in to core dynamics
C      --"-"--  Quadratic  ( linear: 1/s, quadratic: 1/m )
      COMMON /PARM_R/ cg2dTargetResidual, cg2dTargetResWunit, 
     & cg2dpcOffDFac, cg3dTargetResidual,
     & delR, delRc, delX, delY,
     & deltaT, deltaTmom, deltaTtracer, deltaTfreesurf, deltaTClock,
     & abeps, startTime, 
     & phiMin, thetaMin, rSphere, recip_RSphere, f0, beta,
     & fCori, fCoriG, fCoriCos,
     & viscAh,  viscAhMax, viscAhGrid,
     & viscA4,  viscA4Max, viscA4Grid,
     & viscAr, viscAstrain, viscAtension,
     & diffKhT, diffK4T, diffKrT,
     & diffKhS, diffK4S, diffKrS,
     & delT, tauCD, rCD, freeSurfFac, implicSurfPress, implicDiv2Dflow,
     & hFacMin, hFacMinDz, hFacInf, hFacSup,
     & gravity, recip_Gravity, gBaro, rhonil, recip_rhonil, 
     & recip_rhoConst, rhoConst, 
     & rhoConstFresh, convertEmP2rUnit, tRef, sRef,
     & endTime, chkPtFreq, pchkPtFreq, dumpFreq, adjDumpFreq,
     & diagFreq, taveFreq, tave_lastIter, monitorFreq,
     & afFacMom, vfFacMom, pfFacMom, cfFacMom, foFacMom, mtFacMom,
     & cosPower, cAdjFreq, omega, rotationPeriod,
     & tauThetaClimRelax, lambdaThetaClimRelax,
     & tauSaltClimRelax, lambdaSaltClimRelax,
     & tauTr1ClimRelax, lambdaTr1ClimRelax, latBandClimRelax,
     & externForcingCycle, externForcingPeriod,
     & convertFW2Salt, temp_EvPrRn, salt_EvPrRn, trac_EvPrRn,
     & hFacMinDr, hFacMinDp,
     & horiVertRatio, recip_horiVertRatio,
     & ivdc_kappa, Ro_SeaLevel,
     & bottomDragLinear,bottomDragQuadratic

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
      _RL deltaTtracer
      _RL deltaTfreesurf
      _RL abeps
      _RL phiMin
      _RL thetaMin
      _RL rSphere
      _RL recip_RSphere
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
      _RL viscAhMax
      _RL viscAhGrid
      _RL viscAstrain
      _RL viscAtension
      _RL viscAr
      _RL viscA4 
      _RL viscA4Max
      _RL viscA4Grid
      _RL diffKhT 
      _RL diffKrT
      _RL diffK4T 
      _RL diffKhS 
      _RL diffKrS
      _RL diffK4S 
      _RL delt
      _RL tauCD
      _RL rCD
      _RL gravity
      _RL recip_gravity
      _RL gBaro
      _RL rhonil
      _RL recip_rhonil
      _RL rhoConst
      _RL recip_rhoConst
      _RL rhoConstFresh
      _RL convertEmP2rUnit
      _RL tRef(Nr)
      _RL sRef(Nr)
      _RS fCori(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fCoriG(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fCoriCos(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      _RL afFacMom
      _RL vfFacMom
      _RL pfFacMom
      _RL cfFacMom
      _RL foFacMom
      _RL mTFacMom
      _RL cosPower
      _RL cAdjFreq
      _RL omega
      _RL rotationPeriod
      _RL tauThetaClimRelax
      _RL lambdaThetaClimRelax
      _RL tauSaltClimRelax
      _RL lambdaSaltClimRelax
      _RL tauTr1ClimRelax
      _RL lambdaTr1ClimRelax
      _RL latBandClimRelax
      _RL externForcingCycle
      _RL externForcingPeriod
      _RL convertFW2Salt
      _RL temp_EvPrRn
      _RL salt_EvPrRn
      _RL trac_EvPrRn 
      _RL horiVertRatio
      _RL recip_horiVertRatio
      _RL ivdc_kappa
      _RL Ro_SeaLevel
      _RL bottomDragLinear
      _RL bottomDragQuadratic

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
      LOGICAL useKPP
      LOGICAL useGMRedi
      LOGICAL useOBCS
      LOGICAL useAIM
      LOGICAL useLand
      LOGICAL useGrdchk
      LOGICAL useECCO
      LOGICAL useSHAP_FILT
      LOGICAL useZONAL_FILT
      LOGICAL useFLT
      LOGICAL usePTRACERS
      LOGICAL useSBO
      LOGICAL useSEAICE
      LOGICAL useBulkForce
      LOGICAL useThSIce
      LOGICAL usefizhi
      LOGICAL usegridalt
      LOGICAL usediagnostics
      LOGICAL useEBM
      COMMON /PARM_PACKAGES/
     &        useKPP, useGMRedi, useOBCS, useAIM, useLand, useECCO, 
     &        useSHAP_FILT, useZONAL_FILT, useGrdchk, useFLT,
     &        usePTRACERS,  useSBO, useSEAICE, useThSIce, useBulkForce, 
     &        usefizhi,  usegridalt, usediagnostics, useEBM

C     Run-time flags for early-initialization of MNC
      LOGICAL
     &     useMNC,
     &     mnc_use_indir, mnc_use_outdir, mnc_outdir_date,
     &     mnc_echo_gvtypes, mnc_pickup_write, mnc_pickup_read,
     &     mnc_use_for_mon
      CHARACTER*(MAX_LEN_FNAM) mnc_outdir_str
      CHARACTER*(MAX_LEN_FNAM) mnc_indir_str
      COMMON /PARM_MNC/
     &     useMNC,
     &     mnc_use_indir, mnc_use_outdir, mnc_outdir_date, 
     &     mnc_echo_gvtypes, mnc_pickup_write, mnc_pickup_read, 
     &     mnc_outdir_str, mnc_indir_str, 
     &     mnc_use_for_mon


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
