C $Header: /u/gcmpack/MITgcm/model/inc/PARAMS.h,v 1.23 1998/08/15 16:55:48 cnh Exp $
C
C     /==========================================================\
C     | PARAMS.h                                                 |
C     | o Header file defining model "parameters".               |
C     |==========================================================|
C     | The values from the model's standard input file are      |
C     | stored into the variables held here. Notes describing    |
C     | the parameters can also be found here.                   |
C     \==========================================================/

C     Macros for special grid options
#include "PARAMS_MACROS.h"

C--   Contants
C     Useful physical values
      Real*8 PI
      PARAMETER ( PI    = 3.14159265358979323844D0   )
      Real*8 deg2rad
      PARAMETER ( deg2rad = 2.D0*PI/360.D0           )

C     Symbolic values
C     precXXXX - Used to indicate what precision to use for
C                dumping model state.
      INTEGER precFloat32
      PARAMETER ( precFloat32 = 0 )
      INTEGER precFloat64
      PARAMETER ( precFloat64 = 1 )

C     Checkpoint data
      INTEGER maxNoChkptLev
      PARAMETER ( maxNoChkptLev = 2 )

C--   COMMON /PARM_C/ Character valued parameters used by the model.
C     checkPtSuff - List of checkpoint file suffices
C     bathyFile   - File containing bathymetry. If not defined bathymetry
C                   is taken from inline function.
C     hydrogThetaFile - File containing initial hydrographic data for potential
C                       temperature.
C     hydrogSaltFile  - File containing initial hydrographic data for salinity.
C     zonalWindFile   - File containing zonal wind data
C     meridWindFile   - File containing meridional wind data
C     thetaClimFile   - File containing theta climataology used
C                       in relaxation term -lambda(theta-theta*)
C     saltClimFile    - File containing salt climataology used
C                       in relaxation term -lambda(salt-salt*)
      COMMON /PARM_C/ checkPtSuff,
     &                bathyFile, hydrogThetaFile, hydrogSaltFile,
     &                zonalWindFile, meridWindFile, thetaClimFile,
     &                saltClimFile
      CHARACTER*(5) checkPtSuff(maxNoChkptLev)
      CHARACTER*(MAX_LEN_FNAM) bathyFile
      CHARACTER*(MAX_LEN_FNAM) hydrogThetaFile
      CHARACTER*(MAX_LEN_FNAM) hydrogSaltFile
      CHARACTER*(MAX_LEN_FNAM) zonalWindFile
      CHARACTER*(MAX_LEN_FNAM) meridWindFile
      CHARACTER*(MAX_LEN_FNAM) thetaClimFile
      CHARACTER*(MAX_LEN_FNAM) saltClimFile

C--   COMMON /PARM_I/ Integer valued parameters used by the model.
C     cg2dMaxIters        - Maximum number of iterations in the
C                           two-dimensional con. grad solver.
C     cg2dChkResFreq      - Frequency with which to check residual
C                           in con. grad solver.
C     nIter0              - Start time-step number of for this run
C     nTimeSteps          - Number of timesteps to execute
C     numStepsPerPickup   - For offline setup. Frequency of pickup
C                           of flow fields.
C     writeStatePrec      - Precision used for writing model state.
C     writeBinaryPrec     - Precision used for writing binary files
C     readBinaryPrec      - Precision used for reading binary files
C     nCheckLev           - Holds current checkpoint level
      COMMON /PARM_I/
     &        cg2dMaxIters,
     &        cg2dChkResFreq,
     &        nIter0, nTimeSteps,
     &        numStepsPerPickup,
     &        writeStatePrec, nCheckLev,
     &        writeBinaryPrec, readBinaryPrec
      INTEGER cg2dMaxIters
      INTEGER cg2dChkResFreq
      INTEGER nIter0
      INTEGER nTimeSteps
      INTEGER numStepsPerPickup
      INTEGER writeStatePrec
      INTEGER writeBinaryPrec
      INTEGER readBinaryPrec
      INTEGER nCheckLev

C--   COMMON /PARM_L/ Logical valued parameters used by the model.
C     usingCartesianGrid - If TRUE grid generation will be in a cartesian
C                          coordinate frame.
C     usingSphericalPolarGrid - If TRUE grid generation will be in a 
C                               spherical polar frame.
C     momViscosity  - Flag which turns momentum friction terms on and off.
C     momAdvection  - Flag which turns advection of momentum on and off.
C     momForcing    - Flag which turns external forcing of momentum on
C                     and off.
C     momPressureForcing - Flag which turns pressure term in momentum equation
C                          on and off.
C     metricTerms   - Flag which turns metric terms on or off.
C     usingSphericalPolarMTerms - If TRUE use spherical polar metric terms.
C     useCoriolis   - Flag which turns the coriolis terms on and off.
C     tempDiffusion - Flag which turns diffusion of temperature on
C                     and off.
C     tempAdvection - Flag which turns advection of temperature on
C                     and off.
C     tempForcing   - Flag which turns external forcing of temperature on
C                     and off.
C     saltDiffusion - Flag which turns diffusion of salinit on
C                     and off.
C     saltAdvection - Flag which turns advection of salinit on
C                     and off.
C     saltForcing   - Flag which turns external forcing of salinit on
C                     and off.
C     implicitFreeSurface - Set to true to use implcit free surface
C     rigidLid            - Set to true to use rigid lid
C     momStepping   - Turns momentum equation time-stepping off
C     tempStepping  - Turns temperature equation time-stepping off
C     saltStepping  - Turns salinity equation time-stepping off
C     useConstantF  - Coriolis parameter set to f0
C     useBetaPlaneF - Coriolis parameter set to f0 + beta.y
C     useSphereF    - Coriolis parameter set to 2.omega.sin(phi)
C     implicitDiffusion - Turns implicit vertical diffusion on
C     doThetaClimRelax - Set true if relaxation to temperature
C                        climatology is required.
C     doSaltClimRelax  - Set true if relaxation to salinity
C                        climatology is required.
C     periodicExternalForcing - Set true if forcing is time-dependant
      COMMON /PARM_L/ usingCartesianGrid, usingSphericalPolarGrid,
     & momViscosity, momAdvection, momForcing, useCoriolis, momPressureForcing,
     & tempDiffusion, tempAdvection, tempForcing,
     & saltDiffusion, saltAdvection, saltForcing,
     & implicitFreeSurface, rigidLid,
     & momStepping, tempStepping, saltStepping,
     & metricTerms, usingSphericalPolarMTerms,
     & useConstantF, useBetaPlaneF, useSphereF,
     & implicitDiffusion, doThetaClimRelax, doSaltClimRelax,
     & periodicExternalForcing
      LOGICAL usingCartesianGrid
      LOGICAL usingSphericalPolarGrid
      LOGICAL usingSphericalPolarMTerms
      LOGICAL momViscosity
      LOGICAL momAdvection
      LOGICAL momForcing
      LOGICAL momPressureForcing
      LOGICAL useCoriolis
      LOGICAL tempDiffusion
      LOGICAL tempAdvection
      LOGICAL tempForcing
      LOGICAL saltDiffusion
      LOGICAL saltAdvection
      LOGICAL saltForcing
      LOGICAL implicitFreeSurface
      LOGICAL rigidLid
      LOGICAL momStepping
      LOGICAL tempStepping
      LOGICAL saltStepping
      LOGICAL metricTerms
      LOGICAL useConstantF
      LOGICAL useBetaPlaneF
      LOGICAL useSphereF
      LOGICAL implicitDiffusion
      LOGICAL doThetaClimRelax
      LOGICAL doSaltClimRelax
      LOGICAL periodicExternalForcing

C--   COMMON /PARM_R/ "Real" valued parameters used by the model.
C     cg2dTargetResidual
C               - Target residual for cg2d solver.
C     cg2dpcOffDFac - Averaging weight for preconditioner off-diagonal.
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
C     delR      - Vertical grid spacing ( units of r ).
C     delX      - Separation between cell faces (m) or (deg), depending
C     delY        on input flags.
C     gravity   - Accel. due to gravity ( m/s^2 )
C     gBaro     - Accel. due to gravity used in barotropic equation ( m/s^2 )
C     ronil     - Reference density
C     startTime - Start time for model ( s )
C     phiMin    - Latitude of southern most cell face.
C     thetaMin  - Longitude of western most cell face (this
C                 is an "inert" parameter but it is included
C                 to make geographical references simple.)
C     rSphere   - Radius of sphere for a spherical polar grid ( m ).
C     rRSphere  - Reciprocal radius of sphere for a spherical polar grid ( m ).
C     f0        - Reference coriolis parameter ( 1/s )
C                 ( Southern edge f for beta plane )
C     beta      - df/dy ( s^-1.m^-1 )
C     omega     - Angular velocity ( rad/s )
C     viscAh    - Eddy viscosity coeff. for mixing of
C                 momentum laterally ( m^2/s )
C     viscAz    - Eddy viscosity coeff. for mixing of
C                 momentum vertically ( m^2/s )
C     viscA4    - Biharmonic viscosity coeff. for mixing of
C                 momentum laterally ( m^4/s )
C     diffKhT   - Laplacian diffusion coeff. for mixing of
C                 heat laterally ( m^2/s )
C     diffKzT   - Laplacian diffusion coeff. for mixing of
C                 heat vertically ( m^2/s )
C     diffK4T   - Biharmonic diffusion coeff. for mixing of
C                 heat laterally ( m^4/s )
C     diffKhS  -  Laplacian diffusion coeff. for mixing of
C                 salt laterally ( m^2/s )
C     diffKzS   - Laplacian diffusion coeff. for mixing of
C                 salt vertically ( m^2/s )
C     diffK4S   - Biharmonic diffusion coeff. for mixing of
C                 salt laterally ( m^4/s )
C     deltaT    - Default timestep ( s )
C     deltaTClock  - Timestep used as model "clock". This determines the
C                    IO frequencies and is used in tagging output. It can
C                    be totally different to the dynamical time. Typically
C                    it will be the deep-water timestep for accelerated runs.
C                    Frequency of checkpointing and dumping of the model state
C                    are referenced to this clock. ( s )
C     deltaTMom    - Timestep for momemtum equations ( s )
C     deltaTtracer - Timestep for tracer equations ( s )
C     freesurfFac  - Parameter to turn implicit free surface term on or off
C                    freesurfac = 1. uses implicit free surface
C                    freesurfac = 0. uses rigid lid
C     hFacMin   - Minimum fraction size of a cell (affects hFacC etc...)
C     hFacMinDz - Minimum dimesional size of a cell (affects hFacC etc...)
C     tauCD     - CD scheme coupling timescale ( 1/s )
C     rCD       - CD scheme normalised coupling parameter ( 0-1 )
C     GMmaxslope  - max. slope allowed in GM/Redi tensor
C     GMlength  - Length to use in Visbeck et al. formula for K (m)
C     GMalpha   - alpha to use in Visbeck et al. formula for K
C     GMdepth   - Depth over which to integrate Richardson # (Visbeck et al.)
C     GMkbackground - background value of GM/Redi coefficient
C     GMmaxval  - max. value of KapGM allowed in GM/Redi scheme
C     startTime - Starting time for this integration ( s ).
C     endTime   - Ending time for this integration ( s ).
C     chkPtFreq  - Frequency of rolling check pointing ( s ).
C     pChkPtFreq - Frequency of permanent check pointing ( s ).
C     dumpFreq  - Frequency with which model state is written to
C                 post-processing files ( s ).
C     taveFreq  - Frequency with which time-averaged model state is written to
C                 post-processing files ( s ).
C     afFacMom  - Advection of momentum term scaling parameter
C     vfFacMom  - Momentum viscosity scaling parameter
C     pfFacMom  - Momentum pressure forcing parameter
C     cfFacMom  - Coriolis term scaling parameter
C     foFacMom  - Momentum forcing scaling parameter
C     mtFacMom  - Metric terms scaling parameter
C     cAdjFreq  - Frequency of convective adjustment
C     tauThetaClimRelax - Relaxation to climatology time scale ( s ).
C     lambdaThetaClimRelax - Inverse time scale for relaxation ( 1/s ).
C     tauSaltClimRelax - Relaxation to climatology time scale ( s ).
C     lambdaSaltClimRelax - Inverse time scale for relaxation ( 1/s ).
C     externForcingPeriod - Is the period of which forcing varies (eg. 1 month)
C     externForcingCycle - Is the repeat time of the forcing (eg. 1 year)
C                          (note: externForcingCycle must be an integer
C                           number times externForcingPeriod)
      COMMON /PARM_R/ cg2dTargetResidual, cg2dpcOffDFac, delR, delX, delY, 
     & deltaT,deltaTmom, deltaTtracer, deltaTClock,abeps, startTime, phiMin, 
     & thetaMin, rSphere, rRSphere, f0, fCori, beta, viscAh, viscAz, viscA4, 
     & diffKhT, diffKzT, diffK4T, diffKhS, diffKzS, diffK4S, delT, 
     & tauCD, rCD, freeSurfFac, hFacMin, hFacMinDz,
     & GMmaxslope,GMlength,GMalpha,GMdepth,GMkbackground,GMmaxval,
     & gravity, gBaro, rhonil, tRef, sRef,
     & endTime, chkPtFreq, pchkPtFreq, dumpFreq, taveFreq,
     & afFacMom, vfFacMom, pfFacMom, cfFacMom, foFacMom, mtFacMom,
     & cAdjFreq, omega, tauThetaClimRelax, lambdaThetaClimRelax,
     & tauSaltClimRelax, lambdaSaltClimRelax,
     & externForcingCycle, externForcingPeriod
      _RL cg2dTargetResidual
      _RL cg2dpcOffDFac
      _RL delR(Nz)
      _RL delX(Nx)
      _RL delY(Ny)
      _RL deltaT
      _RL deltaTClock
      _RL deltaTmom
      _RL deltaTtracer
      _RL abeps
      _RL phiMin
      _RL thetaMin
      _RL rSphere
      _RL rRSphere
      _RL f0
      _RL freeSurfFac
      _RL hFacMin
      _RL hFacMinDz
      _RL beta
      _RL viscAh
      _RL viscAz
      _RL viscA4 
      _RL diffKhT 
      _RL diffKzT
      _RL diffK4T 
      _RL diffKhS 
      _RL diffKzS
      _RL diffK4S 
      _RL delt
      _RL tauCD
      _RL rCD
      _RL GMmaxslope
      _RL GMlength
      _RL GMalpha
      _RL GMdepth
      _RL GMkbackground
      _RL GMmaxval
      _RL gravity
      _RL gBaro
      _RL rhonil
      _RL tRef(Nz)
      _RL sRef(Nz)
      _RS Fcori(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL startTime
      _RL endTime
      _RL chkPtFreq
      _RL pChkPtFreq
      _RL dumpFreq
      _RL taveFreq
      _RL afFacMom
      _RL vfFacMom
      _RL pfFacMom
      _RL cfFacMom
      _RL foFacMom
      _RL mTFacMom
      _RL cAdjFreq
      _RL omega
      _RL tauThetaClimRelax
      _RL lambdaThetaClimRelax
      _RL tauSaltClimRelax
      _RL lambdaSaltClimRelax
      _RL externForcingCycle
      _RL externForcingPeriod

      COMMON /PARM_A/ HeatCapacity_Cp,
     &                Lamba_theta
      _RL HeatCapacity_Cp
      _RL Lamba_theta

C Equation of State (polynomial coeffients)
      COMMON /PARM_EOS_NL/ eosC,eosSig0,eosRefT,eosRefS
      _RL eosC(9,Nz+1),eosSig0(Nz+1),eosRefT(Nz+1),eosRefS(Nz+1)
C Linear equation of state
C     tAlpha    - Linear EOS thermal expansion coefficient ( 1/degree ).
C     sBeta     - Linear EOS haline contraction coefficient.
      COMMON /PARM_EOS_LIN/ tAlpha,sBeta,eosType
      _RL tAlpha
      _RL sBeta
      character*(6) eosType

