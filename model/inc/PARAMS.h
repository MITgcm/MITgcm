C $Header: /u/gcmpack/MITgcm/model/inc/PARAMS.h,v 1.4 1998/04/27 04:24:22 cnh Exp $
C
C     /==========================================================\
C     | PARAMS.h                                                 |
C     | o Header file defining model "parameters".               |
C     |==========================================================|
C     | The values from the model's standard input file are      |
C     | stored into the variables held here. Notes describing    |
C     | the parameters can also be found here.                   |
C     \==========================================================/

C--   Contants
C     nOBands - No. of offline data time bands
      INTEGER nOBands
      PARAMETER ( nOBands = 12 )
C     Useful physical values
      Real*8 PI
      PARAMETER ( PI    = 3.14159265358979323844D0   )
      Real*8 deg2rad
      PARAMETER ( deg2rad = 2.D0*PI/360.D0           )

C--   COMMON /PARM_C/ Character valued parameters used by the model.
C     oBandId  - Offline dataset identifiers for different periods.
      COMMON /PARM_C/ oBandId
      CHARACTER*3 oBandId(nOBands)
C--   COMMON /PARM_I/ Integer valued parameters used by the model.
C     cg2dMaxIters        - Maximum number of iterations in the
C                           two-dimensional con. grad solver.
C     cg2dChkResFreq      - Frequency with which to check residual
C                           in con. grad solver.
C     nIter0              - Start time-step number of for this run
C     nTimeSteps          - Number of timesteps to execute
C     numStepsPerPickup   - For offline setup. Frequency of pickup
C                           of flow fields.
      COMMON /PARM_I/
     &        cg2dMaxIters,
     &        cg2dChkResFreq,
     &        nIter0, nTimeSteps,
     &        numStepsPerPickup
      INTEGER cg2dMaxIters
      INTEGER cg2dChkResFreq
      INTEGER nIter0
      INTEGER nTimeSteps
      INTEGER numStepsPerPickup

C--   COMMON /PARM_L/ Logical valued parameters used by the model.
C     usingCartesianGrid - If TRUE grid generation will be in a cartesian
C                          coordinate frame.
C     usingSphericalPolarGrid - If TRUE grid generation will be in a 
C                               spherical polar frame.
C     momViscosity  - Flag which turns momentum friction terms on and off.
C     momAdvection  - Flag which turns advection of momentum on and off.
C     momForcing    - Flag which turns external forcing of momentum on
C                     and off.
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
      COMMON /PARM_L/ usingCartesianGrid, usingSphericalPolarGrid,
     & momViscosity, momAdvection, momForcing, useCoriolis,
     & tempDiffusion, tempAdvection, tempForcing,
     & saltDiffusion, saltAdvection, saltForcing
      LOGICAL usingCartesianGrid
      LOGICAL usingSphericalPolarGrid
      LOGICAL momViscosity
      LOGICAL momAdvection
      LOGICAL momForcing
      LOGICAL useCoriolis
      LOGICAL tempDiffusion
      LOGICAL tempAdvection
      LOGICAL tempForcing
      LOGICAL saltDiffusion
      LOGICAL saltAdvection
      LOGICAL saltForcing

C--   COMMON /PARM_R/ "Real" valued parameters used by the model.
C     cg2dTargetResidual
C               - Target residual for cg2d solver.
C     delZ      - Vertical grid spacing ( m ) - delZ is the distance
C                 between "w" surfaces.
C     delX      - Separation between cell faces (m) or (deg), depending
C     delY        on input flags.
C     gravity   - Accel. due to gravity ( m/s^2 )
C     ronil     - Reference density
C     startTime - Start time for model ( s )
C     phiMin    - Latitude of southern most cell face.
C     thetaMin  - Longitude of western most cell face (this
C                 is an "inert" parameter but it is included
C                 to make geographical references simple.)
C     rSphere   - Radius of sphere for a spherical polar grid ( m ).
C     f0        - Reference coriolis parameter ( 1/s )
C                 ( Southern edge f for beta plane )
C     beta      - df/dy ( s^-1.m^-1 )
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
C     tAlpha    - Linear EOS thermal expansion coefficient ( 1/degree ).
C     sBeta     - Linear EOS haline contraction coefficient.
C     deltaT    - Default timestep ( s )
C     deltaTMom    - Timestep for momemtum equations ( s )
C     deltaTtracer - Timestep for tracer equations ( s )
C     tauCD     - CD scheme coupling timescale ( 1/s )
C     rCD       - CD scheme normalised coupling parameter ( 0-1 )
C     startTime - Starting time for this integration ( s ).
C     endTime   - Ending time for this integration ( s ).
C     chkPtFreq - Frequency of check pointing ( s ).
C     dumpFreq  - Frequency with which model state is written to
C                 post-processing files ( s ).
      COMMON /PARM_R/ cg2dTargetResidual, delZ, delX, delY, deltaT,
     & deltaTmom, deltaTtracer, abeps, startTime, phiMin, thetaMin, 
     & rSphere, f0, fCori, beta, viscAh, viscAz, viscA4, diffKhT, diffKzT, 
     & diffK4T, diffKhS, diffKzS, diffK4S, delT, tauCD, rCD,
     & tAlpha, sBeta, gravity, rhonil, tRef, sRef,
     & endTime, chkPtFreq, dumpFreq
      REAL cg2dTargetResidual
      REAL delZ(Nz)
      REAL delX(Nx)
      REAL delY(Ny)
      REAL deltaT
      REAL deltaTmom
      REAL deltaTtracer
      REAL abeps
      REAL phiMin
      REAL thetaMin
      REAL rSphere
      REAL f0
      _RL  beta
      REAL viscAh
      REAL viscAz
      REAL viscA4 
      REAL diffKhT 
      REAL diffKzT
      REAL diffK4T 
      REAL diffKhS 
      REAL diffKzS
      REAL diffK4S 
      REAL delt
      REAL tauCD
      REAL rCD
      REAL tAlpha
      REAL sBeta
      REAL gravity
      REAL rhonil
      REAL tRef(Nz)
      REAL sRef(Nz)
      real Fcori(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL startTime
      REAL endTime
      REAL chkPtFreq
      REAL dumpFreq
      COMMON /PARM_A/ HeatCapacity_Cp,
     &                Lamba_theta
      REAL HeatCapacity_Cp
      REAL Lamba_theta
