C $Id: PARAMS.h,v 1.1 1998/05/25 20:21:07 cnh Exp $
C     Include file defining PARAMS common block variables.
C     These variables contain global constants used throughout the model.
C
C     PARAMS_F  : Floating point constants
C     fCori     : Reference coriolios parameter.
C     ronil     : Reference density of fluid,  kg/m^3.
C     G         : Gravitational acceleration,   m/s^2.
C     Cp        : Specific heat capacity of water.
C     dM        : Horizontal grid spacing, m.
C     dPhi      : North South angular spacing on spherical grid, radians.
C     dTheta    : West East angular spacing on spherical grid, radians.
C     phiMin    : Southern most extent of model on sphere, radians.
C     thetaMin  : Western extent of domain on sphere, radians.
C     delT      : Time step, seconds.
C     rdelT     : Reciprocal of time step, 1/seconds.
C     rZero     : Radius of bottom of domain on sphere. (Mean radius when the
C                 shallow atmosphere approximation is made), m.
C     toler3d   : Tolerance for three dimensional elliptic inverter.
C     divergMax : Maximum of range within which elliptic inverter tries to
C                 hold the maximum divergence.
C     divergMin : Minimum of range within which elliptic inverter tries to 
C                 hold the maximum divergence
C     divLim    : Limiting divergence. Model stops when either 3d or horizontal 
C                 divergence exceeds divLim.
C     toler2d   : Tolerance for two dimensional elliptic inverter.
C     divhMax   : Maximum of range within which elliptic inverter tries to
C                 hold the maximum divergence of depth integrated flow.
C     divhMin   : Minimum of range within which elliptic inverter tries to
C                 hold the maximum divergence of the integrated flow.
C     delPs     : Vertical separation between w points, Pa.
C     rdelPs    : Reciprocal of delps.
C     abEps     : Adams Bashforth time stepping stabilising factor
C                 (see - Potter - Computational Physics page 69. )
C     endTime   : Time in seconds at which integration will end.
C     startTime : TIme in seconds at which integration starts.
C     currentTime : Current simulation time in seconds.
C     cAdjFreq  : Frequency in seconds with which convective adjustment 
C                 performed.
C     dumpFreq  : Default frequency in seconds with which fields are dumped.
C     dStateFreq: Frequency with which state variables are dumped.
C     dPUFreq   : Frequency with which pick up information is dumped.
C     dInvFreq  : Frequency with which inverter information is dumped.
C     ths       : Initial temperature field vertical profile.
C     ssppt     : Initial salinity field vertical profile.
C     us        : Initial velocity field vertical profile
C     vs        : Initial velocity field vertical profile.
C     sBeta     : Saline contraction coefficient.
C     tAlpha    : Thermal expansion coefficient.
C     rAja      : Relative weight of D grid coriolis term to C grid term.
C     a2MomXY   : Laplacian momentum dissipation coefficient in horizontal.
C     a2MomZ    : Laplacian momentum dissipation coefficient in vertical.
C     a4MomXY   : Biharmonic momentum dissipation coefficient in horizontal.
C     a4MomExp  : Biharmonic momentum dissipation scaling coefficient.
C     a2TempXY  : Laplacian temperature dissipation coefficient in horizontal.
C     a2TempZ   : Laplacian temperature dissipation coefficient in vertical.
C     a4TempXY  : Biharmonic temperature dissipation coefficient in horizontal.
C     a2SaltXY  : Laplacian temperature dissipation coefficient in horizontal.
C     a2SaltZ   : Laplacian temperature dissipation coefficient in vertical.
C     a4SaltXY  : Biharmonic temperature dissipation coefficient in horizontal.
C     freeSurfFac : Implicit free surface factor for main diagonal
C     gBaro       : g for the barotropic mode.
C                 : Reducing Gbaro accelerates the convergence of the
C                 : two-d elliptic problem at the expense of distorting the
C                 : barotropic wave speeds.
C     psGam       : Factor for implicit "weight" on P_n+0.5
C                 : PsGam = 1 forwad, PsGam = 0.5 centered, PsGam = 0 lagged.
C     uDivAlpha   : Factor for weight on U_n+0.5 in free surface equation
C                 : UdivAlpha = 1.0 fully implicit
C                 : UdivAlpha = 0.5 centered
C                 : UdivAlpha = 0.0 fully explicit
C     tauCD       : CD relaxation time scale.
C                 : TauCD = dT fully C-grid ( noisy at
C                 :                           unresolved deformation radius )
C                 : TauCD >> dT toward D grid ( decouples )
C     rAja        : C-D relaxation using fractional coupling.
C     rhoRefSurf  : Reference density for surface layers. Used in evaluating ! Steric
C                 : steric anomaly.                                          ! Steric
C     aSyncFac    : Asynchronus time stepping factor.
C     epsDrag     : Bottom drag coefficient
      COMMON /PARAMS_F/ 
     & ronil, G, Cp, dM, dPhi, dTheta, phiMin, thetaMin, delT, rZero,
     & toler3d, divergMax, divergMin, toler2d, divhMax, divhMin, divLim,
     & delPs, rDelt, abEps, endTime, startTime, currentTime, cAdjFreq, 
     & dumpFreq, dStateFreq, dPUFreq, dInvFreq, dCkptFreq,
     & ths, ssppt, us, vs, sBeta, tAlpha, rDelps, rAja, tauCD,
     & a2MomXY,a2MomZ,a4MomXY,a2TempXY,a2TempZ,a4TempXY,a2SaltXY,a2SaltZ,
     & a4SaltXY, fCori,a4MomExp,
     & freeSurfFac, gBaro, psGam, uDivAlpha, 
     & rhoRefSurf,                                        ! Steric
     & aSyncFac, epsDrag
      REAL fCori
      REAL ronil
      REAL G
      REAL Cp
      REAL dM 
      REAL dPhi
      REAL dTheta
      REAL phiMin
      REAL thetaMin
      REAL delT
      REAL rZero
      REAL toler3d
      REAL divergMax
      REAL divergMin
      REAL toler2d
      REAL divhMax
      REAL divhMin
      REAL divLim
      REAL delPs(Nz)
CMF$LAYOUT delPs(:SERIAL)
      REAL rdelPs(Nz)
CMF$LAYOUT rdelPs(:SERIAL)
      REAL rDelt
      REAL abEps
      REAL currentTime
      REAL startTime
      REAL endTime
      REAL dumpFreq
      REAL dStateFreq
      REAL dPUFreq
      REAL dInvFreq
      REAL dCkptFreq
      REAL cAdjFreq
      REAL ths(Nz)
CMF$LAYOUT ths(:SERIAL)
      REAL ssppt(Nz)
CMF$LAYOUT ssppt(:SERIAL)
      REAL us(Nz)
CMF$LAYOUT us(:SERIAL)
      REAL vs(Nz)
CMF$LAYOUT vs(:SERIAL)
      REAL sBeta
      REAL tAlpha
      REAL rAja
      REAL tauCD
      REAL a2MomXY
      REAL a4MomXY
      REAL a4MomExp
      REAL a2MomZ
      REAL a2TempXY
      REAL a4TempXY
      REAL a2TempZ
      REAL a2SaltXY
      REAL a4SaltXY
      REAL a2SaltZ
      REAL freeSurfFac
      REAL gBaro
      REAL psGam
      REAL uDivAlpha
      REAL rhoRefSurf                                               ! Steric
      REAL aSyncFac
      REAL epsDrag
C
C     PARAMS_L: Logical constants.
C     separatePH    : TRUE  -> Calculate hydrostatic pressure by vertically 
C                              integrating g.rho.
C     separatePS    : TRUE  -> Find surface pressure which balances the depth
C                              inteegrated momentum equations separately.
C     nonHydrostatic: TRUE  -> Solve the full Navier-Stokes equation set with 
C                              prognostic equation for the vertical velocity.
C     momentumStepping: TRUE -> Step forward prognostic equations for U,V and W.
C     pressureStepping: TRUE -> Solve for pressure field.
C     tempStepping:     TRUE -> Step forward prognostic equations for T.
C     saltStepping:     TRUE -> Step forward prognostic equations for S.
C     momentumAdvection:TRUE -> Include non-linear momentume terms.
C     momentumDiffusion:TRUE -> Include momentum dissipation.
C     momBhDiiffusion:  TRUE -> Include biharmonic momentum dissipation.
C     metricTerms:      TRUE -> Include curvilinear coordinate forces.
C     coriolis:         TRUE -> Include coriolis terms.
C     verticalCoriolis: TRUE -> Include vertical components of coriolis force.
C     momentumForcing:  TRUE -> Include velocity field forcing.
C     saltForcing:      TRUE -> Include salinity forcing.
C     tempForcing:      TRUE -> Include temperature forcing.
C     freeSlipSide:     TRUE -> Apply free slip on side walls.
C     freeSlipTop:      TRUE -> Apply free slip at top.
C     freeSlipBottom:   TRUE -> Apply free slip at bottom.
C     tempAdvection:    TRUE -> Include advection of temperature.
C     tempDiffusion:    TRUE -> Include dissipation of temperature.
C     tempBhDiiffusion: TRUE -> Include biharmonic temperature dissipation.
C     saltAdvection:    TRUE -> Include advection of salt.
C     saltDiffusion:    TRUE -> Include dissipation of salinity.
C     saltBhDiiffusion: TRUE -> Include biharmonic salinity dissipation.
C     convectiveAdjustment: TRUE -> Include vertical mixing scheme.
C     buoyancy:         TRUE -> Include g.rho term in w equation.
C     gradHpnh:         TRUE -> Include ddx and ddy of pnh in u v momentum eqn
C     findD2pnhDZ2      TRUE -> Solve dirichlet problem for vertical gradient of
C                               pnh. Ignoring lateral interactions.
C     stepW     : Flag to control whether w is diagnosed or stepped forward.
C     stericEffect      TRUE -> Include steric anomaly in surface elevation.    ! Steric
C     OceanModel        TRUE -> G_prime = g*RHO_prime/RHONIL
C     AtmosphericModel  TRUE -> G_prime = g*T_prime/T_ref
C     InitialisationError TRUE -> Error detected during model initialisation phase.
C     IntegrationError    TRUE -> Error detected during model integration phase.
C     IntegrationError    TRUE -> Error detected during model integration phase.
C     pickupRun           TRUE -> Run is starting from previously saved model state.
      COMMON /PARAMS_L/
     & separatePH, separatePS, nonHydrostatic,
     & momentumStepping, pressureStepping, tempStepping, saltStepping,
     & momentumAdvection, momentumDiffusion, momBhDiffusion, metricTerms, 
     & coriolis, verticalCoriolis, momentumForcing, freeSlipSide, 
     & freeSlipBottom, freeSlipTop, tempAdvection, tempDiffusion, 
     & tempBhDiffusion, saltAdvection, saltDiffusion, saltBhDiffusion,
     & tempForcing, saltForcing, convectiveAdjustment, buoyancy, stepW,
     & gradHpnh, findD2pnhDZ2, 
     & stericEffect,                          ! Steric
     & OceanModel, AtmosphericModel, initialisationError, integrationError, pickupRun
      LOGICAL separatePh
      LOGICAL separatePs
      LOGICAL nonHydrostatic
      LOGICAL momentumStepping
      LOGICAL pressureStepping
      LOGICAL tempStepping
      LOGICAL saltStepping
      LOGICAL momentumAdvection
      LOGICAL momentumDiffusion
      LOGICAL momBhDiffusion
      LOGICAL metricTerms
      LOGICAL coriolis
      LOGICAL verticalCoriolis
      LOGICAL momentumForcing
      LOGICAL saltForcing
      LOGICAL tempForcing
      LOGICAL freeSlipSide
      LOGICAL freeSlipTop
      LOGICAL freeSlipBottom
      LOGICAL tempAdvection
      LOGICAL tempDiffusion
      LOGICAL tempBhDiffusion
      LOGICAL saltAdvection
      LOGICAL saltDiffusion
      LOGICAL saltBhDiffusion
      LOGICAL convectiveAdjustment
      LOGICAL buoyancy
      LOGICAL stepW
      LOGICAL gradHpnh
      LOGICAL findD2pnhDZ2
      LOGICAL stericEffect                                   ! Steric
      LOGICAL OceanModel                                     ! Atm
      LOGICAL AtmosphericModel                               ! Atm
      LOGICAL initialisationError
      LOGICAL integrationError
      LOGICAL pickupRun
C
C     PARAMS_I: Integer constants.
C     L                : Model grid x dimension.
C     M                : Model grid y dimension.
C     N                : Model grid z dimension.
C     max3dIt          : Maximum number of iterations three dimensional inverter
C                        perform.
C     max2dIt          : Maximum number of iterations two dimensional inverter
C                        will perform.
C     freqCheckToler3d : Frequeny at which three dimensional inverter checks
C                        for convergence.
C     freqCheckToler2d : Frequeny at which two dimensional inverter checks
C                        for convergence.
C     numberOfTimeSteps: No. of time steps the model will run.
C     nIter            : Simulation iteration number
C     dUnit            : Unit number used when reading miscellaneous data files.
C     stdin            : Unit number used for reading standard input.
C     stdout           : Unit number used for writing to standard output.
C     scrUnit1         : Unit number used for scratch dataset 1.
C     scrUnit2         : Unit number used for scratch dataset 2.
C     scrUnitError     : Unit number used for error message scratch dataset.
      COMMON /PARAMS_I/
     &       L, M, N,
     &       max3dIt, max2dIt, freqCheckToler3d, freqCheckToler2d, nIter,
     &       numberOfTimeSteps, dUnit, stdin, stdout, scrUnit1, scrUnit2, 
     &       scrUnitError
      INTEGER L
      INTEGER M
      INTEGER N
      INTEGER max3dIt
      INTEGER max2dIt
      INTEGER freqCheckToler3d
      INTEGER freqCheckToler2d
      INTEGER numberOfTimeSteps
      INTEGER nIter
      INTEGER dUnit
      INTEGER stdin
      INTEGER stdout
      INTEGER scrUnit1
      INTEGER scrUnit2
      INTEGER scrUnitError
C
C     PARAMS_C: Chracter constants
C     gridStyle : Type of grid to be used for discretising contiuous domain.
C     phSurfBC  : Boundary condition to use on hydrostatic pressure integration.
C     runDate   : Date and time on which integration performed.
C     runMachine: Computer identifier.
C     runTitle  : Title for job read from input data
C     equationOfState: Selects which method is used to evaluate density.
C     coriolisPlane: Selects coriolis variation to use.
C     bathySource: Controls reading of topography from a file.
C     puSuffix   : Suffix on "pickup" datasets.
      COMMON /PARAMS_C/
     & gridStyle,phSurfBC,runTitle, equationOfState,
     & coriolisPlane, bathySource, runDate, runMachine, puSuffix
      CHARACTER*80 gridStyle
      CHARACTER*80 phSurfBC
      CHARACTER*80 runDate
      CHARACTER*80 runMachine
      CHARACTER*80 runTitle
      CHARACTER*80 equationOfState
      CHARACTER*80 coriolisPlane  
      CHARACTER*80 bathySource
      CHARACTER*10 puSuffix
C
C     Constant value parameters used throughout model.
C     LAND  - Symbolic name for value used in masks to signify land!
C     WATER - Symbolic name for value used in masks to signify water!
C     MAXFN - Size of the filename buffer used in IO.
      REAL LAND
      PARAMETER ( LAND  = 0.                         )
      REAL WATER
      PARAMETER ( WATER = 1.                         )
      REAL UNSET_FLOAT
      PARAMETER ( UNSET_FLOAT = -1.2345e-6           )
      REAL UNSET_INT
      PARAMETER ( UNSET_INT   = -123456              )
      INTEGER MAXFN
      PARAMETER ( MAXFN = 80 )
C     Useful physical values
      REAL PI
      PARAMETER ( PI    = 3.14159265358979323844D0   )
      REAL deg2rad
      PARAMETER ( deg2rad = 2.D0*PI/360.D0               )
      REAL        ReferenceDensityForWater
      PARAMETER ( ReferenceDensityForWater   = 999.8D0 )
      REAL        ReferenceDensityForAir  
      PARAMETER ( ReferenceDensityForAir     = 1.0D0   )
      REAL        GravityEarth
      PARAMETER ( GravityEarth               = 9.81D0  )
      REAL        SpecificHeatCapacityAir
      PARAMETER ( SpecificHeatCapacityAir    = 1004D0  )
      REAL        SpecificHeatCapacityWater
      PARAMETER ( SpecificHeatCapacityWater  = 3900D0  )
      REAL        OneMinute
      PARAMETER ( OneMinute                  = 60.D0   )
      REAL        OneHour 
      PARAMETER ( OneHour                    = 60.D0 * OneMinute )
      REAL        OneDay
      PARAMETER ( OneDay                     = 24.D0 * OneHour   )
      REAL        OneMonth
      PARAMETER ( OneMonth                   = 30.D0 * OneDay    )
      REAL        OneYear
      PARAMETER ( OneYear                    = 360.D0* OneDay    )
      REAL        RadiusEarth 
      PARAMETER ( RadiusEarth                = 6.37D6          )
      REAL        alphaSalt                  ! Haline contraction coefficient
      PARAMETER ( alphaSalt                  = 7.4D-4          )
      REAL        alphaTemperature           ! Thermal expansion coefficient
      PARAMETER ( alphaTemperature           = 2.D-4           )
      REAL        omega     
      PARAMETER ( omega                      = 2.D0*PI/OneDay    )
