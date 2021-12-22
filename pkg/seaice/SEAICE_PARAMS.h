C     *==========================================================*
C     | SEAICE_PARAMS.h
C     | o Basic parameter header for sea ice model.
C     *==========================================================*

C--   COMMON /SEAICE_PARM_L/ Logical parameters of sea ice model.
C - dynamics:
C     SEAICEuseDYNAMICS :: If false, do not use dynamics;
C                          default is to use dynamics.
C     SEAICEuseFREEDRIFT :: If True use free drift velocity instead of EVP
C                           or LSR
C     SEAICEuseStrImpCpl:: If true use strongly implicit coupling formulation
C                          for LSR solver (Hutchings et al 2004,
C                          Ocean Modelling, eq.44)
C     SEAICEuseEVP      :: If true use elastic viscous plastic solver
C     SEAICEuseEVPstar  :: If true use modified elastic viscous plastic
C                          solver (EVP*) by Lemieux et al (2012)
C     SEAICEuseEVPrev   :: If true use "revisited" elastic viscous plastic
C                          solver following Bouillon et al. (2013), very similar
C                          to EVP*, but uses fewer implicit terms and drops
C                          one 1/e^2 in equations for sigma2 and sigma12
C     SEAICEuseEVPpickup :: Set to false in order to start EVP solver with
C                          non-EVP pickup files.  Default is true.
C                          Applied only if SEAICEuseEVP=.TRUE.
C     SEAICEuseMultiTileSolver :: in LSR, use full domain tri-diagonal solver
C     SEAICEuseLSR      :: If true, use default Picard solver with Line-
C                          Successive(-over)-Relaxation, can also be true
C                          if LSR is used as a preconditioner for the
C                          non-linear JFNK solver
C     SEAICEusePicardAsPrecon :: If true, allow SEAICEuseLSR = .TRUE. as a
C                          preconditioner for non-linear JFNK problem (def. = F)
C     SEAICEuseKrylov   :: If true, use matrix-free Krylov solver with Picard
C                          solver instead of LSR (default: false)
C     SEAICEuseJFNK     :: If true, use Jacobi-free Newton-Krylov solver
C                          instead of LSR (default: false)
C     SEAICEuseIMEX     :: use IMplicit/EXplicit scheme with JFNK
C     SEAICEuseTEM      :: to use the truncated ellipse method (see Geiger et al.
C                          1998) set this parameter to true, default is false
C     SEAICEuseMCS      :: to use the Mohr-Coulomb yield curve with a shear
C                          only flow rule (Ip et al 1991), set this parameter to
C                          true, default is false
C     SEAICEuseMCE      :: to use the Mohr-Coulomb yield curve with elliptical
C                          plastic potential (similarly to Hibler and Schulson
C                          2000 without the elliptical cap) set this parameter
C                          to true, default is false
C     SEAICEuseTD       :: to use the teardrop yield curve (Zhang and Rothrock,
C                          2005) set this parameter to true, default is false
C     SEAICEusePL       :: to use the parabolic lens yield curve (Zhang and
C                          Rothrock, 2005) set this parameter to true,
C                          default is false
C     SEAICEuseTilt     :: If true then include surface tilt term in dynamics
C     SEAICEuseMetricTerms :: use metric terms for dynamics solver
C                          (default = .true. )
C     SEAICE_no_slip    :: apply no slip boundary conditions to seaice velocity
C     SEAICE_2ndOrderBC :: apply 2nd order no slip boundary conditions (works
C                          only with EVP, JFNK or KRYLOV solver, default=F)
C     SEAICE_maskRHS    :: mask the RHS of the solver where there is no ice
C     SEAICE_clipVelocities :: clip velocities to +/- 40cm/s
C     SEAICEaddSnowMass :: in computing seaiceMass, add snow contribution
C                          default is .TRUE.
C     useHB87stressCoupling :: use an intergral over ice and ocean surface
C                          layer to define surface stresses on ocean
C                          following Hibler and Bryan (1987, JPO)
C     SEAICEupdateOceanStress :: If TRUE, update ocean surface stress
C                                accounting for seaice cover (default= T)
C     SEAICEuseBDF2     :: use 2nd-order backward difference approach
C                          for momentum equations as described in
C                          Lemieux et al. 2014, JCP
C                          so far only implemented for JFNK-solver
C     useHibler79IceStrength :: if true original ice strength parameterization
C                          other use Rothrock (1975) parameterization based
C                          on energetics and an ice thickness distribution
C                          (default = .true.)
C     SEAICEscaleSurfStress :: if TRUE, scale ice-ocean and ice-atmosphere
C                          stress on ice by concenration (AREA) following
C                          Connolley et al. (2004), JPO. (default = .TRUE.)
C     SEAICEsimpleRidging :: use Hibler(1979) ridging (default=.true.)
C     SEAICEuseLinRemapITD :: use linear remapping (Lipscomb et al. 2001)
C                             .TRUE. by default
C - advection:
C     SEAICEuseFluxForm :: use flux form for advection and diffusion
C                          of seaice
C     SEAICEadvHeff     :: turn on advection of effective thickness
C                          (default = .true.)
C     SEAICEadvArea     :: turn on advection of fraction area
C                          (default = .true.)
C     SEAICEadvSnow     :: turn on advection of snow (does not work with
C                          non-default Leap-frog scheme for advection)
C     SEAICEadvSalt     :: turn on advection of salt (does not work with
C                          non-default Leap-frog scheme for advection)
C     SEAICEmultiDimAdvection:: internal flag, set to false if any sea ice
C                          variable uses a non-multi-dimensional advection
C                          scheme
C     SEAICEmomAdvection:: turn on advection of momentum (default = .false.)
C     SEAICEhighOrderVorticity :: momentum advection parameters analogous to
C     SEAICEupwindVorticity    :: highOrderVorticity, upwindVorticity,
C     SEAICEuseAbsVorticity    :: useAbsVorticity, useJamartMomAdv for vector
C     SEAICEuseJamartMomAdv    :: invariant momentum in the ocean
C - thermodynamics:
C     usePW79thermodynamics :: use "0-layer" thermodynamics as described in
C                           Parkinson and Washington (1979) and Hibler (1979)
C     SEAICE_useMultDimSnow :: use same fixed pdf for snow as for
C                              multi-thickness-category ice (default=.TRUE.)
C     SEAICEuseFlooding  :: turn on scheme to convert submerged snow into ice
C     SEAICEheatConsFix  :: If true then fix ocn<->seaice advective heat flux.
C     useMaykutSatVapPoly :: use Maykut Polynomial for saturation vapor pressure
C                         instead of extended temp-range exponential law; def=F.
C     SEAICE_mcPheeStepFunc :: use step function (not linear tapering) in
C                           ocean-ice turbulent flux
C     SEAICE_doOpenWaterGrowth :: use open water heat flux directly to grow ice
C                           (when false cool ocean, and grow later if needed)
C     SEAICE_doOpenWaterMelt   :: use open water heat flux directly to melt ice
C                           (when false warm ocean, and melt later if needed)
C     SEAICE_growMeltByConv :: grow/melt according to convergence of turbulence
C                              and conduction, rather than in two steps (default)
C     SEAICE_salinityTracer    :: use SItracer to exchange and trace ocean
C                           salt in ice
C     SEAICE_ageTracer         :: use SItracer to trace the age of ice
C     SEAICErestoreUnderIce :: restore surface T/S also underneath ice
C                          ( default is false )
C - other (I/O, ...):
C     SEAICEwriteState  :: If true, write sea ice state to file;
C                          default is false.
C     SEAICE_tave_mdsio :: write TimeAverage output using MDSIO
C     SEAICE_dump_mdsio :: write snap-shot output   using MDSIO
C     SEAICE_mon_stdio  :: write monitor to std-outp
C     SEAICE_tave_mnc   :: write TimeAverage output using MNC
C     SEAICE_dump_mnc   :: write snap-shot output   using MNC
C     SEAICE_mon_mnc    :: write monitor to netcdf file
      LOGICAL
     &     SEAICEuseDYNAMICS, SEAICEuseFREEDRIFT, SEAICEuseStrImpCpl,
     &     SEAICEuseEVP, SEAICEuseEVPstar, SEAICEuseEVPrev,
     &     SEAICEuseEVPpickup,
     &     SEAICEuseMultiTileSolver,
     &     SEAICEuseLSR, SEAICEuseKrylov,
     &     SEAICEuseJFNK, SEAICEuseIMEX, SEAICEuseBDF2,
     &     SEAICEusePicardAsPrecon,
     &     useHibler79IceStrength, SEAICEsimpleRidging,
     &     SEAICEuseLinRemapITD, SEAICEuseTD, SEAICEusePL,
     &     SEAICEuseTEM, SEAICEuseTilt, SEAICEuseMetricTerms,
     &     SEAICEuseMCS, SEAICEuseMCE,
     &     SEAICE_no_slip, SEAICE_2ndOrderBC,
     &     SEAICE_maskRHS, SEAICEscaleSurfStress,
     &     SEAICE_clipVelocities, SEAICEaddSnowMass,
     &     useHB87stressCoupling, SEAICEupdateOceanStress,
     &     SEAICEuseFluxForm, SEAICEadvHeff, SEAICEadvArea,
     &     SEAICEmultiDimAdvection,
     &     SEAICEadvSnow, SEAICEadvSalt, SEAICEmomAdvection,
     &     SEAICEhighOrderVorticity, SEAICEupwindVorticity,
     &     SEAICEuseAbsVorticity, SEAICEuseJamartMomAdv,
     &     usePW79thermodynamics,
     &     SEAICE_useMultDimSnow, SEAICEuseFlooding, SEAICEheatConsFix,
     &     useMaykutSatVapPoly, SEAICE_mcPheeStepFunc,
     &     SEAICE_doOpenWaterGrowth, SEAICE_doOpenWaterMelt,
     &     SEAICE_salinityTracer, SEAICE_ageTracer,
     &     SEAICErestoreUnderIce, SEAICE_growMeltByConv,
     &     SEAICEwriteState,
     &     SEAICE_tave_mdsio, SEAICE_dump_mdsio, SEAICE_mon_stdio,
     &     SEAICE_tave_mnc,   SEAICE_dump_mnc,   SEAICE_mon_mnc
      COMMON /SEAICE_PARM_L/
     &     SEAICEuseDYNAMICS, SEAICEuseFREEDRIFT, SEAICEuseStrImpCpl,
     &     SEAICEuseEVP, SEAICEuseEVPstar, SEAICEuseEVPrev,
     &     SEAICEuseEVPpickup,
     &     SEAICEuseMultiTileSolver,
     &     SEAICEuseLSR, SEAICEuseKrylov,
     &     SEAICEuseJFNK, SEAICEuseIMEX, SEAICEuseBDF2,
     &     SEAICEusePicardAsPrecon,
     &     useHibler79IceStrength, SEAICEsimpleRidging,
     &     SEAICEuseLinRemapITD, SEAICEuseTD, SEAICEusePL,
     &     SEAICEuseTEM, SEAICEuseTilt, SEAICEuseMetricTerms,
     &     SEAICEuseMCS, SEAICEuseMCE,
     &     SEAICE_no_slip, SEAICE_2ndOrderBC,
     &     SEAICE_maskRHS, SEAICEscaleSurfStress,
     &     SEAICE_clipVelocities, SEAICEaddSnowMass,
     &     useHB87stressCoupling, SEAICEupdateOceanStress,
     &     SEAICEuseFluxForm, SEAICEadvHeff, SEAICEadvArea,
     &     SEAICEadvSnow, SEAICEadvSalt, SEAICEmomAdvection,
     &     SEAICEmultiDimAdvection,
     &     SEAICEhighOrderVorticity, SEAICEupwindVorticity,
     &     SEAICEuseAbsVorticity, SEAICEuseJamartMomAdv,
     &     usePW79thermodynamics,
     &     SEAICE_useMultDimSnow, SEAICEuseFlooding, SEAICEheatConsFix,
     &     useMaykutSatVapPoly, SEAICE_mcPheeStepFunc,
     &     SEAICE_doOpenWaterGrowth, SEAICE_doOpenWaterMelt,
     &     SEAICE_salinityTracer, SEAICE_ageTracer,
     &     SEAICErestoreUnderIce, SEAICE_growMeltByConv,
     &     SEAICEwriteState,
     &     SEAICE_tave_mdsio, SEAICE_dump_mdsio, SEAICE_mon_stdio,
     &     SEAICE_tave_mnc,   SEAICE_dump_mnc,   SEAICE_mon_mnc

C--   COMMON /SEAICE_PARM_I/ Integer valued parameters of sea ice model.
C     IMAX_TICE         :: number of iterations for ice surface temp
C                          (default=10)
C     postSolvTempIter :: select flux calculation after surf. temp solver
C                         iteration
C                         0 = none, i.e., from last iter
C                         1 = use linearized approx (consistent with tsurf
C                             finding)
C                         2 = full non-lin form
C     SOLV_NCHECK         :: iteration interval for LSR-solver convergence test
C     SEAICEnonLinIterMax :: number of allowed non-linear solver iterations
C                            for implicit solvers (JFNK and Picard) (>= 2)
C     SEAICElinearIterMax :: number of allowed linear solver iterations for
C                            for implicit solvers (JFNK and Picard) C
C     SEAICEpreconNL_Iter :: number non-linear iterations in preconditioner
C     SEAICEpreconLinIter :: number linear iterations in preconditioner
C     SEAICEnEVPstarSteps :: number of evp*-steps
C     SEAICEmomStartBDF   :: number of previous u/vIce time levels available
C                          to start (or restart) BDF2 scheme.
C     SEAICE_JFNK_lsIter  :: number of Newton iterations after which the
C                            line search is started
C     SEAICE_JFNK_lsLmax  :: max. number line search iterations (default = 4)
C     SEAICE_JFNK_tolIter :: number of Newton iterations after which the
C                            the tolerance is relaxed again (default = 100)
C     SEAICE_OLx/y      :: overlaps for LSR-solver and for the
C                          LSR-preconditioner in JFNK and KRYLOV solver;
C                          for 0 < SEAICE_OLx/y 0 <= OLx/y-2 the LSR solver
C                          and preconditioner use a restricted additive
C                          Schwarz method (default = OLx/y-2).
C     LSR_mixIniGuess   :: control mixing of free-drift sol. into LSR initial
C                          guess
C                       :: =0 : nothing; =1 : no mix, but print free-drift
C                          resid.;
C                       :: =2,4 : mix with (1/local-residual)^2,4 factor
C     SEAICEpresPow0    :: HEFF exponent for ice strength below SEAICEpresH0
C     SEAICEpresPow1    :: HEFF exponent for ice strength above SEAICEpresH0
C     rigding parameters (only active when SEAICE_ITD is defined)
C     SEAICEpartFunc    :: =0 use Thorndyke et al (1975) participation function
C                       :: =1 use Lipscomb et al (2007) participation function
C     SEAICEredistFunc  :: =0 assume ridged ice is uniformly distributed
C                             (Hibler, 1980)
C                          =1 Following Lipscomb et al. (2007), ridged ice is
C                             distributed following an exponentially
C                             decaying function
C     SEAICEridgingIterMax :: maximum number of ridging iterations
C     end ridging parameters
C     SEAICEselectKEscheme   :: momentum advection parameters analogous
C     SEAICEselectVortScheme :: to selectKEscheme and selectVortScheme
C     SEAICEadvScheme   :: sets the advection scheme for thickness and area
C                          (default = 77)
C     SEAICEadvSchArea  :: sets the advection scheme for area
C     SEAICEadvSchHeff  :: sets the advection scheme for effective thickness
C                         (=volume), snow thickness, and salt if available
C     SEAICEadvSchSnow  :: sets the advection scheme for snow on sea-ice
C     SEAICEadvSchSalt  :: sets the advection scheme for sea ice salinity
C     SEAICEadvSchSnow  :: sets the advection scheme for snow on sea-ice
C     SEAICE_areaLossFormula :: selects formula for ice cover loss from melt
C                        :: 1=from all but only melt conributions by ATM and OCN
C                        :: 2=from net melt-growth>0 by ATM and OCN
C                        :: 3=from predicted melt by ATM
C     SEAICE_areaGainFormula :: selects formula for ice cover gain from open
C                               water growth
C                        :: 1=from growth by ATM
C                        :: 2=from predicted growth by ATM
C     SEAICEetaZmethod   :: determines how shear-viscosity eta is computed at
C                           Z-points
C                           0=simple averaging from C-points (default and old)
C                           3=weighted averaging of squares of strain rates
C                             (recommended for energy conservation)
C     SEAICE_multDim     :: number of ice categories
C     SEAICE_debugPointI :: I,J index for seaice-specific debuggin
C     SEAICE_debugPointJ
C
      INTEGER IMAX_TICE, postSolvTempIter
      INTEGER SOLV_NCHECK
      INTEGER SEAICEnonLinIterMax, SEAICElinearIterMax
      INTEGER SEAICEpreconLinIter, SEAICEpreconNL_Iter
      INTEGER LSR_mixIniGuess
      INTEGER SEAICEnEVPstarSteps
      INTEGER SEAICEmomStartBDF
      INTEGER SEAICE_JFNK_lsIter, SEAICE_JFNK_tolIter
      INTEGER SEAICE_JFNK_lsLmax
      INTEGER SEAICE_OLx, SEAICE_OLy
      INTEGER SEAICEselectKEscheme, SEAICEselectVortScheme
      INTEGER SEAICEadvScheme
      INTEGER SEAICEadvSchArea
      INTEGER SEAICEadvSchHeff
      INTEGER SEAICEadvSchSnow
      INTEGER SEAICEadvSchSalt
      INTEGER SEAICEadjMODE
      INTEGER SEAICE_areaLossFormula
      INTEGER SEAICE_areaGainFormula
      INTEGER SEAICEetaZmethod
      INTEGER SEAICE_multDim
      INTEGER SEAICE_debugPointI
      INTEGER SEAICE_debugPointJ
      INTEGER SEAICEpresPow0, SEAICEpresPow1
      INTEGER SEAICEpartFunc, SEAICEredistFunc
      INTEGER SEAICEridgingIterMax
      COMMON /SEAICE_PARM_I/
     &     IMAX_TICE, postSolvTempIter, SOLV_NCHECK,
     &     SEAICEnonLinIterMax, SEAICElinearIterMax,
     &     SEAICEpreconLinIter, SEAICEpreconNL_Iter,
     &     LSR_mixIniGuess,
     &     SEAICEnEVPstarSteps,
     &     SEAICEmomStartBDF,
     &     SEAICE_JFNK_lsIter, SEAICE_OLx, SEAICE_OLy,
     &     SEAICE_JFNK_lsLmax, SEAICE_JFNK_tolIter,
     &     SEAICEpresPow0, SEAICEpresPow1,
     &     SEAICEpartFunc, SEAICEredistFunc, SEAICEridgingIterMax,
     &     SEAICEselectKEscheme, SEAICEselectVortScheme,
     &     SEAICEadvScheme,
     &     SEAICEadvSchArea,
     &     SEAICEadvSchHeff,
     &     SEAICEadvSchSnow,
     &     SEAICEadvSchSalt,
     &     SEAICEadjMODE,
     &     SEAICE_areaLossFormula,
     &     SEAICE_areaGainFormula,
     &     SEAICE_multDim,
     &     SEAICEetaZmethod,
     &     SEAICE_debugPointI,
     &     SEAICE_debugPointJ

C--   COMMON /SEAICE_PARM_C/ Character valued sea ice model parameters.
C     AreaFile          :: File containing initial sea-ice concentration
C     HsnowFile         :: File containing initial snow thickness
C     HsaltFile         :: File containing initial sea ice salt content
C     HeffFile          :: File containing initial sea-ice thickness
C     uIceFile          :: File containing initial sea-ice U comp. velocity
C     vIceFile          :: File containing initial sea-ice V comp. velocity
C        !!! NOTE !!! Initial sea-ice thickness can also be set using
C        SEAICE_initialHEFF below.  But a constant initial condition
C        can mean large artificial fluxes of heat and freshwater in
C        the surface layer during the first model time step.
C
      CHARACTER*(MAX_LEN_FNAM) AreaFile
      CHARACTER*(MAX_LEN_FNAM) HsnowFile
      CHARACTER*(MAX_LEN_FNAM) HsaltFile
      CHARACTER*(MAX_LEN_FNAM) HeffFile
      CHARACTER*(MAX_LEN_FNAM) uIceFile
      CHARACTER*(MAX_LEN_FNAM) vIceFile
      COMMON /SEAICE_PARM_C/
     &   AreaFile, HsnowFile, HsaltFile, HeffFile,
     &   uIceFile, vIceFile

C--   COMMON /SEAICE_PARM_RL/ Real valued parameters of sea ice model.
C     SEAICE_deltaTtherm :: Seaice timestep for thermodynamic equations (s)
C     SEAICE_deltaTdyn   :: Seaice timestep for dynamic solver          (s)
C     SEAICE_LSRrelaxU/V :: relaxation parameter for LSR-solver: U/V-component
C     SEAICE_deltaTevp   :: Seaice timestep for EVP solver              (s)
C     SEAICE_elasticParm :: parameter that sets relaxation timescale
C                           tau = SEAICE_elasticParm * SEAICE_deltaTdyn
C     SEAICE_evpTauRelax :: relaxation timescale tau                    (s)
C     SEAICE_evpDampC    :: evp damping constant (Hunke,JCP,2001)       (kg/m^2)
C     SEAICE_evpAlpha    :: dimensionless parameter 2*evpTauRelax/deltaTevp
C     SEAICE_evpBeta     :: dimensionless parameter deltaTdyn/deltaTevp
C     SEAICEaEVPcoeff    :: main coefficent for adaptive EVP (largest
C                           stabilized frequency)
C     SEAICEaEVPcStar    :: multiple of stabilty factor: alpha*beta=cstar*gamma
C     SEAICEaEVPalphaMin :: lower limit of alpha and beta, regularisation
C                           to prevent singularities of system matrix,
C                           e.g. when ice concentration is too low.
C     SEAICEnonLinTol    :: non-linear tolerance parameter for implicit solvers
C     JFNKgamma_lin_min/max :: tolerance parameters for linear JFNK solver
C     JFNKres_t          :: tolerance parameter for FGMRES residual
C     JFNKres_tFac       :: if set, JFNKres_t=JFNKres_tFac*(initial residual)
C     SEAICE_JFNKepsilon :: step size for the FD-gradient in s/r seaice_jacvec
C     SEAICE_JFNK_lsGamma:: reduction factor for line search (default 0.5)
C     SEAICE_JFNKphi     :: [0,1] parameter for inexact Newton Method (def = 1)
C     SEAICE_JFNKalpha   :: (1,2] parameter for inexact Newton Method (def = 1)
C     SEAICE_zetaMaxFac  :: factor determining the maximum viscosity    (s)
C                          (default = 5.e+12/2.e4 = 2.5e8)
C     SEAICE_zetaMin     :: lower bound for viscosity (default = 0)    (N s/m^2)
C     SEAICEpresH0       :: HEFF threshold for ice strength            (m)
C     SEAICE_monFreq     :: SEAICE monitor frequency.                   (s)
C     SEAICE_dumpFreq    :: SEAICE dump frequency.                      (s)
C     SEAICE_taveFreq    :: SEAICE time-averaging frequency.            (s)
C     SEAICE_initialHEFF :: initial sea-ice thickness                   (m)
C     SEAICE_rhoAir      :: density of air                              (kg/m^3)
C     SEAICE_rhoIce      :: density of sea ice                          (kg/m^3)
C     SEAICE_rhoSnow     :: density of snow                             (kg/m^3)
C     ICE2WATR           :: ratio of sea ice density to water density
C     SEAICE_cpAir       :: specific heat of air                        (J/kg/K)
C
C     OCEAN_drag         :: unitless air-ocean drag coefficient (default 0.001)
C     SEAICE_drag        :: unitless air-ice drag coefficient   (default 0.001)
C     SEAICE_waterDrag   :: unitless water-ice drag coefficient (default 0.0055)
C     SEAICEdWatMin      :: minimum linear water-ice drag applied to DWATN
C                           (default 0.25 m/s)
C
C     SEAICE_dryIceAlb   :: winter albedo
C     SEAICE_wetIceAlb   :: summer albedo
C     SEAICE_drySnowAlb  :: dry snow albedo
C     SEAICE_wetSnowAlb  :: wet snow albedo
C     HO                 :: AKA "lead closing parameter", demarcation thickness
C                           between thin and thick ice. Alternatively, HO (in
C                           meters) can be interpreted as the thickness of ice
C                           formed in open water.
C                           HO is a key ice-growth parameter that determines
C                           the partition between vertical and lateral growth.
C                           The default is 0.5m, increasing this value leads
C                           slower formation of a closed ice cover and thus to
C                           more ice (and thicker) ice, decreasing to faster
C                           formation of a closed ice cover (leads are closing
C                           faster) and thus less (thinner) ice.
C
C     SEAICE_drag_south       :: Southern Ocean SEAICE_drag
C     SEAICE_waterDrag_south  :: Southern Ocean SEAICE_waterDrag
C     SEAICE_dryIceAlb_south  :: Southern Ocean SEAICE_dryIceAlb
C     SEAICE_wetIceAlb_south  :: Southern Ocean SEAICE_wetIceAlb
C     SEAICE_drySnowAlb_south :: Southern Ocean SEAICE_drySnowAlb
C     SEAICE_wetSnowAlb_south :: Southern Ocean SEAICE_wetSnowAlb
C     HO_south                :: Southern Ocean HO
C
C     Parameters for basal drag of grounded ice following
C     Lemieux et al. (2015), doi:10.1002/2014JC010678
C     SEAICE_cBasalStar (default = SEAICE_cStar)
C     SEAICEbasalDragU0 (default = 5e-5)
C     SEAICEbasalDragK1 (default = 8)
C     SEAICEbasalDragK2  :: if > 0, turns on basal drag
C                           (default = 0, Lemieux suggests 15)
C
C     SEAICE_wetAlbTemp  :: Temp (deg.C) above which wet-albedo values are used
C     SEAICE_waterAlbedo :: water albedo
C     SEAICE_strength    :: sea-ice strength Pstar
C     SEAICE_cStar       :: sea-ice strength paramter C* (def: 20)
C     SEAICE_tensilFac   :: sea-ice tensile strength factor, values in [0,1]
C     SEAICE_tensilDepth :: crtical depth for sea-ice tensile strength (def 0.)
C     SEAICEpressReplFac :: interpolator between PRESS0 and regularized PRESS
C                           1. (default): pure pressure replace method (PRESS)
C                           0.          : pure Hibler (1979) method (PRESS0)
C     SEAICE_eccen       :: sea-ice eccentricity of the elliptical yield curve
C     SEAICE_eccfr       :: sea-ice eccentricity of the elliptical flow rule
C     SEAICE_lhFusion    :: latent heat of fusion for ice and snow (J/kg)
C     SEAICE_lhEvap      :: latent heat of evaporation for water (J/kg)
C     SEAICE_dalton      :: Dalton number (= sensible heat transfer coefficient)
C     SEAICE_iceConduct  :: sea-ice conductivity
C     SEAICE_snowConduct :: snow conductivity
C     SEAICE_emissivity  :: longwave ocean-surface emissivity (-)
C     SEAICE_ice_emiss   :: longwave ice-surface emissivity (-)
C     SEAICE_snow_emiss  :: longwave snow-surface emissivity (-)
C     SEAICE_boltzmann   :: Stefan-Boltzman constant (not a run time parameter)
C     SEAICE_snowThick   :: cutoff snow thickness (for snow-albedo)
C     SEAICE_shortwave   :: ice penetration shortwave radiation factor
C     SEAICE_saltFrac    :: salinity of newly formed seaice defined as a
C                           fraction of the ocean surface salinity at the time
C                           of freezing
C     SEAICE_salt0       :: prescribed salinity of seaice (in g/kg).
C     facOpenGrow        :: 0./1. version of logical SEAICE_doOpenWaterGrowth
C     facOpenMelt        :: 0./1. version of logical SEAICE_doOpenWaterMelt
C     SEAICE_mcPheePiston:: ocean-ice turbulent flux "piston velocity" (m/s)
C                           that sets melt efficiency.
C     SEAICE_mcPheeTaper :: tapering down of turbulent flux term with ice
C                           concentration. The 100% cover turb. flux is
C                           multiplied by 1.-SEAICE_mcPheeTaper
C     SEAICE_frazilFrac  :: Fraction of surface level negative heat content
C                           anomalies (relative to the local freezing point)
C                           may contribute as frazil over one time step.
C     SEAICE_tempFrz0    :: sea water freezing point is
C     SEAICE_dTempFrz_dS :: tempFrz = SEAICE_tempFrz0 + salt*SEAICE_dTempFrz_dS
C     SEAICE_PDF         :: prescribed sea-ice distribution within grid box
C     SEAICEstressFactor :: factor by which ice affects wind stress (default=1)
C     LSR_ERROR          :: sets accuracy of LSR solver
C     DIFF1              :: parameter used in advect.F
C     SEAICEtdMU         :: slope parameter for the teardrop and parabolic lens
C                           yield curves
C     SEAICE_deltaMin    :: small number used to reduce singularities of Delta
C     SEAICE_area_max    :: usually set to 1. Seeting areaMax below 1 specifies
C                           the minimun amount of leads (1-areaMax) in the
C                           ice pack.
C     SEAICE_area_floor  :: usually set to 1x10^-5. Specifies a minimun
C                           ice fraction in the ice pack.
C     SEAICE_area_reg    :: usually set to 1x10^-5. Specifies a minimun
C                           ice fraction for the purposes of regularization
C     SEAICE_hice_reg    :: usually set to 5 cm. Specifies a minimun
C                           ice thickness for the purposes of regularization
C     SEAICEdiffKhArea   :: sets the diffusivity for area (m^2/s)
C     SEAICEdiffKhHeff   :: sets the diffusivity for effective thickness (m^2/s)
C     SEAICEdiffKhSnow   :: sets the diffusivity for snow on sea-ice (m^2/s)
C     SEAICEdiffKhSalt   :: sets the diffusivity for sea ice salinity (m^2/s)
C     SEAICE_airTurnAngle   :: turning angles of air-ice interfacial stress
C     SEAICE_waterTurnAngle :: and ice-water interfacial stress (in degrees)
C     SEAICE_tauAreaObsRelax :: Timescale of relaxation to observed
C                               sea ice concentration (s), default=unset
C     ridging parameters (Lipscomb et al, 2007, Bitz et al. 2001):
C     SEAICE_cf       :: ratio of total energy sinks to gravitational sink
C                        (scales ice strength, suggested values: 2 to 17)
C     SEAICEgStar     :: maximum ice concentration that participates in ridging
C     SEAICEhStar     :: empirical thickness (ridging parameter)
C     SEAICEaStar     :: ice concentration parameter similar to gStar for
C                        exponential distribution (Lipscomb et al 2007)
C     SEAICEshearParm :: <=1 reduces amount of energy lost to ridge building
C     SEAICEmuRidging :: tuning parameter similar to hStar for Lipcomb et al
C                        (2007)-scheme
C     SEAICEmaxRaft   :: regularization parameter (default=1)
C     SEAICEsnowFracRidge :: fraction of snow that remains on ridged
C     SINegFac        :: SIADV over/undershoot factor in FW/Adjoint
C     SEAICEmcMu      :: parameter for MC yield curve for useMCE, useMCS and
C                        useTEM options, default is one
C
      _RL SEAICE_deltaTtherm, SEAICE_deltaTdyn, SEAICE_deltaTevp
      _RL SEAICE_LSRrelaxU, SEAICE_LSRrelaxV
      _RL SEAICE_monFreq, SEAICE_dumpFreq, SEAICE_taveFreq
      _RL SEAICE_initialHEFF
      _RL SEAICE_rhoAir, SEAICE_rhoIce, SEAICE_rhoSnow, ICE2WATR
      _RL SEAICE_cpAir
      _RL SEAICE_drag, SEAICE_waterDrag, SEAICEdWatMin
      _RL SEAICE_dryIceAlb, SEAICE_wetIceAlb
      _RL SEAICE_drySnowAlb, SEAICE_wetSnowAlb, HO
      _RL SEAICE_drag_south, SEAICE_waterDrag_south
      _RL SEAICE_dryIceAlb_south, SEAICE_wetIceAlb_south
      _RL SEAICE_drySnowAlb_south, SEAICE_wetSnowAlb_south, HO_south
      _RL SEAICE_cBasalStar, SEAICEbasalDragU0
      _RL SEAICEbasalDragK1, SEAICEbasalDragK2
      _RL SEAICE_wetAlbTemp, SEAICE_waterAlbedo
      _RL SEAICE_strength, SEAICE_cStar, SEAICEpressReplFac
      _RL SEAICE_tensilFac, SEAICE_tensilDepth
      _RL SEAICE_eccen, SEAICE_eccfr
      _RL SEAICEmcMu, SEAICEtdMU
      _RL SEAICE_lhFusion, SEAICE_lhEvap
      _RL SEAICE_dalton
      _RL SEAICE_iceConduct, SEAICE_snowConduct
      _RL SEAICE_emissivity, SEAICE_ice_emiss, SEAICE_snow_emiss
      _RL SEAICE_boltzmann
      _RL SEAICE_snowThick, SEAICE_shortwave
      _RL SEAICE_saltFrac, SEAICE_salt0, SEAICEstressFactor
      _RL SEAICE_mcPheeTaper, SEAICE_mcPheePiston
      _RL SEAICE_frazilFrac, SEAICE_availHeatFrac
      _RL facOpenGrow, facOpenMelt
      _RL SEAICE_tempFrz0, SEAICE_dTempFrz_dS
      _RL SEAICE_PDF(nITD)
      _RL OCEAN_drag, LSR_ERROR, DIFF1
      _RL SEAICEnonLinTol, JFNKres_t, JFNKres_tFac
      _RL JFNKgamma_lin_min, JFNKgamma_lin_max, SEAICE_JFNKepsilon
      _RL SEAICE_JFNK_lsGamma
      _RL SEAICE_JFNKphi, SEAICE_JFNKalpha
      _RL SEAICE_deltaMin
      _RL SEAICE_area_reg, SEAICE_hice_reg
      _RL SEAICE_area_floor, SEAICE_area_max
      _RL SEAICE_airTurnAngle, SEAICE_waterTurnAngle
      _RL SEAICE_elasticParm, SEAICE_evpTauRelax
      _RL SEAICE_evpAlpha, SEAICE_evpBeta
      _RL SEAICE_evpDampC, SEAICE_zetaMin, SEAICE_zetaMaxFac
      _RL SEAICEaEVPcoeff, SEAICEaEVPcStar, SEAICEaEVPalphaMin
      _RL SEAICEpresH0
      _RL SEAICEdiffKhArea, SEAICEdiffKhHeff, SEAICEdiffKhSnow
      _RL SEAICEdiffKhSalt
      _RL SEAICE_tauAreaObsRelax
      _RL SEAICEgStar, SEAICEhStar, SEAICEaStar, SEAICEshearParm
      _RL SEAICEmuRidging, SEAICEmaxRaft, SEAICE_cf
      _RL SEAICEsnowFracRidge
      _RL SINegFac

      COMMON /SEAICE_PARM_RL/
     &    SEAICE_deltaTtherm, SEAICE_deltaTdyn,
     &    SEAICE_LSRrelaxU, SEAICE_LSRrelaxV,
     &    SEAICE_deltaTevp, SEAICE_elasticParm, SEAICE_evpTauRelax,
     &    SEAICE_evpAlpha, SEAICE_evpBeta,
     &    SEAICEaEVPcoeff, SEAICEaEVPcStar, SEAICEaEVPalphaMin,
     &    SEAICE_evpDampC, SEAICE_zetaMin, SEAICE_zetaMaxFac,
     &    SEAICEpresH0,
     &    SEAICE_monFreq, SEAICE_dumpFreq, SEAICE_taveFreq,
     &    SEAICE_initialHEFF,
     &    SEAICE_rhoAir, SEAICE_rhoIce, SEAICE_rhoSnow, ICE2WATR,
     &    SEAICE_drag, SEAICE_waterDrag, SEAICEdWatMin,
     &    SEAICE_dryIceAlb, SEAICE_wetIceAlb,
     &    SEAICE_drySnowAlb, SEAICE_wetSnowAlb, HO,
     &    SEAICE_drag_south, SEAICE_waterDrag_south,
     &    SEAICE_dryIceAlb_south, SEAICE_wetIceAlb_south,
     &    SEAICE_drySnowAlb_south, SEAICE_wetSnowAlb_south, HO_south,
     &    SEAICE_cBasalStar, SEAICEbasalDragU0,
     &    SEAICEbasalDragK1, SEAICEbasalDragK2,
     &    SEAICE_wetAlbTemp, SEAICE_waterAlbedo,
     &    SEAICE_strength, SEAICE_cStar, SEAICE_eccen, SEAICE_eccfr,
     &    SEAICEtdMU, SEAICEmcMu,
     &    SEAICEpressReplFac, SEAICE_tensilFac, SEAICE_tensilDepth,
     &    SEAICE_lhFusion, SEAICE_lhEvap,
     &    SEAICE_dalton, SEAICE_cpAir,
     &    SEAICE_iceConduct, SEAICE_snowConduct,
     &    SEAICE_emissivity, SEAICE_ice_emiss, SEAICE_snow_emiss,
     &    SEAICE_boltzmann,
     &    SEAICE_snowThick, SEAICE_shortwave,
     &    SEAICE_saltFrac, SEAICE_salt0, SEAICEstressFactor,
     &    SEAICE_mcPheeTaper, SEAICE_mcPheePiston,
     &    SEAICE_frazilFrac, SEAICE_availHeatFrac,
     &    facOpenGrow, facOpenMelt,
     &    SEAICE_tempFrz0, SEAICE_dTempFrz_dS, SEAICE_PDF,
     &    OCEAN_drag, LSR_ERROR, DIFF1,
     &    SEAICEnonLinTol, JFNKres_t, JFNKres_tFac,
     &    JFNKgamma_lin_min, JFNKgamma_lin_max, SEAICE_JFNKepsilon,
     &    SEAICE_JFNK_lsGamma, SEAICE_JFNKphi, SEAICE_JFNKalpha,
     &    SEAICE_deltaMin, SEAICE_area_reg, SEAICE_hice_reg,
     &    SEAICE_area_floor, SEAICE_area_max,
     &    SEAICEdiffKhArea, SEAICEdiffKhHeff, SEAICEdiffKhSnow,
     &    SEAICEdiffKhSalt, SEAICE_tauAreaObsRelax,
     &    SEAICE_airTurnAngle, SEAICE_waterTurnAngle,
     &    SEAICEgStar, SEAICEhStar, SEAICEaStar, SEAICEshearParm,
     &    SEAICEmuRidging, SEAICEmaxRaft, SEAICE_cf,
     &    SINegFac,
     &    SEAICEsnowFracRidge

C--   COMMON /SEAICE_BOUND_RL/ Various bounding values
C     MIN_ATEMP         :: minimum air temperature   (deg C)
C     MIN_LWDOWN        :: minimum downward longwave (W/m^2)
C     MIN_TICE          :: minimum ice temperature   (deg C)
C     SEAICE_EPS        :: small number
C     SEAICE_EPS_SQ     :: small number square
C
      _RL MIN_ATEMP, MIN_LWDOWN, MIN_TICE
      _RL SEAICE_EPS, SEAICE_EPS_SQ
      COMMON /SEAICE_BOUND_RL/
     &     MIN_ATEMP, MIN_LWDOWN, MIN_TICE,
     &     SEAICE_EPS, SEAICE_EPS_SQ

#ifdef SEAICE_ITD
C     Hlimit            :: ice thickness category limits (m), array of
C                          size nITD+1
C     Hlimit_c1,_c2,_c3 :: coefficients set in seaice_readparams.F to
C                          calculate Hlimit in seaice_init_fixed.F
      _RL Hlimit(0:nITD)
      _RL Hlimit_c1, Hlimit_c2, Hlimit_c3
      COMMON /SEAICE_BOUND_ITD_RL/
     &     Hlimit,
     &     Hlimit_c1,Hlimit_c2,Hlimit_c3
#endif /* SEAICE_ITD */

C--   Constants used by sea-ice model
      _RL         ZERO           , ONE           , TWO
      PARAMETER ( ZERO = 0.0 _d 0, ONE = 1.0 _d 0, TWO = 2.0 _d 0 )
      _RL         QUART            , HALF
      PARAMETER ( QUART = 0.25 _d 0, HALF = 0.5 _d 0 )
      _RL siEps
      PARAMETER ( siEps = 1. _d -5 )

C--   Constants needed by McPhee formulas for turbulent ocean fluxes :
C        Stanton number (dimensionless), typical friction velocity
C        beneath sea ice (m/s), and tapering factor (dimensionless)
      _RL STANTON_NUMBER, USTAR_BASE, MCPHEE_TAPER_FAC
      PARAMETER ( MCPHEE_TAPER_FAC = 12.5 _d 0 , STANTON_NUMBER =
     &            0.0056 _d 0, USTAR_BASE = 0.0125 _d 0 )

C--   identifiers for advected properties
      INTEGER GAD_HEFF,GAD_AREA,GAD_QICE1,GAD_QICE2,GAD_SNOW
      INTEGER GAD_SALT,GAD_SITR
      PARAMETER ( GAD_HEFF  = 1,
     &            GAD_AREA  = 2,
     &            GAD_SNOW  = 3,
     &            GAD_SALT  = 4,
     &            GAD_QICE1 = 5,
     &            GAD_QICE2 = 6,
     &            GAD_SITR  = 7)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
