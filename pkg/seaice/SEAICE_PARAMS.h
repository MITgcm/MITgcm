C $Header: 

#ifdef ALLOW_SEAICE

C     /==========================================================\
C     | SEAICE_PARAMS.h                                          |
C     | o Basic parameter header for sea ice model.              |
C     \==========================================================/

C--   COMMON /SEAICE_PARM_L/ Logical parameters of sea ice model.
C
C     SEAICEwriteState  - If true, write sea ice state to file;
C                         default is false.
C
C     SEAICEuseDYNAMICS - If false, do not use dynamics;
C                         default is to use dynamics.
C
C     SEAICEuseLSR      - If true, use LSR rather than ADI solver;
C                         default is ADI solver.
C
C     SEAICEwindOnCgrid - By default wind files, uwind and vwind,
C        are defined on North-East B-grid U and V points.
C        When this flag is set, wind files are defined on
C        South-West C-grid U and V points.
C
      LOGICAL SEAICEwriteState, SEAICEuseDYNAMICS, SEAICEuseLSR,
     &        SEAICEwindOnCgrid
      COMMON /SEAICE_PARM_L/
     &        SEAICEwriteState, SEAICEuseDYNAMICS, SEAICEuseLSR,
     &        SEAICEwindOnCgrid

C--   COMMON /SEAICE_PARM_I/ Integer valued parameters of sea ice model.
C     LAD        - time stepping used for sea-ice advection:
C                  1 = LEAPFROG,  2 = BACKWARD EULER.
C     IMAX_TICE  - number of iterations for ice heat budget   10
C     NPSEUDO    - number of pseudo-timesteps used in dynsolver
C DO PSEUDO-TIMESTEPS TO OBTAIN AN ACCURATE VISCOUS-PLASTIC SOLUTION
C 10 PSEUDO-TIMESTEPS OR MORE ARE SUGGESTED FOR HIGH-RESOLUTION (~10KM)
C 1 PSEUDO-TIMESTEP CAN BE USED FOR LOW-RESOLUTION GLOBAL MODELING
C
      INTEGER LAD, IMAX_TICE, NPSEUDO
      COMMON /SEAICE_PARM_I/ LAD, IMAX_TICE, NPSEUDO

C--   COMMON /SEAICE_PARM_C/ Character valued sea ice model parameters.
C     uwindFile       - File containing uwind
C     vwindFile       - File containing vwind
C     atempFile       - File containing atemp
C     aqhFile         - File containing aqh
C     lwdownFile      - File containing lwdown
C     swdownFile      - File containing swdown
C     precipFile      - File containing precip
C     evapFile        - File containing evap
C     runoffFile      - File containing runoffF
C     HeffFile        - File containing initial sea-ice thickness
C        !!! NOTE !!! Initial sea-ice thickness can also be set using
C        SEAICE_initialHEFF below.  But a constant initial condition
C        can mean large artificial fluxes of heat and freshwater in
C        the surface layer during the first model time step.
C
      CHARACTER*(MAX_LEN_FNAM) uwindFile
      CHARACTER*(MAX_LEN_FNAM) vwindFile
      CHARACTER*(MAX_LEN_FNAM) atempFile
      CHARACTER*(MAX_LEN_FNAM) aqhFile
      CHARACTER*(MAX_LEN_FNAM) lwdownFile
      CHARACTER*(MAX_LEN_FNAM) swdownFile
      CHARACTER*(MAX_LEN_FNAM) precipFile
      CHARACTER*(MAX_LEN_FNAM) evapFile
      CHARACTER*(MAX_LEN_FNAM) runoffFile
      CHARACTER*(MAX_LEN_FNAM) HeffFile
      COMMON /SEAICE_PARM_C/ uwindFile, vwindFile, atempFile, aqhFile,
     &     lwdownFile, swdownFile, precipFile, evapFile, runoffFile,
     &	   HeffFile

C--   COMMON /SEAICE_PARM_RL/ Real valued parameters of sea ice model.
C     SEAICE_dumpFreq    - SEAICE dump frequency.                     (s)
C     SEAICE_taveFreq    - SEAICE time-averaging frequency.           (s)
C     SEAICE_initialHEFF - initial sea-ice thickness                  (m)
C     SEAICE_drag        - air-ice drag coefficient
C     SEAICE_waterDrag   - water-ice drag coefficient * water density
C     SEAICE_albedo      - summer albedo
C     SEAICE_drySnowAlb  - dry snow albedo
C     SEAICE_wetSnowAlb  - wet snow albedo
C     SEAICE_waterAlbedo - water albedo
C     SEAICE_strength    - sea-ice strength
C     SEAICE_sensHeat    - sensible heat transfer coefficient
C     SEAICE_latentWater - latent heat transfer coefficient for water
C     SEAICE_latentIce   - latent heat transfer coefficient for ice
C     SEAICE_iceConduct  - sea-ice conductivity
C     SEAICE_snowConduct - snow conductivity
C     SEAICE_emissivity  - Stefan-Boltzman constant * emissivity
C     SEAICE_snowThick   - cutoff snow thickness
C     SEAICE_shortwave   - penetration shortwave radiation factor
C     LSR_ERROR          - sets accuracy of LSR solver
C     DIFF1              - parameter used in advect.F
C     A22                - parameter used in growth.F
C     HO                 - parameter used in growth.F
C     WindForcingStart   - Time of first  wind forcing record  (s)
C     WindForcingEnd     - Time of last   wind forcing record  (s)
C     WindForcingPeriod  - Period between wind forcing records (s)
C     FluxForcingStart   - Time of first  flux forcing record  (s)
C     FluxForcingEnd     - Time of last   flux forcing record  (s)
C     FluxForcingPeriod  - Period between flux forcing records (s)
C     SSTForcingStart    - Time of first  SST  forcing record  (s)
C     SSTForcingEnd      - Time of last   SST  forcing record  (s)
C     SSTForcingPeriod   - Period between SST  forcing records (s)
C     SSSForcingStart    - Time of first  SSS  forcing record  (s)
C     SSSForcingEnd      - Time of last   SSS  forcing record  (s)
C     SSSForcingPeriod   - Period between SSS  forcing records (s)
C     StartingYear       - Starting year of integration
C     EndingYear         - Ending year of integration
C
      _RL SEAICE_dumpFreq, SEAICE_taveFreq, SEAICE_initialHEFF
      _RL SEAICE_drag, SEAICE_waterDrag, SEAICE_albedo
      _RL SEAICE_drySnowAlb, SEAICE_wetSnowAlb
      _RL SEAICE_waterAlbedo, SEAICE_strength
      _RL SEAICE_sensHeat, SEAICE_latentWater, SEAICE_latentIce
      _RL SEAICE_iceConduct, SEAICE_snowConduct, SEAICE_emissivity
      _RL SEAICE_snowThick, SEAICE_shortwave
      _RL LSR_ERROR,DIFF1,A22,HO
      _RL WindForcingStart, WindForcingEnd, WindForcingPeriod
      _RL FluxForcingStart, FluxForcingEnd, FluxForcingPeriod
      _RL SSTForcingStart,  SSTForcingEnd,  SSTForcingPeriod
      _RL SSSForcingStart,  SSSForcingEnd,  SSSForcingPeriod
      _RL StartingYear,     EndingYear
      COMMON /SEAICE_PARM_RL/
     &    SEAICE_dumpFreq, SEAICE_taveFreq, SEAICE_initialHEFF,
     &    SEAICE_drag, SEAICE_waterDrag, SEAICE_albedo,
     &    SEAICE_drySnowAlb, SEAICE_wetSnowAlb,
     &    SEAICE_waterAlbedo, SEAICE_strength,
     &    SEAICE_sensHeat, SEAICE_latentWater, SEAICE_latentIce,
     &    SEAICE_iceConduct, SEAICE_snowConduct, SEAICE_emissivity,
     &    SEAICE_snowThick, SEAICE_shortwave,
     &    LSR_ERROR, DIFF1, A22, HO,
     &    WindForcingStart, WindForcingEnd, WindForcingPeriod,
     &    FluxForcingStart, FluxForcingEnd, FluxForcingPeriod,
     &    SSTForcingStart,  SSTForcingEnd,  SSTForcingPeriod,
     &    SSSForcingStart,  SSSForcingEnd,  SSSForcingPeriod,
     &    StartingYear,     EndingYear

C--   COMMON /SEAICE_BOUND_RL/ Various bounding values
C     MAX_HEFF   - maximum ice thickness     (m)
C     MIN_ATEMP  - minimum air temperature   (deg C)
C     MIN_LWDOWN - minimum downward longwave (W/m^2)
C     MAX_TICE   - maximum ice temperature   (deg C)
C     MIN_TICE   - minimum ice temperature   (deg C)
C     SEAICE_EPS, SEAICE_EPS_SQ - used to reduce derivative singularities
C
      _RL MAX_HEFF, MIN_ATEMP, MIN_LWDOWN, MAX_TICE, MIN_TICE
      _RL SEAICE_EPS, SEAICE_EPS_SQ
      COMMON /SEAICE_BOUND_RL/
     &    MAX_HEFF, MIN_ATEMP, MIN_LWDOWN, MAX_TICE, MIN_TICE,
     &    SEAICE_EPS, SEAICE_EPS_SQ

C--   Seaice time step
      COMMON /SEAICE_R/ SEAICE_DT
      _RL               SEAICE_DT

C--   Constants used by sea-ice model
      _RL         ZERO           , ONE           , TWO
      parameter ( ZERO = 0.0 _d 0, ONE = 1.0 _d 0, TWO = 2.0 _d 0 )
      _RL         QUART            , HALF
      parameter ( QUART = 0.25 _d 0, HALF = 0.5 _d 0 ) 

#endif /* ALLOW_SEAICE */
