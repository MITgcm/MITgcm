C     /---------------------------------------------------------------\
C     | Variables for KPP verical mixing parameterization             |
C     | (initialized by kmixinit and ini_parms                        |
C     \---------------------------------------------------------------/

c-----------------------------------------------------------------------
c
C Time varying parameters computed by subroutine kppmix
C     KPPviscAz  - vertical eddy viscosity coefficient                 (m^2/s)
C     KPPdiffKzS - vertical diffusion coefficient for salt and tracers (m^2/s)
C     KPPdiffKzT - vertical diffusion coefficient for heat             (m^2/s)
C     KPPghat    - nonlocal transport coefficient                      (s/m^2)
C     KPPhbl     - mixing layer depth                                  (m)

C Time invariant parameters initialized by subroutine kmixinit
C     pMask           - Mask relating to Pressure/Tracer point grid.
C                       0. if P point is on land.
C                       1. if P point is in water.
C     nzmax (nx,ny)   - maximum number of wet levels in each column
C     zgrid (0:Nr+1)  - vertical levels of tracers (<=0)               (m)
C     hwide (0:Nr+1)  - layer thicknesses          (>=0)               (m)

C For direct access dumping.  Initialized in ini_forcing.F
C Used in WRITE_STATE_DRCT.F
C     KPP?-Handle - File handle of direct access dump
c
c-----------------------------------------------------------------------

      INTEGER nzmax  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)

      _RL KPPviscAz  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPdiffKzS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPdiffKzT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPghat    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KPPhbl     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)

      _RS pMask      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS zgrid      (0:Nr+1)
      _RS hwide      (0:Nr+1)

      COMMON /kpp_rl/ KPPviscAz, KPPdiffKzT, KPPdiffKzS
     1              , KPPghat  , KPPhbl

      COMMON /kpp_rs/ pMask, zgrid, hwide

      INTEGER        KPPvHandle, KPPdHandle, KPPgHandle, KPPhHandle

      COMMON /kpp_i/ nzmax
     1             , KPPvHandle, KPPdHandle, KPPgHandle, KPPhHandle

c-----------------------------------------------------------------------
c
C     KPP flags and min/max permitted values for mixing parameters
c
C     usingKPPmixing    - if true, use KPP vertical mixing
C     KPPmixingMaps     - if true, include KPP diagnostic maps in STDOUT
C     KPPwriteState     - if true, write KPP state to file
C     minKPPghat,    maxKPPghat    - KPP non local transport bounds ( s/m^2 )
C     minKPPviscAz,  maxKPPviscAz  - KPP viscosity bounds           ( m^2/s )
C     minKPPdiffKzT, maxKPPdiffKzT - KPP heat diffusivity bounds    ( m^2/s )
C     minKPPdiffKzS, maxKPPdiffKzS - KPP tracer diffusivity bounds  ( m^2/s )
C
c-----------------------------------------------------------------------

      LOGICAL usingKPPmixing, KPPmixingMaps, KPPwriteState

      COMMON /KPP_PARM_L/
     &        usingKPPmixing, KPPmixingMaps, KPPwriteState

      _RS     minKPPghat   , maxKPPghat   
      _RS     minKPPviscAz , maxKPPviscAz(Nr)
      _RS     minKPPdiffKzT, maxKPPdiffKzT
      _RS    minKPPdiffKzS, maxKPPdiffKzS

      COMMON /KPP_PARM_R/
     &        minKPPghat   , maxKPPghat   
     &      , minKPPviscAz , maxKPPviscAz
     &      , minKPPdiffKzT, maxKPPdiffKzT
     &      , minKPPdiffKzS, maxKPPdiffKzS

c======================  file "kmixcom.h" =======================
c
c-----------------------------------------------------------------------
c     Define various parameters and common blocks for KPP vertical-
c     mixing scheme; used in "kppmix.F" subroutines.
c     Constants are set in subroutine "ini_parms".
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c     parameters for several subroutines
c
c     epsln   = 1.0e-20 
c     epsilon = nondimensional extent of the surface layer = 0.1
c     vonk    = von Karmans constant                       = 0.4
c     conc1,conam,concm,conc2,zetam,conas,concs,conc3,zetas
c             = scalar coefficients
c-----------------------------------------------------------------------

      _RS              epsln,epsilon,vonk,
     $                 conc1,
     $                 conam,concm,conc2,zetam,
     $                 conas,concs,conc3,zetas

      common /kmixcom/ epsln,epsilon,vonk,
     $                 conc1,
     $                 conam,concm,conc2,zetam,
     $                 conas,concs,conc3,zetas

c-----------------------------------------------------------------------
c     parameters for subroutine "bldepth"
c
c
c     to compute depth of boundary layer:
c
c     Ricr    = critical bulk Richardson Number            = 0.3
c     cekman  = coefficient for ekman depth                = 0.7 
c     cmonob  = coefficient for Monin-Obukhov depth        = 1.0
c     concv   = ratio of interior buoyancy frequency to 
c               buoyancy frequency at entrainment depth    = 1.8
c     hbf     = fraction of bounadry layer depth to 
c               which absorbed solar radiation 
c               contributes to surface buoyancy forcing    = 1.0
c     Vtc     = non-dimensional coefficient for velocity
c               scale of turbulant velocity shear
c               (=function of concv,concs,epsilon,vonk,Ricr)
c-----------------------------------------------------------------------

      _RS              Ricr,cekman,cmonob,concv,hbf,Vtc

      common /kmixcbd/ Ricr,cekman,cmonob,concv,hbf,Vtc

c-----------------------------------------------------------------------
c     parameters and common arrays for subroutines "kmixinit" 
c     and "wscale"
c
c
c     to compute turbulent velocity scales:
c
c     nni     = number of values for zehat in the look up table
c     nnj     = number of values for ustar in the look up table
c
c     wmt     = lookup table for wm, the turbulent velocity scale 
c               for momentum
c     wst     = lookup table for ws, the turbulent velocity scale 
c               for scalars
c     deltaz  = delta zehat in table
c     deltau  = delta ustar in table
c     zmin    = minimum limit for zehat in table (m3/s3)
c     zmax    = maximum limit for zehat in table
c     umin    = minimum limit for ustar in table (m/s)
c     umax    = maximum limit for ustar in table
c-----------------------------------------------------------------------

      integer nni, nnj
      parameter (nni = 890, nnj = 480)

      _RS              wmt(0:nni+1,0:nnj+1), wst(0:nni+1,0:nnj+1)
      _RS              deltaz,deltau,zmin,zmax,umin,umax
      common /kmixcws/ wmt, wst
     $               , deltaz,deltau,zmin,zmax,umin,umax

c-----------------------------------------------------------------------
c     parameters for subroutine "ri_iwmix"
c
c
c     to compute vertical mixing coefficients below boundary layer:
c
c     num_v_smooth_Ri = number of times Ri is vertically smoothed
c     num_v_smooth_BV = number of times BV is vertically smoothed
c     num_z_smooth_sh = number of times shear is zonally smoothed
c     num_m_smooth_sh = number of times shear is meridionally smoothed
c     Riinfty = local Richardson Number limit for shear instability = 0.7
c     BVSQcon = Brunt-Vaisala squared                            (1/s^2)
c     difm0   = viscosity max due to shear instability           (m^2/s)
c     difs0   = tracer diffusivity ..                            (m^2/s)
c     dift0   = heat diffusivity ..                              (m^2/s)
c     difmiw  = viscosity background due to internal waves       (m^2/s)
c     difsiw  = tracer diffusivity ..                            (m^2/s)
c     diftiw  = heat diffusivity ..                              (m^2/s)
c     difmcon = viscosity due to convective instability          (m^2/s)
c     difscon = tracer diffusivity ..                            (m^2/s)
c     diftcon = heat diffusivity ..                              (m^2/s)
c-----------------------------------------------------------------------

      INTEGER num_v_smooth_Ri, num_v_smooth_BV
      INTEGER num_z_smooth_sh, num_m_smooth_sh
      _RS     Riinfty, BVSQcon
      _RS     difm0  , difs0  , dift0
      _RS     difmiw , difsiw , diftiw
      _RS     difmcon, difscon, diftcon

      COMMON /kmixcri_i/ num_v_smooth_Ri, num_v_smooth_BV
     1                 , num_z_smooth_sh, num_m_smooth_sh

      COMMON /kmixcri_r/ Riinfty, BVSQcon
     1                 , difm0, difs0, dift0, difmiw, difsiw, diftiw
     2                 , difmcon, difscon, diftcon

c-----------------------------------------------------------------------
c     parameters for subroutine "ddmix"
c
c
c     to compute additional diffusivity due to double diffusion:
c
c     Rrho0   = limit for double diffusive density ratio
c     dsfmax  = maximum diffusivity in case of salt fingering (m2/s)
c-----------------------------------------------------------------------

      _RS              Rrho0, dsfmax 
      common /kmixcdd/ Rrho0, dsfmax

c-----------------------------------------------------------------------
c     parameters for subroutine "blmix"
c
c
c     to compute mixing within boundary layer:
c
c     cstar   = proportionality coefficient for nonlocal transport
c     cg      = non-dimensional coefficient for counter-gradient term
c-----------------------------------------------------------------------

      _RS              cstar, cg

      common /kmixcbm/ cstar, cg
