C $Header: /u/gcmpack/MITgcm/pkg/kpp/KPP_PARAMS.h,v 1.5 2000/11/13 16:37:02 heimbach Exp $

C     /==========================================================\
C     | KPP_PARAMS.h                                             |
C     | o Basic parameter header for KPP vertical mixing         |
C     |   parameterization.  These parameters are initialized by |
C     |   and/or read in from data.kpp file.                     |
C     \==========================================================/

C Parameters used in kpp routine arguments (needed for compilation
C of kpp routines even if ALLOW_KPP is not defined)
C     mdiff                  - number of diffusivities for local arrays
C     Nrm1, Nrp1, Nrp2       - number of vertical levels
C     imt                    - array dimension for local arrays
C     ibot, itop, jbot, jtop - array dimension indices

      integer    mdiff, Nrm1, Nrp1, Nrp2
      integer    imt, ibot, itop, jbot, jtop
      parameter( mdiff = 3    )
      parameter( Nrm1  = Nr-1 )
      parameter( Nrp1  = Nr+1 )
      parameter( Nrp2  = Nr+2 )
#ifdef FRUGAL_KPP
      parameter( imt=(sNx+2)*(sNy+2) )
      parameter( ibot=0, itop=sNx+1, jbot=0, jtop=sNy+1 )
#else
      parameter( imt=(sNx+2*OLx)*(sNy+2*OLy) )
      parameter( ibot=1-OLx, itop=sNx+OLx, jbot=1-OLy, jtop=sNy+OLy )
#endif

#ifdef ALLOW_KPP

C Time invariant parameters initialized by subroutine kmixinit
C     nzmax (nx,ny)   - Maximum number of wet levels in each column
C     pMask           - Mask relating to Pressure/Tracer point grid.
C                       0. if P point is on land.
C                       1. if P point is in water.
C     zgrid (0:Nr+1)  - vertical levels of tracers (<=0)                (m)
C     hwide (0:Nr+1)  - layer thicknesses          (>=0)                (m)
C     kpp_freq        - Re-computation frequency for KPP parameters     (s)
C     kpp_dumpFreq    - KPP dump frequency.                             (s)
C     kpp_taveFreq    - KPP time-averaging frequency.                   (s)


      INTEGER nzmax ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy,     nSx, nSy )
      _KPP_RL pMask ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr, nSx, nSy )
      _KPP_RL zgrid ( 0:Nr+1 )
      _KPP_RL hwide ( 0:Nr+1 )
      _RL kpp_freq
      _RL kpp_dumpFreq
      _RL kpp_taveFreq

      COMMON /kpp_i/  nzmax

      COMMON /kpp_r1/ pMask, zgrid, hwide

      COMMON /kpp_r2/ kpp_freq, kpp_dumpFreq, kpp_taveFreq


C-----------------------------------------------------------------------
C
C     KPP flags and min/max permitted values for mixing parameters
c
C     KPPmixingMaps     - if true, include KPP diagnostic maps in STDOUT
C     KPPwriteState     - if true, write KPP state to file
C     minKPPhbl                    - KPPhbl minimum value               (m)
C
C-----------------------------------------------------------------------

      LOGICAL KPPmixingMaps, KPPwriteState

      COMMON /KPP_PARM_L/
     &        KPPmixingMaps, KPPwriteState

      _KPP_RL minKPPhbl

      COMMON /KPP_PARM_R/
     &        minKPPhbl

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
c     phepsi  = 1.0e-10 
c     epsilon = nondimensional extent of the surface layer = 0.1
c     vonk    = von Karmans constant                       = 0.4
c     dB_dz   = maximum dB/dz in mixed layer hMix          = 5.2e-5 s^-2
c     conc1,conam,concm,conc2,zetam,conas,concs,conc3,zetas
c             = scalar coefficients
c-----------------------------------------------------------------------

      _KPP_RL          epsln,phepsi,epsilon,vonk,dB_dz,
     $                 conc1,
     $                 conam,concm,conc2,zetam,
     $                 conas,concs,conc3,zetas

      common /kmixcom/ epsln,phepsi,epsilon,vonk,dB_dz,
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

      _KPP_RL               Ricr,cekman,cmonob,concv,Vtc
      _RL                   hbf

      common /kpp_bldepth1/ Ricr,cekman,cmonob,concv,Vtc
      common /kpp_bldepth2/ hbf

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

      integer    nni      , nnj
      parameter (nni = 890, nnj = 480)

      _KPP_RL          wmt(0:nni+1,0:nnj+1), wst(0:nni+1,0:nnj+1)
      _KPP_RL          deltaz,deltau,zmin,zmax,umin,umax
      common /kmixcws/ wmt, wst
     $               , deltaz,deltau,zmin,zmax,umin,umax

c-----------------------------------------------------------------------
c     parameters for subroutine "ri_iwmix"
c
c
c     to compute vertical mixing coefficients below boundary layer:
c
c     num_v_smooth_Ri = number of times Ri is vertically smoothed
c     num_v_smooth_BV, num_z_smooth_sh, and num_m_smooth_sh are dummy
c               variables kept for backward compatibility of the data file
c     Riinfty = local Richardson Number limit for shear instability = 0.7
c     BVSQcon = Brunt-Vaisala squared                               (1/s^2)
c     difm0   = viscosity max due to shear instability              (m^2/s)
c     difs0   = tracer diffusivity ..                               (m^2/s)
c     dift0   = heat diffusivity ..                                 (m^2/s)
c     difmcon = viscosity due to convective instability             (m^2/s)
c     difscon = tracer diffusivity ..                               (m^2/s)
c     diftcon = heat diffusivity ..                                 (m^2/s)
c-----------------------------------------------------------------------

      INTEGER num_v_smooth_Ri, num_v_smooth_BV
      INTEGER num_z_smooth_sh, num_m_smooth_sh
      _KPP_RL Riinfty, BVSQcon
      _KPP_RL difm0  , difs0  , dift0
      _KPP_RL difmcon, difscon, diftcon

      COMMON /kmixcri_i/ num_v_smooth_Ri, num_v_smooth_BV
     1                 , num_z_smooth_sh, num_m_smooth_sh

      COMMON /kmixcri_r/ Riinfty, BVSQcon
     1                 , difm0, difs0, dift0
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

      _KPP_RL          Rrho0, dsfmax 
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

      _KPP_RL          cstar, cg

      common /kmixcbm/ cstar, cg

#endif /* ALLOW_KPP */
