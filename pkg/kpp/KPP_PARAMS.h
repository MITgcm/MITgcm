C     *==========================================================*
C     | KPP_PARAMS.h
C     | o Basic parameter header for KPP vertical mixing
C     |   parameterization.  These parameters are initialized by
C     |   and/or read in from data.kpp file.
C     *==========================================================*

C Parameters used in kpp routine arguments (needed for compilation
C  of kpp routines even if ALLOW_KPP is not defined)
C     mdiff                 :: number of diffusivities for local arrays
C     Nrm1, Nrp1, Nrp2      :: number of vertical levels
C     imt                   :: array dimension for local arrays

      integer    mdiff, Nrm1, Nrp1, Nrp2
      integer    imt
      parameter( mdiff = 3    )
      parameter( Nrm1  = Nr-1 )
      parameter( Nrp1  = Nr+1 )
      parameter( Nrp2  = Nr+2 )
      parameter( imt=(sNx+2*OLx)*(sNy+2*OLy) )

#ifdef ALLOW_KPP

C Time invariant parameters initialized by subroutine kmixinit
C     nzmax (nx,ny)  :: Maximum number of wet levels in each column
C     pMask          :: Mask relating to Pressure/Tracer point grid.
C                       0. if P point is on land.
C                       1. if P point is in water.
C                 Note: use now maskC since pMask is identical to maskC
C     zgrid (0:Nr+1) :: vertical levels of tracers (<=0)                (m)
C     hwide (0:Nr+1) :: layer thicknesses          (>=0)                (m)
C     kpp_freq       :: Re-computation frequency for KPP parameters     (s)
C     kpp_dumpFreq   :: KPP dump frequency.                             (s)
C     kpp_taveFreq   :: KPP time-averaging frequency.                   (s)

      INTEGER nzmax ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy,     nSx, nSy )
c     _RL pMask     ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr, nSx, nSy )
      _RL zgrid     ( 0:Nr+1 )
      _RL hwide     ( 0:Nr+1 )
      _RL kpp_freq
      _RL kpp_dumpFreq
      _RL kpp_taveFreq

      COMMON /kpp_i/  nzmax

      COMMON /kpp_r1/ zgrid, hwide

      COMMON /kpp_r2/ kpp_freq, kpp_dumpFreq, kpp_taveFreq

C-----------------------------------------------------------------------
C
C     KPP flags and min/max permitted values for mixing parameters
C
C     KPPwriteState    :: if true, write KPP state to file
C     minKPPhbl        :: KPPhbl minimum value               (m)
C     KPP_ghatUseTotalDiffus ::
C                       if T : Compute the non-local term using
C                            the total vertical diffusivity ;
C                       if F (=default): use KPP vertical diffusivity
C     Note: prior to checkpoint55h_post, was using the total Kz
C     KPPuseDoubleDiff :: if TRUE, include double diffusive
C                         contributions
C     LimitHblStable   :: if TRUE (the default), limits the depth of the
C                         hbl under stable conditions.
C
C-----------------------------------------------------------------------

      LOGICAL KPPwriteState, KPP_ghatUseTotalDiffus
      LOGICAL KPPuseDoubleDiff
      LOGICAL LimitHblStable
      LOGICAL KPPuseSWfrac3D
      COMMON /KPP_PARM_L/
     &        KPPwriteState, KPP_ghatUseTotalDiffus,
     &        KPPuseDoubleDiff, LimitHblStable, KPPuseSWfrac3D

      _RL                 minKPPhbl
      COMMON /KPP_PARM_R/ minKPPhbl

C======================  file "kmixcom.h" =======================
C-----------------------------------------------------------------------
C     Define various parameters and common blocks for KPP vertical-
C     mixing scheme; used in "kppmix.F" subroutines.
C     Constants are set in subroutine KPP_READPARMS.
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C     parameters for several subroutines
C
C     epsln   = 1.0e-20
C     phepsi  = 1.0e-10
C     epsilon = nondimensional extent of the surface layer = 0.1
C     vonk    = von Karmans constant                       = 0.4
C     dB_dz   = maximum dB/dz in mixed layer hMix          = 5.2e-5 s^-2
C     conc1,conam,concm,conc2,zetam,conas,concs,conc3,zetas
C             = scalar coefficients
C-----------------------------------------------------------------------

      _RL              epsln,phepsi,epsilon,vonk,dB_dz,
     &                 conc1,
     &                 conam,concm,conc2,zetam,
     &                 conas,concs,conc3,zetas

      common /kmixcom/ epsln,phepsi,epsilon,vonk,dB_dz,
     &                 conc1,
     &                 conam,concm,conc2,zetam,
     &                 conas,concs,conc3,zetas

C-----------------------------------------------------------------------
C     parameters for subroutine "bldepth"
C
C     to compute depth of boundary layer:
C
C     Ricr    = critical bulk Richardson Number            = 0.3
C     cekman  = coefficient for ekman depth                = 0.7
C     cmonob  = coefficient for Monin-Obukhov depth        = 1.0
C     concv   = ratio of interior buoyancy frequency to
C               buoyancy frequency at entrainment depth    = 1.8
C     hbf     = fraction of bounadry layer depth to
C               which absorbed solar radiation
C               contributes to surface buoyancy forcing    = 1.0
C     Vtc     = non-dimensional coefficient for velocity
C               scale of turbulant velocity shear
C               (=function of concv,concs,epsilon,vonk,Ricr)
C-----------------------------------------------------------------------

      _RL                   Ricr,cekman,cmonob,concv,Vtc
      _RL                   hbf

      common /kpp_bldepth1/ Ricr,cekman,cmonob,concv,Vtc
      common /kpp_bldepth2/ hbf

C-----------------------------------------------------------------------
C     parameters and common arrays for subroutines "kmixinit"
C     and "wscale"
C
C     to compute turbulent velocity scales:
C
C     nni     = number of values for zehat in the look up table
C     nnj     = number of values for ustar in the look up table
C
C     wmt     = lookup table for wm, the turbulent velocity scale
C               for momentum
C     wst     = lookup table for ws, the turbulent velocity scale
C               for scalars
C     deltaz  = delta zehat in table
C     deltau  = delta ustar in table
C     zmin    = minimum limit for zehat in table (m3/s3)
C     zmax    = maximum limit for zehat in table
C     umin    = minimum limit for ustar in table (m/s)
C     umax    = maximum limit for ustar in table
C-----------------------------------------------------------------------

      integer    nni      , nnj
      parameter (nni = 890, nnj = 480)

      _RL              wmt(0:nni+1,0:nnj+1), wst(0:nni+1,0:nnj+1)
      _RL              deltaz,deltau,zmin,zmax,umin,umax
      common /kmixcws/ wmt, wst,
     &                 deltaz,deltau,zmin,zmax,umin,umax

C-----------------------------------------------------------------------
C     parameters for subroutine "ri_iwmix"
C
C     to compute vertical mixing coefficients below boundary layer:
C
C     num_v_smooth_Ri = number of times Ri is vertically smoothed
C     Riinfty = local Richardson Number limit for shear instability = 0.7
C     BVSQcon = Brunt-Vaisala squared                               (1/s^2)
C     difm0   = viscosity max due to shear instability              (m^2/s)
C     difs0   = tracer diffusivity ..                               (m^2/s)
C     dift0   = heat diffusivity ..                                 (m^2/s)
C     difmcon = viscosity due to convective instability             (m^2/s)
C     difscon = tracer diffusivity ..                               (m^2/s)
C     diftcon = heat diffusivity ..                                 (m^2/s)
C-----------------------------------------------------------------------

      INTEGER            num_v_smooth_Ri
      COMMON /kmixcri_i/ num_v_smooth_Ri

      _RL                Riinfty, BVSQcon
      _RL                difm0  , difs0  , dift0
      _RL                difmcon, difscon, diftcon
      COMMON /kmixcri_r/ Riinfty, BVSQcon,
     &                   difm0, difs0, dift0,
     &                   difmcon, difscon, diftcon

C-----------------------------------------------------------------------
C     parameters for subroutine "ddmix"
C
C     to compute additional diffusivity due to double diffusion:
C
C     Rrho0   = limit for double diffusive density ratio
C     dsfmax  = maximum diffusivity in case of salt fingering (m2/s)
C-----------------------------------------------------------------------

      _RL              Rrho0, dsfmax
      common /kmixcdd/ Rrho0, dsfmax

C-----------------------------------------------------------------------
C     parameters for subroutine "blmix"
C
C     to compute mixing within boundary layer:
C
C     cstar   = proportionality coefficient for nonlocal transport
C     cg      = non-dimensional coefficient for counter-gradient term
C-----------------------------------------------------------------------

      _RL              cstar, cg
      common /kmixcbm/ cstar, cg

#endif /* ALLOW_KPP */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
