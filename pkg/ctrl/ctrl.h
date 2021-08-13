c     ==================================================================
c     HEADER CONTROLVARS
c     ==================================================================
c
c     o Control variables of the ECCO state estimation tool.
c
c     Depending on the specific problem to be studied users will have to
c     modify this header file.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     ==================================================================
c     HEADER CONTROLVARS ctrl.h
c     ==================================================================

      integer     maxcvars
#if (defined (CTRL_SET_OLD_MAXCVARS_30))
      parameter ( maxcvars = 30 )
#elif (defined (CTRL_SET_OLD_MAXCVARS_40))
      parameter ( maxcvars = 40 )
#elif (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      parameter ( maxcvars = 400 )
#else
      parameter ( maxcvars = 60 )
#endif

cph ctrlprec will be set to 32 for ECCO to reduce I/O
cph but jeopardizes some gradient checks, so should be
cph set to 64 by default.
      integer     ctrlprec
      common /controlparams_i/ ctrlprec

#ifdef ALLOW_ADMTLM
      integer admtlmrec
      parameter( admtlmrec = Nx*Ny*(4*Nr+1) )
      common / controlvars_admtlm_r /
     &                       cbuffGlobal
      _RL cbuffGlobal( admtlmrec )
#endif

      common /controlparams_r/
     &                       delZexp,
     &                       forcingPrecond

      _RL delZexp
      _RL forcingPrecond

C     doInitXX               ::   at iter 0 only, set ctrls to 0 and write to xx*000.data
C     doMainPack             ::   pack adxx*data files into ecco_cost_* file (usually for optim.x)
C     doMainUnpack           ::   unpack ecco_ctrl_* file (usually from optim.x) into xx_*data files
C     doPackDiag             ::   output diag_pack*/diag_unpack* files during ctrl_pack/ctrl_unpack
C     doSinglePrecTapelev    ::   reduce precision of ad tape files to float32 (only used in pkg/autodiff ...)
C     ctrlSmoothCorrel2D     ::   use pkg/smooth correlation operator (incl. smoother) for 2D controls (Weaver, Courtier 01)
C     ctrlSmoothCorrel3D     ::   use pkg/smooth correlation operator (incl. smoother) for 3D controls (Weaver, Courtier 01)
C     ctrlUseGen             ::   use generic control approach rather than old codes from pkg/ecco

      common /controlvars_l /
     &                       ctrlSmoothCorrel2D,
     &                       ctrlSmoothCorrel3D,
     &                       ctrlUseGen,
     &                       doInitXX,
     &                       doAdmTlm,
     &                       doPackDiag,
     &                       doZscaleUnpack,
     &                       doZscalePack,
     &                       doMainUnpack,
     &                       doMainPack,
     &                       doSinglePrecTapelev,
     &                       doAdmtlmBypassAD

      logical ctrlSmoothCorrel2D, ctrlSmoothCorrel3D
      logical ctrlUseGen
      logical doInitXX
      logical doAdmTlm
      logical doPackDiag
      logical doZscaleUnpack
      logical doZscalePack
      logical doMainUnpack
      logical doMainPack
      logical doSinglePrecTapelev
      logical doAdmtlmBypassAD

      common /controlvars_i/
     &                       nvartype,
     &                       nvarlength,
     &                       ncvarindex,
     &                       ncvarrecs,
     &                       ncvarrecstart,
     &                       ncvarrecsend,
     &                       ncvarxmax,
     &                       ncvarymax,
     &                       ncvarnrmax,
     &                       nwetctile,
     &                       nwetstile,
     &                       nwetwtile,
     &                       nwetvtile,
     &                       nwetcglobal,
     &                       nwetsglobal,
     &                       nwetwglobal,
     &                       nwetvglobal,
     &                       nbuffglobal
      integer nvartype
      integer nvarlength
      integer ncvarindex    ( maxcvars )
      integer ncvarrecs     ( maxcvars )
      integer ncvarrecstart ( maxcvars )
      integer ncvarrecsend  ( maxcvars )
      integer ncvarxmax     ( maxcvars )
      integer ncvarymax     ( maxcvars )
      integer ncvarnrmax    ( maxcvars )
      integer nwetctile     ( nsx,nsy,nr )
      integer nwetstile     ( nsx,nsy,nr )
      integer nwetwtile     ( nsx,nsy,nr )
      integer nwetvtile     ( nsx,nsy,nr )
      integer nwetcglobal     ( nr )
      integer nwetsglobal     ( nr )
      integer nwetwglobal     ( nr )
      integer nwetvglobal     ( nr )
      integer nbuffglobal

#ifdef ALLOW_SHELFICE
      common /controlvars_i_shifwflx/
     &     nwetitile, nwetiglobal, filenWetiGlobal
      integer nwetitile     ( nsx,nsy,nr )
      integer nwetiglobal     ( nr )
      integer filenWetiGlobal(nr)
#endif /* ALLOW_SHELFICE */

      common /controlvars_c/
     &                       ncvargrd
     &                     , yadprefix
      character*(1) ncvargrd(maxcvars)
      character*(2) yadprefix

      common /controlvec_header_i/
     &        filenvartype,
     &        filenvarlength,
     &        fileOptimCycle,
     &        filencbuffindex,
     &        fileIg,
     &        fileJg,
     &        fileI,
     &        fileJ,
     &        filensx,
     &        filensy,
     &        filek,
     &        filenWetcGlobal,
     &        filenWetsGlobal,
     &        filenWetwGlobal,
     &        filenWetvGlobal,
     &        filencvarindex,
     &        filencvarrecs,
     &        filencvarxmax,
     &        filencvarymax,
     &        filencvarnrmax
      integer        filenvartype
      integer        filenvarlength
      integer        fileOptimCycle
      integer        filencbuffindex
      integer        fileIg
      integer        fileJg
      integer        fileI
      integer        fileJ
      integer        filensx
      integer        filensy
      integer        filek
      integer        filenWetcGlobal(nr)
      integer        filenWetsGlobal(nr)
      integer        filenWetwGlobal(nr)
      integer        filenWetvGlobal(nr)
      integer        filencvarindex(maxcvars)
      integer        filencvarrecs(maxcvars)
      integer        filencvarxmax(maxcvars)
      integer        filencvarymax(maxcvars)
      integer        filencvarnrmax(maxcvars)

      common /controlvec_header_r/
     &               filefc
      _RL            filefc

      common /controlvec_header_c/
     &        fileYctrlid,
     &        filencvargrd
      character*(10) fileYctrlid
      character*( 1) filencvargrd(maxcvars)

c     Define unit weight as a placeholder
      common /ctrl_weights_unit_r/
     &                        wunit,
     &                        wareaunit
      _RL wunit     (nr,nsx,nsy)
      _RL wareaunit (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /packnames_c/
     &                      yadmark,
     &                      ctrlname,
     &                      costname,
     &                      scalname,
     &                      maskname,
     &                      metaname,
     &                      yctrlid,
     &                      yctrlposunpack,
     &                      yctrlpospack
      character*2 yadmark
      character*9 ctrlname
      character*9 costname
      character*9 scalname
      character*9 maskname
      character*9 metaname
      character*10 yctrlid
      character*4 yctrlposunpack
      character*4 yctrlpospack

#ifdef ALLOW_ADMTLM
      integer          maxm, maxn
      parameter       ( maxm = Nx*Ny*(4*Nr+1), maxn=Nx*Ny*(4*Nr+1) )

      common /admtlm_i/ nveccount
      integer nveccount

      common /admtlm_r/ phtmpadmtlm
      double precision phtmpadmtlm(maxn)
#endif

#ifdef ECCO_CTRL_DEPRECATED
      _RL num_zero_mean (nsx,nsy)
      _RL objf_zero_mean (nsx,nsy)
      _RL objf_zero_smoo (nsx,nsy)
      common /ctrl_zero_r/ num_zero_mean,
     & objf_zero_mean, objf_zero_smoo
#endif

#ifndef ALLOW_ECCO
      common /ctrl_weights_atmos_r/
     &                      whflux,
     &                      wsflux,
     &                      wtauu,
     &                      wtauv,
     &                      watemp,
     &                      waqh,
     &                      wprecip,
     &                      wswflux,
     &                      wswdown,
     &                      wuwind,
     &                      wvwind,
     &                      wlwflux,
     &                      wlwdown,
     &                      wevap,
     &                      wsnowprecip,
     &                      wapressure,
     &                      wrunoff,
     &                      wsst,
     &                      wsss
      _RL whflux  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wsflux  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wtauu   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wtauv   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL watemp  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL waqh    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wprecip (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wswflux (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wswdown (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wuwind  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wvwind  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wlwflux (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wlwdown (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wevap   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wsnowprecip (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wapressure(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wrunoff (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wsst    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wsss    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

c     Control variables:
c     ==================
c
c     xx_theta - control vector temperature part.
c     xx_salt  - control vector salt part.
c     xx_hflux   - control vector surface heat flux part.
c     xx_sflux   - control vector surface salt flux part.
c     xx_tauu  - control vector zonal wind stress part.
c     xx_tauv  - control vector meridional wind stress part.
cph(
c     xx_... are to be replaced by tmpfld2d/3d throughout the code;
c     control variables are written to / read from active files
c     TAMC sees xx_..._dummy

#ifdef ALLOW_OPENAD
C
      common /controlvars_r_openad/
     &        xx_place_holder
# ifdef ECCO_CTRL_DEPRECATED
     &      , xx_theta
     &      , xx_salt
     &      , xx_uvel
     &      , xx_vvel
     &      , xx_etan
#  ifdef ALLOW_DIFFKR_CONTROL
     &      , xx_diffkr
#  endif
#  ifdef ALLOW_KAPGM_CONTROL
     &      , xx_kapgm
#  endif
#  ifdef ALLOW_TR10_CONTROL
     &      , xx_tr1
#  endif
#  ifdef ALLOW_HFLUXM_CONTROL
     &      , xx_hfluxm
#  endif
# endif /* ECCO_CTRL_DEPRECATED */

# ifdef ALLOW_GENARR2D_CONTROL
     &      , xx_genarr2d
# endif
# ifdef ALLOW_GENARR3D_CONTROL
     &      , xx_genarr3d
# endif
C
# ifdef ECCO_CTRL_DEPRECATED
      _RL xx_theta(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_salt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_uvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_vvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_etan(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#  ifdef ALLOW_DIFFKR_CONTROL
      _RL xx_diffkr(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#  endif
#  ifdef ALLOW_KAPGM_CONTROL
      _RL xx_kapgm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#  endif
#  ifdef ALLOW_TR10_CONTROL
      _RL xx_tr1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#  endif
#  ifdef ALLOW_HFLUXM_CONTROL
      _RL xx_hfluxm(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#  endif
# endif /* ECCO_CTRL_DEPRECATED */

      _RL xx_place_holder

# ifdef ALLOW_GENARR2D_CONTROL
      _RL xx_genarr2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,
     &                maxCtrlArr2D)
# endif
# ifdef ALLOW_GENARR3D_CONTROL
      _RL xx_genarr3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,
     &                maxCtrlArr3D)
# endif

#endif

c     Auxiliary storage arrays for the control variables:
c     ===================================================
c
c     xx_hflux0  - heat flux record before current date.
c     xx_hflux1  - heat flux record after  current date
c     xx_sflux0  - salt flux record before current date.
c     xx_sflux1  - salt flux record after  current date.
c     xx_tauu0 - zonal wind stress record before current date.
c     xx_tauu1 - zonal wind stress record after  current date.
c     xx_tauv0 - meridional wind stress record before current date.
c     xx_tauv1 - meridional wind stress record after  current date.

#ifdef ECCO_CTRL_DEPRECATED
# if     (defined  (ALLOW_HFLUX_CONTROL) || (defined (ALLOW_OPENAD) && defined  (ALLOW_HFLUX0_CONTROL)))
      common /controlaux_hflux_r/
     &                      xx_hflux0,
     &                      xx_hflux1
# elif   (defined  (ALLOW_ATEMP_CONTROL))
      common /controlaux_atemp_r/
     &                      xx_atemp0,
     &                      xx_atemp1
# endif

# if     (defined  (ALLOW_SFLUX_CONTROL) || (defined (ALLOW_OPENAD) && defined  (ALLOW_SFLUX0_CONTROL)))
      common /controlaux_swflux_r/
     &                      xx_sflux0,
     &                      xx_sflux1
# elif   (defined  (ALLOW_AQH_CONTROL))
      common /controlaux_aqh_r/
     &                      xx_aqh0,
     &                      xx_aqh1
# endif

# if (defined (ALLOW_ATM_MEAN_CONTROL))
      common /controlaux_atm_mean_r/
     &                      xx_atemp_mean,
     &                      xx_aqh_mean,
     &                      xx_uwind_mean,
     &                      xx_vwind_mean,
     &                      xx_precip_mean,
     &                      xx_swdown_mean
# endif

# if     (defined  (ALLOW_USTRESS_CONTROL) || (defined (ALLOW_OPENAD) && defined (ALLOW_TAUU0_CONTROL)))
      common /controlaux_ustress_r/
     &                      xx_tauu0,
     &                      xx_tauu1
# endif

# if     (defined  (ALLOW_UWIND_CONTROL))
      common /controlaux_uwind_r/
     &                      xx_uwind0,
     &                      xx_uwind1
# endif

# if     (defined  (ALLOW_VSTRESS_CONTROL) || (defined (ALLOW_OPENAD) && defined (ALLOW_TAUV0_CONTROL)))
      common /controlaux_vstress_r/
     &                      xx_tauv0,
     &                      xx_tauv1
# endif

# if   (defined  (ALLOW_VWIND_CONTROL))
      common /controlaux_vwind_r/
     &                      xx_vwind0,
     &                      xx_vwind1
# endif

# if (defined  (ALLOW_PRECIP_CONTROL))
      common /controlaux_precip_r/
     &                      xx_precip0,
     &                      xx_precip1
# endif

# if (defined  (ALLOW_SWFLUX_CONTROL))
      common /controlaux_swflux_r/
     &                      xx_swflux0,
     &                      xx_swflux1
# endif

# if (defined  (ALLOW_SWDOWN_CONTROL))
      common /controlaux_swdown_r/
     &                      xx_swdown0,
     &                      xx_swdown1
# endif

# if (defined  (ALLOW_LWFLUX_CONTROL))
      common /controlaux_lwflux_r/
     &                      xx_lwflux0,
     &                      xx_lwflux1
# endif

# if (defined  (ALLOW_LWDOWN_CONTROL))
      common /controlaux_lwdown_r/
     &                      xx_lwdown0,
     &                      xx_lwdown1
# endif

# if (defined  (ALLOW_EVAP_CONTROL))
      common /controlaux_evap_r/
     &                      xx_evap0,
     &                      xx_evap1
# endif

# if (defined  (ALLOW_SNOWPRECIP_CONTROL))
      common /controlaux_snowprecip_r/
     &                      xx_snowprecip0,
     &                      xx_snowprecip1
# endif

# if (defined  (ALLOW_APRESSURE_CONTROL))
      common /controlaux_apressure_r/
     &                      xx_apressure0,
     &                      xx_apressure1
# endif

# if (defined  (ALLOW_RUNOFF_CONTROL))
      common /controlaux_runoff_r/
     &                      xx_runoff0,
     &                      xx_runoff1
# endif

# if (defined  (ALLOW_SST_CONTROL))
      common /controlaux_sst_r/
     &                      xx_sst0,
     &                      xx_sst1
# endif
# if (defined  (ALLOW_SSS_CONTROL))
      common /controlaux_sss_r/
     &                      xx_sss0,
     &                      xx_sss1
# endif

# if     (defined  (ALLOW_HFLUX_CONTROL) || (defined (ALLOW_OPENAD) && defined (ALLOW_HFLUX0_CONTROL)))
      _RL xx_hflux0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_hflux1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# elif   (defined  (ALLOW_ATEMP_CONTROL))
      _RL xx_atemp0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_atemp1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if     (defined  (ALLOW_SFLUX_CONTROL) || (defined (ALLOW_OPENAD) && defined (ALLOW_SFLUX0_CONTROL)))
      _RL xx_sflux0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_sflux1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# elif   (defined  (ALLOW_AQH_CONTROL))
      _RL xx_aqh0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_aqh1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if     (defined  (ALLOW_USTRESS_CONTROL) || (defined (ALLOW_OPENAD) && defined (ALLOW_TAUU0_CONTROL)))
      _RL xx_tauu0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_tauu1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if     (defined  (ALLOW_UWIND_CONTROL))
      _RL xx_uwind0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_uwind1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if     (defined  (ALLOW_VSTRESS_CONTROL) || (defined (ALLOW_OPENAD) && defined (ALLOW_TAUV0_CONTROL)))
      _RL xx_tauv0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_tauv1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if     (defined  (ALLOW_VWIND_CONTROL))
      _RL xx_vwind0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_vwind1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
#if (defined  (ALLOW_PRECIP_CONTROL))
      _RL xx_precip0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_precip1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
#if (defined  (ALLOW_SWFLUX_CONTROL))
      _RL xx_swflux0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_swflux1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
# if (defined  (ALLOW_SWDOWN_CONTROL))
      _RL xx_swdown0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_swdown1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if (defined  (ALLOW_LWFLUX_CONTROL))
      _RL xx_lwflux0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_lwflux1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if (defined  (ALLOW_LWDOWN_CONTROL))
      _RL xx_lwdown0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_lwdown1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if (defined  (ALLOW_EVAP_CONTROL))
      _RL xx_evap0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_evap1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if (defined  (ALLOW_SNOWPRECIP_CONTROL))
      _RL xx_snowprecip0
     &    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_snowprecip1
     &    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if (defined  (ALLOW_APRESSURE_CONTROL))
      _RL xx_apressure0
     &    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_apressure1
     &    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if (defined  (ALLOW_RUNOFF_CONTROL))
      _RL xx_runoff0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_runoff1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if (defined  (ALLOW_SST_CONTROL))
      _RL xx_sst0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_sst1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if (defined  (ALLOW_SSS_CONTROL))
      _RL xx_sss0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_sss1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
# if     (defined (ALLOW_ATM_MEAN_CONTROL))
      _RL xx_atemp_mean (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_aqh_mean   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_uwind_mean (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_vwind_mean (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_precip_mean(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_swdown_mean(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif
#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ECCO_CTRL_DEPRECATED
c     Files where the control variables are stored:
c     =============================================
c
c     xx_theta_file - control vector temperature part.
c     xx_salt_file  - control vector salinity part.
c     xx_hflux_file - control vector surface heat flux file.
c     xx_sflux_file - control vector surface salt flux file.
c     xx_tauu_file  - control vector zonal wind stress file.
c     xx_tauv_file  - control vector meridional wind stress file.
      common /controlfiles_c/
     &                      xx_theta_file
     &                    , xx_salt_file
     &                    , xx_hflux_file
     &                    , xx_sflux_file
     &                    , xx_tauu_file
     &                    , xx_tauv_file
     &                    , xx_atemp_file
     &                    , xx_aqh_file
     &                    , xx_precip_file
     &                    , xx_swflux_file
     &                    , xx_swdown_file
     &                    , xx_lwflux_file
     &                    , xx_lwdown_file
     &                    , xx_evap_file
     &                    , xx_snowprecip_file
     &                    , xx_apressure_file
     &                    , xx_runoff_file
     &                    , xx_uwind_file
     &                    , xx_vwind_file
     &                    , xx_atemp_mean_file
     &                    , xx_aqh_mean_file
     &                    , xx_precip_mean_file
     &                    , xx_swdown_mean_file
     &                    , xx_uwind_mean_file
     &                    , xx_vwind_mean_file
     &                    , xx_diffkr_file
     &                    , xx_kapgm_file
     &                    , xx_kapredi_file
     &                    , xx_tr1_file
     &                    , xx_sst_file
     &                    , xx_sss_file
     &                    , xx_depth_file
     &                    , xx_efluxy_file
     &                    , xx_efluxp_file
     &                    , xx_bottomdrag_file
     &                    , xx_edtaux_file
     &                    , xx_edtauy_file
     &                    , xx_uvel_file
     &                    , xx_vvel_file
     &                    , xx_etan_file
     &                    , xx_relaxsst_file
     &                    , xx_relaxsss_file
     &                    , xx_theta_ini_fin_file
     &                    , xx_salt_ini_fin_file
     &                    , xx_siarea_file
     &                    , xx_siheff_file
     &                    , xx_sihsnow_file
     &                    , xx_gen2d_file
     &                    , xx_gen3d_file
cHFLUXM_CONTROL
     &                    , xx_hfluxm_file
cHFLUXM_CONTROL
     &                    , xx_shifwflx_file

      character*(MAX_LEN_FNAM) xx_theta_file
      character*(MAX_LEN_FNAM) xx_salt_file
      character*(MAX_LEN_FNAM) xx_hflux_file
      character*(MAX_LEN_FNAM) xx_sflux_file
      character*(MAX_LEN_FNAM) xx_tauu_file
      character*(MAX_LEN_FNAM) xx_tauv_file
      character*(MAX_LEN_FNAM) xx_atemp_file
      character*(MAX_LEN_FNAM) xx_aqh_file
      character*(MAX_LEN_FNAM) xx_precip_file
      character*(MAX_LEN_FNAM) xx_swflux_file
      character*(MAX_LEN_FNAM) xx_swdown_file
      character*(MAX_LEN_FNAM) xx_lwflux_file
      character*(MAX_LEN_FNAM) xx_lwdown_file
      character*(MAX_LEN_FNAM) xx_evap_file
      character*(MAX_LEN_FNAM) xx_snowprecip_file
      character*(MAX_LEN_FNAM) xx_apressure_file
      character*(MAX_LEN_FNAM) xx_runoff_file
      character*(MAX_LEN_FNAM) xx_uwind_file
      character*(MAX_LEN_FNAM) xx_vwind_file
      character*(MAX_LEN_FNAM) xx_atemp_mean_file
      character*(MAX_LEN_FNAM) xx_aqh_mean_file
      character*(MAX_LEN_FNAM) xx_precip_mean_file
      character*(MAX_LEN_FNAM) xx_swdown_mean_file
      character*(MAX_LEN_FNAM) xx_uwind_mean_file
      character*(MAX_LEN_FNAM) xx_vwind_mean_file
      character*(MAX_LEN_FNAM) xx_diffkr_file
      character*(MAX_LEN_FNAM) xx_kapgm_file
      character*(MAX_LEN_FNAM) xx_kapredi_file
      character*(MAX_LEN_FNAM) xx_tr1_file
      character*(MAX_LEN_FNAM) xx_sst_file
      character*(MAX_LEN_FNAM) xx_sss_file
      character*(MAX_LEN_FNAM) xx_depth_file
      character*(MAX_LEN_FNAM) xx_efluxy_file
      character*(MAX_LEN_FNAM) xx_efluxp_file
      character*(MAX_LEN_FNAM) xx_bottomdrag_file
      character*(MAX_LEN_FNAM) xx_edtaux_file
      character*(MAX_LEN_FNAM) xx_edtauy_file
      character*(MAX_LEN_FNAM) xx_uvel_file
      character*(MAX_LEN_FNAM) xx_vvel_file
      character*(MAX_LEN_FNAM) xx_etan_file
      character*(MAX_LEN_FNAM) xx_relaxsst_file
      character*(MAX_LEN_FNAM) xx_relaxsss_file
      character*(MAX_LEN_FNAM) xx_theta_ini_fin_file
      character*(MAX_LEN_FNAM) xx_salt_ini_fin_file
      character*(MAX_LEN_FNAM) xx_siarea_file
      character*(MAX_LEN_FNAM) xx_siheff_file
      character*(MAX_LEN_FNAM) xx_sihsnow_file
      character*(MAX_LEN_FNAM) xx_gen2d_file
      character*(MAX_LEN_FNAM) xx_gen3d_file
cHFLUXM_CONTROL
      character*(MAX_LEN_FNAM) xx_hfluxm_file
cHFLUXM_CONTROL
      character*(MAX_LEN_FNAM) xx_shifwflx_file

c     Calendar information for the control variables:
c     ===============================================
c
c     xx_${varname}period - sampling interval for the ${varname} control
c                           part in seconds
c     special cases for ifdef ALLOW_CAL (in anology to pkg/exf):
c     xx_${varname}period = -12. : control parameter is the seasonal cycle
c     xx_${varname}period =   0. : control parameter is constant in time
c
c     The naming convention follows mostly that of the exf-pkg. A few
c     examples follow:
c     xx_atempperiod - sampling interval for the atmospheric surface
c                      temperature control part.
c     ...
c     xx_hfluxperiod - sampling interval for the heat flux control part.
c     xx_sfluxperiod - sampling interval for the salt flux control part.
c     xx_tauuperiod  - sampling interval for the zonal wind
c                      stress control part.
c     xx_tauvperiod  - sampling interval for the meridional wind
c                      stress control part.
c     ...

      common /controltimes_r/
     &                        xx_hfluxperiod
     &                      , xx_sfluxperiod
     &                      , xx_tauuperiod
     &                      , xx_tauvperiod
     &                      , xx_atempperiod
     &                      , xx_aqhperiod
     &                      , xx_precipperiod
     &                      , xx_swfluxperiod
     &                      , xx_swdownperiod
     &                      , xx_lwfluxperiod
     &                      , xx_lwdownperiod
     &                      , xx_evapperiod
     &                      , xx_snowprecipperiod
     &                      , xx_apressureperiod
     &                      , xx_runoffperiod
     &                      , xx_uwindperiod
     &                      , xx_vwindperiod
     &                      , xx_sstperiod
     &                      , xx_sssperiod
     &                      , xx_shifwflxperiod
      _RL     xx_hfluxperiod
      _RL     xx_sfluxperiod
      _RL     xx_tauuperiod
      _RL     xx_tauvperiod
      _RL     xx_atempperiod
      _RL     xx_aqhperiod
      _RL     xx_precipperiod
      _RL     xx_swfluxperiod
      _RL     xx_swdownperiod
      _RL     xx_lwfluxperiod
      _RL     xx_lwdownperiod
      _RL     xx_evapperiod
      _RL     xx_snowprecipperiod
      _RL     xx_apressureperiod
      _RL     xx_runoffperiod
      _RL     xx_uwindperiod
      _RL     xx_vwindperiod
      _RL     xx_sstperiod
      _RL     xx_sssperiod
      _RL     xx_shifwflxperiod

      common /ctrl_param_trend_removal/
     &       xx_hflux_remo_intercept, xx_hflux_remo_slope,
     &       xx_sflux_remo_intercept, xx_sflux_remo_slope,
     &       xx_tauu_remo_intercept, xx_tauu_remo_slope,
     &       xx_tauv_remo_intercept, xx_tauv_remo_slope,
     &       xx_atemp_remo_intercept, xx_atemp_remo_slope,
     &       xx_aqh_remo_intercept, xx_aqh_remo_slope,
     &       xx_precip_remo_intercept, xx_precip_remo_slope,
     &       xx_swflux_remo_intercept, xx_swflux_remo_slope,
     &       xx_swdown_remo_intercept, xx_swdown_remo_slope,
     &       xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     &       xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     &       xx_evap_remo_intercept, xx_evap_remo_slope,
     &       xx_snowprecip_remo_intercept,
     &       xx_snowprecip_remo_slope,
     &       xx_apressure_remo_intercept,
     &       xx_apressure_remo_slope,
     &       xx_sst_remo_intercept, xx_sst_remo_slope,
     &       xx_sss_remo_intercept, xx_sss_remo_slope,
     &       xx_runoff_remo_intercept, xx_runoff_remo_slope,
     &       xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &       xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &       xx_shifwflx_remo_intercept, xx_shifwflx_remo_slope

      _RL xx_hflux_remo_intercept, xx_hflux_remo_slope
      _RL xx_sflux_remo_intercept, xx_sflux_remo_slope
      _RL xx_tauu_remo_intercept, xx_tauu_remo_slope
      _RL xx_tauv_remo_intercept, xx_tauv_remo_slope
      _RL xx_atemp_remo_intercept, xx_atemp_remo_slope
      _RL xx_aqh_remo_intercept, xx_aqh_remo_slope
      _RL xx_precip_remo_intercept, xx_precip_remo_slope
      _RL xx_swflux_remo_intercept, xx_swflux_remo_slope
      _RL xx_swdown_remo_intercept, xx_swdown_remo_slope
      _RL xx_lwflux_remo_intercept, xx_lwflux_remo_slope
      _RL xx_lwdown_remo_intercept, xx_lwdown_remo_slope
      _RL xx_evap_remo_intercept, xx_evap_remo_slope
      _RL xx_snowprecip_remo_intercept
      _RL xx_snowprecip_remo_slope
      _RL xx_apressure_remo_intercept
      _RL xx_apressure_remo_slope
      _RL xx_sst_remo_intercept, xx_sst_remo_slope
      _RL xx_sss_remo_intercept, xx_sss_remo_slope
      _RL xx_runoff_remo_intercept, xx_runoff_remo_slope
      _RL xx_uwind_remo_intercept, xx_uwind_remo_slope
      _RL xx_vwind_remo_intercept, xx_vwind_remo_slope
      _RL xx_shifwflx_remo_intercept,xx_shifwflx_remo_slope

c     xx_hfluxstartdate - start date for the heat flux control part.
c     xx_sfluxstartdate - start date for the salt flux control part.
c     xx_tauustartdate  - start date for the zonal wind stress
c                         control part.
c     xx_tauvstartdate  - start date for the meridional wind stress
c                         control part.

      common /controltimes_i/
     &                        xx_hfluxstartdate1
     &                      , xx_hfluxstartdate2
     &                      , xx_sfluxstartdate1
     &                      , xx_sfluxstartdate2
     &                      , xx_tauustartdate1
     &                      , xx_tauustartdate2
     &                      , xx_tauvstartdate1
     &                      , xx_tauvstartdate2
     &                      , xx_atempstartdate1
     &                      , xx_atempstartdate2
     &                      , xx_aqhstartdate1
     &                      , xx_aqhstartdate2
     &                      , xx_precipstartdate1
     &                      , xx_precipstartdate2
     &                      , xx_swfluxstartdate1
     &                      , xx_swfluxstartdate2
     &                      , xx_swdownstartdate1
     &                      , xx_swdownstartdate2
     &                      , xx_snowprecipstartdate1
     &                      , xx_snowprecipstartdate2
     &                      , xx_lwfluxstartdate1
     &                      , xx_lwfluxstartdate2
     &                      , xx_lwdownstartdate1
     &                      , xx_lwdownstartdate2
     &                      , xx_evapstartdate1
     &                      , xx_evapstartdate2
     &                      , xx_apressurestartdate1
     &                      , xx_apressurestartdate2
     &                      , xx_runoffstartdate1
     &                      , xx_runoffstartdate2
     &                      , xx_uwindstartdate1
     &                      , xx_uwindstartdate2
     &                      , xx_vwindstartdate1
     &                      , xx_vwindstartdate2
     &                      , xx_sststartdate1
     &                      , xx_sststartdate2
     &                      , xx_sssstartdate1
     &                      , xx_sssstartdate2
     &                      , xx_hfluxstartdate
     &                      , xx_sfluxstartdate
     &                      , xx_tauustartdate
     &                      , xx_tauvstartdate
     &                      , xx_atempstartdate
     &                      , xx_aqhstartdate
     &                      , xx_precipstartdate
     &                      , xx_swfluxstartdate
     &                      , xx_swdownstartdate
     &                      , xx_uwindstartdate
     &                      , xx_snowprecipstartdate
     &                      , xx_lwfluxstartdate
     &                      , xx_lwdownstartdate
     &                      , xx_evapstartdate
     &                      , xx_apressurestartdate
     &                      , xx_runoffstartdate
     &                      , xx_vwindstartdate
     &                      , xx_sststartdate
     &                      , xx_sssstartdate
     &                      , xx_shifwflxstartdate1
     &                      , xx_shifwflxstartdate2
     &                      , xx_shifwflxstartdate
      integer xx_hfluxstartdate1
      integer xx_hfluxstartdate2
      integer xx_sfluxstartdate1
      integer xx_sfluxstartdate2
      integer xx_tauustartdate1
      integer xx_tauustartdate2
      integer xx_tauvstartdate1
      integer xx_tauvstartdate2
      integer xx_atempstartdate1
      integer xx_atempstartdate2
      integer xx_aqhstartdate1
      integer xx_aqhstartdate2
      integer xx_precipstartdate1
      integer xx_precipstartdate2
      integer xx_swfluxstartdate1
      integer xx_swfluxstartdate2
      integer xx_swdownstartdate1
      integer xx_swdownstartdate2
      integer xx_snowprecipstartdate1
      integer xx_snowprecipstartdate2
      integer xx_lwfluxstartdate1
      integer xx_lwfluxstartdate2
      integer xx_lwdownstartdate1
      integer xx_lwdownstartdate2
      integer xx_evapstartdate1
      integer xx_evapstartdate2
      integer xx_apressurestartdate1
      integer xx_apressurestartdate2
      integer xx_runoffstartdate1
      integer xx_runoffstartdate2
      integer xx_uwindstartdate1
      integer xx_uwindstartdate2
      integer xx_vwindstartdate1
      integer xx_vwindstartdate2
      integer xx_sststartdate1
      integer xx_sststartdate2
      integer xx_sssstartdate1
      integer xx_sssstartdate2
      integer xx_shifwflxstartdate1
      integer xx_shifwflxstartdate2

      integer xx_hfluxstartdate(4)
      integer xx_sfluxstartdate(4)
      integer xx_tauustartdate(4)
      integer xx_tauvstartdate(4)
      integer xx_atempstartdate(4)
      integer xx_aqhstartdate(4)
      integer xx_precipstartdate(4)
      integer xx_swfluxstartdate(4)
      integer xx_swdownstartdate(4)
      integer xx_snowprecipstartdate(4)
      integer xx_lwfluxstartdate(4)
      integer xx_lwdownstartdate(4)
      integer xx_evapstartdate(4)
      integer xx_apressurestartdate(4)
      integer xx_runoffstartdate(4)
      integer xx_uwindstartdate(4)
      integer xx_vwindstartdate(4)
      integer xx_sststartdate(4)
      integer xx_sssstartdate(4)
      integer xx_shifwflxstartdate(4)

#endif /* ECCO_CTRL_DEPRECATED */

c     ==================================================================
c     END OF HEADER CONTROLVARS ctrl.h
c     ==================================================================
