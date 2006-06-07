 

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
c     changed: Christian Eckert eckert@mit.edu
c
c
c     ==================================================================
c     HEADER CONTROLVARS
c     ==================================================================
c
c     nwet[c/s/w]tile - Number of wet points in a tile for center (c),
c                       south (s), and western (w) mask, resp. .

      integer     maxcvars
#ifdef CTRL_SET_OLD_MAXCVARS_30
      parameter ( maxcvars = 30 )
#else
      parameter ( maxcvars = 40 )
#endif

cph ctrlprec will be set to 32 for ECCO to reduce I/O
cph but jeopardizes some gradient checks, so should be
cph set to 64 by default.
cph Need to put this in namelist at some point!
      integer     ctrlprec
#ifdef CTRL_SET_PREC_32
      parameter ( ctrlprec = 32 )
#else
      parameter ( ctrlprec = 64 )
#endif

#ifdef ALLOW_ADMTLM
      integer admtlmrec
      parameter( admtlmrec = Nx*Ny*(4*Nr+1) )
      common / controlvars_admtlm_r /
     &                       cbuffGlobal
      _RL cbuffGlobal( admtlmrec )
#endif
    
      common /controlparams_r/
     &                       delZexp
      _RL delZexp

      common /controlvars_l /
     &                       doInitXX,
     &                       doAdmTlm,
     &                       doPackDiag,
     &                       doZscaleUnpack,
     &                       doZscalePack,
     &                       doMainUnpack,
     &                       doMainPack,
     &                       doAdmtlmBypassAD

      logical doInitXX
      logical doAdmTlm
      logical doPackDiag
      logical doZscaleUnpack
      logical doZscalePack
      logical doMainUnpack
      logical doMainPack
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

#ifdef ALLOW_OBCSN_CONTROL
      common /controlvars_i_obcsn/
     &                       nwetobcsn,
     &                       nwetobcsnglo
      integer nwetobcsn     ( nsx,nsy,nr,nobcs )
      integer nwetobcsnglo  ( nr,nobcs )
#endif
#ifdef ALLOW_OBCSS_CONTROL
      common /controlvars_i_obcss/
     &                       nwetobcss,
     &                       nwetobcssglo
      integer nwetobcss     ( nsx,nsy,nr,nobcs )
      integer nwetobcssglo  ( nr,nobcs )
#endif
#ifdef ALLOW_OBCSW_CONTROL
      common /controlvars_i_obcsw/
     &                       nwetobcsw,
     &                       nwetobcswglo
      integer nwetobcsw     ( nsx,nsy,nr,nobcs )
      integer nwetobcswglo  ( nr,nobcs )
#endif
#ifdef ALLOW_OBCSE_CONTROL
      common /controlvars_i_obcse/
     &                       nwetobcse,
     &                       nwetobcseglo
      integer nwetobcse     ( nsx,nsy,nr,nobcs )
      integer nwetobcseglo  ( nr,nobcs )
#endif

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

      common /controlvars_r/
     &                        tmpfld2d
     &                      , tmpfld3d
      _RL tmpfld2d 
     &    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL tmpfld3d 
     &    (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

#ifdef ALLOW_OPENAD
      common /controlvars_r_openad/
     &        xx_theta
     &      , xx_salt
     &      , xx_uvel
     &      , xx_vvel
     &      , xx_etan
      _RL xx_theta(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_salt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_uvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_vvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xx_etan(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif


cgg  This caused a lot of confusion.
#ifdef ALLOW_OBCS_CONTROL
      common /controlvars_r_obcs/
     &                        tmpfldxz
     &                      , tmpfldxz2
     &                      , tmpfldyz
     &                      , tmpfldyz2

      _RL tmpfldxz  (1-olx:snx+olx,nr,nsx,nsy)
      _RL tmpfldxz2 (1-olx:snx+olx,nr,nsx,nsy)
      _RL tmpfldyz  (1-oly:sny+oly,nr,nsx,nsy)
      _RL tmpfldyz2 (1-oly:sny+oly,nr,nsx,nsy)
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

#if     (defined  (ALLOW_HFLUX_CONTROL))
      common /controlaux_hflux_r/
     &                      xx_hflux0,
     &                      xx_hflux1
#elif   (defined  (ALLOW_ATEMP_CONTROL))
      common /controlaux_atemp_r/
     &                      xx_atemp0,
     &                      xx_atemp1
#endif

#if     (defined  (ALLOW_SFLUX_CONTROL))
      common /controlaux_swflux_r/
     &                      xx_sflux0,
     &                      xx_sflux1
#elif   (defined  (ALLOW_AQH_CONTROL))
      common /controlaux_aqh_r/
     &                      xx_aqh0,
     &                      xx_aqh1
#endif

#if     (defined  (ALLOW_USTRESS_CONTROL))
      common /controlaux_ustress_r/
     &                      xx_tauu0,
     &                      xx_tauu1
#elif   (defined  (ALLOW_UWIND_CONTROL))
      common /controlaux_uwind_r/
     &                      xx_uwind0,
     &                      xx_uwind1
#endif

#if     (defined  (ALLOW_VSTRESS_CONTROL))
      common /controlaux_vstress_r/
     &                      xx_tauv0,
     &                      xx_tauv1
#elif   (defined  (ALLOW_VWIND_CONTROL))
      common /controlaux_vwind_r/
     &                      xx_vwind0,
     &                      xx_vwind1
#endif

#ifdef ALLOW_OBCS_CONTROL
#if     (defined (ALLOW_OBCSN_CONTROL))
      common /controlaux_obcsn_r/
     &                      xx_obcsn0,
     &                      xx_obcsn1
#endif

#if     (defined (ALLOW_OBCSS_CONTROL))
      common /controlaux_obcss_r/
     &                      xx_obcss0,
     &                      xx_obcss1
#endif
#if     (defined (ALLOW_OBCSW_CONTROL))
      common /controlaux_obcsw_r/
     &                      xx_obcsw0,
     &                      xx_obcsw1
#endif
#if     (defined (ALLOW_OBCSE_CONTROL))
      common /controlaux_obcse_r/
     &                      xx_obcse0,
     &                      xx_obcse1
#endif
#endif

#if (defined  (ALLOW_PRECIP_CONTROL))
      common /controlaux_precip_r/
     &                      xx_precip0,
     &                      xx_precip1
#endif

#if (defined  (ALLOW_SWFLUX_CONTROL))
      common /controlaux_swflux_r/
     &                      xx_swflux0,
     &                      xx_swflux1
#endif

#if (defined  (ALLOW_SWDOWN_CONTROL))
      common /controlaux_swdown_r/
     &                      xx_swdown0,
     &                      xx_swdown1
#endif

#if (defined  (ALLOW_SST_CONTROL))
      common /controlaux_sst_r/
     &                      xx_sst0,
     &                      xx_sst1
#endif
#if (defined  (ALLOW_SSS_CONTROL))
      common /controlaux_sss_r/
     &                      xx_sss0,
     &                      xx_sss1
#endif

#if     (defined  (ALLOW_HFLUX_CONTROL))
      _RL xx_hflux0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_hflux1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#elif   (defined  (ALLOW_ATEMP_CONTROL))
      _RL xx_atemp0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_atemp1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#if     (defined  (ALLOW_SFLUX_CONTROL))
      _RL xx_sflux0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_sflux1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#elif   (defined  (ALLOW_AQH_CONTROL))
      _RL xx_aqh0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_aqh1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#if     (defined  (ALLOW_USTRESS_CONTROL))
      _RL xx_tauu0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_tauu1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#elif   (defined  (ALLOW_UWIND_CONTROL))
      _RL xx_uwind0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_uwind1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#if     (defined  (ALLOW_VSTRESS_CONTROL))
      _RL xx_tauv0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_tauv1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#elif   (defined  (ALLOW_VWIND_CONTROL))
      _RL xx_vwind0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_vwind1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#if (defined  (ALLOW_PRECIP_CONTROL))
      _RL xx_precip0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_precip1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#if (defined  (ALLOW_SWFLUX_CONTROL))
      _RL xx_swflux0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_swflux1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#if (defined  (ALLOW_SWDOWN_CONTROL))
      _RL xx_swdown0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_swdown1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#if (defined  (ALLOW_SST_CONTROL))
      _RL xx_sst0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_sst1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#if (defined  (ALLOW_SSS_CONTROL))
      _RL xx_sss0 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xx_sss1 (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#ifdef ALLOW_OBCSN_CONTROL
      _RL xx_obcsn0 (1-Olx:sNx+Olx,Nr,nSx,nSy,nobcs)
      _RL xx_obcsn1 (1-Olx:sNx+Olx,Nr,nSx,nSy,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      _RL xx_obcss0 (1-Olx:sNx+Olx,Nr,nSx,nSy,nobcs)
      _RL xx_obcss1 (1-Olx:sNx+Olx,Nr,nSx,nSy,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      _RL xx_obcsw0 (1-Oly:sNy+Oly,Nr,nSx,nSy,nobcs)
      _RL xx_obcsw1 (1-Oly:sNy+Oly,Nr,nSx,nSy,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      _RL xx_obcse0 (1-Oly:sNy+Oly,Nr,nSx,nSy,nobcs)
      _RL xx_obcse1 (1-Oly:sNy+Oly,Nr,nSx,nSy,nobcs)
#endif


c     Files where the control variables are stored:
c     =============================================
c
c     xx_theta_file - control vector temperature part.
c     xx_salt_file  - control vector salinity part.
c     xx_hflux_file - control vector surface heat flux file.
c     xx_sflux_file - control vector surface salt flux file.
c     xx_tauu_file  - control vector zonal wind stress file.
c     xx_tauv_file  - control vector meridional wind stress file.
c     xx_obcsn_file - control vector Uvel at boundary
c     xx_obcss_file - control vector Vvel at boundary
c     xx_obcsw_file - control vector temp. at boundary
c     xx_obcse_file - control vector salin. at boundary 
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
     &                    , xx_uwind_file
     &                    , xx_vwind_file
     &                    , xx_obcsn_file
     &                    , xx_obcss_file
     &                    , xx_obcsw_file
     &                    , xx_obcse_file
     &                    , xx_diffkr_file
     &                    , xx_kapgm_file
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
      character*(MAX_LEN_FNAM) xx_uwind_file
      character*(MAX_LEN_FNAM) xx_vwind_file
      character*(MAX_LEN_FNAM) xx_obcsn_file
      character*(MAX_LEN_FNAM) xx_obcss_file
      character*(MAX_LEN_FNAM) xx_obcsw_file
      character*(MAX_LEN_FNAM) xx_obcse_file
      character*(MAX_LEN_FNAM) xx_diffkr_file
      character*(MAX_LEN_FNAM) xx_kapgm_file
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

c     Calendar information for the control variables:
c     ===============================================
c
c     xx_hfluxperiod - sampling interval for the heat flux control part.
c     xx_sfluxperiod - sampling interval for the salt flux control part.
c     xx_tauuperiod  - sampling interval for the zonal wind
c                      stress control part.
c     xx_tauvperiod  - sampling interval for the meridional wind
c                      stress control part.
c     xx_obcsuperiod - sampling interval
c     xx_obcsvperiod - sampling interval
c     xx_obcstperiod - sampling interval
c     xx_obcssperiod - sampling interval

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
     &                      , xx_uwindperiod
     &                      , xx_vwindperiod
     &                      , xx_sstperiod
     &                      , xx_sssperiod
     &                      , xx_obcsnperiod
     &                      , xx_obcssperiod
     &                      , xx_obcswperiod
     &                      , xx_obcseperiod
      _RL     xx_hfluxperiod
      _RL     xx_sfluxperiod
      _RL     xx_tauuperiod
      _RL     xx_tauvperiod
      _RL     xx_atempperiod
      _RL     xx_aqhperiod
      _RL     xx_precipperiod
      _RL     xx_swfluxperiod
      _RL     xx_swdownperiod
      _RL     xx_uwindperiod
      _RL     xx_vwindperiod
      _RL     xx_sstperiod
      _RL     xx_sssperiod
      _RL     xx_obcsnperiod
      _RL     xx_obcssperiod
      _RL     xx_obcswperiod
      _RL     xx_obcseperiod

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
     &       xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &       xx_vwind_remo_intercept, xx_vwind_remo_slope

      _RL xx_hflux_remo_intercept, xx_hflux_remo_slope
      _RL xx_sflux_remo_intercept, xx_sflux_remo_slope
      _RL xx_tauu_remo_intercept, xx_tauu_remo_slope
      _RL xx_tauv_remo_intercept, xx_tauv_remo_slope
      _RL xx_atemp_remo_intercept, xx_atemp_remo_slope
      _RL xx_aqh_remo_intercept, xx_aqh_remo_slope
      _RL xx_precip_remo_intercept, xx_precip_remo_slope
      _RL xx_swflux_remo_intercept, xx_swflux_remo_slope
      _RL xx_swdown_remo_intercept, xx_swdown_remo_slope
      _RL xx_uwind_remo_intercept, xx_uwind_remo_slope
      _RL xx_vwind_remo_intercept, xx_vwind_remo_slope


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
     &                      , xx_vwindstartdate
     &                      , xx_sststartdate
     &                      , xx_sssstartdate
     &                      , xx_obcsnstartdate1
     &                      , xx_obcsnstartdate2
     &                      , xx_obcssstartdate1
     &                      , xx_obcssstartdate2
     &                      , xx_obcswstartdate1
     &                      , xx_obcswstartdate2
     &                      , xx_obcsestartdate1
     &                      , xx_obcsestartdate2
     &                      , xx_obcsnstartdate
     &                      , xx_obcssstartdate
     &                      , xx_obcswstartdate
     &                      , xx_obcsestartdate
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
      integer xx_uwindstartdate1
      integer xx_uwindstartdate2
      integer xx_vwindstartdate1
      integer xx_vwindstartdate2
      integer xx_sststartdate1
      integer xx_sststartdate2
      integer xx_sssstartdate1
      integer xx_sssstartdate2
      integer xx_obcsnstartdate1
      integer xx_obcsnstartdate2
      integer xx_obcssstartdate1
      integer xx_obcssstartdate2
      integer xx_obcswstartdate1
      integer xx_obcswstartdate2
      integer xx_obcsestartdate1
      integer xx_obcsestartdate2

      integer xx_hfluxstartdate(4)
      integer xx_sfluxstartdate(4)
      integer xx_tauustartdate(4)
      integer xx_tauvstartdate(4)
      integer xx_atempstartdate(4)
      integer xx_aqhstartdate(4)
      integer xx_precipstartdate(4)
      integer xx_swfluxstartdate(4)
      integer xx_swdownstartdate(4)
      integer xx_uwindstartdate(4)
      integer xx_vwindstartdate(4)
      integer xx_sststartdate(4)
      integer xx_sssstartdate(4)
      integer xx_obcsnstartdate(4)
      integer xx_obcssstartdate(4)
      integer xx_obcswstartdate(4)
      integer xx_obcsestartdate(4)

      character*( 80)   fname_theta(2)
      character*( 80)   fname_salt(2)
      character*( 80)   fname_hflux(2)
      character*( 80)   fname_sflux(2)
      character*( 80)   fname_tauu(2)
      character*( 80)   fname_tauv(2)
      character*( 80)   fname_atemp(2)
      character*( 80)   fname_aqh(2)
      character*( 80)   fname_precip(2)
      character*( 80)   fname_swflux(2)
      character*( 80)   fname_swdown(2)
      character*( 80)   fname_uwind(2)
      character*( 80)   fname_vwind(2)
      character*( 80)   fname_obcsn(2)
      character*( 80)   fname_obcss(2)
      character*( 80)   fname_obcsw(2)
      character*( 80)   fname_obcse(2)
      character*( 80)   fname_diffkr(2)
      character*( 80)   fname_kapgm(2)
      character*( 80)   fname_tr1(2)
      character*( 80)   fname_sst(2)
      character*( 80)   fname_sss(2)
      character*( 80)   fname_depth(2)
      character*( 80)   fname_hfacc(2)
      character*( 80)   fname_efluxy(2)
      character*( 80)   fname_efluxp(2)
      character*( 80)   fname_bottomdrag(2)
      character*( 80)   fname_edtaux(2)
      character*( 80)   fname_edtauy(2)
      character*( 80)   fname_uvel(2)
      character*( 80)   fname_vvel(2)
      character*( 80)   fname_etan(2)
      character*( 80)   fname_relaxsst(2)
      character*( 80)   fname_relaxsss(2)

#ifdef ALLOW_ADMTLM
      integer          maxm, maxn
      parameter       ( maxm = Nx*Ny*(4*Nr+1), maxn=Nx*Ny*(4*Nr+1) )

      common /admtlm_i/ nveccount
      integer nveccount

      common /admtlm_r/ phtmpadmtlm
      double precision phtmpadmtlm(maxn)
#endif


c     ==================================================================
c     END OF HEADER CONTROLVARS
c     ==================================================================


