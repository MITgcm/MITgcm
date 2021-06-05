#include "CTRL_OPTIONS.h"
#include "AD_CONFIG.h"

      subroutine ctrl_readparms( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_readparms
c     ==================================================================
c
c     o read ctrl parameters
c
c     ==================================================================
c     SUBROUTINE ctrl_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "CTRL_GENARR.h"
# include "CTRL_OBCS.h"
# ifndef ECCO_CTRL_DEPRECATED
#  include "ctrl_local_params.h"
# endif
#endif
#ifdef ALLOW_AUTODIFF
#include "AUTODIFF_PARAMS.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer ntmp
      integer ivarindex
      integer iUnit
      integer iobcs
      integer il
      integer errio
      integer startrec
      integer endrec
      integer difftime(4)
      _RL     diffsecs
      _RL     dummy

      character*(80)   ymaskobcs
      character*(max_len_prec) record
      character*(max_len_mbuf) msgbuf

      integer nwetc3d
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      integer iarr,jarr
#endif

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Read the namelist input.
      namelist /ctrl_nml/
     &  xx_theta_file, xx_salt_file,
     &  xx_hflux_file, xx_hflux_remo_intercept, xx_hflux_remo_slope,
     &  xx_hfluxstartdate1, xx_hfluxstartdate2, xx_hfluxperiod,
     &  xx_sflux_file, xx_sflux_remo_intercept, xx_sflux_remo_slope,
     &  xx_sfluxstartdate1, xx_sfluxstartdate2, xx_sfluxperiod,
     &  xx_tauu_file, xx_tauu_remo_intercept, xx_tauu_remo_slope,
     &  xx_tauustartdate1,  xx_tauustartdate2,  xx_tauuperiod,
     &  xx_tauv_file, xx_tauv_remo_intercept, xx_tauv_remo_slope,
     &  xx_tauvstartdate1,  xx_tauvstartdate2,  xx_tauvperiod,
     &  xx_atemp_file, xx_atemp_remo_intercept, xx_atemp_remo_slope,
     &  xx_atempstartdate1, xx_atempstartdate2, xx_atempperiod,
     &  xx_aqh_file, xx_aqh_remo_intercept, xx_aqh_remo_slope,
     &  xx_aqhstartdate1, xx_aqhstartdate2, xx_aqhperiod,
     &  xx_precip_file, xx_precip_remo_intercept, xx_precip_remo_slope,
     &  xx_precipstartdate1, xx_precipstartdate2, xx_precipperiod,
     &  xx_swflux_file, xx_swflux_remo_intercept, xx_swflux_remo_slope,
     &  xx_swfluxstartdate1, xx_swfluxstartdate2, xx_swfluxperiod,
     &  xx_swdown_file, xx_swdown_remo_intercept, xx_swdown_remo_slope,
     &  xx_swdownstartdate1, xx_swdownstartdate2, xx_swdownperiod,
     &  xx_lwflux_file, xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     &  xx_lwfluxstartdate1, xx_lwfluxstartdate2, xx_lwfluxperiod,
     &  xx_lwdown_file, xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     &  xx_lwdownstartdate1, xx_lwdownstartdate2, xx_lwdownperiod,
     &  xx_evap_file, xx_evap_remo_intercept, xx_evap_remo_slope,
     &  xx_evapstartdate1, xx_evapstartdate2, xx_evapperiod,
     &  xx_snowprecip_file, xx_snowprecip_remo_intercept,
     &  xx_snowprecip_remo_slope,  xx_snowprecipperiod,
     &  xx_snowprecipstartdate1, xx_snowprecipstartdate2,
     &  xx_apressure_file, xx_apressure_remo_intercept,
     &  xx_apressure_remo_slope, xx_apressureperiod,
     &  xx_apressurestartdate1, xx_apressurestartdate2,
     &  xx_runoff_file, xx_runoff_remo_intercept, xx_runoff_remo_slope,
     &  xx_runoffstartdate1, xx_runoffstartdate2, xx_runoffperiod,
     &  xx_uwind_file, xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &  xx_uwindstartdate1, xx_uwindstartdate2, xx_uwindperiod,
     &  xx_vwind_file, xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &  xx_vwindstartdate1, xx_vwindstartdate2, xx_vwindperiod,
     &  xx_aqh_mean_file, xx_atemp_mean_file, xx_precip_mean_file,
     &  xx_swdown_mean_file, xx_uwind_mean_file, xx_vwind_mean_file,
     &  xx_obcsn_file,
     &  xx_obcsnstartdate1,  xx_obcsnstartdate2,  xx_obcsnperiod,
     &  xx_obcss_file,
     &  xx_obcssstartdate1,  xx_obcssstartdate2,  xx_obcssperiod,
     &  xx_obcsw_file,
     &  xx_obcswstartdate1,  xx_obcswstartdate2,  xx_obcswperiod,
     &  xx_obcse_file,
     &  xx_obcsestartdate1,  xx_obcsestartdate2,  xx_obcseperiod,
     &  xx_diffkr_file, xx_kapgm_file, xx_kapredi_file, xx_tr1_file,
     &  xx_sst_file, xx_sst_remo_intercept, xx_sst_remo_slope,
     &  xx_sss_file, xx_sss_remo_intercept, xx_sss_remo_slope,
     &  xx_sststartdate1, xx_sststartdate2, xx_sstperiod,
     &  xx_sssstartdate1, xx_sssstartdate2, xx_sssperiod,
     &  xx_depth_file, xx_gen2d_file, xx_gen3d_file,
     &  xx_efluxy_file, xx_efluxp_file, xx_bottomdrag_file,
     &  xx_edtaux_file, xx_edtauy_file,
     &  xx_uvel_file, xx_vvel_file, xx_etan_file,
     &  xx_siarea_file, xx_siheff_file, xx_sihsnow_file,
     &  xx_shifwflx_file,
     &  xx_shifwflx_remo_intercept, xx_shifwflx_remo_slope,
     &  xx_shifwflxstartdate1, xx_shifwflxstartdate2, xx_shifwflxperiod,
     &  doInitXX, ctrlSmoothCorrel3D, ctrlSmoothCorrel2D, ctrlUseGen,
     &  doPackDiag,
     &  doZscaleUnpack, doZscalePack,
     &  doMainUnpack, doMainPack,
     &  doSinglePrecTapelev,
     &  doAdmtlmBypassAD,
     &  delZexp, forcingPrecond
cHFLUXM_CONTROL
     &  ,xx_hfluxm_file
cHFLUXM_CONTROL
     &  ,ctrlprec

      namelist /ctrl_packnames/
     &  yadmark, yctrlid, yctrlposunpack, yctrlpospack,
     &  ctrlname, costname, scalname, maskname, metaname

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      namelist /ctrl_nml_genarr/
     &  xx_genarr2d_file, xx_genarr3d_file, xx_gentim2d_file,
     &  xx_genarr2d_weight, xx_genarr3d_weight, xx_gentim2d_weight,
     &  xx_genarr2d_bounds, xx_genarr3d_bounds, xx_gentim2d_bounds,
     &  genarr2dPrecond, genarr3dPrecond, gentim2dPrecond,
     &  mult_gentim2d,mult_genarr2d,mult_genarr3d,
     &  xx_genarr2d_preproc, xx_genarr2d_preproc_i,
     &  xx_genarr2d_preproc_r, xx_genarr2d_preproc_c,
     &  xx_genarr3d_preproc, xx_genarr3d_preproc_i,
     &  xx_genarr3d_preproc_r, xx_genarr3d_preproc_c,
     &  xx_gentim2d_preproc, xx_gentim2d_preproc_i,
     &  xx_gentim2d_preproc_r, xx_gentim2d_preproc_c,
     &  xx_gentim2d_period,
     &  xx_gentim2d_startdate1, xx_gentim2d_startdate2,
     &  xx_gentim2d_cumsum,xx_gentim2d_glosum
#endif

      IF ( .NOT.useCTRL ) THEN
C-    pkg CTRL is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ctrl is found
         CALL PACKAGES_UNUSED_MSG( 'useCTRL', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

c--     Set default values.
        doInitXX            = .TRUE.
#ifdef ALLOW_SMOOTH_CORREL3D
        ctrlSmoothCorrel3D  = .TRUE.
#else
        ctrlSmoothCorrel3D  = .FALSE.
#endif
#ifdef ALLOW_SMOOTH_CORREL2D
        ctrlSmoothCorrel2D  = .TRUE.
#else
        ctrlSmoothCorrel2D  = .FALSE.
#endif
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
        ctrlUseGen          = .TRUE.
#else
        ctrlUseGen          = .FALSE.
#endif
#ifdef ALLOW_ADMTLM
        doAdmTlm            = .TRUE.
#else
        doAdmTlm            = .FALSE.
#endif
        doPackDiag          = .FALSE.
        doZscaleUnpack      = .FALSE.
        doZscalePack        = .FALSE.
        doMainUnpack        = .TRUE.
#if (defined (ALLOW_ADJOINT_RUN)||defined (ALLOW_TANGENTLINEAR_RUN))
        doMainPack          = .TRUE.
#else
        doMainPack          = .FALSE.
#endif
        doSinglePrecTapelev = .FALSE.
        doAdmtlmBypassAD    = .FALSE.

        delZexp = 0.
        forcingPrecond = 1. _d 0

#ifdef CTRL_SET_PREC_32
C     for backward compatibility set default value to 32 if this
C     flag is defined
        ctrlprec = 32
#else
        ctrlprec = 64
#endif

        xx_theta_file      = 'xx_theta'
        xx_salt_file       = 'xx_salt'
c
        xx_gen2d_file      = 'xx_gen2d'
        xx_gen3d_file      = 'xx_gen3d'

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
        do iarr = 1, maxCtrlArr2D
         write(xx_genarr2d_file(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
#ifdef ALLOW_OPENAD
         write(xx_genarr2d_file(iarr)(1:14),'(a,i2.2)')
     &        'xx_genarr2d_', iarr
         write(xx_genarr2d_file(iarr)(15:MAX_LEN_FNAM),'(a)') ' '
#endif
         write(xx_genarr2d_weight(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
         genarr2dPrecond(iarr)  = 1. _d 0
         mult_genarr2d(iarr)    = 1. _d 0
         do jarr = 1, 5
           xx_genarr2d_bounds(jarr,iarr) = 0. _d 0
         enddo
         do jarr=1,maxCtrlProc
          xx_genarr2d_preproc(jarr,iarr)   = ' '
          xx_genarr2d_preproc_c(jarr,iarr) = ' '
          xx_genarr2d_preproc_i(jarr,iarr) = 0
          xx_genarr2d_preproc_r(jarr,iarr) = 0. _d 0
         enddo
        enddo
c
        do iarr = 1, maxCtrlArr3D
         write(xx_genarr3d_file(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
#ifdef ALLOW_OPENAD
         write(xx_genarr3d_file(iarr)(1:14),'(a,i2.2)')
     &        'xx_genarr3d_', iarr
         write(xx_genarr3d_file(iarr)(15:MAX_LEN_FNAM),'(a)') ' '
#endif
         write(xx_genarr3d_weight(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
         genarr3dPrecond(iarr)  = 1. _d 0
         mult_genarr3d(iarr)    = 1. _d 0
         do jarr = 1, 5
           xx_genarr3d_bounds(jarr,iarr) = 0. _d 0
         enddo
         do jarr=1,maxCtrlProc
          xx_genarr3d_preproc(jarr,iarr)   = ' '
          xx_genarr3d_preproc_c(jarr,iarr) = ' '
          xx_genarr3d_preproc_i(jarr,iarr) = 0
          xx_genarr3d_preproc_r(jarr,iarr) = 0. _d 0
         enddo
        enddo
c
        do iarr = 1, maxCtrlTim2D
         write(xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
#ifdef ALLOW_OPENAD
         write(xx_gentim2d_file(iarr)(1:14),'(a,i2.2)')
     &        'xx_gentim2d_', iarr
         write(xx_gentim2d_file(iarr)(15:MAX_LEN_FNAM),'(a)') ' '
#endif
         write(xx_gentim2d_weight(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
         xx_gentim2d_startdate1(iarr) = 0
         xx_gentim2d_startdate2(iarr) = 0
         xx_gentim2d_period(iarr)     = 0. _d 0
         xx_gentim2d_cumsum(iarr)     = .FALSE.
         xx_gentim2d_glosum(iarr)     = .FALSE.
         do jarr=1,maxCtrlProc
          xx_gentim2d_preproc(jarr,iarr)   = ' '
          xx_gentim2d_preproc_c(jarr,iarr) = ' '
          xx_gentim2d_preproc_i(jarr,iarr) = 0
          xx_gentim2d_preproc_r(jarr,iarr) = 0. _d 0
         enddo
         mult_gentim2d(iarr)          = 1. _d 0
         gentim2dPrecond(iarr)        = 1. _d 0
         do jarr = 1, 5
           xx_gentim2d_bounds(jarr,iarr) = 0. _d 0
         enddo
        enddo
#endif
c
        xx_hfluxstartdate1 =   0
        xx_hfluxstartdate2 =   0
        xx_hfluxperiod     =   0. _d 0
        xx_hflux_file      = 'xx_hfl'
        xx_hflux_remo_intercept =   0. _d 0
        xx_hflux_remo_slope =   0. _d 0
c
        xx_sfluxstartdate1 =   0
        xx_sfluxstartdate2 =   0
        xx_sfluxperiod     =   0. _d 0
        xx_sflux_file      = 'xx_sfl'
        xx_sflux_remo_intercept =   0. _d 0
        xx_sflux_remo_slope =   0. _d 0
c
        xx_tauustartdate1  =   0
        xx_tauustartdate2  =   0
        xx_tauuperiod      =   0. _d 0
        xx_tauu_file       = 'xx_tauu'
        xx_tauu_remo_intercept =   0. _d 0
        xx_tauu_remo_slope =   0. _d 0
c
        xx_tauvstartdate1  =   0
        xx_tauvstartdate2  =   0
        xx_tauvperiod      =   0. _d 0
        xx_tauv_file       = 'xx_tauv'
        xx_tauv_remo_intercept =   0. _d 0
        xx_tauv_remo_slope =   0. _d 0
c
        xx_atempstartdate1  =   0
        xx_atempstartdate2  =   0
        xx_atempperiod      =   0. _d 0
        xx_atemp_file       = 'xx_atemp'
        xx_atemp_mean_file  = 'xx_atemp_mean'
        xx_atemp_remo_intercept =   0. _d 0
        xx_atemp_remo_slope =   0. _d 0
c
        xx_aqhstartdate1    =   0
        xx_aqhstartdate2    =   0
        xx_aqhperiod        =   0. _d 0
        xx_aqh_file         = 'xx_aqh'
        xx_aqh_mean_file    = 'xx_aqh_mean'
        xx_aqh_remo_intercept =   0. _d 0
        xx_aqh_remo_slope =   0. _d 0
c
        xx_precipstartdate1 =   0
        xx_precipstartdate2 =   0
        xx_precipperiod     =   0. _d 0
        xx_precip_file      = 'xx_precip'
        xx_precip_mean_file = 'xx_precip_mean'
        xx_precip_remo_intercept =   0. _d 0
        xx_precip_remo_slope =   0. _d 0
c
        xx_swfluxstartdate1 =   0
        xx_swfluxstartdate2 =   0
        xx_swfluxperiod     =   0. _d 0
        xx_swflux_file      = 'xx_swflux'
        xx_swflux_remo_intercept =   0. _d 0
        xx_swflux_remo_slope =   0. _d 0
c
        xx_swdownstartdate1 =   0
        xx_swdownstartdate2 =   0
        xx_swdownperiod     =   0. _d 0
        xx_swdown_file      = 'xx_swdown'
        xx_swdown_mean_file = 'xx_swdown_mean'
        xx_swdown_remo_intercept =   0. _d 0
        xx_swdown_remo_slope =   0. _d 0
c
        xx_lwfluxstartdate1 =   0
        xx_lwfluxstartdate2 =   0
        xx_lwfluxperiod     =   0. _d 0
        xx_lwflux_file      = 'xx_lwflux'
        xx_lwflux_remo_intercept =   0. _d 0
        xx_lwflux_remo_slope =   0. _d 0
c
        xx_lwdownstartdate1 =   0
        xx_lwdownstartdate2 =   0
        xx_lwdownperiod     =   0. _d 0
        xx_lwdown_file      = 'xx_lwdown'
        xx_lwdown_remo_intercept =   0. _d 0
        xx_lwdown_remo_slope =   0. _d 0
c
        xx_evapstartdate1 =   0
        xx_evapstartdate2 =   0
        xx_evapperiod     =   0. _d 0
        xx_evap_file      = 'xx_evap'
        xx_evap_remo_intercept =   0. _d 0
        xx_evap_remo_slope =   0. _d 0
c
        xx_snowprecipstartdate1 =   0
        xx_snowprecipstartdate2 =   0
        xx_snowprecipperiod     =   0. _d 0
        xx_snowprecip_file      = 'xx_snowprecip'
        xx_snowprecip_remo_intercept =   0. _d 0
        xx_snowprecip_remo_slope =   0. _d 0
c
        xx_apressurestartdate1 =   0
        xx_apressurestartdate2 =   0
        xx_apressureperiod     =   0. _d 0
        xx_apressure_file      = 'xx_apressure'
        xx_apressure_remo_intercept =   0. _d 0
        xx_apressure_remo_slope =   0. _d 0
c
        xx_runoffstartdate1 =   0
        xx_runoffstartdate2 =   0
        xx_runoffperiod     =   0. _d 0
        xx_runoff_file      = 'xx_runoff'
        xx_runoff_remo_intercept =   0. _d 0
        xx_runoff_remo_slope =   0. _d 0
c
        xx_uwindstartdate1  =   0
        xx_uwindstartdate2  =   0
        xx_uwindperiod      =   0. _d 0
        xx_uwind_file       = 'xx_uwind'
        xx_uwind_mean_file  = 'xx_uwind_mean'
        xx_uwind_remo_intercept =   0. _d 0
        xx_uwind_remo_slope =   0. _d 0
c
        xx_vwindstartdate1  =   0
        xx_vwindstartdate2  =   0
        xx_vwindperiod      =   0. _d 0
        xx_vwind_file       = 'xx_vwind'
        xx_vwind_mean_file  = 'xx_vwind_mean'
        xx_vwind_remo_intercept =   0. _d 0
        xx_vwind_remo_slope =   0. _d 0
c
        xx_obcsnstartdate1  =   0
        xx_obcsnstartdate2  =   0
        xx_obcsnperiod      =   0. _d 0
        xx_obcsn_file       = 'xx_obcsn'
c
        xx_obcssstartdate1  =   0
        xx_obcssstartdate2  =   0
        xx_obcssperiod      =   0. _d 0
        xx_obcss_file       = 'xx_obcss'
c
        xx_obcswstartdate1  =   0
        xx_obcswstartdate2  =   0
        xx_obcswperiod      =   0. _d 0
        xx_obcsw_file       = 'xx_obcsw'
c
        xx_obcsestartdate1  =   0
        xx_obcsestartdate2  =   0
        xx_obcseperiod      =   0. _d 0
        xx_obcse_file       = 'xx_obcse'
c
        xx_sststartdate1    =   0
        xx_sststartdate2    =   0
        xx_sstperiod        =   0. _d 0
        xx_sst_file         = 'xx_sst'
        xx_sst_remo_intercept =   0. _d 0
        xx_sst_remo_slope =   0. _d 0
c
        xx_sssstartdate1    =   0
        xx_sssstartdate2    =   0
        xx_sssperiod        =   0. _d 0
        xx_sss_file         = 'xx_sss'
        xx_sss_remo_intercept =   0. _d 0
        xx_sss_remo_slope =   0. _d 0
c
        xx_shifwflxstartdate1  =   0
        xx_shifwflxstartdate2  =   0
        xx_shifwflxperiod      =   0. _d 0
        xx_shifwflx_file       = 'xx_shifwflx'
        xx_shifwflx_remo_intercept=0. _d 0
        xx_shifwflx_remo_slope =   0. _d 0
c
        xx_diffkr_file      = 'xx_diffkr'
        xx_kapgm_file       = 'xx_kapgm'
        xx_kapredi_file     = 'xx_kapredi'
        xx_tr1_file         = 'xx_ptr'
        xx_depth_file       = 'xx_depth'
        xx_efluxy_file      = 'xx_efluxy'
        xx_efluxp_file      = 'xx_efluxp'
        xx_bottomdrag_file  = 'xx_bottomdrag'
        xx_edtaux_file      = 'xx_edtaux'
        xx_edtauy_file      = 'xx_edtauy'
        xx_uvel_file        = 'xx_uvel'
        xx_vvel_file        = 'xx_vvel'
        xx_etan_file        = 'xx_etan'
        xx_siarea_file      = 'xx_siarea'
        xx_siheff_file      = 'xx_siheff'
        xx_sihsnow_file     = 'xx_sihsnow'
cHFLUXM_CONTROL
        xx_hfluxm_file      = 'xx_hfluxm'
cHFLUXM_CONTROL

#ifdef ALLOW_TANGENTLINEAR_RUN
        yadprefix           = 'g_'
        yadmark             = 'g_'
#else
        yadprefix           = 'ad'
        yadmark             = 'ad'
#endif
        yctrlid             = 'MIT_CE_000'
        yctrlposunpack      = '.opt'
        yctrlpospack        = '.opt'
        ctrlname            = 'ecco_ctrl'
        costname            = 'ecco_cost'
        scalname            = ' '
        maskname            = ' '
        metaname            = ' '

c--     Next, read the cost data file.
        WRITE(msgBuf,'(A)') 'CTRL_READPARMS: opening data.ctrl'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.ctrl', 'CTRL_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(unit = iUnit, nml = ctrl_nml)
        READ(unit = iUnit, nml = ctrl_packnames)
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
        READ(unit = iUnit, nml = ctrl_nml_genarr)
#endif

        WRITE(msgBuf,'(A)')
     &       'CTRL_READPARMS: finished reading data.ctrl'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

#ifdef SINGLE_DISK_IO
        CLOSE(iUnit)
#else
        CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

#ifdef ALLOW_AUTODIFF
c allow for switching off correl2d in adjoint
      useSmoothCorrel2DinFwdMode = ctrlSmoothCorrel2D
      useSmoothCorrel2DinAdMode =
     &         useSmoothCorrel2DinAdMode .AND. ctrlSmoothCorrel2D
#endif

      _END_MASTER( myThid )

      _BARRIER

      return
      end
