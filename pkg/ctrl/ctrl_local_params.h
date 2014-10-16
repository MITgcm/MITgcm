C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_local_params.h,v 1.1 2014/10/16 19:52:27 gforget Exp $
C $Name:  $

c     ==================================================================
c     HEADER CTRL_legacy
c     ==================================================================
c

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

c     ==================================================================
c     END OF HEADER CONTROLVARS ctrl.h
c     ==================================================================

