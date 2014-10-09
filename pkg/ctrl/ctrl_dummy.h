C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_dummy.h,v 1.34 2014/10/09 00:49:26 gforget Exp $
C $Name:  $

c     ==================================================================
c     HEADER CONTROL DUMMIES
c     ==================================================================
c
c     o Control dummy variables of the ECCO state estimation tool.
c
c     ==================================================================
c     HEADER CONTROL DUMMIES
c     ==================================================================
      common /ctrl_dummy/
     &                    xx_theta_dummy
     &                  , xx_salt_dummy
     &                  , xx_hflux_dummy
     &                  , xx_sflux_dummy
     &                  , xx_tauu_dummy
     &                  , xx_tauv_dummy
     &                  , xx_atemp_dummy
     &                  , xx_aqh_dummy
     &                  , xx_precip_dummy
     &                  , xx_swflux_dummy
     &                  , xx_swdown_dummy
     &                  , xx_snowprecip_dummy
     &                  , xx_lwflux_dummy
     &                  , xx_lwdown_dummy
     &                  , xx_evap_dummy
     &                  , xx_apressure_dummy
     &                  , xx_runoff_dummy
     &                  , xx_uwind_dummy
     &                  , xx_vwind_dummy
     &                  , xx_diffkr_dummy
     &                  , xx_kapgm_dummy
     &                  , xx_kapredi_dummy
     &                  , xx_tr1_dummy
     &                  , xx_sst_dummy
     &                  , xx_sss_dummy
     &                  , xx_depth_dummy
     &                  , xx_efluxy_dummy
     &                  , xx_efluxp_dummy
     &                  , xx_bottomdrag_dummy
     &                  , xx_edtaux_dummy
     &                  , xx_edtauy_dummy
     &                  , xx_uvel_dummy
     &                  , xx_vvel_dummy
     &                  , xx_etan_dummy
     &                  , xx_siarea_dummy
     &                  , xx_siheff_dummy
     &                  , xx_sihsnow_dummy
     &                  , xx_relaxsst_dummy
     &                  , xx_relaxsss_dummy
     &                  , xx_tbar_mean_dummy
     &                  , xx_tbar_daily_mean_dummy
     &                  , xx_sbar_mean_dummy
     &                  , xx_sbar_daily_mean_dummy
     &                  , xx_ubar_mean_dummy
     &                  , xx_vbar_mean_dummy
     &                  , xx_wbar_mean_dummy
     &                  , xx_psbar_mean_dummy
     &                  , xx_bpbar_mean_dummy
     &                  , xx_taux_mean_dummy
     &                  , xx_tauy_mean_dummy
     &                  , xx_hflux_mean_dummy
     &                  , xx_sflux_mean_dummy
     &                  , xx_sstbar_mean_dummy
     &                  , xx_sssbar_mean_dummy
     &                  , xx_atmfwbar_mean_dummy
     &                  , xx_atemp_mean_dummy
     &                  , xx_aqh_mean_dummy
     &                  , xx_precip_mean_dummy
     &                  , xx_swflux_mean_dummy
     &                  , xx_swdown_mean_dummy
     &                  , xx_snowprecip_mean_dummy
     &                  , xx_lwflux_mean_dummy
     &                  , xx_lwdown_mean_dummy
     &                  , xx_evap_mean_dummy
     &                  , xx_apressure_mean_dummy
     &                  , xx_runoff_mean_dummy
     &                  , xx_uwind_mean_dummy
     &                  , xx_vwind_mean_dummy
     &                  , xx_theta_ini_fin_dummy
     &                  , xx_salt_ini_fin_dummy
     &                  , xx_smrareabar_mean_dummy
     &                  , xx_smrsstbar_mean_dummy
     &                  , xx_smrsssbar_mean_dummy
     &                  , xx_iestaubar_mean_dummy
     &                  , xx_gen2d_dummy
     &                  , xx_gen3d_dummy
cHFLUXM_CONTROL
     &                  , xx_hfluxm_dummy
cHFLUXM_CONTROL
     &                  , xx_shifwflx_dummy

      _RL xx_theta_dummy
      _RL xx_salt_dummy
      _RL xx_hflux_dummy
      _RL xx_sflux_dummy
      _RL xx_tauu_dummy
      _RL xx_tauv_dummy
      _RL xx_atemp_dummy
      _RL xx_aqh_dummy
      _RL xx_precip_dummy
      _RL xx_swflux_dummy
      _RL xx_swdown_dummy
      _RL xx_snowprecip_dummy
      _RL xx_lwflux_dummy
      _RL xx_lwdown_dummy
      _RL xx_evap_dummy
      _RL xx_apressure_dummy
      _RL xx_runoff_dummy
      _RL xx_uwind_dummy
      _RL xx_vwind_dummy
      _RL xx_diffkr_dummy
      _RL xx_kapgm_dummy
      _RL xx_kapredi_dummy
      _RL xx_tr1_dummy
      _RL xx_sst_dummy
      _RL xx_sss_dummy
      _RL xx_depth_dummy
      _RL xx_efluxy_dummy
      _RL xx_efluxp_dummy
      _RL xx_bottomdrag_dummy
      _RL xx_edtaux_dummy
      _RL xx_edtauy_dummy
      _RL xx_uvel_dummy
      _RL xx_vvel_dummy
      _RL xx_etan_dummy
      _RL xx_siarea_dummy
      _RL xx_siheff_dummy
      _RL xx_sihsnow_dummy
      _RL xx_relaxsst_dummy
      _RL xx_relaxsss_dummy
      _RL xx_gen2d_dummy
      _RL xx_gen3d_dummy
c
      _RL xx_tbar_mean_dummy
      _RL xx_tbar_daily_mean_dummy
      _RL xx_sbar_mean_dummy
      _RL xx_sbar_daily_mean_dummy
      _RL xx_ubar_mean_dummy
      _RL xx_vbar_mean_dummy
      _RL xx_wbar_mean_dummy
      _RL xx_psbar_mean_dummy
      _RL xx_bpbar_mean_dummy
      _RL xx_hflux_mean_dummy
      _RL xx_sflux_mean_dummy
      _RL xx_sstbar_mean_dummy
      _RL xx_sssbar_mean_dummy
      _RL xx_atmfwbar_mean_dummy
      _RL xx_taux_mean_dummy
      _RL xx_tauy_mean_dummy
      _RL xx_atemp_mean_dummy
      _RL xx_aqh_mean_dummy
      _RL xx_precip_mean_dummy
      _RL xx_swflux_mean_dummy
      _RL xx_swdown_mean_dummy
      _RL xx_snowprecip_mean_dummy
      _RL xx_lwflux_mean_dummy
      _RL xx_lwdown_mean_dummy
      _RL xx_evap_mean_dummy
      _RL xx_apressure_mean_dummy
      _RL xx_runoff_mean_dummy
      _RL xx_uwind_mean_dummy
      _RL xx_vwind_mean_dummy
      _RL xx_theta_ini_fin_dummy
      _RL xx_salt_ini_fin_dummy
      _RL xx_smrareabar_mean_dummy
      _RL xx_smrsstbar_mean_dummy
      _RL xx_smrsssbar_mean_dummy
      _RL xx_iestaubar_mean_dummy
cHFLUXM_CONTROL
      _RL xx_hfluxm_dummy
cHFLUXM_CONTROL
      _RL xx_shifwflx_dummy

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
C--   Parameters maxCtrlArr2D, maxCtrlArr3D, maxCtrlTim2D are set in CTRL_SIZE.h
#else
      INTEGER maxCtrlArr2D, maxCtrlArr3D, maxCtrlTim2D
      PARAMETER(maxCtrlArr2D=1,maxCtrlArr3D=1,maxCtrlTim2D=1)
#endif
      common /ctrl_dummy_arr/
     &    xx_genarr2d_dummy
     &  , xx_genarr3d_dummy
     &  , xx_gentim2d_dummy
      _RL xx_genarr2d_dummy(maxCtrlArr2D)
      _RL xx_genarr3d_dummy(maxCtrlArr3D)
      _RL xx_gentim2d_dummy(maxCtrlTim2D)

c     ==================================================================
c     END OF HEADER CONTROL DUMMIES
c     ==================================================================


