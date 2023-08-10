C--   These common blocks are extracted from the
C--   automatically created tangent linear code.
C--   You need to make sure that they are up-to-date
C--   (i.e. in right order), and customize them
C--   accordingly.
C--
C--   heimbach@mit.edu 11-Jan-2001

#ifdef ALLOW_AUTODIFF_MONITOR

      COMMON /g_DYNVARS_R/
     &                     g_EtaN,
     &                     g_Uvel, g_Vvel, g_Wvel,
     &                     g_Theta, g_Salt,
     &                     g_Gu, g_Gv, g_Gt, g_Gs,
#ifdef ALLOW_ADAMSBASHFORTH_3
     &                     g_GuNm, g_GvNm, g_GtNm, g_GsNm
#else
     &                     g_GuNm1, g_GvNm1, g_GtNm1, g_GsNm1
#endif
      _RL g_Etan(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_Gs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Gt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Gu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Gv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Salt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Uvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Vvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Wvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_ADAMSBASHFORTH_3
      _RL g_GtNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL g_GsNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL g_GuNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL g_GvNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
#else
      _RL g_GtNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_GsNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_GuNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_GvNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

      COMMON /g_DYNVARS_R_2/
     &                     g_EtaH
      _RL g_EtaH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
C Special Care: more forward vars in FWD common block ; check TAF TL-code !
      COMMON /g_DYNVARS_DIAG/
     &                     g_RhoInSitu, g_TotPhiHyd
      _RL g_RhoInSitu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_TotPhiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      COMMON /g_DYNVARS_DIFFKR/
     &                       g_diffKr
      _RL  g_diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_CD_CODE
      COMMON /g_DYNVARS_CD/
     &                      g_UvelD, g_VvelD,
     &                      g_EtaNm1,
     &                      g_Unm1, g_Vnm1
      _RL g_UvelD(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_VvelD(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_EtaNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_Unm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL g_Vnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

      COMMON /g_FFIELDS_fu/ g_fu
      COMMON /g_FFIELDS_fv/ g_fv
      COMMON /g_FFIELDS_Qnet/ g_Qnet
      COMMON /g_FFIELDS_Qsw/ g_Qsw
      COMMON /g_FFIELDS_EmPmR/ g_EmPmR
      COMMON /g_FFIELDS_saltFlux/ g_saltFlux
      COMMON /g_FFIELDS_SST/ g_SST
      COMMON /g_FFIELDS_SSS/ g_SSS
      COMMON /g_FFIELDS_lambdaThetaClimRelax/ g_lambdaThetaClimRelax
      COMMON /g_FFIELDS_lambdaSaltClimRelax/ g_lambdaSaltClimRelax
      _RS  g_fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_Qnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_Qsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_EmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_saltFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_SST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_SSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_lambdaThetaClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_lambdaSaltClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ATMOSPHERIC_LOADING
      COMMON /g_FFIELDS_pload/ g_pload
      COMMON /g_FFIELDS_sIceLoad/ g_sIceLoad
      _RS  g_pload    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_sIceLoad (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_KAPGM_CONTROL
      COMMON /g_GM_INP_K3D_GM/
     &                       g_kapgm
      _RL  g_kapgm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
      COMMON /g_GM_INP_K3D_REDI/
     &                       g_kapredi
      _RL  g_kapredi (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      COMMON /g_CTRL_FIELDS_BOTTOMDRAG/
     &                       g_bottomdragfld
      _RL  g_bottomdragfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_EXF
      _RL g_ustress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_vstress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_exf_stress_r/ g_ustress, g_vstress
      _RL g_uwind     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_vwind     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_exf_atm_wind_r/ g_uwind, g_vwind
      _RL g_hflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_sflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_exf_netflux_r/ g_hflux, g_sflux
# ifdef ALLOW_ATM_TEMP
      _RL g_atemp     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_aqh       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_hs        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_hl        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_lwflux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_evap      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_precip    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_snowprecip(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_exf_atm_temp_r/ g_atemp, g_aqh, g_hs, g_hl,
     &  g_lwflux, g_evap, g_precip, g_snowprecip
# endif /* ALLOW_ATM_TEMP */
# if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      _RL g_swflux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_exf_swflux_r/ g_swflux
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
      _RL g_swdown    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_lwdown    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_exf_rad_down_r/ g_swdown, g_lwdown
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
      _RL g_climsst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_exf_clim_sst_r/ g_climsst
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
      _RL g_climsss(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_exf_clim_sss_r/ g_climsss
# endif
#endif /* ALLOW_EXF */

#ifdef ALLOW_SEAICE
      _RL g_area  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_heff  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_hsnow (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_uice  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL g_vice  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /g_SEAICE_DYNVARS_1/
     &     g_area, g_heff, g_hsnow, g_uice, g_vice
#endif

#ifdef ALLOW_DEPTH_CONTROL
      _RS g_hfacc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_hfacs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_hfacw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_recip_hfacc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_recip_hfacs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_recip_hfacw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_r_low     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS g_recip_rcol(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /g_grid_var_rs/
     &     g_hfacc, g_hfacw, g_hfacs,
     &     g_recip_hfacc, g_recip_hfacw, g_recip_hfacs,
     &     g_r_low, g_recip_rcol
#endif /* ALLOW_DEPTH_CONTROL */

#endif /* ALLOW_AUTODIFF_MONITOR */
