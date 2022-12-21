C--   These common blocks are extracted from the
C--   automatically created tangent linear code.
C--   You need to make sure that they are up-to-date
C--   (i.e. in right order), and customize them accordingly.
C--
C--   heimbach@mit.edu 11-Jan-2001

#ifdef ALLOW_AUTODIFF_MONITOR

      COMMON /adDYNVARS_R/
     &                     adEtaN,
     &                     adUvel, adVvel, adWvel,
     &                     adTheta, adSalt,
     &                     adGu, adGv,
#ifdef ALLOW_ADAMSBASHFORTH_3
     &                     adGuNm, adGvNm, adGtNm, adGsNm
#else
     &                     adGuNm1, adGvNm1, adGtNm1, adGsNm1
#endif
      _RL adEtaN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adGu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adGv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adSalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adTheta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adUvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adVvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adWvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_ADAMSBASHFORTH_3
      _RL adGtNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL adGsNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL adGuNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL adGvNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
#else
      _RL adGtNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adGsNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adGuNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adGvNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

      COMMON /adDYNVARS_R_2/
     &                     adEtaH
      _RL adEtaH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
C Special Care: more forward vars in FWD common block ; check TAF AD-code !
      COMMON /adDYNVARS_DIAG/
     &                     adRhoInSitu, adTotPhihyd
      _RL adRhoInSitu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adTotPhiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      COMMON /adDYNVARS_DIFFKR/
     &                       adDiffKr
      _RL  adDiffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_CD_CODE
      COMMON /adDYNVARS_CD/
     &                      adUvelD, adVvelD,
     &                      adEtaNm1,
     &                      adUnm1, adVnm1
      _RL adUvelD(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adVvelD(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adEtaNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adUnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adVnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

      COMMON /adFFIELDS_fu/ adFu
      COMMON /adFFIELDS_fv/ adFv
      COMMON /adFFIELDS_Qnet/ adQnet
      COMMON /adFFIELDS_Qsw/ adQsw
      COMMON /adFFIELDS_EmPmR/ adEmPmR
      COMMON /adFFIELDS_saltFlux/ adSaltFlux
      COMMON /adFFIELDS_SST/ adSST
      COMMON /adFFIELDS_SSS/ adSSS
      COMMON /adFFIELDS_lambdaThetaClimRelax/ adLambdaThetaClimRelax
      COMMON /adFFIELDS_lambdaSaltClimRelax/ adLambdaSaltClimRelax
      _RS  adFu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adFv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adQnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adQsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adEmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adSaltFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adSST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adSSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adLambdaThetaClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adLambdaSaltClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ATMOSPHERIC_LOADING
      COMMON /adFFIELDS_pload/ adPload
      COMMON /adFFIELDS_sIceLoad/ adSIceLoad
      _RS  adPload    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adSIceLoad (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
      _RS adEddyPsiX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS adEddyPsiY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /adFFIELDS_eddyPsi_RS/ adEddyPsiX, adEddyPsiY
#endif

#ifdef ALLOW_KAPGM_CONTROL
      COMMON /adGM_INP_K3D_GM/
     &                       adKapGM
      _RL  adKapGM (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
      COMMON /adGM_INP_K3D_REDI/
     &                       adKapRedi
      _RL  adKapRedi (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      COMMON /adCTRL_FIELDS_BOTTOMDRAG/
     &                adBottomDragFld
      _RL  adBottomDragFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_EXF

      _RL adustress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL advstress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_stress_r/ adustress, advstress

      _RL adwspeed(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_wspeed_r/ adwspeed

      _RL aduwind     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL advwind     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_atm_wind_r/ aduwind, advwind

      _RL adhflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adsflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_netflux_r/ adhflux, adsflux

# ifdef ALLOW_ATM_TEMP
      _RL adatemp     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adaqh       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adhs        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adhl        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adlwflux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adevap      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adprecip    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adsnowprecip(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_atm_temp_r/ adatemp, adaqh, adhs, adhl,
     & adlwflux, adevap, adprecip, adsnowprecip
# endif /* ALLOW_ATM_TEMP */
# if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      _RL adswflux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_swflux_r/ adswflux
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
      _RL adswdown    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adlwdown    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_rad_down_r/ adswdown, adlwdown
# endif

# ifdef ALLOW_RUNOFF
      _RL adrunoff    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adrunoff0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adrunoff1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /exfl_runoff_r_ad/ adrunoff, adrunoff0, adrunoff1
# endif

# ifdef ALLOW_CLIMSST_RELAXATION
      _RL adclimsst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_clim_sst_r/ adclimsst
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
      _RL adclimsss(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adexf_clim_sss_r/ adclimsss
# endif

#endif /* ALLOW_EXF */

#ifdef ALLOW_SEAICE
      _RL adarea  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adheff  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adhsnow (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL aduice  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL advice  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adSEAICE_DYNVARS_1/
     &     adarea, adheff, adhsnow, aduice, advice
# ifdef SEAICE_VARIABLE_SALINITY
      _RL adhsalt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adSEAICE_SALINITY_R/ adhsalt
# endif
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_GGL90
      _RL adGGL90TKE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL adGGL90viscArU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
c     _RL adGGL90viscArV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
c     _RL adGGL90diffKr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      COMMON /adGGL90_FIELDS/ adGGL90TKE
c    &      , adGGL90viscArU, adGGL90viscArV, adGGL90diffKr
#endif

#ifdef ALLOW_DEPTH_CONTROL
C Special Care: more forward vars in FWD common block ; check TAF AD-code !
      _RS adhfacc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS adhfacs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS adhfacw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS adrecip_hfacc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS adrecip_hfacs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS adrecip_hfacw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS adr_low     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS adrecip_rcol(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adGRID_VAR_RS/
     &     adhfacc, adhfacw, adhfacs,
     &     adrecip_hfacc, adrecip_hfacw, adrecip_hfacs,
     &     adr_low, adrecip_rcol
#endif /* ALLOW_DEPTH_CONTROL */

#ifdef ALLOW_SHELFICE
C Special Care: more forward vars in FWD common block ; check TAF AD-code !
      _RL adshelficeforcings(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshelficeforcingt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshelficemass    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshicdragfld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshidragquadfld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshitranscoeffs  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshitranscoefft  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /adSHELFICE_FIELDS_RL/ adshelficemass,
     &  adshelficeforcingt, adshelficeforcings, adshitranscoefft,
     &  adshitranscoeffs, adshicdragfld, adshidragquadfld
#endif

#endif /* ALLOW_AUTODIFF_MONITOR */
