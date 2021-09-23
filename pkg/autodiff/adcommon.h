C--   These common blocks are extracted from the
C--   automatically created tangent linear code.
C--   You need to make sure that they are up-to-date
C--   (i.e. in right order), and customize them accordingly.
C--
C--   heimbach@mit.edu 11-Jan-2001

#ifdef ALLOW_AUTODIFF_MONITOR

      common /addynvars_r/
     &                     adetan,
     &                     aduvel, advvel, adwvel,
     &                     adtheta, adsalt,
     &                     adgu, adgv,
#ifdef ALLOW_ADAMSBASHFORTH_3
     &                     adgunm, adgvnm, adgtnm, adgsnm
#else
     &                     adgunm1, adgvnm1, adgtnm1, adgsnm1
#endif
      _RL adetan(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adgu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adgv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adsalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adtheta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL aduvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL advvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adwvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
#ifdef ALLOW_ADAMSBASHFORTH_3
      _RL adgtnm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy,2)
      _RL adgsnm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy,2)
      _RL adgunm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy,2)
      _RL adgvnm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy,2)
#else
      _RL adgtnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adgsnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adgunm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adgvnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
#endif

#ifdef USE_OLD_EXTERNAL_FORCING
c     common /addynvars_old/
c    &                   adgt,   adgs
c     _RL adgs(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
c     _RL adgt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

      common /addynvars_r_2/
     &                     adetah
      _RL adetah(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
      common /addynvars_diag/
     &                     adtotphihyd, adrhoinsitu
      _RL adrhoinsitu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adtotphihyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
#endif

#ifdef ALLOW_CD_CODE
      common /addynvars_cd/
     &                      aduveld, advveld,
     &                       adetanm1,
     &                      adunm1, advnm1
      _RL aduveld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL advveld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adetanm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adunm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL advnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
#endif

      COMMON /adffields_fu/ adfu
      COMMON /adffields_fv/ adfv
      COMMON /adffields_Qnet/ adQnet
      COMMON /adffields_Qsw/ adQsw
      COMMON /adffields_EmPmR/ adEmPmR
      COMMON /adffields_saltFlux/ adsaltFlux
      COMMON /adffields_SST/ adSST
      COMMON /adffields_SSS/ adSSS
      COMMON /adffields_lambdaThetaClimRelax/ adlambdaThetaClimRelax
      COMMON /adffields_lambdaSaltClimRelax/ adlambdaSaltClimRelax
      _RS  adfu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adfv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adQnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adQsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adEmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adsaltFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adSST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adSSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adlambdaThetaClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adlambdaSaltClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ATMOSPHERIC_LOADING
      COMMON /adffields_pload/ adpload
      COMMON /adffields_sIceLoad/ adsIceLoad
      _RS  adpload    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adsIceLoad (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      COMMON /ADDYNVARS_DIFFKR/
     &                       addiffKr
      _RL  addiffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_KAPGM_CONTROL
      COMMON /ADCTRL_FIELDS_KAPGM/
     &                       adkapgm
      _RL  adkapgm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
      COMMON /ADCTRL_FIELDS_KAPREDI/
     &                       adkapredi
      _RL  adkapredi (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      COMMON /ADCTRL_FIELDS_BOTTOMDRAG/
     &                adbottomdragfld
      _RL  adbottomdragfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
      _RS adEddyPsiX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS adEddyPsiY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      common /adeddypsiffields/ adeddypsix, adeddypsiy
#endif

#ifdef ALLOW_EXF

      _RL adhflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adsflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_hsflux_r/ adhflux, adsflux

      _RL adustress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL advstress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_stress_r/ adustress, advstress

      _RL adwspeed(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_wspeed_r/ adwspeed

# ifdef ALLOW_RUNOFF
      _RL adrunoff    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adrunoff0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adrunoff1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /exfl_runoff_r_ad/ adrunoff, adrunoff0, adrunoff1
# endif

# ifdef ALLOW_ATM_TEMP
      _RL adatemp     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adaqh       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adhs        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adhl        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adlwflux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adevap      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adprecip    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adsnowprecip(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_atm_temp_r/ adatemp, adaqh, adhs, adhl,
     & adlwflux, adevap, adprecip, adsnowprecip
#  ifdef SHORTWAVE_HEATING
      _RL adswflux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_swflux_r/ adswflux
#  endif
# endif /* ALLOW_ATM_TEMP */

      _RL aduwind     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL advwind     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_atm_wind_r/ aduwind, advwind

# ifdef ALLOW_DOWNWARD_RADIATION
      _RL adswdown    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adlwdown    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_rad_down_r/ adswdown, adlwdown
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
      _RL adclimsst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_clim_sst_r/ adclimsst
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
      _RL adclimsss(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_clim_sss_r/ adclimsss
# endif

#endif /* ALLOW_EXF */

#ifdef ALLOW_SEAICE
      _RL adarea  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adheff  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adhsnow (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL aduice  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL advice  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adseaice_dynvars_1/
     &     adarea, adheff, adhsnow, aduice, advice
# ifdef SEAICE_VARIABLE_SALINITY
      _RL adhsalt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /seaice_salinity_r/ adhsalt
# endif
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_GGL90
      _RL adggl90tke     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL adggl90diffkr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      common /adggl90_fields/ adggl90tke, adggl90diffkr
#endif

#ifdef ALLOW_DEPTH_CONTROL
      _RS adr_low_control(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS adhfacc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS adhfacs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS adhfacw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS adrecip_rcol(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS adrecip_hfacc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS adrecip_hfacs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS adrecip_hfacw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      common /adgrid_rs/
     &     adr_low_control, adhfacc, adhfacw, adhfacs,
     &     adrecip_rcol, adrecip_hfacc, adrecip_hfacw, adrecip_hfacs
#endif /* ALLOW_DEPTH_CONTROL */

#ifdef ALLOW_SHELFICE
      _RL adshelficeforcings(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshelficeforcingt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshelficemass    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshicdragfld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshidragquadfld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshitranscoeffs  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adshitranscoefft  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      common /adshelfice_fields_rl/ adshelficemass,
     &  adshelficeforcingt, adshelficeforcings, adshitranscoefft,
     &  adshitranscoeffs, adshicdragfld, adshidragquadfld
#endif

#endif /* ALLOW_AUTODIFF_MONITOR */
