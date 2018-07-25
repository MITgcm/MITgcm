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
      _RL adetan(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adgu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgv(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adsalt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adtheta(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL aduvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL advvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adwvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#ifdef ALLOW_ADAMSBASHFORTH_3
      _RL adgtnm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,2)
      _RL adgsnm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,2)
      _RL adgunm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,2)
      _RL adgvnm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,2)
#else
      _RL adgtnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgsnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgunm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgvnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef USE_OLD_EXTERNAL_FORCING
c     common /addynvars_old/
c    &                   adgt,   adgs
c     _RL adgs(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
c     _RL adgt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

      common /addynvars_r_2/
     &                     adetah
      _RL adetah(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
      common /addynvars_diag/
     &                     adtotphihyd, adrhoinsitu
      _RL adrhoinsitu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adtotphihyd(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_CD_CODE
      common /addynvars_cd/
     &                      aduveld, advveld,
     &                       adetanm1,
     &                      adunm1, advnm1
      _RL aduveld(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL advveld(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adetanm1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adunm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL advnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
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

      _RL adhflux(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adsflux(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_hsflux_r/ adhflux, adsflux

      _RL adustress(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL advstress(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_stress_r/ adustress, advstress

      _RL adwspeed(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_wspeed_r/ adwspeed

# ifdef ALLOW_RUNOFF
      _RL adrunoff    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adrunoff0   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adrunoff1   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_runoff_r_ad/ adrunoff, adrunoff0, adrunoff1
# endif

# ifdef ALLOW_ATM_TEMP
      _RL adatemp     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adaqh       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adhs        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adhl        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adlwflux    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adevap      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adprecip    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adsnowprecip(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_atm_temp_r/ adatemp, adaqh, adhs, adhl,
     & adlwflux, adevap, adprecip, adsnowprecip
#  ifdef SHORTWAVE_HEATING
      _RL adswflux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /adexf_swflux_r/ adswflux
#  endif
# endif /* ALLOW_ATM_TEMP */

      _RL aduwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL advwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_atm_wind_r/ aduwind, advwind

# ifdef ALLOW_DOWNWARD_RADIATION
      _RL adswdown    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adlwdown    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_rad_down_r/ adswdown, adlwdown
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
      _RL adclimsst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_clim_sst_r/ adclimsst
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
      _RL adclimsss(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_clim_sss_r/ adclimsss
# endif

#endif /* ALLOW_EXF */

#ifdef ALLOW_SEAICE
      _RL adarea  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adheff  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adhsnow (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL aduice  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL advice  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adseaice_dynvars_1/
     &     adarea, adheff, adhsnow, aduice, advice
# ifdef SEAICE_VARIABLE_SALINITY
      _RL adhsalt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /seaice_salinity_r/ adhsalt
# endif
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_GGL90
      _RL adggl90tke     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adggl90diffkr  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      common /adggl90_fields/ adggl90tke, adggl90diffkr
#endif

#ifdef ALLOW_DEPTH_CONTROL
      _RL adr_low_control(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adhfacc(1-olx:snx+olx,1-oly:sny+oly,1:nr,nsx,nsy)
      _RL adhfacs(1-olx:snx+olx,1-oly:sny+oly,1:nr,nsx,nsy)
      _RL adhfacw(1-olx:snx+olx,1-oly:sny+oly,1:nr,nsx,nsy)
      _RL adrecip_hfacc(1-olx:snx+olx,1-oly:sny+oly,1:nr,nsx,nsy)
      _RL adrecip_hfacs(1-olx:snx+olx,1-oly:sny+oly,1:nr,nsx,nsy)
      _RL adrecip_hfacw(1-olx:snx+olx,1-oly:sny+oly,1:nr,nsx,nsy)
      common /adgrid_r/ adr_low_control,
     &                  adrecip_hfacc, adrecip_hfacw, adrecip_hfacs
      common /adgrid_r_c/ adhfacc
      common /adgrid_r_s/ adhfacs
      common /adgrid_r_w/ adhfacw
#endif /* ALLOW_DEPTH_CONTROL */

#endif /* ALLOW_AUTODIFF_MONITOR */
