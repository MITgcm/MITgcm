C--   These common blocks are extracted from the
C--   automatically created tangent linear code.
C--   You need to make sure that they are up-to-date
C--   (i.e. in right order), and customize them
C--   accordingly.
C--
C--   heimbach@mit.edu 11-Jan-2001

      common /addynvars_r/
     &                     adetan,
     &                     aduvel, advvel, adwvel,
     &                     adtheta, adsalt,
     &                     adgu, adgv, adgt, adgs,
     &                     adgunm1, adgvnm1, adgtnm1, adgsnm1
      _RL adetan(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adgs(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgsnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgtnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgunm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgv(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgvnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adsalt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adtheta(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL aduvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL advvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adwvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      common /addynvars_r_2/
     &                     adetah
      _RL adetah(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      COMMON /adffields_fu/ adfu
      COMMON /adffields_fv/ adfv
      COMMON /adffields_Qnet/ adQnet
      COMMON /adffields_Qsw/ adQsw
      COMMON /adffields_dQdT/ addQdT
      COMMON /adffields_EmPmR/ adEmPmR
      COMMON /adffields_saltFlux/ adsaltFlux
      COMMON /adffields_SST/ adSST
      COMMON /adffields_SSS/ adSSS
      COMMON /adffields_lambdaThetaClimRelax/ adlambdaThetaClimRelax
      COMMON /adffields_lambdaSaltClimRelax/ adlambdaSaltClimRelax
#ifdef ATMOSPHERIC_LOADING
      COMMON /adffields_pload/ adpload
      COMMON /adffields_sIceLoad/ adsIceLoad
#endif

      _RS  adfu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adfv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adQnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adQsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  addQdT     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adEmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adsaltFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adSST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adSSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adlambdaThetaClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adlambdaSaltClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ATMOSPHERIC_LOADING
      _RS  adpload    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  adsIceLoad (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      COMMON /ADDYNVARS_DIFFKR/
     &                       addiffKr
      _RL  addiffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_KAPGM_CONTROL
      COMMON /ADDYNVARS_KAPGM/
     &                       adkapgm
      _RL  adkapgm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
