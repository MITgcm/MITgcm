C--   copied stuff from _ad.f file

      common /addynvars_r/ 
     &                     adetan, adetah, 
     &                     aduvel, advvel, adwvel, 
     &                     adtheta, adsalt, 
     &                     adgu, adgv, adgt, adgs, 
     &                     adgunm1, adgvnm1, adgtnm1, adgsnm1
      _RL adetan(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adetah(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
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

      common /adffields/
     &                   adfu, adfv, adqnet, adempmr, adsst, adsss
      _RL adfu(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adfv(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adqnet(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adempmr(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adsst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adsss(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#ifdef ALLOW_PASSIVE_TRACER
      common /adtr1_r/ 
     &                 adtr1, adgtr1, adgtr1nm1
      _RL adgtr1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgtr1nm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adtr1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      common /addynvars_diffkr/ 
     &                          addiffkr
      _RL addiffkr(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_KAPGM_CONTROL
      common /addynvars_kapgm/ 
     &                          adkapgm
      _RL adkapgm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif



