C--   copied stuff from _ftl.f file

      common /addynvars_r/ 
     &                     adetan, aduvel, advvel, adwvel, 
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

      common /addynvars_cd/ 
     &                      aduveld, advveld, adetanm1, 
     &                      adunm1, advnm1, 
     &                      adgucd, adgvcd
      _RL adetanm1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL adgucd(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adgvcd(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL adunm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL aduveld(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL advnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL advveld(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

