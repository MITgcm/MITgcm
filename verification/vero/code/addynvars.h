C--   copied stuff from _ftl.f file

      common /addynvars_r/ aduvel, advvel, adtheta, adsalt
     &                   , adgu, adgv, adgt, adgs
     &                   , adgunm1, adgvnm1, adgtnm1, adgsnm1
      _RL adgs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adgsnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adgt   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adgtnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adgu   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adgunm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adgv   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adgvnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adsalt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adtheta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL aduvel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL advvel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      common /adcg2d_e_wk_r/ adcg2d_b, adcg2d_x
      _RL adcg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adcg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      common /addynvars_cd/ aduveld, advveld, adcg2d_xnm1
     &                    , adunm1, advnm1, adgucd, adgvcd
      _RL adcg2d_xnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adgucd (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adgvcd (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL adunm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL aduveld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL advnm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL advveld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

