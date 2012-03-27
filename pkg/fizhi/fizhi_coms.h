C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_coms.h,v 1.8 2012/03/27 15:48:27 jmc Exp $
C $Name:  $

C The physics state uses the dynamics dimensions in the horizontal
C     and the land dimensions in the horizontal for turbulence variables
C
C Fizhi State Common - State variables on physics grid
C ----------------------------------------------------------------------
      COMMON /physics_state/ uphy,vphy,thphy,sphy,
     &   ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke

      _RL uphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL vphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL thphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL sphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL ctmt(nchp,nSx,nSy), xxmt(nchp,nSx,nSy), yymt(nchp,nSx,nSy)
      _RL zetamt(nchp,nSx,nSy)
      _RL xlmt(nchp,Nrphys,nSx,nSy), khmt(nchp,Nrphys,nSx,nSy)
      _RL tke(nchp,Nrphys,nSx,nSy)

C Fizhi Tendency Common - Changes in state variables due to physics
C ----------------------------------------------------------------------
      COMMON /physics_tendency/ duphy,dvphy,dthphy,dsphy

      _RL duphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL dvphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL dthphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL dsphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)

C Fizhi Exports Common - Physics variables on other grids for export
C ----------------------------------------------------------------------
      COMMON /physics_exports/ guphy,gvphy,gthphy,gsphy

      _RL guphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gvphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gthphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gsphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C Fizhi Saver Common - Internal fizhi variables to be written on pickup
C ----------------------------------------------------------------------
      COMMON /physics_saver/ cldtot_lw,cldras_lw,cldlsp_lw,lwlz,
     &                       cldtot_sw,cldras_sw,cldlsp_sw,swlz,
     &                       qliqavesw,qliqavelw,fccavesw,fccavelw,
     &                       raincon,rainlsp,snowfall,
     &                       iras,nlwcld,nlwlz,nswcld,nswlz,
     &                       imstturbsw,imstturblw

      _RL cldtot_lw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL cldras_lw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL cldlsp_lw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL lwlz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL cldtot_sw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL cldras_sw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL cldlsp_sw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL swlz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL qliqavesw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL qliqavelw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL fccavesw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL fccavelw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL raincon(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rainlsp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL snowfall(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iras(nSx,nSy)
      INTEGER nlwcld(nSx,nSy),nlwlz(nSx,nSy)
      INTEGER nswcld(nSx,nSy),nswlz(nSx,nSy)
      INTEGER imstturbsw(nSx,nSy),imstturblw(nSx,nSy)

C ----------------------------------------------------------------------
C     turbStart :: true when doing a cold-start for turbulence
      COMMON /FIZHI_START/ turbStart
      LOGICAL turbStart(nSx,nSy)
