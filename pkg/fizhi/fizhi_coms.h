C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_coms.h,v 1.7 2004/10/14 22:11:42 molod Exp $
C $Name:  $


C The physics state uses the dynamics dimensions in the horizontal
C     and the land dimensions in the horizontal for turbulence variables
c
c Fizhi State Common - State variables on physics grid
c ----------------------------------------------------------------------
      common /physics_state/ uphy,vphy,thphy,sphy,
     .   ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke

      _RL uphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL vphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL thphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL sphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL ctmt(nchp,Nsx,Nsy), xxmt(nchp,Nsx,Nsy), yymt(nchp,Nsx,Nsy)
      _RL zetamt(nchp,Nsx,Nsy)
      _RL xlmt(nchp,Nrphys,Nsx,Nsy), khmt(nchp,Nrphys,Nsx,Nsy) 
      _RL tke(nchp,Nrphys,Nsx,Nsy)

c Fizhi Tendency Common - Changes in state variables due to physics
c ----------------------------------------------------------------------
      common /physics_tendency/ duphy,dvphy,dthphy,dsphy

      _RL duphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL dvphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL dthphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL dsphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)

c Fizhi Exports Common - Physics variables on other grids for export
c ----------------------------------------------------------------------
      common /physics_exports/ guphy,gvphy,gthphy,gsphy

      _RL guphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,Nsx,Nsy)
      _RL gvphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,Nsx,Nsy)
      _RL gthphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,Nsx,Nsy)
      _RL gsphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,Nsx,Nsy)

c Fizhi Saver Common - Internal fizhi variables to be written on pickup
c ----------------------------------------------------------------------
      common /physics_saver/ cldtot_lw,cldras_lw,cldlsp_lw,lwlz,
     .                       cldtot_sw,cldras_sw,cldlsp_sw,swlz,
     .                       qliqavesw,qliqavelw,fccavesw,fccavelw,
     .                       raincon,rainlsp,snowfall,
     .                       iras,nlwcld,nlwlz,nswcld,nswlz,
     .                       imstturbsw,imstturblw

      _RL cldtot_lw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL cldras_lw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL cldlsp_lw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL lwlz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL cldtot_sw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL cldras_sw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL cldlsp_sw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL swlz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL qliqavesw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL qliqavelw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL fccavesw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL fccavelw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL raincon(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nsx,Nsy)
      _RL rainlsp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nsx,Nsy)
      _RL snowfall(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nsx,Nsy)
      integer iras(Nsx,Nsy)
      integer nlwcld(Nsx,Nsy),nlwlz(Nsx,Nsy)
      integer nswcld(Nsx,Nsy),nswlz(Nsx,Nsy)
      integer imstturbsw(Nsx,Nsy),imstturblw(Nsx,Nsy)
