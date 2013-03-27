C $Header: /u/gcmpack/MITgcm/pkg/ctrl/CTRL_GENARR.h,v 1.8 2013/03/27 00:10:26 gforget Exp $
C $Name:  $

C     ==================================================================
C     CTRL_GENARR.h
C     ==================================================================

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))

      COMMON /CONTROLFILES_CARR/
     &     xx_genarr2d_file,
     &     xx_genarr3d_file,
     &     xx_genarr2d_weight,
     &     xx_genarr3d_weight
      CHARACTER*(MAX_LEN_FNAM) xx_genarr2d_file(maxCtrlArr2D)
      CHARACTER*(MAX_LEN_FNAM) xx_genarr3d_file(maxCtrlArr3D)
      CHARACTER*(MAX_LEN_FNAM) xx_genarr2d_weight(maxCtrlArr2D)
      CHARACTER*(MAX_LEN_FNAM) xx_genarr3d_weight(maxCtrlArr3D)

      COMMON /CONTROLFILES_RARR/
     &     genarr2dPrecond
      _RL genarr2dPrecond(maxCtrlArr2D)

      COMMON /CONTROLFILES_CTIM/
     &     xx_gentim2d_file,
     &     xx_gentim2d_weight
      CHARACTER*(MAX_LEN_FNAM) xx_gentim2d_file(maxCtrlTim2D)
      CHARACTER*(MAX_LEN_FNAM) xx_gentim2d_weight(maxCtrlTim2D)

      COMMON /CONTROLFILES_ITIM/
     &     xx_gentim2d_startdate1,
     &     xx_gentim2d_startdate2,
     &     xx_gentim2d_startdate
      INTEGER xx_gentim2d_startdate1(maxCtrlTim2D)
      INTEGER xx_gentim2d_startdate2(maxCtrlTim2D)
      INTEGER xx_gentim2d_startdate(4,maxCtrlTim2D)

      COMMON /CONTROLFILES_RTIM/
     &     xx_gentim2d_period
     &   , gentim2dPrecond
      _RL xx_gentim2d_period(maxCtrlTim2D)
      _RL gentim2dPrecond(maxCtrlTim2D)

      COMMON /CONTROLFILES_LTIM/
     &     xx_gentim2d_cumsum, xx_gentim2d_glosum
      LOGICAL xx_gentim2d_cumsum(maxCtrlTim2D)
      LOGICAL xx_gentim2d_glosum(maxCtrlTim2D)

#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      common /controlaux_gentim2d_r/
     &                      xx_gentim2d0,
     &                      xx_gentim2d1,
     &                      xx_gentim2d,
     &                      wgentim2d
      _RL 
     & xx_gentim2d0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
      _RL 
     & xx_gentim2d1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
      _RL
     & xx_gentim2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
      _RL
     & wgentim2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
#endif

      common /controlaux_gencost_r/
     &     objf_gentim2d,  objf_gentim2dm,  objf_gentim2dsmoo,
     &     num_gentim2d,   num_gentim2dm,   mult_gentim2d
      _RL  objf_gentim2d(nsx,nsy,maxCtrlTim2D)
      _RL  objf_gentim2dm(nsx,nsy,maxCtrlTim2D)
      _RL  objf_gentim2dsmoo(nsx,nsy,maxCtrlTim2D)
      _RL  num_gentim2d(nsx,nsy,maxCtrlTim2D)
      _RL  num_gentim2dm(nsx,nsy,maxCtrlTim2D)
      _RL  mult_gentim2d(maxCtrlTim2D)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
