C $Header: /u/gcmpack/MITgcm/pkg/ctrl/CTRL_GENARR.h,v 1.4 2013/01/26 14:45:56 heimbach Exp $
C $Name:  $

C     ==================================================================
C     CTRL_GENARR.h
C     ==================================================================

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))

      COMMON /CONTROLFILES_CARR/
     &     xx_genarr2d_file,
     &     xx_genarr3d_file
      CHARACTER*(MAX_LEN_FNAM) xx_genarr2d_file(maxCtrlArr2D)
      CHARACTER*(MAX_LEN_FNAM) xx_genarr3d_file(maxCtrlArr3D)

      COMMON /CONTROLFILES_CTIM/
     &     xx_gentim2d_file
      CHARACTER*(MAX_LEN_FNAM) xx_gentim2d_file(maxCtrlTim2D)

      COMMON /CONTROLFILES_ITIM/
     &     xx_gentim2d_startdate1,
     &     xx_gentim2d_startdate2,
     &     xx_gentim2d_startdate
      INTEGER xx_gentim2d_startdate1(maxCtrlTim2D)
      INTEGER xx_gentim2d_startdate2(maxCtrlTim2D)
      INTEGER xx_gentim2d_startdate(4,maxCtrlTim2D)

      COMMON /CONTROLFILES_RTIM/
     &     xx_gentim2d_period
      _RL xx_gentim2d_period(maxCtrlTim2D)

#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      common /controlaux_gentim2d_r/
     &                      xx_gentim2d0,
     &                      xx_gentim2d1
      _RL 
     & xx_gentim2d0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
      _RL 
     & xx_gentim2d1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
