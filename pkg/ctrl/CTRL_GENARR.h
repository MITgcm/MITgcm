C $Header: /u/gcmpack/MITgcm/pkg/ctrl/CTRL_GENARR.h,v 1.2 2012/09/11 23:46:15 heimbach Exp $
C $Name:  $

c     ==================================================================
c     CTRL_GENARR.h
c     ==================================================================

      common /controlfiles_carr/
     &     xx_genarr2d_file
     &   , xx_genarr3d_file
      character*(MAX_LEN_FNAM)
     &     xx_genarr2d_file(maxCtrlArr2D)
     &   , xx_genarr3d_file(maxCtrlArr3D)
     
      character*( 80) fname_genarr2d(3,maxCtrlArr2D)
      character*( 80) fname_genarr3d(3,maxCtrlArr3D)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***



