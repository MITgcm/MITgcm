C $Header: /u/gcmpack/MITgcm/pkg/ctrl/CTRL_GENARR.h,v 1.1 2012/07/31 16:05:56 heimbach Exp $
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

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***



