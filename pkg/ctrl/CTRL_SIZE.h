C $Header: /u/gcmpack/MITgcm/pkg/ctrl/CTRL_SIZE.h,v 1.1 2012/07/31 16:05:56 heimbach Exp $
C $Name:  $

c     ==================================================================
c     CTRL_SIZE.h
c     ==================================================================

C     Generic control variable array dimension
C     ----------------------------------------
C
C     maxCtrlArr2D :: number of 2-dim. generic control variables
C     maxCtrlArr3D :: number of 3-dim. generic control variables

#ifdef ALLOW_GENARR2D_CONTROL
      integer     maxCtrlArr2D
      parameter ( maxCtrlArr2D = 3 )
#endif

#ifdef ALLOW_GENARR3D_CONTROL
      integer     maxCtrlArr3D
      parameter ( maxCtrlArr3D = 3 )
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
