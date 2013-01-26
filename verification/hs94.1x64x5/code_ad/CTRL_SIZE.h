C $Header: /u/gcmpack/MITgcm/verification/hs94.1x64x5/code_ad/CTRL_SIZE.h,v 1.2 2013/01/26 14:47:45 heimbach Exp $
C $Name:  $

c     ==================================================================
c     CTRL_SIZE.h
c     ==================================================================

C     Generic control variable array dimension
C     ----------------------------------------
C
C     maxCtrlArr2D :: number of 2-dim. generic control variables
C     maxCtrlArr3D :: number of 3-dim. generic control variables

      integer     maxCtrlArr2D
      parameter ( maxCtrlArr2D = 3 )

      integer     maxCtrlArr3D
      parameter ( maxCtrlArr3D = 3 )

      integer     maxCtrlTim2D
      parameter ( maxCtrlTim2D = 1 )

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
