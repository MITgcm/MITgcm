C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_biogeo/code_oad/CTRL_SIZE.h,v 1.1 2013/04/23 16:29:35 jahn Exp $
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
      parameter ( maxCtrlArr2D = 1 )

      integer     maxCtrlArr3D
      parameter ( maxCtrlArr3D = 1 )

      integer     maxCtrlTim2D
      parameter ( maxCtrlTim2D = 1 )

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
