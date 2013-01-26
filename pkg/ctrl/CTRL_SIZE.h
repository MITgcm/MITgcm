C $Header: /u/gcmpack/MITgcm/pkg/ctrl/CTRL_SIZE.h,v 1.4 2013/01/26 14:45:56 heimbach Exp $
C $Name:  $

c     ==================================================================
c     CTRL_SIZE.h
c     ==================================================================

C     Generic control variable array dimension
C     ----------------------------------------
C
C     maxCtrlArr2D :: number of 2-d generic init. ctrl variables
C     maxCtrlArr3D :: number of 3-d generic init. ctrl variables
C     maxCtrlTim2D :: number of 2-d generic tim-varying ctrl variables

      integer     maxCtrlArr2D
      parameter ( maxCtrlArr2D = 1 )

      integer     maxCtrlArr3D
      parameter ( maxCtrlArr3D = 1 )

      integer     maxCtrlTim2D
      parameter ( maxCtrlTim2D = 1 )

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
