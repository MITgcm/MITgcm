c     ==================================================================
c     CTRL_SIZE.h
c     ==================================================================

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))

C     Generic control variable array dimension
C     ----------------------------------------
C
C     maxCtrlArr2D :: number of 2-d generic init. ctrl variables
C     maxCtrlArr3D :: number of 3-d generic init. ctrl variables
C     maxCtrlTim2D :: number of 2-d generic tim-varying ctrl variables
C     maxCtrlProc  :: number of pre-processing options per ctrl variable

      integer     maxCtrlArr2D
      parameter ( maxCtrlArr2D = 1 )

      integer     maxCtrlArr3D
      parameter ( maxCtrlArr3D = 8 )

      integer     maxCtrlTim2D
      parameter ( maxCtrlTim2D = 1 )

      integer     maxCtrlProc
      parameter ( maxCtrlProc = 1 )

#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
