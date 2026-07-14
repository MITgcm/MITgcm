CBOP
C     !ROUTINE: CTRL_SIZE.h
C     !INTERFACE:
C     #include "CTRL_SIZE.h"

C     !DESCRIPTION:
C     *================================================================*
C     | CTRL_SIZE.h
C     | o set maximum number of control variables
C     *================================================================*
CEOP

C     Generic control variable array dimension
C     ----------------------------------------
C
C     maxCtrlArr2D :: number of 2-D generic init. ctrl variables
C     maxCtrlArr3D :: number of 3-D generic init. ctrl variables
C     maxCtrlTim2D :: number of 2-D generic time-varying ctrl variables
C     maxCtrlProc  :: number of pre-processing options per ctrl variable

      INTEGER     maxCtrlArr2D
      PARAMETER ( maxCtrlArr2D = 1 )

      INTEGER     maxCtrlArr3D
      PARAMETER ( maxCtrlArr3D = 2 )

      INTEGER     maxCtrlTim2D
      PARAMETER ( maxCtrlTim2D = 1 )

      INTEGER     maxCtrlProc
      PARAMETER ( maxCtrlProc = 1 )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
