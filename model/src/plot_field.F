C $Header: /u/gcmpack/MITgcm/model/src/plot_field.F,v 1.14 2002/02/07 20:00:09 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C--   File plot_field.F: Routines for "formatted" I/O in the MITgcm UV 
C--                      implementation.
C--    Contents
C--    o plot_field_xyrs  - Writes a XY  _RS field
C--    o plot_field_xyrl  - Writes a XY  _RL field
C--    o plot_field_xyzrs - Writes a XYZ _RS field
C--    o plot_field_xyzrl - Writes a XYZ _RL field
      SUBROUTINE PLOT_FIELD_XYRS( 
     I                            fld, fldNam , myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYRS                               |
C     | Print out an XY _RS field using text map.                |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     | _RS is usually REAL*4                                    |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(*) fldNam
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   To get around synchronisation and multi-threaded I/O issues
C--   thread 1 will do all the writes.
      _BARRIER
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') 
     &  '// Field ', fldNam, ' at iteration ',
     &  myIter
       ELSE
        WRITE(fldTitle,'(A,A)') '// Field ', fldNam
       ENDIF
C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1-OLx
       iEnd     =  sNx+OLx
       iStride  =  1
       jStart   =  sNy+OLy
       jEnd     =  1-OLy
       jStride  = -1
       kStart   =  1
       kEnd     =  1
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx
       biStride =  1
       bjStart  =  nSy
       bjEnd    =  1    
       bjStride = -1
       CALL PRINT_MAPRS(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,1,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF
      _BARRIER

      RETURN
      END
      SUBROUTINE PLOT_FIELD_XYRL( 
     I                            fld, fldNam , myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYRL                               |
C     | Print out an XY _RL field using text map.                |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     | _RL is usually REAL*8                                    |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(*) fldNam
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   To get around synchronisation and multi-threaded I/O issues
C--   thread 1 will do all the writes.
      _BARRIER
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') 
     &  '// Field ', fldNam, ' at iteration ',
     &  myIter
       ELSE
        WRITE(fldTitle,'(A,A)') '// Field ', fldNam
       ENDIF
C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1-OLx
       iEnd     =  sNx+OLx
       iStride  =  1
       jStart   =  sNy+OLy
       jEnd     =  1-OLy
       jStride  = -1
       kStart   =  1
       kEnd     =  1
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx
       biStride =  1
       bjStart  =  nSy
       bjEnd    =  1    
       bjStride = -1
       CALL PRINT_MAPRL(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,1,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF
      _BARRIER

      RETURN
      END
      SUBROUTINE PLOT_FIELD_XYZRS( 
     I                            fld, fldNam , fldNz, myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYZR4                              |
C     | Print out an XYZ _RS field using text map.               |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     | _RS is usually a REAL*4 field                            |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     fldNz  - No. of layers in the vertical
C              (Different fields may have different vertical extents)
C              (Under the present implementation all fields have the)
C              (same lateral extents.                               )
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      INTEGER fldNz
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:fldNz,nSx,nSy)
      CHARACTER*(*) fldNam
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   To get around synchronisation and multi-threaded I/O issues
C--   thread 1 will do all the writes.
      _BARRIER
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') 
     &  '// Field ', fldNam, ' at iteration ',
     &  myIter
       ELSE
        WRITE(fldTitle,'(A,A)') '// Field ', fldNam
       ENDIF
C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1-OLx
       iEnd     =  sNx+OLx
C      iStart   =  1
C      iEnd     =  sNx
       iStride  =  1
       jStart   =  sNy+OLy
       jEnd     =  1-OLy
C      jStart   =  sNy
C      jEnd     =  1
       jStride  = -1
       kStart   =  1
C      kEnd     =  fldNz
       kEnd     =  1
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx
       biStride =  1
       bjStart  =  nSy
       bjEnd    =  1    
       bjStride = -1
       CALL PRINT_MAPRS(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,fldNz,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF
      _BARRIER

      RETURN
      END
      SUBROUTINE PLOT_FIELD_XYZRL( 
     I                            fld, fldNam , fldNz, myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYZRL                              |
C     | Print out an XYZ _RL field using text map.               |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     | _RL is usually a REAL*8 field                            |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     fldNz  - No. of layers in the vertical
C              (Different fields may have different vertical extents)
C              (Under the present implementation all fields have the)
C              (same lateral extents.                               )
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      INTEGER fldNz
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:fldNz,nSx,nSy)
      CHARACTER*(*) fldNam
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   To get around synchronisation and multi-threaded I/O issues
C--   thread 1 will do all the writes.
      _BARRIER
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') 
     &  '// Field ', fldNam, ' at iteration ',
     &  myIter
       ELSE
        WRITE(fldTitle,'(A,A)') '// Field ', fldNam
       ENDIF
C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1-OLx
       iEnd     =  sNx+OLx
       iStart   =  1
       iEnd     =  sNx
       iStride  =  1
       jStart   =  sNy+OLy
       jEnd     =  1-OLy
       jStart   =  sNy
       jEnd     =  1
       jStride  = -1
       kStart   =  1
       kEnd     =  fldNz
       kEnd     =  1
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx
       biStride =  1
       bjStart  =  nSy
       bjEnd    =  1    
       bjStride = -1
       CALL PRINT_MAPRL(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,fldNz,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF
      _BARRIER

      RETURN
      END

      SUBROUTINE PLOT_FIELD_XZRS( 
     I                            fld, fldNam , fldNz, myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYZR4                              |
C     | Print out an XYZ _RS field using text map.               |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     | _RS is usually a REAL*4 field                            |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     fldNz  - No. of layers in the vertical
C              (Different fields may have different vertical extents)
C              (Under the present implementation all fields have the)
C              (same lateral extents.                               )
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      INTEGER fldNz
      _RS fld(1-OLx:sNx+OLx,1:fldNz,nSx,nSy)
      CHARACTER*(*) fldNam
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   To get around synchronisation and multi-threaded I/O issues
C--   thread 1 will do all the writes.
      _BARRIER
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') 
     &  '// Field ', fldNam, ' at iteration ',
     &  myIter
       ELSE
        WRITE(fldTitle,'(A,A)') '// Field ', fldNam
       ENDIF
C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1
       iEnd     =  sNx
       iStride  =  1
       jStart   =  1
       jEnd     =  1
       jStride  =  1
       kStart   =  1
       kEnd     =  fldNz
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx
       biStride =  1
       bjStart  =  nSy 
       bjEnd    =  1    
       bjStride = -1
       CALL PRINT_MAPRS(
     I        fld, fldTitle, PRINT_MAP_XZ,
     I         1-OLx,sNx+OLx,1,fldNz,1,1,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF
      _BARRIER

      RETURN
      END
      SUBROUTINE PLOT_FIELD_XZRL( 
     I                            fld, fldNam , fldNz, myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYZRL                              |
C     | Print out an XYZ _RL field using text map.               |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     | _RL is usually a REAL*8 field                            |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     fldNz  - No. of layers in the vertical
C              (Different fields may have different vertical extents)
C              (Under the present implementation all fields have the)
C              (same lateral extents.                               )
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      INTEGER fldNz
      _RL fld(1-OLx:sNx+OLx,1:fldNz,nSx,nSy)
      CHARACTER*(*) fldNam
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   To get around synchronisation and multi-threaded I/O issues
C--   thread 1 will do all the writes.
      _BARRIER
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') 
     &  '// Field ', fldNam, ' at iteration ',
     &  myIter
       ELSE
        WRITE(fldTitle,'(A,A)') '// Field ', fldNam
       ENDIF
C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1
       iEnd     =  sNx
       iStride  =  1
       jStart   =  1
       jEnd     =  1
       jStride  = -1
       kStart   =  1
       kEnd     =  fldNz
c      kEnd     =  1
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx
       biStride =  1
       bjStart  =  nSy
       bjEnd    =  1    
       bjStride = -1
       CALL PRINT_MAPRL(
     I        fld, fldTitle, PRINT_MAP_XZ,
     I         1-OLx,sNx+OLx,1,1,1,fldNz,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF
      _BARRIER

      RETURN
      END

      SUBROUTINE PLOT_FIELD_YZRS( 
     I                            fld, fldNam , fldNz, myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYZR4                              |
C     | Print out an XYZ _RS field using text map.               |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     | _RS is usually a REAL*4 field                            |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     fldNz  - No. of layers in the vertical
C              (Different fields may have different vertical extents)
C              (Under the present implementation all fields have the)
C              (same lateral extents.                               )
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      INTEGER fldNz
      _RS fld(1-OLy:sNy+OLy,1:fldNz,nSx,nSy)
      CHARACTER*(*) fldNam
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   To get around synchronisation and multi-threaded I/O issues
C--   thread 1 will do all the writes.
      _BARRIER
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') 
     &  '// Field ', fldNam, ' at iteration ',
     &  myIter
       ELSE
        WRITE(fldTitle,'(A,A)') '// Field ', fldNam
       ENDIF
C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1
       iEnd     =  1
       iStride  =  1
       jStart   =  sNy
       jEnd     =  1
       jStride  = -1
       kStart   =  1
       kEnd     =  fldNz
C      kEnd     =  1
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx 
       biStride =  1
       bjStart  =  nSy
       bjEnd    =  1    
       bjStride = -1
       CALL PRINT_MAPRS(
     I        fld, fldTitle, PRINT_MAP_YZ,
     I         1,1,1-OLy,sNy+OLy,1,fldNz,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF
      _BARRIER

      RETURN
      END
      SUBROUTINE PLOT_FIELD_YZRL( 
     I                            fld, fldNam , fldNz, myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYZRL                              |
C     | Print out an XYZ _RL field using text map.               |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     | _RL is usually a REAL*8 field                            |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     fldNz  - No. of layers in the vertical
C              (Different fields may have different vertical extents)
C              (Under the present implementation all fields have the)
C              (same lateral extents.                               )
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      INTEGER fldNz
      _RL fld(1-OLy:sNy+OLy,1:fldNz,nSx,nSy)
      CHARACTER*(*) fldNam
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   To get around synchronisation and multi-threaded I/O issues
C--   thread 1 will do all the writes.
      _BARRIER
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') 
     &  '// Field ', fldNam, ' at iteration ',
     &  myIter
       ELSE
        WRITE(fldTitle,'(A,A)') '// Field ', fldNam
       ENDIF
C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1
       iEnd     =  1
       iStride  =  1
       jStart   =  sNy
       jEnd     =  1
       jStride  = -1
       kStart   =  1
       kEnd     =  fldNz
c      kEnd     =  1
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx 
       biStride =  1
       bjStart  =  nSy
       bjEnd    =  1    
       bjStride = -1
       CALL PRINT_MAPRL(
     I        fld, fldTitle, PRINT_MAP_YZ,
     I         1,1,1-OLy,sNy+OLy,1,fldNz,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF
      _BARRIER

      RETURN
      END
