C $Header: /u/gcmpack/MITgcm/model/src/plot_field.F,v 1.4 1998/06/12 19:33:34 cnh Exp $

#include "CPP_EEOPTIONS.h"

C--   File plot_field.F: Routines for "formatted" I/O in the MITgcm UV 
C--                      implementation.
C--    Contents
C--    o plot_field_xyr4  - Writes a XY  Real*4 field
C--    o plot_field_xyr8  - Writes a XY  Real*8 field
C--    o plot_field_xyzr4 - Writes a XYZ Real*4 field
C--    o plot_field_xyzr8 - Writes a XYZ Real*8 field
      SUBROUTINE PLOT_FIELD_XYR4( 
     I                            fld, fldNam , myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYR4                               |
C     | Print out an XY Real 4 field using text map.             |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     \==========================================================/
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      Real*4 fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') '// Field ', fldNam, ' at iteration ',
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
       CALL PRINT_MAPR4(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,1,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF

      RETURN
      END
      SUBROUTINE PLOT_FIELD_XYR8( 
     I                            fld, fldNam , myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYR8                               |
C     | Print out an XY Real 8 field using text map.             |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     \==========================================================/
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"

C     == Routine arguments ==
C     fld - Field to plot
C     fldNam - Name of field
C     myIter - Iteration number for plot
C     myThid - Thread id of thread instance calling plot_field
      REAL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') '// Field ', fldNam, ' at iteration ',
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
       CALL PRINT_MAPR8(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,1,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF

      RETURN
      END
      SUBROUTINE PLOT_FIELD_XYZR4( 
     I                            fld, fldNam , fldNz, myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYZR4                              |
C     | Print out an XYZ Real 4 field using text map.            |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     \==========================================================/
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"

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
      Real*4 fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:fldNz,nSx,nSy)
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
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') '// Field ', fldNam, ' at iteration ',
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
       kEnd     =  fldNz
C      kEnd     =  1
       kStride  =  1
       biStart  =  1
       biEnd    =  nSx
       biStride =  1
       bjStart  =  nSy
       bjEnd    =  1    
       bjStride = -1
C      CALL PRINT_MAPR8(
C    I        fld, fldTitle, PRINT_MAP_YZ,
C    I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,fldNz,  nSx,  nSy,
C    I         iStart,   iEnd,  iStride,
C    I         jStart,   jEnd,  jStride,
C    I         kStart,   kEnd,  kStride,
C    I        biStart,  biEnd, biStride,
C    I        bjStart,  bjEnd, bjStride )
       CALL PRINT_MAPR4(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,fldNz,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF

      RETURN
      END
      SUBROUTINE PLOT_FIELD_XYZR8( 
     I                            fld, fldNam , fldNz, myIter, myThid )

C     /==========================================================\
C     | SUBROUTINE PLOT_FIELD_XYZR8                              |
C     | Print out an XYZ Real 8 field using text map.            |
C     |==========================================================|
C     | This routine references "numerical model" parameters like|
C     | like the integration time. It uses these to create a     |
C     | title for the field before calling a generic execution   |
C     | environment support routine.                             |
C     | This routine can also be edited to cause only some region|
C     | of a field to be printed by default, or every other      |
C     | point etc..                                              |
C     | Other plot formats can also be substituted here.         |
C     \==========================================================/
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"

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
      REAL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:fldNz,nSx,nSy)
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
      IF ( myThid .EQ. 1 ) THEN
C--    Form name for identifying "plot"
       IF ( myIter .GE. 0 ) THEN
        WRITE(fldTitle,'(A,A,A,I10)') '// Field ', fldNam, ' at iteration ',
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
C      CALL PRINT_MAPR8(
C    I        fld, fldTitle, PRINT_MAP_YZ,
C    I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,fldNz,  nSx,  nSy,
C    I         iStart,   iEnd,  iStride,
C    I         jStart,   jEnd,  jStride,
C    I         kStart,   kEnd,  kStride,
C    I        biStart,  biEnd, biStride,
C    I        bjStart,  bjEnd, bjStride )
       CALL PRINT_MAPR8(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1-OLx,sNx+OLx,1-OLy,sNy+OLy,1,fldNz,  nSx,  nSy,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )
      ENDIF

      RETURN
      END

