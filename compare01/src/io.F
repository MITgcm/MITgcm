C $Id: io.F,v 1.1 1998/05/25 20:21:05 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C ===========
C | io.F    |
C ===========
C Contents
C o IO_R2D
C o IO_R3D

C================================================================================
C     Procedure name: IO_R2D                                                    |
C           Function: Peforms I/O for a 2D Real field of default precision.     |
C           Comments:                                                           |
C================================================================================
CStartOfInterface
      SUBROUTINE IO_R2D(
     I                  N1, N2, fName, direction, eUnit,
     &                  field, iErr )
      IMPLICIT NONE
#include "EXTERNAL.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | N1, N2    - Dimensions of data array.                        |
C     | fName     - Filename to written or read.                     |
C     | direction - Read | Write indicator.                          |
C     | field     - Data array.                                      |
C     | eUnit     - Stream to which error messages are written.      |
C     | iErr      - Error flag.                                      |
C     \--------------------------------------------------------------/
      INTEGER N1, N2
      CHARACTER*(*) fName
      CHARACTER     direction
      REAL field(N1,N2)
      INTEGER iErr
      INTEGER eUnit
CEndOfInterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I,J       - Loop counters.                                   |
C     | dir       - Work variable.                                   |
C     | READ_IN   - Character used to request read.                  |
C     | WRITE_OUT - Character used to request write.                 |
C     | dUnit     - unit number for I/O.                             |
C     | s1, s2    - Start and end character position in fName        |
C     | ioErr     - Flag for returning I/O error.                    |
C     \--------------------------------------------------------------/
      INTEGER I, J
      CHARACTER dir
      CHARACTER READ_IN
      PARAMETER ( READ_IN   = 'R' )
      CHARACTER WRITE_OUT
      PARAMETER ( WRITE_OUT = 'W' )
      INTEGER   dUnit
      INTEGER   s1, s2
      INTEGER   ioErr
C
      ioErr = 0
      dUnit = 23
      dir = direction
      CALL UCASE(dir)
      s1 = IFNBLNK(fName)
      s2 = ILNBLNK(fName)
      IF     ( dir .EQ. WRITE_OUT ) THEN
       OPEN (UNIT=dUnit, FILE=fName(s1:s2), IOSTAT=ioErr,
     &       form='unformatted',status='UNKNOWN')
       IF ( ioErr .LT. 0 ) GOTO 903
      ELSEIF ( dir .EQ. READ_IN   ) THEN
       OPEN (UNIT=dUnit, FILE=fName(s1:s2), IOSTAT=ioErr,
     &       form='unformatted',status='OLD')
       IF ( ioErr .NE. 0 ) GOTO 904
      ELSE
       GOTO 905
      ENDIF
C
      IF ( dir .EQ. READ_IN ) THEN
       READ (unit=dUnit, IOSTAT=ioErr ) field
       IF ( ioErr .NE. 0 ) GOTO 906
       WRITE (0,'(''Read dump file: '', 1024A1)')
     & (fName(I:I),I=s1,s2)
      ELSEIF ( DIR .EQ. WRITE_OUT ) THEN
       WRITE (unit=dUnit, IOSTAT=ioErr ) field
       IF ( ioErr .NE. 0 ) GOTO 907
       WRITE (0,'(''Wrote dump file: '', 1024A1)')
     & (fName(I:I),I=s1,s2)
      ENDIF
      CLOSE(dUnit)
C
 1000 CONTINUE
      RETURN
  903 CONTINUE
       WRITE (eUnit,'('' ** Error during readwrite open of file: '',1024A1)')
     & (fName(I:I),I=s1,s2)
       iErr = 1
      GOTO 1000
  904 CONTINUE
       WRITE (eUnit,'('' ** Error during readonly open of file: '',1024A1)')
     & (fName(I:I),I=s1,s2) 
       iErr = 1
      GOTO 1000
  905 CONTINUE
       WRITE (eUnit,'('' ** Error dump direction not '',A1,'' or '',A1)')
     & READ_IN, WRITE_OUT
       iErr = 1
      GOTO 1000
  906 CONTINUE
       WRITE (eUnit,'('' ** Error reading file: '',1024A1)')
     & (fName(I:I),I=s1,s2) 
       iErr = 1
      GOTO 1000
  907 CONTINUE
       WRITE (eUnit,'('' ** Error writing file: '',1024A1)')
     & (fName(I:I),I=s1,s2) 
       iErr = 1
      GOTO 1000
      END
C================================================================================
C     Procedure name: IO_R3D                                                    |
C           Function: Peforms I/O for a 3D Real field of default precision.     |
C           Comments:                                                           |
C================================================================================
CStartOfInterface
      SUBROUTINE IO_R3D(
     I                  N1, N2, N3, fName, direction, eUnit,
     &                  field, iErr )
      IMPLICIT NONE
#include "EXTERNAL.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | N1, N2, N3- Dimensions of data array.                        |
C     | fName     - Filename to written or read.                     |
C     | direction - Read | Write indicator.                          |
C     | field     - Data array.                                      |
C     | eUnit     - Stream to which error messages are written.      |
C     | iErr      - Error flag.                                      |
C     \--------------------------------------------------------------/
      INTEGER N1, N2, N3
      CHARACTER*(*) fName
      CHARACTER     direction
      REAL field(_I3(N3,N1,N2))
      INTEGER iErr
      INTEGER eUnit
CEndOfInterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I,J       - Loop counters.                                   |
C     | dir       - Work variable.                                   |
C     | READ_IN   - Character used to request read.                  |
C     | WRITE_OUT - Character used to request write.                 |
C     | dUnit     - unit number for I/O.                             |
C     | s1, s2    - Start and end character position in fName        |
C     | ioErr     - Flag for returning I/O error.                    |
C     \--------------------------------------------------------------/
      INTEGER I, J
      CHARACTER dir
      CHARACTER READ_IN
      PARAMETER ( READ_IN   = 'R' )
      CHARACTER WRITE_OUT
      PARAMETER ( WRITE_OUT = 'W' )
      INTEGER   dUnit
      INTEGER   s1, s2
      INTEGER   ioErr
C
      ioErr = 0
      dUnit = 23
      dir = direction
      CALL UCASE(dir)
      s1 = IFNBLNK(fName)
      s2 = ILNBLNK(fName)
      IF     ( dir .EQ. WRITE_OUT ) THEN
       OPEN (UNIT=dUnit, FILE=fName(s1:s2), IOSTAT=ioErr,
     &       form='unformatted',status='UNKNOWN')
       IF ( ioErr .NE. 0 ) GOTO 903
      ELSEIF ( dir .EQ. READ_IN   ) THEN
       OPEN (UNIT=dUnit, FILE=fName(s1:s2), IOSTAT=ioErr,
     &       form='unformatted',status='OLD')
       IF ( ioErr .NE. 0 ) GOTO 904
      ELSE
       GOTO 905
      ENDIF
C
      IF ( dir .EQ. READ_IN ) THEN
       READ (unit=dUnit, IOSTAT=ioErr ) field
       IF ( ioErr .NE. 0 ) GOTO 906
       WRITE (0,'(''Read dump file: '', 1024A1)')
     & (fName(I:I),I=s1,s2)
      ELSEIF ( DIR .EQ. WRITE_OUT ) THEN
       WRITE (unit=dUnit, IOSTAT=ioErr ) field
       IF ( ioErr .NE. 0 ) GOTO 907
       WRITE (0,'(''Wrote dump file: '', 1024A1)')
     & (fName(I:I),I=s1,s2)
      ENDIF
      CLOSE(dUnit)
C
 1000 CONTINUE
      RETURN
  903 CONTINUE
       WRITE (eUnit,'('' ** Error during readwrite open of file: '',1024A1)')
     & (fName(I:I),I=s1,s2)
       iErr = 1
      GOTO 1000
  904 CONTINUE
       WRITE (eUnit,'('' ** Error during readonly open of file: '',1024A1)')
     & (fName(I:I),I=s1,s2)
       iErr = 1
      GOTO 1000
  905 CONTINUE
       WRITE (eUnit,'('' ** Error dump direction not '',A1,'' or '',A1)')
     & READ_IN, WRITE_OUT
       iErr = 1
      GOTO 1000
  906 CONTINUE
       WRITE (eUnit,'('' ** Error reading file: '',1024A1)')
     & (fName(I:I),I=s1,s2)
       iErr = 1
      GOTO 1000
  907 CONTINUE
       WRITE (eUnit,'('' ** Error writing file: '',1024A1)')
     & (fName(I:I),I=s1,s2)
       iErr = 1
      GOTO 1000
      END
