C $Id: get_map.F,v 1.1 1998/05/25 20:21:05 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"

C/-------------------------------------------------------------------\
C|||  Procedure: GET_MAP                                           |||
C|||===============================================================|||
C|||        Function: Loads two dimensional (XY) general map.      |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE GET_MAP ( 
     I                    mapFile, IOUNIT, 
     O                    basinMask,
     I                    Nx, Ny, Nmax, Nb,
     U                    iErr )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     |==============================================================|
C     | ** NONE **                                                   |
C     \--------------------------------------------------------------/
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | mapFile - Name of file from which map will be read.          |
C     | IOUNIT  - Unit number for reading mapFile                    |
C     | basinMask - Array into which mask will be read.              |
C     | Nx, Ny, Nmax - Size of basinMask.                            |
C     | Nb           - Index of the basin being read.                |
C     | iErr         - Error flag.                                   |
C     \--------------------------------------------------------------/
      CHARACTER*(*) mapFile      
      INTEGER       IOUNIT        
      INTEGER       Nx
      INTEGER       Ny
      INTEGER       Nmax
      REAL          basinMask(Nx,Ny,Nmax)
      INTEGER       Nb
      INTEGER       iErr
CEndofinterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | RECORD  - I/O Buffer.                                        |
C     | NREC, I, I1, J, I2 - Loop counters                           |
C     \--------------------------------------------------------------/
      CHARACTER*1024 RECORD
      INTEGER NREC
      INTEGER I, I1
      INTEGER J, I2
C
      IF ( Nx .GT. 1024 ) GOTO 999
      OPEN(IOUNIT,FILE=mapFile,STATUS='OLD',ERR=998)
      NREC = 0
      I2   = Ny+1
    3 CONTINUE
       READ ( IOUNIT, '(A1024)', END = 4 ) RECORD
       NREC = NREC+1
C      Skip commented lines
       IF (RECORD(1:1) .EQ. '#' ) GOTO 3
C      Skip blank lines at the end of the file
       IF ( I2 .EQ. 1 .AND. RECORD .EQ. ' ' ) GOTO 3
       I2 = I2 - 1
       IF ( I2 .LT. 1 ) GOTO 4
       READ (RECORD,'(1024F1.0)',ERR=997) basinMask(:,I2,Nb)
       GOTO 3
    4  CONTINUE
       IF ( I2 .NE. 1 ) GOTO 996
       CLOSE(IOUNIT)
C      Map loaded O.K.
      
 1000 CONTINUE
      RETURN
  999 CONTINUE
       iErr = 1 ! Buffer too small
      GOTO 1000
  998 CONTINUE
       iErr = 2 ! Error opening file
      GOTO 1000
  997 CONTINUE
       iErr = 3 ! Error reading file
       CLOSE(IOUNIT)
      GOTO 1000
  996 CONTINUE
       iErr = 4 ! Incorrect number of records in map file.
      GOTO 1000
      END
