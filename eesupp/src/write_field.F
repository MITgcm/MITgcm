C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/write_field.F,v 1.7 2000/03/15 16:00:52 adcroft Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE WRITE_FIELD_XYZR8(
     O                   fld,
     I                   filNam, filFmt, myThid )
C     /==========================================================\
C     | SUBROUTINE WRITE_FIELD_XYZR8                             |
C     | o Writes out a three-dimensional model array             |
C     |==========================================================|
C     | Routine that controls the writing of external datasets   |
C     | into the model. In a multi-threaded and/or MPI world     |
C     | this can be a non-trivial exercise. Here we use the      |
C     | following approach:                                      |
C     | Thread 1. writes data for the process domain i.e. all    |
C     | threads into a buffer. This thread then writes out       |
C     | the data for process.                                    |
C     | By default output files are written split and have to be |
C     | merged in a post-processing stage - YUK!                 |
C     \==========================================================/
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EEIO.h"

C     == Routine arguments ==
C     fld -  Array into which data will be written.
C     filNam - Name of file to read.
C     filFmt - Format to use to read the file.
C     myNz   - No. vertical layers for array fld.
C     myThid - Thread number for this instance of the routine.
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx, nSy )
      CHARACTER*(*) filNam
      CHARACTER*(*) filFmt
      INTEGER       myThid

#ifdef USE_EEIO

C     == Local variables ==
C     msgBuf       - Variable for writing error messages
C     I,J,K, bi,bj - Loop counters
C     dUnit        - Unit number for file I/O
C     ioStatus     - I/O error code
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER bi
      INTEGER bj
      INTEGER iG, jG
      INTEGER dUnit
      INTEGER ioStatus
C
      dUnit = 42

C--   Open the file
C     Note: The error trapping here is inelegant. There is no
C           easy way to tell other threads and/or MPI processes that
C           there was an error. Here we simply STOP if there is an error.
C           Under a multi-threaded mode this will halt all the threads.
C           Under MPI the other processes may die or they may just hang!
      _BEGIN_MASTER(myThid)
       OPEN(dUnit,FILE=filNam,FORM='unformatted',STATUS='old',
     &      IOSTAT=ioStatus)
       IF ( ioStatus .GT. 0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R READ_FIELD_XYZR8'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'Open for read failed for'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A,A50)')
     &   'file ',filNam
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R READ_FIELD_XYZR8'
       ENDIF
      _END_MASTER(myThid)

      DO K = 1, Nr
C--    Read data from file one XY layer at a time
       _BEGIN_MASTER(myThid)
C      READ ...
       DO J=1,Ny
        DO I=1,Nx
         IF     ( filNam(1:1) .EQ. 'u' ) THEN
          IO_tmpXY_R8(I,J) = 0.0 _d 0
          IF ( J .GT. 15 .AND. J .LT. 24 ) 
     &     IO_tmpXY_R8(I,J) = 0.1 _d 0
         ELSEIF ( filNam(1:1) .EQ. 'v' ) THEN
          IO_tmpXY_R8(I,J) = 0.0 _d 0
         ELSE
          IO_tmpXY_R8(I,J) = 0.0 _d 0
         ENDIF
        ENDDO
       ENDDO
       _END_MASTER(myThid)
       _BARRIER
C--    Copy data into per thread data structures
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           iG = myXGlobalLo+(bi-1)*sNx+I-1
           jG = myYGlobalLo+(bj-1)*sNy+J-1
           fld(i,j,k,bi,bj) = IO_tmpXY_R8(iG,jG)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _BARRIER
      ENDDO
C
      _EXCH_XYZ_R8(fld, myThid )
C
#endif

      RETURN
      END
