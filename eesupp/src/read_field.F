C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/read_field.F,v 1.5 2000/03/15 16:00:52 adcroft Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE READ_FIELD_XYZR8(
     O                   fld,
     I                   filNam, filFmt, myThid )
C     /==========================================================\
C     | SUBROUTINE READ_FIELD_XYZR8                              |
C     | o Reads a file into three-dimensional model array        |
C     |==========================================================|
C     | Routine that controls the reading of external datasets   |
C     | into the model. In a multi-threaded and/or MPI world     |
C     | this can be a non-trivial exercise. Here we use the      |
C     | following approach:                                      |
C     | Thread 1. reads data for the complete domain i.e. all    |
C     | processes and all threads into a buffer. Each individual |
C     | thread then reads its portion of data into the actual    |
C     | model array. This is clean because there is just one     |
C     | input file with a single format irrespective of the      |
C     | of processes or threads in use. However, it has several  |
C     | potential difficulties.                                  |
C     |  1. Very large problems may have individual fields of    |
C     |     several GB. For example 1/20th degree global and     |
C     |     fifty levels is 10GB per field at 8 byte precision.  |
C     |  2. MPI 1.nn is vague about I/O support - not all        |
C     |     processes have to support I/O.                       |
C     |  MPI 2. includes a standard API for distributed data,    |
C     |  parallel I/O. If applications funnel all their field    |
C     |  I/O through this routine then adopting this or some     |
C     |  alternative should be fairly straight-forward.          |
C     |   In the light of problem 1. the following strategy      |
C     |  is adopted. Files are read one layer at a time. After   |
C     |  each layer has been read there is a barrier and then    |
C     |  the threads all copy data from the buffer to the arrays.|
C     |  This creates a lower-performance I/O code but reduces   |
C     |  the degree to which a single large array is required for|
C     |  the master thread. To be consistent with this binary    |
C     |  input files must be written by code of the form         |
C     |  WRITE(N) ((array(I,J,K),I=1,Nx),J=1,Ny)                 |
C     |  rather than of the form                                 |
C     |  WRITE(N) array                                          |
C     |   The approach taken here also avoids one other ugly     |
C     |  behaviour. On several systems even Fortran internal     |
C     |  reads and writes are not thread-safe. This means that   |
C     |  the portion of the code that builds file names has to   |
C     |  be a critical section. However, if only the master      |
C     |  thread is interested in the value of the file name then |
C     |  only the master need set its value.                     |
C     |   Finally the IO performed here is for the whole XY      |
C     |  domain - even under MPI. The input files can stay the   |
C     |  same no matter what processor count is being used.      |
C     |  This is not a scalable approach to IO and MPI 2 has much|
C     |  better support for this. Output is handled differently. |
C     |  By default output files are written split and have to be|
C     |  merged in a post-processing stage - YUK!                |
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
