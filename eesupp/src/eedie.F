C $Header: /u/gcmpack/MITgcm/eesupp/src/eedie.F,v 1.2 1998/04/23 20:37:30 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE EEDIE
C     /==========================================================\
C     | SUBROUTINE EEDIE                                         |
C     | o Close execution "environment", particularly perform    |
C     |   steps to terminate parallel processing.                |
C     |==========================================================|
C     | Note: This routine can also be compiled with CPP         |
C     | directives set so that no multi-processing is initialised|
C     | This is OK and should work fine.                         |
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
CEndOfInterface

C     == Local variables ==
C     msgBuf       - I/O Buffer
C     nThreadsDone - Used to count number of completed threads.
C     I            - Loop counter.
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER nThreadsDone
      INTEGER I
#ifdef ALLOW_USE_MPI
C     mpiRC            - Error code reporting variable used
C                        with MPI.
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Check that all the threads have ended
C     No thread should reach this loop before all threads have set
C     threadIsComplete to TRUE. If they do then either there is a bug
C     in the code or the behaviour of the parallel compiler directives 
C     are not right for this code. In the latter case different directives
C     may be available or the compiler itself may have a bug or you may
C     need a different parallel compiler for main.F
      nThreadsDone = 0
      DO I = 1, nThreads
       IF ( threadIsComplete(I) ) nThreadsDone = nThreadsDone+1
      ENDDO
      IF ( nThreadsDone .LT. nThreads ) THEN
       WRITE(msgBuf,'(A,I,A,I,A)')
     & 'S/R EEDIE: Only ',nThreadsDone,
     & ' threads have completed, ',nThreads,' are expected for this configuration!'
       WRITE(0,*) msgBuf
       WRITE(0,*) 'Possibly you have different values of setenv PARALLEL and nThreads?'
       eeEndError = .TRUE.
       fatalError = .TRUE.
      ENDIF

#ifdef ALLOW_USE_MPI
C--
C--   MPI style multiple-process termination
C--   ======================================
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
       CALL MPI_FINALIZE  ( mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeEndError = .TRUE.
        fatalError = .TRUE.
        WRITE(msgBuf,'(A,I)')
     &       'S/R FIN_PROCS: MPI_FINALIZE return code',
     &       mpiRC
        CALL PRINT_ERROR( msgBuf, 1 )
       ENDIF
C
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /* ALLOW_USE_MPI */


      RETURN
      END
