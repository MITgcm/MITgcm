C $Header: /u/gcmpack/MITgcm/eesupp/src/check_threads.F,v 1.16 2012/03/30 14:18:07 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: CHECK_THREADS

C     !INTERFACE:
      SUBROUTINE CHECK_THREADS( myThid )

C     !DESCRIPTION:
C     *==========================================================
C     | SUBROUTINE CHECK\_THREADS
C     | o Check that all the threads we need did indeed start.
C     *==========================================================
C     | This routine is called during the initialisation phase
C     | to check whether all the threads have started.
C     | It is invoked by every thread and if any thread finds an
C     | error it should set its error flag.
C     | Notes:
C     |  Different mechanisms may be required on different
C     | platforms to actually perform the check. For example as
C     | coded here each thread checks for a semaphore set by the
C     | other threads to see if they are running.
C     | It is also possible for a system to schedule threads
C     | sequentially, unless some system call is made to yield
C     | the process. This routine would detect this situation too
C     | and allow a programmer to modify this routine and the
C     | barrier code to allow threads to be scheduled more
C     | appropriately.
C     *==========================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !INPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: My thread number
      INTEGER myThid

C     !FUNCTIONS:
#ifdef USE_OMP_THREADING
      INTEGER  OMP_GET_NUM_THREADS
      EXTERNAL OMP_GET_NUM_THREADS
#endif

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I         :: Loop counter
C     numberThreadRunning :: Count of number of threads this thread
C                            thinks are running.
C     nChecks   :: Number of times checked for all threads. After so
C                  many checks give up and report an error.
C     msgBuf    :: Informational/error message buffer
      INTEGER nChecks
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef USE_OMP_THREADING
#ifdef ALLOW_USE_MPI
      INTEGER myErr, mpiRC
#endif
#else /* USE_OMP_THREADING */
      INTEGER I, numberThreadsRunning
#endif /* USE_OMP_THREADING */
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef USE_OMP_THREADING
C--   Check early-on that number of threads match

      IF ( OMP_GET_NUM_THREADS() .NE. nThreads ) THEN
C-    This process has problems in multi-threads setting (detected by
C     all pseudo-threads); note: cannot use any Barrier in this context

       WRITE(msgBuf,'(2A,I6)') 'CHECK_THREADS:',
     &              ' from "eedata", nThreads=', nThreads
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,I6)') ' not equal to ',
     &              'Env.Var. OMP_NUM_THREADS=', OMP_GET_NUM_THREADS()
       CALL PRINT_ERROR( msgBuf, myThid )
       thError(myThid) = .TRUE.
       eeBootError     = .TRUE.
       IF ( myThid.EQ.1 ) THEN
C-    one pseudo-thread (thId=1) export the error to other MPI processes
        nChecks = 1
#ifdef ALLOW_USE_MPI
        IF ( usingMPI ) THEN
         myErr = nChecks
         CALL MPI_Allreduce( myErr,nChecks,1,MPI_INTEGER,
     &                       MPI_SUM,MPI_COMM_MODEL,mpiRC )
        ENDIF
#endif /* ALLOW_USE_MPI */
       ENDIF

      ELSE
C-    this process has a working multi-threads setting

       threadIsRunning(myThid) = .TRUE.
       IF ( myThid.EQ.1 ) THEN
C-    master collects error from other MPI processes
        nChecks = 0
#ifdef ALLOW_USE_MPI
        IF ( usingMPI ) THEN
         myErr = nChecks
         CALL MPI_Allreduce( myErr,nChecks,1,MPI_INTEGER,
     &                       MPI_SUM,MPI_COMM_MODEL,mpiRC )
        ENDIF
#endif /* ALLOW_USE_MPI */
        IF ( nChecks.NE.0 ) THEN
          WRITE(msgBuf,'(A,I5,A)') 'CHECK_THREADS:', nChecks,
     &                     ' error(s) from other Processes'
          CALL PRINT_ERROR( msgBuf, myThid )
          eeBootError = .TRUE.
        ENDIF
       ENDIF
C-    ensure all threads leave with updated eeBootError (shared) value
C$OMP BARRIER

      ENDIF

#else /* ndef USE_OMP_THREADING */

      threadIsRunning(myThid) = .TRUE.
      nChecks                 = 0

   10 CONTINUE
      numberThreadsRunning = 0
      nChecks = nChecks + 1
      DO I = 1, nThreads
       IF ( threadIsRunning(I) )
     &  numberThreadsRunning = numberThreadsRunning+1
      ENDDO
      IF ( nChecks .GT. 10 ) THEN
       thError(myThid) = .TRUE.
       eeBootError     = .TRUE.
       WRITE(msgBuf,'(A,I5,A,I5,A)')
     &  'CHECK_THREADS: Only ',numberThreadsRunning,
     &  ' thread(s), ',nThreads,' are needed for this config!'
        CALL PRINT_ERROR( msgBuf, myThid )
C--     Not enough threads are running so halt the program.
C       I did not want this here but it is the only place I have found that
C       KAP deadlocks if it there are fewer threads than iterations in a BLOCKED(1)
C       loop. The deadlock appears to be in the routine mppjoin which never
C       returns. I tried putting the STOP in main or breaking out of the loop in main
C       but this causes KAP to insert a call to mppjoin - which then deadlocks!
        IF ( myThid .EQ. 1 ) THEN
         STOP 'ABNORMAL END: S/R CHECK_THREADS'
        ENDIF
       GOTO 11
      ENDIF
      IF ( numberThreadsRunning .NE. nThreads ) THEN
#ifdef HAVE_SYSTEM
       CALL SYSTEM('sleep 1')
#endif
       GOTO 10
      ENDIF
   11 CONTINUE

#endif /* ndef USE_OMP_THREADING */

C--   check barrier synchronization: 1rst (initial) call.
      IF ( .NOT. eeBootError ) THEN
        CALL BAR_CHECK( 1, myThid )
      ENDIF

      RETURN
      END
