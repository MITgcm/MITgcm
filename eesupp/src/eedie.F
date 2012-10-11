C $Header: /u/gcmpack/MITgcm/eesupp/src/eedie.F,v 1.13 2012/10/11 19:15:18 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#ifdef USE_LIBHPM
# include "f_hpm.h"
#endif

CBOP
      SUBROUTINE EEDIE
C     *==========================================================*
C     | SUBROUTINE EEDIE                                         |
C     | o Close execution "environment", particularly perform    |
C     |   steps to terminate parallel processing.                |
C     *==========================================================*
C     | Note: This routine can also be compiled with CPP         |
C     | directives set so that no multi-processing is initialised|
C     | This is OK and should work fine.                         |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
CEOP

C     == Local variables ==
C     msgBuf       :: I/O Buffer
C     nThreadsDone :: Used to count number of completed threads.
C     I            :: Loop counter.
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER nThreadsDone
      INTEGER I
#ifdef ALLOW_USE_MPI
C     mpiRC        :: Error code reporting variable used with MPI.
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

      IF ( eeBootError ) THEN
C--   Skip ended threads counting if earlier error was found
        WRITE(msgBuf,'(2A)')
     &   'EEDIE: earlier error in multi-proc/thread setting'
        CALL PRINT_ERROR( msgBuf, 1 )
        fatalError = .TRUE.

      ELSE
C--   Check that all the threads have ended
C     No thread should reach this loop before all threads have set
C     threadIsComplete to TRUE. If they do then either there is a bug
C     in the code or the behaviour of the parallel compiler directives
C     are not right for this code. In the latter case different
C     directives may be available or the compiler itself may have a
C     bug or you may need a different parallel compiler for main.F
        nThreadsDone = 0
        DO I = 1, nThreads
         IF ( threadIsComplete(I) ) nThreadsDone = nThreadsDone+1
        ENDDO
        IF ( nThreadsDone .LT. nThreads ) THEN
         WRITE(msgBuf,'(A,I5,A)')
     &    'S/R EEDIE: Only',nThreadsDone,' threads have completed,'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I5,A)')
     &    'S/R EEDIE:',nThreads,' are expected for this config !'
         CALL PRINT_ERROR( msgBuf, 1 )
         eeEndError = .TRUE.
         fatalError = .TRUE.
        ENDIF

C--   end if/else eebootError
      ENDIF

#ifdef USE_LIBHPM
      CALL F_HPMTERMINATE(myProcId)
#endif

C--   Flush IO-unit before MPI termination
      CALL MDS_FLUSH( errorMessageUnit, 1 )
c#ifdef ALLOW_USE_MPI
      CALL MDS_FLUSH( standardMessageUnit, 1 )
c#endif /* ALLOW_USE_MPI */

#ifdef ALLOW_USE_MPI
C- Note: since MPI_INIT is always called, better to also always terminate MPI
C        (even if usingMPI=F) --> comment out test on usingMPI
c     IF ( usingMPI ) THEN

C--   MPI style multiple-process termination
C--   ======================================
#ifdef COMPONENT_MODULE
       IF ( useCoupler) CALL MPI_BARRIER( MPI_COMM_WORLD, mpiRC )
#endif
#ifdef ALLOW_OASIS
       IF ( useOASIS ) CALL OASIS_FINALIZE
#endif
       CALL MPI_FINALIZE  ( mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeEndError = .TRUE.
        fatalError = .TRUE.
        WRITE(msgBuf,'(A,I5)')
     &       'S/R FIN_PROCS: MPI_FINALIZE return code',
     &       mpiRC
        CALL PRINT_ERROR( msgBuf, 1 )
       ENDIF

c     ENDIF
#endif /* ALLOW_USE_MPI */

      RETURN
      END
