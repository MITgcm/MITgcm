C $Header: /u/gcmpack/MITgcm/eesupp/src/global_sum.F,v 1.8 2001/02/04 14:38:43 cnh Exp $
C $Name:  $

C--   File global_sum.F: Routines that perform global sum on an array
C                        of thread values.
C      Contents
C      o global_sum_r4
C      o global_sum_r8
#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE GLOBAL_SUM_R4( 
     U                       sumPhi,
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_SUM_R4                                 |
C     | o Handle sum for real*4 data.                            |
C     |==========================================================|
C     | Perform sum an array of one value per thread and then    |
C     | sum result of all the processes.                         |
C     | Notes                                                    |
C     | =====                                                    |
C     | Within a process only one thread does the sum, each      |
C     | thread is assumed to have already summed its local data. |
C     | The same thread also does the inter-process sum for      |
C     | example with MPI and then writes the result into a shared|
C     | location. All threads wait until the sum is avaiailable. |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     == Routine arguments ==
C     sumPhi - Result of sum.
C     myThid - My thread id.
      Real*4 sumPhi
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     phi    - Array to be summed.
C     I      - Loop counters
C     mpiRC  - MPI return code
      Real*4 phi(lShare4,MAX_NO_THREADS)
      INTEGER I
      Real*4  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   write local sum into array
      phi(1,myThid) = sumPhi

C--   Can not start until everyone is ready
      _BARRIER

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0.
       DO I=1,nThreads
        tmp = tmp + phi(1,I)
       ENDDO
       sumPhi = tmp
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp,sumPhi,1,MPI_REAL,MPI_SUM,
     &                   MPI_COMM_WORLD,mpiRC)
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
       phi(1,1) = sumPhi

      _END_MASTER( myThid )
C--
      _BARRIER

C--   set result for every process
      sumPhi = phi(1,1)

      RETURN
      END
      

CStartOfInterface
      SUBROUTINE GLOBAL_SUM_R8( 
     U                       sumPhi,
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_SUM_R8                                 |
C     | o Handle sum for real*8 data.                            |
C     |==========================================================|
C     | Perform sum an array of one value per thread and then    |
C     | sum result of all the processes.                         |
C     | Notes                                                    |
C     | =====                                                    |
C     | Within a process only one thread does the sum, each      |
C     | thread is assumed to have already summed its local data. |
C     | The same thread also does the inter-process sum for      |
C     | example with MPI and then writes the result into a shared|
C     | location. All threads wait until the sum is avaiailable. |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     sumPhi - Result of sum.
C     myThid - My thread id.
      Real*8 sumPhi
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     phi    - Array to be summed.
C     I      - Loop counters
C     mpiRC  - MPI return code
      Real*8 phi(lShare8,MAX_NO_THREADS)
      INTEGER I
      Real*8  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */

C--   write local sum into array
      phi(1,myThid) = sumPhi

C--   Can not start until everyone is ready
      _BARRIER

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0. _d 0
       DO I=1,nThreads
        tmp = tmp + phi(1,I)
       ENDDO
       sumPhi = tmp
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp,sumPhi,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &                   MPI_COMM_WORLD,mpiRC)
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
        phi(1,1) = sumPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the sum is done
      _BARRIER

C--   set result for every process
      sumPhi = phi(1,1)

      RETURN
      END
CStartOfInterface
      SUBROUTINE GLOBAL_SUM_INT( 
     U                       sumPhi,
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_SUM_INT                                |
C     | o Handle sum for integer data.                           |
C     |==========================================================|
C     | Perform sum an array of one value per thread and then    |
C     | sum result of all the processes.                         |
C     | Notes                                                    |
C     | =====                                                    |
C     | Within a process only one thread does the sum, each      |
C     | thread is assumed to have already summed its local data. |
C     | The same thread also does the inter-process sum for      |
C     | example with MPI and then writes the result into a shared|
C     | location. All threads wait until the sum is avaiailable. |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     sumPhi - Result of sum.
C     myThid - My thread id.
      INTEGER sumPhi
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     phi    - Array to be summed.
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER phi(lShare8,MAX_NO_THREADS)
      INTEGER I
      INTEGER  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */

C--   write local sum into array
      phi(1,myThid) = sumPhi

C--   Can not start until everyone is ready
      _BARRIER

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0. _d 0
       DO I=1,nThreads
        tmp = tmp + phi(1,I)
       ENDDO
       sumPhi = tmp
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp,sumPhi,1,MPI_INTEGER,MPI_SUM,
     &                   MPI_COMM_WORLD,mpiRC)
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
        phi(1,1) = sumPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the sum is done
      _BARRIER

C--   set result for every process
      sumPhi = phi(1,1)

      RETURN
      END
