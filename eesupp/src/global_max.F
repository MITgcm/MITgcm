C $Header: /u/gcmpack/MITgcm/eesupp/src/global_max.F,v 1.5 1999/03/22 17:37:43 adcroft Exp $

C--   File global_max.F: Routines that perform global max reduction on an array
C                        of thread values.
C      Contents
C      o global_max_r4
C      o global_max_r8
#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE GLOBAL_MAX_R4( 
     I                       phi, 
     O                       maxPhi,
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_MAX_R4                                 |
C     | o Handle max for real*4 data.                            |
C     |==========================================================|
C     | Perform max on array of one value per thread and then    |
C     | max result of all the processes.                         |
C     | Notes                                                    |
C     | =====                                                    |
C     | Within a process only one thread does the max, each      |
C     | thread is assumed to have already maxed  its local data. |
C     | The same thread also does the inter-process max for      |
C     | example with MPI and then writes the result into a shared|
C     | location. All threads wait until the max is avaiailable. |
C     \==========================================================/

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     == Routine arguments ==
C     phi    - Array to be maxed.
C     maxPhi - Result of max.
C     myThid - My thread id.
      Real*4 phi(lShare4,MAX_NO_THREADS)
      Real*4 maxPhi
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER I
      Real*4  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Can not start until everyone is ready
      _BARRIER

C--   Max within the process first
      _BEGIN_MASTER( myThid )
       tmp = phi(1,1)
       DO I=2,nThreads
        tmp = MAX(tmp,phi(1,I))
       ENDDO
       maxPhi = tmp
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp,maxPhi,1,MPI_REAL,MPI_MAX,
     &                   MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
       phi(1,1) = maxPhi
      _END_MASTER( myThid )
C--
      _BARRIER
C
      RETURN
      END
      

CStartOfInterface
      SUBROUTINE GLOBAL_MAX_R8( 
     I                       phi, 
     O                       maxPhi,
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_MAX_R8                                 |
C     | o Handle max for real*8 data.                            |
C     |==========================================================|
C     | Perform max on array of one value per thread and then    |
C     | max result of all the processes.                         |
C     | Notes                                                    |
C     | =====                                                    |
C     | Within a process only one thread does the max, each      |
C     | thread is assumed to have already maxed  its local data. |
C     | The same thread also does the inter-process max for      |
C     | example with MPI and then writes the result into a shared|
C     | location. All threads wait until the max is avaiailable. |
C     \==========================================================/

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array to be maxed. 
C     maxPhi - Result of max.
C     myThid - My thread id.
      Real*8 phi(lShare8,MAX_NO_THREADS)
      Real*8 maxPhi
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER I
      Real*8  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */

C--   Can not start until everyone is ready
      _BARRIER

C--   Max within the process first
      _BEGIN_MASTER( myThid )
       tmp = phi(1,1)
       DO I=2,nThreads
        tmp = MAX(tmp,phi(1,I))
       ENDDO
       maxPhi = tmp
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp,maxPhi,1,MPI_DOUBLE_PRECISION,MPI_MAX,
     &                   MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
        phi(1,1) = maxPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the max is done
      _BARRIER
C
      RETURN
      END
