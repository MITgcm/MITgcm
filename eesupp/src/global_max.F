C $Header: /u/gcmpack/MITgcm/eesupp/src/global_max.F,v 1.13 2012/09/06 15:25:01 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C--   File global_max.F: Routines that perform global max reduction on an array
C                        of thread values.
C      Contents
C      o GLOBAL_MAX_R4
C      o GLOBAL_MAX_R8

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_MAX_R4

C     !INTERFACE:
      SUBROUTINE GLOBAL_MAX_R4(
     U                       maxPhi,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_MAX\_R4
C     | o Handle max for real*4 data.
C     *==========================================================*
C     | Perform max on array of one value per thread and then
C     | max result of all the processes.
C     | Notes
C     | =====
C     | Within a process only one thread does the max, each
C     | thread is assumed to have already maxed  its local data.
C     | The same thread also does the inter-process max for
C     | example with MPI and then writes the result into a shared
C     | location. All threads wait until the max is avaiailable.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_MAX.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     maxPhi :: Result of max.
C     myThid :: My thread id.
      Real*4 maxPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I      :: Loop counters
C     mpiRC  :: MPI return code
      INTEGER I
      Real*4  tmp
#ifdef ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */
CEOP

C--   write local max into array
      phiGMR4(1,myThid) = maxPhi

C--   Can not start until everyone is ready
      CALL BAR2( myThid )

C--   Max within the process first
      _BEGIN_MASTER( myThid )
       tmp = phiGMR4(1,1)
       DO I=2,nThreads
        tmp = MAX(tmp,phiGMR4(1,I))
       ENDDO
       maxPhi = tmp
#ifdef ALLOW_USE_MPI
       IF ( usingMPI ) THEN
        CALL MPI_Allreduce(tmp,maxPhi,1,MPI_REAL,MPI_MAX,
     &                   MPI_COMM_MODEL,mpiRC)
       ENDIF
#endif /* ALLOW_USE_MPI */
       phiGMR4(1,0) = maxPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the max is done
      CALL BAR2( myThid )

C--   set result for every process
      maxPhi = phiGMR4(1,0)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_MAX_R8

C     !INTERFACE:
      SUBROUTINE GLOBAL_MAX_R8(
     O                       maxPhi,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_MAX\_R8
C     | o Handle max for real*8 data.
C     *==========================================================*
C     | Perform max on array of one value per thread and then
C     | max result of all the processes.
C     | Notes
C     | =====
C     | Within a process only one thread does the max, each
C     | thread is assumed to have already maxed  its local data.
C     | The same thread also does the inter-process max for
C     | example with MPI and then writes the result into a shared
C     | location. All threads wait until the max is avaiailable.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_MAX.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     maxPhi :: Result of max.
C     myThid :: My thread id.
      Real*8 maxPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     I      :: Loop counters
C     mpiRC  :: MPI return code
      INTEGER I
      Real*8  tmp
#ifdef ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */
CEOP

C--   write local max into array
      phiGMR8(1,myThid) = maxPhi

C--   Can not start until everyone is ready
      CALL BAR2( myThid )

C--   Max within the process first
      _BEGIN_MASTER( myThid )
       tmp = phiGMR8(1,1)
       DO I=2,nThreads
        tmp = MAX(tmp,phiGMR8(1,I))
       ENDDO
       maxPhi = tmp
#ifdef ALLOW_USE_MPI
       IF ( usingMPI ) THEN
        CALL MPI_Allreduce(tmp,maxPhi,1,MPI_DOUBLE_PRECISION,MPI_MAX,
     &                   MPI_COMM_MODEL,mpiRC)
       ENDIF
#endif /* ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
       phiGMR8(1,0) = maxPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the max is done
      CALL BAR2( myThid )

C--   set result for every process
      maxPhi = phiGMR8(1,0)

      RETURN
      END
