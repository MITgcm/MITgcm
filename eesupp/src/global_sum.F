C $Header: /u/gcmpack/MITgcm/eesupp/src/global_sum.F,v 1.16 2012/09/06 15:25:01 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C--   File global_sum.F: Routines that perform global sum on an array
C                        of thread values.
C      Contents
C      o GLOBAL_SUM_R4
C      o GLOBAL_SUM_R8
C      o GLOBAL_SUM_INT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_R4

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_R4(
     U                       sumPhi,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_R4
C     | o Handle sum for real*4 data.
C     *==========================================================*
C     | Perform sum an array of one value per thread and then
C     | sum result of all the processes.
C     | Notes:
C     | Within a process only one thread does the sum.
C     | The same thread also does the inter-process sum for
C     | example with MPI and then writes the result into a shared
C     | location. All threads wait until the sum is avaiailable.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     sumPhi :: Result of sum.
C     myThid :: My thread id.
      Real*4 sumPhi
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

C--   write local sum into array
      phiGSR4(1,myThid) = sumPhi

C--   Can not start until everyone is ready
      CALL BAR2( myThid )

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0.
       DO I=1,nThreads
        tmp = tmp + phiGSR4(1,I)
       ENDDO
       sumPhi = tmp
#ifdef ALLOW_USE_MPI
       IF ( usingMPI ) THEN
        CALL MPI_Allreduce(tmp,sumPhi,1,MPI_REAL,MPI_SUM,
     &                   MPI_COMM_MODEL,mpiRC)
       ENDIF
#endif /* ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
       phiGSR4(1,0) = sumPhi

      _END_MASTER( myThid )
C--
      CALL BAR2( myThid )

C--   set result for every process
      sumPhi = phiGSR4(1,0)

      RETURN
      END


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_R8

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_R8(
     U                       sumPhi,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_R8
C     | o Handle sum for real*8 data.
C     *==========================================================*
C     | Perform sum an array of one value per thread and then
C     | sum result of all the processes.
C     | Notes:
C     | Within a process only one thread does the sum.
C     | The same thread also does the inter-process sum for
C     | example with MPI and then writes the result into a shared
C     | location. All threads wait until the sum is avaiailable.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     sumPhi :: Result of sum.
C     myThid :: My thread id.
      Real*8 sumPhi
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

C--   write local sum into array
      phiGSR8(1,myThid) = sumPhi

C--   Can not start until everyone is ready
C     CALL FOOL_THE_COMPILER( phiGSR8 )
C     CALL MS
      CALL BAR2( myThid )
C     _BARRIER
C     _BARRIER
C     CALL FOOL_THE_COMPILER( phiGSR8 )

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0. _d 0
       DO I=1,nThreads
        tmp = tmp + phiGSR8(1,I)
       ENDDO
       sumPhi = tmp
#ifdef ALLOW_USE_MPI
       IF ( usingMPI ) THEN
        CALL MPI_Allreduce(tmp,sumPhi,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &                   MPI_COMM_MODEL,mpiRC)
       ENDIF
#endif /* ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
       phiGSR8(1,0) = sumPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the sum is done
C     CALL FOOL_THE_COMPILER( phiGSR8 )
C     CALL MS
C     _BARRIER
C     _BARRIER
      CALL BAR2( myThid )
C     CALL FOOL_THE_COMPILER( phiGSR8 )

C--   set result for every process
      sumPhi = phiGSR8(1,0)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_INT
C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_INT(
     U                       sumPhi,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_INT
C     | o Handle sum for integer data.
C     *==========================================================*
C     | Perform sum an array of one value per thread and then
C     | sum result of all the processes.
C     | Notes:
C     | Within a process only one thread does the sum.
C     | The same thread also does the inter-process sum for
C     | example with MPI and then writes the result into a shared
C     | location. All threads wait until the sum is avaiailable.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     sumPhi :: Result of sum.
C     myThid :: My thread id.
      INTEGER sumPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     I      :: Loop counters
C     mpiRC  :: MPI return code
      INTEGER I
      INTEGER  tmp
#ifdef ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */
CEOP

C--   write local sum into array
      phiGSI(1,myThid) = sumPhi

C--   Can not start until everyone is ready
      _BARRIER

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0
       DO I=1,nThreads
        tmp = tmp + phiGSI(1,I)
       ENDDO
       sumPhi = tmp
#ifdef ALLOW_USE_MPI
       IF ( usingMPI ) THEN
        CALL MPI_Allreduce(tmp,sumPhi,1,MPI_INTEGER,MPI_SUM,
     &                   MPI_COMM_MODEL,mpiRC)
       ENDIF
#endif /* ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
       phiGSI(1,0) = sumPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the sum is done
      _BARRIER

C--   set result for every process
      sumPhi = phiGSI(1,0)

      RETURN
      END
