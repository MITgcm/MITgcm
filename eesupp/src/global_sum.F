C $Header: /u/gcmpack/MITgcm/eesupp/src/global_sum.F,v 1.10 2001/09/21 03:54:35 cnh Exp $
C $Name:  $

C--   File global_sum.F: Routines that perform global sum on an array
C                        of thread values.
C      Contents
C      o global_sum_r4
C      o global_sum_r8
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: GLOBAL_SUM_R4

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_R4( 
     U                       sumPhi,
     I                       myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_SUM_R4                                  
C     | o Handle sum for real*4 data.                             
C     *==========================================================*
C     | Perform sum an array of one value per thread and then     
C     | sum result of all the processes.                          
C     | Notes                                                     
C     | =====                                                     
C     | Within a process only one thread does the sum, each       
C     | thread is assumed to have already summed its local data.  
C     | The same thread also does the inter-process sum for       
C     | example with MPI and then writes the result into a shared 
C     | location. All threads wait until the sum is avaiailable.  
C     *==========================================================*

C     !USES:
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     sumPhi - Result of sum.
C     myThid - My thread id.
      Real*4 sumPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER I
      Real*4  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */
CEOP

C--   write local sum into array
      CALL BAR2( myThid )
      phiGSRS(1,myThid) = sumPhi

C--   Can not start until everyone is ready
      CALL BAR2( myThid )

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0.
       DO I=1,nThreads
        tmp = tmp + phiGSRS(1,I)
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
       phiGSRS(1,1) = sumPhi

      _END_MASTER( myThid )
C--
      CALL BAR2( myThid )

C--   set result for every process
      sumPhi = phiGSRS(1,1)
      CALL BAR2( myThid )

      RETURN
      END
      

CBOP
C     !ROUTINE: GLOBAL_SUM_R8

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_R8( 
     U                       sumPhi,
     I                       myThid )
      IMPLICIT NONE 
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_SUM_R8                                  
C     | o Handle sum for real*8 data.                             
C     *==========================================================*
C     | Perform sum an array of one value per thread and then     
C     | sum result of all the processes.                          
C     | Notes                                                     
C     | =====                                                     
C     | Within a process only one thread does the sum, each       
C     | thread is assumed to have already summed its local data.  
C     | The same thread also does the inter-process sum for       
C     | example with MPI and then writes the result into a shared 
C     | location. All threads wait until the sum is avaiailable.  
C     *==========================================================*

C     !USES:
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     sumPhi - Result of sum.
C     myThid - My thread id.
      Real*8 sumPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER I
      Real*8  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */
CEOP

      CALL BAR2( myThid )
C--   write local sum into array
      phiGSRL(1,myThid) = sumPhi

C--   Can not start until everyone is ready
C     CALL FOOL_THE_COMPILER( phiGSRL )
C     CALL MS
      CALL BAR2( myThid )
C     _BARRIER
C     _BARRIER
C     CALL FOOL_THE_COMPILER( phiGSRL )

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0. _d 0
       DO I=1,nThreads
        tmp = tmp + phiGSRL(1,I)
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
        phiGSRL(1,1) = sumPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the sum is done
C     CALL FOOL_THE_COMPILER( phiGSRL )
C     CALL MS
C     _BARRIER
C     _BARRIER
      CALL BAR2( myThid )
C     CALL FOOL_THE_COMPILER( phiGSRL )

C--   set result for every process
      sumPhi = phiGSRL(1,1)
      CALL BAR2( myThid )

      RETURN
      END

CBOP
C     !ROUTINE: GLOBAL_SUM_INT
C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_INT( 
     U                       sumPhi,
     I                       myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_SUM_INT                                 
C     | o Handle sum for integer data.                            
C     *==========================================================*
C     | Perform sum an array of one value per thread and then     
C     | sum result of all the processes.                          
C     | Notes                                                     
C     | =====                                                     
C     | Within a process only one thread does the sum, each       
C     | thread is assumed to have already summed its local data.  
C     | The same thread also does the inter-process sum for       
C     | example with MPI and then writes the result into a shared 
C     | location. All threads wait until the sum is avaiailable.  
C     *==========================================================*

C     !USES:
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     sumPhi - Result of sum.
C     myThid - My thread id.
      INTEGER sumPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER I
      INTEGER  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */
CEOP

      _BARRIER
C--   write local sum into array
      phiGSI(1,myThid) = sumPhi

C--   Can not start until everyone is ready
      _BARRIER

C--   Sum within the process first
      _BEGIN_MASTER( myThid )
       tmp = 0. _d 0
       DO I=1,nThreads
        tmp = tmp + phiGSI(1,I)
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
        phiGSI(1,1) = sumPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the sum is done
      _BARRIER

C--   set result for every process
      sumPhi = phiGSI(1,1)
      _BARRIER

      RETURN
      END
