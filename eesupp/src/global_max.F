C $Header: /u/gcmpack/MITgcm/eesupp/src/global_max.F,v 1.9 2001/09/21 03:54:35 cnh Exp $
C $Name:  $

C--   File global_max.F: Routines that perform global max reduction on an array
C                        of thread values.
C      Contents
C      o global_max_r4
C      o global_max_r8
#include "CPP_EEOPTIONS.h"

CBOP

C     !ROUTINE: GLOBAL_MAX_R4

C     !INTERFACE:
      SUBROUTINE GLOBAL_MAX_R4( 
     U                       maxPhi,
     I                       myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_MAX_R4                                  
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
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */
CEOP

      CALL BAR2( myThid )
C--   write local max into array
      phiGMRS(1,myThid) = maxPhi

C--   Can not start until everyone is ready
      CALL BAR2( myThid )

C--   Max within the process first
      _BEGIN_MASTER( myThid )
       tmp = phiGMRS(1,1)
       DO I=2,nThreads
        tmp = MAX(tmp,phiGMRS(1,I))
       ENDDO
       maxPhi = tmp
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp,maxPhi,1,MPI_REAL,MPI_MAX,
     &                   MPI_COMM_WORLD,mpiRC)
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
       phiGMRS(1,1) = maxPhi
      _END_MASTER( myThid )
C--
      CALL BAR2( myThid )

C--   set result for every process
      maxPhi = phiGMRS(1,1)
      CALL BAR2( myThid )

      RETURN
      END
      
CBOP

C     !ROUTINE: GLOBAL_MAX_R8

C     !INTERFACE:
      SUBROUTINE GLOBAL_MAX_R8( 
     O                       maxPhi,
     I                       myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_MAX_R8                                  
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
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */
CEOP

      CALL BAR2( myThid )
C--   write local max into array
      phiGMRL(1,myThid) = maxPhi

C--   Can not start until everyone is ready
      CALL BAR2( myThid )

C--   Max within the process first
      _BEGIN_MASTER( myThid )
       tmp = phiGMRL(1,1)
       DO I=2,nThreads
        tmp = MAX(tmp,phiGMRL(1,I))
       ENDDO
       maxPhi = tmp
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp,maxPhi,1,MPI_DOUBLE_PRECISION,MPI_MAX,
     &                   MPI_COMM_WORLD,mpiRC)
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
C--     Write solution to place where all threads can see it
        phiGMRL(1,1) = maxPhi
      _END_MASTER( myThid )

C--   Do not leave until we are sure that the max is done
      CALL BAR2( myThid )

C--   set result for every process
      maxPhi = phiGMRL(1,1)
      CALL BAR2( myThid )

      RETURN
      END
