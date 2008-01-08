C $Header: /u/gcmpack/MITgcm/eesupp/src/global_sum_singlecpu.F,v 1.1 2008/01/08 23:53:47 jahn Exp $
C $Name:  $

C--   File global_sum_singlecpu.F: Routines that perform global sum
C                             on a single CPU
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: GLOBAL_SUM_SINGLECPU_RL

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_SINGLECPU_RL(
     I                       phiLocal,
     O                       sumPhi,
     I                       myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_SINGLECPU\_RL
C     | o Handle sum for _RL data.
C     *==========================================================*
C     | Global sum of 2d array
C     | independent of tiling as sum is performed on a single CPU
C     | sum is performed in REAL*8
C     *==========================================================*

C     !USES:
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     phiLocal :: local input array without overlap regions.
C     sumPhi   :: Result of sum.
C     myThid   :: My thread id.
      _RL    phiLocal(1:sNx,1:sNy,nSx,nSy)
      _RL     sumPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C- type declaration of: sumAll, globalBuf :
C         sumAll needs to have the same length as MPI_DOUBLE_PRECISION
      INTEGER I,J
      Real*8  globalBuf(Nx,Ny)
      Real*8  sumAll
#ifdef ALLOW_USE_MPI
      INTEGER npe
      INTEGER lbuff, idest, itag
      INTEGER istatus(MPI_STATUS_SIZE), ierr
#endif /* ALLOW_USE_MPI */
CEOP

C- note(jmc): do not see why we need this Barrier here + the one at the end ;
C     however a) both are present in the standard version of global_sum ;
C             b) with OpenMP on hugo, does not work if both are commented out
      CALL BAR2( myThid )

C--   Gather local arrays
      CALL GATHER_2D( globalBuf, phiLocal, myThid )

C--   Master thread does the communications and the global sum
C--   Master thread cannot start until everyone is ready:
      CALL BAR2( myThid )
      _BEGIN_MASTER( myThid )

#ifdef ALLOW_USE_MPI
       idest = 0
       itag  = 0

       IF ( mpiMyId.EQ.0 ) THEN
#endif

C--   Process 0 sums the global array
	sumAll = 0.d0
	DO J=1,Ny
	 DO I=1,Nx
	  sumAll = sumAll + globalBuf(I,J)
	 ENDDO
	ENDDO

#ifdef ALLOW_USE_MPI
C--   Process 0 sends result to all other processes
        lbuff = 1
        DO npe = 1, numberOfProcs-1
          CALL MPI_SEND (sumAll, 1, MPI_DOUBLE_PRECISION,
     &                   npe, itag, MPI_COMM_MODEL, ierr)
        ENDDO

       ELSE
      
C--   All proceses except 0 receive result from process 0
          CALL MPI_RECV (sumAll, 1, MPI_DOUBLE_PRECISION,
     &         idest, itag, MPI_COMM_MODEL, istatus, ierr)

       ENDIF
#endif /* not ALLOW_USE_MPI */

C--   Write solution to shared buffer (all threads can see it)
      shareBufGSR8(1,1) = sumAll

      _END_MASTER( myThid )
C--   Everyone wait for Master thread to be ready
      CALL BAR2( myThid )

C--   set result for every threads
      sumPhi = shareBufGSR8(1,1)

C- note(jmc): do not see why we need this Barrier here (see comment @ the top)
      CALL BAR2( myThid )

      RETURN
      END
