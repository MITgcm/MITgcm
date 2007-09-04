C $Header: /u/gcmpack/MITgcm/eesupp/src/global_sum_tile.F,v 1.1 2007/09/04 14:46:31 jmc Exp $
C $Name:  $

C--   File global_sum_tile.F: Routines that perform global sum
C                             on a tile array
C      Contents
C      o global_sum_tile_rl
C      o global_sum_tile_rs <- not yet coded
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: GLOBAL_SUM_TILE_RL

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_TILE_RL(
     I                       phiTile,
     O                       sumPhi,
     I                       myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_TILE\_RL
C     | o Handle sum for _RL data.
C     *==========================================================*
C     | Apply sum on an array of one value per tile
C     |  and operate over all tiles & all the processes.
C     *==========================================================*

C     !USES:
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     phiTile :: Input array with one value per tile
C     sumPhi  :: Result of sum.
C     myThid  :: My thread id.
      _RL     phiTile(nSx,nSy)
      _RL     sumPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj      :: Loop counters
C     mpiRC  - MPI return code
C- type declaration of: sumMyPr, sumAllP, localBuf and shareBufGSR8 :
C         all 4 needs to have the same length as MPI_DOUBLE_PRECISION
      INTEGER bi,bj
#ifdef ALLOW_USE_MPI
#ifdef GLOBAL_SUM_SEND_RECV
      INTEGER biG, bjG, npe
      INTEGER lbuff, idest, itag, ready_to_receive
      INTEGER istatus(MPI_STATUS_SIZE), ierr
      Real*8  localBuf (nSx,nSy)
      Real*8  globalBuf(nSx*nPx,nSy*nPy)
#else
      INTEGER mpiRC
      Real*8  sumMyPr
#endif
#else /* ALLOW_USE_MPI */
      Real*8  sumMyPr
#endif /* ALLOW_USE_MPI */
      Real*8  sumAllP
CEOP

C- note(jmc): do not see why we need this Barrier here + the one at the end ;
C     however a) both are present in the standard version of global_sum ;
C             b) with OpenMP on hugo, does not work if both are commented out
      CALL BAR2( myThid )

C--   write local sum into shared-buffer array
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         shareBufGSR8(bi,bj) = phiTile(bi,bj)
       ENDDO
      ENDDO

C--   Master thread cannot start until everyone is ready:
      CALL BAR2( myThid )
      _BEGIN_MASTER( myThid )

#if (defined (GLOBAL_SUM_SEND_RECV) && defined (ALLOW_USE_MPI) )

      lbuff = nSx*nSy
      idest = 0
      itag  = 0
      ready_to_receive = 0

      IF ( mpiMyId.NE.0 ) THEN

C--   All proceses except 0 wait to be polled then send local array
#ifndef DISABLE_MPI_READY_TO_RECEIVE
          CALL MPI_RECV (ready_to_receive, 1, MPI_INTEGER,
     &         idest, itag, MPI_COMM_MODEL, istatus, ierr)
#endif
          CALL MPI_SEND (shareBufGSR8, lbuff, MPI_DOUBLE_PRECISION,
     &         idest, itag, MPI_COMM_MODEL, ierr)

C--   All proceses except 0 receive result from process 0
          CALL MPI_RECV (sumAllP, 1, MPI_DOUBLE_PRECISION,
     &         idest, itag, MPI_COMM_MODEL, istatus, ierr)

      ELSE

C--   Process 0 fills-in its local data
        npe = 0
        DO bj=1,nSy
          DO bi=1,nSx
            biG = (mpi_myXGlobalLo(npe+1)-1)/sNx+bi
            bjG = (mpi_myYGlobalLo(npe+1)-1)/sNy+bj
            globalBuf(biG,bjG) = shareBufGSR8(bi,bj)
          ENDDO
        ENDDO

C--   Process 0 polls and receives data from each process in turn
        DO npe = 1, numberOfProcs-1
#ifndef DISABLE_MPI_READY_TO_RECEIVE
          CALL MPI_SEND (ready_to_receive, 1, MPI_INTEGER,
     &           npe, itag, MPI_COMM_MODEL, ierr)
#endif
          CALL MPI_RECV (localBuf, lbuff, MPI_DOUBLE_PRECISION,
     &           npe, itag, MPI_COMM_MODEL, istatus, ierr)

C--   Process 0 gathers the local arrays into a global array.
          DO bj=1,nSy
           DO bi=1,nSx
            biG = (mpi_myXGlobalLo(npe+1)-1)/sNx+bi
            bjG = (mpi_myYGlobalLo(npe+1)-1)/sNy+bj
            globalBuf(biG,bjG) = localBuf(bi,bj)
           ENDDO
          ENDDO
        ENDDO

C--   Sum over all tiles:
        sumAllP = 0.
        DO bjG = 1,nSy*nPy
         DO biG = 1,nSx*nPx
           sumAllP = sumAllP + globalBuf(biG,bjG)
         ENDDO
        ENDDO

C--   Process 0 sends result to all other processes
        lbuff = 1
        DO npe = 1, numberOfProcs-1
          CALL MPI_SEND (sumAllP, 1, MPI_DOUBLE_PRECISION,
     &                   npe, itag, MPI_COMM_MODEL, ierr)
        ENDDO


      ENDIF

#else /* not (GLOBAL_SUM_SEND_RECV & ALLOW_USE_MPI) */

C--   Sum over all tiles (of the same process) first
       sumMyPr = 0.
       DO bj = 1,nSy
        DO bi = 1,nSx
          sumMyPr = sumMyPr + shareBufGSR8(bi,bj)
        ENDDO
       ENDDO

C      in case MPI is not used:
       sumAllP = sumMyPr

#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(sumMyPr,sumAllP,1,MPI_DOUBLE_PRECISION,
     &                     MPI_SUM,MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /* ALLOW_USE_MPI */

#endif /* not (GLOBAL_SUM_SEND_RECV & ALLOW_USE_MPI) */

C--    Write solution to shared buffer (all threads can see it)
       shareBufGSR8(1,1) = sumAllP

      _END_MASTER( myThid )
C--   Everyone wait for Master thread to be ready
      CALL BAR2( myThid )

C--   set result for every threads
      sumPhi = shareBufGSR8(1,1)

C- note(jmc): do not see why we need this Barrier here (see comment @ the top)
      CALL BAR2( myThid )

      RETURN
      END
