C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_cumulsum_z_tile.F,v 1.2 2011/09/21 16:27:42 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C--   File w2_cumulsum_z_tile.F: Routines that perform cumulated sum
C                                on a tiled array, corner grid-cell location
C      Contents
C      o W2_CUMULSUM_Z_TILE_RL
C      o W2_CUMULSUM_Z_TILE_RS <- not yet coded

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: W2_CUMULSUM_Z_TILE_RL

C     !INTERFACE:
      SUBROUTINE W2_CUMULSUM_Z_TILE_RL(
     O                       psiZ, psiLoc,
     I                       dPsiX, dPsiY, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE W2\_CUMULSUM\_Z\_TILE\_RL
C     | o Handle cumulated sum for _RL tile data.
C     *==========================================================*
C     | Cumulate sum on tiled array, corner grid-cell location:
C     |  Starts from 1rst tile and, going through all tiles & all
C     |  the processes, add increment in both directions
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "CUMULSUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     psiZ    :: results of cumulated sum, corresponds to tile South-East corner
C     psiLoc  :: cumulated sum at special locations
C     dPsiX   :: tile increment in X direction
C     dPsiY   :: tile increment in Y direction
C     myThid  :: my Thread Id. number
      _RL     psiZ  (nSx,nSy)
      _RL     psiLoc(2)
      _RL     dPsiX (nSx,nSy)
      _RL     dPsiY (nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C- type declaration of: loc[1,2]Buf and shareBufCS[1,2]_R8 :
C         all 4 needs to have the same length as MPI_DOUBLE_PRECISION
      INTEGER bi,bj
      INTEGER tN, tS
      Real*8  globalBuf(3,W2_maxNbTiles)
#ifdef ALLOW_USE_MPI
      INTEGER npe, np1
      INTEGER lbuf1, lbuf2, idest, itag, ready_to_receive
      INTEGER istatus(MPI_STATUS_SIZE), ierr
      Real*8  loc1Buf  (nSx,nSy)
      Real*8  loc2Buf(2,nSx,nSy)
#endif /* ALLOW_USE_MPI */
CEOP

C--   Initialise to zero:
      psiLoc(1) = 0.
      psiLoc(2) = 0.
      DO tN = 1,exch2_nTiles
        globalBuf(1,tN) = 0.
        globalBuf(2,tN) = 0.
        globalBuf(3,tN) = 0.
      ENDDO

C--   write input into shared-buffer array
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         shareBufCS2_R8(1,bi,bj) = dPsiX(bi,bj)
         shareBufCS2_R8(2,bi,bj) = dPsiY(bi,bj)
       ENDDO
      ENDDO

C--   Master thread cannot start until everyone is ready:
      CALL BAR2( myThid )
      _BEGIN_MASTER( myThid )

#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif

       lbuf1 = nSx*nSy
       lbuf2 = 2*lbuf1
       idest = 0
       itag  = 0
       ready_to_receive = 0

       IF ( mpiMyId.NE.0 ) THEN

C--   All proceses except 0 wait to be polled then send local array
#ifndef DISABLE_MPI_READY_TO_RECEIVE
           CALL MPI_RECV (ready_to_receive, 1, MPI_INTEGER,
     &                    idest, itag, MPI_COMM_MODEL, istatus, ierr)
#endif
           CALL MPI_SEND (shareBufCS2_R8, lbuf2, MPI_DOUBLE_PRECISION,
     &                    idest, itag, MPI_COMM_MODEL, ierr)

C--   All proceses except 0 receive result from process 0
           CALL MPI_RECV (shareBufCS1_R8, lbuf1, MPI_DOUBLE_PRECISION,
     &                    idest, itag, MPI_COMM_MODEL, istatus, ierr)

       ELSE

C--   Process 0 polls and receives data from each process in turn
         DO npe = 1, numberOfProcs-1
#ifndef DISABLE_MPI_READY_TO_RECEIVE
           CALL MPI_SEND (ready_to_receive, 1, MPI_INTEGER,
     &                    npe, itag, MPI_COMM_MODEL, ierr)
#endif
           CALL MPI_RECV (loc2Buf, lbuf2, MPI_DOUBLE_PRECISION,
     &                    npe, itag, MPI_COMM_MODEL, istatus, ierr)

C--   Process 0 gathers the local arrays into a global array.
           np1 = npe + 1
           DO bj=1,nSy
            DO bi=1,nSx
              tN = W2_procTileList(bi,bj,np1)
              globalBuf(1,tN) = loc2Buf(1,bi,bj)
              globalBuf(2,tN) = loc2Buf(2,bi,bj)
            ENDDO
           ENDDO
         ENDDO

C--   end if process not 0 / else = 0
       ENDIF

#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /* ALLOW_USE_MPI */

      IF ( myProcId.EQ.0 ) THEN

C--   Process 0 fills-in its local data
         DO bj=1,nSy
          DO bi=1,nSx
            tN = W2_myTileList(bi,bj)
            globalBuf(1,tN) = shareBufCS2_R8(1,bi,bj)
            globalBuf(2,tN) = shareBufCS2_R8(2,bi,bj)
          ENDDO
         ENDDO

C--   Cumulate Sum over all tiles:
#ifdef W2_CUMSUM_USE_MATRIX
         DO tN = 1,exch2_nTiles
           globalBuf(3,tN) = 0.
           DO tS = 1,exch2_nTiles
             globalBuf(3,tN) = globalBuf(3,tN)
     &                       + W2_cumSum_tiles(1,tS,tN)*globalBuf(1,tS)
     &                       + W2_cumSum_tiles(2,tS,tN)*globalBuf(2,tS)
           ENDDO
         ENDDO
#else /* W2_CUMSUM_USE_MATRIX */
         STOP 'ABNORMAL END: missing code in S/R W2_CUMULSUM_Z_TILE'
#endif /* W2_CUMSUM_USE_MATRIX */

C-    Value at Special location (e.g., Missing-Corner values)
         IF ( W2_tMC1.GE.1 )
     &     psiLoc(1) = globalBuf(3,W2_tMC1) + globalBuf(2,W2_tMC1)
         IF ( W2_tMC2.GE.1 )
     &     psiLoc(2) = globalBuf(3,W2_tMC2) + globalBuf(1,W2_tMC2)

C--   Process 0 fills-in its local data
         DO bj=1,nSy
          DO bi=1,nSx
            tN = W2_myTileList(bi,bj)
            shareBufCS1_R8(bi,bj) = globalBuf(3,tN)
          ENDDO
         ENDDO

#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
        IF ( usingMPI ) THEN
#endif
C--   Process 0 sends result to all other processes
         DO npe = 1, numberOfProcs-1
C-    fill local array with relevant portion of global array
           np1 = npe + 1
           DO bj=1,nSy
            DO bi=1,nSx
              tN = W2_procTileList(bi,bj,np1)
              loc1Buf(bi,bj) = globalBuf(3,tN)
            ENDDO
           ENDDO
           CALL MPI_SEND (loc1Buf, lbuf1, MPI_DOUBLE_PRECISION,
     &                    npe, itag, MPI_COMM_MODEL, ierr)
         ENDDO

#ifndef ALWAYS_USE_MPI
        ENDIF
#endif
#endif /* ALLOW_USE_MPI */

C--   end if process 0
      ENDIF

      _END_MASTER( myThid )
C--   Everyone wait for Master thread to be ready
      CALL BAR2( myThid )

C--   set result for every threads
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         psiZ(bi,bj) = shareBufCS1_R8(bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
