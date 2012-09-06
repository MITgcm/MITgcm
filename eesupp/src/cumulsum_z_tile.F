C $Header: /u/gcmpack/MITgcm/eesupp/src/cumulsum_z_tile.F,v 1.3 2012/09/06 15:25:01 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

C--   File cumulsum_z_tile.F: Routines that perform cumulated sum
C                             on a tiled array, corner grid-cell location
C      Contents
C      o CUMULSUM_Z_TILE_RL
C      o CUMULSUM_Z_TILE_RS <- not yet coded

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CUMULSUM_Z_TILE_RL

C     !INTERFACE:
      SUBROUTINE CUMULSUM_Z_TILE_RL(
     O                       psiZ, psiLoc,
     I                       dPsiX, dPsiY, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CUMULSUM\_Z\_TILE\_RL
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
#ifndef ALLOW_EXCH2
C     == Local variables ==
C     bi,bj   :: tile indices
C- type declaration of: loc[1,2]Buf and shareBufCS[1,2]_R8 :
C         all 4 needs to have the same length as MPI_DOUBLE_PRECISION
      INTEGER bi,bj
      INTEGER nf
#ifdef ALLOW_USE_MPI
      INTEGER biG, bjG, npe, np1
      INTEGER lbuf1, lbuf2, idest, itag, ready_to_receive
      INTEGER istatus(MPI_STATUS_SIZE), ierr
      Real*8  loc1Buf  (nSx,nSy)
      Real*8  loc2Buf(2,nSx,nSy)
      Real*8  globalBuf(3,nSx*nPx,nSy*nPy)
#endif /* ALLOW_USE_MPI */
#endif /* ALLOW_EXCH2 */
CEOP

#ifdef ALLOW_EXCH2
      CALL W2_CUMULSUM_Z_TILE_RL(
     O                       psiZ, psiLoc,
     I                       dPsiX, dPsiY, myThid )

#else /* ALLOW_EXCH2 */
C--   write input into shared-buffer array
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         shareBufCS2_R8(1,bi,bj) = dPsiX(bi,bj)
         shareBufCS2_R8(2,bi,bj) = dPsiY(bi,bj)
       ENDDO
      ENDDO
      psiLoc(1) = 0.
      psiLoc(2) = 0.

C--   Master thread cannot start until everyone is ready:
      CALL BAR2( myThid )
      _BEGIN_MASTER( myThid )

#ifdef ALLOW_USE_MPI
      IF ( usingMPI ) THEN

        lbuf1 = nSx*nSy
        lbuf2 = 2*lbuf1
        idest = 0
        itag  = 0
        ready_to_receive = 0

        IF ( mpiMyId.NE.0 ) THEN

C--   All proceses except 0 wait to be polled then send local array
#ifndef DISABLE_MPI_READY_TO_RECEIVE
            CALL MPI_RECV (ready_to_receive, 1, MPI_INTEGER,
     &           idest, itag, MPI_COMM_MODEL, istatus, ierr)
#endif
            CALL MPI_SEND (shareBufCS2_R8, lbuf2, MPI_DOUBLE_PRECISION,
     &           idest, itag, MPI_COMM_MODEL, ierr)

C--   All proceses except 0 receive result from process 0
            CALL MPI_RECV (shareBufCS1_R8, lbuf1, MPI_DOUBLE_PRECISION,
     &           idest, itag, MPI_COMM_MODEL, istatus, ierr)

        ELSE

C--   Process 0 fills-in its local data
          np1 = 1
          DO bj=1,nSy
            DO bi=1,nSx
              biG = (mpi_myXGlobalLo(np1)-1)/sNx+bi
              bjG = (mpi_myYGlobalLo(np1)-1)/sNy+bj
              globalBuf(1,biG,bjG) = shareBufCS2_R8(1,bi,bj)
              globalBuf(2,biG,bjG) = shareBufCS2_R8(2,bi,bj)
            ENDDO
          ENDDO

C--   Process 0 polls and receives data from each process in turn
          DO npe = 1, numberOfProcs-1
#ifndef DISABLE_MPI_READY_TO_RECEIVE
            CALL MPI_SEND (ready_to_receive, 1, MPI_INTEGER,
     &             npe, itag, MPI_COMM_MODEL, ierr)
#endif
            CALL MPI_RECV (loc2Buf, lbuf2, MPI_DOUBLE_PRECISION,
     &             npe, itag, MPI_COMM_MODEL, istatus, ierr)

C--   Process 0 gathers the local arrays into a global array.
            np1 = npe + 1
            DO bj=1,nSy
             DO bi=1,nSx
              biG = (mpi_myXGlobalLo(np1)-1)/sNx+bi
              bjG = (mpi_myYGlobalLo(np1)-1)/sNy+bj
              globalBuf(1,biG,bjG) = loc2Buf(1,bi,bj)
              globalBuf(2,biG,bjG) = loc2Buf(2,bi,bj)
             ENDDO
            ENDDO
          ENDDO

C--   Cumulate Sum over all tiles:
          globalBuf(3,1,1) = 0.
          bj = 1
          DO bi = 1,nSx*nPx-1
            globalBuf(3,1+bi,bj) = globalBuf(3,bi,bj)
     &                           + globalBuf(1,bi,bj)
          ENDDO
          DO bj = 1,nSy*nPy-1
           DO bi = 1,nSx*nPx
            globalBuf(3,bi,1+bj) = globalBuf(3,bi,bj)
     &                           + globalBuf(2,bi,bj)
           ENDDO
          ENDDO

C--   Process 0 fills-in its local data
          np1 = 1
          DO bj=1,nSy
            DO bi=1,nSx
              biG = (mpi_myXGlobalLo(np1)-1)/sNx+bi
              bjG = (mpi_myYGlobalLo(np1)-1)/sNy+bj
              shareBufCS1_R8(bi,bj) = globalBuf(3,biG,bjG)
            ENDDO
          ENDDO

C--   Process 0 sends result to all other processes
          DO npe = 1, numberOfProcs-1
C-    fill local array with relevant portion of global array
            np1 = npe + 1
            DO bj=1,nSy
             DO bi=1,nSx
              biG = (mpi_myXGlobalLo(np1)-1)/sNx+bi
              bjG = (mpi_myYGlobalLo(np1)-1)/sNy+bj
              loc1Buf(bi,bj) = globalBuf(3,biG,bjG)
             ENDDO
            ENDDO
            CALL MPI_SEND (loc1Buf, lbuf1, MPI_DOUBLE_PRECISION,
     &                     npe, itag, MPI_COMM_MODEL, ierr)

          ENDDO

        ENDIF

      ELSEIF (useCubedSphereExchange) THEN
#else /* not USE_MPI */
      IF     (useCubedSphereExchange) THEN
#endif /* ALLOW_USE_MPI */

C--   assume 1 tile / face, from bi=1 to 6, no MPI
        shareBufCS1_R8(1,1) = 0.
        bj = 1
        DO bi = 1,nSx-1
           nf = 1 + MOD(1+bi,2)
           shareBufCS1_R8(1+bi,bj) = shareBufCS1_R8(bi,bj)
     &                             + shareBufCS2_R8(nf,bi,bj)
        ENDDO
C-    fill in missing corner: 1 = North-West corner of face 1
C-                            2 = South-East corner of face 2
        bi = 1
        psiLoc(1) = shareBufCS1_R8(bi,bj) + shareBufCS2_R8(2,bi,bj)
        bi = MIN(2,nSx)
        psiLoc(2) = shareBufCS1_R8(bi,bj) + shareBufCS2_R8(1,bi,bj)

      ELSE

C--   Cumulate Sum over all tiles:
        shareBufCS1_R8(1,1) = 0.
        bj = 1
        DO bi = 1,nSx-1
           shareBufCS1_R8(1+bi,bj) = shareBufCS1_R8(bi,bj)
     &                             + shareBufCS2_R8(1,bi,bj)
        ENDDO
        DO bj = 1,nSy-1
         DO bi = 1,nSx
           shareBufCS1_R8(bi,1+bj) = shareBufCS1_R8(bi,bj)
     &                             + shareBufCS2_R8(2,bi,bj)
         ENDDO
        ENDDO

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

#endif /* ALLOW_EXCH2 */
      RETURN
      END
