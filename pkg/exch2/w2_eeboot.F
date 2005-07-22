C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_eeboot.F,v 1.5 2005/07/22 18:21:55 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP             
C     !ROUTINE: W2_EEBOOT

C     !INTERFACE:
      SUBROUTINE W2_EEBOOT
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE W2_EEBOOT                                         
C     | o Setup execution "environment" for WRAPPER2             
C     *==========================================================*
C     | WRAPPER2 provides complex topology support. In this routine
C     | we setup the base topology for the default halo operations.
C     *==========================================================*
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"

C     == Local variables ==
      INTEGER nt_check, nt_perproc
      INTEGER thisPtileLo, thisPtileHi
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
      CHARACTER commFlag
      INTEGER myTileId
      INTEGER myThid, I, J, II
      INTEGER iErr, tNx, tNy
      INTEGER pRank
#ifdef ALLOW_USE_MPI
      INTEGER npe,itemp(nSx),mpiBufSize,mpiRequest
      INTEGER istatus(MPI_STATUS_SIZE)
#endif

C     Set dummy myThid value (we are not multi-threaded here)
      myThid = 1
 
C     Define toplogy for every tile
      CALL W2_E2SETUP

C     Decide which tiles this process handles - do this inline for now, but
C     should go in subroutine.
C     Total number of tiles should be divisible by nPx and nSx
C     ( there is no two dimensional decomposition for W2 ) and
C     number of tiles per process should be nSx
      nt_check = NTILES/(nPx*nSx)
      nt_check = nt_check*nPx*nSx
      IF ( nt_check .NE. NTILES ) THEN
      STOP 
     &'ERROR: W2_EEBOOT number of tiles is not divisible by nPx*nSx'
      ENDIF
      nt_perproc = NTILES/nPx
      IF ( nt_perproc .NE. nSx ) THEN
      STOP 
     &'ERROR: W2_EEBOOT tiles per process is not equal to nSx'
      ENDIF
C     Number of tiles I handle is nSx, range of tile numbers I handle
C     depends on my rank.
#ifdef ALLOW_USE_MPI
      thisPtileLo = myPid*nt_perproc+1
      thisPtileHi = (myPid+1)*nt_perproc
#else
      thisPtileLo = 1
      thisPtileHi = nt_perproc
#endif
      DO I=thisPtileLo, thisPtileHi
       W2_myTileList(I-thisPtileLo+1)=I
      ENDDO
      iErr = 0

      DO I=1,nSx
C      Check tile sizes
       myTileId = W2_myTileList(I)
       tnx = exch2_tnx(myTileId)
       tny = exch2_tny(myTileId)
       IF ( tnx .NE. sNx ) THEN
        WRITE(msgBuffer,'(A,I4,A,I4)')
     &   'ERROR: S/R W2_EEBOOT Topology tnx=',
     &   tnx,
     &   ' is not equal to subgrid size sNx=',
     &   sNx
         CALL PRINT_MESSAGE(msgBuffer, 
     &        errorMessageUnit, SQUEEZE_RIGHT, 1 )
         iErr = iErr+1
       ENDIF
       IF ( tny .NE. sNy ) THEN
        WRITE(msgBuffer,'(A,I4,A,I4,A,I4)')
     &   'ERROR: S/R W2_EEBOOT Topology for tile ',myTileId,
     &   'tny=',
     &   tny,
     &   ' is not equal to subgrid size sNy=',
     &   sNy
         CALL PRINT_MESSAGE(msgBuffer, 
     &        errorMessageUnit, SQUEEZE_RIGHT, 1 )
         iErr = iErr+1
       ENDIF
      ENDDO
      IF ( iErr .NE. 0 ) THEN
       STOP 'ABNORMAL END: W2_EEBOOT'
      ENDIF

C     Set which rank processes "own" which tiles. This should probably
C     be queried as part of some hand-shaking but for now we use the
C     functiional relationship that was used above.
      DO I=1,nTiles
       pRank = (I-1)/nt_perproc
       exch2_tProc(I) = pRank+1
      ENDDO

      WRITE(msgBuffer,'(A)') '===== W2 TILE TOPLOGY ====='
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &     SQUEEZE_BOTH,myThid)
      DO I=1,nSx
       myTileId = W2_myTileList(I)
       WRITE(msgBuffer,'(A,I4)') ' TILE: ', myTileId
       CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &      SQUEEZE_RIGHT,myThid)
       DO J=1,exch2_nNeighbours(myTileId)
        commFlag = 'M'
        DO II=1,nSx
         IF ( W2_myTileList(II) .EQ. exch2_neighbourId(J,myTileId) )
     &    commFlag = 'P'
        ENDDO
        IF ( commFlag .EQ. 'M' ) THEN
         WRITE(msgBuffer,'(A,I4,A,I4,A,A,I4,A)')
     &   '      NEIGHBOUR ',J,' = TILE ', 
     &   exch2_neighbourId(J,myTileId), ' Comm = MSG',
     &   ' ( PROC = ',exch2_tProc(exch2_neighbourId(J,myTileId)),')'
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &        SQUEEZE_RIGHT,myThid)
        ENDIF
        IF ( commFlag .EQ. 'P' ) THEN
         WRITE(msgBuffer,'(A,I4,A,I4,A,A,I4,A)')
     &   '      NEIGHBOUR ',J,' = TILE ', 
     &   exch2_neighbourId(J,myTileId), ' Comm = PUT',
     &   ' ( PROC = ',exch2_tProc(exch2_neighbourId(J,myTileId)),')'
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &        SQUEEZE_RIGHT,myThid)
        ENDIF
        W2_myCommFlag(J,I) = commFlag
       ENDDO
      ENDDO

C     Fill in values for W2_mpi_myTileList
#ifdef ALLOW_USE_MPI
      mpiBufSize=nSx
      mpiRequest=0
      DO npe = 0, numberOfProcs-1
         CALL MPI_ISEND (W2_myTileList, mpiBufSize, MPI_INTEGER,
     &        npe, mpiMyId, MPI_COMM_MODEL, mpiRequest, ierr)
      ENDDO
      DO npe = 0, numberOfProcs-1
         CALL MPI_RECV (itemp, mpiBufSize, MPI_INTEGER,
     &        npe, npe, MPI_COMM_MODEL, istatus, ierr)
         DO I=1,nSx
            W2_mpi_myTileList(npe+1,I)=itemp(I)
         ENDDO
      ENDDO
#else /* ALLOW_USE_MPI */
      DO I=1,nSx
         W2_mpi_myTileList(1,I)=W2_myTileList(I)
      ENDDO
#endif /* ALLOW_USE_MPI */

C     Print out the topology communication schedule
      CALL W2_PRINT_COMM_SEQUENCE
C
      RETURN
      END
