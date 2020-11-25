#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: W2_MAP_PROCS

C     !INTERFACE:
      SUBROUTINE W2_MAP_PROCS( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE W2_MAP_PROCS
C     | o Setup Mapping of W2-topology tiles to processes
C     *==========================================================*
C     | Set which process "own" which tiles
C     | and store the 2-way relation between, on one side,
C     |  tile Id from W2-topology and, on the other side,
C     |  process Id with local tile indices bi,bj.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"

C     !INPUT PARAMETERS:
C     myThid  :: my Thread Id number
C               (Note: not relevant since threading has not yet started)
      INTEGER myThid
CEOP

C     !FUNCTIONS:

C     !LOCAL VARIABLES:
      INTEGER thisProc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER commFlag
      INTEGER myTileId
      INTEGER I, J
      INTEGER np, ii, jj, bi, bj
      INTEGER iErr, tNx, tNy

C--   Initialise common blocs W2_MAP_TILE2PROC & W2_EXCH2_COMMFLAG:
      DO I = 1,W2_maxNbTiles
        W2_tileProc(I)  = 0
        W2_tileIndex(I) = 0
c       W2_tileRank(I)  = 0
      ENDDO
      DO bj=1,nSy
       DO bi=1,nSx
         W2_myTileList(bi,bj) = 0
         DO np=1,nPx*nPy
           W2_procTileList(bi,bj,np) = 0
         ENDDO
         DO J=1,W2_maxNeighbours
           W2_myCommFlag(J,bi,bj) = ' '
         ENDDO
       ENDDO
      ENDDO

C--   Decide which tiles this process handles - do this inline for now, but
C     should go in subroutine.
C     Set which rank processes "own" which tiles. This should probably
C     be queried as part of some hand-shaking but for now we use the
C     functional relationship that was used above.
C     Fill also W2_procTileList for Single-CPU-IO.

C     Number of tiles I handle is nSx*nSy
      thisProc = 1 + myProcId
      J = 0
      DO I=1,exch2_nTiles
       IF ( exch2_myFace(I) .NE. 0 ) THEN
C--   old ordering (makes no difference if nSy*nPy=1 )
c       np = 1 + J/(nSx*nSy)
c       jj = MOD(J,nSx*nSy)
c       bj = 1 + jj/nSx
c       bi = 1 + MOD(jj,nSx)
C--   new ordering: for single sub-domain (nFacets=1) case, match default setting
        jj = J/(nSx*nPx)
        ii = MOD(J,nSx*nPx)
C--   natural way to order processors:
c       np = 1 + ii/nSx + (jj/nSy)*nPx
C--   switch processor order to match MPI_CART set-up
        np = 1 + jj/nSy + (ii/nSx)*nPy
        bj = 1 + MOD(jj,nSy)
        bi = 1 + MOD(ii,nSx)
C--
        W2_tileProc(I) = np
        W2_tileIndex(I)= bi + (bj-1)*nSx
        W2_procTileList(bi,bj,np) = I
        IF ( np.EQ.thisProc ) W2_myTileList(bi,bj) = I
        J = J + 1
c       W2_tileRank(I) = J
       ENDIF
      ENDDO
      IF ( J .NE. nSx*nSy*nPx*nPy ) THEN
       STOP
     & 'ERROR W2_MAP_PROCS: number of active tiles not =nPx*nSx*nPy*nSy'
      ENDIF

C--   Check tile sizes
      iErr = 0
      DO bj=1,nSy
       DO bi=1,nSx
        myTileId = W2_myTileList(bi,bj)
        tNx = exch2_tNx(myTileId)
        tNy = exch2_tNy(myTileId)
        IF ( tNx .NE. sNx ) THEN
         WRITE(msgBuf,'(A,I8,2(A,I5))')
     &   'ERROR: S/R W2_MAP_PROCS Topology for tile', myTileId,
     &   'tNx=', tNx, ' is not equal to subgrid size sNx=', sNx
         CALL PRINT_MESSAGE(msgBuf,
     &        errorMessageUnit, SQUEEZE_RIGHT, 1 )
         iErr = iErr+1
        ENDIF
        IF ( tNy .NE. sNy ) THEN
         WRITE(msgBuf,'(A,I8,2(A,I5))')
     &   'ERROR: S/R W2_MAP_PROCS Topology for tile', myTileId,
     &   'tNy=', tNy, ' is not equal to subgrid size sNy=', sNy
         CALL PRINT_MESSAGE(msgBuf,
     &        errorMessageUnit, SQUEEZE_RIGHT, 1 )
         iErr = iErr+1
        ENDIF
       ENDDO
      ENDDO
      IF ( iErr .NE. 0 ) THEN
       STOP 'ABNORMAL END: W2_MAP_PROCS'
      ENDIF

C--   Print tiles connection for this process and set myCommonFlag :
      WRITE(msgBuf,'(A)') '===== W2 TILE TOPOLOGY ====='
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_BOTH, myThid )
      DO bj=1,nSy
       DO bi=1,nSx
        myTileId = W2_myTileList(bi,bj)
        WRITE(msgBuf,'(A,I5,A,2I4,2A,I3)')
     &       ' TILE: ', myTileId,' (bi,bj=', bi, bj, ' )',
     &       ', Nb of Neighbours =', exch2_nNeighbours(myTileId)
c       WRITE(msgBuf,'(2(A,I5),A,I3)') ' TILE: ', myTileId,
c    &       ' , rank=', W2_tileRank(myTileId),
c    &       ' , Nb of Neighbours =', exch2_nNeighbours(myTileId)
        CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
        DO J=1,exch2_nNeighbours(myTileId)
         commFlag = 'M'
         jj = exch2_neighbourId(J,myTileId)
         IF ( W2_tileProc(jj).EQ.thisProc ) commFlag = 'P'
         IF ( commFlag .EQ. 'M' ) THEN
          WRITE(msgBuf,'(A,I3,A,I8,A,I3,2A,I8,A)')
     &    '    NEIGHBOUR',J,' = TILE', exch2_neighbourId(J,myTileId),
     &    ' (n=', exch2_opposingSend(J,myTileId), ') Comm = MSG',
     &    ' (PROC=',W2_tileProc(exch2_neighbourId(J,myTileId)),')'
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
         IF ( commFlag .EQ. 'P' ) THEN
          WRITE(msgBuf,'(A,I3,A,I8,A,I3,2A,I8,A)')
     &    '    NEIGHBOUR',J,' = TILE', exch2_neighbourId(J,myTileId),
     &    ' (n=', exch2_opposingSend(J,myTileId), ') Comm = PUT',
     &    ' (PROC=',W2_tileProc(exch2_neighbourId(J,myTileId)),')'
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
         W2_myCommFlag(J,bi,bj) = commFlag
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
