C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_eeboot.F,v 1.14 2011/07/09 21:52:34 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: W2_EEBOOT

C     !INTERFACE:
      SUBROUTINE W2_EEBOOT

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE W2_EEBOOT
C     | o Setup execution "environment" for WRAPPER2
C     *==========================================================*
C     | WRAPPER2 provides complex topology support. In this routine
C     | we setup the base topology for the default halo operations.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#include "W2_EXCH2_BUFFER.h"
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Local variables ==
      INTEGER thisProc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) fName
c     INTEGER W2_oUnit
      INTEGER stdUnit, iLen
      CHARACTER commFlag
      INTEGER myTileId
      INTEGER myThid, I, J
      INTEGER np, ii, jj, bi, bj
      INTEGER iErr, tNx, tNy

C     Set dummy myThid value (we are not multi-threaded here)
      myThid = 1

C     Initialise to zero EXCH2_TOPOLOGY common blocks
      exch2_nTiles = 0
      DO I = 1,W2_maxNbTiles
        exch2_tNx(I)    = 0
        exch2_tNy(I)    = 0
        exch2_tBasex(I) = 0
        exch2_tBasey(I) = 0
        exch2_txGlobalo(I) = 0
        exch2_tyGlobalo(I) = 0
        exch2_isWedge(I) = 0
        exch2_isNedge(I) = 0
        exch2_isEedge(I) = 0
        exch2_isSedge(I) = 0
        exch2_tProc(I)   = 0
        exch2_myFace(I)  = 0
        exch2_mydNx(I)   = 0
        exch2_mydNy(I)   = 0
        exch2_nNeighbours(I) = 0
        DO J = 1,W2_maxNeighbours
          exch2_neighbourId(J,I)  = 0
          exch2_opposingSend(J,I) = 0
          DO ii = 1,4
           exch2_pij(ii,J,I) = 0
          ENDDO
          exch2_oi(J,I)  = 0
          exch2_oj(J,I)  = 0
          exch2_iLo(J,I) = 0
          exch2_iHi(J,I) = 0
          exch2_jLo(J,I) = 0
          exch2_jHi(J,I) = 0
        ENDDO
      ENDDO
      W2_oUnit = standardMessageUnit

C     Set W2-EXCH2 parameters
      CALL W2_READPARMS( myThid )

      stdUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') '===== Start setting W2 TOPOLOGY:'
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )

C     Open message output-file (if needed)
      IF ( W2_printMsg .LT. 0 ) THEN
        WRITE(fName,'(A,I4.4,A)')
     &     'w2_tile_topology.',myProcId,'.log'
        iLen = ILNBLNK(fName)
        CALL MDSFINDUNIT( W2_oUnit, myThid )
        OPEN( W2_oUnit, file=fName(1:iLen),
     &                  status='unknown', form='formatted')
        WRITE(msgBuf,'(2A)') ' write to log-file: ', fName(1:iLen)
        CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

C     Define topology for every tile
      CALL W2_E2SETUP( myThid )

C     Decide which tiles this process handles - do this inline for now, but
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
        exch2_tProc(I) = np
        W2_procTileList(bi,bj,np) = I
        IF ( np.EQ.thisProc ) W2_myTileList(bi,bj) = I
        J = J + 1
       ENDIF
      ENDDO
      IF ( J .NE. nSx*nSy*nPx*nPy ) THEN
       STOP
     & 'ERROR W2_EEBOOT: number of active tiles is not nPx*nSx*nPy*nSy'
      ENDIF

C--   Check tile sizes
      iErr = 0
      DO bj=1,nSy
       DO bi=1,nSx
        myTileId = W2_myTileList(bi,bj)
        tNx = exch2_tNx(myTileId)
        tNy = exch2_tNy(myTileId)
        IF ( tNx .NE. sNx ) THEN
         WRITE(msgBuf,'(3(A,I5))')
     &   'ERROR: S/R W2_EEBOOT Topology for tile', myTileId,
     &   'tNx=', tNx, ' is not equal to subgrid size sNx=', sNx
         CALL PRINT_MESSAGE(msgBuf,
     &        errorMessageUnit, SQUEEZE_RIGHT, 1 )
         iErr = iErr+1
        ENDIF
        IF ( tNy .NE. sNy ) THEN
         WRITE(msgBuf,'(3(A,I5))')
     &   'ERROR: S/R W2_EEBOOT Topology for tile', myTileId,
     &   'tNy=', tNy, ' is not equal to subgrid size sNy=', sNy
         CALL PRINT_MESSAGE(msgBuf,
     &        errorMessageUnit, SQUEEZE_RIGHT, 1 )
         iErr = iErr+1
        ENDIF
       ENDDO
      ENDDO
      IF ( iErr .NE. 0 ) THEN
       STOP 'ABNORMAL END: W2_EEBOOT'
      ENDIF

C--   Print tiles connection for this process and set myCommonFlag :
      WRITE(msgBuf,'(A)') '===== W2 TILE TOPOLOGY ====='
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_BOTH, myThid )
      DO bj=1,nSy
       DO bi=1,nSx
        myTileId = W2_myTileList(bi,bj)
c       WRITE(msgBuf,'(A,I4)') ' TILE: ', myTileId
        WRITE(msgBuf,'(A,I5,A,I3)') ' TILE: ', myTileId,
     &       ' , Nb of Neighbours =', exch2_nNeighbours(myTileId)
        CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
        DO J=1,exch2_nNeighbours(myTileId)
         commFlag = 'M'
         DO jj=1,nSy
          DO ii=1,nSx
          IF ( W2_myTileList(ii,jj).EQ.exch2_neighbourId(J,myTileId) )
     &     commFlag = 'P'
          ENDDO
         ENDDO
         IF ( commFlag .EQ. 'M' ) THEN
          WRITE(msgBuf,'(A,I3,A,I5,A,I3,2A,I5,A)')
     &    '    NEIGHBOUR',J,' = TILE', exch2_neighbourId(J,myTileId),
     &    ' (n=', exch2_opposingSend(J,myTileId), ') Comm = MSG',
     &    ' (PROC=',exch2_tProc(exch2_neighbourId(J,myTileId)),')'
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
         IF ( commFlag .EQ. 'P' ) THEN
          WRITE(msgBuf,'(A,I3,A,I5,A,I3,2A,I5,A)')
     &    '    NEIGHBOUR',J,' = TILE', exch2_neighbourId(J,myTileId),
     &    ' (n=', exch2_opposingSend(J,myTileId), ') Comm = PUT',
     &    ' (PROC=',exch2_tProc(exch2_neighbourId(J,myTileId)),')'
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
         W2_myCommFlag(J,bi,bj) = commFlag
        ENDDO
       ENDDO
      ENDDO

C     Set filling value for face-corner halo regions
      e2FillValue_RL = 0. _d 0
      e2FillValue_RS = 0. _d 0
      e2FillValue_R4 = 0.e0
      e2FillValue_R8 = 0.d0
C-    for testing only: put a large value (should not affects the results)
c     e2FillValue_RL = 1. _d+20
c     e2FillValue_RS = 1. _d+20
c     e2FillValue_R4 = 1.e+20
c     e2FillValue_R8 = 1.d+20

C     Print out the topology communication schedule
      IF ( W2_printMsg .NE. 0 ) THEN
        CALL W2_PRINT_COMM_SEQUENCE( myThid )
      ENDIF

C     Close message output-file (if needed)
      IF ( W2_oUnit.NE.standardMessageUnit ) THEN
        WRITE(msgBuf,'(A)') '===  End TOPOLOGY report ==='
        CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_BOTH, myThid )
        CLOSE( W2_oUnit )
      ENDIF
      WRITE(msgBuf,'(A)') '=====       setting W2 TOPOLOGY: Done'
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )

      RETURN
      END
