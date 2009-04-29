C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_eeboot.F,v 1.8 2009/04/29 19:44:44 jmc Exp $
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
#include "EESUPPORT.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Local variables ==
      INTEGER nt_perProc, thisProc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) fName
      INTEGER stdUnit, W2_oUnit, iLen
      CHARACTER commFlag
      INTEGER myTileId
      INTEGER myThid, I, J, II, np, jp
      INTEGER iErr, tNx, tNy

C     Set dummy myThid value (we are not multi-threaded here)
      myThid = 1

C     Initialise to zero EXCH2_TOPOLOGY common blocks
      DO I = 1,NTILES
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
        DO J = 1,MAX_NEIGHBOURS
          exch2_neighbourId(J,I)  = 0
          exch2_opposingSend(J,I) = 0
          DO II = 1,4
           exch2_pij(II,J,I) = 0
          ENDDO
          exch2_oi(J,I)  = 0
          exch2_oj(J,I)  = 0
          exch2_iLo(J,I) = 0
          exch2_iHi(J,I) = 0
          exch2_jLo(J,I) = 0
          exch2_jHi(J,I) = 0
        ENDDO
      ENDDO
C     Initialise parameters from EXCH2_PARAMS common blocks
      W2_oUnit = standardMessageUnit

C     Set W2-EXCH2 parameters
c     CALL W2_EXCH2_READPARMS
c     W2_oUnit = -1

      stdUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') '===== Start setting W2 TOPOLOGY:'
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )

C     Open message output-file (if needed)
      IF ( W2_oUnit.LT.0 ) THEN
        WRITE(fName,'(A,I4.4,A)')
     &     'w2_tile_topology.',myProcId,'.log'
        iLen = ILNBLNK(fName)
        CALL MDSFINDUNIT( W2_oUnit, myThid )
        OPEN( W2_oUnit, file=fName(1:iLen),
     &                  status='unknown', form='formatted')
c       WRITE(msgBuf,'(2AA)') '  repport on file: ', fName(1:iLen)
        WRITE(msgBuf,'(2AA)') ' write to log-file: ', fName(1:iLen)
        CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

C     Define topology for every tile
      CALL W2_E2SETUP

C     Decide which tiles this process handles - do this inline for now, but
C     should go in subroutine.
C     Set which rank processes "own" which tiles. This should probably
C     be queried as part of some hand-shaking but for now we use the
C     functional relationship that was used above.
C     Fill also W2_mpi_myTileList for Single-CPU-IO.

C     Number of tiles I handle is nSx*nSy
      nt_perProc = nSx*nSy
      thisProc = 1
#ifdef ALLOW_USE_MPI
      thisProc = 1+myPid
#endif
      J = 0
      DO I=1,NTILES
       IF ( exch2_myFace(I) .NE. 0 ) THEN
        np = 1 + J/nt_perProc
        jp = 1 + MOD(J,nt_perProc)
        exch2_tProc(I) = np
        W2_mpi_myTileList(np,jp) = I
        IF ( np.EQ.thisProc ) W2_myTileList(jp) = I
        J = J + 1
       ENDIF
      ENDDO
      IF ( J .NE. nSx*nSy*nPx*nPy ) THEN
       STOP
     & 'ERROR W2_EEBOOT: number of active tiles is not nPx*nSx*nPy*nSy'
      ENDIF

C--   Check tile sizes
      iErr = 0
      DO I=1,nSx
       myTileId = W2_myTileList(I)
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
      IF ( iErr .NE. 0 ) THEN
       STOP 'ABNORMAL END: W2_EEBOOT'
      ENDIF

C--   Print tiles connection for this process and set myCommonFlag :
      WRITE(msgBuf,'(A)') '===== W2 TILE TOPOLOGY ====='
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_BOTH, myThid )
      DO I=1,nSx
       myTileId = W2_myTileList(I)
       WRITE(msgBuf,'(A,I4)') ' TILE: ', myTileId
       CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
       DO J=1,exch2_nNeighbours(myTileId)
        commFlag = 'M'
        DO II=1,nSx
         IF ( W2_myTileList(II) .EQ. exch2_neighbourId(J,myTileId) )
     &    commFlag = 'P'
        ENDDO
        IF ( commFlag .EQ. 'M' ) THEN
         WRITE(msgBuf,'(A,I4,A,I4,A,A,I4,A)')
     &   '      NEIGHBOUR ',J,' = TILE ',
     &   exch2_neighbourId(J,myTileId), ' Comm = MSG',
     &   ' ( PROC = ',exch2_tProc(exch2_neighbourId(J,myTileId)),')'
         CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
        IF ( commFlag .EQ. 'P' ) THEN
         WRITE(msgBuf,'(A,I4,A,I4,A,A,I4,A)')
     &   '      NEIGHBOUR ',J,' = TILE ',
     &   exch2_neighbourId(J,myTileId), ' Comm = PUT',
     &   ' ( PROC = ',exch2_tProc(exch2_neighbourId(J,myTileId)),')'
         CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
        W2_myCommFlag(J,I) = commFlag
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
      CALL W2_PRINT_COMM_SEQUENCE( W2_oUnit )

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
