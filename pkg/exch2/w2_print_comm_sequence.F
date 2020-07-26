#include "CPP_EEOPTIONS.h"
#undef W2_PRINT_PREFIX

CBOP
C     !ROUTINE: W2_PRINT_COMM_SEQUENCE

C     !INTERFACE:
      SUBROUTINE W2_PRINT_COMM_SEQUENCE( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE W2_PRINT_COMM_SEQUENCE
C     | o Write communication sequence for a given WRAPPER2
C     |   topology
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     W2_oUnit   :: fortran I/O unit
C     myThid     :: my Thread Id number
c     INTEGER W2_oUnit
      INTEGER myThid

#ifndef W2_PRINT_PREFIX
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi         :: tile index
C     N          :: Neighbours index
C     nN         :: number of Neighbours
C     targetTile ::
C     sourceProc ::
      INTEGER myTileId, nN
c     INTEGER PI_TC2SC(2), PJ_TC2SC(2), O_TC2SC(2)
c     _RL     SXDIR_TX2CX(2), SYDIR_TX2CX(2)
      INTEGER targetIlo, targetIhi, targetJlo, targetJhi
      INTEGER sourceIlo, sourceIhi, sourceJlo, sourceJhi
      INTEGER targetTile, targetProc, sourceProc
      INTEGER bi, bj, N
      INTEGER iStride, jStride
      INTEGER pi(2), pj(2), oi, oj, tN
      INTEGER itb, jtb, isb, jsb
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifndef W2_PRINT_PREFIX
      INTEGER iLen
#endif
CEOP

C     Send loop for cell centered
      DO bj=1,nSy
       DO bi=1,nSx
        myTileId=W2_myTileList(bi,bj)
        nN=exch2_nNeighbours(myTileId)
        sourceProc=W2_tileProc(myTileId)
        DO N=1,nN
         targetTile=exch2_neighbourId(N,myTileId)
         targetProc=W2_tileProc(targetTile)
         tN = exch2_opposingSend(N,myTileId)
         pi(1)     =exch2_pij(1,N,myTileId)
         pi(2)     =exch2_pij(2,N,myTileId)
         pj(1)     =exch2_pij(3,N,myTileId)
         pj(2)     =exch2_pij(4,N,myTileId)
         oi        =exch2_oi(N,myTileId)
         oj        =exch2_oj(N,myTileId)
         CALL EXCH2_GET_SCAL_BOUNDS(
     I              'T ', OLx, .TRUE.,
     I              targetTile, tN,
     O              targetIlo, targetIhi, targetJlo, targetJhi,
     O              iStride, jStride,
     I              myThid )

         itb = exch2_tBasex(targetTile)
         jtb = exch2_tBasey(targetTile)
         isb = exch2_tBasex(myTileId)
         jsb = exch2_tBasey(myTileId)
         sourceIlo=pi(1)*(targetIlo+itb)+pi(2)*(targetJlo+jtb)+oi-isb
         sourceJlo=pj(1)*(targetIlo+itb)+pj(2)*(targetJlo+jtb)+oj-jsb
         sourceIhi=pi(1)*(targetIhi+itb)+pi(2)*(targetJhi+jtb)+oi-isb
         sourceJhi=pj(1)*(targetIhi+itb)+pj(2)*(targetJhi+jtb)+oj-jsb
C        Tile XX sends to points i=ilo:ihi,j=jlo:jhi in tile YY
         WRITE(msgBuf,'(A,I8,A,I8,A,4(A,I4))')
     &    'Tile', myTileId,' (pr=',sourceProc,')',
     &    ' sends pts i=',sourceIlo,':',sourceIhi,
     &             ', j=',sourceJlo,':',sourceJhi
#ifdef W2_PRINT_PREFIX
         CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
#else
         iLen = ILNBLNK(msgBuf)
         WRITE(W2_oUnit,'(A)') msgBuf(1:iLen)
#endif
         WRITE(msgBuf,'(26X,4(A,I4),A,I8,A,I8,A)')
     &    '    to pts i=',targetIlo,':',targetIhi,
     &             ', j=',targetJlo,':',targetJhi,
     &    ' in tile ',targetTile,' (pr=',targetProc,')'
#ifdef W2_PRINT_PREFIX
         CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
#else
         iLen = ILNBLNK(msgBuf)
         WRITE(W2_oUnit,'(A)') msgBuf(1:iLen)
#endif
        ENDDO
       ENDDO
      ENDDO

C     Recv loop for cell centered
      DO bj=1,nSy
       DO bi=1,nSx
        myTileId=W2_myTileList(bi,bj)
        nN=exch2_nNeighbours(myTileId)
        sourceProc=W2_tileProc(myTileId)
        DO N=1,nN
         targetTile=exch2_neighbourId(N,myTileId)
         targetProc=W2_tileProc(targetTile)
C        Find entry for tile targetTile entry that sent to this edge.
         tN=exch2_opposingSend(N,myTileId)
C        Get the range of points associated with that entry
         CALL EXCH2_GET_SCAL_BOUNDS(
     I              'T ', OLx, .TRUE.,
     I              myTileId, N,
     O              targetIlo, targetIhi, targetJlo, targetJhi,
     O              iStride, jStride,
     I              myThid )
C        Tile XX receives points i=ilo:ihi,j=jlo:jhi in tile YY
         WRITE(msgBuf,'(A,I8,A,I8,A,4(A,I4),A,I8,A,I8,A)')
     &    'Tile', myTileId,' (pr=',sourceProc,')',
     &    ' recv pts i=',targetIlo,':',targetIhi,
     &            ', j=',targetJlo, ':',targetJhi,
     &    ' from tile',targetTile,' (pr=',targetProc,')'
#ifdef W2_PRINT_PREFIX
         CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
#else
         iLen = ILNBLNK(msgBuf)
         WRITE(W2_oUnit,'(A)') msgBuf(1:iLen)
#endif
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
