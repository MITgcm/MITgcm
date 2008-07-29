C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_print_comm_sequence.F,v 1.5 2008/07/29 20:25:23 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: W2_PRINT_COMM_SEQUENCE

C     !INTERFACE:
      SUBROUTINE W2_PRINT_COMM_SEQUENCE
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE W2_PRINT_COMM_SEQUENCE
C     | o Write communication sequence for a given WRAPPER2
C     |   toplogy
C     *==========================================================*
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
      INTEGER myTileId, nN
c     INTEGER PI_TC2SC(2), PJ_TC2SC(2), O_TC2SC(2)
c     _RL     SXDIR_TX2CX(2), SYDIR_TX2CX(2)
      INTEGER targetIlo, targetIhi, targetJlo, targetJhi
      INTEGER sourceIlo, sourceIhi, sourceJlo, sourceJhi
      INTEGER I, N, targetTile, myThid, targetProc, sourceProc
      INTEGER iStride, jStride
      INTEGER pi(2), pj(2), oi, oj, tN
      INTEGER itb, jtb, isb, jsb

      myThid = 1

C     Send loop for cell centered
      DO I=1,nSx
       myTileId=W2_myTileList(I)
       nN=exch2_nNeighbours(myTileId)
       sourceProc=exch2_tProc(myTileId)
       DO N=1,nN
        targetTile=exch2_neighbourId(N,myTileId)
        targetProc=exch2_tProc(targetTile)
        tN = exch2_opposingSend(N,myTileId)
        targetIlo =exch2_iLo(tN,targetTile)
        targetIhi =exch2_iHi(tN,targetTile)
        targetJlo =exch2_jLo(tN,targetTile)
        targetJhi =exch2_jHi(tN,targetTile)
        pi(1)     =exch2_pij(1,N,myTileId)
        pi(2)     =exch2_pij(2,N,myTileId)
        pj(1)     =exch2_pij(3,N,myTileId)
        pj(2)     =exch2_pij(4,N,myTileId)
        oi        =exch2_oi(N,myTileId)
        oj        =exch2_oj(N,myTileId)
        CALL EXCH2_GET_SEND_BOUNDS(
     I       'T ', OLx,
     O       iStride, jStride,
     U       targetIlo, targetIhi, targetJlo, targetJhi )

        itb = exch2_tBasex(targetTile)
        jtb = exch2_tBasey(targetTile)
        isb = exch2_tBasex(myTileId)
        jsb = exch2_tBasey(myTileId)
        sourceIlo=pi(1)*(targetIlo+itb)+pi(2)*(targetJlo+jtb)+oi-isb
        sourceJlo=pj(1)*(targetIlo+itb)+pj(2)*(targetJlo+jtb)+oj-jsb
        sourceIhi=pi(1)*(targetIhi+itb)+pi(2)*(targetJhi+jtb)+oi-isb
        sourceJhi=pj(1)*(targetIhi+itb)+pj(2)*(targetJhi+jtb)+oj-jsb
C       Tile XX sends to points i=ilo:ihi,j=jlo:jhi in tile YY
        WRITE(msgBuffer,'(A,I6,A,I4,A,4(A,I4))')
     &   'Tile', myTileId,' (pr=',sourceProc,')',
     &   ' sends pts i=',sourceIlo,':',sourceIhi,
     &            ', j=',sourceJlo,':',sourceJhi
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &         SQUEEZE_RIGHT,myThid)
        WRITE(msgBuffer,'(4(A,I4),A,I6,A,I4,A)')
     &   '                        to pts i=',targetIlo,':',targetIhi,
     &                                ', j=',targetJlo,':',targetJhi,
     &                ' in tile ',targetTile,' (pr=',targetProc,')'
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &         SQUEEZE_RIGHT,myThid)
       ENDDO
      ENDDO

C     Recv loop for cell centered
      DO I=1,nSx
       myTileId=W2_myTileList(I)
       nN=exch2_nNeighbours(myTileId)
       sourceProc=exch2_tProc(myTileId)
       DO N=1,nN
        targetTile=exch2_neighbourId(N,myTileId)
        targetProc=exch2_tProc(targetTile)
C       Find entry for tile targetTile entry that sent to this edge.
        tN=exch2_opposingSend(N,myTileId)
C       Get the range of points associated with that entry
        targetIlo =exch2_iLo(N,myTileId)
        targetIhi =exch2_iHi(N,myTileId)
        targetJlo =exch2_jLo(N,myTileId)
        targetJhi =exch2_jHi(N,myTileId)
        CALL EXCH2_GET_RECV_BOUNDS(
     I       'T ', OLx,
     O       iStride, jStride,
     U       targetIlo, targetIhi, targetJlo, targetJhi )
C       Tile XX receives points i=ilo:ihi,j=jlo:jhi in tile YY
        WRITE(msgBuffer,'(A,I6,A,I4,A,4(A,I4),A,I6,A,I4,A)')
     &   'Tile', myTileId,' (pr=',sourceProc,')',
     &   ' recv pts i=',targetIlo,':',targetIhi,
     &           ', j=',targetJlo, ':',targetJhi,
     &   ' from tile',targetTile,' (pr=',targetProc,')'
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &         SQUEEZE_RIGHT,myThid)
       ENDDO
      ENDDO

      RETURN
      END
