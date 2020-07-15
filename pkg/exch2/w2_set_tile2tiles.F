#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_SET_TILE2TILES

C !INTERFACE:
      SUBROUTINE W2_SET_TILE2TILES( myThid )

C     !DESCRIPTION:
C     Set-up tile neighbours and index relations for EXCH2.

C     !USES:
      IMPLICIT NONE

C      Tile topology settings data structures
#include "SIZE.h"
#include "EEPARAMS.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_PARAMS.h"
#include "W2_EXCH2_TOPOLOGY.h"

C     !INPUT PARAMETERS:
C     myThid  :: my Thread Id number
C               (Note: not relevant since threading has not yet started)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf  :: Informational/error message buffer
C     tile_edge2edge(nId,tId) :: Tile edge to edge connection (of tile "tId"
C                                and neighbour "nId"):
C                1rst digit gives local tile Edge (10,20,30,40 <==> N,S,E,W)
C                2nd  digit gives remote tile Edge (1,2,3,4 <==> N,S,E,W)
C                corresponding to this neighbour connection.
      INTEGER tile_edge2edge( W2_maxNeighbours, W2_maxNbTiles )
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER tNx, tNy, nbTx, nbNeighb
      INTEGER i, k, ii, nn
      INTEGER is, js, ns, it, jt, nt, tx, ty
      INTEGER iLo, iHi, jLo, jHi
      INTEGER ii1, ii2, jj1, jj2, ddi, ddj
      INTEGER ibnd1, ibnd2, jbnd1, jbnd2
      INTEGER itbd1, itbd2, jtbd1, jtbd2
      INTEGER isbd1, isbd2, jsbd1, jsbd2
      INTEGER txbnd1, txbnd2, tybnd1, tybnd2
      INTEGER errCnt
      LOGICAL internConnect, prtFlag
CEOP

      WRITE(msgBuf,'(2A)') 'W2_SET_TILE2TILES:',
     &       ' tile neighbours and index connection:'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      prtFlag = ABS(W2_printMsg).GE.2
     &       .OR. ( W2_printMsg .NE.0 .AND. myProcId.EQ.0 )

C--   Initialise local arrays
      DO is=1,W2_maxNbTiles
       DO ns=1,W2_maxNeighbours
        tile_edge2edge(ns,is) = 0
        exch2_neighbourDir(ns,is) = 0
       ENDDO
      ENDDO

      tNx = sNx
      tNy = sNy
      DO is=1,exch2_nTiles
       js  = exch2_myFace(is)
C     test "myFace" for blank tile; no need for connection if tile is blank
       IF ( js.NE.0 ) THEN
        js  = exch2_myFace(is)
        iLo = exch2_tBasex(is)+1
        iHi = exch2_tBasex(is)+exch2_tNx(is)
        jLo = exch2_tBasey(is)+1
        jHi = exch2_tBasey(is)+exch2_tNy(is)

        nbNeighb = 0
        DO i=1,4
         ii1 = iLo
         ii2 = iHi
         jj1 = jLo
         jj2 = jHi
         IF ( i.EQ.1 ) THEN
C--   Northern Edge: [iLo:iHi,jHi]
          jj1 = jHi+1
          jj2 = jHi+1
          internConnect = jHi.LT.exch2_mydNy(is)
          IF ( .NOT.internConnect ) exch2_isNedge(is) = 1
         ELSEIF ( i.EQ.2 ) THEN
C--   Southern Edge: [iLo:iHi,jLo]
          jj1 = jLo-1
          jj2 = jLo-1
          internConnect = jLo.GT.1
          IF ( .NOT.internConnect ) exch2_isSedge(is) = 1
         ELSEIF ( i.EQ.3 ) THEN
C--   Eastern Edge: [iHi,jLo:jHi]
          ii1 = iHi+1
          ii2 = iHi+1
          internConnect = iHi.LT.exch2_mydNx(is)
          IF ( .NOT.internConnect ) exch2_isEedge(is) = 1
         ELSE
C--   Western Edge: [iLo,jLo:jHi]
          ii1 = iLo-1
          ii2 = iLo-1
          internConnect = iLo.GT.1
          IF ( .NOT.internConnect ) exch2_isWedge(is) = 1
         ENDIF
         ddi = MIN( ii2-ii1, 1)
         ddj = MIN( jj2-jj1, 1)

         IF ( internConnect ) THEN
C---  Internal (from the same facet)
C-    N(i=1) -> S(ii=2); S(i=2) -> N(ii=1); E(i=3) -> W(ii=4); W(i=4) -> E(ii=3)
C-    get tile neighbour Id "it":
            nbTx = facet_dims(2*js-1)/tNx
            ii = 1 + MOD(i,2)
            it = 2*ii - 3
            IF ( i.LE.2 ) THEN
               it = is + it*nbTx
            ELSE
               it = is + it
               ii = ii + 2
            ENDIF
            IF ( exch2_myFace(it).NE.0 ) THEN
               nbNeighb = nbNeighb + 1
               ns = MIN(nbNeighb,W2_maxNeighbours)
               exch2_neighbourId(ns,is) = it
               tile_edge2edge(ns,is) = 10*i + ii
               exch2_pij(1,ns,is) = 1
               exch2_pij(2,ns,is) = 0
               exch2_pij(3,ns,is) = 0
               exch2_pij(4,ns,is) = 1
               exch2_oi(ns,is) = 0
               exch2_oj(ns,is) = 0
               exch2_iLo(ns,is) = ii1 - ddi - exch2_tBasex(is)
               exch2_iHi(ns,is) = ii2 + ddi - exch2_tBasex(is)
               exch2_jLo(ns,is) = jj1 - ddj - exch2_tBasey(is)
               exch2_jHi(ns,is) = jj2 + ddj - exch2_tBasey(is)
            ENDIF

         ELSE

C---  External (from an other facet)
          jt = INT(facet_link(i,js))
          ii = MOD( NINT(facet_link(i,js)*10.), 10 )
          IF ( jt.GT.0 ) THEN
C--   needs to find list of tiles in target facet "jt" which connect to "is"
C-    index range on target facet:
           ibnd1 = facet_pij(1,ii,jt)*ii1
     &           + facet_pij(2,ii,jt)*jj1 + facet_oi(ii,jt)
           ibnd2 = facet_pij(1,ii,jt)*ii2
     &           + facet_pij(2,ii,jt)*jj2 + facet_oi(ii,jt)
           jbnd1 = facet_pij(3,ii,jt)*ii1
     &           + facet_pij(4,ii,jt)*jj1 + facet_oj(ii,jt)
           jbnd2 = facet_pij(3,ii,jt)*ii2
     &           + facet_pij(4,ii,jt)*jj2 + facet_oj(ii,jt)
C-    at least 1 index bnd is common (either ibnd1=ibnd2 or jbnd1=jbnd2)
           IF ( ibnd1.LE.ibnd2 ) THEN
             txbnd1 = ( ibnd1 -1 )/tNx
             txbnd2 = ( ibnd2 -1 )/tNx
           ELSE
             txbnd1 = ( ibnd2 -1 )/tNx
             txbnd2 = ( ibnd1 -1 )/tNx
           ENDIF
           IF ( jbnd1.LE.jbnd2 ) THEN
             tybnd1 = ( jbnd1 -1 )/tNy
             tybnd2 = ( jbnd2 -1 )/tNy
           ELSE
             tybnd1 = ( jbnd2 -1 )/tNy
             tybnd2 = ( jbnd1 -1 )/tNy
           ENDIF
           nbTx = facet_dims(2*jt-1)/tNx
           DO ty=tybnd1,tybnd2
            DO tx=txbnd1,txbnd2
             it = facet_owns(1,jt) + tx + ty*nbTx
             IF ( exch2_myFace(it).NE.0 ) THEN
C-    Save to common block this neighbour connection :
               nbNeighb = nbNeighb + 1
               ns = MIN(nbNeighb,W2_maxNeighbours)
               exch2_neighbourId(ns,is)  = it
               tile_edge2edge(ns,is) = 10*i + ii
               DO k=1,4
                 exch2_pij(k,ns,is) = facet_pij(k,i,js)
               ENDDO
               exch2_oi(ns,is) = facet_oi(i,js)
               exch2_oj(ns,is) = facet_oj(i,js)
C     Edge length to be exchanged between tiles is & it:
               itbd1 = MIN( MAX( ibnd1, exch2_tBasex(it)+1 ),
     &                                  exch2_tBasex(it)+tNx )
               itbd2 = MIN( MAX( ibnd2, exch2_tBasex(it)+1 ),
     &                                  exch2_tBasex(it)+tNx )
               jtbd1 = MIN( MAX( jbnd1, exch2_tBasey(it)+1 ),
     &                                  exch2_tBasey(it)+tNy )
               jtbd2 = MIN( MAX( jbnd2, exch2_tBasey(it)+1 ),
     &                                  exch2_tBasey(it)+tNy )
               isbd1 = facet_pij(1,i,js)*itbd1
     &               + facet_pij(2,i,js)*jtbd1 + facet_oi(i,js)
               isbd2 = facet_pij(1,i,js)*itbd2
     &               + facet_pij(2,i,js)*jtbd2 + facet_oi(i,js)
               jsbd1 = facet_pij(3,i,js)*itbd1
     &               + facet_pij(4,i,js)*jtbd1 + facet_oj(i,js)
               jsbd2 = facet_pij(3,i,js)*itbd2
     &               + facet_pij(4,i,js)*jtbd2 + facet_oj(i,js)
               exch2_iLo(ns,is) = isbd1 - ddi - exch2_tBasex(is)
               exch2_iHi(ns,is) = isbd2 + ddi - exch2_tBasex(is)
               exch2_jLo(ns,is) = jsbd1 - ddj - exch2_tBasey(is)
               exch2_jHi(ns,is) = jsbd2 + ddj - exch2_tBasey(is)
C-           end active tile "it"
             ENDIF
C-         end loops on tile indices tx,ty
            ENDDO
           ENDDO
C-        end active connection (it > 0)
          ENDIF
C-       end internal/external connection
         ENDIF
C-      end N,S,E,W Edge loop
        ENDDO
        exch2_nNeighbours(is) = nbNeighb
        IF ( prtFlag ) THEN
         WRITE(W2_oUnit,'(A,I8,A,I3,A,4(A,I2))')
     &    'Tile',is,' : nbNeighb=',nbNeighb,' ; is-at-Facet-Edge:',
     &        ' N=', exch2_isNedge(is), ' , S=', exch2_isSedge(is),
     &      ' , E=', exch2_isEedge(is), ' , W=', exch2_isWedge(is)
         DO ns=1,MIN(nbNeighb,W2_maxNeighbours)
          WRITE(W2_oUnit,'(A,I3,A,I8,2(A,2I6),A,4I3,A,2I6,A)')
     &     ' ns:',ns,' it=',exch2_neighbourId(ns,is),
     &     ', iLo,iHi=', exch2_iLo(ns,is), exch2_iHi(ns,is),
     &     ', jLo,jHi=', exch2_jLo(ns,is), exch2_jHi(ns,is)
c    &     , ' (pij=',(exch2_pij(k,ns,is),k=1,4),
c    &     ', oi,oj=', exch2_oi(ns,is), exch2_oj(ns,is),')'
         ENDDO
        ENDIF
C-     end active tile "is"
       ENDIF
C-    end loop on tile "is"
      ENDDO

C-  Check nbNeighb =< W2_maxNeighbours
      nbNeighb = 0
      it = 0
      DO is=1,exch2_nTiles
        IF ( exch2_nNeighbours(is).GT.nbNeighb ) THEN
          nbNeighb = exch2_nNeighbours(is)
          it = is
        ENDIF
      ENDDO
      WRITE(msgBuf,'(A,I5,A,I3)')
     &  'current Max.Nb.Neighbours (e.g., on tile',it,' ) =',nbNeighb
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      IF ( nbNeighb.GT.W2_maxNeighbours ) THEN
        WRITE(msgBuf,'(2(A,I4),A)')
     &           'W2_SET_TILE2TILES: Max.Nb.Neighbours=', nbNeighb,
     &           ' >', W2_maxNeighbours,' =W2_maxNeighbours'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'Must increase "W2_maxNeighbours"',
     &                       ' in "W2_EXCH2_SIZE.h" + recompile'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_SET_TILE2TILES (W2_maxNeighbours)'
      ENDIF

C-    Set exch2_opposingSend(ns,is) = Neighbour Id (in list of neighbours
C     of tile exch2_neighbourId(ns,is)) which is connected to tile "is"
C     neighbour Id "ns" with matching edge <-> edge connection (ii==i).
      errCnt = 0
      DO is=1,exch2_nTiles
       DO ns=1,exch2_nNeighbours(is)
        i  = tile_edge2edge(ns,is)/10
        ii = MOD(tile_edge2edge(ns,is),10)
        IF ( ii .NE. 0) THEN
          exch2_neighbourDir(ns,is) = i
        ENDIF
        it = exch2_neighbourId(ns,is)
        DO nt=1,exch2_nNeighbours(it)
c         i  = tile_edge2edge(nt,it)/10
          ii = MOD(tile_edge2edge(nt,it),10)
          IF ( exch2_neighbourId(nt,it).EQ.is .AND. ii.EQ.i ) THEN
           IF ( exch2_opposingSend(ns,is).EQ.0 ) THEN
            exch2_opposingSend(ns,is) = nt
           ELSE
            nn = exch2_opposingSend(ns,is)
            WRITE(msgBuf,'(A,I8,2(A,I3),A)') 'Tile',is,' neighb:',
     &      ns,' (',tile_edge2edge(ns,is),' ) has multiple connections'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I8,5(A,I3))') ' with tile', it, ' :',
     &      nn,' (',tile_edge2edge(nn,it),' ) and',
     &      nt,' (',tile_edge2edge(nt,it),' )'
            CALL PRINT_ERROR( msgBuf, myThid )
            errCnt = errCnt + 1
           ENDIF
          ENDIF
        ENDDO
        IF ( exch2_opposingSend(ns,is).EQ.0 ) THEN
            WRITE(msgBuf,'(A,I8,2(A,I3),A,I8)') 'Tile',is,' neighb:',
     &      ns,' (',tile_edge2edge(ns,is),' ) no connection from',it
            CALL PRINT_ERROR( msgBuf, myThid )
            errCnt = errCnt + 1
        ENDIF

       ENDDO
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &   ' W2_SET_TILE2TILES: found', errCnt, ' Dbl/No connection'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_SET_TILE2TILES (tile connection)'
      ENDIF
C--  Check opposingSend reciprocity:
      errCnt = 0
      DO is=1,exch2_nTiles
       DO ns=1,exch2_nNeighbours(is)
        it = exch2_neighbourId(ns,is)
        nt = exch2_opposingSend(ns,is)
        ii = exch2_neighbourId(nt,it)
        nn = exch2_opposingSend(nt,it)
        IF ( ii.NE.is .OR. nn.NE.ns ) THEN
          WRITE(msgBuf,'(A,I8,2(A,I3),A)') 'Tile',is,' neighb:',
     &      ns,' (',tile_edge2edge(ns,is),' ) connected'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I8,5(A,I3))') ' with tile', it, ' :',
     &      nt,' (',tile_edge2edge(nt,it),' )'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I8,2(A,I3),A)') ' but',it,' neighb:',
     &      nt,' (',tile_edge2edge(nt,it),' ) connected'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I8,3(A,I3))') ' with tile', ii, ' :',
     &      nn,' (',tile_edge2edge(nn,ii),' )'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCnt = errCnt + 1
        ENDIF
       ENDDO
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &   ' W2_SET_TILE2TILES: found', errCnt, ' opposingSend error'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_SET_TILE2TILES (opposingSend)'
      ENDIF

      RETURN
      END
