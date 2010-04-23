C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_set_f2f_index.F,v 1.2 2010/04/23 20:21:06 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_SET_F2F_INDEX

C !INTERFACE:
      SUBROUTINE W2_SET_F2F_INDEX( myThid )

C     !DESCRIPTION:
C     Set-up index correspondence matrix for connected Facet-Edges

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
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*1 edge(4)
      INTEGER i, j, ii, jj, i1, j1, k, lo, ll
      INTEGER errCnt
      INTEGER chk1, chk2, chk3, chk4, chk5, chk6
      LOGICAL prtFlag
CEOP
      DATA edge / 'N' , 'S' , 'E' , 'W' /

      WRITE(msgBuf,'(2A)') 'W2_SET_F2F_INDEX:',
     &       ' index matrix for connected Facet-Edges:'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      prtFlag = ABS(W2_printMsg).GE.2
     &       .OR. ( W2_printMsg .NE.0 .AND. myProcId.EQ.0 )

C--   Check Topology
      errCnt = 0
      DO j=1,nFacets
       DO i=1,4
C-    connected to:
         jj = INT(facet_link(i,j))
         ii = MOD( NINT(facet_link(i,j)*10.), 10 )
         IF ( facet_link(i,j).EQ. 0. ) THEN
          WRITE(msgBuf,'(3A,I3,A,F6.2,A)')
     &           '** WARNING ** ',  edge(i), '.Edge of facet #',
     &        j, ' disconnected (facet_link=',facet_link(i,j),')'
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
          CALL PRINT_MESSAGE( msgBuf,errorMessageUnit,SQUEEZE_RIGHT,1 )
         ELSEIF ( jj.LT.1 .OR. jj.GT.nFacets
     &            .OR. ii.LT.1 .OR. ii.GT.4 ) THEN
          WRITE(msgBuf,'(2A,I3,A,F6.2,A)') edge(i), '.Edge of facet #',
     &      j, ' : bad connection (facet_link=',facet_link(i,j),')'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCnt = errCnt + 1
         ELSE
C-    check connection is set-up both ways:
          j1 = INT(facet_link(ii,jj))
          i1 = MOD( NINT(facet_link(ii,jj)*10.), 10 )
          IF ( j1.NE.j .OR. i1.NE.i ) THEN
           WRITE(msgBuf,'(2(2A,I3,A),F5.2,A)')
     &      edge(i), '.Edge facet #', j,' connect to: ',
     &      edge(ii),'.Edge facet #',jj,' (',facet_link(i,j),' )'
           CALL PRINT_ERROR( msgBuf, myThid )
          IF ( i1.GE.1 .AND. i1.LE.4 ) THEN
           WRITE(msgBuf,'(A,2(2A,I3,A),F5.2,A)') ' but ',
     &      edge(ii),'.Edge facet #',jj,' connect to: ',
     &      edge(i1),'.Edge facet #',j1,' (',facet_link(ii,jj),' )'
          ELSE
           WRITE(msgBuf,'(A,2(2A,I3,A),F5.2,A)') ' but ',
     &      edge(ii),'.Edge facet #',jj,' connect to: ',
     &           '?' ,'.Edge facet #',j1,' (',facet_link(ii,jj),' )'
          ENDIF
           CALL PRINT_ERROR( msgBuf, myThid )
           errCnt = errCnt + 1
          ENDIF
         ENDIF
       ENDDO
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &   ' W2_SET_F2F_INDEX: found', errCnt, ' Topology errors'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_SET_F2F_INDEX'
      ENDIF

C--   Check length of connection (facet size) between connected facet-edges
      errCnt = 0
      DO j=1,nFacets
       DO i=1,4
C-    Length of N or S Edge = x-size, E or W Edge = y-size
        lo = 2*(j-1) + (i+1)/2
        lo = facet_dims( lo )
C-    connected to:
        jj = INT(facet_link(i,j))
        ii = MOD( NINT(facet_link(i,j)*10.), 10 )
        IF ( jj.GE.1 ) THEN
         ll = 2*(jj-1)+(ii+1)/2
         ll = facet_dims( ll )
         IF ( lo.NE.ll ) THEN
           WRITE(msgBuf,'(3A,I3,A,I8,A)') 'Length of connection: ',
     &      edge(i), '.Edge facet #', j,  ' (=',lo,')'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(3A,I3,A,I8,A)') '  to: ',
     &      edge(ii),'.Edge facet #', jj, ' (=',ll,') are different'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCnt = errCnt + 1
         ENDIF
C--   Set indices correspondence matrix (facet_pij):
C     suffix "so" for indices of source facet j ;
C     suffix "tg" for indices of target facet jj= INT(facet_link(i,j))
C     pij(:,i,j) : matrix which gives so indices when applied to tg indices
C        iso = pij(1)*itg + pij(2)*jtg + oi
C        jso = pij(3)*itg + pij(4)*jtg + oj

C-    Default: Same orientation (works for N <-> S or E <-> W)
         facet_pij(1,i,j) = 1
         facet_pij(2,i,j) = 0
         facet_pij(3,i,j) = 0
         facet_pij(4,i,j) = 1
C--   1rst: cases with same orientation
         IF ( i.EQ.1 .AND. ii.EQ.2 ) THEN
C-    N <-- S ; [:,jHi]_so <-- [:,0]_tg
           facet_oi(i,j) = 0
           facet_oj(i,j) = +facet_dims(2*j)
         ELSEIF ( i.EQ.2 .AND. ii.EQ.1 ) THEN
C-    S <-- N ; [:, 1 ]_so <-- [:,1+jHi]_tg
           facet_oi(i,j) = 0
           facet_oj(i,j) = -facet_dims(2*jj)
         ELSEIF ( i.EQ.3 .AND. ii.EQ.4 ) THEN
C-    E <-- W ; [iHi,:]_so <-- [0,:]_tg
           facet_oi(i,j) = +facet_dims(2*j-1)
           facet_oj(i,j) = 0
         ELSEIF ( i.EQ.4 .AND. ii.EQ.3 ) THEN
C-    W <-- E  (i=4 & ii=3); [ 1 ,:]_so <-- [iHi,:]_tg
           facet_oi(i,j) = -facet_dims(2*jj-1)
           facet_oj(i,j) = 0
C--   2nd : cases where orientation changes
         ELSEIF ( i.EQ.1 .AND. ii.EQ.4 ) THEN
C-    N <-- W ; mapping W.face target indices to N.face source indices
C-         [:,jHi]_so <-- [0,:]_tg
           facet_pij(1,i,j) = 0
           facet_pij(2,i,j) =-1
           facet_pij(3,i,j) = 1
           facet_pij(4,i,j) = 0
           facet_oi(i,j) = lo+1
           facet_oj(i,j) = +facet_dims(2*j)
         ELSEIF ( i.EQ.2 .AND. ii.EQ.3 ) THEN
C-    S <-- E ; mapping E.face target indices to S.face source indices
C-         [:,1]_so <-- [1+iHi,:]_tg
           facet_pij(1,i,j) = 0
           facet_pij(2,i,j) =-1
           facet_pij(3,i,j) = 1
           facet_pij(4,i,j) = 0
           facet_oi(i,j) = lo+1
           facet_oj(i,j) = -facet_dims(2*jj-1)
         ELSEIF ( i.EQ.3 .AND. ii.EQ.2 ) THEN
C-    E <-- S ; mapping S.face target indices to E.face source indices
C-         [iHi,:]_so <-- [:,0]_tg
           facet_pij(1,i,j) = 0
           facet_pij(2,i,j) = 1
           facet_pij(3,i,j) =-1
           facet_pij(4,i,j) = 0
           facet_oi(i,j) = +facet_dims(2*j-1)
           facet_oj(i,j) = lo+1
         ELSEIF ( i.EQ.4 .AND. ii.EQ.1 ) THEN
C-    W <-- N ; mapping N.face target indices to W.face source indices
C-         [ 1 ,:]_so <-- [:,1+jHi]_tg
           facet_pij(1,i,j) = 0
           facet_pij(2,i,j) = 1
           facet_pij(3,i,j) =-1
           facet_pij(4,i,j) = 0
           facet_oi(i,j) = -facet_dims(2*jj)
           facet_oj(i,j) = lo+1
         ELSE
           WRITE(msgBuf,'(2(3A,I3),A)') ' connect ',
     &      edge(i), '.Edge (facet#', j, ' ) to: ',
     &      edge(ii),'.Edge (facet#', jj,' )'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A)')
     &       ' W2_SET_F2F_INDEX: Connection not supported'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCnt = errCnt + 1
         ENDIF
C--   Print resulting index matrix:
         IF ( prtFlag ) WRITE(W2_oUnit,'(2(3A,I3),A,4I3,A,2I6)')
     &    '  ', edge(i), '.Edge Facet', j, ' <-- ',
     &          edge(ii),'.Edge Facet', jj,
     &    ' : pij=', (facet_pij(k,i,j),k=1,4),
     &    ' ; oi,oj=', facet_oi(i,j), facet_oj(i,j)
        ENDIF
       ENDDO
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &   ' W2_SET_F2F_INDEX: found', errCnt, ' Connection errors'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_SET_F2F_INDEX'
      ENDIF

C--   Check indices correspondence matrix reciprocity :
C     This is not necessary (if code above is right) ; However:
C     a) reciprocity is used later-on => good to check;
C     b) easy to add an error in setting offset => worth to check.
      errCnt = 0
      DO j=1,nFacets
       DO i=1,4
C-    connected to:
        jj = INT(facet_link(i,j))
        ii = MOD( NINT(facet_link(i,j)*10.), 10 )
        IF ( jj.GE.1 ) THEN
C        iso = pij(1)*( Pij(1)*iso + Pij(2)*jso + Oi )
C            + pij(2)*( Pij(3)*iso + Pij(4)*jso + Oj )
C            + oi
C        jso = pij(3)*( Pij(1)*iso + Pij(2)*jso + Oi )
C            + pij(4)*( Pij(3)*iso + Pij(4)*jso + Oj )
C            + oj
C-      Matrix product:
         chk1 = facet_pij(1,i,j)*facet_pij(1,ii,jj)
     &        + facet_pij(2,i,j)*facet_pij(3,ii,jj)
         chk2 = facet_pij(1,i,j)*facet_pij(2,ii,jj)
     &        + facet_pij(2,i,j)*facet_pij(4,ii,jj)
         chk3 = facet_pij(3,i,j)*facet_pij(1,ii,jj)
     &        + facet_pij(4,i,j)*facet_pij(3,ii,jj)
         chk4 = facet_pij(3,i,j)*facet_pij(2,ii,jj)
     &        + facet_pij(4,i,j)*facet_pij(4,ii,jj)
C-      Offsets:
         chk5 = facet_pij(1,i,j)*facet_oi(ii,jj)
     &        + facet_pij(2,i,j)*facet_oj(ii,jj)
     &        + facet_oi(i,j)
         chk6 = facet_pij(3,i,j)*facet_oi(ii,jj)
     &        + facet_pij(4,i,j)*facet_oj(ii,jj)
     &        + facet_oj(i,j)
         IF ( chk1.NE.1 .OR. chk2.NE.0 .OR. chk5.NE.0 .OR.
     &        chk3.NE.0 .OR. chk4.NE.1 .OR. chk6.NE.0 ) THEN
           WRITE(msgBuf,'(2(3A,I3),A)') ' connect ',
     &      edge(i), '.Edge (facet#', j, ' ) to: ',
     &      edge(ii),'.Edge (facet#', jj,' )'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,4I4,2I8)')
     &      ' Bug in Matrix Product:',chk1,chk2,chk3,chk4,chk5,chk6
           CALL PRINT_ERROR( msgBuf, myThid )
           errCnt = errCnt + 1
         ENDIF
        ENDIF
       ENDDO
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &   ' W2_SET_F2F_INDEX: found', errCnt, ' bugs in Matrix product'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_SET_F2F_INDEX'
      ENDIF

      RETURN
      END
