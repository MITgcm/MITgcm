#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_PRINT_E2SETUP

C !INTERFACE:
      SUBROUTINE W2_PRINT_E2SETUP( myThid )

C     !DESCRIPTION:
C     Print out Wrapper-Exch2 Set-Up as defined by matlab generated source
C     files (W2_EXCH2_SIZE.h & W2_E2SETUP). Allows a direct comparison
C     with standard Fortran src generated topology.

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
      CHARACTER*1 edge(0:4)
      INTEGER tNx, tNy, fNx, fNy
      INTEGER nbTx, nbTy
      INTEGER ip(4), np(4)
      INTEGER i, j, js, jp, jt, ii, is, it, ns, nt, k, tx, ty
      LOGICAL prtFlag
CEOP
      DATA edge / '?' , 'N' , 'S' , 'E' , 'W' /

      tNx = sNx
      tNy = sNy
      prtFlag = ABS(W2_printMsg).GE.2
     &       .OR. ( W2_printMsg .NE.0 .AND. myProcId.EQ.0 )

C=================== from W2_SET_F2F_INDEX :
c     WRITE(msgBuf,'(2A)') 'W2_SET_F2F_INDEX:',
      WRITE(msgBuf,'(2A)') 'W2_PRINT_E2SETUP:',
     &       ' index matrix for connected Facet-Edges:'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

      jp = 0
      IF ( prtFlag ) THEN
       DO is=1,exch2_nTiles
        js  = exch2_myFace(is)
        IF ( js.NE.0 ) THEN
C--     tile is is active
         fNx = exch2_mydNx(is)
         fNy = exch2_mydNy(is)
         nbTx = fNx/tNx
         nbTy = fNy/tNy
         IF ( js.NE.jp ) THEN
           IF ( jp.NE.0 ) THEN
C---     write
         DO i=1,4
          IF ( ip(i).NE.0 ) THEN
            j  = exch2_myFace(ip(i))
            it = exch2_neighbourId (np(i),ip(i))
            nt = exch2_opposingSend(np(i),ip(i))
            jt = exch2_myFace(it)
            ii = 0
            IF ( exch2_jLo(nt,it).EQ.exch2_jHi(nt,it) )
     &        ii = 2 - MIN(1,exch2_jHi(nt,it))
            IF ( exch2_iLo(nt,it).EQ.exch2_iHi(nt,it) )
     &        ii = 4 - MIN(1,exch2_iHi(nt,it))
            WRITE(W2_oUnit,'(2(3A,I3),A,4I3,A,2I6)')
     &      '  ', edge(i), '.Edge Facet', j, ' <-- ',
     &           edge(ii), '.Edge Facet', jt,
     &      ' : pij=', (exch2_pij(k,np(i),ip(i)),k=1,4),
     &      ' ; oi,oj=',exch2_oi(np(i),ip(i)),exch2_oj(np(i),ip(i))
          ENDIF
         ENDDO
C---
           ENDIF
           jp = js
           DO i=1,4
            ip(i) = 0
            np(i) = 0
           ENDDO
         ENDIF
         DO ns=1,exch2_nNeighbours(is)
          IF ( ip(1).EQ.0 .AND. exch2_isNedge(is).EQ.1
     &                    .AND. exch2_jLo(ns,is).EQ.(tNy+1)
     &                    .AND. exch2_jHi(ns,is).EQ.(tNy+1) ) THEN
            ip(1) = is
            np(1) = ns
          ENDIF
          IF ( ip(2).EQ.0 .AND. exch2_isSedge(is).EQ.1
     &                    .AND. exch2_jLo(ns,is).EQ. 0
     &                    .AND. exch2_jHi(ns,is).EQ. 0 ) THEN
            ip(2) = is
            np(2) = ns
          ENDIF
          IF ( ip(3).EQ.0 .AND. exch2_isEedge(is).EQ.1
     &                    .AND. exch2_iLo(ns,is).EQ.(tNx+1)
     &                    .AND. exch2_iHi(ns,is).EQ.(tNx+1) ) THEN
            ip(3) = is
            np(3) = ns
          ENDIF
          IF ( ip(4).EQ.0 .AND. exch2_isWedge(is).EQ.1
     &                    .AND. exch2_iLo(ns,is).EQ. 0
     &                    .AND. exch2_iHi(ns,is).EQ. 0 ) THEN
            ip(4) = is
            np(4) = ns
          ENDIF
         ENDDO

C--     end if active tile
        ENDIF
       ENDDO
C---   write the last one:
         DO i=1,4
          IF ( ip(i).NE.0 ) THEN
            j  = exch2_myFace(ip(i))
            it = exch2_neighbourId (np(i),ip(i))
            nt = exch2_opposingSend(np(i),ip(i))
            jt = exch2_myFace(it)
            ii = 0
            IF ( exch2_jLo(nt,it).EQ.exch2_jHi(nt,it) )
     &        ii = 2 - MIN(1,exch2_jHi(nt,it))
            IF ( exch2_iLo(nt,it).EQ.exch2_iHi(nt,it) )
     &        ii = 4 - MIN(1,exch2_iHi(nt,it))
            WRITE(W2_oUnit,'(2(3A,I3),A,4I3,A,2I6)')
     &      '  ', edge(i), '.Edge Facet', j, ' <-- ',
     &           edge(ii), '.Edge Facet', jt,
     &      ' : pij=', (exch2_pij(k,np(i),ip(i)),k=1,4),
     &      ' ; oi,oj=',exch2_oi(np(i),ip(i)),exch2_oj(np(i),ip(i))
          ENDIF
         ENDDO
C---
      ENDIF

C=================== from W2_SET_MAP_TILES :

C     Set-up tiles mapping and IO global mapping
c     WRITE(msgBuf,'(2A)') 'W2_SET_MAP_TILES:',
      WRITE(msgBuf,'(2A)') 'W2_PRINT_E2SETUP:',
     &       ' tile mapping within facet and global Map:'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

C--   Check that tile dims divide facet dims
C--   Check that domain size and (SIZE.h + blankList) match:
C--   Compact IO map (mostly in Y dir): search for Greatest Common Divisor
C     of all x-size (faster to apply GCD to Nb of Tiles in X):

      WRITE(msgBuf,'(A,2(A,I8))') ' Global Map (IO):',
     &  ' X-size=', exch2_global_Nx, ' , Y-size=', exch2_global_Ny
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

C--   Set tiles mapping within facet (sub-domain) and within Global Map
c     WRITE(msgBuf,'(2A)') 'W2_SET_MAP_TILES:',
      WRITE(msgBuf,'(2A)') 'W2_PRINT_E2SETUP:',
     &       ' tile offset within facet and global Map:'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      jp = 0
      DO is=1,exch2_nTiles
       js  = exch2_myFace(is)
       IF ( js.NE.0 ) THEN
         fNx = exch2_mydNx(is)
         fNy = exch2_mydNy(is)
         nbTx = fNx/tNx
         nbTy = fNy/tNy
         IF ( js .NE. jp )
     &   WRITE(W2_oUnit,'(A,I3,2(A,I6),A,I5,2(A,I4),A)')
     &    '- facet', js, ' : X-size=', fNx, ' , Y-size=', fNy,
     &    ' ;', nbTx*nbTy, ' tiles (Tx,Ty=', nbTx,',',nbTy,')'
         jp = js
         IF ( prtFlag ) THEN
           tx = 1 + exch2_tBasex(is)/tNx
           ty = 1 + exch2_tBasey(is)/tNy
          WRITE(W2_oUnit,'(A,I5,3(A,I3),2A,2I5,2A,2I8)') '  tile',is,
     &    ' on facet', exch2_myFace(is),' (',tx,',',ty,'):',
     &         ' offset=', exch2_tBasex(is), exch2_tBasey(is),' ;',
     &    ' on Glob.Map=', exch2_txGlobalo(is),exch2_tyGlobalo(is)
         ENDIF
       ENDIF
      ENDDO

C=================== from W2_SET_TILE2TILES :
c     WRITE(msgBuf,'(2A)') 'W2_SET_TILE2TILES:',
      WRITE(msgBuf,'(2A)') 'W2_PRINT_E2SETUP:',
     &       ' tile neighbours and index connection:'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

      it = 1
      DO is=1,exch2_nTiles
       js  = exch2_myFace(is)
       IF ( js.NE.0 ) THEN
        IF ( exch2_nNeighbours(is).GT.exch2_nNeighbours(it) ) it = is
        IF ( prtFlag ) THEN
         WRITE(W2_oUnit,'(A,I8,A,I3,A,4(A,I2))') 'Tile',is,
     &    ' : nbNeighb=',exch2_nNeighbours(is),' ; is-at-Facet-Edge:',
     &        ' N=', exch2_isNedge(is), ' , S=', exch2_isSedge(is),
     &      ' , E=', exch2_isEedge(is), ' , W=', exch2_isWedge(is)
         DO ns=1,exch2_nNeighbours(is)
          WRITE(W2_oUnit,'(A,I3,A,I8,2(A,2I6),A,4I3,A,2I6,A)')
     &     ' ns:',ns,' it=',exch2_neighbourId(ns,is),
     &     ', iLo,iHi=', exch2_iLo(ns,is), exch2_iHi(ns,is),
     &     ', jLo,jHi=', exch2_jLo(ns,is), exch2_jHi(ns,is)
c    &     , ' (pij=',(exch2_pij(k,ns,is),k=1,4),
c    &     ', oi,oj=', exch2_oi(ns,is), exch2_oj(ns,is),')'
         ENDDO
        ENDIF
       ENDIF
      ENDDO
      IF ( it.NE.0 ) THEN
       WRITE(msgBuf,'(A,I5,A,I3)')
     &  'current Max.Nb.Neighbours (e.g., on tile',it,
     &  ' ) =', exch2_nNeighbours(it)
       CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

      RETURN
      END
