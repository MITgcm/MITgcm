#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

CBOP 0
C !ROUTINE: W2_SET_MAP_CUMSUM

C !INTERFACE:
      SUBROUTINE W2_SET_MAP_CUMSUM( myThid )

C     !DESCRIPTION:
C     Set-up mapping for global cumulated sum.

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
C     facetXYSum   :: Tile to Facet Matrix for facet-Increment in X & Y dir.
C     facet_CSum   :: Tile to Facet Matrix for CumSum @ facet-origin
C     msgBuf       :: Informational/error message buffer
      INTEGER fNx, fNy, nbTx, nbTy
      INTEGER nActiveFacets
      INTEGER fCnt, prev_fCnt
      INTEGER npass, nType
      LOGICAL prtFlag
      LOGICAL fIsSet(0:W2_maxNbFacets)
      INTEGER tN, i, j, k, ii, jj
#ifdef W2_CUMSUM_USE_MATRIX
      INTEGER tS, bi, bj, l, is, ie
      INTEGER facetXYSum(2,W2_maxNbTiles,W2_maxNbFacets)
      INTEGER facet_CSum(2,W2_maxNbTiles,W2_maxNbFacets)
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP
c      DATA edge / 'N' , 'S' , 'E' , 'W' /

      WRITE(msgBuf,'(2A)') 'W2_SET_MAP_CUMSUM: ',
     &       'setting Facet Matrix for CUMUL-SUM'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      prtFlag = ABS(W2_printMsg).GE.2
     &       .OR. ( W2_printMsg .NE.0 .AND. myProcId.EQ.0 )

C--   Initialise Common-block
      W2_tMC1 = 0
      W2_tMC2 = 0
      DO j=1,W2_maxNbFacets
       DO i=1,W2_maxNbFacets
         W2_cumSum_facet(1,i,j) = 0
         W2_cumSum_facet(2,i,j) = 0
       ENDDO
      ENDDO
#ifdef W2_CUMSUM_USE_MATRIX
      DO j=1,W2_maxNbTiles
       DO i=1,W2_maxNbTiles
         W2_cumSum_tiles(1,i,j) = 0
         W2_cumSum_tiles(2,i,j) = 0
       ENDDO
      ENDDO
#endif /* W2_CUMSUM_USE_MATRIX */

C--   Start setting cumul-sum Face mapping
      fCnt = 0
      fIsSet(0) = .TRUE.
      DO j=1,nFacets
        fIsSet(j) = .FALSE.
      ENDDO

C--   Start with first non-empty face:
      nActiveFacets = 0
      DO j=1,nFacets
        IF ( facet_dims(2*j-1)*facet_dims(2*j).GE.1 ) THEN
          nActiveFacets = nActiveFacets + 1
          IF ( fCnt.EQ.0 ) THEN
            fIsSet(j) = .TRUE.
            fCnt = 1
            IF ( ABS(W2_printMsg).GE.2 ) WRITE(W2_oUnit,'(A,I4)')
     &        ' CumSum starts @ SW.corner of facet #', j
          ENDIF
        ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Go through list of connections:
      prev_fCnt = 0
      npass = 0
      DO WHILE ( fCnt.GT.prev_fCnt )
       npass = npass + 1
       prev_fCnt = fCnt
       DO nType=1,3
        IF ( fCnt.EQ.prev_fCnt ) THEN
         DO j=1,nFacets
          IF ( fIsSet(j) ) THEN
           DO i=1,4
C-    connected to:
            jj = INT(facet_link(i,j))
            ii = MOD( NINT(facet_link(i,j)*10.), 10 )
C--   1) with same orientation ( N <-> S or E <-> W ), forward progression
            IF ( .NOT.fIsSet(jj) .AND. nType.EQ.1 ) THEN
C-    N <-- S ;
             IF ( i.EQ.W2_NORTH .AND. ii.EQ.W2_SOUTH ) THEN
              DO k=1,nFacets
               W2_cumSum_facet(1,k,jj) = W2_cumSum_facet(1,k,j)
               W2_cumSum_facet(2,k,jj) = W2_cumSum_facet(2,k,j)
              ENDDO
              W2_cumSum_facet(2,j,jj) = W2_cumSum_facet(2,j,jj) + 1
              fCnt = fCnt + 1
              fIsSet(jj) = .TRUE.
              IF ( ABS(W2_printMsg).GE.2 ) WRITE(W2_oUnit,'(5(A,I4))')
     &         ' CumSum SW.corner of facet #', jj,' set from facet',j,
     &         ' (pass,type=', npass,',', nType,')'
             ENDIF
C-    E <-- W ;
             IF ( i.EQ.W2_EAST .AND. ii.EQ.W2_WEST ) THEN
              DO k=1,nFacets
               W2_cumSum_facet(1,k,jj) = W2_cumSum_facet(1,k,j)
               W2_cumSum_facet(2,k,jj) = W2_cumSum_facet(2,k,j)
              ENDDO
              W2_cumSum_facet(1,j,jj) = W2_cumSum_facet(1,j,jj) + 1
              fCnt = fCnt + 1
              fIsSet(jj) = .TRUE.
              IF ( ABS(W2_printMsg).GE.2 ) WRITE(W2_oUnit,'(5(A,I4))')
     &         ' CumSum SW.corner of facet #', jj,' set from facet',j,
     &         ' (pass,type=', npass,',', nType,')'
             ENDIF
            ENDIF
C--   2) with same orientation ( N <-> S or E <-> W ), backward progression
            IF ( .NOT.fIsSet(jj) .AND. nType.EQ.2 ) THEN
C-    S <-- N ;
             IF ( i.EQ.W2_SOUTH .AND. ii.EQ.W2_NORTH ) THEN
              DO k=1,nFacets
               W2_cumSum_facet(1,k,jj) = W2_cumSum_facet(1,k,j)
               W2_cumSum_facet(2,k,jj) = W2_cumSum_facet(2,k,j)
              ENDDO
              W2_cumSum_facet(2,jj,jj) = W2_cumSum_facet(2,jj,jj) - 1
              fCnt = fCnt + 1
              fIsSet(jj) = .TRUE.
              IF ( ABS(W2_printMsg).GE.2 ) WRITE(W2_oUnit,'(5(A,I4))')
     &         ' CumSum SW.corner of facet #', jj,' set from facet',j,
     &         ' (pass,type=', npass,',', nType,')'
             ENDIF
C-    W <-- E ;
             IF ( i.EQ.W2_WEST .AND. ii.EQ.W2_EAST ) THEN
              DO k=1,nFacets
               W2_cumSum_facet(1,k,jj) = W2_cumSum_facet(1,k,j)
               W2_cumSum_facet(2,k,jj) = W2_cumSum_facet(2,k,j)
              ENDDO
              W2_cumSum_facet(1,jj,jj) = W2_cumSum_facet(1,jj,jj) - 1
              fCnt = fCnt + 1
              fIsSet(jj) = .TRUE.
              IF ( ABS(W2_printMsg).GE.2 ) WRITE(W2_oUnit,'(5(A,I4))')
     &         ' CumSum SW.corner of facet #', jj,' set from facet',j,
     &         ' (pass,type=', npass,',', nType,')'
             ENDIF
            ENDIF
C--   3) with different orientation ( N <-> W or S <-> E )
C- Note: cannot rely on these connections for Cumul-Sum @ grid-cell center
            IF ( .NOT.fIsSet(jj) .AND. nType.EQ.3
     &                           .AND. fCnt.EQ.prev_fCnt ) THEN
C-    N <-- W or W <-- N ;
             IF (  ( i.EQ.W2_NORTH .AND. ii.EQ.W2_WEST  ) .OR.
     &             ( i.EQ.W2_WEST  .AND. ii.EQ.W2_NORTH ) ) THEN
              DO k=1,nFacets
               W2_cumSum_facet(1,k,jj) = W2_cumSum_facet(1,k,j)
               W2_cumSum_facet(2,k,jj) = W2_cumSum_facet(2,k,j)
              ENDDO
              W2_cumSum_facet(2,j, jj) = W2_cumSum_facet(2,j, jj) + 1
              W2_cumSum_facet(2,jj,jj) = W2_cumSum_facet(2,jj,jj) - 1
              fCnt = fCnt + 1
              fIsSet(jj) = .TRUE.
              IF ( ABS(W2_printMsg).GE.2 ) WRITE(W2_oUnit,'(5(A,I4))')
     &         ' CumSum SW.corner of facet #', jj,' set from facet',j,
     &         ' (pass,type=', npass,',', nType,')'
             ENDIF
C-    E <-- S or S <-- E ;
             IF (  ( i.EQ.W2_EAST  .AND. ii.EQ.W2_SOUTH ) .OR.
     &             ( i.EQ.W2_SOUTH .AND. ii.EQ.W2_EAST  ) ) THEN
              DO k=1,nFacets
               W2_cumSum_facet(1,k,jj) = W2_cumSum_facet(1,k,j)
               W2_cumSum_facet(2,k,jj) = W2_cumSum_facet(2,k,j)
              ENDDO
              W2_cumSum_facet(1,j, jj) = W2_cumSum_facet(1,j, jj) + 1
              W2_cumSum_facet(1,jj,jj) = W2_cumSum_facet(1,jj,jj) - 1
              fCnt = fCnt + 1
              fIsSet(jj) = .TRUE.
              IF ( ABS(W2_printMsg).GE.2 ) WRITE(W2_oUnit,'(5(A,I4))')
     &         ' CumSum SW.corner of facet #', jj,' set from facet',j,
     &         ' (pass,type=', npass,',', nType,')'
             ENDIF
            ENDIF
C          end i loop
           ENDDO
          ENDIF
C        end facets loop
         ENDDO
         IF ( fCnt.GT.prev_fCnt ) THEN
          WRITE(msgBuf,'(2A,3(I4,A),I2,A)') 'W2_SET_MAP_CUMSUM: ',
     &     'set ', fCnt - prev_fCnt, ' /', nActiveFacets,
     &     ' active facets (pass,type=', npass, ',', nType, ')'
          CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDIF
C      end nType loop
       ENDDO
C-    end do while (npass count)
      ENDDO
      IF ( fCnt.LT.nActiveFacets ) THEN
        WRITE(msgBuf,'(2A,2(I4,A))') 'W2_SET_MAP_CUMSUM: ',
     &  'Only get', fCnt, ' /', nActiveFacets,' active facets done'
        CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** W2_SET_MAP_CUMSUM: ',
     &    ' missing connections in Cumulated Sum'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( myProcId.EQ.0 ) THEN
        WRITE(W2_oUnit,'(2A,2(I4,A))')' Facet Matrix for CUMUL-SUM (',
     &         'nFacets=',nFacets, ', nActive=', nActiveFacets, ' ):'
        DO j=1,nFacets
          WRITE(W2_oUnit,'(A,I3,A,30(2I3,A))') '- facet', j, ' :',
     &         (W2_cumSum_facet(1,i,j),W2_cumSum_facet(2,i,j),' ,',
     &                                 i=1,nFacets)
        ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   record "missing corner" tile

      IF ( useCubedSphereExchange ) THEN
       DO j=1,nFacets
        fNx = facet_dims(2*j-1)
        fNy = facet_dims( 2*j )
        IF ( fNx*fNy .GE. 1 ) THEN
         nbTx = fNx/sNx
         nbTy = fNy/sNy
         tN = facet_owns(1,j) - 1 + nbTx
         IF ( W2_tMC2.EQ.0 .AND. MOD(j,2).EQ.0
     &                     .AND. exch2_myFace(tN).NE.0 ) W2_tMC2 = tN
         tN = facet_owns(1,j) + (nbTy-1)*nbTx
         IF ( W2_tMC1.EQ.0 .AND. MOD(j,2).EQ.1
     &                     .AND. exch2_myFace(tN).NE.0 ) W2_tMC1 = tN
        ENDIF
       ENDDO
       IF ( myProcId.EQ.0 ) WRITE(W2_oUnit,'(3(A,I8))')
     &   ' missing-corner Tile for CUMUL-SUM (nTiles=', exch2_nTiles,
     &   ' ): W2_tMC1=', W2_tMC1, ' , W2_tMC2=', W2_tMC2
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Now Set cumul-sum Tile mapping

#ifdef W2_CUMSUM_USE_MATRIX
      DO j=1,W2_maxNbFacets
       DO i=1,W2_maxNbTiles
        facetXYSum(1,i,j) = 0
        facetXYSum(2,i,j) = 0
        facet_CSum(1,i,j) = 0
        facet_CSum(2,i,j) = 0
       ENDDO
      ENDDO

C-    First within each face:
      DO j=1,nFacets
       fNx = facet_dims(2*j-1)
       fNy = facet_dims( 2*j )
       IF ( fNx*fNy .GE. 1 ) THEN
        nbTx = fNx/sNx
        nbTy = fNy/sNy

        DO bj=1,nbTy
         DO bi=1,nbTx-1
           tS = facet_owns(1,j) - 1 + bi
           tN = tS + 1 + (bj-1)*nbTx
           DO k=facet_owns(1,j),tS
            W2_cumSum_tiles(1,k,tN) = 1
           ENDDO
         ENDDO
        ENDDO
        tN = facet_owns(1,j) - 1 + nbTx
        facetXYSum(1,tN,j) = 1
        DO k=facet_owns(1,j),tN-1
          facetXYSum(1,k,j) = W2_cumSum_tiles(1,k,tN)
        ENDDO

        DO bj=1,nbTy-1
         DO bi=1,nbTx
          tS = facet_owns(1,j) - 1 + bi
          tN = tS + bj*nbTx
          DO k=1,bj
           l = tS + (k-1)*nbTx
           W2_cumSum_tiles(2,l,tN) = 1
          ENDDO
         ENDDO
        ENDDO
        tN = facet_owns(1,j) + (nbTy-1)*nbTx
        facetXYSum(2,tN,j) = 1
        DO k=facet_owns(1,j),tN-1
          facetXYSum(2,k,j) = W2_cumSum_tiles(2,k,tN)
        ENDDO

       ENDIF
      ENDDO

C-    Then across facet:
      DO j=1,nFacets
       DO k=1,exch2_nTiles
        DO i=1,nFacets
        facet_CSum(1,k,j) = facet_CSum(1,k,j)
     &                    + W2_cumSum_facet(1,i,j)*facetXYSum(1,k,i)
        facet_CSum(2,k,j) = facet_CSum(2,k,j)
     &                    + W2_cumSum_facet(2,i,j)*facetXYSum(2,k,i)
        ENDDO
       ENDDO
      ENDDO

C-    Finally, account for cumulated sum at facet origin:
      DO j=1,nFacets
       DO tN=facet_owns(1,j),facet_owns(2,j)
        DO k=1,exch2_nTiles
         W2_cumSum_tiles(1,k,tN) = W2_cumSum_tiles(1,k,tN)
     &                           + facet_CSum(1,k,j)
         W2_cumSum_tiles(2,k,tN) = W2_cumSum_tiles(2,k,tN)
     &                           + facet_CSum(2,k,j)
        ENDDO
       ENDDO
      ENDDO

      IF (prtFlag) THEN
       WRITE(W2_oUnit,'(A,I8,A)')
     &    ' Tile Matrix for CUMUL-SUM (nTiles=', exch2_nTiles, ' ):'
       DO j=1,exch2_nTiles
        DO is=1,exch2_nTiles,10
         ie = MIN(is+9,exch2_nTiles)
         IF ( is.EQ.1 ) THEN
          WRITE(W2_oUnit,'(3(I8,A),10(2I3,A))') j,' ,',is,' ->',ie,' :',
     &     (W2_cumSum_tiles(1,i,j),W2_cumSum_tiles(2,i,j),' ,',i=is,ie)
         ELSE
          WRITE(W2_oUnit,'(8X,2(I8,A),10(2I3,A))') is,' ->',ie,' :',
     &     (W2_cumSum_tiles(1,i,j),W2_cumSum_tiles(2,i,j),' ,',i=is,ie)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      WRITE(msgBuf,'(2A)') 'W2_SET_MAP_CUMSUM: ',
     &  'setting Tile Matrix for CUMUL-SUM : done'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

#else /* W2_CUMSUM_USE_MATRIX */
      WRITE(msgBuf,'(2A)') 'W2_SET_MAP_CUMSUM: ',
     &  'done (skip Tile Matrix setting)'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
#endif /* W2_CUMSUM_USE_MATRIX */

      RETURN
      END
