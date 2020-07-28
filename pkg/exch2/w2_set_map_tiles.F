#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C--  File w2_set_map_tiles.F:
C--   Contents
C--   o W2_SET_MAP_TILES :: Set tiles and IO mapping
C--   o FIND_GCD_N       :: Returns the Greatest Common Divisor

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_SET_MAP_TILES

C !INTERFACE:
      SUBROUTINE W2_SET_MAP_TILES( myThid )

C     !DESCRIPTION:
C     Set-up tiles mapping and IO global mapping

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

C     !FUNCTIONS:
      INTEGER  FIND_GCD_N
      EXTERNAL FIND_GCD_N

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER tNx, tNy, fNx, fNy, nbPts, fBaseX, fBaseY
      INTEGER nbTx, nbTy
      INTEGER j, ii, k, tId, tx, ty
      INTEGER divide, nnx(W2_maxNbFacets)
      INTEGER errCnt, tCnt
      LOGICAL tileIsActive, prtFlag
CEOP

C     Set-up tiles mapping and IO global mapping
      WRITE(msgBuf,'(2A)') 'W2_SET_MAP_TILES:',
     &       ' tile mapping within facet and global Map:'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      prtFlag = ABS(W2_printMsg).GE.2
     &       .OR. ( W2_printMsg .NE.0 .AND. myProcId.EQ.0 )

      tNx = sNx
      tNy = sNy
C--   Check that tile dims divide facet dims
      errCnt = 0
      tCnt = 0
      nbPts = 0
      DO j=1,nFacets
        fNx = facet_dims(2*j-1)
        fNy = facet_dims( 2*j )
        nbTx = fNx/tNx
        nbTy = fNy/tNy
        IF ( nbTx*tNx .NE. fNx ) THEN
          WRITE(msgBuf,'(A,I3,2(A,I7))') 'Facet',j,
     &      ' : X-size=', fNx, ' not multiple of sNx=', tNx
          CALL PRINT_ERROR( msgBuf, myThid )
          errCnt = errCnt + 1
        ENDIF
        IF ( nbTy*tNy .NE. fNy ) THEN
          WRITE(msgBuf,'(A,I3,2(A,I7))') 'Facet',j,
     &      ' : Y-size=', fNy, ' not multiple of sNy=', tNy
          CALL PRINT_ERROR( msgBuf, myThid )
          errCnt = errCnt + 1
        ENDIF
        facet_owns(1,j) = tCnt+1
        tCnt = tCnt + nbTx*nbTy
        facet_owns(2,j) = tCnt
        nbPts = nbPts + fNx*fNy
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &   ' W2_SET_MAP_TILES: found', errCnt, ' Fatal errors'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_SET_MAP_TILES'
      ENDIF
C--   Check that domain size and (SIZE.h + blankList) match:
      IF ( tCnt.NE.exch2_nTiles ) THEN
        WRITE(msgBuf,'(A,I8,A)')
     &   'W2_SET_MAP_TILES: Domain Total # of tiles =', tCnt, ' does'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,I8)')
     &   'W2_SET_MAP_TILES:  not match (SIZE.h+blankList)=',exch2_nTiles
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_SET_MAP_TILES'
      ENDIF

      IF ( W2_mapIO.EQ.1 ) THEN
C--   Compact IO map (mostly in Y dir): search for Greatest Common Divisor
C     of all x-size (faster to apply GCD to Nb of Tiles in X):
        k = 0
        nnx(1) = 0
        DO j=1,nFacets
C     skip empty facet
          IF ( facet_dims(2*j-1).GT.0 ) THEN
            k = k + 1
            nnx(k) = facet_dims(2*j-1)/tNx
          ENDIF
        ENDDO
        divide = FIND_GCD_N( nnx, k )
        W2_mapIO = divide*tNx
        WRITE(msgBuf,'(A,2(I5,A))') ' W2_mapIO =', W2_mapIO,
     &                              ' (=', divide, '*sNx)'
        CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

C--   Global Map size:
C     facets stacked in x direction
      exch2_xStack_Nx = 0
      exch2_xStack_Ny = 0
      DO j=1,nFacets
        exch2_xStack_Nx =   exch2_xStack_Nx + facet_dims(2*j-1)
        exch2_xStack_Ny = MAX( exch2_xStack_Ny, facet_dims(2*j) )
      ENDDO
C     facets stacked in y direction
      exch2_yStack_Nx = 0
      exch2_yStack_Ny = 0
      DO j=1,nFacets
        exch2_yStack_Nx = MAX( exch2_yStack_Nx, facet_dims(2*j-1) )
        exch2_yStack_Ny =   exch2_yStack_Ny + facet_dims(2*j)
      ENDDO
      IF ( W2_mapIO.EQ.-1 ) THEN
        exch2_global_Nx = exch2_xStack_Nx
        exch2_global_Ny = exch2_xStack_Ny
      ELSEIF ( W2_mapIO.EQ.0 ) THEN
        exch2_global_Nx = nbPts
        exch2_global_Ny = 1
      ELSE
        exch2_global_Nx = W2_mapIO
        exch2_global_Ny = nbPts/W2_mapIO
      ENDIF
      WRITE(msgBuf,'(A,2(A,I8))') ' Global Map (IO):',
     &  ' X-size=', exch2_global_Nx, ' , Y-size=', exch2_global_Ny
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

C--   Set tiles mapping within facet (sub-domain) and within Global Map
      WRITE(msgBuf,'(2A)') 'W2_SET_MAP_TILES:',
     &       ' tile offset within facet and global Map:'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      tId = 0
      nbPts = 0
      fBaseX = 0
      fBaseY = 0
      DO j=1,nFacets
        fNx = facet_dims(2*j-1)
        fNy = facet_dims( 2*j )
        nbTx = fNx/tNx
        nbTy = fNy/tNy
        WRITE(W2_oUnit,'(A,I3,2(A,I6),A,I8,2(A,I4),A)')
     &    '- facet', j, ' : X-size=', fNx, ' , Y-size=', fNy,
     &    ' ;', nbTx*nbTy, ' tiles (Tx,Ty=', nbTx,',',nbTy,')'
c       CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
        DO ty=1,nbTy
         DO tx=1,nbTx
          tId = tId + 1
C--   Tags blank tile by removing facet # (exch2_myFace) but keeps its location
          tileIsActive = .TRUE.
          DO k=1,nBlankTiles
           IF ( blankList(k).EQ.tId ) tileIsActive = .FALSE.
          ENDDO
          IF ( tileIsActive ) exch2_myFace(tId) = j
          exch2_mydNx ( tId ) = fNx
          exch2_mydNy ( tId ) = fNy
          exch2_tNx   ( tId ) = tNx
          exch2_tNy   ( tId ) = tNy
          exch2_tBasex( tId ) = (tx-1)*tNx
          exch2_tBasey( tId ) = (ty-1)*tNy
C--   Global IO Mappings
C       these are for OBCS (vertical slices)
          exch2_txXStackLo( tId ) = 1 + exch2_tBasex(tId) + fBaseX
          exch2_tyXStackLo( tId ) = 1 + exch2_tBasey(tId)
          exch2_txYStackLo( tId ) = 1 + exch2_tBasex(tId)
          exch2_tyYStackLo( tId ) = 1 + exch2_tBasey(tId) + fBaseY
C       and these for global files (3d files/horizontal 2d files)
          IF ( W2_mapIO.EQ.-1 ) THEN
C-        Old format
            exch2_txGlobalo( tId ) = 1 + exch2_tBasex(tId) + fBaseX
            exch2_tyGlobalo( tId ) = 1 + exch2_tBasey(tId)
          ELSEIF ( W2_mapIO.EQ.0 ) THEN
C-        Compact format = 1 long line
            ii = nbPts + exch2_tBasex(tId) + exch2_tBasey(tId)*fNx
            exch2_txGlobalo( tId ) = 1 + ii
            exch2_tyGlobalo( tId ) = 1
          ELSE
C         Compact format: piled in the Y direction
            ii = nbPts + exch2_tBasex(tId) + exch2_tBasey(tId)*fNx
            exch2_txGlobalo( tId ) = 1 + MOD(ii,W2_mapIO)
            exch2_tyGlobalo( tId ) = 1 + ii/W2_mapIO
          ENDIF
          IF ( prtFlag )
     &    WRITE(W2_oUnit,'(A,I8,3(A,I3),2A,2I5,2A,2I8)') '  tile',tId,
     &    ' on facet', exch2_myFace(tId),' (',tx,',',ty,'):',
     &         ' offset=', exch2_tBasex(tId), exch2_tBasey(tId),' ;',
     &    ' on Glob.Map=', exch2_txGlobalo(tId),exch2_tyGlobalo(tId)
         ENDDO
        ENDDO
        fBaseX = fBaseX + fNx
        fBaseY = fBaseY + fNy
        nbPts = nbPts + fNx*fNy
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_GCD_N

C     !INTERFACE:
      INTEGER FUNCTION FIND_GCD_N( fldList, nFld )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION FIND_GCD_N
C     | o Find the Greatest Common Divisor of N integers
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
C     fldList :: list of integers to search for GCD
C     nFLd    :: length of the input integer list.
      INTEGER nFLd
      INTEGER fldList(nFld)

C     !LOCAL VARIABLES:
      INTEGER mnFld, divide
      INTEGER j, ii
      LOGICAL flag
      LOGICAL localDBg
CEOP
      PARAMETER ( localDBg = .FALSE. )
c     PARAMETER ( localDBg = .TRUE. )

      mnFld = fldList(1)
      DO j=1,nFld
        mnFld = MIN( mnFld, fldList(j) )
      ENDDO
      IF (localDBg) WRITE(0,'(A,I8)') 'FIND_GCD_N: mnFld=',mnFld

      IF (mnFld.GT.1 ) THEN
        divide = 1
        ii = 2
        DO WHILE ( ii.LE.mnFld )
          IF (localDBg) WRITE(0,'(A,I8)') '     GCD  : try',ii
          flag = .TRUE.
          DO j=1,nFld
            flag = flag.AND.(MOD(fldList(j),ii).EQ.0 )
          ENDDO
          IF ( flag ) THEN
            divide = divide*ii
            DO j=1,nFld
              fldList(j) = fldList(j)/ii
            ENDDO
            IF (localDBg) WRITE(0,'(A,I8)')
     &                              'FIND_GCD_N: com.fact=',ii
            mnFld = mnFld/ii
          ELSE
            ii = ii+2
            IF (ii.EQ.4) ii=3
          ENDIF
        ENDDO
C-    Put back the original Nb:
        IF (localDBg) WRITE(0,'(10I8)') (fldList(j),j=1,nFld)
        DO j=1,nFld
          fldList(j) = fldList(j)*divide
        ENDDO
      ELSE
        divide = MAX( 0, mnFld )
      ENDIF

      FIND_GCD_N = divide

      RETURN
      END
