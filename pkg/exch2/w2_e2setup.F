#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_E2SETUP

C !INTERFACE:
      SUBROUTINE W2_E2SETUP( myThid )

C     !DESCRIPTION:
C     Set-up W2_EXCH2 tile topology structures

C     !USES:
      IMPLICIT NONE

C      Tile topology settings data structures
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif

C     !INPUT PARAMETERS:
C     myThid  :: my Thread Id number
C               (Note: not relevant since threading has not yet started)
      INTEGER myThid

#ifdef ALLOW_EXCH2

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     stdUnit    :: Standard-Output IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER stdUnit
      INTEGER i, j, k
      LOGICAL addBlank
CEOP

      stdUnit = standardMessageUnit

C--   Initialise parameters from EXCH2_PARAMS common blocks
C     (except params from namelist which are set in W2_READPARMS)
      DO j=1,W2_maxNbFacets
        facet_owns(1,j) = 0
        facet_owns(2,j) = 0
        DO i=1,4
         DO k=1,4
          facet_pij(k,i,j) = 0
         ENDDO
          facet_oi(i,j) = 0
          facet_oj(i,j) = 0
        ENDDO
      ENDDO

C--   Count Nb of Blank-Tiles and set Number of tiles:
      nBlankTiles = 0
      DO i=1,W2_maxNbTiles
       IF (blankList(i).NE.0 ) THEN
         addBlank = .TRUE.
         DO j=1,nBlankTiles
          IF ( blankList(i).EQ.blankList(j) ) THEN
           addBlank = .FALSE.
           WRITE(msgBuf,'(A,I8,A,2I8,A)')
     &     '** WARNING ** W2_E2SETUP: #', blankList(i),
     &     ' appears several times in blankList (',j,i,')'
           CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
         ENDDO
         IF ( addBlank ) THEN
           nBlankTiles = nBlankTiles + 1
           blankList(nBlankTiles) = blankList(i)
         ENDIF
       ENDIF
      ENDDO
      exch2_nTiles = nBlankTiles + (nSx*nSy*nPx*nPy)

      WRITE(msgBuf,'(A,I8)')
     &    'W2_E2SETUP: number of Active Tiles =', nSx*nSy*nPx*nPy
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,I8)')
     &    'W2_E2SETUP: number of Blank Tiles  =', nBlankTiles
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,I8)')
     &    'W2_E2SETUP: Total number of Tiles  =', exch2_nTiles
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

      IF ( exch2_nTiles.GT.W2_maxNbTiles ) THEN
        WRITE(msgBuf,'(3(A,I8))') 'W2_E2SETUP: Number of Tiles=',
     &             exch2_nTiles, ' >', W2_maxNbTiles, ' =W2_maxNbTiles'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'Must increase "W2_maxNbTiles"',
     &                       ' in "W2_EXCH2_SIZE.h" + recompile'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R W2_E2SETUP (nTiles>maxNbTiles)'
      ENDIF

C--   Check blankList:
      DO i=1,nBlankTiles
       IF ( blankList(i).LT.1 .OR. blankList(i).GT.exch2_nTiles ) THEN
         WRITE(msgBuf,'(A,I8,A,I8)')
     &     'W2_E2SETUP: Invalid blankTile number (i=', i,
     &     ' )=', blankList(i)
         WRITE(msgBuf,'(A,I8,A,I8,A)') 'W2_E2SETUP:', blankList(i),
     &                    ' = Invalid blankTile number (i=', i, ')'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R W2_E2SETUP (blankList error)'
       ENDIF
      ENDDO

C--   Define Facet (sub-domain) Topology: Size and Connections
      IF     ( preDefTopol.EQ.0 ) THEN
        CALL W2_SET_GEN_FACETS( myThid )
      ELSEIF ( preDefTopol.EQ.1 ) THEN
        CALL W2_SET_SINGLE_FACET( myThid )
      ELSEIF ( preDefTopol.EQ.2 ) THEN
        CALL W2_SET_MYOWN_FACETS( myThid )
      ELSEIF ( preDefTopol.EQ.3 ) THEN
        CALL W2_SET_CS6_FACETS( myThid )
      ELSE
        STOP 'ABNORMAL END: S/R W2_E2SETUP (invalid preDefTopol)'
      ENDIF

      WRITE(msgBuf,'(A,I8)')
     &    'W2_E2SETUP: Total number of Facets =', nFacets
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

C--   Check Topology; setup correspondence matrix for connected Facet-Edges
      CALL W2_SET_F2F_INDEX( myThid )

C--   Define Tile Mapping (+ IO global mapping)
      CALL W2_SET_MAP_TILES( myThid )

C--   Define Tile Mapping (for Cumulated Sum)
      CALL W2_SET_MAP_CUMSUM( myThid )

C--   Set-up tile neighbours and index relations for EXCH2
      CALL W2_SET_TILE2TILES( myThid )

#endif /* ALLOW_EXCH2 */

      RETURN
      END
