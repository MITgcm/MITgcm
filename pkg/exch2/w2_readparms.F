C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_readparms.F,v 1.5 2012/09/04 00:44:30 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_READPARMS

C !INTERFACE:
      SUBROUTINE W2_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize W2_EXCH2 variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"

C     !INPUT PARAMETERS:
C     myThid  :: my Thread Id number
C               (Note: not relevant since threading has not yet started)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL fileExist, errFlag
      INTEGER i, j, iUnit, stdUnit, errCnt
C--   Note: To avoid error in reading the namelist,
C     use larger local array to read-in lists dimsFacets & facetEdgeLink,
C     store only W2_maxNbFacets values, and stop if more values are found.
      INTEGER namList_NbFacets
      PARAMETER ( namList_NbFacets = W2_maxNbFacets*2 )
      INTEGER dimsFacets( 2*namList_NbFacets )
      Real*4  facetEdgeLink( 4, namList_NbFacets )

C--   topology defined from processing "data.exch2" (selectTopol=0):
C     dimsFacets    :: facet pair of dimensions (n1x,n1y,n2x,n2y ...)
C     facetEdgeLink :: Face-Edge connectivity map:
C       facetEdgeLink(i,j)=XX.1 : face(j)-edge(i) (i=1,2,3,4 <==> N,S,E,W)
C       is connected to Northern edge of face "XX" ; similarly,
C       = XX.2 : to Southern.E, XX.3 = Eastern.E, XX.4 = Western.E of face "XX".
C--
C     edges order: N,S,E,W <==> 1,2,3,4

      NAMELIST /W2_EXCH2_PARM01/
     &       preDefTopol,
     &       dimsFacets, facetEdgeLink,
     &       blankList,
     &       W2_mapIO,
     &       W2_printMsg,
     &       W2_useE2ioLayOut

      stdUnit  = standardMessageUnit

C--   Default values for W2_EXCH2
      W2_printMsg = -1
      W2_mapIO = -1
      W2_useE2ioLayOut = .TRUE.
      IF ( useCubedSphereExchange ) THEN
        preDefTopol = 3
      ELSE
        preDefTopol = 1
      ENDIF
      DO i=1,W2_maxNbTiles
        blankList(i) = 0
      ENDDO

C--   Initialise other params in namelist
      DO j=1,W2_maxNbFacets*2
        dimsFacets(2*j-1) = 0
        dimsFacets( 2*j ) = 0
        DO i=1,4
          facetEdgeLink(i,j) = 0.
        ENDDO
      ENDDO

C-    Initialise other parameters:
      nFacets = 0
      nBlankTiles = 0
      DO j=1,W2_maxNbFacets
        facet_dims(2*j-1) = 0
        facet_dims( 2*j ) = 0
        DO i=1,4
          facet_link(i,j) = 0.
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

C-    Check for file "data.ech2":
      fileExist = .FALSE.
      INQUIRE( FILE='data.exch2', EXIST=fileExist )

      IF ( fileExist ) THEN
        WRITE(msgBuf,'(A)') 'W2_READPARMS: opening data.exch2'
        CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT , myThid )
        CALL OPEN_COPY_DATA_FILE(
     I                      'data.exch2', 'W2_READPARMS',
     O                      iUnit,
     I                      myThid )

C     Read parameters from open data file
        READ(UNIT=iUnit,NML=W2_EXCH2_PARM01)
        WRITE(msgBuf,'(A)')
     &    'W2_READPARMS: finished reading data.exch2'
        CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT , myThid )
C     Close the open data file
        CLOSE(iUnit)
      ELSE
        WRITE(msgBuf,'(A)') 'W2_READPARMS: file data.exch2 not found'
        CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT , myThid )
        IF ( preDefTopol.EQ.1 ) THEN
          WRITE(msgBuf,'(2A,I3)') '=> use W2_EXCH2 default:',
     &      ' Single sub-domain (nFacets=1)'
        ELSEIF ( preDefTopol .EQ. 3 ) THEN
          WRITE(msgBuf,'(2A,I3)') '=> use W2_EXCH2 default:',
     &      ' regular 6-facets Cube'
        ELSE
          WRITE(msgBuf,'(2A,I3)') '=> use W2_EXCH2 default:',
     &      ' preDefTopol=', preDefTopol
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT , myThid )
      ENDIF

C--   copy local arrays dimsFacets & facetEdgeLink to var in common block
      DO j=1,2*W2_maxNbFacets
c       write(0,*) j, dimsFacets(j)
        facet_dims(j) = dimsFacets(j)
      ENDDO
      DO j=1,W2_maxNbFacets
       DO i=1,4
        facet_link(i,j) = facetEdgeLink(i,j)
       ENDDO
      ENDDO

C--   Check if too many values are specified in data.exch2:
      errCnt = 0
      DO j=W2_maxNbFacets+1,namList_NbFacets
       errFlag = .FALSE.
       DO i=1,4
        IF ( facetEdgeLink(i,j).NE.0. ) errFlag = .TRUE.
       ENDDO
       IF ( errFlag ) errCnt = errCnt + 1
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(2A)') ' W2_READPARMS:',
     &    ' Number of "facetEdgeLink" list in "data.exch2"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,2(A,I3))') ' W2_READPARMS:',
     &    ' exceeds maxNbFacets(=',W2_maxNbFacets,') by', errCnt
        CALL PRINT_ERROR( msgBuf, myThid )
        errFlag = .TRUE.
      ELSE
        errFlag = .FALSE.
      ENDIF

      errCnt = 0
      DO j=2*W2_maxNbFacets+1,2*namList_NbFacets
        IF ( dimsFacets(j).NE.0 ) errCnt = errCnt + 1
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(2A)') ' W2_READPARMS:',
     &    ' Number of "dimsFacets" in "data.exch2"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,2(A,I3))') ' W2_READPARMS:',
     &    ' exceeds 2*maxNbFacets(=',W2_maxNbFacets*2,') by', errCnt
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( errFlag .OR. errCnt.GT.0 ) THEN
        STOP 'ABNORMAL END: S/R W2_READPARMS'
      ENDIF

C--   Print some Exch2 parameters:
      WRITE(msgBuf,'(A,L5,A)') 'W2_useE2ioLayOut=', W2_useE2ioLayOut,
     &  ' ;/* T: use Exch2 glob IO map; F: use model default */'
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A,I4,A)') 'W2_mapIO        =', W2_mapIO,
     & ' ; /* select option for Exch2 global-IO map */'
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A,I4,A)') 'W2_printMsg     =', W2_printMsg,
     & ' ; /* select option for printing information */'
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT , myThid )

      RETURN
      END
