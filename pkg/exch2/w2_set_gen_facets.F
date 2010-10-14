C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_set_gen_facets.F,v 1.3 2010/10/14 17:34:35 jahn Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_SET_GEN_FACETS( myThid )

C !INTERFACE:
      SUBROUTINE W2_SET_GEN_FACETS( myThid )

C     !DESCRIPTION:
C     Set-up multi-facets (=sub-domain) topology : general case
C     process topology information from "data.exch2" (facet_dims,facet_link)

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
      INTEGER i,j,jj,fNx,fNy
      INTEGER errCnt
CEOP
      DATA edge / 'N' , 'S' , 'E' , 'W' /

      WRITE(msgBuf,'(2A,I3,A)') 'W2_SET_GEN_FACETS:',
     &              ' preDefTopol=', preDefTopol, ' selected'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )

C     count Nb of Facets (from facet_dims) ; set nFacets
C     Assume: consecutive pair (x-dim,y-dim) of non-zero dimension
      errCnt = 0
      nFacets = 0
C     find last pair of non-zero dims
      DO j=1,W2_maxNbFacets
        fNx = facet_dims(2*j-1)
        fNy = facet_dims( 2*j )
C       IF ( nFacets.EQ.0 .AND. fNx*fNy.EQ.0 ) THEN
        IF ( fNx.NE.0 .AND. fNy.NE.0 ) THEN
          nFacets = j
        ELSEIF ( fNx.NE.0 .OR. fNy.NE.0 ) THEN
          errCnt = errCnt + 1
          WRITE(msgBuf,'(A,I3,A,2I6)')
     &      'dimsFacets: Expect pair of >0 dims : facet',j,
     &      ' :',fNx,fNy
          CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
      ENDDO
      IF ( nFacets.EQ.0 ) THEN
        errCnt = errCnt + 1
        WRITE(msgBuf,'(A)')
     &    'dimsFacets: All dimensions are zero!'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)') 'W2_SET_GEN_FACETS: found', errCnt,
     &                           ' errors in dimsFacets list'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: W2_SET_GEN_FACETS (dimsFacets list)'
      ENDIF

C-    print out Nb of facets:
      WRITE(msgBuf,'(A,I3,A)')
     &  'W2_SET_GEN_FACETS: Number of facets =', nFacets,
     &  ' (inferred from "dimsFacets")'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_RIGHT, myThid )

C-    Check remaining part of the list:
      errCnt = 0
      DO jj=2*nFacets+1,2*W2_maxNbFacets
        IF ( facet_dims(jj).NE.0 ) THEN
          errCnt = errCnt + 1
          WRITE(msgBuf,'(A,I3,A,I5,A)') ' dimsFacets(j=',jj,') =',
     &           facet_dims(jj), ' : beyond end of list (=1rst zero)'
          CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
      ENDDO
C-    check sign
      DO jj=1,2*nFacets
       IF ( facet_dims(jj).LT.0 ) THEN
         errCnt = errCnt + 1
         i=1+MOD(jj-1,2)
         j = (jj+1)/2
         WRITE(msgBuf,'(A,I2,A,I3,A,I6,A)') 'dimension', i,
     &     ' of facet', j, ' =', facet_dims(jj), ' : invalid (< 0)'
         CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)') 'W2_SET_GEN_FACETS: found', errCnt,
     &                           ' invalid dims'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: W2_SET_GEN_FACETS (dimsFacets list)'
      ENDIF

C     check "facet_link" list:
      errCnt = 0
      DO j=nFacets+1,W2_maxNbFacets
       DO i=1,4
        IF ( facet_link(i,j).NE.0 ) THEN
          errCnt = errCnt + 1
          WRITE(msgBuf,'(3A,I3,A,F6.2,A)')
     &       'Link for ',edge(i), '.Edge of facet #',j,
     &       ' (facetEdgeLink=',facet_link(i,j),')'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I3,A)')
     &      ' is beyond range (> nFacets=',nFacets,')'
          CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
       ENDDO
      ENDDO
      IF ( errCnt.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)') 'W2_SET_GEN_FACETS: found', errCnt,
     &                           ' errors in facetEdgeLink list'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: W2_SET_GEN_FACETS (facetEdgeLink list)'
      ENDIF

      RETURN
      END
