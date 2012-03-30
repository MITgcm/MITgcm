C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_set_single_facet.F,v 1.4 2012/03/30 18:23:13 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_SET_SINGLE_FACET

C !INTERFACE:
      SUBROUTINE W2_SET_SINGLE_FACET( myThid )

C     !DESCRIPTION:
C     Set-up simple single facet (domain in 1 piece) topology

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
      INTEGER j
CEOP

      WRITE(msgBuf,'(2A,I3,A)') 'W2_SET_SINGLE_FACET:',
     &              ' preDefTopol=', preDefTopol, ' selected'
      CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )

C--   Number of facets:
      nFacets = 1

C--   Set Facet Edge connections (topology graph) ignoring any previous
C     setting from data.exch2 (Edges order: N,S,E,W <==> 1,2,3,4 )
C     face 1 N(=1) edge connects to face 1 S(=2) edge:
      facet_link(1,1) = 1.2
C     face 1 S(=2) edge connects to face 1 N(=1) edge:
      facet_link(2,1) = 1.1
C     face 1 E(=3) edge connects to face 1 W(=4) edge:
      facet_link(3,1) = 1.4
C     face 1 W(=4) edge connects to face 1 E(=3) edge:
      facet_link(4,1) = 1.3

C--   Facet dimension: take the 1rst 2 numbers from facet_dims (if correct)
      IF ( facet_dims(1).EQ.0 .AND. facet_dims(2).EQ.0 ) THEN
C-    Default: take global dimension from SIZE.h (will fail with blank tiles)
        facet_dims(1) = Nx
        facet_dims(2) = Ny
      ENDIF
      IF ( facet_dims(1).LE.0 .OR. facet_dims(2).LE.0 ) THEN
         WRITE(msgBuf,'(2A,2I5)') 'W2_SET_SINGLE_FACET:',
     &     ' unvalid 1rst 2 dimensions:', facet_dims(1), facet_dims(2)
         CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R W2_SET_SINGLE_FACET: unvalid dims'
      ENDIF
      DO j=3,W2_maxNbFacets*2
       IF ( facet_dims(j).NE.0 ) THEN
         WRITE(msgBuf,'(2A,I5)') 'W2_SET_SINGLE_FACET:',
     &     ' no more than 2 dims (X,Y) expected for single facet'
         CALL PRINT_MESSAGE( msgBuf, W2_oUnit,SQUEEZE_RIGHT,myThid )
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R W2_SET_SINGLE_FACET: unexpected dims'
       ENDIF
      ENDDO

      RETURN
      END
