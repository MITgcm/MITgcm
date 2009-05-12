C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_set_single_facet.F,v 1.1 2009/05/12 19:40:33 jmc Exp $
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

C      Tile toplogy settings data structures
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
C     msgBuf     :: Informational/error meesage buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      nFacets = 1
C-    set connection graph (topology)
C-    need to set/check facet dims

      STOP 'ABNORMAL END: S/R W2_SET_SINGLE_FACET empty'

      RETURN
      END
