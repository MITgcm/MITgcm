C $Header: /u/gcmpack/MITgcm/pkg/exch2/w2_set_myown_facets.F,v 1.2 2010/04/23 20:21:06 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"
#include "W2_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: W2_SET_MYOWN_FACETS

C !INTERFACE:
      SUBROUTINE W2_SET_MYOWN_FACETS( myThid )

C     !DESCRIPTION:
C     Set-up multi-facets (=sub-domain) topology :
C     User specific version to be modified accordingly

C     !USES:
      IMPLICIT NONE

C      Tile topology settings data structures
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_PARAMS.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT PARAMETERS:
C     myThid  :: my Thread Id number
C               (Note: not relevant since threading has not yet started)
      INTEGER myThid

#ifdef ALLOW_EXCH2
C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      STOP 'ABNORMAL END: S/R W2_SET_MYOWN_FACETS empty'

#endif /* ALLOW_EXCH2 */

      RETURN
      END
