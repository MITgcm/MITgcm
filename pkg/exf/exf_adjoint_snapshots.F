#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: EXF_ADJOINT_SNAPSHOTS
C     !INTERFACE:
      SUBROUTINE EXF_ADJOINT_SNAPSHOTS(
     I               iwhen, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXF_ADJOINT_SNAPSHOTS                         |
C     *==========================================================*
C     Forward version of EXF Adjoint-variable output subroutine:
C     does nothing to forward variables version (empty S/R) but
C     argument list matters (for ADM and TLM version and calls)
C     *==========================================================*
C     | SUBROUTINE EXF_ADJOINT_SNAPSHOTS                         |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     iwhen  :: indicates where this S/R is called from
C     myIter :: Iteration counter for this thread
C     myTime :: Time counter for this thread
C     myThid :: Thread number for this instance of the routine.
      INTEGER iwhen
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf :: Error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      RETURN
      END
