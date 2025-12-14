#include "COST_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: COST_INIT_FIXED

C !INTERFACE:
      SUBROUTINE COST_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize COST variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "cost.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

C--   The first time around we want to write the cost function to a file
      costWriteCostFunction = .TRUE.

      RETURN
      END
