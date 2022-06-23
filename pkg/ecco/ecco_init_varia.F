#include "ECCO_OPTIONS.h"

      SUBROUTINE ECCO_INIT_VARIA( myThid )

c     ==================================================================
c     SUBROUTINE ecco_init_varia
c     ==================================================================
c
c     o Initialise ecco variables.
c
c     ==================================================================
c     SUBROUTINE ecco_cost_init_varia
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif

c     == routine arguments ==

      INTEGER myThid

c     == local variables ==

c     == end of interface ==

#ifdef ALLOW_PSBAR_STERIC
      RHOsumGlob_0=0. _d 0
      VOLsumGlob_0=0. _d 0
#endif

      CALL ECCO_PHYS( startTime, -1, myThid )

#ifdef ALLOW_PSBAR_STERIC
      RHOsumGlob_0=RHOsumGlob
      VOLsumGlob_0=VOLsumGlob
#endif

      CALL ECCO_COST_INIT_VARIA( myThid )

      _BARRIER

      RETURN
      END
