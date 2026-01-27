#include "COST_OPTIONS.h"

CBOP
C !ROUTINE: COST_DRIVER

C !INTERFACE: ==========================================================
      SUBROUTINE COST_DRIVER(
     I                        myTime, myIter, myThid )

C !DESCRIPTION:
C Driver routine to calculate cost function terms at the end of the model run

C !USES: ===============================================================
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C myTime    :: Current time in simulation
C myIter    :: Current time-step number
C myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
CEOP

#ifdef ALLOW_COST

#ifdef ALLOW_OBCS
      IF ( useOBCS ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('OBCS_COST_DRIVER',myThid)
# endif
       CALL TIMER_START('OBCS_COST_DRIVER  [COST_DRIVER]', myThid)
       CALL OBCS_COST_DRIVER( myTime, myIter, myThid )
       CALL TIMER_STOP ('OBCS_COST_DRIVER  [COST_DRIVER]', myThid)
      ENDIF
#endif

#ifdef ALLOW_ECCO
      IF ( useECCO ) then
c--   Compute ecco specific cost function contributions, that is,
c--   user defined cost function contributions.
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('COST_GENCOST_ALL',myThid)
# endif
       CALL TIMER_START('COST_GENCOST_ALL  [COST_DRIVER]', myThid)
       CALL COST_GENCOST_ALL( myIter, myTime, myThid )
       CALL TIMER_STOP ('COST_GENCOST_ALL  [COST_DRIVER]', myThid)
      ENDIF
#endif

#ifdef ALLOW_CTRL
      IF ( useCTRL ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CTRL_COST_DRIVER',myThid)
# endif
       CALL TIMER_START('CTRL_COST_DRIVER  [COST_DRIVER]', myThid)
       CALL CTRL_COST_DRIVER( myTime, myIter, myThid )
       CALL TIMER_STOP ('CTRL_COST_DRIVER  [COST_DRIVER]', myThid)
      ENDIF
#endif

#endif /* ALLOW_COST */

      RETURN
      END
