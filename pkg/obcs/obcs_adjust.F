#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_ADJUST

C     !INTERFACE:
      SUBROUTINE OBCS_ADJUST(
     &     futureTime, futureIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_ADJUST
C     | o adjust boundary fluxes to balance flow
C     | o this code used to be part of S/R OBCS_CALC, but with
C     |   surface balancing, we need all forcing terms so this
C     |   part needs be called after S/R EXTERNAL_FORCING_SURF
C     | o tidal forcing needs to be applied at the very end after
C     |   balancing the flow because it introduces new
C     |   (purposefully) unbalanced flow
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "OBCS_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER futureIter
      _RL futureTime
      INTEGER myThid

#ifdef ALLOW_OBCS

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_ADJUST',myThid)
#endif

#ifdef ALLOW_OBCS_BALANCE
      IF ( useOBCSbalance ) THEN
        CALL OBCS_BALANCE_FLOW( futureTime, futureIter, myThid )
      ENDIF
#endif /* ALLOW_OBCS_BALANCE */

#ifdef ALLOW_OBCS_TIDES
      IF ( useOBCStides ) THEN
        CALL OBCS_ADD_TIDES( futureTime, futureIter, myThid )
      ENDIF
#endif /* ALLOW_OBCS_TIDES */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_ADJUST',myThid)
#endif
#endif /* ALLOW_OBCS */

      RETURN
      END
