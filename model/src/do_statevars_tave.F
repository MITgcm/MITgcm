C $Header: /u/gcmpack/MITgcm/model/src/do_statevars_tave.F,v 1.5 2010/01/03 00:07:03 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"


CBOP
C     !ROUTINE: DO_STATEVARS_TAVE
C     !INTERFACE:
      SUBROUTINE DO_STATEVARS_TAVE( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_STATEVARS_TAVE
C     | o Controlling routine for state variables time-averaging
C     *==========================================================*
C     | Computing statistics of the model state (state-variables)
C     | is done at this level (after updating the halo region),
C     | as opposed to other diagnostic calls (fluxes, tendencies)
C     | that remain within the computation sequence.
C     | Note: IO are not supposed to be done at this level
C     |       but later (in DO_THE_MODEL_IO)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid - Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_TIMEAVE
C     !LOCAL VARIABLES:
C     == Local variables
      INTEGER bi,bj

C--   Fill-in TimeAve pkg diagnostics (for state-variables)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

         IF ( taveFreq.GT.0. ) THEN
           CALL TIMEAVE_STATVARS( myTime, myIter, bi, bj, myThid )
         ENDIF
# ifdef ALLOW_PTRACERS
         IF ( usePTRACERS ) THEN
           CALL PTRACERS_TIMEAVE( myTime, myIter, bi, bj, myThid )
         ENDIF
# endif /* ALLOW_PTRACERS */

C--    End of bi,bj loop
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

      RETURN
      END
