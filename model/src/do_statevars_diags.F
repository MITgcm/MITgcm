C $Header: /u/gcmpack/MITgcm/model/src/do_statevars_diags.F,v 1.2 2004/11/23 20:20:19 mlosch Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_GCHEM
#include "GCHEM_OPTIONS.h"
#endif


CBOP
C     !ROUTINE: DO_STATEVARS_DIAGS
C     !INTERFACE:
      SUBROUTINE DO_STATEVARS_DIAGS( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_STATEVARS_DIAGS                                
C     | o Controlling routine for state variables diagnostics
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

C     !LOCAL VARIABLES:
C     == Local variables
      INTEGER bi,bj

#ifdef ALLOW_TIMEAVE
C--   Fill-in TimeAve pkg diagnostics (for state-variables)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        IF ( taveFreq.GT.0. ) THEN
          CALL TIMEAVE_STATVARS(myTime, myIter, bi, bj, myThid)
# ifdef ALLOW_PTRACERS
          IF ( usePTRACERS ) THEN	
           CALL PTRACERS_STATVARS(myTime, myIter, bi, bj, myThid)
          ENDIF	
# endif /* ALLOW_PTRACERS */
        ENDIF

C--    End of bi,bj loop
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_DIAGNOSTICS
C--   Fill-in Diagnostics pkg storage array (for state-variables)

      IF ( usediagnostics ) THEN
        CALL DIAGNOSTICS_FILL_STATE(myThid)
# ifdef ALLOW_PTRACERS
       IF (useptracers) THEN
        CALL PTRACERS_FILL_DIAGNOSTICS(myThid)
       ENDIF
# endif
      ENDIf

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
