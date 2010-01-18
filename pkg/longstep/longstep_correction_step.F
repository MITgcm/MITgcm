C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_correction_step.F,v 1.2 2010/01/18 01:04:18 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE LONGSTEP_CORRECTION_STEP(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_CORRECTION_STEP
C     *==========================================================*
C     | part of tracer_correction_step that affect ptracers
C     | (except convective adjustment):
C     |
C     | The arrays used for time stepping are cycled.
C     | Tracers:
C     |           T(n) = Gt(n)
C     |
C     | pTracers* (contained in gPtr) is copied to pTracers
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "LONGSTEP.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     == Local variables
      INTEGER bi,bj
CEOP

#ifdef ALLOW_LONGSTEP
C     only cycle if we have computed gPtr in this time step
      IF ( LS_doTimeStep ) THEN
#else
      IF ( .TRUE. ) THEN
#endif
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_PTRACERS
C-        Update passive tracer fields:  T(n) = T**
          IF (usePTRACERS)
     &      CALL PTRACERS_CYCLE(bi,bj,myTime,myIter,myThid)
#endif /* ALLOW_PTRACERS */

C--      End of 1rst bi,bj loop
         ENDDO
        ENDDO
C     endif LS_doTimeStep
      ENDIF

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
