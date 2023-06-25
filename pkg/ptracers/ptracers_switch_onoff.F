#include "PTRACERS_OPTIONS.h"

CBOP
C     !ROUTINE: PTRACERS_SWITCH_ONOFF

C     !INTERFACE:
      SUBROUTINE PTRACERS_SWITCH_ONOFF(
     I                    myTime, myIter, myThid )

C     !DESCRIPTION:
C     Decide to switch on/off individual tracer time-stepping

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"

C     !INPUT PARAMETERS:
C     myTime    :: model time
C     myIter    :: time-step number
C     myThid    :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS
C     !LOCAL VARIABLES:
#ifndef ALLOW_AUTODIFF
      INTEGER iTracer
#endif

      IF ( .NOT.PTRACERS_startAllTrc ) THEN

#ifdef ALLOW_AUTODIFF
         STOP 'ABNORMAL END: S/R PTRACERS_SWITCH_ONOFF'
#else /* ALLOW_AUTODIFF */

        _BARRIER
        _BEGIN_MASTER(myThid)

        DO iTracer=1,PTRACERS_numInUse
          IF ( myTime.GE.PTRACERS_startStepFwd(iTracer) ) THEN
            PTRACERS_StepFwd(iTracer) = .TRUE.
          ELSE
            PTRACERS_StepFwd(iTracer) = .FALSE.
          ENDIF
        ENDDO

        _END_MASTER(myThid)
        _BARRIER

#endif /* ALLOW_AUTODIFF */

      ENDIF

#endif /* ALLOW_PTRACERS */

      RETURN
      END
