cswdptr -- added routine ---
#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

      SUBROUTINE PTRACERS_STATVARS(
     I     myTime, myIter, bi, bj, myThid)
C     /==========================================================\
C     | SUBROUTINE PTRACERS_STATVARS                              |
C     | o Time averaging routine for PTRACERS  |
C     |   in model main time-stepping                            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "TIMEAVE_STATV.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS.h"
#include "PTRACERS_OPTIONS.h"
#include "PTRACERS_STATV.h"
#endif

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter, bi, bj
      _RL     myTime

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_TIMEAVE

C     == Local variables ==
      INTEGER iTracer, k
      _RL DDTT
 
C-    Initialize fields for the first call ever
      IF ( myIter .EQ. nIter0 ) THEN
        do iTracer=1,PTRACERS_numInUse
         CALL TIMEAVE_RESET(ptracertave(1-Olx,1-Oly,1,1,1,iTracer),
     &                                        Nr, bi, bj, myThid)
        enddo
        DO K=1,Nr
         ptracer_half(k,bi,bj)=0.
        ENDDO
      ENDIF
      
C--   Cumulate state-variables with Half or Full time step :
      IF ( myIter .EQ. nIter0 .OR.
     &  DIFFERENT_MULTIPLE(taveFreq, myTime, myTime-deltaTClock)) THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF

C-    Time Averages of single fields (no hFactor)
      do iTracer=1,PTRACERS_numInUse
       CALL TIMEAVE_CUMULATE(ptracertave(1-Olx,1-Oly,1,1,1,iTracer),
     &                       ptracer(1-Olx,1-Oly,1,1,1,iTracer),
     &                       Nr, DDTT, bi, bj, myThid)
      enddo

C-    Keep record of how much time has been integrated over
      DO K=1,Nr
        ptracer_half(k,bi,bj)=ptracer_half(k,bi,bj)+DDTT
      ENDDO




#endif /* ALLOW_TIMEAVE */ 
#endif

      RETURN
      END
