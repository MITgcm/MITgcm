C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_statvars.F,v 1.1 2001/03/06 15:58:19 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

      SUBROUTINE TIMEAVE_STATVARS(
     I     myTime, myIter, bi, bj, myThid)
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_STATVARS                              |
C     | o Time averaging routine for eta, U, V, W, T, S, UT, VT  |
C     |   in model main time-stepping                            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "TIMEAVE_STATV.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter, bi, bj
      _RL     myTime

C     == Local variables ==
      INTEGER K
      _RL DDTT
 
C-    Initialize fields for the first call ever
      IF ( myIter .EQ. nIter0 ) THEN
        CALL TIMEAVE_RESET(etaTave,   1,  bi, bj, myThid)
        CALL TIMEAVE_RESET(thetatave, Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(salttave,  Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(uVeltave,  Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(vVeltave,  Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(wVeltave,  Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(uttave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(vttave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(phiHydtave,Nr,bi, bj, myThid)
        CALL TIMEAVE_RESET(ConvectCountTave,Nr,bi,bj,myThid)
        DO K=1,Nr
         TimeAve_half(k,bi,bj)=0.
         TimeAve_full(k,bi,bj)=0.
        ENDDO
      ENDIF
      
C--   Cumulate state-variables with Half or Full time step : 
      IF ( myIter .EQ. nIter0 .OR.
     &  DIFFERENT_MULTIPLE(taveFreq, myTime, myTime-deltaTClock)) THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF

C-    Time Averages of single fields
      CALL TIMEAVE_CUMULATE(etaTave,  etaN,  1 , DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(thetatave,theta, Nr, DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(salttave, salt,  Nr, DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(uVeltave, uVel,  Nr, DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(vVeltave, vVel,  Nr, DDTT, bi, bj, myThid)

C-    Time Averages of "double" fields
      CALL TIMEAVE_CUMUL_2V(uttave, theta, uVel, Nr, 1, DDTT, 
     &  bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2V(vttave, theta, vVel, Nr, 2, DDTT, 
     &  bi, bj, myThid)

C-    Keep record of how much time has been integrated over
      DO K=1,Nr
        TimeAve_half(k,bi,bj)=TimeAve_half(k,bi,bj)+DDTT
      ENDDO

C-    Time Averages of "intermediate" fields
      IF ( myIter .NE. nIter0 ) THEN
        CALL TIMEAVE_CUMULATE(wVeltave, wVel, Nr, deltaTclock, 
     &                          bi, bj, myThid)
        DO K=1,Nr
         TimeAve_full(k,bi,bj)=TimeAve_full(k,bi,bj)+deltaTclock
        ENDDO
      ENDIF

      RETURN
      END
