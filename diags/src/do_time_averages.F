C $Header: /u/gcmpack/MITgcm/diags/src/Attic/do_time_averages.F,v 1.14 2001/02/15 19:01:39 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

      SUBROUTINE DO_TIME_AVERAGES(
     I     myTime, myIter, bi, bj, myThid)
C     /==========================================================\
C     | SUBROUTINE DO_TIME_AVERAGES                              |
C     | o Time averaging routine for U, V, T, S, UT, VT in model |
C     |   main time-stepping                                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "AVER.h"

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
 
C     Initialize fields for the first call ever
      IF ( myIter .EQ. nIter0 ) THEN
       DO K=1,Nr
         CALL TIMEAVER_INI_XYZ(thetatave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(salttave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(uVeltave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(vVeltave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(wVeltave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(uttave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(vttave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(phiHydtave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(ConvectCountTave, bi, bj, K, myThid)
         TimeAve_half(k,bi,bj)=0.
         TimeAve_full(k,bi,bj)=0.
       ENDDO
      ENDIF
      
      IF ( myIter .EQ. nIter0 .OR.
     &  DIFFERENT_MULTIPLE(taveFreq, myTime, myTime-deltaTClock)) THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF

      DO K=1,Nr

C     Time Averages of single fields
      CALL TIMEAVER_1FLD_XYZ(theta, thetatave, DDTT, bi, bj, K, myThid)
      CALL TIMEAVER_1FLD_XYZ(salt,  salttave,  DDTT, bi, bj, K, myThid)
      CALL TIMEAVER_1FLD_XYZ(uVel,  uVeltave,  DDTT, bi, bj, K, myThid)
      CALL TIMEAVER_1FLD_XYZ(vVel,  vVeltave,  DDTT, bi, bj, K, myThid)

C     Time Averages of "double" fields
      CALL TIMEAVER_2FLD_XYZ(theta, uVel, uttave, 1, DDTT, 
     & bi, bj, K, myThid)
      CALL TIMEAVER_2FLD_XYZ(theta, vVel, vttave, 2, DDTT, 
     & bi, bj, K, myThid)

C     Keep record of how much time has been integrated over
      TimeAve_half(k,bi,bj)=TimeAve_half(k,bi,bj)+DDTT

C     Time Averages of "intermediate" fields
      IF ( myIter .NE. nIter0 ) THEN
         CALL TIMEAVER_1FLD_XYZ(wVel,  wVeltave,  deltaTclock, 
     &                          bi, bj, K, myThid)
         TimeAve_full(k,bi,bj)=TimeAve_full(k,bi,bj)+deltaTclock
      ENDIF

C--   End DO K=1,Nr
      ENDDO

      RETURN
      END
