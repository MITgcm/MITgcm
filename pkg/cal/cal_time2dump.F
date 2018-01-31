#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_TIME2DUMP(
     I                          phase, freq, step,
     U                          time2write,
     I                          myTime, myIter, myThid )

c     ==================================================================
C--   Convert approximate months (30-31 days) and years (360-372 days)
C     to exact calendar months and years.
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "cal.h"

c     == routine arguments ==
      _RL     phase, freq, step
      LOGICAL time2write
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

c     == local variables ==
      INTEGER thisDate(4), prevDate(4)
      _RL posFreq
      _RL shTime, prTime

      IF ( calendarDumps .AND. freq .NE. 0. ) THEN
C-     Same as in FCT DIFF_PHASE_MULTIPLE:
c      IF ( myTime+step .GE. phase+baseTime ) THEN
C-     should compare to phase+baseTime (above), but would need PARAMS.h ;
C      choose to disable this condition for negative time:
       IF ( myTime+step.GE.phase .OR. myTime.LT.0. ) THEN
        posFreq = ABS(freq)
C-    First determine calendar dates for this and previous time step.
        shTime = myTime - phase
        prTime = shTime - step
        CALL CAL_GETDATE( myIter, shTime, thisDate, myThid )
        CALL CAL_GETDATE( myIter, prTime, prevDate, myThid )
C-    Monthly Freq:
        IF ( posFreq.GE.2592000. .AND. posFreq.LE.2678400. ) THEN
           time2write = .FALSE.
           IF ( (thisdate(1)-prevdate(1)).GT.50 )   time2write=.TRUE.
        ENDIF
C-    Yearly  Freq:
        IF ( posFreq.GE.31104000. .AND. posFreq.LE.31968000. ) THEN
           time2write = .FALSE.
           IF ( (thisdate(1)-prevdate(1)).GT.5000 ) time2write=.TRUE.
        ENDIF
       ENDIF
      ENDIF

      RETURN
      END
