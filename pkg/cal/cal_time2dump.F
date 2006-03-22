C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_time2dump.F,v 1.1 2006/03/22 21:05:37 heimbach Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_TIME2DUMP(
     I                          freq, step,
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
      _RL     freq, step
      LOGICAL time2write
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

c     == local variables ==
      INTEGER thisDate(4), prevDate(4)

      IF ( calendarDumps ) THEN
C-    First determine calendar dates for this and previous time step.
        CALL CAL_GETDATE( myIter  ,myTime     ,thisDate,myThid )
        CALL CAL_GETDATE( myIter-1,myTime-step,prevDate,myThid )
C-    Monthly Freq:
        IF ( freq.GE.2592000. .AND. freq.LE.2678400. ) THEN
           time2write = .FALSE.
           IF ( (thisdate(1)-prevdate(1)).GT.50 )   time2write=.TRUE.
        ENDIF
C-    Yearly  Freq:
        IF ( freq.GE.31104000. .AND. freq.LE.31968000. ) THEN
           time2write = .FALSE.
           IF ( (thisdate(1)-prevdate(1)).GT.5000 ) time2write=.TRUE.
        ENDIF
      ENDIF

      RETURN
      END
