C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_write.F,v 1.11 2005/02/20 19:41:11 dimitri Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

      SUBROUTINE DIAGNOSTICS_WRITE ( myTime, myIter, myThid ) 
C***********************************************************************
C  Purpose
C  -------
C    Output sequence for the (multiple) diagnostics output files
C
C  Arguments  Description
C  ----------------------
C     myTime :: Current time of simulation ( s )
C     myIter :: Current Iteration Number
C     myThid :: my thread Id number
C***********************************************************************
       IMPLICIT NONE
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      _RL     myTime
      INTEGER myIter, myThid

c Local variables
c ===============
      INTEGER   n
      INTEGER   myItM1, wrIter
      LOGICAL   dump2fileNow
      INTEGER thisdate(4), prevdate(4)
      _RL     freqSec

      IF ( myIter.NE.nIter0 ) THEN
        myItM1 = myIter - 1

C***********************************************************************
C***   Check to see IF its time for Diagnostic Output                ***
C***********************************************************************

#ifdef ALLOW_CAL
        IF ( calendarDumps ) THEN
C-    Determine calendar dates for this and previous time step.
           call cal_GetDate(myiter  ,mytime            ,thisdate,mythid)
           call cal_GetDate(myiter-1,mytime-deltaTClock,prevdate,mythid)
        ENDIF
#endif

        DO n = 1,nlists
          IF ( freq(n).LT.0 ) THEN
            dump2fileNow = MOD(myItM1,-freq(n)) .EQ. INT(-freq(n)/2)
C--     write snap-shot with suffix = myIter to be consistent with 
C       time-average diagnostics (e.g., freq=-1 & freq=1):
c           wrIter = myIter
C--     write snap-shot with suffix = myIter-1 to be consistent with
C       state-variable time-step:
            wrIter = myItM1
          ELSE
            dump2fileNow = MOD(myIter,freq(n)) .EQ. 0
            wrIter = myIter
          ENDIF

#ifdef ALLOW_CAL
          freqSec = freq(n) * deltaTClock
          IF ( calendarDumps .AND. (
     &     ( freqSec.GE. 2592000 .AND. freqSec.LE. 2678400 ) .OR.
     &     ( freqSec.GE.31104000 .AND. freqSec.LE.31968000 ))) THEN
C--   Convert approximate months (30-31 days) and years (360-372 days)
C     to exact calendar months and years.
           dump2fileNow = .FALSE.
C-    Monthly freqSec:
           IF( freqSec.GE. 2592000 .AND. freqSec.LE. 2678400 .AND.
     &        (thisdate(1)-prevdate(1)).GT.50   ) dump2fileNow = .TRUE.
C-    Yearly  freqSec:
           IF( freqSec.GE.31104000 .AND. freqSec.LE.31968000 .AND.
     &        (thisdate(1)-prevdate(1)).GT.5000 ) dump2fileNow = .TRUE.
          ENDIF
#endif

          IF ( dump2fileNow ) THEN
            CALL DIAGNOSTICS_OUT(n,wrIter,myThid)
          ENDIF
        ENDDO

C-      wait for everyone before setting arrays to zero: 
        _BARRIER
        DO n = 1,nlists
          IF ( freq(n).LT.0 ) THEN
            dump2fileNow = MOD(myItM1,-freq(n)) .EQ. INT(-freq(n)/2)
          ELSE
            dump2fileNow = MOD(myIter,freq(n)) .EQ. 0
          ENDIF
          IF ( dump2fileNow ) CALL CLRINDX(n,myThid)
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

      RETURN 
      END
