C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_write.F,v 1.17 2005/05/20 07:28:50 jmc Exp $
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

C     !INPUT PARAMETERS:
      _RL     myTime
      INTEGER myIter, myThid

c Local variables
c ===============
      INTEGER   n
      INTEGER   myItM1, wrIter
      LOGICAL   dump2fileNow
      _RL       phiSec, freqSec, wrTime
#ifdef ALLOW_CAL
      INTEGER thisdate(4), prevdate(4)
#endif
#ifdef ALLOW_FIZHI
      logical alarm2
      character *9 tagname
#endif

      LOGICAL  DIFF_PHASE_MULTIPLE
      EXTERNAL DIFF_PHASE_MULTIPLE

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
          freqSec = freq(n)
          phiSec = phase(n)

          IF ( freqSec.LT.0. ) THEN
C--     write snap-shot with suffix = myIter to be consistent with 
C       time-average diagnostics (e.g., freq=-1 & freq=1):
c           wrIter = myIter
c           wrTime = myTime
C--     write snap-shot with suffix = myIter-1 to be consistent with
C       state-variable time-step:
            wrIter = myItM1
            wrTime = myTime - deltaTclock
          ELSE
            wrIter = myIter
            wrTime = myTime
          ENDIF
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec, 
     &                                        wrTime, deltaTclock )
#ifdef ALLOW_FIZHI
         if( useFIZHI) then 
          write(tagname,'(A,I2.2)')'diagtag',n
          dump2fileNow = alarm2(tagname)
         endif
#endif

#ifdef ALLOW_CAL
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

C---   Check to see IF its time for Statistics Diag. Output 

        DO n = 1,diagSt_nbLists
          freqSec = diagSt_freq(n)
          phiSec = diagSt_phase(n)

          IF ( freqSec.LT.0. ) THEN
C--     write snap-shot with suffix = myIter to be consistent with 
C       time-average diagnostics (e.g., freq=-1 & freq=1):
c           wrIter = myIter
c           wrTime = myTime
C--     write snap-shot with suffix = myIter-1 to be consistent with
C       state-variable time-step:
            wrIter = myItM1
            wrTime = myTime - deltaTclock
          ELSE
            wrIter = myIter
            wrTime = myTime
          ENDIF
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec, 
     &                                        wrTime, deltaTclock )
          IF ( dump2fileNow ) THEN
            CALL DIAGSTATS_OUTPUT(n,wrIter,myThid)
          ENDIF
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-      wait for everyone before setting arrays to zero: 
        _BARRIER

C--     Clear storage space:

        DO n = 1,nlists
          freqSec = freq(n)
          phiSec = phase(n)

          wrTime = myTime
          IF ( freqSec.LT.0. ) wrTime = myTime - deltaTclock
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec, 
     &                                        wrTime, deltaTclock )
#ifdef ALLOW_FIZHI
          if( useFIZHI) then 
           write(tagname,'(A,I2.2)')'diagtag',n
           dump2fileNow = alarm2(tagname)
          endif
#endif
          IF ( dump2fileNow ) CALL CLRINDX(n,myThid)
        ENDDO

        DO n = 1,diagSt_nbLists
          freqSec = diagSt_freq(n)
          phiSec = diagSt_phase(n)
          wrTime = myTime
          IF ( freqSec.LT.0. ) wrTime = myTime - deltaTclock
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec, 
     &                                        wrTime, deltaTclock )
          IF ( dump2fileNow ) CALL DIAGSTATS_CLEAR(n,myThid)
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

      RETURN 
      END
