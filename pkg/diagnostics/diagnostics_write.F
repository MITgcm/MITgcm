C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_write.F,v 1.29 2006/06/05 18:17:23 jmc Exp $
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
C     myThid :: my Thread Id number
C***********************************************************************
       IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "PARAMS.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      _RL     myTime
      INTEGER myIter, myThid

c Local variables
c ===============
      INTEGER   n
      INTEGER   myItM1, wrIter
      LOGICAL   dump2fileNow, write2file
      _RL       phiSec, freqSec, wrTime
#ifdef ALLOW_FIZHI
      LOGICAL alarm2
      CHARACTER *9 tagname
#endif

      LOGICAL  DIFF_PHASE_MULTIPLE
      EXTERNAL DIFF_PHASE_MULTIPLE

      IF ( myIter.NE.nIter0 ) THEN
        myItM1 = myIter - 1

C***********************************************************************
C***   Check to see IF its time for Diagnostic Output                ***
C***********************************************************************

        write2file = .FALSE.
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
          IF ( useFIZHI ) THEN
           WRITE(tagname,'(A,I2.2)')'diagtag',n
           dump2fileNow = alarm2(tagname)
          ENDIF
#endif
#ifdef ALLOW_CAL
          IF ( useCAL ) THEN
            CALL CAL_TIME2DUMP( freqSec, deltaTClock,
     U                          dump2fileNow,
     I                          myTime, myIter, myThid )
          ENDIF
#endif /* ALLOW_CAL */
          IF ( dump2fileNow .OR.
     &        (myTime.EQ.endTime .AND. dumpAtLast) ) THEN
            write2file = .TRUE.
            CALL DIAGNOSTICS_OUT(n,wrIter,wrTime,myThid)
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
#ifdef ALLOW_FIZHI
          IF ( useFIZHI ) THEN
           WRITE(tagname,'(A,I2.2)')'diagStg',n
           dump2fileNow = alarm2(tagname)
          ENDIF
#endif
          IF ( dump2fileNow .OR.
     &        (myTime.EQ.endTime .AND. dumpAtLast) ) THEN
            write2file = .TRUE.
            CALL DIAGSTATS_OUTPUT(n,wrTime,wrIter,myThid)
          ENDIF
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( write2file ) THEN
          IF ( debugLevel.GE.debLevB ) THEN
            CALL DIAGNOSTICS_SUMMARY( myTime, myIter, myThid )
          ENDIF
C-      wait for everyone before setting arrays to zero:
          _BARRIER
        ENDIF

C--     Clear storage space:

        DO n = 1,nlists
          freqSec = freq(n)
          phiSec = phase(n)

          wrTime = myTime
          IF ( freqSec.LT.0. ) wrTime = myTime - deltaTclock
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                        wrTime, deltaTclock )
#ifdef ALLOW_FIZHI
          IF ( useFIZHI ) THEN
           WRITE(tagname,'(A,I2.2)')'diagtag',n
           dump2fileNow = alarm2(tagname)
          ENDIF
#endif
#ifdef ALLOW_CAL
          IF ( useCAL ) THEN
            CALL CAL_TIME2DUMP( freqSec, deltaTClock,
     U                          dump2fileNow,
     I                          myTime, myIter, myThid )
          ENDIF
#endif /* ALLOW_CAL */
          IF ( dump2fileNow .OR.
     &        (myTime.EQ.endTime .AND. dumpAtLast)
     &       ) CALL DIAGNOSTICS_CLEAR(n,myThid)
        ENDDO

        DO n = 1,diagSt_nbLists
          freqSec = diagSt_freq(n)
          phiSec = diagSt_phase(n)
          wrTime = myTime
          IF ( freqSec.LT.0. ) wrTime = myTime - deltaTclock
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                        wrTime, deltaTclock )
#ifdef ALLOW_FIZHI
          IF ( useFIZHI ) THEN
           WRITE(tagname,'(A,I2.2)')'diagStg',n
           dump2fileNow = alarm2(tagname)
          ENDIF
#endif
          IF ( dump2fileNow .OR.
     &        (myTime.EQ.endTime .AND. dumpAtLast)
     &       ) CALL DIAGSTATS_CLEAR( n, myThid )
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

      RETURN
      END
