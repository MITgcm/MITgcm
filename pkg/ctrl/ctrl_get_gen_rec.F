#include "CTRL_OPTIONS.h"

      SUBROUTINE CTRL_GET_GEN_REC(
     I                        xx_genstartdate,
     I                        xx_genperiod,
     O                        fac,
     O                        first,
     O                        changed,
     O                        count0,
     O                        count1,
     I                        myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE ctrl_get_gen_rec
C     ==================================================================
C
C     o Get flags, counters, and the linear interpolation factor for a
C       given control vector contribution.
C     o New, generic, for new routine ctrl_get_gen
C
C     ==================================================================
C     SUBROUTINE ctrl_get_gen_rec
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CTRL_SIZE.h"
#include "CTRL.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     == routine arguments ==
      INTEGER xx_genstartdate(4)
      _RL     xx_genperiod
      _RL     fac
      LOGICAL first
      LOGICAL changed
      INTEGER count0
      INTEGER count1
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == local variables ==
#ifdef ALLOW_CAL
      INTEGER mydate(4)
      INTEGER previousdate(4)
      INTEGER difftime(4)

      INTEGER fldcount
      _RL     fldsecs
      INTEGER prevfldcount
      _RL     prevfldsecs
      INTEGER flddate(4)

      INTEGER fldstartdate(4)
      _RL     fldperiod

      INTEGER year0
      INTEGER year1
#endif /* ALLOW_CAL */

C     Declarations for code, adapted from external_fields_load,
C     for simplied default model calendar without exf/cal
      _RL myRelTime, tmpFac
      INTEGER countP

      INTEGER shiftRec
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == end of interface ==

      shiftRec = 0
      IF ( useCAL ) THEN
#ifdef ALLOW_CAL
      fldperiod = 0.

C     Map the field parameters.

      CALL cal_CopyDate(
     I     xx_genstartdate,
     O     fldstartdate,
     I     myThid
     &     )
      fldperiod = xx_genperiod

      IF ( xx_genperiod .EQ. -12. _d 0 ) THEN
C     record numbers are assumed 1 to 12 corresponding to
C     Jan. through Dec.
       CALL cal_GetMonthsRec(
     O      fac, first, changed,
     O      count0, count1, year0, year1,
     I      myTime, myIter, myThid
     &      )
      ELSEIF ( fldperiod .EQ. 0. _d 0 ) THEN
C     Read field only once in the beginning. Hack: count1=count0 causes
C     the model to read the first record twice, but since this this is
C     done only the first time around it is not too much of an overhead.
       first   = ((myTime - modelstart) .LT. 0.5*modelstep)
       changed = .false.
       fac     = 1. _d 0
       count0  = 1
       count1  = count0
      ELSE
C     fldperiod .NE. 0
C--   Determine the current date.
       CALL cal_GetDate( myIter, myTime, mydate, myThid )

C     Determine first record:
       CALL cal_TimePassed( fldstartdate, modelStartDate,
     &                      difftime, myThid )
       CALL cal_ToSeconds ( difftime, fldsecs, myThid )
C     set record-number shift between full ctrl time-serie and
C     "effective" sub-set (i.e., that covers this simulation)
       shiftRec = int( fldsecs/fldperiod )

C     Determine the ctrl record just before mycurrentdate.
       CALL cal_TimePassed( fldstartdate, mydate, difftime,
     &                      myThid )
       CALL cal_ToSeconds( difftime, fldsecs, myThid )
       fldsecs  = int((fldsecs+0.5)/fldperiod)*fldperiod
       fldcount = int((fldsecs+0.5)/fldperiod) + 1

C     Set switches for reading new records.
       first = ((myTime - startTime) .LT. 0.5*deltaTClock)

       IF ( first) THEN
        changed = .false.
       ELSE
        CALL cal_GetDate( myIter-1, myTime-modelstep,
     &                    previousdate, myThid )

        CALL cal_TimePassed( fldstartdate, previousdate,
     &                       difftime, myThid )
        CALL cal_ToSeconds( difftime, prevfldsecs, myThid )
        prevfldsecs  = int((prevfldsecs+0.5)/fldperiod)*fldperiod
        prevfldcount = int((prevfldsecs+0.5)/fldperiod) + 1

        IF (fldcount .NE. prevfldcount) THEN
         changed = .true.
        ELSE
         changed = .false.
        ENDIF
       ENDIF

       count0 = fldcount - shiftRec
       count1 = count0 + 1

       CALL cal_TimeInterval( fldsecs, 'secs', difftime, myThid )
       CALL cal_AddTime( fldstartdate, difftime, flddate, myThid )
       CALL cal_TimePassed( flddate, mydate, difftime, myThid )
       CALL cal_ToSeconds( difftime, fldsecs, myThid )

C     Weight belonging to irec for linear interpolation purposes.
C     Note: The weight as chosen here is 1. - fac of the "old"
C           MITgcm estimation program.
       fac = 1. _d 0 - fldsecs/fldperiod

C     fldperiod .NE. 0.
      ENDIF
#endif /* ALLOW_CAL */
      ELSE
C-    case useCAL=F

C     Code, adapted from external_fields_load, for simplied
C     default model calendar without exf/cal, but
C     based on myTime, myIter, deltaTClock, externForcingCycle, and startTime

      myRelTime = myTime - startTime
      first = (myRelTime .LT. 0.5*deltaTClock)
      IF ( xx_genperiod .EQ. 0. _d 0
     &     .OR. externForcingCycle .EQ. 0. _d 0 ) THEN
C     control parameter is constant in time and only needs to be updated
C     once in the beginning
       changed = .false.
       count0  = 1
       count1  = 1
       fac     = 1. _d 0
      ELSE

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   countP, count0, count1, tmpFac, fac,
     I                   externForcingCycle, xx_genperiod,
     I                   deltaTClock, myTime, myThid )

       IF ( count0.NE.countP ) THEN
        changed = .true.
       ELSE
        changed = .false.
       ENDIF
       IF ( first ) changed = .false.

      ENDIF

C-    end if/else useCAL
      ENDIF

      IF ( debugLevel.GE.debLevC ) THEN
C     Do some printing for the protocol.
      _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(2A,I10,A,1PE21.14)') 'CTRL_GET_GEN_REC:',
     &    ' myIter, myTime =', myIter, ', ', myTime
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,2L3,F21.17,I5)')
     &    '     first, changed, fac, shiftRec:',
     &          first, changed, fac, shiftRec
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,2I5,A,2I5)')
     &    '  in xx_*.effective, count0,1=', count0, count1,
     &    ' ;  in xx_*, rec.#',  count0+shiftRec, count1+shiftRec
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      _END_MASTER( myThid )
      ENDIF

      RETURN
      END
