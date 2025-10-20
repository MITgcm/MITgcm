#include "CTRL_OPTIONS.h"

      subroutine ctrl_get_gen_rec(
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

      integer xx_genstartdate(4)
      _RL     xx_genperiod
      _RL     fac
      logical first
      logical changed
      integer count0
      integer count1
      _RL     myTime
      integer myIter
      integer myThid

C     == local variables ==

#ifdef ALLOW_CAL

      integer mydate(4)
      integer previousdate(4)
      integer difftime(4)

      integer fldcount
      _RL     fldsecs
      integer prevfldcount
      _RL     prevfldsecs
      integer flddate(4)

      integer fldstartdate(4)
      _RL     fldperiod

      integer year0
      integer year1

#else
C     Declarations for code, adapted from external_fields_load,
C     for simplied default model calendar without exf/cal
      _RL myRelTime, tmpFac
      INTEGER countP
#endif

      INTEGER shiftRec
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == end of interface ==

      shiftRec = 0
#ifdef ALLOW_CAL
      fldperiod = 0.

C     Map the field parameters.

      call cal_CopyDate(
     I     xx_genstartdate,
     O     fldstartdate,
     I     myThid
     &     )
      fldperiod = xx_genperiod

      if ( xx_genperiod .eq. -12. _d 0 ) then
C     record numbers are assumed 1 to 12 corresponding to
C     Jan. through Dec.
       call cal_GetMonthsRec(
     O      fac, first, changed,
     O      count0, count1, year0, year1,
     I      myTime, myIter, myThid
     &      )
      elseif ( fldperiod .eq. 0. _d 0 ) then
C     Read field only once in the beginning. Hack: count1=count0 causes
C     the model to read the first record twice, but since this this is
C     done only the first time around it is not too much of an overhead.
       first   = ((myTime - modelstart) .lt. 0.5*modelstep)
       changed = .false.
       fac     = 1. _d 0
       count0  = 1
       count1  = count0
      else
C     fldperiod .ne. 0
C--   Determine the current date.
       call cal_GetDate( myIter, myTime, mydate, myThid )

C     Determine first record:
       call cal_TimePassed( fldstartdate, modelStartDate,
     &                      difftime, myThid )
       call cal_ToSeconds ( difftime, fldsecs, myThid )
C     set record-number shift between full ctrl time-serie and
C     "effective" sub-set (i.e., that covers this simulation)
       shiftRec = int( fldsecs/fldperiod )

C     Determine the ctrl record just before mycurrentdate.
       call cal_TimePassed( fldstartdate, mydate, difftime,
     &                      myThid )
       call cal_ToSeconds( difftime, fldsecs, myThid )
       fldsecs  = int((fldsecs+0.5)/fldperiod)*fldperiod
       fldcount = int((fldsecs+0.5)/fldperiod) + 1

C     Set switches for reading new records.
       first = ((myTime - startTime) .LT. 0.5*deltaTClock)

       if ( first) then
        changed = .false.
       else
        call cal_GetDate( myIter-1, myTime-modelstep,
     &                    previousdate, myThid )

        call cal_TimePassed( fldstartdate, previousdate,
     &                       difftime, myThid )
        call cal_ToSeconds( difftime, prevfldsecs, myThid )
        prevfldsecs  = int((prevfldsecs+0.5)/fldperiod)*fldperiod
        prevfldcount = int((prevfldsecs+0.5)/fldperiod) + 1

        if (fldcount .ne. prevfldcount) then
         changed = .true.
        else
         changed = .false.
        endif
       endif

       count0 = fldcount - shiftRec
       count1 = count0 + 1

       call cal_TimeInterval( fldsecs, 'secs', difftime, myThid )
       call cal_AddTime( fldstartdate, difftime, flddate, myThid )
       call cal_TimePassed( flddate, mydate, difftime, myThid )
       call cal_ToSeconds( difftime, fldsecs, myThid )

C     Weight belonging to irec for linear interpolation purposes.
C     Note: The weight as chosen here is 1. - fac of the "old"
C           MITgcm estimation program.
       fac = 1. _d 0 - fldsecs/fldperiod

C     fldperiod .ne. 0.
      endif
#else /* not ALLOW_CAL */
C     Code, adapted from external_fields_load, for simplied
C     default model calendar without exf/cal, but
C     based on myTime, myIter, deltaTClock, externForcingCycle, and startTime

      myRelTime = myTime - startTime
      first = (myRelTime .lt. 0.5*deltaTClock)
      if ( xx_genperiod .eq. 0. _d 0
     &     .or. externForcingCycle .eq. 0. _d 0 ) then
C     control parameter is constant in time and only needs to be updated
C     once in the beginning
       changed = .false.
       count0  = 1
       count1  = 1
       fac     = 1. _d 0
      else

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

      endif

#endif /* ALLOW_CAL */

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
