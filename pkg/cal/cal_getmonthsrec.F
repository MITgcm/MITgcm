#include "CAL_OPTIONS.h"

      subroutine cal_GetMonthsRec(
     O                             fac, first, changed,
     O                             month0, month1, year0, year1,
     I                             mytime, myiter, mythid
     &                           )

c     ==================================================================
c     SUBROUTINE cal_GetMonthsRec
c     ==================================================================
c
c     o Given the current model time or iteration number this routine
c       returns the corrresponding months that will have to be used in
c       order to interpolate monthly mean fields. The routine derives 
c       from *exf_GetMonthsRec* of the external forcing package.
c
c     started: Christian Eckert eckert@mit.edu 21-Apr-2000
c              - ported from the external forcing package and slightly
c                modified (10 --> nmonthyear-2, 12 --> nmonthyear).
c
c     changed: Patrick Heimbach heimbach@mit.edu 15-Jun-2000
c              - fixed bug for month1 = nmonthyear
c
c     ==================================================================
c     SUBROUTINE cal_GetMonthsRec
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      _RL     fac
      logical first
      logical changed
      integer month0
      integer month1
      integer year0
      integer year1
      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

      integer currentdate(4)
      integer midtime(4)
      integer middate(4)
      integer tempDate(4)
      integer middate0_1, middate0_2
      integer middate0(4)
      integer middate1_1, middate1_2
      integer middate1(4)
      integer prevdate(4)
      integer shifttime(4)
      integer startofmonth_1, startofmonth_2
      integer endofmonth_1,  endofmonth_2
      integer startofmonth(4)
      integer endofmonth(4)
      integer difftime(4)
      integer presentmonth
      integer presentyear
      integer previous
      integer next
      integer prevcount
      integer modelsteptime(4)

      _RL     currentsecs
      _RL     prevsecs
      _RL     midsecs_np
      _RL     diffsecs
      _RL     midsecs

c     == end of interface ==

ce    --> Include a check whether the right calendar is used.

      shifttime(1) =  1
      shifttime(2) =  0
      shifttime(3) =  0
      shifttime(4) = -1

      call cal_TimeInterval( -modelstep, 'secs', modelsteptime,
     &                        mythid )

c     Determine the current date and the current month.
      call cal_GetDate( myiter, mytime, currentdate, mythid )

      presentyear     = currentdate(1)/10000
      presentmonth    = mod(currentdate(1)/100,100)
      startofmonth_1  = (currentdate(1)/100)*100 + 1
      startofmonth_2  = 0
      call cal_FullDate( startofmonth_1, startofmonth_2,
     &                   startofmonth, mythid )

      endofmonth_1    = (currentdate(1)/100)*100 +
     &                  ndaymonth(presentmonth,currentdate(3))
      endofmonth_2    = 235959
      call cal_FullDate( endofmonth_1, endofmonth_2,
     &                   endofmonth, mythid )

c     Determine middle of current month.
      currentsecs = float(
     &              (mod(currentdate(1),100)-1)*secondsperday +
     &              currentdate(2)/10000*secondsperhour +
     &              mod(currentdate(2)/100,100)*secondsperminute +
     &              mod(currentdate(2),100)
     &              )
      midsecs     = float(ndaymonth(presentmonth,currentdate(3))*
     &                    secondsperday/2)

      call cal_TimeInterval( midsecs, 'secs', midtime, mythid )
      call cal_AddTime( startofmonth, midtime, middate, mythid )
      call cal_AddTime( currentdate, modelsteptime, prevdate, mythid )

      prevsecs = float(
     &           (mod(prevdate(1),100)-1)*secondsperday +
     &           prevdate(2)/10000*secondsperhour +
     &           mod(prevdate(2)/100,100)*secondsperminute +
     &           mod(prevdate(2),100)
     &           )

c--   Set switches for reading new records.
      first = ((mytime - modelstart) .lt. 0.5*modelstep)

      if ( first ) then
        changed = .false.
      endif

      if ( currentsecs .lt. midsecs ) then

        month0 = mod(presentmonth+nmonthyear-2,nmonthyear)+1
        year0 = presentyear
        if (month0 .EQ. 12) year0 = year0 - 1
        prevcount = month0

        shifttime(1) = -shifttime(1)
        call cal_AddTime( startofmonth, shifttime, middate0, mythid )
        middate0_1  = (middate0(1)/100)*100 + 1
        middate0_2  = 0
        call cal_FullDate( middate0_1, middate0_2, tempDate,
     &                     mythid )

        previous   = mod(tempDate(1)/100,100)

        midsecs_np = float(ndaymonth(previous,tempDate(3))*
     &                     secondsperday/2)

        call cal_TimeInterval( midsecs_np, 'secs', midtime, mythid )
        call cal_AddTime( tempDate, midtime, middate0, mythid )

        month1 = presentmonth
        year1 = presentyear

        middate1(1) = middate(1)
        middate1(2) = middate(2)
        middate1(3) = middate(3)
        middate1(4) = middate(4)

      else

        month0 = presentmonth
        year0 = presentyear

        if ( prevsecs .lt. midsecs ) then
          prevcount = mod(presentmonth+nmonthyear-2,nmonthyear)+1
        else
          prevcount = presentmonth
        endif

        middate0(1) = middate(1)
        middate0(2) = middate(2)
        middate0(3) = middate(3)
        middate0(4) = middate(4)

        month1 = mod(presentmonth, nmonthyear) + 1
        year1 = presentyear
        if ( month1 .EQ. 1 ) year1 = year1 + 1

        call cal_AddTime( endofmonth, shifttime, middate1, mythid )
        middate1_1  = (middate1(1)/100)*100 + 1
        middate1_2  = 0

        call cal_FullDate( middate1_1, middate1_2, tempDate,
     &                     mythid )
        next       = mod(tempDate(1)/100,100)
        midsecs_np = float(ndaymonth(next,tempDate(3))*
     &                     secondsperday/2)
        call cal_TimeInterval( midsecs_np, 'secs', midtime, mythid )
        call cal_AddTime( tempDate, midtime, middate1, mythid )

      endif

      call cal_SubDates( middate1, middate0, difftime, mythid )
      call cal_ToSeconds( difftime, diffsecs, mythid )

c     Set counters, switches, and the linear interpolation factor.
c     only check month, not year, as it will always change
      if ( (.not. first) .and. (prevcount .ne. month0) ) then
        changed = .true.
      else
        changed = .false.
      endif

      if ( currentsecs .lt. midsecs ) then
        fac = (midsecs - currentsecs)/diffsecs
      else
        fac = (2.*midsecs + midsecs_np - currentsecs)/
     &        diffsecs
      endif

      return
      end

