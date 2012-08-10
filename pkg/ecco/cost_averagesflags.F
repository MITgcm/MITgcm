C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_averagesflags.F,v 1.6 2012/08/10 19:45:25 jmc Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


      subroutine cost_AveragesFlags(
     I                               myiter,
     I                               mytime,
     I                               mythid,
     O                               first,
     O                               last,
     O                               startofday,
     O                               startofmonth,
     O                               startofyear,
     O                               inday,
     O                               inmonth,
     O                               inyear,
     O                               endofday,
     O                               endofmonth,
     O                               endofyear,
     O                               sum1day,
     O                               dayrec,
     O                               sum1mon,
     O                               monrec,
     O                               sum1year,
     O                               yearrec
     &                             )

c     ==================================================================
c     SUBROUTINE cost_AveragesFlags
c     ==================================================================
c
c     o Get flags for the daily and monthly time averaging.
c
c     started: Christian Eckert eckert@mit.edu 24-Feb-2000
c
c              - Introduced in order to create a package for the
c                MITgcmUV.
c
c     changed:
c
c     ==================================================================
c     SUBROUTINE cost_AveragesFlags
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cal.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid
      logical first
      logical startofday
      logical startofmonth
      logical startofyear
      logical inday
      logical inmonth
      logical inyear
      logical last
      logical endofday
      logical endofmonth
      logical endofyear
      integer sum1day
      integer dayrec
      integer sum1mon
      integer monrec
      integer sum1year
      integer yearrec

c     == local variables ==

      integer mydate(4)
      integer nextdate(4)
      integer prevdate(4)
      integer timediff(4)
      integer datediff(4)
      integer targetdate(4)
      integer targetdate1
      integer targetdate2
      integer mydateday
      integer mydatemonth
      integer mydateyear
      integer nextdateday
      integer nextdatemonth
      integer nextdateyear
      integer prevdateday
      integer prevdatemonth
      integer prevdateyear

      logical equal

c     == external functions ==

      logical  cal_CompDates
      external cal_CompDates

      integer  cal_NumInts
      external cal_NumInts

c     == end of interface ==

c--   First, get some dates.
      call cal_GetDate( myiter,   mytime,           mydate,   mythid )
      call cal_GetDate( myiter+1, mytime+modelstep, nextdate, mythid )

      call cal_TimeInterval( -modelstep, 'secs', timediff, mythid )
      call cal_AddTime( mydate, timediff, prevdate, mythid )

ce      print*,' time average flags:   mydate = ', mydate
ce      print*,' time average flags: nextdate = ', nextdate
ce      print*,' time average flags: prevdate = ', prevdate

c--   Where are we with respect to the calendar?

c--   The very first model step?
      equal = cal_CompDates(modelstartdate,mydate,mythid)
      if ( equal ) then
        first  = .true.
        dayrec = 0
        monrec = 0
        yearrec = 0
      else
        first = .false.
      endif

c--   The very last model step?
      equal = cal_CompDates(modelenddate,mydate,mythid)
      if ( equal ) then
        last = .true.
      else
        last = .false.
      endif

c--   Start of a calendar day?
      mydateday   = mod(mydate(1),100)
      prevdateday = mod(prevdate(1),100)
      if ( mydateday .ne. prevdateday ) then
        startofday = .true.
      else
        startofday = .false.
      endif

c--   End of a calendar day?
      mydateday   = mod(mydate(1),100)
      nextdateday = mod(nextdate(1),100)
      if ( mydateday .ne. nextdateday ) then
        endofday = .true.
      else
        endofday = .false.
      endif

c--   In a calendar day? As coded here, inday can not be true
c--   for either the first or the last timestep in the day.
c--   So the cases are mutually exclusive.
      if ( ( mydateday .eq. prevdateday ) .and.
     &     ( mydateday .eq. nextdateday )       ) then
        inday = .true.
      else
        inday = .false.
      endif

c--   Determine sum1day and dayrec explicitly.
      if ( last .or. endofday ) then
        if ( mydate(1) .eq. modelstartdate(1) ) then
          call cal_CopyDate( modelstartdate, targetdate, mythid )
          dayrec = 1
        else
          targetdate(1) = mydate(1)
          targetdate(2) = 0
          targetdate(3) = mydate(3)
          targetdate(4) = mydate(4)
          call cal_TimePassed( modelstartdate, targetdate, datediff,
     &                         mythid )
          if ( datediff(2) .eq. 0) then
            dayrec = datediff(1) + 1
          else
            dayrec = datediff(1) + 2
          endif
        endif
        call cal_TimeInterval( modelstep, 'secs', timediff, mythid )

        sum1day =
     &    cal_NumInts( targetdate, mydate, timediff, mythid ) + 1
      else
        sum1day = 0
      endif

c--   Start of a calendar month?
      mydatemonth   = mod(mydate(1)/100,100)
      prevdatemonth = mod(prevdate(1)/100,100)
      if ( mydatemonth .ne. prevdatemonth ) then
        startofmonth = .true.
      else
        startofmonth = .false.
      endif

c--   End of a calendar month?
      mydatemonth   = mod(mydate(1)/100,100)
      nextdatemonth = mod(nextdate(1)/100,100)
      if ( mydatemonth .ne. nextdatemonth ) then
        endofmonth = .true.
      else
        endofmonth = .false.
      endif

c--   In a calendar month? As coded here, inmonth can not be true
c--   for either the first or the last timestep in the month.
c--   So the cases are mutually exclusive.
      if ( ( mydatemonth .eq. prevdatemonth ) .and.
     &     ( mydatemonth .eq. nextdatemonth )       ) then
        inmonth = .true.
      else
        inmonth = .false.
      endif

c--   Determine sum1mon and monrec explicitly.
      if ( last .or. endofmonth ) then
        if ( (mydate(1)/100)*100 .eq. (modelstartdate(1)/100)*100 ) then
          call cal_CopyDate( modelstartdate, targetdate, mythid )
          monrec  = 1
        else
          targetdate1 = (mydate(1)/100)*100+1
          targetdate2 = 0
          call cal_FullDate( targetdate1, targetdate2, targetdate,
     &                       mythid )
          if ( mydate(1)/10000 .eq. modelstartdate(1)/10000 ) then
            monrec = mod(         mydate(1)/100, 100 )
     &             - mod( modelstartdate(1)/100, 100 ) + 1
          else
            monrec = mod( mydate(1)/100, 100 )
     &             + nmonthyear - mod(modelstartdate(1)/100,100) + 1
     &             + ( mydate(1)/10000 - modelstartdate(1)/10000 - 1)*
     &                nmonthyear
          endif
        endif
        call cal_TimeInterval( modelstep, 'secs', timediff, mythid )

        sum1mon =
     &    cal_NumInts( targetdate, mydate, timediff, mythid ) + 1
      else
        sum1mon = 0
      endif

c--   Start of a Year?
      mydateyear   = int(mydate(1)/10000)
      prevdateyear = int(prevdate(1)/10000)
      if ( mydateyear .ne. prevdateyear ) then
        startofyear = .true.
      else
        startofyear = .false.
      endif

c--   End of a Year?
      mydateyear   = int(mydate(1)/10000)
      nextdateyear = int(nextdate(1)/10000)
      if ( mydateyear .ne. nextdateyear ) then
        endofyear = .true.
      else
        endofyear = .false.
      endif

c--   In a calendar year? As coded here, inyear can not be true
c--   for either the first or the last timestep in the year.
c--   So the cases are mutually exclusive.
      if ( ( mydateyear .eq. prevdateyear ) .and.
     &     ( mydateyear .eq. nextdateyear )       ) then
        inyear = .true.
      else
        inyear = .false.
      endif

c--   Determine sum1year and yearrec explicitly.
      if ( last .or. endofyear ) then
        if (  int(mydate(1)/10000) .eq.
     &        int(modelstartdate(1)/10000) ) then
          call cal_CopyDate( modelstartdate, targetdate, mythid )
          yearrec  = 1
        else
          targetdate1 = int(mydate(1)/10000)*10000+101
          targetdate2 = 0
          call cal_FullDate( targetdate1, targetdate2, targetdate,
     &                       mythid )
          yearrec  = mydate(1)/10000 - modelstartdate(1)/10000 + 1
        endif
c
        call cal_TimeInterval( modelstep, 'secs', timediff, mythid )

        sum1year =
     &    cal_NumInts( targetdate, mydate, timediff, mythid ) + 1
      else
        sum1year = 0
      endif

      end
