C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_daysformonth.F,v 1.3 2012/04/19 19:43:31 heimbach Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_DaysForMonth( 
     I                             imonth,
     O                             firstday,
     O                             lastday,
     O                             ndays,
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE cal_DaysForMonth
c     ==================================================================
c
c     o Given the current month of the integration this routine returns
c       first, the last and the number of calendar days that will have
c       to be performed.
c
c       This routine also checks consistency of variables quite
c       extensively.
c
c     started: Christian Eckert eckert@mit.edu  06-Apr-2000
c
c     changed: 
c
c     ==================================================================
c     SUBROUTINE cal_DaysForMonth
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer imonth
      integer firstday
      integer lastday
      integer ndays
      integer mythid

c     == local variables ==

      integer i
      integer ierr
      integer nummonths
      integer numdays
      integer firstyear
      integer firstmonth
      integer firstd
      integer lyfirst
      integer lastyear
      integer lastmonth
      integer lastd
      integer lastsecs
      integer lylast
      integer currentyear
      integer currentmonth
      integer lycurrent

c     == external ==

      integer  cal_IntMonths
      external cal_IntMonths
      integer  cal_IsLeap
      external cal_IsLeap

c     == end of interface ==

      lyfirst     = cal_IsLeap( firstyear, mythid )
      lylast      = cal_IsLeap( lastyear, mythid )

      nummonths   = cal_Intmonths( mythid )

      firstyear   = modelstartdate(1)/10000
      firstmonth  = mod(modelstartdate(1)/100,100)
      firstd      = mod(modelstartdate(1),100)
      lastyear    = modelenddate(1)/10000
      lastmonth   = mod(modelenddate(1)/100,100)
      lastd       = mod(modelenddate(1),100)
      lastsecs    = modelenddate(2)/10000*secondsperhour +
     &              mod(modelenddate(2)/100,100)*secondsperminute +
     &              mod(modelenddate(2),100)

      if ( nummonths .eq. 1 ) then
        if ( imonth .eq. 1 ) then
c--       Get the number of days in the first month.
          if ( firstmonth .eq. lastmonth ) then
            if (lastsecs .eq. 0) then
c--           Not really another day.
              lastday  = lastd - 1
            else
              lastday  = lastd
            endif
            firstday = 1
          else if ( mod(firstmonth+1,nmonthyear) .eq. lastmonth ) then
c--         This can only happen if we end at midnight of the first
c--         day of the next month.
            if ( ( modelenddate(2) .eq. 0 ) .and.
     &           ( mod(modelenddate(1),100) .eq. 1 ) ) then
              firstday = firstd
              lastday  = ndaymonth(firstmonth,lyfirst)
            else
c--           We do not end at midnight of the first day of
c--           the next month.
              ierr = 2704
              call cal_PrintError( ierr, mythid )
              stop ' stopped in cal_DaysForMonth.'
            endif
          else
c--         The first and the last month are inconsistent with imonth.
            ierr = 2703
            call cal_PrintError( ierr, mythid )
            stop ' stopped in cal_DaysForMonth.'
          endif
        else
c--       The variables nummonths and imonth are inconsistent;
c--       ( imonth .gt. nummonths ).
          ierr = 2702
          call cal_PrintError( ierr, mythid )
          stop ' stopped in cal_DaysForMonth.'
        endif

      else if ( nummonths .gt. 1 ) then
c--     More than one month of integration.
        if ( imonth .eq. 1 ) then
          firstday = 1
          lastday  = ndaymonth(firstmonth,lyfirst) - firstd + 1
        else if ( ( imonth .gt.     1     )        .and.
     &            ( imonth .lt. nummonths )       ) then
c--       Somewhere between first and last month.
          currentmonth = firstmonth
          currentyear  = firstyear
          numdays      = ndaymonth(firstmonth,lyfirst) - firstd + 1
          do i = 2,imonth-1
c--         Accumulate days of the intermediate months.
            currentmonth = mod(currentmonth+1,nmonthyear)
            if ( currentmonth .eq. 0 ) then
              currentmonth = 12
            endif
            if ( currentmonth .eq. 1 ) then
              currentyear = currentyear + 1
            endif
            lycurrent = cal_IsLeap( currentyear, mythid )
            numdays   = numdays + ndaymonth(currentmonth,lycurrent)
          enddo
          currentmonth = mod(currentmonth+1,nmonthyear)
          if ( currentmonth .eq. 0 ) then
             currentmonth = 12
          endif
          if ( currentmonth .eq. 1 ) then
            currentyear = currentyear + 1
          endif
          lycurrent = cal_IsLeap( currentyear, mythid )
          firstday  = numdays + 1
          lastday   = numdays + ndaymonth(currentmonth,lycurrent)
        else if ( imonth .eq. nummonths ) then
c--       The last month of the integration.
          currentmonth = firstmonth
          currentyear  = firstyear
          numdays      = ndaymonth(firstmonth,lyfirst) - firstd + 1
          do i = 2,nummonths-1
c--         Accumulate days of the intermediate months.
            currentmonth = mod(currentmonth+1,nmonthyear)
            if ( currentmonth .eq. 0 ) then
              currentmonth = 12
            endif
            if ( currentmonth .eq. 1 ) then
              currentyear = currentyear + 1
            endif
            lycurrent = cal_IsLeap( currentyear, mythid )
            numdays   = numdays + ndaymonth(currentmonth,lycurrent)
          enddo
c--       Prepare for the last month of integration.
          currentmonth = mod(currentmonth+1,nmonthyear)
          if ( currentmonth .eq. 0 ) then
             currentmonth = 12
          endif
          if ( currentmonth .eq. 1 ) then
            currentyear = currentyear + 1
          endif
          lycurrent = cal_IsLeap( currentyear, mythid )
          if ( ( modelenddate(2) .eq. 0 ) .and.
     &         ( mod(modelenddate(1),100) .eq. 1 ) ) then
c--         This can only happen if we end at midnight of the first
c--         day of the next month.
            lastday  = numdays + ndaymonth(currentmonth,lycurrent)
          else
c--         We do not stop at midnight of the first day of the
c--         next month.
            if (lastsecs .eq. 0) then
c--           But we might stop at midnight of a day.
              lastday = numdays + lastd - 1
            else
              lastday = numdays + lastd
            endif
          endif
          firstday = numdays + 1
        else
c--       The variables imonth and nummonths are inconsistent.
          ierr = 2705
          call cal_PrintError( ierr, mythid )
          stop ' stopped in cal_DaysForMonth.'
        endif
      else
c--     The number of months to integrate is wrong; check cal_IntMonths.
        ierr = 2701
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_DaysForMonth.'
      endif

c--   The number of days to integrate in the given month.
      ndays = lastday - firstday + 1

      return
      end

