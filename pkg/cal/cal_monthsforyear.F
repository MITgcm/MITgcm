C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_monthsforyear.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_MonthsForYear(
     I                              iyear,
     O                              firstmonth,
     O                              lastmonth,
     O                              nmonths,
     I                              mythid
     &                            )

c     ==================================================================
c     SUBROUTINE cal_MonthsForYear
c     ==================================================================
c
c     o Given the current year of integration this routine returns
c       the number of months affected by the integration as well as the
c       first month and the last month in the sequence of the total
c       number of months that are to be integrated.
c
c       This routine also checks consistency of variables quite
c       extensively.
c
c     started: Christian Eckert eckert@mit.edu  06-Apr-2000
c
c     changed: 
c
c     ==================================================================
c     SUBROUTINE cal_MonthsForYear
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer iyear
      integer firstmonth
      integer lastmonth
      integer nmonths
      integer mythid

c     == local variables ==

      integer ierr
      integer numyears
      integer firstyear
      integer firstmon
      integer lastyear
      integer lastmon
      integer lastday
      integer lastsecs

c     == external ==

      integer  cal_IntYears
      external cal_IntYears

c     == end of interface ==

      numyears = cal_IntYears( mythid )

      firstyear = modelstartdate(1)/10000
      firstmon  = mod(modelstartdate(1)/100,100)
      lastyear  = modelenddate(1)/10000
      lastmon   = mod(modelenddate(1)/100,100)
      lastday   = mod(modelenddate(1),100)
      lastsecs  = modelenddate(2)/10000*secondsperhour +
     &            mod(modelenddate(2)/100,100)*secondsperminute +
     &            mod(modelenddate(2),100)

      if ( numyears .eq. 1 ) then
c--     Only one calendar year affected by the integration.
        if ( iyear .eq. 1 ) then
          if ( firstyear .eq. lastyear ) then
            if ( (lastday .eq. 1) .and. (lastsecs .eq. 0) ) then
c--           Not really next month yet.
              lastmonth  = lastmon - firstmon
            else
c--           The most frequent case.
              lastmonth  = lastmon - firstmon + 1
            endif
            firstmonth = 1
          else if ( firstyear+1 .eq. lastyear ) then
c--         This is only the case if we end at midnight of 01-Jan
c--         of the next year.
            if ( ( modelenddate(2) .eq. 0) .and.
     &         ( mod(modelenddate(1),100) .eq. 1 ) .and.
     &           mod(modelenddate(1)/100,100) .eq. 1 ) then
              firstmonth = 1
              lastmonth  = nmonthyear - firstmon + 1
            else
c--           Inconsistent modelenddate; check cal_IntYears.
              ierr = 2804
              call cal_PrintError( ierr, mythid )
              stop ' stopped in cal_MonthsForYear.'
            endif
          else
c--         The specification of lastyear is wrong.
            ierr = 2803
            call cal_PrintError( ierr, mythid )
            stop ' stopped in cal_MonthsForYear.'
          endif
        else
c--       The variables numyears and iyear are inconsistent;
c--       ( iyear .gt. numyears ).
          ierr = 2802
          call cal_PrintError( ierr, mythid )
          stop ' stopped in cal_MonthsForYear.'
        endif

      else if ( numyears .gt. 1 ) then
c--     More than one year of integration.
        if ( iyear .eq. 1 ) then
          firstmonth = 1
          lastmonth  = nmonthyear - firstmon + 1
        else if ( ( iyear .gt. 1 )        .and.
     &            ( iyear .lt. numyears )       ) then
c--       Somewhere between first and last year.
          firstmonth = (nmonthyear - firstmon + 1) +
     &                 (iyear - 2)*nmonthyear + 1
          lastmonth  = (nmonthyear - firstmon + 1) +
     &                 (iyear - 2)*nmonthyear + nmonthyear
        else if ( iyear .eq. numyears ) then
c--       The last year of the integration.
          if ( lastyear .eq. (firstyear + numyears - 1) ) then
            if ( (lastday .eq. 1) .and. (lastsecs .eq. 0) ) then
c--           Not really next month yet.
              lastmonth  = (nmonthyear - firstmon + 1) +
     &                     (numyears - 2)*nmonthyear + lastmon - 1
            else
c--           The most frequent case.
              lastmonth  = (nmonthyear - firstmon + 1) +
     &                     (numyears - 2)*nmonthyear + lastmon
            endif
            firstmonth = (nmonthyear - firstmon + 1) +
     &                   (numyears - 2)*nmonthyear + 1
          else if ( lastyear .eq. (firstyear + numyears) ) then
c--         This is only the case if we end at midnight of 01-Jan.
            if ( ( modelenddate(2) .eq. 0) .and.
     &         ( mod(modelenddate(1),100) .eq. 1 ) .and.
     &           mod(modelenddate(1)/100,100) .eq. 1 ) then
              firstmonth = (nmonthyear - firstmon) +
     &                     (numyears - 2)*nmonthyear + 1
              lastmonth  = (nmonthyear - firstmon) +
     &                     (numyears - 2)*nmonthyear + nmonthyear
            else
c--           Inconsistent modelenddate; check cal_IntYears.
              ierr = 2807
              call cal_PrintError( ierr, mythid )
              stop ' stopped in cal_MonthsForYear.'
            endif
          else
c--         The variables lastyear and numyears are inconsistent.
            ierr = 2806
            call cal_PrintError( ierr, mythid )
            stop ' stopped in cal_MonthsForYear.'
          endif
        else
c--       The variables iyear and numyears are inconsistent.
          ierr = 2805
          call cal_PrintError( ierr, mythid )
          stop ' stopped in cal_MonthsForYear.'
        endif

      else
c--     The number of years to integrate is wrong; check cal_IntYears.
        ierr = 2801
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_MonthsForYear.'
      endif

c--   The number of months to integrate in the given year.
      nmonths = lastmonth - firstmonth + 1

      return
      end

