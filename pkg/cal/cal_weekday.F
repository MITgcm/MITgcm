C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_weekday.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_WeekDay(
     I                        indate,
     O                        daystring,
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE cal_WeekDay
c     ==================================================================
c
c     o Given a calendar date return the weekday as a string.
c
c     started: Christian Eckert eckert@mit.edu  19-Jan-1999
c
c     changed: Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c     ==================================================================
c     SUBROUTINE cal_WeekDay
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer indate(4)
      character*(3) daystring
      integer mythid

c     == local variables ==

      integer nweekday
      integer numberofdays(4)

c     == end of interface ==

c     Determine the day of the week.
      call cal_TimePassed( refdate, indate, numberofdays, mythid )
      nweekday = mod(numberofdays(1),7)+1

      daystring = dayofweek(nweekday)

      return
      end

