C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_fulldate.F,v 1.1 2001/05/14 22:07:26 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      subroutine cal_FullDate(
     I                         yymmdd,
     I                         hhmmss,
     O                         date,
     I                         mythid
     &                       )

c     ==================================================================
c     SUBROUTINE cal_FullDate
c     ==================================================================
c
c     o Set a date array given the year, month, day, hour, minute,
c       and second. Check the input for errors.
c
c              
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
c
c              - restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c     ==================================================================
c     SUBROUTINE cal_FullDate
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer yymmdd
      integer hhmmss
      integer date(4)
      integer mythid

c     == local variables ==

      integer theyear
      integer numberofdays(4)
      integer calerr

      logical valid

      integer  cal_IsLeap
      external cal_IsLeap

c     == end of interface ==

      date(1) = yymmdd
      date(2) = hhmmss
      date(3) = 1
      date(4) = 1

c     Check the input for obvious errors.
      call cal_CheckDate( date, valid, calerr, mythid )

      if (valid) then
c       Determine whether we are in a leap year or not.
        theyear = yymmdd/10000
        date(3) = cal_IsLeap( theyear, mythid )

c       Determine the day of the week.
        call cal_TimePassed( refdate, date, numberofdays, mythid )
        date(4) = mod(numberofdays(1),7)+1
      endif

      return
      end
