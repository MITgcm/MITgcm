C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_convdate.F,v 1.4 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_ConvDate(
     I                         date,
     O                         yy, mm, dd, ss,
     O                         lp, wd,
     I                         mythid
     &                       )

c     ==================================================================
c     SUBROUTINE cal_ConvDate
c     ==================================================================
c
c     o Decompose the first part of a date array.
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
c              21-Sep-2003: fixed check_sign logic to work with
c              negative intervals (menemenlis@jpl.nasa.gov)
c
c     ==================================================================
c     SUBROUTINE cal_ConvDate
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer date(4)
      integer mythid

c     == local variables ==

      integer yy
      integer mm
      integer dd
      integer ss
      integer lp
      integer wd
      integer fac
      integer date_1
      integer date_2
      integer ierr
      integer check_sign

c     == end of interface ==

      fac = 1

c     Check the sign of the date.

      check_sign = 1
      if ( ( (date(1).lt.0) .and. date(2).gt.0 ) .or.
     &     ( (date(1).gt.0) .and. date(2).lt.0 ) )
     &     check_sign = -1

      if ( check_sign .ge. 0 ) then
        if (date(1) .eq. 0) then
          date_1 = date(1)
          if (date(2) .lt. 0) then
            date_2 = -date(2)
            fac    = -1
          else
            date_2 = date(2)
            fac    = 1
          endif
        else if (date(1) .lt. 0) then
          date_1 = -date(1)
          date_2 = -date(2)
          fac    = -1
        else
          date_1 = date(1)
          date_2 = date(2)
          fac    = 1
        endif
      else

        ierr = 901
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_ConvDate.'

      endif

c     Decompose the entries.
      if (date(4) .ne. -1) then
        yy = date_1/10000
        mm = mod(date_1/100,100)
        dd = mod(date_1,100)
      else
        yy = 0
        mm = 0
        dd = date_1
      endif
      ss = mod(date_2,100) +
     &     mod(date_2/100,100)*secondsperminute +
     &     date_2/10000*secondsperhour

c     Include the sign.
      yy = fac*yy
      mm = fac*mm
      dd = fac*dd
      ss = fac*ss

      lp = date(3)
      wd = date(4)

      return
      end

