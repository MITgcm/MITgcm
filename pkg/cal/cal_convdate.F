C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_convdate.F,v 1.1 2001/05/14 22:07:26 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

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
      integer check_sign_1
      integer check_sign_2
      integer ierr

c     == end of interface ==

      fac = 1

c     Check the sign of the date.

      check_sign_1 = sign(1,date(1))
      check_sign_2 = sign(1,date(2))

      if ( check_sign_1*check_sign_2 .ge. 0 ) then
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

