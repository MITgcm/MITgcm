C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_checkdate.F,v 1.4 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_CheckDate(
     I                          date,
     O                          valid,
     O                          calerr,
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE cal_CheckDate
c     ==================================================================
c
c     o Check whether the array date conforms with the required format.
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
c     SUBROUTINE cal_CheckDate
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer date(4)
      logical valid
      integer calerr
      integer mythid

c     == local variables ==

      integer yy,mm,dd
      integer nsecs
      integer lp,wd
      integer hhmmss
      integer yymmdd
      integer fac
      _RL check_sign

c     == end of interface ==

      valid  = .true.
      hhmmss = 0
      calerr = 0
      fac    = 1

      check_sign = 1
      if ( ( (date(1).lt.0) .and. date(2).gt.0 ) .or.
     &     ( (date(1).gt.0) .and. date(2).lt.0 ) )
     &     check_sign = -1

      if (date(4) .le. 0) then
        if (date(4) .ne. -1) then
          calerr = 1801
        else
          if (date(3) .ne. 0) then
            calerr = 1802
          else
            if (check_sign .lt. 0) then
              calerr = 1803
            else
              call cal_ConvDate(date,yy,mm,dd,nsecs,lp,wd,mythid)
              if (nsecs .lt. 0) fac = -1
              hhmmss  = fac*nsecs/secondsperminute
              hhmmss  = hhmmss/minutesperhour*10000 +
     &                  mod(hhmmss,minutesperhour)*100 +
     &                  mod(fac*nsecs,secondsperminute)
              hhmmss  = fac*hhmmss
              if (date(2) .ne. hhmmss) then
                calerr = 1804
              endif
            endif
          endif
        endif
      else
        if (date(4) .gt. 8) then
          calerr = 1805
        else
          if ((date(3) .ne. 1) .and.
     &        (date(3) .ne. 2)) then
            calerr = 1806
          else
            if (check_sign .lt. 0) then
              calerr = 1803
            else
              call cal_ConvDate( date,yy,mm,dd,nsecs,lp,wd,mythid )
              if (date(1) .lt. refdate(1)) then
                calerr = 1807
              else
                hhmmss  = nsecs/secondsperminute
                hhmmss  = hhmmss/minutesperhour*10000 +
     &                    mod(hhmmss,minutesperhour)*100 +
     &                    mod(nsecs,secondsperminute)
              endif
              if (date(2) .ne. hhmmss) then
                calerr = 1804
              endif
              yymmdd  = yy*10000 + mm*100 + dd
              if (date(1) .ne. yymmdd) then
                calerr = 1808
              endif
            endif
          endif
        endif
      endif

      if (calerr .ne. 0) valid = .not. valid

      return
      end

