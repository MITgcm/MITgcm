C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_toseconds.F,v 1.1 2001/05/14 22:07:27 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      subroutine cal_ToSeconds(
     I                          date,
     O                          timeint,
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE cal_ToSeconds
c     ==================================================================
c
c     o Given a time interval as a date array return the number of
c       seconds in that time interval.
c
c       If one wanted to use calendar dates in this routine, then
c       the date should be after the calendar's refdate and timeint
c       would be the number of seconds that have elapsed since the
c       refdate. Of course this can also be done by first calling
c       sub cal_TimePassed and the calling this routine with the
c       resulting time interval array.
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
c     SUBROUTINE cal_ToSeconds
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer date(4)
      _RL     timeint
      integer mythid

c     == local variables ==

      integer fac
      integer ndays
      integer nsecs
      integer hhmmss
      integer check_sign_1
      integer check_sign_2
      integer ierr

c     == end of interface ==

      check_sign_1 = sign(1,date(1))
      check_sign_2 = sign(1,date(2))
      if ((date(4) .eq. -1) .and.
     &    (date(3) .eq.  0) .and.
     &    (check_sign_1*check_sign_2 .ge. 0)) then
        if ((date(1) .lt. 0) .or.
     &      (date(2) .lt. 0)) then
          ndays  = -date(1)
          hhmmss = -date(2)
          fac    = -1
        else
          ndays  = date(1)
          hhmmss = date(2)
          fac    = 1
        endif
        nsecs   = ndays*secondsperday +
     &            (hhmmss/10000)*secondsperhour +
     &            mod(hhmmss/100,100)*secondsperminute +
     &            mod(hhmmss,100)
        timeint = fac*nsecs
      else

        ierr = 1001
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_ToSeconds.'

      endif

      return
      end
