C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_timeinterval.F,v 1.4 2003/10/20 06:25:16 dimitri Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_TimeInterval(
     I                             timeint,
     I                             timeunit,
     O                             date,
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE cal_TimeInterval
c     ==================================================================
c
c     o Create an array in date format given a time interval measured in
c       units of timeunit.
c
c       Available time units: 'secs'
c                             'model'
c
c       Fractions of seconds are not resolved in this version.
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
c     SUBROUTINE cal_TimeInterval
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer date(4)
      _RL     timeint
      character*(*) timeunit
      integer mythid

c     == local variables ==

      integer fac
      integer nsecs
      integer hhmmss
      integer ierr
      _RL     tmp1, tmp2

c     == end of interface ==

      fac = 1
      if (timeint .lt. 0) fac = -1

      date(4) = -1
      date(3) =  0
      if (timeunit .eq. 'secs') then

        date(1) = int(timeint/float(secondsperday))
        tmp1    = date(1)
        tmp2    = secondsperday
        nsecs   = int(timeint - tmp1 * tmp2 )

      else if (timeunit .eq. 'model') then

        date(1) = int(timeint*modelstep/float(secondsperday))
        nsecs   = int(timeint*modelstep -
     &                float(date(1)) * float(secondsperday))

      else

        ierr = 701
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_TimeInterval.'

      endif

      hhmmss  = nsecs/secondsperminute
      date(2) = hhmmss/minutesperhour*10000 +
     &          (mod(fac*hhmmss,minutesperhour)*100 +
     &           mod(fac*nsecs,secondsperminute))*fac

      return
      end
