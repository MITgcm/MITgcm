C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_isleap.F,v 1.1 2001/05/14 22:07:26 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      integer function cal_IsLeap(
     I                             year,
     I                             mythid
     &                           )

c     ==================================================================
c     FUNCTION cal_IsLeap
c     ==================================================================
c
c     o In case the Gregorian calendar is used determine whether the
c       given year is a leap year or not.
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
c     FUNCTION cal_IsLeap
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer year
      integer mythid

c     == end of interface ==

      if ( usingGregorianCalendar ) then
        if ( mod(year,4) .ne. 0 ) then
          cal_IsLeap = 1
        else
          cal_IsLeap = 2
          if ( (mod(year,100) .eq. 0)  .and.
     &         (mod(year,400) .ne. 0) ) then
            cal_IsLeap = 1
          endif
        endif
      else if ( usingJulianCalendar ) then
        if ( mod(year,4) .ne. 0 ) then
          cal_IsLeap = 1
        else
          cal_IsLeap = 2
        endif
      else
       cal_IsLeap = 1
      endif

      return
      end

