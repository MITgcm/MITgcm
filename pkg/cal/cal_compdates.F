C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_compdates.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      logical function cal_CompDates(
     I                                date_a,
     I                                date_b,
     I                                mythid
     &                              )

c     ==================================================================
c     FUNCTION cal_CompDates
c     ==================================================================
c
c     o Compare two calendar dates or time interval.
c              
c     started: Christian Eckert eckert@mit.edu  24-Feb-2000
c
c              - Introduced to calendar version 0.1.4
c
c     changed: 
c
c     ==================================================================
c     FUNCTION cal_CompDates
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer date_a(4)
      integer date_b(4)
      integer mythid

c     == local variables ==

c     == end of interface ==

      if ( ( date_a(1) .eq. date_b(1) ) .and.
     &     ( date_a(2) .eq. date_b(2) ) .and.
     &     ( date_a(3) .eq. date_b(3) ) .and.
     &     ( date_a(4) .eq. date_b(4) )       ) then
        cal_CompDates = .true.
      else
        cal_CompDates = .false.
      endif

      return
      end

