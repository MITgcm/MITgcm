C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_monthsperyear.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_MonthsPerYear(
     I                              myiter,
     I                              mytime,
     O                              nmonths,
     I                              mythid
     &                            )

c     ==================================================================
c     SUBROUTINE cal_MonthsPerYear
c     ==================================================================
c
c     o Given the iteration number during the integration and/or the
c       currenttime of integration return the number of months left in
c       the current calendar year including the one containing
c       currenttime/iter.
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
c     SUBROUTINE cal_MonthsPerYear
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer nmonths
      integer mythid

c     == local variables ==

      integer mydate(4)
      integer present
      integer final

c     == end of interface ==

      call cal_GetDate( myiter, mytime, mydate, mythid )

      present = mydate(1)/10000
      final   = modelenddate(1)/10000

      if (present .eq. final) then
        present = mod(mydate(1)/100,100)
        final   = mod(modelenddate(1)/100,100)
ce      nmonths = final - present + 1
      else
        present = mod(mydate(1)/100,100)
        final   = nmonthyear
ce      nmonths = nmonthyear - mod(mydate(1)/100,100) + 1
      endif
      nmonths = final - present + 1

      return
      end
