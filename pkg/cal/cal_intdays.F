C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_intdays.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      integer function cal_IntDays(
     I                              mythid
     &                            )

c     ==================================================================
c     FUNCTION cal_IntDays
c     ==================================================================
c
c     o Return the number of calendar days that are affected by the 
c       current model integration.
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
c     FUNCTION cal_IntDays
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer tempdate(4)
      integer timediff(4)

c     == end of interface ==

      tempdate(1) = modelstartdate(1)
      tempdate(2) = 0
      tempdate(3) = modelstartdate(3)
      tempdate(4) = modelstartdate(4)

      call cal_SubDates( modelenddate, tempdate, timediff, mythid )

      if ( timediff(2) .ne. 0 ) then
        cal_IntDays = timediff(1) + 1
      else
        cal_IntDays = timediff(1)
      endif

      return
      end
