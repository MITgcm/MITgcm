C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_numints.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      integer function cal_NumInts(
     I                              date_a,
     I                              date_b,
     I                              timeint,
     I                              mythid
     &                            )

c     ==================================================================
c     SUBROUTINE cal_NumInts
c     ==================================================================
c
c     o Determine the number of time intervals of size timeint between
c       date_a and date_b.
c
c     Note: date_a and date_b should also be checked.
c           treat case ((passed(1) .ne. 0) .and. (timeint(1) .ne. 0))
c
c     started: Christian Eckert eckert@mit.edu  
c
c              - Introduced to version 0.1.4
c
c     changed: 
c
c
c     ==================================================================
c     SUBROUTINE cal_NumInts
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer date_a(4)
      integer date_b(4)
      integer timeint(4)
      integer mythid

c     == local variables ==

      _RL     timeintsecs
      integer passed(4)
      _RL     passedsecs
      integer ierr

c     == end of interface ==

      if ( timeint(4) .eq. -1 ) then
        call cal_TimePassed( date_a, date_b, passed, mythid )

        call cal_ToSeconds( passed,  passedsecs,  mythid )
        call cal_ToSeconds( timeint, timeintsecs, mythid )

        cal_NumInts = abs( passedsecs/timeintsecs )
      else
        ierr = 2501
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_NumInts.'
      endif

      return
      end
