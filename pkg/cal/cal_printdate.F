C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_printdate.F,v 1.1 2001/05/14 22:07:27 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      subroutine cal_PrintDate(
     I                          caldate,
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE cal_PrintDate
c     ==================================================================
c
c     o Print a calendar date.
c
c     Purpose: Print a date as used by the calendar tool in some nice
c              format by using the MITgcmuv's print routine
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  19-Jan-2000
c
c              - inserted print statements
c
c              Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c     ==================================================================
c     SUBROUTINE cal_PrintDate
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"

c     == routine arguments ==

c     caldate - date in the format used by the calendar tool.
c     mythid  - thread number for this instance of the routine.

      integer caldate(4)
      integer mythid

c     == local variables ==

      integer ierr
ce    character*(max_len_mbuf) msgbuf

c     == end of interface ==

      if ( caldate(4) .gt. 0 ) then
c       Print the calendar date.
        print*,caldate
ce      write(*,'(i10,i8,i3,i4)') caldate(1), caldate(2),
ce   &                            caldate(3), caldate(4)

      else if ( caldate(4) .eq. -1 ) then
c       Print the time interval.
        print*,caldate
ce      write(*,'(i10,i8,i3,i4)') caldate(1), caldate(2),
ce   &                            caldate(3), caldate(4)

      else

        ierr = 2001
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_PrintDate.'

      endif

      return
      end
