C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_subdates.F,v 1.1 2001/05/14 22:07:27 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      subroutine cal_SubDates(
     I                         finaldate,
     I                         initialdate,
     O                         diffdate,
     I                         mythid
     &                       )

c     ==================================================================
c     SUBROUTINE cal_SubDates
c     ==================================================================
c
c     o Subtract two dates. In case calendar dates are given finaldate
c       must be after initialdate.
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
c     SUBROUTINE cal_SubDates
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer finaldate(4)
      integer initialdate(4)
      integer diffdate(4)
      integer mythid

c     == local variables ==

      integer workdate(4)
      integer ierr

c     == end of interface ==

      if ((initialdate(4) .gt. 0) .eqv.
     &    (  finaldate(4) .gt. 0)) then


        if (initialdate(4) .eq. -1) then
c         The time interval is subtracted.
          workdate(1) = -initialdate(1)
          workdate(2) = -initialdate(2)
          workdate(3) =  0
          workdate(4) = -1
          call cal_AddTime( finaldate, workdate, diffdate, mythid )
        else
c         The time interval between initial and final date is calculated.
          call cal_TimePassed( 
     &          initialdate, finaldate, diffdate, mythid )
        endif
      else

        ierr = 801
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_SubDates.'

      endif

 100  continue

      return
      end
