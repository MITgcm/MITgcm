C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_getdate.F,v 1.1 2001/05/14 22:07:26 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      subroutine cal_GetDate(
     I                        myiter,
     I                        mytime,
     O                        mydate,
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE cal_GetDate
c     ==================================================================
c
c     o Determine the current date given the iteration number and/or the
c       current time of integration.
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
c     SUBROUTINE cal_GetDate
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mydate(4)
      integer mythid

c     == local variables ==

      _RL     secs
      integer workdate(4)

c     == end of interface ==

      if (mytime .lt. 0) then
        if (myiter .ge. 0) then
          secs = float(myiter - modeliter0)*modelstep
        else
          print*,' cal_GetDate: Not a valid input!'
        endif
      else
        secs = mytime - modelstart
      endif

      call cal_TimeInterval( secs, 'secs', workdate, mythid )
      call cal_AddTime( modelstartdate, workdate, mydate, mythid )

      return
      end

