C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_dayspermonth.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_DaysPerMonth( 
     I                             myiter,
     I                             mytime,
     O                             ndays,
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE cal_DaysPerMonth
c     ==================================================================
c
c     o Given the iteration number and/or the current time of integration
c       return the number of days left in the current calendar month
c       including the day belonging to iter/currenttime.
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
c     SUBROUTINE cal_DaysPerMonth
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer ndays
      integer mythid

c     == local variables ==

      integer mydate(4)
      integer myday
      integer mymonth
      integer myyear
      integer finalyear
      integer finalmonth
      integer present
      integer final
      integer ierr

c     == end of interface ==

      call cal_GetDate( myiter, mytime, mydate, mythid )

      myday      = mod(mydate(1),100)
      mymonth    = mod(mydate(1)/100,100)
      myyear     = mydate(1)/10000
      present    = mydate(1)/100
      finalyear  = modelenddate(1)/10000
      finalmonth = mod(modelenddate(1)/100,100)
      final      = modelenddate(1)/100

      if (myyear .lt. finalyear) then
        ndays = ndaymonth(mymonth,mydate(3)) - myday + 1
      else if (myyear .eq. finalyear) then
        if (mymonth .eq. finalmonth) then
          ndays = mod(modelenddate(1),100) - myday + 1
        else
          ndays = ndaymonth(mymonth,mydate(3)) - myday + 1
        endif
      else

        ierr = 1201
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_DaysPerMonth.'

      endif

      return
      end
