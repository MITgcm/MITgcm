C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_timepassed.F,v 1.1 2001/05/14 22:07:27 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      subroutine cal_TimePassed(
     I                           initialdate,
     I                           finaldate,
     O                           numdays,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cal_TimePassed
c     ==================================================================
c
c     o Calculate the time that passed between initialdate and
c       finaldate.
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
c     SUBROUTINE cal_TimePassed
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer initialdate(4)
      integer finaldate(4)
      integer numdays(4)
      integer mythid

c     == local variables ==

      integer yi,yf
      integer mi,mf
      integer di,df
      integer si,sf
      integer li,lf
      integer wi,wf
      integer cdi,cdf
      integer csi,csf
      integer ndays
      integer nsecs
      integer hhmmss
      integer imon
      integer iyr
      integer ierr

      logical swap
      logical caldates
      logical nothingtodo

c     == external ==

      integer  cal_IsLeap
      external cal_IsLeap

c     == end of interface ==

      nothingtodo = .false.

c     Initialise output.
      numdays(1) =  0
      numdays(2) =  0
      numdays(3) =  0
      numdays(4) = -1

      if ((initialdate(4) .gt. 0) .eqv.
     &    (  finaldate(4) .gt. 0)) then

        caldates = (initialdate(4) .gt. 0) .and.
     &             (  finaldate(4) .gt. 0)

c       Check relation between initial and final dates.
        if (initialdate(1) .eq. finaldate(1)) then
          if (initialdate(2) .eq. finaldate(2)) then
            nothingtodo = .true.
          else if (initialdate(2) .gt. finaldate(2)) then
            swap = .true.
          else
            swap = .false.
          endif
        else if (initialdate(1) .gt. finaldate(1)) then
          swap = .true.
        else
          swap = .false.
        endif

        if (.not. nothingtodo) then

          if (swap) then
            call cal_ConvDate(   finaldate,yi,mi,di,si,li,wi,mythid )
            call cal_ConvDate( initialdate,yf,mf,df,sf,lf,wf,mythid )
          else
            call cal_ConvDate( initialdate,yi,mi,di,si,li,wi,mythid )
            call cal_ConvDate(   finaldate,yf,mf,df,sf,lf,wf,mythid )
          endif

c         Determine the time interval.
          if (.not. caldates) then
            ndays = df - di
            nsecs = sf - si
            if (nsecs .lt. 0) then
              nsecs = nsecs + secondsperday
              ndays = ndays - 1
            endif
            ndays = ndays + nsecs/secondsperday
            nsecs = mod(nsecs,secondsperday)
          else
            si = si + (di-1)*secondsperday
            sf = sf + (df-1)*secondsperday
            cdi = 0
            do imon = 1,mod(mi-1,12)
              cdi = cdi + ndaymonth(imon,li)
            enddo
            csi = si
            cdf = 0
            do imon = 1,mod(mf-1,12)
              cdf = cdf + ndaymonth(imon,lf)
            enddo
            csf = sf

            if (yi .eq. yf) then
              ndays = (cdf + csf/secondsperday) -
     &                (cdi + csi/secondsperday)
              nsecs = (csf - (csf/secondsperday)*secondsperday) -
     &                (csi - (csi/secondsperday)*secondsperday)
              if (nsecs .lt. 0) then
                nsecs = nsecs + secondsperday
                ndays = ndays - 1
              endif
            else
              ndays = (ndaysnoleap - 1) + cal_IsLeap( yi, mythid ) -
     &                cdi - ndaymonth(mi,li)
              do iyr = yi+1,yf-1
                ndays = ndays + (ndaysnoleap - 1) +
     &                  cal_IsLeap( iyr, mythid )
              enddo
              ndays = ndays + cdf
              csi   = ndaymonth(mi,li)*secondsperday - csi
              nsecs = csi + csf
            endif
          endif

c         Convert to calendar format.
          numdays(1) = ndays + nsecs/secondsperday
          nsecs      = mod(nsecs,secondsperday)
          hhmmss     = nsecs/secondsperminute
          numdays(2) = hhmmss/minutesperhour*10000 +
     &                 mod(hhmmss,minutesperhour)*100 +
     &                 mod(nsecs,secondsperminute)
          if (swap) then
            numdays(1) = -numdays(1)
            numdays(2) = -numdays(2)
          endif

        else
c         Dates are equal.
        endif

      else

        ierr = 501
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_TimePassed'

      endif

      return
      end

