C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_addtime.F,v 1.4 2004/07/26 23:24:11 heimbach Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_AddTime(
     I                        date,
     I                        interval,
     O                        added,
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE cal_AddTime
c     ==================================================================
c
c     o Add a time interval either to a calendar date or to a time
c       interval.
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
c              ralf.giering@fastopt.de 31-May-2000
c                datesecs was computed at wrong place (cph)
c
c              menemenlis@jpl.nasa.gov 8-Oct-2003
c              speed-up computations for long integration interval
c
c     ==================================================================
c     SUBROUTINE cal_AddTime
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer date(4)
      integer interval(4)
      integer added(4)
      integer mythid

c     == local variables ==

      integer intsecs
      integer datesecs
      integer nsecs
      integer hhmmss
      integer yi,mi,di,si,li,wi
      integer ndays, ndays_left, days_in_year
      integer date_1,date_2
      integer intv_1,intv_2
      integer fac
      integer iday
      integer switch
      integer ndayssub
      integer ierr

c     == external ==

      integer  cal_IsLeap
      external cal_IsLeap

c     == end of interface ==

      if (interval(4) .ne. -1) then
        ierr = 601
        call cal_PrintError( ierr, mythid)
        stop ' stopped in cal_AddTime.'
      endif

      date_1 = 0
      date_2 = 0
      fac    = 1

      if (date(4) .eq. -1) then
         if (date(1) .ge. 0) then
            date_1 = date(1)
            date_2 = date(2)
            intv_1 = interval(1)
            intv_2 = interval(2)
         else
            if (interval(1) .lt. 0) then
               date_1 = -date(1)
               date_2 = -date(2)
               intv_1 = -interval(1)
               intv_2 = -interval(2)
               fac    = -1
            else
               date_1 = interval(1)
               date_2 = interval(2)
               intv_1 = date(1)
               intv_2 = date(2)
               fac    = 1
            endif
         endif
      else
         if (interval(1) .ge. 0) then
            intv_1 = interval(1)
            intv_2 = interval(2)
         else
            intv_1 = -interval(1)
            intv_2 = -interval(2)
            fac    = -1
         endif
      endif

      intsecs  = fac*(intv_2/10000*secondsperhour + 
     &     (mod(intv_2/100,100)*secondsperminute +
     &     mod(intv_2,100)))

      if (date(4) .eq. -1) then
         datesecs = date_2/10000*secondsperhour + 
     &        mod(date_2/100,100)*secondsperminute +
     &        mod(date_2,100)
         date_1 = date_1 + intv_1
         nsecs  = datesecs + intsecs
         if ((date_1 .gt. 0) .and.
     &        (nsecs  .lt. 0)) then
            date_1 = date_1 - 1
            nsecs  = nsecs + secondsperday
         endif
         nsecs = fac*nsecs
         yi     = 0
         mi     = 0
         di     = fac*date_1
         li     = 0
         wi     = -1
      else
         call cal_ConvDate( date,yi,mi,di,si,li,wi,mythid )
         if ((interval(1) .ge. 0) .and.
     &        (interval(2) .ge. 0)) then
            nsecs = si + intsecs
            ndays = interval(1)+nsecs/secondsperday
            nsecs = mod(nsecs,secondsperday)
            
c     This used to be called by exf_getffieldrec -> cal_GetDate
c     and was very slow for a long integration interval.
c           do iday = 1,ndays
c             di = di + 1
c             if (di .gt. ndaymonth(mi,li)) then
c               di = 1
c               mi = mi + 1
c             endif
c             switch = (mi-1)/nmonthyear
c             yi = yi + switch
c             mi = mod(mi-1,nmonthyear)+1
c             if (switch .eq. 1) li = cal_IsLeap( yi, mythid )
c           enddo

c     Set start value
            ndays_left=ndays

c     First take care of February 29
            if ( usingGregorianCalendar ) then
               if ( mi.eq.2 .and. di.eq.29 .and. ndays_left.gt.1 ) then
                  mi = 3
                  di = 1
                  ndays_left = ndays_left - 1
               endif
            endif

c     Next compute year
            days_in_year=ndaysnoleap
            if ((mi.gt.2.and.cal_IsLeap(yi+1,mythid).eq.2).or.
     &           (mi.le.2.and.cal_IsLeap(yi,mythid).eq.2) )
     &           days_in_year=ndaysleap
            do while (ndays_left .ge. days_in_year)
               ndays_left = ndays_left - days_in_year
               yi = yi + 1
               days_in_year=ndaysnoleap
               if ((mi.gt.2.and.cal_IsLeap(yi+1,mythid).eq.2).or.
     &              (mi.le.2.and.cal_IsLeap(yi,mythid).eq.2) )
     &              days_in_year=ndaysleap
            enddo
            li = cal_IsLeap( yi, mythid )

c     Finally compute day and month
            do iday = 1,ndays_left
               di = di + 1
               if (di .gt. ndaymonth(mi,li)) then
                  di = 1
                  mi = mi + 1
               endif
               switch = (mi-1)/nmonthyear
               yi = yi + switch
               mi = mod(mi-1,nmonthyear)+1
               if (switch .eq. 1) li = cal_IsLeap( yi, mythid )
            enddo
            wi = mod(wi+ndays-1,7)+1

         else
            nsecs = si + intsecs
            if (nsecs .ge. 0) then
               ndayssub = intv_1
            else
               nsecs = nsecs + secondsperday
               ndayssub = intv_1 + 1
            endif
            do iday = 1,ndayssub
               di = di - 1
               if (di .eq. 0) then
                  mi = mod(mi+10,nmonthyear)+1
                  switch = mi/nmonthyear
                  yi = yi - switch
                  if (switch .eq. 1) li = cal_IsLeap( yi, mythid )
                  di = ndaymonth(mi,li)
               endif
            enddo
            wi = mod(wi+6-mod(ndayssub,7),7)+1
         endif
      endif

c     Convert to calendar format.
      added(1) = yi*10000 + mi*100 + di
      hhmmss   = nsecs/secondsperminute
      added(2) = hhmmss/minutesperhour*10000 +
     &     (mod(fac*hhmmss,minutesperhour)*100 +
     &     mod(fac*nsecs,secondsperminute))*fac
      added(3) = li
      added(4) = wi

      return
      end
