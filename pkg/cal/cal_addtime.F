C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_addtime.F,v 1.5 2012/04/07 16:21:05 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_ADDTIME(
     I                        date,
     I                        interval,
     O                        added,
     I                        myThid )

C     ==================================================================
C     SUBROUTINE cal_AddTime
C     ==================================================================
C
C     o Add a time interval either to a calendar date or to a time
C       interval.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
C              - restructured the original version in order to have a
C                better interface to the MITgcmUV.
C              Christian Eckert eckert@mit.edu  03-Feb-2000
C              - Introduced new routine and function names, cal_<NAME>,
C                for verion 0.1.3.
C              ralf.giering@fastopt.de 31-May-2000
C                datesecs was computed at wrong place (cph)
C              menemenlis@jpl.nasa.gov 8-Oct-2003
C              speed-up computations for long integration interval
C
C     ==================================================================
C     SUBROUTINE cal_AddTime
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER date(4)
      INTEGER interval(4)
      INTEGER added(4)
      INTEGER myThid

C     == external ==
      INTEGER  cal_IsLeap
      EXTERNAL cal_IsLeap

C     == local variables ==
      INTEGER intsecs
      INTEGER datesecs
      INTEGER nsecs
      INTEGER hhmmss
      INTEGER yi,mi,di,si,li,wi
      INTEGER ndays, ndays_left, days_in_year
      INTEGER date_1,date_2
      INTEGER intv_1,intv_2
      INTEGER fac
      INTEGER iday
      INTEGER switch
      INTEGER ndayssub
      INTEGER ierr
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

      IF ( cal_setStatus .LT. 1 ) THEN
        WRITE( msgBuf,'(2A,4I9)')  'CAL_ADDTIME: ',
     &       'date=',date(1),date(2),date(3),date(4)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,4I9)')  'CAL_ADDTIME: ',
     &   'interval=',interval(1),interval(2),interval(3),interval(4)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,I2,A)') 'CAL_ADDTIME: ',
     &    'called too early (cal_setStatus=',cal_setStatus,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CAL_ADDTIME'
      ENDIF

      if (interval(4) .ne. -1) then
        ierr = 601
        call cal_PrintError( ierr, myThid)
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
         call cal_ConvDate( date,yi,mi,di,si,li,wi,myThid )
         if ((interval(1) .ge. 0) .and.
     &        (interval(2) .ge. 0)) then
            nsecs = si + intsecs
            ndays = interval(1)+nsecs/secondsperday
            nsecs = mod(nsecs,secondsperday)

C     This used to be called by exf_getffieldrec -> cal_GetDate
C     and was very slow for a long integration interval.
c           do iday = 1,ndays
c             di = di + 1
c             if (di .gt. ndaymonth(mi,li)) then
c               di = 1
c               mi = mi + 1
c             endif
c             switch = (mi-1)/nmonthyear
c             yi = yi + switch
c             mi = mod(mi-1,nmonthyear)+1
c             if (switch .eq. 1) li = cal_IsLeap( yi, myThid )
c           enddo

C     Set start value
            ndays_left=ndays

C     First take care of February 29
            if ( usingGregorianCalendar ) then
               if ( mi.eq.2 .and. di.eq.29 .and. ndays_left.gt.1 ) then
                  mi = 3
                  di = 1
                  ndays_left = ndays_left - 1
               endif
            endif

C     Next compute year
            days_in_year=ndaysnoleap
            if ((mi.gt.2.and.cal_IsLeap(yi+1,myThid).eq.2).or.
     &           (mi.le.2.and.cal_IsLeap(yi,myThid).eq.2) )
     &           days_in_year=ndaysleap
            do while (ndays_left .ge. days_in_year)
               ndays_left = ndays_left - days_in_year
               yi = yi + 1
               days_in_year=ndaysnoleap
               if ((mi.gt.2.and.cal_IsLeap(yi+1,myThid).eq.2).or.
     &              (mi.le.2.and.cal_IsLeap(yi,myThid).eq.2) )
     &              days_in_year=ndaysleap
            enddo
            li = cal_IsLeap( yi, myThid )

C     Finally compute day and month
            do iday = 1,ndays_left
               di = di + 1
               if (di .gt. ndaymonth(mi,li)) then
                  di = 1
                  mi = mi + 1
               endif
               switch = (mi-1)/nmonthyear
               yi = yi + switch
               mi = mod(mi-1,nmonthyear)+1
               if (switch .eq. 1) li = cal_IsLeap( yi, myThid )
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
                  if (switch .eq. 1) li = cal_IsLeap( yi, myThid )
                  di = ndaymonth(mi,li)
               endif
            enddo
            wi = mod(wi+6-mod(ndayssub,7),7)+1
         endif
      endif

C     Convert to calendar format.
      added(1) = yi*10000 + mi*100 + di
      hhmmss   = nsecs/secondsperminute
      added(2) = hhmmss/minutesperhour*10000 +
     &     (mod(fac*hhmmss,minutesperhour)*100 +
     &     mod(fac*nsecs,secondsperminute))*fac
      added(3) = li
      added(4) = wi

      RETURN
      END
