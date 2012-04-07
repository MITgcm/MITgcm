C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_timepassed.F,v 1.3 2012/04/07 16:21:05 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_TIMEPASSED(
     I                           initialdate,
     I                           finaldate,
     O                           numdays,
     I                           myThid )

C     ==================================================================
C     SUBROUTINE cal_TimePassed
C     ==================================================================
C
C     o Calculate the time that passed between initialdate and
C       finaldate.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
C              - restructured the original version in order to have a
C                better interface to the MITgcmUV.
C              Christian Eckert eckert@mit.edu  03-Feb-2000
C              - Introduced new routine and function names, cal_<NAME>,
C                for verion 0.1.3.
C
C     ==================================================================
C     SUBROUTINE cal_TimePassed
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER initialdate(4)
      INTEGER finaldate(4)
      INTEGER numdays(4)
      INTEGER myThid

C     == external ==
      INTEGER  cal_IsLeap
      EXTERNAL cal_IsLeap

C     == local variables ==
      INTEGER yi,yf
      INTEGER mi,mf
      INTEGER di,df
      INTEGER si,sf
      INTEGER li,lf
      INTEGER wi,wf
      INTEGER cdi,cdf
      INTEGER csi,csf
      INTEGER ndays
      INTEGER nsecs
      INTEGER hhmmss
      INTEGER imon
      INTEGER iyr
      INTEGER ierr
      LOGICAL swap
      LOGICAL caldates
      LOGICAL nothingtodo
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == end of interface ==

      IF ( cal_setStatus .LT. 1 ) THEN
        WRITE( msgBuf,'(2A,4I9)') 'CAL_TIMEPASSED: ', 'initialdate=',
     &    initialdate(1),initialdate(2),initialdate(3),initialdate(4)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,4I9)') 'CAL_TIMEPASSED: ', 'finaldate=',
     &    finaldate(1),finaldate(2),finaldate(3),finaldate(4)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,I2,A)') 'CAL_TIMEPASSED: ',
     &    'called too early (cal_setStatus=',cal_setStatus,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CAL_TIMEPASSED'
      ENDIF

      nothingtodo = .false.

C     Initialise output.
      numdays(1) =  0
      numdays(2) =  0
      numdays(3) =  0
      numdays(4) = -1

      if ((initialdate(4) .gt. 0) .eqv.
     &    (  finaldate(4) .gt. 0)) then

        caldates = (initialdate(4) .gt. 0) .and.
     &             (  finaldate(4) .gt. 0)

C       Check relation between initial and final dates.
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
            call cal_ConvDate(   finaldate,yi,mi,di,si,li,wi,myThid )
            call cal_ConvDate( initialdate,yf,mf,df,sf,lf,wf,myThid )
          else
            call cal_ConvDate( initialdate,yi,mi,di,si,li,wi,myThid )
            call cal_ConvDate(   finaldate,yf,mf,df,sf,lf,wf,myThid )
          endif

C         Determine the time interval.
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
              ndays = (ndaysnoleap - 1) + cal_IsLeap( yi, myThid ) -
     &                cdi - ndaymonth(mi,li)
              do iyr = yi+1,yf-1
                ndays = ndays + (ndaysnoleap - 1) +
     &                  cal_IsLeap( iyr, myThid )
              enddo
              ndays = ndays + cdf
              csi   = ndaymonth(mi,li)*secondsperday - csi
              nsecs = csi + csf
            endif
          endif

C         Convert to calendar format.
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
C         Dates are equal.
        endif

      else

        ierr = 501
        call cal_PrintError( ierr, myThid )
        stop ' stopped in cal_TimePassed'

      endif

      RETURN
      END
