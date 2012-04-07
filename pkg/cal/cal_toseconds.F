C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_toseconds.F,v 1.9 2012/04/07 16:21:05 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_TOSECONDS(
     I                          date,
     O                          timeint,
     I                          myThid )

C     ==================================================================
C     SUBROUTINE cal_ToSeconds
C     ==================================================================
C
C     o Given a time interval as a date array return the number of
C       seconds in that time interval.
C
C       If one wanted to use calendar dates in this routine, then
C       the date should be after the calendar refdate and timeint
C       would be the number of seconds that have elapsed since the
C       refdate. Of course this can also be done by first calling
C       sub cal_TimePassed and then calling this routine with the
C       resulting time interval array.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
C              - restructured the original version in order to have a
C                better interface to the MITgcmUV.
C              Christian Eckert eckert@mit.edu  03-Feb-2000
C              - Introduced new routine and function names, cal_<NAME>,
C                for verion 0.1.3.
C              21-Sep-2003: fixed check_sign logic to work with
C              negative intervals (menemenlis@jpl.nasa.gov)
C
C     ==================================================================
C     SUBROUTINE cal_ToSeconds
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER date(4)
      _RL     timeint
      INTEGER myThid

C     == local variables ==
      _RL     fac, nsecs, ndays
      INTEGER ierr, check_sign, hhmmss
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

c     print *,'cal_toseconds: date',date
c     print *,'cal_toseconds: timeint',timeint

      IF ( cal_setStatus .LT. 1 ) THEN
        WRITE( msgBuf,'(2A,4I9)') 'CAL_TOSECONDS: ',
     &       'date=',date(1),date(2),date(3),date(4)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,I2,A)') 'CAL_TOSECONDS: ',
     &    'called too early (cal_setStatus=',cal_setStatus,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CAL_CONVDATE'
      ENDIF

      check_sign = 1
      if ( ( (date(1).lt.0) .and. date(2).gt.0 ) .or.
     &     ( (date(1).gt.0) .and. date(2).lt.0 ) )
     &     check_sign = -1

      if (((date(4) .eq. -1) .and.
     &    (date(3) .eq.  0) .and.
     &    (check_sign .ge. 0)) .or.
     &    usingModelCalendar) then
        if ((date(1) .lt. 0) .or.
     &      (date(2) .lt. 0)) then
          ndays  = -date(1)
          hhmmss = -date(2)
          fac    = -1
        else
          ndays  = date(1)
          hhmmss = date(2)
          fac    = 1
        endif
        nsecs   = ndays*secondsperday +
     &            (hhmmss/10000)*secondsperhour +
     &            mod(hhmmss/100,100)*secondsperminute +
     &            mod(hhmmss,100)
        timeint = fac*nsecs
      else

        ierr = 1001
        call cal_PrintError( ierr, myThid )
        stop ' stopped in cal_ToSeconds.'

      endif

      RETURN
      END
