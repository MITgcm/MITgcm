C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_checkdate.F,v 1.6 2012/04/08 19:31:46 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_CHECKDATE(
     I                          date,
     O                          valid,
     O                          calerr,
     I                          myThid )

C     ==================================================================
C     SUBROUTINE cal_CheckDate
C     ==================================================================
C
C     o Check whether the array date conforms with the required format.
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
C     SUBROUTINE cal_CheckDate
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER date(4)
      LOGICAL valid
      INTEGER calerr
      INTEGER myThid

C     == local variables ==
C     msgBuf     :: Informational/error message buffer
      INTEGER yy, mm, dd
      INTEGER nsecs
      INTEGER lp,wd
      INTEGER hh, mn, ss
      INTEGER hhmmss
      LOGICAL wrong_sign
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

      valid  = .true.
      calerr = 0
c     wrong_sign = date(1)*date(2).lt.0
C     product above might go over integer*4 limit; better to check each one:
      wrong_sign = ( (date(1).LT.0) .AND. date(2).GT.0 )
     &        .OR. ( (date(1).GT.0) .AND. date(2).LT.0 )

      IF ( wrong_sign ) THEN
C         cal_CheckDate: Signs of first two components unequal
          calerr = 1803
C         invalid sign is fatal (since we need to check for valid month)
          valid = .FALSE.
      ELSEIF ( cal_setStatus .LT. 1 ) THEN
          WRITE( msgBuf,'(2A,4I9)')  'CAL_CHECKDATE:',
     &      ' date=',date(1),date(2),date(3),date(4)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE( msgBuf,'(2A,I2,A)') 'CAL_CHECKDATE:',
     &      ' called too early (cal_setStatus=',cal_setStatus,' )'
          CALL PRINT_ERROR( msgBuf, myThid )
c         valid = .FALSE.

      ELSEIF ( date(4).LE.0 ) THEN
C--   date without weekday (date(4)= -1) and no LeapYear index (date(3)= 0)

        IF ( date(4).NE.-1 ) THEN
C         cal_CheckDate: Last component of array not valid
          calerr = 1801
        ELSEIF ( date(3).NE.0 ) THEN
C         cal_CheckDate: Third component of interval array not 0
          calerr = 1802
        ENDIF

      ELSE
C--   normal date with weekday (date(4)> 0) and LeapYear index (date(3)> 0)

        CALL CAL_CONVDATE( date, yy, mm, dd, nsecs, lp, wd, myThid )
        IF ( mm.EQ.0 .OR. ABS(mm).GT.nMonthYear ) THEN
          WRITE( msgBuf,'(2A,I10)') 'CAL_CHECKDATE:',
     &      ' Invalid month in date(1)=', date(1)
          CALL PRINT_ERROR( msgBuf, myThid )
C       invalid month is fatal (used as index in nDayMonth array)
          valid = .FALSE.
        ELSEIF ( wd.LT.1 .OR. wd.GT.7 ) THEN
C         cal_CheckDate: Weekday indentifier not correct
          calerr = 1805
C       invalid weekday is not safe (index in dayOfWeek, but just to print)
        ELSEIF ( lp.NE.1 .AND. lp.NE.2 ) then
C         cal_CheckDate: Leap year identifier not correct
          calerr = 1806
C       invalid leap-year index is fatal (used as index in nDayMonth array)
          valid = .FALSE.
        ELSEIF ( dd.EQ.0 .OR. ABS(dd).GT.nMaxDayMonth ) THEN
C-note: can refine above using Nb of days of the corresponding month:
c       ELSEIF ( dd.EQ.0 .OR. ABS(dd).GT.nDayMonth(mm,lp) ) THEN
          WRITE( msgBuf,'(2A,I10)') 'CAL_CHECKDATE:',
     &      ' Invalid day in date(1)=', date(1)
          CALL PRINT_ERROR( msgBuf, myThid )
        ELSEIF ( date(1).LT.refDate(1) ) THEN
C         cal_CheckDate: Calendar date before predef. reference date
          calerr = 1807
        ENDIF

      ENDIF

      IF ( valid .AND. cal_setStatus.GE.1 ) THEN
C--   check 2nd component (hhmmss=date(2)) and print warning
        hhmmss  = ABS(date(2))
        hh = hhmmss/10000
        mn = MOD(hhmmss/100,100)
        ss = MOD(hhmmss,100)
        IF ( ss.GE.secondsPerMinute ) THEN
          WRITE( msgBuf,'(2A,I10)') '** WARNING ** CAL_CHECKDATE:',
     &      ' Invalid Seconds in date(2)=', date(2)
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
        IF ( mn.GE.minutesPerHour ) THEN
          WRITE( msgBuf,'(2A,I10)') '** WARNING ** CAL_CHECKDATE:',
     &      ' Invalid Minutes in date(2)=', date(2)
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
        IF ( hh.GE.hoursPerDay ) THEN
          WRITE( msgBuf,'(2A,I10)') '** WARNING ** CAL_CHECKDATE:',
     &      ' Invalid  Hours  in date(2)=', date(2)
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF

      RETURN
      END
