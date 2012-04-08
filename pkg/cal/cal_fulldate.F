C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_fulldate.F,v 1.5 2012/04/08 19:31:46 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_FULLDATE(
     I                         yymmdd,
     I                         hhmmss,
     O                         date,
     I                         myThid )

C     ==================================================================
C     SUBROUTINE cal_FullDate
C     ==================================================================
C
C     o Set a date array given the year, month, day, hour, minute,
C       and second. Check the input for errors.
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
C     SUBROUTINE cal_FullDate
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
C     myThid - thread number for this instance of the routine.
      INTEGER yymmdd
      INTEGER hhmmss
      INTEGER date(4)
      INTEGER myThid

C     == functions ==
      INTEGER  cal_IsLeap
      EXTERNAL cal_IsLeap

C     == local variables ==
      INTEGER theyear
      INTEGER numberOfDays(4)
      INTEGER calerr
      LOGICAL valid
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

      date(1) = yymmdd
      date(2) = hhmmss
      date(3) = 1
      date(4) = 1

      IF ( cal_setStatus .LT. 1 ) THEN
        WRITE( msgBuf,'(A,2(A,I9))')  'CAL_FULLDATE: ',
     &          'yymmdd=',yymmdd,' , hhmmss=',hhmmss
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,I2,A)') 'CAL_FULLDATE: ',
     &    'called too early (cal_setStatus=',cal_setStatus,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CAL_FULLDATE'
      ENDIF

C     Check the input for obvious errors.
      CALL CAL_CHECKDATE( date, valid, calerr, myThid )
      IF ( calerr.NE.0 ) THEN
        WRITE( msgBuf,'(A,2(A,I9))')  'CAL_FULLDATE: ',
     &          'yymmdd=',yymmdd,' , hhmmss=',hhmmss
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL CAL_PRINTERROR( calerr, myThid )
      ENDIF

      IF (valid) THEN
C       Determine whether we are in a leap year or not.
        theyear = yymmdd/10000
        date(3) = cal_IsLeap( theyear, myThid )

C       Determine the day of the week.
        CALL CAL_TIMEPASSED( refDate, date, numberOfDays, myThid )
        IF ( numberOfDays(1).LT.0 ) THEN
C-      when numberOfDays < 0 ,  TIMEPASSED output is not very logical (print);
C       in addition, in this case, formula below is wrong (skipped).
          WRITE(errorMessageUnit,'(2A,4I9)') ' in CAL_FULLDATE: ',
     &     'refDate=', refDate(1), refDate(2), refDate(3), refDate(4)
          WRITE(errorMessageUnit,'(2A,4I9)') ' in CAL_FULLDATE: ',
     &     '   date=', date(1), date(2), date(3), date(4)
          WRITE(errorMessageUnit,'(2A,4I9)') ' in CAL_FULLDATE: ',
     &     'numDays=', numberOfDays(1), numberOfDays(2),
     &                 numberOfDays(3), numberOfDays(4)
        ELSE
          date(4) = MOD(numberOfDays(1),7)+1
        ENDIF
      ELSE
        WRITE( msgBuf,'(2A)') 'CAL_FULLDATE: ',
     &    'fatal error from cal_CheckDate'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CAL_FULLDATE'
      ENDIF

      RETURN
      END
