C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_fulldate.F,v 1.4 2012/04/07 16:21:05 jmc Exp $
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
      INTEGER numberofdays(4)
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
      call cal_CheckDate( date, valid, calerr, myThid )

      if (valid) then
C       Determine whether we are in a leap year or not.
        theyear = yymmdd/10000
        date(3) = cal_IsLeap( theyear, myThid )

C       Determine the day of the week.
        call cal_TimePassed( refdate, date, numberofdays, myThid )
        date(4) = mod(numberofdays(1),7)+1
      else
        call cal_PrintError( calerr, myThid )
cml   if you want the CheckDate error to stop the model uncomment
cml   the following line, otherwise there is just going to be a
cml   warning.
cml        stop ' stopped in cal_FullDate'
      endif

      RETURN
      END
