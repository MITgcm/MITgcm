C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_convdate.F,v 1.6 2012/04/08 19:22:34 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_CONVDATE(
     I                         date,
     O                         yy, mm, dd, ss,
     O                         lp, wd,
     I                         myThid )

C     ==================================================================
C     SUBROUTINE cal_ConvDate
C     ==================================================================
C
C     o Decompose the first part of a date array.
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
C     SUBROUTINE cal_ConvDate
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER date(4)
      INTEGER yy, mm, dd
      INTEGER ss, lp, wd
      INTEGER myThid

C     == local variables ==
      INTEGER fac
      INTEGER date_1
      INTEGER date_2
      INTEGER ierr
      LOGICAL wrong_sign
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

      IF ( cal_setStatus .LT. 1 ) THEN
        WRITE( msgBuf,'(2A,4I9)') 'CAL_CONVDATE: ',
     &       'date=',date(1),date(2),date(3),date(4)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,I2,A)') 'CAL_CONVDATE: ',
     &    'called too early (cal_setStatus=',cal_setStatus,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CAL_CONVDATE'
      ENDIF

C     Check the sign of the date.
      fac = 1
      wrong_sign = ( (date(1).lt.0) .and. date(2).gt.0 )
     &        .OR. ( (date(1).gt.0) .and. date(2).lt.0 )

      if ( wrong_sign ) then
        ierr = 901
        call cal_PrintError( ierr, myThid )
        stop ' stopped in cal_ConvDate.'
      else
        if ( date(1).lt.0 .OR. date(2).lt.0 ) then
          date_1 = -date(1)
          date_2 = -date(2)
          fac    = -1
        else
          date_1 = date(1)
          date_2 = date(2)
          fac    = 1
        endif
      endif

C     Decompose the entries.
      if (date(4) .ne. -1) then
        yy = date_1/10000
        mm = mod(date_1/100,100)
        dd = mod(date_1,100)
      else
        yy = 0
        mm = 0
        dd = date_1
      endif
      ss = mod(date_2,100) +
     &     mod(date_2/100,100)*secondsPerMinute +
     &     date_2/10000*secondsPerHour

C     Include the sign.
      yy = fac*yy
      mm = fac*mm
      dd = fac*dd
      ss = fac*ss

      lp = date(3)
      wd = date(4)

      RETURN
      END
