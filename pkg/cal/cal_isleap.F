C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_isleap.F,v 1.3 2012/04/07 16:21:05 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      INTEGER FUNCTION CAL_ISLEAP(
     I                             year,
     I                             myThid )

C     ==================================================================
C     FUNCTION cal_IsLeap
C     ==================================================================
C
C     o In case the Gregorian calendar is used determine whether the
C       given year is a leap year or not.
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
C     FUNCTION cal_IsLeap
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER year
      INTEGER myThid

C     == local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

      IF ( cal_setStatus .LT. 1 ) THEN
        WRITE( msgBuf,'(A,2(A,I9))')  'CAL_ISLEAP: ',
     &          'year=', year
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,I2,A)') 'CAL_ISLEAP: ',
     &    'called too early (cal_setStatus=',cal_setStatus,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: FUNCTION CAL_ISLEAP'
      ENDIF

      if ( usingGregorianCalendar ) then
        if ( mod(year,4) .ne. 0 ) then
          cal_IsLeap = 1
        else
          cal_IsLeap = 2
          if ( (mod(year,100) .eq. 0)  .and.
     &         (mod(year,400) .ne. 0) ) then
            cal_IsLeap = 1
          endif
        endif
      else if ( usingJulianCalendar ) then
        if ( mod(year,4) .ne. 0 ) then
          cal_IsLeap = 1
        else
          cal_IsLeap = 2
        endif
      else
       cal_IsLeap = 1
      endif

      RETURN
      END
