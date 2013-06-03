C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_timeinterval.F,v 1.7 2013/06/03 22:27:03 heimbach Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_TIMEINTERVAL(
     I                             timeint,
     I                             timeunit,
     O                             date,
     I                             myThid )

C     ==================================================================
C     SUBROUTINE cal_TimeInterval
C     ==================================================================
C
C     o Create an array in date format given a time interval measured in
C       units of timeunit.
C       Available time units: 'secs'
C                             'model'
C       Fractions of seconds are not resolved in this version.
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
C     SUBROUTINE cal_TimeInterval
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER date(4)
      _RL     timeint
      CHARACTER*(*) timeunit
      INTEGER myThid

C     == local variables ==
      INTEGER fac
      INTEGER nsecs
      INTEGER hhmmss
      INTEGER ierr
      _RL     tmp1, tmp2
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

      fac = 1
      if (timeint .lt. 0) fac = -1

      date(4) = -1
      date(3) =  0
      if (timeunit .eq. 'secs') then

        IF ( cal_setStatus .LT. 1 ) THEN
          WRITE( msgBuf,'(2A,F19.2,2A)')  'CAL_TIMEINTERVAL: ',
     &          'timeint=',timeint,' , timeunit=',timeunit
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE( msgBuf,'(2A,I2,A)') 'CAL_TIMEINTERVAL: ',
     &      'called too early (cal_setStatus=',cal_setStatus,' )'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R CAL_TIMEINTERVAL'
        ENDIF
        date(1) = int(timeint/float(secondsperday))
        tmp1    = date(1)
        tmp2    = secondsperday
        nsecs   = int(timeint - tmp1 * tmp2 )

      else if (timeunit .eq. 'model') then

        IF ( cal_setStatus .LT. 2 ) THEN
          WRITE( msgBuf,'(2A,F15.2,2A)')  'CAL_TIMEINTERVAL: ',
     &          'timeint=',timeint,' , timeunit=',timeunit
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE( msgBuf,'(2A,I2,A)') 'CAL_TIMEINTERVAL: ',
     &      'called too early (cal_setStatus=',cal_setStatus,' )'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R CAL_TIMEINTERVAL'
        ENDIF
        date(1) = int(timeint*modelstep/float(secondsperday))
        nsecs   = int(timeint*modelstep -
     &                float(date(1)) * float(secondsperday))

      else

        ierr = 701
        call cal_PrintError( ierr, myThid )
        stop ' stopped in cal_TimeInterval.'

      endif

      hhmmss  = nsecs/secondsperminute
      date(2) = hhmmss/minutesperhour*10000 +
     &          (mod(fac*hhmmss,minutesperhour)*100 +
     &           mod(fac*nsecs,secondsperminute))*fac

      RETURN
      END
