C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_subdates.F,v 1.3 2012/04/07 16:21:05 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_SUBDATES(
     I                         finaldate,
     I                         initialdate,
     O                         diffdate,
     I                         myThid )

C     ==================================================================
C     SUBROUTINE cal_SubDates
C     ==================================================================
C
C     o Subtract two dates. In case calendar dates are given finaldate
C       must be after initialdate.
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
C     SUBROUTINE cal_SubDates
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER finaldate(4)
      INTEGER initialdate(4)
      INTEGER diffdate(4)
      INTEGER myThid

C     == local variables ==
      INTEGER workdate(4)
      INTEGER ierr
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == end of interface ==

      IF ( cal_setStatus .LT. 1 ) THEN
        WRITE( msgBuf,'(2A,4I9)') 'CAL_SUBDATES: ', 'finaldate=',
     &    finaldate(1),finaldate(2),finaldate(3),finaldate(4)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,4I9)') 'CAL_SUBDATES: ', 'initialdate=',
     &    initialdate(1),initialdate(2),initialdate(3),initialdate(4)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,I2,A)') 'CAL_SUBDATES: ',
     &    'called too early (cal_setStatus=',cal_setStatus,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CAL_SUBDATES'
      ENDIF

      if ((initialdate(4) .gt. 0) .eqv.
     &    (  finaldate(4) .gt. 0)) then

        if (initialdate(4) .eq. -1) then
C         The time interval is subtracted.
          workdate(1) = -initialdate(1)
          workdate(2) = -initialdate(2)
          workdate(3) =  0
          workdate(4) = -1
          call cal_AddTime( finaldate, workdate, diffdate, myThid )
        else
C         The time interval between initial and final date is calculated.
          call cal_TimePassed(
     &          initialdate, finaldate, diffdate, myThid )
        endif
      else

        ierr = 801
        call cal_PrintError( ierr, myThid )
        stop ' stopped in cal_SubDates.'

      endif

      RETURN
      END
