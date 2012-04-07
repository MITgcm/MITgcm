C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_getdate.F,v 1.4 2012/04/07 15:53:14 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_GETDATE(
     I                        myIter,
     I                        myTime,
     O                        mydate,
     I                        myThid )

C     ==================================================================
C     SUBROUTINE cal_GetDate
C     ==================================================================
C
C     o Determine the current date given the iteration number and/or the
C       current time of integration.
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
C     SUBROUTINE cal_GetDate
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER myIter
      _RL     myTime
      INTEGER mydate(4)
      INTEGER myThid

C     == local variables ==
      _RL     secs
      INTEGER workdate(4)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

      IF ( myIter .EQ. -1 ) THEN

C-    Special case to return starDate_1 & _2 :
        mydate(1) = startdate_1
        mydate(2) = startdate_2
        mydate(3) = 1
        mydate(4) = 1

      ELSEIF ( cal_setStatus .LT. 3 ) THEN

        WRITE( msgBuf,'(2A,I10,AF19.2)') 'CAL_GETDATE: ',
     &       'myIter=', myIter, ' , myTime=', myTime
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE( msgBuf,'(2A,I2,A)') 'CAL_GETDATE: ',
     &    'called too early (cal_setStatus=',cal_setStatus,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CAL_GETDATE'

      ELSEIF ( myIter.EQ.modeliter0 .OR. myTime.EQ.modelstart ) THEN

C-    faster to just copy modelstartdate:
        mydate(1) = modelstartdate(1)
        mydate(2) = modelstartdate(2)
        mydate(3) = modelstartdate(3)
        mydate(4) = modelstartdate(4)

      ELSE

        if (myTime .lt. 0) then
          if (myIter .ge. 0) then
            secs = float(myIter - modeliter0)*modelstep
          else
            print*,' cal_GetDate: Not a valid input!'
            STOP 'ABNORMAL END: S/R CAL_GETDATE'
          endif
        else
          secs = myTime - modelstart
        endif

        call cal_TimeInterval( secs, 'secs', workdate, myThid )
        call cal_AddTime( modelstartdate, workdate, mydate, myThid )

      ENDIF

      RETURN
      END
