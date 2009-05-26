C $Header: /u/gcmpack/MITgcm/eesupp/src/stop_if_error.F,v 1.2 2009/05/26 22:56:32 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: STOP_IF_ERROR

C     !INTERFACE:
      SUBROUTINE STOP_IF_ERROR( errFlag, errMsg, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE STOP_IF_ERROR
C     | o stop every Processes if flag is true
C     *==========================================================*
C     | Presently, gathering of error signal involves a
C     | global_sum which could degrade performance if called too
C     | many times. A potentially faster method (not implemented):
C     | only the proc(s) in error send a non-blocking error signal
C     | to everybody; however, this requires to check for error
C     | signal reception before doing any communication.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     errFlag :: stop if this logical flag is true
C     errMsg  :: error message to print in case it stops
C     myThid  :: my Thread Id number
      LOGICAL errFlag
      CHARACTER*(*) errMsg
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Local variables ==
C     msgBuf       :: I/O Buffer
C     errCount     :: error counter
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount

C--   Collect error from all Threads and Procs
      errCount = 0
      IF ( errFlag ) THEN
        errCount = 1
      ENDIF
      CALL GLOBAL_SUM_INT( errCount, myThid )

      IF ( errCount.GE.1 ) THEN
C--   Print message
        IF ( errFlag ) CALL PRINT_ERROR( errMsg, myThid )
        WRITE(msgBuf,'(A,I5,A)')
     &   'occurs', errCount, ' time(s) among all Threads and Procs'
        CALL PRINT_ERROR( msgBuf, myThid )
C--   Finishes
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R STOP_IF_ERROR'
      ENDIF

      RETURN
      END
