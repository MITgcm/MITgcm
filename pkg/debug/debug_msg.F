C $Header: /u/gcmpack/MITgcm/pkg/debug/debug_msg.F,v 1.4 2004/12/14 16:54:08 edhill Exp $
C $Name:  $

#include "DEBUG_OPTIONS.h"

      SUBROUTINE DEBUG_MSG(
     I                text,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE DEBUG_MSG                                     |
C     | o Prints to STDOUT the text argument                     |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     === Routine arguments ===
      CHARACTER*(*) text
      INTEGER myThid

C     === Local variables ====
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER( myThid )
      WRITE(msgBuf,'(A11,A40)') 'DEBUG_MSG: ',text
      CALL PRINT_MESSAGE( msgBuf,
     &   standardMessageUnit, SQUEEZE_RIGHT, myThid )
      _END_MASTER( myThid )

      RETURN
      END
