C $Header: /u/gcmpack/MITgcm/pkg/debug/debug_call.F,v 1.1 2003/05/13 17:42:00 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE DEBUG_CALL(
     I                text,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE DEBUG_CALL                                    |
C     | o Prints to STDOUT the text argument after "CALLING S/R" |
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

      WRITE(msgBuf,'(A,A)') 'CALLING S/R ',text
      CALL DEBUG_MSG( msgBuf, myThid )

      RETURN
      END
