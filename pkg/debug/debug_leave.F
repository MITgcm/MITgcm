C $Header: /u/gcmpack/MITgcm/pkg/debug/debug_leave.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "DEBUG_OPTIONS.h"

      SUBROUTINE DEBUG_LEAVE(
     I                text,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE DEBUG_LEAVE                                   |
C     | o Prints to STDOUT the text argument after "LEAVING S/R" |
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

      WRITE(msgBuf,'(A,A)') 'LEAVING S/R ',text
      CALL DEBUG_MSG( msgBuf, myThid )

      RETURN
      END
