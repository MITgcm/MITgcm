#include "DEBUG_OPTIONS.h"

      SUBROUTINE DEBUG_ENTER(
     I                text,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE DEBUG_ENTER                                   |
C     | o Prints to STDOUT the text argument after "ENTERED S/R" |
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

      WRITE(msgBuf,'(A,A)') 'ENTERED S/R ',text
      CALL DEBUG_MSG( msgBuf, myThid )

      RETURN
      END
