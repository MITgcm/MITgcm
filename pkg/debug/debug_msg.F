C $Header: /u/gcmpack/MITgcm/pkg/debug/debug_msg.F,v 1.5 2011/08/24 02:45:36 jmc Exp $
C $Name:  $

#include "DEBUG_OPTIONS.h"

CBOP
C     !ROUTINE: DEBUG_MSG
C     !INTERFACE:
      SUBROUTINE DEBUG_MSG(
     I                text,
     I                myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DEBUG_MSG
C     | o Prints to STDOUT the text argument
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     text      :: test to write
C     myThid    :: my Thread Id number
      CHARACTER*(*) text
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf    :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER( myThid )
      WRITE(msgBuf,'(A11,A60)') 'DEBUG_MSG: ',text
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C-    can be usefull to flush the io unit (but not standard f77)
c     CALL FLUSH(standardMessageUnit)
      _END_MASTER( myThid )

      RETURN
      END
