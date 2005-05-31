C $Header: /u/gcmpack/MITgcm/pkg/runclock/runclock_check.F,v 1.1 2005/05/31 18:24:33 adcroft Exp $
C $Name:  $

#include "RUNCLOCK_OPTIONS.h"

      SUBROUTINE RUNCLOCK_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE RUNCLOCK_CHECK                                  |
C     | o Check dependances with other packages                  |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "RUNCLOCK.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf


#ifdef ALLOW_RUNCLOCK

       WRITE(msgBuf,'(A)') 'RUNCLOCK_CHECK: #define RUNCLOCK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


C--  Check parameters:

      _BEGIN_MASTER(myThid)


      _END_MASTER(myThid)

#endif /* ALLOW_RUNCLOCK */
      RETURN
      END
