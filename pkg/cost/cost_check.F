#include "COST_OPTIONS.h"

      SUBROUTINE COST_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE CTRK_CHECK                                    |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid  :: my Thread Id number
      INTEGER myThid

C     === Local variables ===
C     nRetired :: Counter used to trap gracefully "retired" option & parameters
C     msgBuf   :: Informational/error message buffer
      INTEGER nRetired
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)
      nRetired = 0

      WRITE(msgBuf,'(A)') 'COST_CHECK: #define ALLOW_COST'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_COST_TRANSPORT
        WRITE(msgBuf,'(2A)') 'COST_CHECK: ',
     &    'ALLOW_COST_TRANSPORT not allowed anymore in COST_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        nRetired = nRetired + 1
#endif
#ifdef ALLOW_COST_ATLANTIC
        WRITE(msgBuf,'(2A)') 'COST_CHECK: ',
     &    'ALLOW_COST_ATLANTIC not allowed anymore in COST_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        nRetired = nRetired + 1
#endif

      IF ( nRetired .GT. 0 ) THEN
        WRITE(msgBuf,'(2A)') 'S/R COST_CHECK: ',
     &   'retired compile-time options need to be undefined'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R COST_CHECK'
      ENDIF

      _END_MASTER(myThid)

      RETURN
      END
