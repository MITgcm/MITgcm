#include "ICEBERG_OPTIONS.h"

      SUBROUTINE ICEBERG_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE ICEBERG_CHECK                                |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE  

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"


C     === Routine arguments ===
C     myThid -  Number of this instance of ICEBERG_CHECK
      INTEGER myThid

#ifdef ALLOW_ICEBERG

C     === Local variables ===
C     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'ICEBERG_CHECK: #define ALLOW_ICEBERG'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C     So far, ICEBERG works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'ICEBERG works only with buoyancyRelation = OCEANIC'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R ICEBERG_CHECK'
      ENDIF

C     Only implemented so far with explicit drag
      IF ( selectImplicitDrag .NE. 0 ) THEN
         WRITE(msgBuf,'(A)')
     &      'ICEBERG works only with selectImplicitDrag = 0'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R ICEBERG_CHECK'
      ENDIF

C     If trying to use ptracers, ptracers has to be loaded too
      IF ( brg_useInputPtracers .EQ. 1 .AND. (.NOT. usePTRACERS)) THEN
         WRITE(msgBuf,'(A)')
     &      'PTRACERS must be enabled for brg_useInputPtracers = 1'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R ICEBERG_CHECK'
      ENDIF
     
      _END_MASTER(myThid)
#endif /* ALLOW_ICEBERG */

      RETURN
      END
