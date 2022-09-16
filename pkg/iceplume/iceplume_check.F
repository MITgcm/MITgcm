
#include "ICEPLUME_OPTIONS.h"

      SUBROUTINE ICEPLUME_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE ICEPLUME_CHECK                                |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ICEPLUME.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEPLUME_CHECK
      INTEGER myThid

#ifdef ALLOW_ICEPLUME

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'ICEPLUME_CHECK: #define ALLOW_ICEPLUME'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C    INSERT OTHER CHECKS HERE AS NECESSARY

       IF ( selectAddFluid .NE. 1 ) THEN
       WRITE(msgBuf,'(2A)')
     &  'Cannot use ICEPLUME if selectAddFluid not equal to 1'
                CALL PRINT_ERROR( msgBuf, 1 )
                STOP 'ABNORMAL END: S/R ICEPLUME_CHECK'
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_ICEPLUME */

      RETURN
      END
