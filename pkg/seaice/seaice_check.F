C $Header:

#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_CHECK                                  |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of SEAICE_CHECK
      INTEGER myThid

#ifdef ALLOW_SEAICE

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'SEAICE_CHECK: #define ALLOW_SEAICE'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , myThid )

#endif ALLOW_SEAICE

      return
      end
