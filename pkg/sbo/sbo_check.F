C $Header:

#include "SBO_OPTIONS.h"

      SUBROUTINE SBO_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE SBO_CHECK                                     |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid - Number of this instance of SBO_CHECK
      INTEGER myThid

#ifdef ALLOW_SBO

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'SBO_CHECK: #define ALLOW_SBO'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , myThid )

C     SBO assumes spherical polar coordinates
      IF ( .NOT. usingSphericalPolarGrid ) THEN
         WRITE(msgBuf,'(A)') 'SBO requires usingSphericalPolarGrid'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SBO_CHECK'
      ENDIF

#endif ALLOW_SBO

      return
      end
