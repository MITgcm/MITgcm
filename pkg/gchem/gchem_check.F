C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_check.F,v 1.1 2004/11/19 01:34:27 mlosch Exp $
C $Name:  $
#include "GCHEM_OPTIONS.h"

      SUBROUTINE GCHEM_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE GCHEM_CHECK                                   |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance 
      INTEGER myThid

#ifdef ALLOW_GCHEM

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'GCHEM_CHECK: #define ALLOW_GCHEM'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     GCHEM needs passive tracer package
      IF ( .NOT.usePTRACERS) THEN
         WRITE(msgBuf,'(A)') 'usePTRACERS has to be .TRUE. for GCHEM'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF

#endif /* ALLOW_GCHEM */

      return
      end
