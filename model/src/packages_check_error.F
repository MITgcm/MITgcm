C $Header: /u/gcmpack/MITgcm/model/src/Attic/packages_check_error.F,v 1.1 2000/09/11 20:42:57 heimbach Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGES_CHECK_ERROR( msg, myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGES_CHECK_ERROR                          |
C     | o Generate error message due to failed package           |
C     |   dependancies check                                     |
C     |==========================================================|
C     |   This routine is called if use_PKG was true but the CPP |
C     |   flows enabling the code were unset                     |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      CHARACTER*(*) msg
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A,A,A)')
     &  'Run-time control flag use_',msg,' was used'
      CALL PRINT_ERROR( msgBuf , 1)
      WRITE(msgBuf,'(A,A,A)') 'when CPP flag ALLOW_',msg,' was unset'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R PACKAGE_CHECK_ERROR'

      return
      end
