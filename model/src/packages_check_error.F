C $Header: /u/gcmpack/MITgcm/model/src/Attic/packages_check_error.F,v 1.3 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_CHECK_ERROR
C     !INTERFACE:
      SUBROUTINE PACKAGES_CHECK_ERROR( msg, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_CHECK_ERROR                           
C     | o Generate error message due to failed package            
C     |   dependancies check                                      
C     *==========================================================*
C     |   This routine is called if use_PKG was true but the CPP  
C     |   flows enabling the code were unset                      
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instances
      CHARACTER*(*) msg
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A,A,A)')
     &  'Run-time control flag use_',msg,' was used'
      CALL PRINT_ERROR( msgBuf , 1)
      WRITE(msgBuf,'(A,A,A)') 'when CPP flag ALLOW_',msg,' was unset'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R PACKAGE_CHECK_ERROR'

      return
      end
