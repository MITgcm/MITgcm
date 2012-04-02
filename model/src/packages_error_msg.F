C $Header: /u/gcmpack/MITgcm/model/src/packages_error_msg.F,v 1.2 2012/04/02 21:31:06 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_ERROR_MSG
C     !INTERFACE:
      SUBROUTINE PACKAGES_ERROR_MSG( msg1, msg2, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_ERROR_MSG
C     | o Generate error message due to failed package
C     |   dependancies check
C     *==========================================================*
C     |   This routine is called if use_PKG was true but PKG code
C     |   was not compiled (#undef ALLOW_KPG)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     msg1   ::  package name
C     msg2   ::  package On/Off flag
C     myThid ::  My thread Id number
      CHARACTER*(*) msg1, msg2
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_MBUF) msgLwc,msgUpc
      INTEGER iLen
CEOP

      iLen = LEN(msg1)
      msgLwc(1:iLen) = msg1
      msgUpc(1:iLen) = msg1
      CALL LCASE(msgLwc(1:iLen))
      CALL UCASE(msgUpc(1:iLen))
      IF ( msg2 .EQ. ' ' ) THEN
       WRITE(msgBuf,'(4A)') 'from PACKAGES_CHECK: ',
     &  'run-time control flag use',msg1,' is set'
      ELSE
       WRITE(msgBuf,'(4A)') 'from PACKAGES_CHECK: ',
     &  'run-time parameter ',msg2,' is used'
      ENDIF
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(5A)') 'but pkg/',msgLwc(1:iLen),
     &  ' was not compiled (ALLOW_',msgUpc(1:iLen),' undef).'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(3A)')
     &  '==> Re-compile with pkg "',msgLwc(1:iLen),
     &  '" in file "packages.conf"'
      CALL PRINT_ERROR( msgBuf, myThid )
      CALL ALL_PROC_DIE( myThid )
      STOP 'ABNORMAL END: S/R PACKAGE_ERROR_MSG'

      RETURN
      END
