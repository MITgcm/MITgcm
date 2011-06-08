C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_check.F,v 1.4 2011/06/08 03:41:32 gforget Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_CHECK
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "SALT_PLUME.h"

C     !INPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'SALT_PLUME_CHECK: #define SALT_PLUME'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )

      IF ( (SPsalFRAC .LT. 0. _d 0).OR.(SPsalFRAC .GT. 1. _d 0) ) then
       WRITE(msgBuf,'(2A)') 'SALT_PLUME_CHECK:',
     &  ' SPsalFRAC needs to lie between 0. and 1.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SALT_PLUME_CHECK'
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
