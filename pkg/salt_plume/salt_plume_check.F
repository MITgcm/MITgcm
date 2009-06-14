C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_check.F,v 1.3 2009/06/14 03:59:47 jmc Exp $
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

      _END_MASTER(myThid)

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
