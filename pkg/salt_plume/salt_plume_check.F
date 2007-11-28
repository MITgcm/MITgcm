C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_check.F,v 1.2 2007/11/28 17:55:36 dimitri Exp $
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
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

       WRITE(msgBuf,'(A)') 'SALT_PLUME_CHECK: #define SALT_PLUME'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
