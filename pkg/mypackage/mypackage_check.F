C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_check.F,v 1.2 2006/08/07 21:13:52 edhill Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_CHECK

C !INTERFACE:
      SUBROUTINE MYPACKAGE_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf


#ifdef ALLOW_MYPACKAGE

       WRITE(msgBuf,'(A)') 'MYPACKAGE_CHECK: #define MYPACKAGE'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--  Check parameters:

      _BEGIN_MASTER(myThid)

C-     Put stops here if some flags are wrongly chosen.
C-     For example, require this package to use tempStepping
       IF (.NOT.tempStepping) THEN
        WRITE(msgBuf,'(A)') 'An example requirement is:'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)') 'mypackage needs tempStepping=.true.'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R MYPACKAGE_CHECK'
       ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_MYPACKAGE */
      RETURN
      END
