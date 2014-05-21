C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_check.F,v 1.6 2014/05/21 10:46:03 heimbach Exp $
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

#ifdef SALT_PLUME_SPLIT_BASIN
      IF ( (SPsalFRAC(1) .LT. 0. _d 0).OR.
     &     (SPsalFRAC(1) .GT. 1. _d 0) ) then
       WRITE(msgBuf,'(2A)') 'SALT_PLUME_CHECK:',
     &  ' SPsalFRAC(1) needs to lie between 0. and 1.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SALT_PLUME_CHECK'
      ENDIF
      IF ( (SPsalFRAC(2) .LT. 0. _d 0).OR.
     &     (SPsalFRAC(2) .GT. 1. _d 0) ) then
       WRITE(msgBuf,'(2A)') 'SALT_PLUME_CHECK:',
     &  ' SPsalFRAC(2) needs to lie between 0. and 1.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SALT_PLUME_CHECK'
      ENDIF
#else
      IF ( (SPsalFRAC .LT. 0. _d 0).OR.(SPsalFRAC .GT. 1. _d 0) ) then
       WRITE(msgBuf,'(2A)') 'SALT_PLUME_CHECK:',
     &  ' SPsalFRAC needs to lie between 0. and 1.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SALT_PLUME_CHECK'
      ENDIF
#endif

#ifdef SALT_PLUME_IN_LEADS
      IF ( (SPinflectionPoint .LT. 0. _d 0).OR.
     &     (SPinflectionPoint .GT. 1. _d 0) ) then
       WRITE(msgBuf,'(2A)') 'SALT_PLUME_CHECK:',
     &  ' SPinflectionPoint needs to lie between 0. and 1.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SALT_PLUME_CHECK'
      ENDIF
#endif
#ifdef SALT_PLUME_VOLUME
      IF ( (SPbrineSconst .LT. 0. _d 0).OR.
     &     (SPbrineSconst .GT. SPbrineSaltmax) ) then
       WRITE(msgBuf,'(2A,F6.1,A)') 'SALT_PLUME_CHECK:',
     &  ' SPbrineSconst valid range: [0.,',SPbrineSaltmax,']'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SALT_PLUME_CHECK'
      ENDIF
#endif

      _END_MASTER(myThid)

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
