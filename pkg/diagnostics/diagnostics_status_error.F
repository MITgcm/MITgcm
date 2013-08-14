C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_status_error.F,v 1.1 2013/08/14 00:54:45 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_STATUS_ERROR

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_STATUS_ERROR (
     I                       callerSubR, errMsg, diagName,
     I                       expectStatus, myThid )

C     !DESCRIPTION:
C     Routine to print the appropriate error message when one of the public
C     diagnostics interface S/R (e.g., DIAGNOSTICS_ADDTOLIST or one of the
C     DIAGNOSTICS_[]_FILL S/R) is called at the wrong place in the sequence
C     of calls (initialisation stages or time-stepping part).

C     !USES:
       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C- needed to get "useFizhi":
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     callerSubR   :: name of subroutine which is calling this S/R
C     errMsg       :: additional error message to print
C     diagName     :: diagnostic name (if relevant for this call)
C     expectStatus :: expected pkg-status when this S/R is called
C     myThid       :: my Thread Id number
      CHARACTER*(*) callerSubR
      CHARACTER*(*) errMsg
      CHARACTER*8   diagName
      INTEGER expectStatus
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Initialise

      _BEGIN_MASTER( myThid)

C--   Check if this S/R is called from the right place
      WRITE(msgBuf,'(4A)') '*** DIAGNOSTICS_STATUS_ERROR ***',
     &                     ' from: ', callerSubR, ' call'
      CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      IF ( diagName.EQ.blkName ) THEN
        WRITE(msgBuf,'(1A,2(A,I3),2A)') callerSubR,
     &     ': expectStatus=', expectStatus,
     &     ', pkgStatus=',  diag_pkgStatus, ' : ', errMsg
        CALL PRINT_ERROR( msgBuf, myThid )
      ELSE
        WRITE(msgBuf,'(3A,2(A,I3))') callerSubR,
     &     ': diagName="', diagName, '", expectStatus=',
     &     expectStatus, ', pkgStatus=', diag_pkgStatus
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

      IF ( diag_pkgStatus.EQ.-1 ) THEN
        WRITE(msgBuf,'(4A)') callerSubR,
     &    ': cannot be used if useDiagnostics=FALSE (data.pkg)'
        CALL PRINT_ERROR( msgBuf, myThid )
        IF ( .NOT.useFizhi )
     &  STOP 'ABNORMAL END: S/R DIAGNOSTICS_STATUS_ERROR'
      ELSEIF ( diag_pkgStatus.GT.expectStatus ) THEN
C--   case pkgStatus > expectStatus
        WRITE(msgBuf,'(3A)') callerSubR,
     &    ': <== called from the WRONG place, i.e.'
        CALL PRINT_ERROR( msgBuf, myThid )
        IF ( expectStatus.EQ.1 ) THEN
         WRITE(msgBuf,'(3A)') callerSubR, ': after ',
     &    'DIAGNOSTICS_INIT_EARLY call in PACKAGES_INIT_FIXED'
        ELSEIF ( expectStatus.EQ.2 ) THEN
         WRITE(msgBuf,'(3A)') callerSubR, ': after ',
     &    'DIAGNOSTICS_INIT_FIXED call in PACKAGES_INIT_FIXED'
        ELSEIF ( expectStatus.EQ.3 ) THEN
         WRITE(msgBuf,'(3A)') callerSubR, ': after ',
     &    'DIAGNOSTICS_INIT_VARIA call in PACKAGES_INIT_VARIABLES'
        ELSEIF ( expectStatus.EQ.10 ) THEN
         WRITE(msgBuf,'(3A)') callerSubR, ': after ',
     &    'DIAGNOSTICS_SWITCH_ONOFF call in FORWARD_STEP'
        ELSE
         WRITE(msgBuf,'(3A)') callerSubR, ': after ',
     &    'the last DIAGNOSTICS_WRITE call in DO_THE_MODEL_IO'
        ENDIF
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R DIAGNOSTICS_STATUS_ERROR'
      ELSEIF ( diag_pkgStatus.GE.1 ) THEN
C--   case pkgStatus < expectStatus
        WRITE(msgBuf,'(2A)') callerSubR,
     &    ': <== called from the WRONG place, i.e.'
        CALL PRINT_ERROR( msgBuf, myThid )
        IF ( expectStatus.EQ.2 ) THEN
         WRITE(msgBuf,'(3A)') callerSubR, ': before ',
     &    'DIAGNOSTICS_INIT_EARLY call in PACKAGES_INIT_FIXED'
        ELSEIF ( expectStatus.EQ.3 ) THEN
         WRITE(msgBuf,'(3A)') callerSubR, ': before ',
     &    'DIAGNOSTICS_INIT_FIXED call in PACKAGES_INIT_FIXED'
        ELSEIF ( expectStatus.EQ.10 ) THEN
         WRITE(msgBuf,'(3A)') callerSubR, ': before ',
     &    'DIAGNOSTICS_INIT_VARIA call in PACKAGES_INIT_VARIABLES'
        ELSEIF ( expectStatus.EQ.20 ) THEN
         WRITE(msgBuf,'(3A)') callerSubR, ': before ',
     &    'DIAGNOSTICS_SWITCH_ONOFF call in FORWARD_STEP'
        ELSE
         WRITE(msgBuf,'(3A)') callerSubR, ': before ',
     &    'the last DIAGNOSTICS_WRITE call in DO_THE_MODEL_IO'
        ENDIF
        CALL PRINT_ERROR( msgBuf, myThid )
        IF ( .NOT.useFizhi )
     &  STOP 'ABNORMAL END: S/R DIAGNOSTICS_STATUS_ERROR'
      ELSE
C--   case pkgStatus < 1 (most likely: pkgStatus=0 )
        WRITE(msgBuf,'(4A)') callerSubR,
     &        ': called but nothing set in pkg/diagnostics'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R DIAGNOSTICS_STATUS_ERROR'
      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
