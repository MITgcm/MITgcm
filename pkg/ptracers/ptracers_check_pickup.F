C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_check_pickup.F,v 1.1 2007/12/17 22:05:48 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PTRACERS_CHECK_PICKUP
C     !INTERFACE:
      SUBROUTINE PTRACERS_CHECK_PICKUP(
     I                 missFldList,
     I                 nMissing, nbFields,
     I                 myIter, myThid )

C     !DESCRIPTION:
C     Check that fields that are needed to restart have been read.
C     In case some fields are missing, stop if pickupStrictlyMatch=T
C     or try, if possible, to restart without the missing field.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_RESTART.h"

C     !INPUT/OUTPUT PARAMETERS:
C     missFldList :: List of missing fields   (attempted to read but not found)
C     nMissing    :: Number of missing fields (attempted to read but not found)
C     nbFields    :: number of fields in pickup file (read from meta file)
C     myIter      :: Iteration number
C     myThid      :: my Thread Id. number
      CHARACTER*(8) missFldList(*)
      INTEGER nMissing
      INTEGER nbFields
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER i, iTracer
      INTEGER j
      INTEGER ioUnit
      INTEGER warnCnts
      LOGICAL stopFlag
      CHARACTER*(8) fldName
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      ioUnit = errorMessageUnit

      _BEGIN_MASTER( myThid )

       IF ( nMissing.GE.1 ) THEN
        stopFlag = .FALSE.
        warnCnts = nMissing
        DO j=1,nMissing
         fldName = missFldList(j)
         iTracer = 1
C-    passive tracer field is always needed:
         IF     ( fldName(1:3).EQ.'pTr' .AND.
     &            fldName(6:8).EQ.'   ' ) THEN
C     find the corresponding pTracer:
           iTracer = 0
           DO i=1,PTRACERS_numInUse
             IF ( iTracer.EQ.0 .AND.
     &            fldName(4:5).EQ.PTRACERS_ioLabel(i) ) iTracer = i
           ENDDO
           IF ( iTracer.GT.0 ) THEN
             stopFlag = .TRUE.
             WRITE(msgBuf,'(2A,I4,3A)') 'PTRACERS_CHECK_PICKUP: ',
     &       'cannot restart without tracer ',iTracer,
     &       ' field "',fldName,'"'
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF

C-    fields with alternative in place to restart without:
C-    (but get a non-perfect restart)
         ELSEIF ( fldName(1:4).EQ.'gPtr' .AND.
     &            fldName(7:8).EQ.'m1' ) THEN
C     find the corresponding pTracer:
           iTracer = 0
           DO i=1,PTRACERS_numInUse
             IF ( iTracer.EQ.0 .AND.
     &            fldName(5:6).EQ.PTRACERS_ioLabel(i) ) iTracer = i
           ENDDO
           IF ( iTracer.GT.0 ) THEN
             PTRACERS_startAB(iTracer) = 0
             WRITE(msgBuf,'(2A,I4)')
     &        '** WARNINGS ** PTRACERS_CHECK_PICKUP: ',
     &        'tracer Tendency is missing for pTr# :',iTracer
             CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
           ENDIF
           IF ( iTracer.GT.0 .AND.
     &          .NOT.pickupStrictlyMatch .AND. .NOT.stopFlag ) THEN
             WRITE(msgBuf,'(4A)')
     &        '** WARNINGS ** PTRACERS_CHECK_PICKUP: ',
     &        '1rst time-step will use simple Euler time-stepping'
             CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
           ENDIF

         ELSE
C-    unrecognized field:
           iTracer = 0
         ENDIF

C-    unrecognized field or tracer:
         IF ( iTracer.EQ.0 ) THEN
           stopFlag = .TRUE.
           WRITE(msgBuf,'(4A)') 'PTRACERS_CHECK_PICKUP: ',
     &       'missing field "',missFldList(j),'" not recognized'
           CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
        ENDDO

        IF ( stopFlag ) THEN
         STOP 'ABNORMAL END: S/R PTRACERS_CHECK_PICKUP'
        ELSEIF ( pickupStrictlyMatch ) THEN
         WRITE(msgBuf,'(4A)') 'PTRACERS_CHECK_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R PTRACERS_CHECK_PICKUP'
        ELSEIF ( warnCnts .GT. 0 ) THEN
         WRITE(msgBuf,'(4A)') '** WARNINGS ** PTRACERS_CHECK_PICKUP: ',
     &     'Will get only an approximated Restart'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF

       ENDIF

      _END_MASTER( myThid )

      RETURN
      END
