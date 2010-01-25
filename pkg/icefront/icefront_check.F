C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_check.F,v 1.2 2010/01/25 22:37:19 dimitri Exp $
C $Name:  $
#include "ICEFRONT_OPTIONS.h"

      SUBROUTINE ICEFRONT_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE ICEFRONT_CHECK                                |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ICEFRONT.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEFRONT_CHECK
      INTEGER myThid

#ifdef ALLOW_ICEFRONT

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'ICEFRONT_CHECK: #define ALLOW_ICEFRONT'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C     So far, ICEFRONT works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'ICEFRONT works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
      ENDIF

CML#ifdef ALLOW_NONHYDROSTATIC
CML      IF ( nonHydrostatic ) THEN
CML         WRITE(msgBuf,'(A)')
CML     &      'Currently ICEFRONT and nonHydrostatic cannot be turned'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         WRITE(msgBuf,'(A)') 'on at the same time'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
CML      ENDIF
CML#endif /* ALLOW_NONHYDROSTATIC */

CDM#ifndef ALLOW_ISOMIP_TD
CDM      IF ( useISOMIPTD ) THEN
CDM       WRITE(msgBuf,'(A,A,A)')
CDM     &      'Run-time control flag useISOMIPTD was used'
CDM       CALL PRINT_ERROR( msgBuf, myThid )
CDM       WRITE(msgBuf,'(A,A,A)')
CDM     &      'when CPP flag ALLOW_ISOMIP_TD was unset'
CDM       CALL PRINT_ERROR( msgBuf, myThid )
CDM       STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
CDM      ENDIF
CDM#else
CDM      IF ( useISOMIPTD ) THEN
CDM       IF ( ICEFRONTconserve ) THEN
CDM        WRITE(msgBuf,'(A,A,A)')
CDM     &       'Run-time control flag ICEFRONTconserve=.TRUE. was used'
CDM        CALL PRINT_ERROR( msgBuf, myThid )
CDM        WRITE(msgBuf,'(A,A,A)')
CDM     &       'when useISOMIPTD=.TRUE.; this does not work!'
CDM        CALL PRINT_ERROR( msgBuf, myThid )
CDM        STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
CDM       ENDIF
CDM      ENDIF
CDM#endif /* ALLOW_ISOMIP_TD */

      _END_MASTER(myThid)

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
