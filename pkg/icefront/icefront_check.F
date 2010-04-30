C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_check.F,v 1.5 2010/04/30 22:10:39 yunx Exp $
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

#ifdef ALLOW_SUBGLACIAL_RUNOFF
#ifndef ALLOW_ADDFLUID
C--  If there is subglacial runoff, addMass (in main code) should be 
C    turned on by setting #define ALLOW_ADDFLUID in 'CPP_OPTIONS.h'
      WRITE(msgBuf,'(A,A)')
     &       'SUBGLACIAL_RUNOFF requires ALLOW_ADDFLUID turned on ',
     &       'in CPP_OPTIONS.h'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
#endif
      IF (SGrunoffFile .NE. ' ') THEN
C--   If there is subglacial runoff, addMass (in main code) should
C     be turned on by setting selectAddFluid = 1;
       IF ( selectAddFluid .NE. 1 ) THEN
        WRITE(msgBuf,'(A)')
     &       'SUBGLACIAL_RUNOFF requires selectAddFluid = 1'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
       ENDIF
C--   Issue a warning if temp_addMass or salt_addMass are not set.
       IF ( temp_addMass .EQ. UNSET_RL ) THEN
        WRITE(msgBuf,'(A)')
     &       'temp_addMass has not been set; defaults to local value'
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF ( salt_addMass .EQ. UNSET_RL ) THEN
        WRITE(msgBuf,'(A)')
     &       'salt_addMass has not been set; defaults to local value'
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
      ENDIF
#endif /* ALLOW_SUBGLACIAL_RUNOFF*/

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

      _END_MASTER(myThid)

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
