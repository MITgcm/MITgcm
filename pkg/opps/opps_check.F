C $Header: /u/gcmpack/MITgcm/pkg/opps/opps_check.F,v 1.2 2004/09/23 12:02:54 mlosch Exp $
C $Name:  $
#include "OPPS_OPTIONS.h"

      SUBROUTINE OPPS_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE OPPS_CHECK                                     |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of OPPS_CHECK
      INTEGER myThid

#ifdef ALLOW_OPPS

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'OPPS_CHECK: #define ALLOW_OPPS'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     So far, OPPS works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'OPPS works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OPPS_CHECK'
      ENDIF

C     OPPS needs convection turned off (will be packaged later)
      IF (cAdjFreq.NE.0. .OR.
     &     ivdc_kappa.NE.0. ) THEN
         WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OPPS_CHECK'
      ENDIF

      IF ( useKPP ) THEN
         WRITE(msgBuf,'(A)')
     &      'OPPS and KPP cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OPPS_CHECK'
      ENDIF

      IF ( usePP81 ) THEN
         WRITE(msgBuf,'(A)') 
     &      'OPPS and PP81 cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OPPS_CHECK'
      ENDIF

      IF ( useMY82 ) THEN
         WRITE(msgBuf,'(A)') 
     &      'OPPS and MY82 cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OPPS_CHECK'
      ENDIF

      IF ( useGGL90 ) THEN
         WRITE(msgBuf,'(A)') 
     &      'OPPS and GGL90 cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OPPS_CHECK'
      ENDIF

#endif /* ALLOW_OPPS */

      return
      end
