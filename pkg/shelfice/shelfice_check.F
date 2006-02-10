C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_check.F,v 1.2 2006/02/10 10:00:42 mlosch Exp $
C $Name:  $
#include "SHELFICE_OPTIONS.h"

      SUBROUTINE SHELFICE_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE SHELFICE_CHECK                                |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of SHELFICE_CHECK
      INTEGER myThid

#ifdef ALLOW_SHELFICE

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'SHELFICE_CHECK: #define ALLOW_SHELFICE'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     So far, SHELFICE works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'SHELFICE works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF

C     SHELFICE doesmay not work with many other packages
#ifdef ALLOW_NONHYDROSTATIC
      IF ( nonHydrostatic ) THEN
         WRITE(msgBuf,'(A)')
     &      'Currently SHELFICE and nonHydrostatic cannot be turned'
         CALL PRINT_ERROR( msgBuf , 1)
         WRITE(msgBuf,'(A)') 'on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

#ifdef ALLOW_KPP
CMLC     SHELFICE may not work with many other packages
CML      IF ( useKPP ) THEN
CML         WRITE(msgBuf,'(A)')
CML     &      'SHELFICE and KPP cannot be turned on at the same time'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
CML      ENDIF
#endif ALLOW_KPP

#ifndef ALLOW_ISOMIP_TD
      IF ( useISOMIPTD ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &      'Run-time control flag useISOMIPTD was used'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A,A)') 
     &      'when CPP flag ALLOW_ISOMIP_TD was unset'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF 
#endif /* ALLOW_ISOMIP_TD */

#endif /* ALLOW_SHELFICE */

      return
      end
