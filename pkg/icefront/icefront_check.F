C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_check.F,v 1.1 2010/01/20 23:33:45 dimitri Exp $
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

C     ICEFRONT may not work with many other packages, 
C     e.g. vertical mixing schemes, in particular KPP will not work properly,
C     as KPP requires surface fluxes at the surface, whereas icefront will
C     provide interfacial fluxes at some depth. Richardson flux number based 
C     schemes such as Packanowski-Philander (PP81) should be no problem.
CML#ifdef ALLOW_KPP
CML      IF ( useKPP ) THEN
CML         WRITE(msgBuf,'(A)')
CML     &      'ICEFRONT and KPP cannot be turned on at the same time'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
CML      ENDIF
CML#endif ALLOW_KPP

#ifndef ALLOW_ISOMIP_TD
      IF ( useISOMIPTD ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &      'Run-time control flag useISOMIPTD was used'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A,A)')
     &      'when CPP flag ALLOW_ISOMIP_TD was unset'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
      ENDIF
#else
      IF ( useISOMIPTD ) THEN
       IF ( ICEFRONTconserve ) THEN
        WRITE(msgBuf,'(A,A,A)')
     &       'Run-time control flag ICEFRONTconserve=.TRUE. was used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,A,A)')
     &       'when useISOMIPTD=.TRUE.; this does not work!'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R ICEFRONT_CHECK'
       ENDIF
      ENDIF
#endif /* ALLOW_ISOMIP_TD */

      _END_MASTER(myThid)

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
