C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_check.F,v 1.5 2007/10/10 09:26:39 mlosch Exp $
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
#include "SHELFICE.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of SHELFICE_CHECK
      INTEGER myThid

#ifdef ALLOW_SHELFICE

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'SHELFICE_CHECK: #define ALLOW_SHELFICE'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C     So far, SHELFICE works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'SHELFICE works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF

CML#ifdef ALLOW_NONHYDROSTATIC
CML      IF ( nonHydrostatic ) THEN
CML         WRITE(msgBuf,'(A)')
CML     &      'Currently SHELFICE and nonHydrostatic cannot be turned'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         WRITE(msgBuf,'(A)') 'on at the same time'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
CML      ENDIF
CML#endif /* ALLOW_NONHYDROSTATIC */

C     SHELFICE may not work with many other packages, 
C     e.g. vertical mixing schemes, in particular KPP will not work properly,
C     as KPP requires surface fluxes at the surface, whereas shelfice will
C     provide interfacial fluxes at some depth. Richardson flux number based 
C     schemes such as Packanowski-Philander (PP81) should be no problem.
CML#ifdef ALLOW_KPP
CML      IF ( useKPP ) THEN
CML         WRITE(msgBuf,'(A)')
CML     &      'SHELFICE and KPP cannot be turned on at the same time'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
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
       STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF
#else
      IF ( useISOMIPTD ) THEN
       IF ( SHELFICEconserve ) THEN
        WRITE(msgBuf,'(A,A,A)')
     &       'Run-time control flag SHELFICEconserve=.TRUE. was used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,A,A)')
     &       'when useISOMIPTD=.TRUE.; this does not work!'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
       ENDIF
      ENDIF
#endif /* ALLOW_ISOMIP_TD */

      _END_MASTER(myThid)

#endif /* ALLOW_SHELFICE */

      RETURN
      END
