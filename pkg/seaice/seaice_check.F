C $Header:

#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_CHECK                                  |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of SEAICE_CHECK
      INTEGER myThid

#ifdef ALLOW_SEAICE

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   ALLOW_SEAICE
      WRITE(msgBuf,'(A)') 'SEAICE_CHECK: #define ALLOW_SEAICE'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , myThid )

C--   SEAICE_ALLOW_DYNAMICS and SEAICEuseDYNAMICS
#ifndef SEAICE_ALLOW_DYNAMICS
      IF (SEAICEuseDYNAMICS) THEN
         WRITE(msgBuf,'(A)')
     &        'SEAICE_ALLOW_DYNAMICS needed for SEAICEuseDYNAMICS'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif SEAICE_ALLOW_DYNAMICS

C--   SEAICE_EXTERNAL_FORCING
C     At present this option is hardwired for pkg/exf.
#ifdef SEAICE_EXTERNAL_FORCING
#ifndef INCLUDE_EXTERNAL_FORCING_PACKAGE
         WRITE(msgBuf,'(A)')
     &        'need to define INCLUDE_EXTERNAL_FORCING_PACKAGE'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif INCLUDE_EXTERNAL_FORCING_PACKAGE
#ifndef ALLOW_BULKFORMULAE
         WRITE(msgBuf,'(A)')
     &        'need to define ALLOW_BULKFORMULAE'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif ALLOW_BULKFORMULAE
#ifndef ALLOW_ATM_TEMP
         WRITE(msgBuf,'(A)')
     &        'need to define ALLOW_ATM_TEMP'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif ALLOW_ATM_TEMP
#ifndef ALLOW_ATM_WIND
         WRITE(msgBuf,'(A)')
     &        'need to define ALLOW_ATM_WIND'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif ALLOW_ATM_WIND
#ifndef ALLOW_RUNOFF
         WRITE(msgBuf,'(A)')
     &        'need to define ALLOW_RUNOFF'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif ALLOW_RUNOFF
#endif SEAICE_EXTERNAL_FORCING

#endif ALLOW_SEAICE

      return
      end
