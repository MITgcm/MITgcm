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
#include "SEAICE_PARAMS.h"
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
      LOGICAL             EXFwindOnBgrid
      COMMON /EXF_PARM_L/ EXFwindOnBgrid
#endif

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
#endif

C--   SEAICE_EXTERNAL_FORCING
C     At present this option is hardwired for pkg/exf.
#ifdef SEAICE_EXTERNAL_FORCING
#ifndef INCLUDE_EXTERNAL_FORCING_PACKAGE
         WRITE(msgBuf,'(A)')
     &        'need to define INCLUDE_EXTERNAL_FORCING_PACKAGE'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#else /* INCLUDE_EXTERNAL_FORCING_PACKAGE */
#ifndef ALLOW_ATM_TEMP
         WRITE(msgBuf,'(A)')
     &        'need to define pkg/exf ALLOW_ATM_TEMP'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#ifndef ALLOW_ATM_WIND
         WRITE(msgBuf,'(A)')
     &        'need to define pkg/exf ALLOW_ATM_WIND'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#ifndef ALLOW_DOWNWARD_RADIATION
         WRITE(msgBuf,'(A)')
     &        'need to define pkg/exf ALLOW_DOWNWARD_RADIATION'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#if !defined(EXF_READ_EVAP) && !defined(ALLOW_BULKFORMULAE)
         WRITE(msgBuf,'(A)')
     &     'need to set EXF_READ_EVAP or ALLOW_BULKFORMULAE in pkg/exf '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
         IF(.NOT.SEAICEwindOnCgrid .AND. .NOT.EXFwindOnBgrid) THEN
         WRITE(msgBuf,'(A)')
     &        'please set either SEAICEwindOnCgrid or EXFwindOnBgrid '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
         ENDIF
         IF(SEAICEwindOnCgrid .AND. EXFwindOnBgrid) THEN
         WRITE(msgBuf,'(A)')
     &        'please unset either SEAICEwindOnCgrid or EXFwindOnBgrid '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
         ENDIF
#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */
#endif /* SEAICE_EXTERNAL_FORCING */

C--   SEAICE_EXTERNAL_FLUXES
C     At present this option is hardwired for pkg/exf.
#ifdef SEAICE_EXTERNAL_FLUXES
#ifndef INCLUDE_EXTERNAL_FORCING_PACKAGE
         WRITE(msgBuf,'(A)')
     &        'need to define INCLUDE_EXTERNAL_FORCING_PACKAGE'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#else /* INCLUDE_EXTERNAL_FORCING_PACKAGE */
         IF(.NOT.SEAICEwindOnCgrid .AND. .NOT.EXFwindOnBgrid) THEN
         WRITE(msgBuf,'(A)')
     &        'please set either SEAICEwindOnCgrid or EXFwindOnBgrid '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
         ENDIF
         IF(SEAICEwindOnCgrid .AND. EXFwindOnBgrid) THEN
         WRITE(msgBuf,'(A)')
     &        'please unset either SEAICEwindOnCgrid or EXFwindOnBgrid '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
         ENDIF
#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */
#endif /* SEAICE_EXTERNAL_FLUXES */

#endif /* ALLOW_SEAICE */

      return
      end
