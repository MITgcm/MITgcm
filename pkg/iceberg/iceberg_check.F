C $Header: /u/gcmpack/MITgcm/pkg/iceberg/iceberg_check.F,v 1.6 2013/11/10 02:58:34 yunx Exp $
C $Name: checkpoint65m $
#include "ICEBERG_OPTIONS.h"

      SUBROUTINE ICEBERG_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE ICEBERG_CHECK                                |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE  

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"


C     === Routine arguments ===
C     myThid -  Number of this instance of ICEBERG_CHECK
      INTEGER myThid

#ifdef ALLOW_ICEBERG

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'ICEBERG_CHECK: #define ALLOW_ICEBERG'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C     So far, ICEBERG works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'ICEBERG works only with buoyancyRelation = OCEANIC'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R ICEBERG_CHECK'
      ENDIF

C     Only implimented so far with explicit drag
      IF ( selectImplicitDrag .NE. 0 ) THEN
         WRITE(msgBuf,'(A)')
     &      'ICEBERG works only with selectImplicitDrag = 0'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R ICEBERG_CHECK'
      ENDIF

C     So far, ICEBERG is not compatible with nonlinear surface, 
C     specifically the blocking effect is incompatible
#ifdef NONLIN_FRSURF    
C      WRITE(msgBuf,'(A)')
C     &  'ICEBERG works only with #undef NONLIN_FRSURF in CPP_OPTIONS'
C         CALL PRINT_ERROR( msgBuf, myThid )
C         STOP 'ABNORMAL END: S/R ICEBERG_CHECK'
#endif 
     


CML#ifdef ALLOW_NONHYDROSTATIC
CML      IF ( nonHydrostatic ) THEN
CML         WRITE(msgBuf,'(A)')
CML     &      'Currently ICEBERG and nonHydrostatic cannot be turned'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         WRITE(msgBuf,'(A)') 'on at the same time'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         STOP 'ABNORMAL END: S/R ICEBERG_CHECK'
CML      ENDIF
CML#endif /* ALLOW_NONHYDROSTATIC */

      _END_MASTER(myThid)

#endif /* ALLOW_ICEBERG */

      RETURN
      END
