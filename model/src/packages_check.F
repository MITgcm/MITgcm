C $Header: /u/gcmpack/MITgcm/model/src/packages_check.F,v 1.2 2000/09/11 20:42:57 heimbach Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGES_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGES_CHECK                                |
C     | o Check dependances between selected packages            |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf


#ifndef ALLOW_KPP
      IF (useKPP) THEN
       WRITE(msgBuf,'(A)') '#define ALLOW_KPP to use useKPP'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
      ENDIF
#else
      WRITE(msgBuf,'(A)') 'PACKAGE_CHECK: #define ALLOW_KPP'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
C     KPP needs convection turned off (will be packaged later)
      IF (useKPP) THEN
       IF (cAdjFreq.NE.0.  .OR.
     &     ivdc_kappa.NE.0.) THEN
        WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
       ENDIF
       IF (.NOT.implicitDiffusion ) THEN
        WRITE(msgBuf,'(A)') 'KPP needs implicitDiffusion to be enabled'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
       ENDIF
       IF (.NOT.implicitViscosity) THEN
        WRITE(msgBuf,'(A)') 'KPP needs implicitViscosity to be enabled'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
       ENDIF
      ENDIF
#endif

#ifndef ALLOW_GMREDI
      IF (useGMRedi) CALL PACKAGES_CHECK_ERROR('GMREDI')
#else
      IF (useGMRedi) CALL GMREDI_CHECK( myThid )
#endif

      return
      end
