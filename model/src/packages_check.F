C $Header: /u/gcmpack/MITgcm/model/src/packages_check.F,v 1.1 2000/06/21 19:25:06 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGE_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGE_CHECK                                 |
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
      IF (use_KPPmixing) THEN
       WRITE(msgBuf,'(A)') '#define ALLOW_KPP to use use_KPPmixing'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
      ENDIF
#else
      WRITE(msgBuf,'(A)') 'PACKAGE_CHECK: #define ALLOW_KPP'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
C     KPP needs convection turned off (will be packaged later)
      IF (use_KPPmixing) THEN
       IF (cAdjFreq.NE.0.  .OR.
     &     ivdc_kappa.NE.0.) THEN
        WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
       ENDIF
       IF (.NOT.implicitDiffusion) THEN
        WRITE(msgBuf,'(A)') 'KPP needs implicitDiffusion to be enabled'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
       ENDIF
      ENDIF
#endif

#ifndef ALLOW_GMREDI
      IF (use_GMRedi) THEN
       WRITE(msgBuf,'(A)') '#define ALLOW_GMREDI to use use_GMRedi'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
      ENDIF
#else
      WRITE(msgBuf,'(A)') 'PACKAGE_CHECK: #define GMREDI'
C     GM/Redi needs implicit diffusion (will be packaged later)
      IF (use_GMRedi) THEN
       IF (.NOT.implicitDiffusion) THEN
        WRITE(msgBuf,'(A)') 'GM/Redi needs implicitDiffusion=.true.'
        CALL PRINT_ERROR( msgBuf , 1)
cTemp   STOP 'ABNORMAL END: S/R PACKAGE_CHECK'
       ENDIF
      ENDIF
#endif


      return
      end
