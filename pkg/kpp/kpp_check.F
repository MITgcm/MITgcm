#include "KPP_OPTIONS.h"

      SUBROUTINE KPP_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE KPP_CHECK                                     |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of KPP_INIT
      INTEGER myThid

#ifdef ALLOW_KPP

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'KPP_CHECK: #define ALLOW_KPP'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     KPP needs convection turned off (will be packaged later)
      IF (cAdjFreq.NE.0.  .OR.
     &     ivdc_kappa.NE.0.) THEN
         WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF

C     KPP needs implicit vertical diffusion and viscosity
      IF (.NOT.implicitDiffusion ) THEN
         WRITE(msgBuf,'(A)') 'KPP needs implicitDiffusion to be enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF
      IF (.NOT.implicitViscosity) THEN
         WRITE(msgBuf,'(A)') 'KPP needs implicitViscosity to be enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF

C     Shortwave heating should be defined with KPP
#ifndef SHORTWAVE_HEATING
      WRITE(msgBuf,'(A)') 'KPP needs CPP flag SHORTWAVE_HEATING enabled'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R KPP_CHECK'
#endif /* SHORTWAVE_HEATING */

#endif /* ALLOW_KPP */

      return
      end
