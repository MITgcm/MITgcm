C $Header: /u/gcmpack/MITgcm/pkg/kl10/kl10_check.F,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#include "KL10_OPTIONS.h"

      SUBROUTINE KL10_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE KL10_CHECK                                    |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of KL10_CHECK
      INTEGER myThid

#ifdef ALLOW_KL10

C     === Local variables ===
C     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'KL10_CHECK: #define ALLOW_KL10'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     So far, KL10 works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'KL10 works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R KL10_CHECK'
      ENDIF

C     KL10 needs convection turned off (will be packaged later)
      IF (cAdjFreq.NE.0.  .OR.
     &     ivdc_kappa.NE.0. ) THEN
         WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R KL10_CHECK'
      ENDIF

C TODO: JMK: Check if this is really necessary.  I don't see why they can't
C co-exist
C      IF ( useKPP ) THEN
C         WRITE(msgBuf,'(A)')
C     &      'KL10 and KPP cannot be turned on at the same time'
C         CALL PRINT_ERROR( msgBuf , 1)
C         STOP 'ABNORMAL END: S/R KL10_CHECK'
C      ENDIF

C     KL10 needs implicit vertical diffusion and viscosity
      IF (.NOT.implicitDiffusion ) THEN
         WRITE(msgBuf,'(A)')
     &      'KL10 needs implicitDiffusion to be enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R KL10_CHECK'
      ENDIF
      IF (.NOT.implicitViscosity) THEN
         WRITE(msgBuf,'(A)')
     &      'KL10 needs implicitViscosity to be enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R KL10_CHECK'
      ENDIF

#endif /* ALLOW_KL10 */

      return
      end
