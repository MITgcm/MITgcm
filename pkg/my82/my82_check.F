C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_check.F,v 1.2 2004/09/23 12:02:54 mlosch Exp $
C $Name:  $
#include "MY82_OPTIONS.h"

      SUBROUTINE MY82_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE MY82_CHECK                                     |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of MY82_CHECK
      INTEGER myThid

#ifdef ALLOW_MY82

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'MY82_CHECK: #define ALLOW_MY82'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     So far, MY82 works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'MY82 works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R MY82_CHECK'
      ENDIF

C     MY82 needs convection turned off (will be packaged later)
      IF (cAdjFreq.NE.0.  .OR.
     &     ivdc_kappa.NE.0. ) THEN
         WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R MY82_CHECK'
      ENDIF

      IF ( useKPP ) THEN
         WRITE(msgBuf,'(A)')
     &      'MY82 and KPP cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R MY82_CHECK'
      ENDIF

      IF ( usePP81 ) THEN
         WRITE(msgBuf,'(A)') 
     &      'MY82 and PP81 cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R MY82_CHECK'
      ENDIF

C     MY82 needs implicit vertical diffusion and viscosity
      IF (.NOT.implicitDiffusion ) THEN
         WRITE(msgBuf,'(A)') 
     &      'MY82 needs implicitDiffusion to be enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R MY82_CHECK'
      ENDIF
      IF (.NOT.implicitViscosity) THEN
         WRITE(msgBuf,'(A)') 
     &      'MY82 needs implicitViscosity to be enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R MY82_CHECK'
      ENDIF

#endif /* ALLOW_MY82 */

      return
      end
