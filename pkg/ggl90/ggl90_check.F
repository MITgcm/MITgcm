C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_check.F,v 1.2 2004/09/23 12:02:54 mlosch Exp $
C $Name:  $
#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE GGL90_CHECK                                   |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of GGL90_CHECK
      INTEGER myThid

#ifdef ALLOW_GGL90

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'GGL90_CHECK: #define ALLOW_GGL90'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     So far, GGL90 works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'GGL90 works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GGL90_CHECK'
      ENDIF

C     GGL90 needs convection turned off (will be packaged later)
      IF (cAdjFreq.NE.0.  .OR.
     &     ivdc_kappa.NE.0. ) THEN
         WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GGL90_CHECK'
      ENDIF

      IF ( useKPP ) THEN
         WRITE(msgBuf,'(A)')
     &      'GGL90 and KPP cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GGL90_CHECK'
      ENDIF

      IF ( usePP81 ) THEN
         WRITE(msgBuf,'(A)') 
     &      'GGL90 and PP81 cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GGL90_CHECK'
      ENDIF

      IF ( useMY82 ) THEN
         WRITE(msgBuf,'(A)') 
     &      'GGL90 and MY82 cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GGL90_CHECK'
      ENDIF

C     GGL90 needs implicit vertical diffusion and viscosity
      IF (.NOT.implicitDiffusion ) THEN
         WRITE(msgBuf,'(A)') 
     &      'GGL90 needs implicitDiffusion to be enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GGL90_CHECK'
      ENDIF
      IF (.NOT.implicitViscosity) THEN
         WRITE(msgBuf,'(A)') 
     &      'GGL90 needs implicitViscosity to be enabled'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GGL90_CHECK'
      ENDIF

#endif /* ALLOW_GGL90 */

      return
      end
