#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE GGL90_CHECK                                   |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GGL90.h"

C     === Routine arguments ===
C     myThid   :: Number of this instance of GGL90_CHECK
      INTEGER myThid

#ifdef ALLOW_GGL90

C     === Local variables ===
C     errCount :: error counter
C     msgBuf   :: Informational/error message buffer
      INTEGER errCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)
      errCount = 0

      WRITE(msgBuf,'(A)') 'GGL90_CHECK: #define ALLOW_GGL90'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C     Convective adjustment might be needed even with ggl90
      IF ( cAdjFreq.NE.0. .OR. ivdc_kappa.NE.0. ) THEN
         WRITE(msgBuf,'(A)')
     &  'GGL90_CHECK: Some form of convection has been enabled'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
      ENDIF

#ifdef ALLOW_GGL90_HORIZDIFF
#ifdef ALLOW_GGL90_SMOOTH
      IF ( GGL90diffTKEh .NE. 0. _d 0 ) THEN
         WRITE(msgBuf,'(A)')
     &      'Horizontal diffusion of TKE and smoothing are both on'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF
#endif
#else
      IF ( GGL90diffTKEh .NE. 0. _d 0 ) THEN
         WRITE(msgBuf,'(2A)')
     &      'Define ALLOW_GGL90_HORIZDIFF in GGL90_OPTIONS.h',
     &      ' to get diffusion of TKE'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF
#endif
#ifndef ALLOW_GGL90_LANGMUIR
      IF (useLANGMUIR ) THEN
         WRITE(msgBuf,'(2A)')
     &      'Define ALLOW_GGL90_LANGMUIR in GGL90_OPTIONS.h',
     &      ' to use Langmuir Parameterization'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF
#endif

C     GGL90 needs implicit vertical diffusion and viscosity
      IF (.NOT.implicitDiffusion ) THEN
         WRITE(msgBuf,'(A)')
     &      'GGL90 needs implicitDiffusion to be enabled'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF
      IF (.NOT.implicitViscosity) THEN
         WRITE(msgBuf,'(A)')
     &      'GGL90 needs implicitViscosity to be enabled'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

C     Limitation of Langmuir Parameterization implementation:
      IF ( useLANGMUIR .AND. useAbsVorticity ) THEN
c        WRITE(msgBuf,'(2A)') '** WARNING ** GGL90_CHECK: ',
         WRITE(msgBuf,'(2A)') 'GGL90_CHECK: ',
     &      'with useAbsVorticity, missing Coriolis'
         CALL PRINT_ERROR( msgBuf, myThid )
c        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
c    &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A)') 'GGL90_CHECK: ',
     &      ' contrib from Langmuir Parameterization'
         CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( useLANGMUIR .AND. useCDscheme ) THEN
         WRITE(msgBuf,'(2A)') 'GGL90_CHECK: ',
     &      'with useCDscheme, missing Coriolis'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A)') 'GGL90_CHECK: ',
     &      ' contrib from Langmuir Parameterization'
         CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C     Compatibility with the use of other pkgs:
      IF ( useKPP ) THEN
         WRITE(msgBuf,'(A)')
     &      'GGL90 and KPP cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

      IF ( usePP81 ) THEN
         WRITE(msgBuf,'(A)')
     &      'GGL90 and PP81 cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

      IF ( useMY82 ) THEN
         WRITE(msgBuf,'(A)')
     &      'GGL90 and MY82 cannot be turned on at the same time'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

C-    Stop cleanly when error was detected
      IF ( errCount.GE.1 ) THEN
         WRITE(msgBuf,'(A,I3,A)')
     &       'GGL90_CHECK: detected', errCount,' fatal error(s)'
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R GGL90_CHECK'
      ENDIF
      _END_MASTER(myThid)

#endif /* ALLOW_GGL90 */

      RETURN
      END
