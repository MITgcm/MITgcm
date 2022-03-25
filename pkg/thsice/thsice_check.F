#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_CHECK
C     !INTERFACE:

      SUBROUTINE THSICE_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_CHECK
C     | o Check parameters and other package dependences
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
CEOP

      _BEGIN_MASTER(myThid)
      errCount = 0

       WRITE(msgBuf,'(A)') 'THSICE_CHECK: #define THSICE'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

C--   Check parameter consistency:
      IF ( .NOT.( thSIce_skipThermo .OR. useBulkForce .OR. useEXF
     &       .OR. useAIM .OR. useCheapAML .OR. useATM2d ) ) THEN
C-    need some form of ATM-surface pkg to provide Air-Ice surf fluxes
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: needs some PKG (EXF,Bulk-Force,AIM,CheapAML)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: to provide Air-Ice surface fluxes for THSICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( thSIceAdvScheme.EQ.0 .AND. thSIce_diffK.NE.0. ) THEN
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: to use thSIce_diffK, needs to select',
     &   ' one advection scheme (thSIceAdvScheme<>0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#ifndef ALLOW_GENERIC_ADVDIFF
      IF ( thSIceAdvScheme.NE.0 ) THEN
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: Need to compile ',
     &     '"generic_advdiff" pkg in order to use thSIceAdvScheme'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef ALLOW_GENERIC_ADVDIFF */

      IF ( thSIceBalanceAtmFW.NE.0 ) THEN
#ifndef ALLOW_BALANCE_FLUXES
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: needs to have ',
     &     'ALLOW_BALANCE_FLUXES defined to use thSIceBalanceAtmFW'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif
       IF ( selectBalanceEmPmR.GE.1 ) THEN
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: cannot use',
     &       ' both selectBalanceEmPmR and thSIceBalanceAtmFW'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( nitMaxTsf .GT. MaxTsf ) THEN
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: Need to set MaxTsf >= nitMaxTsf for TAF'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_DBUG_THSICE
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: cannot compile AD-code with ALLOW_DBUG_THSICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
# endif
#endif /* ALLOW_AUTODIFF */

      IF ( useAIM .AND. .NOT.( stepFwd_oceMxL .OR. useCoupler )
     &            .AND.  tauRelax_MxL.NE. -1. _d 0 ) THEN
C-    with pkg/aim, usual way to use pkg/thsice is to step-forward Mixed-Layer
C     (stepFwd_oceMxL=T); otherwise, need to be sure SST are set, e.g. by AIM
C     when setting tauRelax_MxL=-1 ; add a warning if this is not the case
        WRITE(msgBuf,'(2A)') '** WARNING ** THSICE_CHECK: ',
     &   'SST are not stepped forward (stepFwd_oceMx=F)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** THSICE_CHECK: ',
     &   'nor set (from files) by pkg/aim (tauRelax_MxL<>-1)'
c    &   'nor copied from pkg/aim SST (tauRelax_MxL<>-1.)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'THSICE_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF

      _END_MASTER(myThid)

      RETURN
      END
