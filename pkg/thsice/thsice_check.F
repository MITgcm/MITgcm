C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_check.F,v 1.7 2014/05/29 21:43:14 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

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
CEOP

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'THSICE_CHECK: #define THSICE'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

C--   Check parameter consistency:
      IF ( .NOT.( useBulkForce .OR. useEXF .OR.
     &            useAIM .OR. useCheapAML .OR. useATM2d ) ) THEN
C-    need some form of ATM-surface pkg to provide Air-Ice surf fluxes
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: needs some PKG (EXF,Bulk-Force,AIM,CheapAML)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: to provide Air-Ice surface fluxes for THSICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF
      IF ( thSIceAdvScheme.EQ.0 .AND. thSIce_diffK.NE.0. ) THEN
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: to use thSIce_diffK, needs to select',
     &   ' one advection scheme (thSIceAdvScheme<>0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF
#ifndef ALLOW_GENERIC_ADVDIFF
      IF ( thSIceAdvScheme.NE.0 ) THEN
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: Need to compile ',
     &     '"generic_advdiff" pkg in order to use thSIceAdvScheme'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF
#endif /* ndef ALLOW_GENERIC_ADVDIFF */

      IF ( thSIceBalanceAtmFW.NE.0 ) THEN
#ifndef ALLOW_BALANCE_FLUXES
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: to use thSIceBalanceAtmFW',
     &       'needs to compile with ALLOW_BALANCE_FLUXES defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
#endif
       IF ( balanceEmPmR ) THEN
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: cannot use',
     &       ' both balanceEmPmR and thSIceBalanceAtmFW'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
       ENDIF
      ENDIF

#ifdef ALLOW_AUTODIFF
      IF ( nitMaxTsf .GT. MaxTsf ) THEN
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: Need to set MaxTsf >= nitMaxTsf for TAF'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF
# ifdef ALLOW_DBUG_THSICE
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: cannot compile AD-code with ALLOW_DBUG_THSICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
# endif
#endif /* ALLOW_AUTODIFF */

#ifdef OLD_THSICE_CALL_SEQUENCE
        WRITE(msgBuf,'(2A)') '** WARNING ** THSICE_CHECK: ',
     &   'OLD_THSICE_CALL_SEQUENCE code no longer maintained'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** THSICE_CHECK: ',
     &   'option and related code will be removed after chkpt-64e'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
#endif /* OLD_THSICE_CALL_SEQUENCE */

      _END_MASTER(myThid)

      RETURN
      END
