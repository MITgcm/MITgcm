C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_check.F,v 1.4 2012/08/01 18:20:55 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      SUBROUTINE THSICE_CHECK( myThid )

c     ==================================================================
c     SUBROUTINE THSICE_CHECK
c     ==================================================================
c
      IMPLICIT NONE

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"

c     == routine arguments ==

c     myThid :: my Thread Id number

      INTEGER myThid

c     == local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'THSICE_CHECK: #define THSICE'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )

C--   Check parameter consistency:
      IF ( thSIceAdvScheme.EQ.0 .AND. thSIce_diffK.NE.0. ) THEN
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: to use thSIce_diffK, needs to select',
     &   ' one advection scheme (thSIceAdvScheme<>0)'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF
#ifndef ALLOW_GENERIC_ADVDIFF
      IF ( thSIceAdvScheme.NE.0 ) THEN
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: Need to compile ',
     &     '"generic_advdiff" pkg in order to use thSIceAdvScheme'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF
#endif /* ndef ALLOW_GENERIC_ADVDIFF */

      IF ( thSIceBalanceAtmFW.NE.0 ) THEN
#ifndef ALLOW_BALANCE_FLUXES
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: to use thSIceBalanceAtmFW',
     &       'needs to compile with ALLOW_BALANCE_FLUXES defined'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
#endif
       IF ( balanceEmPmR ) THEN
        WRITE(msgBuf,'(2A)') 'THSICE_CHECK: cannot use',
     &       ' both balanceEmPmR and thSIceBalanceAtmFW'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
       ENDIF
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( nitMaxTsf .GT. MaxTsf ) THEN
        WRITE(msgBuf,'(2A)')
     &   'THSICE_CHECK: Need to set MaxTsf >= nitMaxTsf for TAF'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF
#endif

      _END_MASTER(myThid)

      RETURN
      END
