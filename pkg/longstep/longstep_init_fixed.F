C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_init_fixed.F,v 1.2 2010/01/12 23:55:48 jahn Exp $
C $Name:  $

#include "LONGSTEP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE LONGSTEP_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize longstep constant

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LONGSTEP_PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_LONGSTEP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     whether to average EmPmR or PmEpR in LS_fwFlux
#ifdef EXACT_CONSERV
      IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &     .AND. useRealFreshWaterFlux ) THEN
        LS_usePmEpR = .TRUE.
C     can only do LS_staggerTimeStep if staggerTimeStep
        IF ( staggerTimeStep .AND. LS_whenToSample.LT.2 ) THEN
          WRITE(msgBuf,'(2A)')
     &       ' LONGSTEP: staggerTimeStep with EXACT_CONSERV,',
     &       ' useRealFreshWaterFlux'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A)')
     &       ' LONGSTEP: and nonlinFreeSurf or PCoords',
     &       ' requires LS_whenToSample=2'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LONGSTEP_INIT_FIXED'
        ENDIF
      ELSE
#else /* EXACT_CONSERV */
      IF (.TRUE.) THEN
#endif /* EXACT_CONSERV */
        LS_usePmEpR = .FALSE.
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL LONGSTEP_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
