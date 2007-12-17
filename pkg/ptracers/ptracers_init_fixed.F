C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_init_fixed.F,v 1.3 2007/12/17 22:03:15 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PTRACERS_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE PTRACERS_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize PTRACERS constant

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_RESTART.h"
#include "GAD.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS

C     !LOCAL VARIABLES:
C     iTracer  :: tracer index
C     msgBuf      - Informational/error meesage buffer
      INTEGER iTracer
      INTEGER minOlSize
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER( myThid )

C     Initialise internal parameter in common block:
      DO iTracer = 1, PTRACERS_num
        PTRACERS_MultiDimAdv(iTracer) = multiDimAdvection
        PTRACERS_AdamsBashGtr(iTracer) = .FALSE.
        PTRACERS_startAB(iTracer) = nIter0 - PTRACERS_Iter0
      ENDDO

C     Loop over tracers
      DO iTracer = 1, PTRACERS_numInUse

        IF (
     &       PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_2ND .OR.
     &       PTRACERS_advScheme(iTracer).EQ.ENUM_UPWIND_3RD .OR.
     &       PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_4TH
     &     ) PTRACERS_MultiDimAdv(iTracer) = .FALSE.
        useMultiDimAdvec = useMultiDimAdvec
     &                .OR. PTRACERS_MultiDimAdv(iTracer)
        PTRACERS_AdamsBashGtr(iTracer) =
     &       PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_2ND .OR.
     &       PTRACERS_advScheme(iTracer).EQ.ENUM_UPWIND_3RD .OR.
     &       PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_4TH

C       end of Tracer loop
      ENDDO
      _END_MASTER( myThid )
      _BARRIER

C--   Check size of the overlap :
      IF ( useCubedSphereExchange .AND. useMultiDimAdvec ) THEN
C-    multi-dim-advection on CS-grid requires to double the size of Olx,Oly
        minOlSize = 2
        DO iTracer = 1, PTRACERS_numInUse
         IF ( PTRACERS_advScheme(iTracer).EQ.ENUM_FLUX_LIMIT .OR.
     &        PTRACERS_advScheme(iTracer).EQ.ENUM_DST3_FLUX_LIMIT .OR.
     &        PTRACERS_advScheme(iTracer).EQ.ENUM_DST3 ) minOlSize = 4
        ENDDO
        IF ( Olx.LT.minOlSize .OR. Oly.LT.minOlSize ) THEN
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &     'Multi-Dim Advection with 5-points stencil'
          CALL PRINT_ERROR( msgBuf , myThid)
          WRITE(msgBuf,'(2A,I2)') 'PTRACERS_INIT_FIXED: ',
     &     'advection scheme needs at least Olx,Oly=', minOlSize
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R PTRACERS_INIT_FIXED'
        ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_MNC
      IF (useMNC) THEN
C       Initialize the MNC variable types for PTRACERS
        CALL PTRACERS_MNC_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL PTRACERS_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

#endif /* ALLOW_PTRACERS */

      RETURN
      END
