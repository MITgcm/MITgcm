C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_init_fixed.F,v 1.2 2007/11/05 18:48:04 jmc Exp $
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

C     Loop over tracers
      DO iTracer = 1, PTRACERS_numInUse

        IF (
     &       multiDimAdvection .AND.
     &       PTRACERS_advScheme(iTracer).NE.ENUM_CENTERED_2ND .AND.
     &       PTRACERS_advScheme(iTracer).NE.ENUM_UPWIND_3RD .AND.
     &       PTRACERS_advScheme(iTracer).NE.ENUM_CENTERED_4TH ) THEN
c         PTRACERS_multiDimAdv(iTracer) = .TRUE.
          useMultiDimAdvec = .TRUE.
        ENDIF

C       end of Tracer loop
      ENDDO

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
