C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_init_fixed.F,v 1.10 2012/04/18 22:05:18 jmc Exp $
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
#include "PTRACERS_MOD.h"
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
C     msgBuf   :: Informational/error message buffer
      INTEGER iTracer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER( myThid )

C     Initialise internal parameter in common block:
      DO iTracer = 1, PTRACERS_num
        PTRACERS_MultiDimAdv(iTracer)  = multiDimAdvection
        PTRACERS_SOM_Advection(iTracer)= .FALSE.
        PTRACERS_AdamsBashGtr(iTracer) = .FALSE.
      ENDDO

C--   Loop over tracers
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

        PTRACERS_SOM_Advection(iTracer) =
     &    PTRACERS_advScheme(iTracer).GE.ENUM_SOM_PRATHER
     &    .AND. PTRACERS_advScheme(iTracer).LE.ENUM_SOM_LIMITER
#ifndef PTRACERS_ALLOW_DYN_STATE
        IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &   'trying to use 2nd.Order-Moment Advection without'
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &   'dynamical internal state data structures compiled'
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &   'Re-compile with:  #define PTRACERS_ALLOW_DYN_STATE'
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R PTRACERS_INIT_FIXED'
        ENDIF
#endif /* ndef PTRACERS_ALLOW_DYN_STATE */

C--   end of Tracer loop
      ENDDO

#ifdef PTRACERS_ALLOW_DYN_STATE
      CALL PTRACERS_INIT_FIXED_DYNAMIC( PtrISt,
     &                                  PTRACERS_numInUse,
     &                                  PTRACERS_SOM_Advection,
     &                                  sNx, sNy, Nr, OLx, OLy,
     &                                  nSx, nSy, nSOM,
     &                                  myThid )
#endif

      _END_MASTER( myThid )
      _BARRIER

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
