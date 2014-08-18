C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_init_fixed.F,v 1.13 2014/08/18 14:34:27 jmc Exp $
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
C     !FUNCTIONS
      INTEGER  GAD_ADVSCHEME_GET
      EXTERNAL GAD_ADVSCHEME_GET

C     !LOCAL VARIABLES:
C     iTracer     :: tracer index
C     errCount    :: error counter
C     tracMinSize :: overlap minimum size for ptracers advection
C     msgBuf      :: Informational/error message buffer
      INTEGER iTracer
      INTEGER errCount
      INTEGER tracMinSize, minSize
      LOGICAL updateMinSize
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER( myThid )
      errCount = 0

C     Initialise internal parameter in common block:
      DO iTracer = 1, PTRACERS_num
        PTRACERS_MultiDimAdv(iTracer)  = multiDimAdvection
        PTRACERS_SOM_Advection(iTracer)= .FALSE.
        PTRACERS_AdamsBashGtr(iTracer) = .FALSE.
        PTRACERS_AdamsBash_Tr(iTracer) = .FALSE.
      ENDDO

C--   Loop over tracers
      tracMinSize = 0
      DO iTracer = 1, PTRACERS_numInUse

C-    Check for valid advection-scheme number
        minSize = GAD_ADVSCHEME_GET( PTRACERS_advScheme(iTracer) )
        IF ( minSize.LT.0 ) THEN
          WRITE(msgBuf,'(2A,I6)') 'PTRACERS_INIT_FIXED: ',
     &    'invalid Adv. Scheme number=', PTRACERS_advScheme(iTracer)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I6)') 'PTRACERS_INIT_FIXED: ',
     &    'for tracer #', iTracer
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
C     Overlap minimum size consistent with ptracers advection
        tracMinSize = MAX( tracMinSize, minSize )

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
        IF ( .NOT.PTRACERS_doAB_onGpTr ) THEN
         PTRACERS_AdamsBash_Tr(iTracer) = PTRACERS_AdamsBashGtr(iTracer)
         PTRACERS_AdamsBashGtr(iTracer) = .FALSE.
        ENDIF

        PTRACERS_SOM_Advection(iTracer) =
     &    PTRACERS_advScheme(iTracer).GE.ENUM_SOM_PRATHER
     &    .AND. PTRACERS_advScheme(iTracer).LE.ENUM_SOM_LIMITER
#ifndef PTRACERS_ALLOW_DYN_STATE
        IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &   'trying to use 2nd.Order-Moment Advection without'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &   'dynamical internal state data structures compiled'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &   'Re-compile with:  #define PTRACERS_ALLOW_DYN_STATE'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
#endif /* ndef PTRACERS_ALLOW_DYN_STATE */

C--   end of Tracer loop
      ENDDO

C--   Update Overlap minimum size according to tracer advection
      updateMinSize = GAD_OlMinSize(1).LT.tracMinSize
      GAD_OlMinSize(1) = MAX( GAD_OlMinSize(1), tracMinSize )
C-    Constraint on size of the overlap (after updating "useMultiDimAdvec"):
      IF ( useCubedSphereExchange .AND. useMultiDimAdvec ) THEN
C-    multi-dim-advection on CS-grid requires to double the size of OLx,OLy
        updateMinSize = updateMinSize .OR. ( GAD_OlMinSize(3).LT.2 )
        GAD_OlMinSize(3) = MAX( GAD_OlMinSize(3), 2 )
      ENDIF
      IF ( updateMinSize ) THEN
        WRITE(msgBuf,'(2A,9I3)') 'PTRACERS_INIT_FIXED: ',
     &      'updated GAD_OlMinSize=', GAD_OlMinSize
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      ENDIF

#ifdef PTRACERS_ALLOW_DYN_STATE
      CALL PTRACERS_INIT_FIXED_DYNAMIC( PtrISt,
     &                                  PTRACERS_numInUse,
     &                                  PTRACERS_SOM_Advection,
     &                                  sNx, sNy, Nr, OLx, OLy,
     &                                  nSx, nSy, nSOM,
     &                                  myThid )
#endif

C--   Stop if any error was found:
      IF ( errCount .GE. 1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &  'S/R PTRACERS_INIT_FIXED: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R PTRACERS_INIT_FIXED'
      ENDIF

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
