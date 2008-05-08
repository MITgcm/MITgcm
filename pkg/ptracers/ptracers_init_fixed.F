C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_init_fixed.F,v 1.4 2008/05/08 19:50:09 jahn Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#include "GAD_OPTIONS.h"

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
        PTRACERS_SOM_Advection(iTracer) =
     &    PTRACERS_advScheme(iTracer).GE.ENUM_SOM_PRATHER
     &    .AND. PTRACERS_advScheme(iTracer).LE.ENUM_SOM_LIMITER
#ifndef GAD_ALLOW_SOM_ADVECT
        IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &   'trying to use 2nd.Order-Moment Advection that is not compiled'
          WRITE(msgBuf,'(2A)') 'PTRACERS_INIT_FIXED: ',
     &   'Re-compile with:  #define GAD_ALLOW_SOM_ADVECT'
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R PTRACERS_INIT_FIXED'
        ENDIF
#endif /* ndef GAD_ALLOW_SOM_ADVECT */
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

        WRITE(msgBuf,'(2A,I3,A,L)') 'PTRACERS_INIT_FIXED: ',
     &     'PTRACERS_SOM_Advection(', iTracer, ') = ',
     &     PTRACERS_SOM_Advection(iTracer)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid)

        PTRACERS_MultiDimAdv(iTracer) = multiDimAdvection
        PTRACERS_AdamsBashGtr(iTracer) = .FALSE.
        PTRACERS_startAB(iTracer) = nIter0 - PTRACERS_Iter0
      ENDDO

#ifdef PTRACERS_ALLOW_DYN_STATE
      CALL PTRACERS_INIT_FIXED_DYNAMIC( PtrISt, 
     &                                  PTRACERS_numInUse,
     &                                  PTRACERS_SOM_Advection,
     &                                  sNx, sNy, Nr, OLx, OLy,
     &                                  nSx, nSy, nSOM,
     &                                  myThid )
#endif

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
