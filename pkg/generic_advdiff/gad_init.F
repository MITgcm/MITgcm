C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/Attic/gad_init.F,v 1.5 2004/07/07 22:25:58 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_INIT
C     !INTERFACE:
      SUBROUTINE GAD_INIT( myThid )
C     !DESCRIPTION:
C     Routine to initialize Generic Advection/Diffusion variables and
C     constants.

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instance of GAD_INIT
      INTEGER myThid
CEOP

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Set Temp & Salt multi-Dim Advec. flag according to advection scheme used
      tempMultiDimAdvec = multiDimAdvection .AND. tempAdvection
      saltMultiDimAdvec = multiDimAdvection .AND. saltAdvection
      IF ( tempAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.tempAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.tempAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
           tempMultiDimAdvec = .FALSE. 
      ENDIF
      IF ( saltAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.saltAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.saltAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
           saltMultiDimAdvec = .FALSE. 
      ENDIF

C-- Set general multi-Dim Advec. flag when at least 1 tracer use multi-Dim Advec.
      useMultiDimAdvec = .FALSE.
      useMultiDimAdvec = useMultiDimAdvec.OR.tempMultiDimAdvec
      useMultiDimAdvec = useMultiDimAdvec.OR.saltMultiDimAdvec

C-- Set Temp & Salt Adams-Bashforth flag according to advection scheme used
      tempAdamsBashforth = .FALSE.
      saltAdamsBashforth = .FALSE.
      IF ( tempAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.tempAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.tempAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
           tempAdamsBashforth = .TRUE.
      ENDIF
      IF ( saltAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.saltAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.saltAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
           saltAdamsBashforth = .TRUE.
      ENDIF

#ifdef ALLOW_PASSIVE_TRACER
      tr1_MultiDimAdvec = multiDimAdvection
      tr1_AdamsBashforth = .FALSE.
      IF ( tracerAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.tracerAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.tracerAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
           tr1_MultiDimAdvec = .FALSE.
           tr1_AdamsBashforth = .TRUE.
      ENDIF
      useMultiDimAdvec = useMultiDimAdvec.OR.tr1_MultiDimAdvec
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Print out GAD parameters :
      _BARRIER
      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)')'// GAD parameters :'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL WRITE_0D_I( tempAdvScheme, INDEX_NONE,
     & 'tempAdvScheme =',
     & '   /* Temp. Horiz.Advection scheme selector */') 
      CALL WRITE_0D_I( tempVertAdvScheme, INDEX_NONE,
     & 'tempVertAdvScheme =',
     & '   /* Temp. Vert. Advection scheme selector */') 
      CALL WRITE_0D_L( tempMultiDimAdvec, INDEX_NONE,
     & 'tempMultiDimAdvec =',
     & '   /* use Muti-Dim Advec method for Temp */')
      CALL WRITE_0D_L( tempAdamsBashforth, INDEX_NONE,
     & 'tempAdamsBashforth =',
     & '   /* use Adams-Bashforth time-stepping for Temp */')

      CALL WRITE_0D_I( saltAdvScheme, INDEX_NONE,
     & 'saltAdvScheme =',
     & '   /* Salt. Horiz.advection scheme selector */') 
      CALL WRITE_0D_I( saltVertAdvScheme, INDEX_NONE,
     & 'saltVertAdvScheme =',
     & '   /* Salt. Vert. Advection scheme selector */') 
      CALL WRITE_0D_L( saltMultiDimAdvec, INDEX_NONE,
     & 'saltMultiDimAdvec =',
     & '   /* use Muti-Dim Advec method for Salt */')
      CALL WRITE_0D_L( saltAdamsBashforth, INDEX_NONE,
     & 'saltAdamsBashforth =',
     & '   /* use Adams-Bashforth time-stepping for Salt */')

      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
