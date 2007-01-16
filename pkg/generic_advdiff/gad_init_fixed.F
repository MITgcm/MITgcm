C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_init_fixed.F,v 1.2 2007/01/16 04:38:34 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE GAD_INIT_FIXED( myThid )
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
C     myThid -  Number of this instance of GAD_INIT_FIXED
      INTEGER myThid
CEOP

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER   minOlSize

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C-- Set Temp & Salt 2nd-Order Moment Advec. flag according to advection scheme
      tempSOM_Advection = tempAdvScheme.GE.ENUM_SOM_PRATHER
     &              .AND. tempAdvScheme.LE.ENUM_SOM_LIMITER
      tempSOM_Advection = tempSOM_Advection .AND. tempAdvection
      saltSOM_Advection = saltAdvScheme.GE.ENUM_SOM_PRATHER
     &              .AND. saltAdvScheme.LE.ENUM_SOM_LIMITER
      saltSOM_Advection = saltSOM_Advection .AND. saltAdvection

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
      useMultiDimAdvec = useMultiDimAdvec.OR.tempMultiDimAdvec
      useMultiDimAdvec = useMultiDimAdvec.OR.saltMultiDimAdvec

C-- Set Temp & Salt Adams-Bashforth flag according to advection scheme used
      AdamsBashforthGt = .FALSE.
      AdamsBashforthGs = .FALSE.
      AdamsBashforth_T = .FALSE.
      AdamsBashforth_S = .FALSE.
      IF ( tempAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.tempAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.tempAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
           AdamsBashforthGt = tempStepping
      ENDIF
      IF ( saltAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.saltAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.saltAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
           AdamsBashforthGs = saltStepping
      ENDIF
#ifdef ALLOW_ADAMSBASHFORTH_3
C-    For now, A-B on T,S is only implemented in AB-3 code, and not working
C     with synchronous time-step, which would require to do also AB(u,v,w)
      IF ( .NOT.doAB_onGtGs .AND.
     &     (staggerTimeStep .OR. implicitIntGravWave) ) THEN
        AdamsBashforth_T = AdamsBashforthGt
        AdamsBashforth_S = AdamsBashforthGs
        AdamsBashforthGt = .FALSE.
        AdamsBashforthGs = .FALSE.
      ENDIF
#endif /* ALLOW_ADAMSBASHFORTH_3 */

C-     Initialize AB starting flags for T & S:
C      notes: those could be modified when reading a pickup that does
C             correspond to what is actually needed.
       tempStartAB = nIter0.EQ.0
       saltStartAB = nIter0.EQ.0

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
C--   Add diagnostics of Temp & Salt fluxes to the (long) list of diagnostics:
        CALL GAD_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Print out GAD parameters :
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
      CALL WRITE_0D_L( tempSOM_Advection, INDEX_NONE,
     & 'tempSOM_Advection =',
     & ' /* use 2nd Order Moment Advection for Temp */')
      CALL WRITE_0D_L( AdamsBashforthGt, INDEX_NONE,
     & 'AdamsBashforthGt =',
     & ' /* apply Adams-Bashforth extrapolation on Gt */')
      CALL WRITE_0D_L( AdamsBashforth_T, INDEX_NONE,
     & 'AdamsBashforth_T =',
     & ' /* apply Adams-Bashforth extrapolation on Temp */')

      CALL WRITE_0D_I( saltAdvScheme, INDEX_NONE,
     & 'saltAdvScheme =',
     & '   /* Salt. Horiz.advection scheme selector */')
      CALL WRITE_0D_I( saltVertAdvScheme, INDEX_NONE,
     & 'saltVertAdvScheme =',
     & '   /* Salt. Vert. Advection scheme selector */')
      CALL WRITE_0D_L( saltMultiDimAdvec, INDEX_NONE,
     & 'saltMultiDimAdvec =',
     & '   /* use Muti-Dim Advec method for Salt */')
      CALL WRITE_0D_L( saltSOM_Advection, INDEX_NONE,
     & 'saltSOM_Advection =',
     & ' /* use 2nd Order Moment Advection for Salt */')
      CALL WRITE_0D_L( AdamsBashforthGs, INDEX_NONE,
     & 'AdamsBashforthGs =',
     & ' /* apply Adams-Bashforth extrapolation on Gs */')
      CALL WRITE_0D_L( AdamsBashforth_S, INDEX_NONE,
     & 'AdamsBashforth_S =',
     & ' /* apply Adams-Bashforth extrapolation on Salt */')

      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Check size of the overlap :
      IF ( useCubedSphereExchange .AND. useMultiDimAdvec ) THEN
C-    multi-dim-advection on CS-grid requires to double the size of Olx,Oly
        minOlSize = 2
        IF ( tempMultiDimAdvec ) THEN
          IF ( tempAdvScheme.EQ.ENUM_FLUX_LIMIT .OR.
     &         tempAdvScheme.EQ.ENUM_DST3_FLUX_LIMIT .OR.
     &         tempAdvScheme.EQ.ENUM_DST3 ) minOlSize = 4
        ENDIF
        IF ( saltMultiDimAdvec ) THEN
          IF ( saltAdvScheme.EQ.ENUM_FLUX_LIMIT .OR.
     &         saltAdvScheme.EQ.ENUM_DST3_FLUX_LIMIT .OR.
     &         saltAdvScheme.EQ.ENUM_DST3 ) minOlSize = 4
        ENDIF
        IF ( Olx.LT.minOlSize .OR. Oly.LT.minOlSize ) THEN
          WRITE(msgBuf,'(A)')
     &     'GAD_INIT_FIXED: Multi-Dim Advection with 5-points stencil'
          CALL PRINT_ERROR( msgBuf , myThid)
          WRITE(msgBuf,'(A,I2)')
     &     'GAD_INIT_FIXED: advection scheme needs at least Olx,Oly=',
     &      minOlSize
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R GAD_INIT_FIXED'
        ENDIF
      ENDIF

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
