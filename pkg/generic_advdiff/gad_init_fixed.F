C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_init_fixed.F,v 1.10 2014/08/18 14:29:29 jmc Exp $
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
C     myThid  :: My Thread Id. number
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  GAD_ADVSCHEME_GET
      EXTERNAL GAD_ADVSCHEME_GET

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf  :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER   errCode, n, minSize

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C-- Initialise advection scheme parameter
      CALL GAD_ADVSCHEME_INIT( myThid )

C-  Set advection scheme parameter (overlap minimum size) for each scheme:
      errCode = 0
      CALL GAD_ADVSCHEME_SET( ENUM_UPWIND_1RST   , 1, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_CENTERED_2ND  , 1, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_UPWIND_3RD    , 2, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_CENTERED_4TH  , 2, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_DST2          , 1, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_FLUX_LIMIT    , 2, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_DST3          , 2, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_DST3_FLUX_LIMIT,2, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_OS7MP         , 4, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_SOM_PRATHER   , 1, errCode, myThid )
      CALL GAD_ADVSCHEME_SET( ENUM_SOM_LIMITER   , 1, errCode, myThid )
      IF ( errCode.GT.0 ) THEN
        WRITE(msgBuf,'(A)')
     &     'GAD_INIT_FIXED: Invalid Advection-Scheme Number setting'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_INIT_FIXED'
      ENDIF

C-  Initialise overlap minimum size for GAD pkg:
      GAD_OlMinSize(1) = 0
      GAD_OlMinSize(2) = 0
      GAD_OlMinSize(3) = 1

C-  Set SOM I/O suffix (used for pickup, diagnostics ...)
      DO n=1,nSOM
        somSfx(n) = '  '
        IF (n.EQ.1) somSfx(n) = '_x'
        IF (n.EQ.2) somSfx(n) = '_y'
        IF (n.EQ.3) somSfx(n) = '_z'
        IF (n.EQ.4) somSfx(n) = 'xx'
        IF (n.EQ.5) somSfx(n) = 'yy'
        IF (n.EQ.6) somSfx(n) = 'zz'
        IF (n.EQ.7) somSfx(n) = 'xy'
        IF (n.EQ.8) somSfx(n) = 'xz'
        IF (n.EQ.9) somSfx(n) = 'yz'
      ENDDO

C-- Check that Temp & Salt have valid advection scheme number:
      n = GAD_ADVSCHEME_GET( tempAdvScheme )
      IF ( n.LT.0 ) THEN
        WRITE(msgBuf,'(2A,I6)') 'GAD_INIT_FIXED: ',
     &   'invalid Temp. advection scheme number=', tempAdvScheme
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_INIT_FIXED'
      ENDIF
      n = GAD_ADVSCHEME_GET( tempVertAdvScheme )
      IF ( n.LT.0 ) THEN
        WRITE(msgBuf,'(2A,I6)') 'GAD_INIT_FIXED: ',
     &   'invalid Temp. Vert. Adv.scheme number=', tempVertAdvScheme
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_INIT_FIXED'
      ENDIF
      n = GAD_ADVSCHEME_GET( saltAdvScheme )
      IF ( n.LT.0 ) THEN
        WRITE(msgBuf,'(2A,I6)') 'GAD_INIT_FIXED: ',
     &   'invalid Salt. advection scheme number=', saltAdvScheme
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_INIT_FIXED'
      ENDIF
      n = GAD_ADVSCHEME_GET( saltVertAdvScheme )
      IF ( n.LT.0 ) THEN
        WRITE(msgBuf,'(2A,I6)') 'GAD_INIT_FIXED: ',
     &   'invalid Salt. Vert. Adv.scheme number=', saltVertAdvScheme
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_INIT_FIXED'
      ENDIF

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
      IF ( .NOT.doAB_onGtGs ) THEN
        AdamsBashforth_T = AdamsBashforthGt
        AdamsBashforth_S = AdamsBashforthGs
        AdamsBashforthGt = .FALSE.
        AdamsBashforthGs = .FALSE.
      ENDIF

#ifdef GAD_SMOLARKIEWICZ_HACK
      SmolarkiewiczMaxFrac = 1. _d 0
#endif

C-- Set Overlap minimum size according to Temp & Salt advection
      IF ( tempAdvection ) THEN
         minSize = GAD_ADVSCHEME_GET( tempAdvScheme )
         GAD_OlMinSize(1) = MAX( GAD_OlMinSize(1), minSize )
      ENDIF
      IF ( saltAdvection ) THEN
         minSize = GAD_ADVSCHEME_GET( saltAdvScheme )
         GAD_OlMinSize(1) = MAX( GAD_OlMinSize(1), minSize )
      ENDIF
      IF ( useCubedSphereExchange .AND. useMultiDimAdvec ) THEN
C-    multi-dim-advection on CS-grid requires to double the size of OLx,OLy
        GAD_OlMinSize(3) = MAX( GAD_OlMinSize(3), 2 )
      ENDIF
      WRITE(msgBuf,'(A,9I3)')
     &      'GAD_INIT_FIXED: GAD_OlMinSize=', GAD_OlMinSize
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

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
#ifdef GAD_SMOLARKIEWICZ_HACK
      CALL WRITE_0D_RL( SmolarkiewiczMaxFrac, INDEX_NONE,
     & 'SmolarkiewiczMaxFrac =',
     & ' /* maximal fraction of tracer to flow out of a cell */')
#endif

      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
