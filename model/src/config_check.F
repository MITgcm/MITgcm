C $Header: /u/gcmpack/MITgcm/model/src/config_check.F,v 1.11 2003/10/28 22:57:59 edhill Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CONFIG_CHECK
C     !INTERFACE:
      SUBROUTINE CONFIG_CHECK( myThid )
C     !DESCRIPTION: \bv
C     *=========================================================*
C     | SUBROUTINE CONFIG_CHECK
C     | o Check model parameter settings.
C     *=========================================================*
C     | This routine help to prevent the use of parameters
C     | that are not compatible with the model configuration.
C     *=========================================================*
C     \ev                                                           

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instances of CONFIG_CHECK
      INTEGER myThid
CEndOfInterface

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C-  check that CPP option is "defined" when running-flag parameter is on:

#ifndef ALLOW_CD_CODE
      IF (useCDscheme) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #undef ALLOW_CD_CODE and'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: useCDscheme is TRUE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_NONHYDROSTATIC
      IF (nonHydrostatic) THEN 
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #undef ALLOW_NONHYDROSTATIC and'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonHydrostatic is TRUE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef EXACT_CONSERV
      IF (exactConserv) THEN 
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #undef EXACT_CONSERV and'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: exactConserv is TRUE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef NONLIN_FRSURF
      IF (nonlinFreeSurf.NE.0) THEN 
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #undef NONLIN_FRSURF and'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonlinFreeSurf is non-zero'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef NONLIN_FRSURF
      IF (select_rStar .NE. 0) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: rStar is part of NonLin-FS '
        CALL PRINT_ERROR( msgBuf, myThid)                      
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: ==> use #define NONLIN_FRSURF to use it'
        CALL PRINT_ERROR( msgBuf, myThid)                      
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif /* NONLIN_FRSURF */

#ifdef USE_NATURAL_BCS
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: USE_NATURAL_BCS option has been replaced'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: by useRealFreshWaterFlux=TRUE in data file'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
#endif

C     o If pLoadFile is set, then we should make sure the corresponing
C       code is being compiled
#ifndef ATMOSPHERIC_LOADING
      IF (pLoadFile.NE.' ') THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: pLoadFile is set but you have not'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &  'compiled the model with the pressure loading code.'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A,A)')
     &  'Re-compile with:  #define ATMOSPHERIC_LOADING',
     &  '              or  -DATMOSPHERIC_LOADING'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

C     o If taveFreq is finite, then we must make sure the diagnostics
C       code is being compiled
#ifndef ALLOW_TIMEAVE
ceh3 needs an IF ( useTIMEAVE ) THEN
      IF (taveFreq.NE.0.) THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: taveFreq <> 0  but you have'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'not compiled the model with the diagnostics routines.'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A,A)')
     &  'Re-compile with:  #define ALLOW_TIMEAVE',
     &  '              or  -DALLOW_TIMEAVE'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-  check parameter consistency :

      IF ( viscA4.NE.0. .AND. (Olx.LT.3 .OR. Oly.LT.3)) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use Biharmonic Visc. (viscA4) with',
     &  ' overlap (Olx,Oly) smaller than 3'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF                 

      IF ( rigidLid .AND. implicitFreeSurface ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Cannot select both implicitFreeSurface',
     &  ' and rigidLid.'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF                 

      IF (rigidLid .AND. exactConserv) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: exactConserv not compatible with'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (rigidLid .AND. useRealFreshWaterFlux) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: useRealFreshWaterFlux not compatible with'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( (implicSurfPress.NE.1. .OR. implicDiv2DFlow.NE.1.)
     &    .AND. nonHydrostatic ) THEN
        WRITE(msgBuf,'(A,A)') 'CONFIG_CHECK: nonHydrostatic',
     & ' NOT SAFE with non-fully implicit Barotropic solver'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A,A)') 'CONFIG_CHECK: To by-pass this',
     &    'STOP, comment this test and re-compile config_check'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (nonlinFreeSurf.NE.0 .AND. .NOT.exactConserv) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: nonlinFreeSurf cannot be used'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: without exactConserv'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (select_rStar.NE.0 .AND. .NOT.exactConserv) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: r* Coordinate cannot be used'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: without exactConserv'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

C- note : not implemented in checkpoint48b but it's done now (since 01-28-03)
c     IF (select_rStar.GT.0 .AND. useOBCS ) THEN
c       WRITE(msgBuf,'(A)') 
c    &   'CONFIG_CHECK: r* Coordinate not yet implemented'
c       CALL PRINT_ERROR( msgBuf , 1)
c       WRITE(msgBuf,'(A)')
c    &   'CONFIG_CHECK: in OBC package'
c       CALL PRINT_ERROR( msgBuf , 1)
c       STOP 'ABNORMAL END: S/R CONFIG_CHECK'
c     ENDIF

      IF (nonlinFreeSurf.NE.0 .AND. nonHydrostatic) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: nonlinFreeSurf not yet implemented'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: in nonHydrostatic code'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (nonlinFreeSurf.NE.0.AND.deltaTfreesurf.NE.deltaTtracer) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: WARNING: nonlinFreeSurf might cause problems'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)                      
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: with different FreeSurf & Tracer time-steps'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)                      
      ENDIF

      IF (useRealFreshWaterFlux .AND. exactConserv
     &    .AND.startTime.NE.0. .AND. implicSurfPress.EQ.0. _d 0) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: RealFreshWaterFlux+implicSurfP=0+exactConserv:'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: restart not implemented in this config'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

#ifdef NONLIN_FRSURF
      IF (useRealFreshWaterFlux .AND. .NOT.exactConserv
     &            .AND. buoyancyRelation.EQ.'OCEANICP' ) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: RealFreshWaterFlux with OCEANICP'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: requires exactConserv=T'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#else
      IF (useRealFreshWaterFlux .AND. exactConserv
     &            .AND. implicSurfPress.NE.1. _d 0 ) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: Pb with restart in this config'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: ==> use #define NONLIN_FRSURF to fix it'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (useRealFreshWaterFlux
     &            .AND. buoyancyRelation.EQ.'OCEANICP' ) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: E-P effects on wVel are not included'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)                      
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: ==> use #define NONLIN_FRSURF to fix it'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)                      
      ENDIF
#endif /* NONLIN_FRSURF */

      IF (.NOT.useCDscheme .AND. (tauCD.NE.0. .OR. rCD.NE.-1.) ) THEN
C- jmc: since useCDscheme is a new [04-13-03] flag (default=F), 
C       put this WARNING to stress that even if CD-scheme parameters 
C       (tauCD,rCD) are set, CD-scheme is not used without useCDscheme=T 
C-    and STOP if using mom_fluxform (following Chris advise).
C- jmc: but ultimately, this block can/will be removed.
       IF (.NOT.vectorInvariantMomentum.AND.momStepping) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: CD-scheme is OFF but params(tauCD,rCD) are set'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: to turn ON CD-scheme: => "useCDscheme=.TRUE."',
     &   ' in "data", namelist PARM01' 
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
       ENDIF
        WRITE(msgBuf,'(2A)') '**WARNNING** ',
     &   'CONFIG_CHECK: CD-scheme is OFF but params(tauCD,rCD) are set'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)                      
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: to turn ON CD-scheme: => "useCDscheme=.TRUE."',
     &   ' in "data", namelist PARM01' 
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)                      
      ENDIF

      WRITE(msgBuf,'(A)') 'CONFIG_CHECK: OK'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

      RETURN
      END
