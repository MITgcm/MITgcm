C $Header: /u/gcmpack/MITgcm/model/src/config_check.F,v 1.40 2006/12/24 20:03:49 jmc Exp $
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

#ifndef ALLOW_MNC
      IF (useMNC) THEN
        WRITE(msgBuf,'(2A)') '**WARNING** ',
     &   'CONFIG_CHECK: useMNC is TRUE and #undef ALLOW_MNC'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
      ENDIF
#endif

#ifndef ALLOW_CD_CODE
      IF (useCDscheme) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: useCDscheme is TRUE and #undef ALLOW_CD_CODE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF (tauCD.NE.0.) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: tauCD has been set but the cd_code package is',
     &   ' enabled'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_NONHYDROSTATIC
      IF (use3Dsolver) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef ALLOW_NONHYDROSTATIC and'
        CALL PRINT_ERROR( msgBuf , myThid)
       IF ( implicitIntGravWave ) WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: implicitIntGravWave is TRUE'
       IF ( nonHydrostatic ) WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonHydrostatic is TRUE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_ADAMSBASHFORTH_3
      IF ( alph_AB.NE.UNSET_RL .OR. beta_AB.NE.UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   '#undef ALLOW_ADAMSBASHFORTH_3 but alph_AB,beta_AB'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A,1P2E20.7)')
     &   'CONFIG_CHECK: are set to:',alph_AB,beta_AB
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef INCLUDE_IMPLVERTADV_CODE
      IF ( momImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: but momImplVertAdv is TRUE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( tempImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: but tempImplVertAdv is TRUE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( saltImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: but saltImplVertAdv is TRUE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( dTtracerLev(1).NE.dTtracerLev(Nr) .AND. implicitDiffusion
     &     .AND. ( saltStepping .OR. tempStepping .OR. usePTRACERS )
     &   ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'but implicitDiffusion=T with non-uniform dTtracerLev'
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
        WRITE(msgBuf,'(A)')
     &  'Re-compile with:  #define ATMOSPHERIC_LOADING'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( useRealFreshWaterFlux .AND. useThSIce ) THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: sIceLoad is computed but'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &  'pressure loading code is not compiled.'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &  'Re-compile with:  #define ATMOSPHERIC_LOADING'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_MOM_VECINV
      IF ( momStepping .AND. vectorInvariantMomentum ) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: cannot step forward Momentum',
     &   ' without pkg/mom_vecinv'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'Re-compile with pkg "mom_vecinv" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif
#ifndef ALLOW_MOM_FLUXFORM
      IF ( momStepping .AND. .NOT.vectorInvariantMomentum ) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: cannot step forward Momentum',
     &   ' without pkg/mom_fluxform'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'Re-compile with pkg "mom_fluxform" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_GENERIC_ADVDIFF
      IF ( tempStepping .OR. saltStepping ) THEN
        WRITE(msgBuf,'(2A)')
     &  'CONFIG_CHECK: cannot step forward Temp or Salt',
     &  ' without pkg/generic_advdiff'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'Re-compile with pkg "generic_advdiff" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

C     o If taveFreq is finite, then we must make sure the diagnostics
C       code is being compiled
#ifndef ALLOW_TIMEAVE
      IF (taveFreq.NE.0.) THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: taveFreq <> 0  but pkg/timeave is not compiled'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'Re-compile with pkg "timeave" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_BALANCE_FLUXES
      IF (balanceEmPmR .OR. balanceQnet) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: balanceEmPmR/Qnet is set but balance code ',
     &  'is not compiled.'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'Re-compile with  ALLOW_BALANCE_FLUXES defined'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-  check parameter consistency :

      IF ( ( Olx.LT.3 .OR. Oly.LT.3 ) .AND.
     &     ( viscC4leithD.NE.0.  .OR. viscC4leith.NE.0.
     &     .OR. viscC4smag.NE.0. .OR. viscA4Grid.NE.0.
     &     .OR. viscA4D.NE.0.    .OR. viscA4Z.NE.0. ) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use Biharmonic Visc. (viscA4) with',
     &  ' overlap (Olx,Oly) smaller than 3'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( ( Olx.LT.3 .OR. Oly.LT.3 ) .AND.
     &     ( viscC2leithD.NE.0. .OR. viscC4leithD.NE.0. )
     &   ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use Leith Visc.(div.part) with',
     &  ' overlap (Olx,Oly) smaller than 3'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

C-    Deep-Atmosphere & Anelastic limitations:
      IF ( deepAtmosphere .AND.
     &     useRealFreshWaterFlux .AND. usingPCoords ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Deep-Atmosphere not yet implemented with',
     &  ' real-Fresh-Water option in P-coordinate'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( select_rStar.NE.0 .AND.
     &        ( deepAtmosphere .OR.
     &          usingZCoords.AND.rhoRefFile .NE. ' ' ) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Deep-Atmosphere or Anelastic',
     &  ' not yet implemented with rStar'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( vectorInvariantMomentum .AND.
     &        ( deepAtmosphere .OR.
     &          usingZCoords.AND.rhoRefFile .NE. ' ' ) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Deep-Atmosphere or Anelastic',
     &  ' not yet implemented in Vector-Invariant momentum code'
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

c     IF (nonlinFreeSurf.NE.0 .AND. nonHydrostatic) THEN
      IF (nonlinFreeSurf.NE.0 .AND. use3Dsolver) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonlinFreeSurf not yet implemented'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: in nonHydrostatic code'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( nonlinFreeSurf.NE.0 .AND.
     &     deltaTfreesurf.NE.dTtracerLev(1) ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: WARNING: nonlinFreeSurf might cause problems'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: with different FreeSurf & Tracer time-steps'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      ENDIF

      IF ( useRealFreshWaterFlux .AND. exactConserv
     &     .AND. implicDiv2DFlow.EQ.0. _d 0
     &     .AND. startTime.NE.baseTime .AND. usePickupBeforeC54 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: RealFreshWaterFlux+implicSurfP=0+exactConserv:'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: restart not implemented in this config'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( useRealFreshWaterFlux .AND. .NOT.exactConserv
     &     .AND. implicDiv2DFlow.NE.1. ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: **WARNNING** ',
     &   'RealFreshWater & implicDiv2DFlow < 1'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: works better',
     &   ' with exactConserv=.T. (+ #define EXACT_CONSERV)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      ENDIF

#ifdef EXACT_CONSERV
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
      IF (useRealFreshWaterFlux
     &            .AND. buoyancyRelation.EQ.'OCEANICP' ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: E-P effects on wVel are not included'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: ==> use #define EXACT_CONSERV to fix it'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      ENDIF
#endif /* EXACT_CONSERV */

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

      IF ( useCDscheme .AND. useCubedSphereExchange ) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: CD-scheme not implemented on CubedSphere grid'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( momForcingOutAB.NE.0 .AND. momForcingOutAB.NE.1 ) THEN
        WRITE(msgBuf,'(A,I10,A)') 'CONFIG_CHECK: momForcingOutAB=',
     &                             momForcingOutAB, ' not allowed'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: momForcingOutAB ',
     &                       'should be =1 (Out of AB) or =0 (In AB)'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( tracForcingOutAB.NE.0 .AND. tracForcingOutAB.NE.1 ) THEN
        WRITE(msgBuf,'(A,I10,A)') 'CONFIG_CHECK: tracForcingOutAB=',
     &                             tracForcingOutAB, ' not allowed'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: tracForcingOutAB ',
     &                       'should be =1 (Out of AB) or =0 (In AB)'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( useMATRIX .AND. useGCHEM ) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: cannot set both: useMATRIX & useGCHEM'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( useMATRIX .AND. .NOT.usePTRACERS ) THEN
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: cannot set useMATRIX without ',
     &       'setting usePTRACERS'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      WRITE(msgBuf,'(A)') 'CONFIG_CHECK: OK'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

      RETURN
      END
