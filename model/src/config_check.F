C $Header: /u/gcmpack/MITgcm/model/src/config_check.F,v 1.58 2011/03/15 00:15:23 jmc Exp $
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
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C-  check that CPP option is "defined" when running-flag parameter is on:

C     o If diffKrFile is set, then we should make sure the corresponing
C       code is being compiled
#ifndef ALLOW_3D_DIFFKR
      IF (diffKrFile.NE.' ') THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: diffKrFile is set but never used.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'Re-compile with:  #define ALLOW_3D_DIFFKR'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_NONHYDROSTATIC
      IF (use3Dsolver) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef ALLOW_NONHYDROSTATIC and'
        CALL PRINT_ERROR( msgBuf, myThid )
       IF ( implicitIntGravWave ) WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: implicitIntGravWave is TRUE'
       IF ( nonHydrostatic ) WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonHydrostatic is TRUE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_ADAMSBASHFORTH_3
      IF ( alph_AB.NE.UNSET_RL .OR. beta_AB.NE.UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   '#undef ALLOW_ADAMSBASHFORTH_3 but alph_AB,beta_AB'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,1P2E20.7)')
     &   'CONFIG_CHECK: are set to:',alph_AB,beta_AB
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef INCLUDE_IMPLVERTADV_CODE
      IF ( momImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: but momImplVertAdv is TRUE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( tempImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: but tempImplVertAdv is TRUE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( saltImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: but saltImplVertAdv is TRUE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( dTtracerLev(1).NE.dTtracerLev(Nr) .AND. implicitDiffusion
     &     .AND. ( saltStepping .OR. tempStepping .OR. usePTRACERS )
     &   ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'but implicitDiffusion=T with non-uniform dTtracerLev'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( momImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: momImplVertAdv is not yet'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: supported in adjoint mode'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifdef ALLOW_DEPTH_CONTROL
      IF ( useOBCS ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: DEPTH_CONTROL code not compatible with OBCS'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef EXACT_CONSERV
      IF (exactConserv) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef EXACT_CONSERV and'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: exactConserv is TRUE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef NONLIN_FRSURF
      IF (nonlinFreeSurf.NE.0) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef NONLIN_FRSURF and'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonlinFreeSurf is non-zero'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef NONLIN_FRSURF
      IF (select_rStar .NE. 0) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rStar is part of NonLin-FS '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: ==> set #define NONLIN_FRSURF to use it'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif /* NONLIN_FRSURF */

#ifdef DISABLE_RSTAR_CODE
      IF ( select_rStar.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rStar code disable (DISABLE_RSTAR_CODE defined)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: ==> set #undef DISABLE_RSTAR_CODE to use it'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif /* DISABLE_RSTAR_CODE */

#ifdef DISABLE_SIGMA_CODE
      IF ( selectSigmaCoord.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Sigma code disable (DISABLE_SIGMA_CODE defined)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: ==> set #undef DISABLE_SIGMA_CODE to use it'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif /* DISABLE_SIGMA_CODE */

#ifdef USE_NATURAL_BCS
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: USE_NATURAL_BCS option has been replaced'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: by useRealFreshWaterFlux=TRUE in data file'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
#endif

#ifndef ALLOW_ADDFLUID
      IF ( selectAddFluid.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef ALLOW_ADDFLUID and'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,I4,A)') 'CONFIG_CHECK: selectAddFluid=',
     &                           selectAddFluid, ' is not zero'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif /* ALLOW_ADDFLUID */

C     o If pLoadFile is set, then we should make sure the corresponing
C       code is being compiled
#ifndef ATMOSPHERIC_LOADING
      IF (pLoadFile.NE.' ') THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: pLoadFile is set but you have not'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'compiled the model with the pressure loading code.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'Re-compile with:  #define ATMOSPHERIC_LOADING'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( useRealFreshWaterFlux .AND. useThSIce ) THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: sIceLoad is computed but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'pressure loading code is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'Re-compile with:  #define ATMOSPHERIC_LOADING'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_BALANCE_FLUXES
      IF (balanceEmPmR .OR. balanceQnet) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: balanceEmPmR/Qnet is set but balance code ',
     &  'is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'Re-compile with  ALLOW_BALANCE_FLUXES defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef ALLOW_SRCG
      IF (useSRCGSolver) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: useSRCGSolver = .TRUE., but single reduction ',
     &  'code is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: Re-compile with ALLOW_SRCG defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif /* ALLOW_SRCG */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Check parameter consistency :

      IF ( ( Olx.LT.3 .OR. Oly.LT.3 ) .AND.
     &     ( viscC4leithD.NE.0.  .OR. viscC4leith.NE.0.
     &     .OR. viscC4smag.NE.0. .OR. viscA4Grid.NE.0.
     &     .OR. viscA4D.NE.0.    .OR. viscA4Z.NE.0. ) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use Biharmonic Visc. (viscA4) with',
     &  ' overlap (Olx,Oly) smaller than 3'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( ( Olx.LT.3 .OR. Oly.LT.3 ) .AND.
     &     ( viscC2leithD.NE.0. .OR. viscC4leithD.NE.0. )
     &   ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use Leith Visc.(div.part) with',
     &  ' overlap (Olx,Oly) smaller than 3'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

C--   Deep-Atmosphere & Anelastic limitations:
      IF ( deepAtmosphere .AND.
     &     useRealFreshWaterFlux .AND. usingPCoords ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Deep-Atmosphere not yet implemented with',
     &  ' real-Fresh-Water option in P-coordinate'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( select_rStar.NE.0 .AND.
     &        ( deepAtmosphere .OR.
     &          usingZCoords.AND.rhoRefFile .NE. ' ' ) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Deep-Atmosphere or Anelastic',
     &  ' not yet implemented with rStar'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( vectorInvariantMomentum .AND.
     &        ( deepAtmosphere .OR.
     &          usingZCoords.AND.rhoRefFile .NE. ' ' ) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Deep-Atmosphere or Anelastic',
     &  ' not yet implemented in Vector-Invariant momentum code'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

C--   Free-surface related limitations:
      IF ( rigidLid .AND. implicitFreeSurface ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Cannot select both implicitFreeSurface',
     &  ' and rigidLid.'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (rigidLid .AND. exactConserv) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: exactConserv not compatible with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( linFSConserveTr .AND. nonlinFreeSurf.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Cannot select both a Nonlinear Free Surf.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: and Tracer Correction of Lin. Free Surf.'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (rigidLid .AND. useRealFreshWaterFlux) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: useRealFreshWaterFlux not compatible with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (nonlinFreeSurf.NE.0 .AND. .NOT.exactConserv) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonlinFreeSurf cannot be used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: without exactConserv'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (select_rStar.NE.0 .AND. .NOT.exactConserv) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: r* Coordinate cannot be used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: without exactConserv'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( selectSigmaCoord.NE.0 ) THEN
       IF ( fluidIsWater ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Sigma-Coords not yet coded for Oceanic set-up'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
       ENDIF
       IF ( nonlinFreeSurf.LE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Sigma-Coords not coded for Lin-FreeSurf'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
       ENDIF
       IF (select_rStar.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Sigma-Coords and rStar are not compatible'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
       ENDIF
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Sigma-Coords code neither complete nor tested'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C- note : not implemented in checkpoint48b but it is done now (since 01-28-03)
c     IF (select_rStar.GT.0 .AND. useOBCS ) THEN
c       STOP 'ABNORMAL END: S/R CONFIG_CHECK'
c     ENDIF

      IF ( nonlinFreeSurf.NE.0 .AND.
     &     deltaTfreesurf.NE.dTtracerLev(1) ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &                       'nonlinFreeSurf might cause problems'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** ',
     &               'with different FreeSurf & Tracer time-steps'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( useRealFreshWaterFlux .AND. exactConserv
     &     .AND. implicDiv2Dflow.EQ.0. _d 0
     &     .AND. startTime.NE.baseTime .AND. usePickupBeforeC54 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: RealFreshWaterFlux+implicSurfP=0+exactConserv:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: restart not implemented in this config'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( useRealFreshWaterFlux .AND. .NOT.exactConserv
     &     .AND. implicDiv2Dflow.NE.1. ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &   'RealFreshWater & implicDiv2Dflow < 1'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** works better',
     &   ' with exactConserv=.T. (+ #define EXACT_CONSERV)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

#ifdef EXACT_CONSERV
      IF (useRealFreshWaterFlux .AND. .NOT.exactConserv
     &            .AND. buoyancyRelation.EQ.'OCEANICP' ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: RealFreshWaterFlux with OCEANICP'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: requires exactConserv=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#else
      IF (useRealFreshWaterFlux
     &            .AND. buoyancyRelation.EQ.'OCEANICP' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &               'E-P effects on wVel are not included'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &               '==> use #define EXACT_CONSERV to fix it'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif /* EXACT_CONSERV */

      IF ( selectAddFluid.LT.-1 .OR. selectAddFluid.GT.2 ) THEN
        WRITE(msgBuf,'(A,I10,A)') 'CONFIG_CHECK: selectAddFluid=',
     &                             selectAddFluid, ' not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &       'should be =0 (Off), 1,2 (Add Mass) or -1 (Virtual Flux)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( selectAddFluid.GE.1 .AND. rigidLid ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: selectAddFluid > 0 not compatible with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( selectAddFluid.GE.1 .AND. .NOT.staggerTimeStep ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &   'synchronous time-stepping =>'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** ',
     &   '1 time-step mismatch in AddFluid effects on T & S'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C--   Non-hydrostatic and 3-D solver related limitations:
      IF (nonlinFreeSurf.NE.0 .AND. use3Dsolver) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonlinFreeSurf not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: in nonHydrostatic code'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( implicitNHPress*implicSurfPress*implicDiv2Dflow.NE.1.
     &     .AND. implicitIntGravWave ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: implicitIntGravWave',
     &    ' NOT SAFE with non-fully implicit solver'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: To by-pass this',
     &    'STOP, comment this test and re-compile config_check'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( nonHydrostatic .AND. .NOT.exactConserv
     &     .AND. implicDiv2Dflow.NE.1. ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: Needs exactConserv=T',
     &               ' for nonHydrostatic with implicDiv2Dflow < 1'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( nonHydrostatic .AND.
     &     implicitNHPress.NE.implicSurfPress ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &               ' nonHydrostatic might cause problems with'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &               'different implicitNHPress & implicSurfPress'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( implicitViscosity .AND. use3Dsolver ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &    'Implicit viscosity applies to provisional u,vVel'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** => not consistent with',
     &    'final vertical shear (after appling 3-D solver solution'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( implicitViscosity .AND. nonHydrostatic ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &    'Implicit viscosity not implemented in CALC_GW'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &    'Explicit viscosity might become unstable if too large'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C--   Momentum related limitations:
      IF ( vectorInvariantMomentum.AND.momStepping ) THEN
       IF ( highOrderVorticity.AND.upwindVorticity ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   '"highOrderVorticity" conflicts with "upwindVorticity"'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
       ENDIF
      ENDIF
      IF ( selectCoriMap.LT.0 .OR. selectCoriMap.GT.3 ) THEN
        WRITE(msgBuf,'(2A,I4)') 'CONFIG_CHECK: ',
     &       'Invalid option: selectCoriMap=', selectCoriMap
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (.NOT.useCDscheme .AND. (tauCD.NE.0. .OR. rCD.NE.-1.) ) THEN
C- jmc: since useCDscheme is a new [04-13-03] flag (default=F),
C       put this WARNING to stress that even if CD-scheme parameters
C       (tauCD,rCD) are set, CD-scheme is not used without useCDscheme=T
C-    and STOP if using mom_fluxform (following Chris advise).
C- jmc: but ultimately, this block can/will be removed.
       IF (.NOT.vectorInvariantMomentum.AND.momStepping) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: CD-scheme is OFF but params(tauCD,rCD) are set'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: to turn ON CD-scheme: => "useCDscheme=.TRUE."',
     &   ' in "data", namelist PARM01'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
       ENDIF
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &   'CD-scheme is OFF but params(tauCD,rCD) are set'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3A)') '** WARNING ** ',
     &   'to turn ON CD-scheme: => "useCDscheme=.TRUE."',
     &   ' in "data", namelist PARM01'
        WRITE(msgBuf,'(3A)') '** WARNING ** to turn ON CD-scheme:',
     &   ' => "useCDscheme=.TRUE." in "data", namelist PARM01'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( useCDscheme .AND. useCubedSphereExchange ) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: CD-scheme not implemented on CubedSphere grid'
        CALL PRINT_ERROR( msgBuf, myThid )
cph        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

C--   Time-stepping limitations
      IF ( momForcingOutAB.NE.0 .AND. momForcingOutAB.NE.1 ) THEN
        WRITE(msgBuf,'(A,I10,A)') 'CONFIG_CHECK: momForcingOutAB=',
     &                             momForcingOutAB, ' not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: momForcingOutAB ',
     &                       'should be =1 (Out of AB) or =0 (In AB)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF ( tracForcingOutAB.NE.0 .AND. tracForcingOutAB.NE.1 ) THEN
        WRITE(msgBuf,'(A,I10,A)') 'CONFIG_CHECK: tracForcingOutAB=',
     &                             tracForcingOutAB, ' not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: tracForcingOutAB ',
     &                       'should be =1 (Out of AB) or =0 (In AB)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

C--   Grid limitations:
      IF ( rotateGrid ) THEN
       IF ( .NOT. usingSphericalPolarGrid ) THEN
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: specifying Euler angles makes only ',
     &       'sense with usingSphericalGrid=.TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
       ENDIF
       IF ( useFLT .OR. useZonal_Filt .OR. useECCO ) THEN
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: specifying Euler angles will probably ',
     &       'not work with pkgs FLT, ZONAL_FLT, ECCO'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
       ENDIF
#ifdef ALLOW_PROFILES
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: specifying Euler angles will probably ',
     &       'not work with pkg profiles'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
#endif /* ALLOW_PROFILES */
      ENDIF

C--   Packages conflict
      IF ( useMATRIX .AND. useGCHEM ) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: cannot set both: useMATRIX & useGCHEM'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( useMATRIX .AND. .NOT.usePTRACERS ) THEN
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: cannot set useMATRIX without ',
     &       'setting usePTRACERS'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF ( (useSEAICE .OR. useThSIce) .AND. allowFreezing ) THEN
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: cannot set allowFreezing',
     &       ' with pkgs SEAICE or THSICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// CONFIG_CHECK : Normal End'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      RETURN
      END
