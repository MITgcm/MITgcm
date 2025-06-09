#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

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
#ifdef ALLOW_MOM_COMMON
# include "MOM_VISC.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   ::  my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
C     errCount :: error counter (inconsitent params and other errors)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
CEOP

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// Check Model config. (CONFIG_CHECK):'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      _END_MASTER(myThid)

C--   MPI + multi-threads: seems to be OK to let master-thread check & stop
C      (as long as all procs finish cleanly by calling ALL_PROC_DIE)
      _BEGIN_MASTER(myThid)
      errCount = 0

C-  check that CPP option is "defined" when running-flag parameter is on:

#ifndef INCLUDE_CONVECT_CALL
C     o Check that Convective-adjustment code is compiled If cAdjFreq <> 0
      IF ( cAdjFreq .NE. 0. ) THEN
#ifdef INCLUDE_CONVECT_INI_CALL
        WRITE(msgBuf,'(2A)') '** Warning ** CONFIG_CHECK: ',
     &  'Convective-Adjustment (cAdjFreq) turned off (except INI)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** Warning ** CONFIG_CHECK: ',
     &  ' without "#define INCLUDE_CONVECT_CALL" in CPP_OPTIONS.h'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
#else
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'Cannot use Convective-Adjustment (cAdjFreq) without'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  ' defining INCLUDE_CONVECT_(CALL/INI_CALL) in CPP_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif
      ENDIF
#endif /* ndef INCLUDE_CONVECT_CALL */

C     o If diffKrFile is set, then we should make sure the corresponing
C       code is being compiled
#ifndef ALLOW_3D_DIFFKR
      IF (diffKrFile.NE.' ') THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: diffKrFile is set but never used.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'Re-compile with: "#define ALLOW_3D_DIFFKR"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifdef EXCLUDE_PCELL_MIX_CODE
      IF ( interDiffKr_pCell .OR. interViscAr_pCell
     &                       .OR. pCellMix_select.NE.0 ) THEN
        IF ( interDiffKr_pCell ) THEN
          WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &    'Cannot use interDiffKr_pCell=.TRUE.'
          CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( interViscAr_pCell ) THEN
          WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &    'Cannot use interViscAr_pCell=.TRUE.'
          CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( pCellMix_select.NE.0 ) THEN
          WRITE(msgBuf,'(2A,I6)') 'CONFIG_CHECK: ',
     &    'Cannot use pCellMix_select=', pCellMix_select
          CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: when compiled ',
     &  'with "#define EXCLUDE_PCELL_MIX_CODE" in CPP_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifndef ALLOW_LEITH_QG
C     Check that Leith QG viscosity coefficeint is zero if the
C     QGLeith code has not been compiled.
      IF ( viscC2LeithQG.NE.zeroRL )  THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'must #define ALLOW_LEITH_QG in MOM_COMMON_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  ' in order to use QG-Leith (viscC2LeithQG > 0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_LEITH_QG */

#ifndef ALLOW_SMAG_3D
      IF ( useSmag3D ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'Cannot set useSmag3D=TRUE when compiled with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  '"#undef ALLOW_SMAG_3D" in MOM_COMMON_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif
#ifndef ALLOW_SMAG_3D_DIFFUSIVITY
      IF ( smag3D_diffCoeff.NE.zeroRL ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'Cannot use smag3D_diffCoeff > 0 when compiled with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  '"#undef ALLOW_SMAG_3D_DIFFUSIVITY" in CPP_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
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
        errCount = errCount + 1
      ENDIF
#endif

#ifndef ALLOW_QHYD_STAGGER_TS
      IF ( quasiHydrostatic .AND. staggerTimeStep ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &    'Cannot use quasiHydrostatic with staggerTimeStep'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: when compiled ',
     &  'with "#undef ALLOW_QHYD_STAGGER_TS" in CPP_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
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
        errCount = errCount + 1
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
        errCount = errCount + 1
      ENDIF
      IF ( tempImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: but tempImplVertAdv is TRUE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( saltImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: but saltImplVertAdv is TRUE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
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
        errCount = errCount + 1
      ENDIF
      IF ( selectImplicitDrag.GT.0 ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'Needs to compile with #define INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK:  to use selectImplicitDrag > 0 ==> STOP here'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifndef ALLOW_SOLVE4_PS_AND_DRAG
      IF ( selectImplicitDrag.EQ.2 ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'Needs to compile with #define ALLOW_SOLVE4_PS_AND_DRAG'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK:  to use selectImplicitDrag = 2 ==> STOP here'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF
      IF ( momImplVertAdv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: momImplVertAdv is not yet'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: supported in adjoint mode'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectImplicitDrag.GT.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: selectImplicitDrag is not yet'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: supported in adjoint mode'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifdef ALLOW_DEPTH_CONTROL
      IF ( useOBCS ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: DEPTH_CONTROL code not compatible with OBCS'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
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
        errCount = errCount + 1
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
        errCount = errCount + 1
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
        errCount = errCount + 1
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
        errCount = errCount + 1
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
        errCount = errCount + 1
      ENDIF
#endif /* DISABLE_SIGMA_CODE */

#ifdef USE_NATURAL_BCS
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: USE_NATURAL_BCS option has been replaced'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: by useRealFreshWaterFlux=TRUE in data file'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif

#ifndef ALLOW_ADDFLUID
      IF ( selectAddFluid.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: #undef ALLOW_ADDFLUID (CPP_OPTIONS.h) and'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,I4,A)') 'CONFIG_CHECK: selectAddFluid=',
     &                           selectAddFluid, ' is not zero'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
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
     &  ' compiled the model with the pressure loading code.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  ' Re-compile with: "#define ATMOSPHERIC_LOADING"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useRealFreshWaterFlux .AND. useThSIce ) THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: sIceLoad is computed but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  ' pressure loading code is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  ' Re-compile with: "#define ATMOSPHERIC_LOADING"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

      IF ( geothermalFile.NE.' ' ) THEN
C     o Need to use S/R APPLY_FORCING_T to get Geothermal forcing
#ifdef USE_OLD_EXTERNAL_FORCING
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'geothermalFile is set but Geothermal-Flux code'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')' is missing.',
     &  ' Re-compile with "#undef USE_OLD_EXTERNAL_FORCING"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif /* USE_OLD_EXTERNAL_FORCING */
C     o Need to define ALLOW_GEOTHERMAL_FLUX to use geothermalFile forcing
#ifndef ALLOW_GEOTHERMAL_FLUX
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'geothermalFile is set but Geothermal-Flux code'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')' is not compiled.',
     &  ' Re-compile with "#define ALLOW_GEOTHERMAL_FLUX"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif /* ALLOW_GEOTHERMAL_FLUX */
      ENDIF

C     o Need to use S/R APPLY_FORCING_U,V to get Tidal forcing
#ifdef USE_OLD_EXTERNAL_FORCING
      IF ( momTidalForcing ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'momTidalForcing is set but Tidal forcing code'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')' is missing.',
     &  ' Re-compile with "#undef USE_OLD_EXTERNAL_FORCING"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* USE_OLD_EXTERNAL_FORCING */

#ifndef ALLOW_FRICTION_HEATING
      IF ( addFrictionHeating ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: addFrictionHeating=T',
     &  ' but FRICTIONAL_HEATING code is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: Re-compile with:',
     &   ' "#define ALLOW_FRICTION_HEATING" (CPP_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifdef ALLOW_BALANCE_FLUXES
      IF ( selectBalanceEmPmR.LT.0 .OR. selectBalanceEmPmR.GT.2 ) THEN
        WRITE(msgBuf,'(2A,I6,A)') 'CONFIG_CHECK: ',
     &   'selectBalanceEmPmR=', selectBalanceEmPmR,' : invalid value'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#else /* ALLOW_BALANCE_FLUXES */
      IF ( selectBalanceEmPmR.NE.0 .OR. balanceQnet ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: balanceEmPmR/Qnet is set but balance code ',
     &  'is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'Re-compile with:  ALLOW_BALANCE_FLUXES defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_BALANCE_FLUXES */

#ifndef ALLOW_BALANCE_RELAX
      IF (balanceThetaClimRelax .OR. balanceSaltClimRelax) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: balanceTheta/SaltClimRelax is set ',
     &  'but balance code is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &  'Re-compile with  ALLOW_BALANCE_RELAX defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifndef ALLOW_CG2D_NSA
      IF (useNSACGSolver) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: useNSACGSolver = .TRUE., but CG2D_NSA ',
     &  'code is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: Re-compile with ALLOW_CG2D_NSA defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_CG2D_NSA */

#ifndef ALLOW_SRCG
      IF (useSRCGSolver) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: useSRCGSolver = .TRUE., but single reduction ',
     &  'code is not compiled.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: Re-compile with ALLOW_SRCG defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_SRCG */

#ifndef ALLOW_BOTTOMDRAG_ROUGHNESS
      IF (zRoughBot .GT. 0.) THEN
        WRITE(msgBuf,'(A,A)') 'CONFIG_CHECK: zRoughBot > 0.',
     &  ' but bottom drag code for roughness length is not compiled'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: Re-compile with',
     &  ' #define ALLOW_BOTTOMDRAG_ROUGHNESS (MOM_COMMON_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_BOTTOMDRAG_ROUGHNESS */

      IF ( useNSACGSolver .AND. useSRCGSolver ) THEN
        WRITE(msgBuf,'(A,A)')
     &      'CONFIG_CHECK: both useNSACGSolver and ',
     &      'useSRCGSolver = .TRUE., this is not allowed.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Check parameter consistency :

      IF ( ( OLx.LT.2 .OR. OLy.LT.2 ) .AND. momStepping ) THEN
C-    Current algorithm (solve_for_pressure, RHS) prevents to use
C     momAdvection or viscAh > 0 with OLx,y=1 ; it should still allow us
C     to use OLx,y=1 when momAdvection=F and viscAh=0 but some loop range
C     in few momentum S/R are not carefully set to support this.
        WRITE(msgBuf,'(A,A)') 'CONFIG_CHECK: Cannot use momStepping',
     &  ' with overlap (OLx,OLy) smaller than 2'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( ( OLx.LT.3 .OR. OLy.LT.3 ) .AND.
     &     ( viscC4leithD.NE.0.  .OR. viscC4leith.NE.0.
     &     .OR. viscC4smag.NE.0. .OR. viscA4Grid.NE.0.
     &     .OR. viscA4D.NE.0.    .OR. viscA4Z.NE.0. ) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use Biharmonic Visc. (viscA4) with',
     &  ' overlap (OLx,OLy) smaller than 3'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( ( OLx.LT.3 .OR. OLy.LT.3 ) .AND.
     &     ( viscC2leithD.NE.0. .OR. viscC4leithD.NE.0. .OR.
     &       viscC2LeithQG.NE.0.)
     &   ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use Leith Visc.(div.part or QG) with',
     &  ' overlap (OLx,OLy) smaller than 3'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( ( OLx.LT.3 .OR. OLy.LT.3 ) .AND.
     &     useSmag3D .AND. useCDscheme ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use Smag-3D + CD-scheme with',
     &  ' overlap (OLx,OLy) smaller than 3'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( ( viscC4leithD.NE.0. .OR. viscC4leith.NE.0. .OR.
     &       viscC2leithD.NE.0. .OR. viscC2leith.NE.0.)
     &     .AND. (viscC2LeithQG.NE.0) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: cannot use LeithQG in conjunction with',
     &  ' either standard Leith or LeithD'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

#ifndef DISCONNECTED_TILES
C     Overlaps cannot be larger than interior tile except for special cases
      IF ( sNx.LT.OLx ) THEN
#ifdef ALLOW_EXCH2
       WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: sNx<OLx not allowed with ALLOW_EXCH2 defined'
       CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif /* ALLOW_EXCH2 */
       IF ( Nx.NE.1 ) THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: sNx<OLx not allowed unless Nx=1'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
      IF ( sNy.LT.OLy ) THEN
#ifdef ALLOW_EXCH2
       WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: sNy<OLy not allowed with ALLOW_EXCH2 defined'
       CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif /* ALLOW_EXCH2 */
       IF ( Ny.NE.1 ) THEN
        WRITE(msgBuf,'(A)')
     &  'CONFIG_CHECK: sNy<OLy not allowed unless Ny=1'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
#endif /* ndef DISCONNECTED_TILES */

C--   Gravity vertical profile limitations:
      IF ( usingPCoords .AND. gravityFile .NE. ' ' ) THEN
        WRITE(msgBuf,'(A,A)') 'CONFIG_CHECK: Variable gravity',
     &  ' not yet implemented for P-coordinate'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( select_rStar.NE.0 .AND. gravityFile .NE. ' ' ) THEN
        WRITE(msgBuf,'(A,A)') 'CONFIG_CHECK: Variable gravity',
     &  ' not yet implemented with rStar'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--   Deep-Atmosphere & Anelastic limitations:
      IF ( deepAtmosphere .AND.
     &     useRealFreshWaterFlux .AND. usingPCoords ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Deep-Atmosphere not yet implemented with',
     &  ' real-Fresh-Water option in P-coordinate'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( select_rStar.NE.0 .AND.
     &        ( deepAtmosphere .OR.
     &          usingZCoords.AND.rhoRefFile .NE. ' ' ) ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Deep-Atmosphere or Anelastic',
     &  ' not yet implemented with rStar'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--   Free-surface related limitations:
      IF ( cg2dUseMinResSol.LT.0 .OR. cg2dUseMinResSol.GT.1 ) THEN
        WRITE(msgBuf,'(A,I10,A)')
     &   'CONFIG_CHECK: cg2dUseMinResSol set to unvalid value(=',
     &                  cg2dUseMinResSol, ')'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( rigidLid .AND. implicitFreeSurface ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'CONFIG_CHECK: Cannot select both implicitFreeSurface',
     &  ' and rigidLid.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( rigidLid .AND. exactConserv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: exactConserv not compatible with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( linFSConserveTr .AND. nonlinFreeSurf.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Cannot select both a Nonlinear Free Surf.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: and Tracer Correction of Lin. Free Surf.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( rigidLid .AND. useRealFreshWaterFlux ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: useRealFreshWaterFlux not compatible with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( implicDiv2DFlow.NE.oneRL .AND. .NOT.exactConserv ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: implicDiv2DFlow < 1 ',
     &   'requires exactConserv= T (+ #define EXACT_CONSERV)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( nonlinFreeSurf.NE.0 .AND. .NOT.exactConserv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonlinFreeSurf cannot be used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: without exactConserv'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( select_rStar.NE.0 .AND. .NOT.exactConserv ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: r* Coordinate cannot be used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: without exactConserv'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( select_rStar.GE.1 .AND. nonlinFreeSurf.LE.0 ) THEN
        WRITE(msgBuf,'(2A,I3,A)') 'CONFIG_CHECK: r* Coordinate',
     &   ' (select_rStar=', select_rStar, ' ) cannot be used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I3,A)') 'CONFIG_CHECK: ',
     &   ' with Linear FreeSurf (nonlinFreeSurf=', nonlinFreeSurf,' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( select_rStar.EQ.2 .AND. nonlinFreeSurf.NE.4 ) THEN
C-    not consistent to account for the slope of the coordinate when
C     ignoring the variations of level-thickness in PhiHyd calculation;
C     for now, issue a warning (but might change the code later on):
        WRITE(msgBuf,'(2A,I3)') '** WARNING ** CONFIG_CHECK: ',
     &   'select_rStar=2 not right with nonlinFreeSurf=', nonlinFreeSurf
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( selectSigmaCoord.NE.0 ) THEN
       IF ( fluidIsWater ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Sigma-Coords not yet coded for Oceanic set-up'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( nonlinFreeSurf.LE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Sigma-Coords not coded for Lin-FreeSurf'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF (select_rStar.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: Sigma-Coords and rStar are not compatible'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
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
     &     deltaTFreeSurf.NE.dTtracerLev(1) ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &                       'nonlinFreeSurf might cause problems'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** ',
     &               'with different FreeSurf & Tracer time-steps'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( useRealFreshWaterFlux .AND. implicDiv2DFlow.EQ.zeroRL
     &     .AND. startTime.NE.baseTime .AND. usePickupBeforeC54 ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: usePickupBeforeC54 restart does not work with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK:  implicDiv2DFlow=0 + useRealFreshWaterFlux'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

#ifdef EXACT_CONSERV
      IF ( useRealFreshWaterFlux .AND. .NOT.exactConserv
     &            .AND. buoyancyRelation.EQ.'OCEANICP' ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: RealFreshWaterFlux with OCEANICP'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: requires exactConserv=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#else
      IF ( useRealFreshWaterFlux
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
        errCount = errCount + 1
      ENDIF
      IF ( selectAddFluid.GE.1 .AND. rigidLid ) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: selectAddFluid > 0 not compatible with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
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

C--   Pressure calculation and pressure gradient:
#ifndef INCLUDE_PHIHYD_CALCULATION_CODE
      IF ( momPressureForcing .OR. storePhiHyd4Phys ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'missing code to calculate pressure (totPhiHyd)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */
      IF ( storePhiHyd4Phys .AND. .NOT.momStepping ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'storePhiHyd4Phys = TRUE but pressure is not computed'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--   Non-hydrostatic and 3-D solver related limitations:
      IF (nonlinFreeSurf.NE.0 .AND. use3Dsolver) THEN
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonlinFreeSurf not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: in nonHydrostatic code'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( implicitNHPress*implicSurfPress*implicDiv2DFlow.NE.1.
     &     .AND. implicitIntGravWave ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: implicitIntGravWave',
     &    ' NOT SAFE with non-fully implicit solver'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: To by-pass this',
     &    'STOP, comment this test and re-compile config_check'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( implicitNHPress*implicSurfPress*implicDiv2DFlow.EQ.0.
     &     .AND. nonHydrostatic ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   'Needs non-zero implicit factors (i.e., implicitNHPress,'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &   ' implicSurfPress & implicDiv2DFlow) to run nonHydrostatic'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( nonHydrostatic .AND.
     &     implicitNHPress.NE.implicSurfPress ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &               'nonHydrostatic might cause problems with'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &               'different implicitNHPress & implicSurfPress'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( selectImplicitDrag.EQ.2 .AND. use3Dsolver ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: selectImplicitDrag=2',
     &               ' not compatible with 3-D Press. solver'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( implicitViscosity .AND. use3Dsolver ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &    'Implicit viscosity applies to provisional u,vVel'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** => not consistent with',
     &    ' final vertical shear (after appling 3-D solver solution'
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
        errCount = errCount + 1
       ENDIF
      ENDIF
#ifdef ALLOW_MOM_COMMON
      IF ( vectorInvariantMomentum .AND. momViscosity .AND.
     &     usingSphericalPolarGrid .AND. cosPower.NE.zeroRL ) THEN
#ifdef ISOTROPIC_COS_SCALING
       IF ( no_slip_sides ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: inconsistent ',
     &    'no_slip_sides & ISOTROPIC_COS_SCALING in mom_vecinv'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSE
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &    'ISOTROPIC_COS_SCALING not implemented in mom_vecinv'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
#endif /* ISOTROPIC_COS_SCALING */
#ifdef COSINEMETH_III
       IF ( useBiharmonicVisc .AND. no_slip_sides ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: inconsistent ',
     &    'no_slip_sides & COSINEMETH_III in mom_vecinv'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSEIF ( useBiharmonicVisc ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &    'COSINEMETH_III not implemented in mom_vecinv'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
#endif /* COSINEMETH_III */
      ENDIF
#endif /* ALLOW_MOM_COMMON */
      IF ( .NOT.vectorInvariantMomentum .AND. momAdvection ) THEN
       IF ( usingCurvilinearGrid ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &       'missing metric-terms for CurvilinearGrid'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( hasWetCSCorners ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: momAdvection ',
     &   'in flux-form is wrong on CubedSphere grid (corners)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
      IF ( selectCoriMap.LT.0 .OR. selectCoriMap.GT.3 ) THEN
        WRITE(msgBuf,'(2A,I4)') 'CONFIG_CHECK: ',
     &       'Invalid option: selectCoriMap=', selectCoriMap
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectBotDragQuadr.LT.-1 .OR. selectBotDragQuadr.GT.2 ) THEN
        WRITE(msgBuf,'(2A,I8,A)') 'CONFIG_CHECK: ',
     &       'selectBotDragQuadr=', selectBotDragQuadr,
     &       ' not valid (-1,0,1,2)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useSmag3D .AND.
     &    ( usingPCoords .OR. deepAtmosphere .OR. selectSigmaCoord.NE.0
     &                   .OR. rhoRefFile.NE.' ' .OR. hasWetCSCorners )
     &   ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: ',
     &       'Smag-3D not yet implemented for this set-up'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( momStepping .AND. .NOT.useCDscheme
     &                 .AND. (tauCD.NE.0. .OR. rCD.NE.-1.) ) THEN
C- jmc: since useCDscheme is a new [04-13-03] flag (default=F),
C       put this WARNING to stress that even if CD-scheme parameters
C       (tauCD,rCD) are set, CD-scheme is not used without useCDscheme=T
C-    and STOP if using mom_fluxform (following Chris advise).
C- jmc: but ultimately, this block can/will be removed.
       IF ( vectorInvariantMomentum ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: ',
     &   'CD-scheme is OFF but params(tauCD,rCD) are set'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3A)') '** WARNING ** to turn ON CD-scheme:',
     &   ' => "useCDscheme=.TRUE." in "data", namelist PARM01'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ELSE
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: CD-scheme is OFF but params(tauCD,rCD) are set'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: to turn ON CD-scheme: => "useCDscheme=.TRUE."',
     &   ' in "data", namelist PARM01'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

      IF ( useCDscheme .AND. hasWetCSCorners ) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: CD-scheme not implemented on CubedSphere grid'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useCDscheme .AND. momImplVertAdv ) THEN
        WRITE(msgBuf,'(2A)')'CONFIG_CHECK: ',
     &   'Implicit Vert. Advect. not coded for CD-scheme velocity'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useCDscheme .AND. selectImplicitDrag.GT.0 ) THEN
        WRITE(msgBuf,'(2A)')'CONFIG_CHECK: ',
     &   'Implicit (bottom) drag not coded for CD-scheme velocity'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--   Time-stepping limitations
      IF ( implicitIntGravWave .AND. staggerTimeStep ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: staggerTimeStep',
     &    ' incompatible with implicitIntGravWave'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( momForcingOutAB.NE.0 .AND. momForcingOutAB.NE.1 ) THEN
        WRITE(msgBuf,'(A,I10,A)') 'CONFIG_CHECK: momForcingOutAB=',
     &                             momForcingOutAB, ' not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: momForcingOutAB ',
     &                       'should be =1 (Out of AB) or =0 (In AB)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( tracForcingOutAB.NE.0 .AND. tracForcingOutAB.NE.1 ) THEN
        WRITE(msgBuf,'(A,I10,A)') 'CONFIG_CHECK: tracForcingOutAB=',
     &                             tracForcingOutAB, ' not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: tracForcingOutAB ',
     &                       'should be =1 (Out of AB) or =0 (In AB)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectImplicitDrag.LT.0 .OR. selectImplicitDrag.GT.2 ) THEN
        WRITE(msgBuf,'(2A,I4)') 'CONFIG_CHECK: ',
     &    'Invalid option: selectImplicitDrag=',  selectImplicitDrag
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectImplicitDrag.EQ.1 ) THEN
        WRITE(msgBuf,'(2A)') 'CONFIG_CHECK: selectImplicitDrag=1',
     &  ' not yet available'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--   Grid limitations:
      IF ( rotateGrid ) THEN
       IF ( .NOT. usingSphericalPolarGrid ) THEN
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: specifying Euler angles makes only ',
     &       'sense with usingSphericalGrid=.TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( useFLT .OR. useECCO ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CONFIG_CHECK: specifying',
     &       ' Euler angles may not work with pkgs FLT, ECCO'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF

C--   Packages conflict
      IF ( useMATRIX .AND. useGCHEM ) THEN
        WRITE(msgBuf,'(2A)')
     &   'CONFIG_CHECK: cannot set both: useMATRIX & useGCHEM'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( useMATRIX .AND. .NOT.usePTRACERS ) THEN
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: cannot set useMATRIX without ',
     &       'setting usePTRACERS'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( (useSEAICE .OR. useThSIce) .AND. allowFreezing ) THEN
        WRITE(msgBuf,'(2A)')
     &       'CONFIG_CHECK: cannot set allowFreezing',
     &       ' with pkgs SEAICE or THSICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'CONFIG_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)
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
      _END_MASTER(myThid)

      RETURN
      END
