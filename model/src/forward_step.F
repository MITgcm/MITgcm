C $Header: /u/gcmpack/MITgcm/model/src/forward_step.F,v 1.148 2007/04/16 23:31:59 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: FORWARD_STEP
C     !INTERFACE:
      SUBROUTINE FORWARD_STEP( iloop, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | SUBROUTINE forward_step
C     | o Run the ocean model and, optionally, evaluate a cost function.
C     *==================================================================
C     |
C     | THE_MAIN_LOOP is the toplevel routine for the Tangent Linear and
C     | Adjoint Model Compiler (TAMC). For this purpose the initialization
C     | of the model was split into two parts. Those parameters that do
C     | not depend on a specific model run are set in INITIALISE_FIXED,
C     | whereas those that do depend on the specific realization are
C     | initialized in INITIALISE_VARIA.
C     |
C     *==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE
#endif

#ifdef HAVE_SIGREG
#include "SIGREG.h"
#endif

#ifdef ALLOW_SHAP_FILT
# include "SHAP_FILT.h"
#endif
#ifdef ALLOW_ZONAL_FILT
# include "ZONAL_FILT.h"
#endif
#ifdef COMPONENT_MODULE
# include "CPL_PARAMS.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "FFIELDS.h"

# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"
# include "EOS.h"
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
#  include "GRID.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_FIELDS.h"
#  include "EXF_CLIM_FIELDS.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "EXF_CONSTANTS.h"
#  endif
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS.h"
#  ifdef ALLOW_PTRACERS
#   include "OBCS_PTRACERS.h"
#  endif
# endif
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_THSICE
#  include "THSICE_VARS.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef EXACT_CONSERV
#  include "SURFACE.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
# ifdef ALLOW_RBCS
#  include "RBCS.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     !LOCAL VARIABLES:
C     == Routine arguments ==
C     note: under the multi-threaded model myIter and
C           myTime are local variables passed around as routine
C           arguments. Although this is fiddly it saves the need to
C           impose additional synchronisation points when they are
C           updated.
C     myIter - iteration counter for this thread
C     myTime - time counter for this thread
C     myThid - thread number for this instance of the routine.
      INTEGER iloop
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

C     == Local variables ==
#ifdef COMPONENT_MODULE
      INTEGER myItP1
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_ENTER('FORWARD_STEP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
C--   Reset the model iteration counter and the model time.
      myIter = nIter0 + (iloop-1)
      myTime = startTime + float(iloop-1)*deltaTclock
#endif

#ifdef ALLOW_AUTODIFF_TAMC
c**************************************
#include "checkpoint_lev1_directives.h"
#include "checkpoint_lev1_template.h"
c**************************************
#endif

C--   Switch on/off diagnostics for snap-shot output:
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_SWITCH_ONOFF( myTime, myIter, myThid )
C--   State-variables diagnostics
        CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
        CALL DO_STATEVARS_DIAGS( myTime, 0, myIter, myThid )
        CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
      ENDIF
#endif

C--   Call driver to load external forcing fields from file
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     & CALL DEBUG_CALL('LOAD_FIELDS_DRIVER',myThid)
#endif
      CALL TIMER_START('LOAD_FIELDS_DRIVER  [FORWARD_STEP]',myThid)
      CALL LOAD_FIELDS_DRIVER( myTime, myIter, myThid )
      CALL TIMER_STOP ('LOAD_FIELDS_DRIVER  [FORWARD_STEP]',myThid)

C--   Call Bulk-Formulae forcing package
#ifdef ALLOW_BULK_FORCE
      IF ( useBulkForce ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_CALL('BULKF_FORCING',myThid)
#endif
        CALL TIMER_START('BULKF_FORCING       [FORWARD_STEP]',myThid)
C-    calculate qnet and empmr (and wind stress)
        CALL BULKF_FORCING( myTime, myIter, myThid )
        CALL TIMER_STOP ('BULKF_FORCING       [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_BULK_FORCE */

#ifdef ALLOW_AUTODIFF
c--   Add control vector for forcing and parameter fields
      IF ( myIter .EQ. nIter0 )
     &     CALL CTRL_MAP_FORCING (myThid)
#endif

#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_AUTODIFF_MONITOR))
      CALL DUMMY_IN_STEPPING( myTime, myIter, myThid )
#endif

#ifdef COMPONENT_MODULE
      IF ( useCoupler .AND. cpl_earlyExpImpCall ) THEN
C      Post coupling data that I export.
C      Read in coupling data that I import.
         CALL TIMER_START('CPL_EXPORT-IMPORT  [FORWARD_STEP]',myThid)
         CALL CPL_EXPORT_MY_DATA(       myIter, myTime, myThid )
         CALL CPL_IMPORT_EXTERNAL_DATA( myIter, myTime, myThid )
         CALL TIMER_STOP ('CPL_EXPORT-IMPORT  [FORWARD_STEP]',myThid)
      ENDIF
#endif /* COMPONENT_MODULE */

#ifdef ALLOW_EBM
      IF ( useEBM ) THEN
# ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('EBM',myThid)
# endif
         CALL TIMER_START('EBM                [FORWARD_STEP]',myThid)
         CALL EBM_DRIVER ( myTime, myIter, myThid )
         CALL TIMER_STOP ('EBM                [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_EBM */

C--     Step forward fields and calculate time tendency terms.

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     & CALL DEBUG_CALL('DO_ATMOSPHERIC_PHYS',myThid)
#endif
      CALL TIMER_START('DO_ATMOSPHERIC_PHYS [FORWARD_STEP]',myThid)
      CALL DO_ATMOSPHERIC_PHYS( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_ATMOSPHERIC_PHYS [FORWARD_STEP]',myThid)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE surfaceforcingtice    = comlev1, key = ikey_dynamics
# ifdef ALLOW_KPP
CADJ STORE uvel               = comlev1, key = ikey_dynamics
CADJ STORE vvel               = comlev1, key = ikey_dynamics
# endif
# ifdef EXACT_CONSERV
cphCADJ STORE empmr              = comlev1, key = ikey_dynamics
cphCADJ STORE pmepr              = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_PTRACERS
CADJ STORE ptracer               = comlev1, key = ikey_dynamics
# endif
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
cph-test
CADJ STORE hFacC                 = comlev1, key = ikey_dynamics
#  ifndef DISABLE_RSTAR_CODE
CADJ STORE rstarexpc             = comlev1, key = ikey_dynamics
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifndef ALLOW_AUTODIFF_TAMC
      IF ( .NOT. useOffLine ) THEN
#endif
#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('DO_OCEANIC_PHYS',myThid)
#endif
       CALL TIMER_START('DO_OCEANIC_PHYS     [FORWARD_STEP]',myThid)
       CALL DO_OCEANIC_PHYS( myTime, myIter, myThid )
       CALL TIMER_STOP ('DO_OCEANIC_PHYS     [FORWARD_STEP]',myThid)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE EmPmR    = comlev1, key = ikey_dynamics
# ifdef EXACT_CONSERV
CADJ STORE pmepr    = comlev1, key = ikey_dynamics
# endif
#else
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
cph-test
CADJ STORE hFac_surfC         = comlev1, key = ikey_dynamics
CADJ STORE hfac_surfs         = comlev1, key = ikey_dynamics
CADJ STORE hfac_surfw         = comlev1, key = ikey_dynamics
# endif
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE hFacC, hFacS, hFacW
CADJ &     = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC, recip_hFacS, recip_hFacW
CADJ &     = comlev1, key = ikey_dynamics
c
CADJ STORE surfaceforcingu = comlev1, key = ikey_dynamics
CADJ STORE surfaceforcingv = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_GCHEM
C     GCHEM package is an interface for any bio-geochemical or
C     ecosystem model you would like to include. 
C     If GCHEM_SEPARATE_FORCING is not defined, you are 
C     responsible for computing tendency terms for passive 
C     tracers and storing them on a 3DxNumPtracers-array called
C     gchemTendency in GCHEM_CALC_TENDENCY. This tendency is then added
C     to gPtr in ptracers_forcing later-on.
C     If GCHEM_SEPARATE_FORCING is defined, you are reponsible for
C     UPDATING ptracers directly in GCHEM_FORCING_SEP. This amounts
C     to a completely separate time step that you have to implement
C     yourself (Eulerian seems to be fine in most cases).
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CAVEAT: Up to now, when GCHEM is turned on the field ptracerForcingSurf,
C     which is needed for KPP is not set properly. ptracerForcingSurf must
C     be treated differently depending on whether GCHEM_SEPARATE_FORCING
C     is define or not. TBD.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF ( useGCHEM ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &       CALL DEBUG_CALL('GCHEM_CALC_TENDENCY',myThid)
#endif
        CALL TIMER_START('GCHEM_CALC_TENDENCY [FORWARD_STEP]',myThid)
        CALL GCHEM_CALC_TENDENCY( myTime, myIter, myThid )
        CALL TIMER_STOP ('GCHEM_CALC_TENDENCY [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_GCHEM */

#ifdef ALLOW_AUTODIFF_TAMC
cph needed to be moved here from do_oceanic_physics
cph to be visible down the road
c
CADJ STORE surfaceForcingS    = comlev1, key = ikey_dynamics
CADJ STORE surfaceForcingT    = comlev1, key = ikey_dynamics
CADJ STORE surfaceForcingTice = comlev1, key = ikey_dynamics
ctest(
CADJ STORE IVDConvCount       = comlev1, key = ikey_dynamics
ctest)
# ifdef ALLOW_PTRACERS
CADJ STORE surfaceForcingPtr  = comlev1, key = ikey_dynamics
# endif
c
# ifdef ALLOW_GMREDI
CADJ STORE Kwx                = comlev1, key = ikey_dynamics
CADJ STORE Kwy                = comlev1, key = ikey_dynamics
CADJ STORE Kwz                = comlev1, key = ikey_dynamics
#  ifdef GM_BOLUS_ADVEC
CADJ STORE GM_PsiX            = comlev1, key = ikey_dynamics
CADJ STORE GM_PsiY            = comlev1, key = ikey_dynamics
#  endif
# endif
c
# ifdef ALLOW_KPP
CADJ STORE KPPghat            = comlev1, key = ikey_dynamics
CADJ STORE KPPfrac            = comlev1, key = ikey_dynamics
CADJ STORE KPPdiffKzS         = comlev1, key = ikey_dynamics
CADJ STORE KPPdiffKzT         = comlev1, key = ikey_dynamics
# endif
c
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE etaH               = comlev1, key = ikey_dynamics
#  ifdef ALLOW_CD_CODE
CADJ STORE etanm1             = comlev1, key = ikey_dynamics
#  endif
#  ifndef DISABLE_RSTAR_CODE
cph-test
CADJ STORE rstarexpc = comlev1, key = ikey_dynamics
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

      IF ( .NOT.staggerTimeStep ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('THERMODYNAMICS      [FORWARD_STEP]',myThid)
        CALL THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('THERMODYNAMICS      [FORWARD_STEP]',myThid)
C--    if not staggerTimeStep: end
      ENDIF
c #ifdef ALLOW_NONHYDROSTATIC
      IF ( implicitIntGravWave ) THEN
        CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
        CALL DO_STAGGER_FIELDS_EXCHANGES( myTime, myIter, myThid )
        CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
      ENDIF
c #endif

#ifdef COMPONENT_MODULE
      IF ( useCoupler .AND. .NOT.cpl_earlyExpImpCall ) THEN
C      Post coupling data that I export.
C      Read in coupling data that I import.
         myItP1 = myIter + 1
         CALL TIMER_START('CPL_EXPORT-IMPORT  [FORWARD_STEP]',myThid)
         CALL CPL_EXPORT_MY_DATA(       myItP1, myTime, myThid )
         CALL CPL_IMPORT_EXTERNAL_DATA( myItP1, myTime, myThid )
         CALL TIMER_STOP ('CPL_EXPORT-IMPORT  [FORWARD_STEP]',myThid)
# ifdef ALLOW_OCN_COMPON_INTERF
        IF ( useRealFreshWaterFlux ) THEN
         CALL OCN_APPLY_IMPORT( .FALSE., myTime, myIter, myThid )
        ENDIF
# endif /* ALLOW_OCN_COMPON_INTERF */
      ENDIF
#endif /* COMPONENT_MODULE */

#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE hFacC          = comlev1, key = ikey_dynamics
CADJ STORE hFacS          = comlev1, key = ikey_dynamics
CADJ STORE hFacW          = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacS    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacW    = comlev1, key = ikey_dynamics
CADJ STORE etaN           = comlev1, key = ikey_dynamics
c
#  ifndef DISABLE_RSTAR_CODE
CADJ STORE rstarFacC    = comlev1, key = ikey_dynamics
CADJ STORE rstarFacS    = comlev1, key = ikey_dynamics
CADJ STORE rstarFacW    = comlev1, key = ikey_dynamics
c
CADJ STORE h0facc,h0facs,h0facw  
CADJ &     = comlev1, key = ikey_dynamics
CADJ STORE rstardhcdt,rstardhsdt,rstardhwdt 
CADJ &     = comlev1, key = ikey_dynamics
CADJ STORE rstarexpc,rstarexps,rstarexpw 
CADJ &     = comlev1, key = ikey_dynamics
#  endif
# endif
#endif
C--   Step forward fields and calculate time tendency terms.
#ifndef ALLOW_AUTODIFF_TAMC
      IF ( momStepping ) THEN
#endif
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('DYNAMICS',myThid)
#endif
        CALL TIMER_START('DYNAMICS            [FORWARD_STEP]',myThid)
        CALL DYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('DYNAMICS            [FORWARD_STEP]',myThid)
#ifndef ALLOW_AUTODIFF_TAMC
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
cph-test
CADJ STORE gU, gV  = comlev1, key = ikey_dynamics
# endif
#endif

C--   Update time-counter
      myIter = nIter0 + iLoop
      myTime = startTime + deltaTClock * float(iLoop)

#ifdef ALLOW_MNC
C     Update the default next iter for MNC
      IF ( useMNC ) THEN
         CALL MNC_CW_CITER_SETG( 1, 1, -1, myIter , myThid )

C        TODO: Logic should be added here so that users can specify, on
C        a per-citer-group basis, when it is time to update the
C        "current" (and not just the "next") iteration

C        TODO: the following is just a temporary band-aid (mostly, for
C        Baylor) until someone writes a routine that better handles time
C        boundaries such as weeks, months, years, etc.
         IF ( mnc_filefreq .GT. 0 ) THEN
           IF (DIFFERENT_MULTIPLE(mnc_filefreq,myTime,deltaTClock))
     &          THEN
             CALL MNC_CW_CITER_SETG( 1, 1, myIter, -1 , myThid )
           ENDIF
         ENDIF
      ENDIF
#endif /* ALLOW_MNC */

C--   Update geometric factors:
#ifdef NONLIN_FRSURF
C-    update hfacC,W,S and recip_hFac according to etaH(n+1) :
      IF ( nonlinFreeSurf.GT.0) THEN
       IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
# ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE hFacC    = comlev1, key = ikey_dynamics
CADJ STORE hFacS    = comlev1, key = ikey_dynamics
CADJ STORE hFacW    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacS    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacW    = comlev1, key = ikey_dynamics
c
CADJ STORE rstarFacC    = comlev1, key = ikey_dynamics
CADJ STORE rstarFacS    = comlev1, key = ikey_dynamics
CADJ STORE rstarFacW    = comlev1, key = ikey_dynamics
c
CADJ STORE h0facc,h0facs,h0facw  = comlev1, key = ikey_dynamics
# endif
        CALL TIMER_START('UPDATE_R_STAR       [FORWARD_STEP]',myThid)
        CALL UPDATE_R_STAR( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_R_STAR       [FORWARD_STEP]',myThid)
# ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE hFacC    = comlev1, key = ikey_dynamics
CADJ STORE hFacS    = comlev1, key = ikey_dynamics
CADJ STORE hFacW    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacS    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacW    = comlev1, key = ikey_dynamics
# endif
# endif /* DISABLE_RSTAR_CODE */
       ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hFac_surfC, hFac_surfS, hFac_surfW 
CADJ &     = comlev1, key = ikey_dynamics
#endif
        CALL TIMER_START('UPDATE_SURF_DR      [FORWARD_STEP]',myThid)
        CALL UPDATE_SURF_DR( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_SURF_DR      [FORWARD_STEP]',myThid)
       ENDIF
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE hFacC    = comlev1, key = ikey_dynamics
CADJ STORE hFacS    = comlev1, key = ikey_dynamics
CADJ STORE hFacW    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacC    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacS    = comlev1, key = ikey_dynamics
CADJ STORE recip_hFacW    = comlev1, key = ikey_dynamics
# endif
C-    update also CG2D matrix (and preconditioner)
      IF ( momStepping .AND. nonlinFreeSurf.GT.2 ) THEN
        CALL TIMER_START('UPDATE_CG2D         [FORWARD_STEP]',myThid)
        CALL UPDATE_CG2D( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_CG2D         [FORWARD_STEP]',myThid)
      ENDIF
#endif /* NONLIN_FRSURF */

C--   Apply Filters to u*,v* before SOLVE_FOR_PRESSURE
#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT .AND. shap_filt_uvStar) THEN
        CALL TIMER_START('SHAP_FILT_UV        [FORWARD_STEP]',myThid)
        IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
          CALL SHAP_FILT_APPLY_UV( uVel,vVel,
     &                             myTime, myIter, myThid )
        ENDIF
        CALL SHAP_FILT_APPLY_UV( gU,gV,myTime,myIter,myThid)
        CALL TIMER_STOP ('SHAP_FILT_UV        [FORWARD_STEP]',myThid)
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT .AND. zonal_filt_uvStar) THEN
        CALL TIMER_START('ZONAL_FILT_UV       [FORWARD_STEP]',myThid)
        IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
          CALL ZONAL_FILT_APPLY_UV( uVel, vVel, myThid )
        ENDIF
        CALL ZONAL_FILT_APPLY_UV( gU, gV, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_UV       [FORWARD_STEP]',myThid)
      ENDIF
#endif

C--   Solve elliptic equation(s).
C     Two-dimensional only for conventional hydrostatic or
C     three-dimensional for non-hydrostatic and/or IGW scheme.
      IF ( momStepping ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE uvel, vvel
CADJ &     = comlev1, key = ikey_dynamics
CADJ STORE empmr,hfacs,hfacw
CADJ &     = comlev1, key = ikey_dynamics
# endif
#endif
        CALL TIMER_START('SOLVE_FOR_PRESSURE  [FORWARD_STEP]',myThid)
        CALL SOLVE_FOR_PRESSURE(myTime, myIter, myThid)
        CALL TIMER_STOP ('SOLVE_FOR_PRESSURE  [FORWARD_STEP]',myThid)
      ENDIF

C--   Correct divergence in flow field and cycle time-stepping momentum
#ifndef ALLOW_AUTODIFF_TAMC
      IF ( momStepping ) THEN
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
#  ifndef DISABLE_RSTAR_CODE
cph-test
cph not clear, why this one
CADJ STORE h0facc = comlev1, key = ikey_dynamics
#  endif
# endif
# ifdef ALLOW_DEPTH_CONTROL
CADJ STORE etan, uvel,vvel
CADJ &     = comlev1, key = ikey_dynamics
# endif
#endif
        CALL TIMER_START('MOM_CORRECTION_STEP [FORWARD_STEP]',myThid)
        CALL MOMENTUM_CORRECTION_STEP(myTime, myIter, myThid)
        CALL TIMER_STOP ('MOM_CORRECTION_STEP [FORWARD_STEP]',myThid)
#ifndef ALLOW_AUTODIFF_TAMC
      ENDIF
#endif

#ifdef EXACT_CONSERV
      IF (exactConserv) THEN
C--   Update etaH(n+1) :
        CALL TIMER_START('UPDATE_ETAH         [FORWARD_STEP]',myThid)
        CALL UPDATE_ETAH( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_ETAH         [FORWARD_STEP]',myThid)
      ENDIF
#endif /* EXACT_CONSERV */

#ifdef NONLIN_FRSURF
      IF ( select_rStar.NE.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
#  ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE rstarfacc,rstarfacs,rstarfacw = comlev1, key = ikey_dynamics
#  endif
C--   r* : compute the future level thickness according to etaH(n+1)
        CALL TIMER_START('CALC_R_STAR         [FORWARD_STEP]',myThid)
        CALL CALC_R_STAR(etaH, myTime, myIter, myThid )
        CALL TIMER_STOP ('CALC_R_STAR         [FORWARD_STEP]',myThid)
# endif /* DISABLE_RSTAR_CODE */
      ELSEIF ( nonlinFreeSurf.GT.0) THEN
C--   compute the future surface level thickness according to etaH(n+1)
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE etaH          = comlev1, key = ikey_dynamics
# endif
        CALL TIMER_START('CALC_SURF_DR      [FORWARD_STEP]',myThid)
        CALL CALC_SURF_DR(etaH, myTime, myIter, myThid )
        CALL TIMER_STOP ('CALC_SURF_DR      [FORWARD_STEP]',myThid)
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
cph-test
CADJ STORE hFac_surfC    = comlev1, key = ikey_dynamics
# endif
#endif /* NONLIN_FRSURF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( staggerTimeStep ) THEN
C--   do exchanges of U,V (needed for multiDim) when using stagger time-step :
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_CALL('DO_STAGGER_FIELDS_EXCH.',myThid)
#endif
        CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
        CALL DO_STAGGER_FIELDS_EXCHANGES( myTime, myIter, myThid )
        CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

#ifdef ALLOW_DIAGNOSTICS
C--   State-variables diagnostics
        IF ( useDiagnostics ) THEN
          CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
          CALL DO_STATEVARS_DIAGS( myTime, 1, myIter, myThid )
          CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
        ENDIF
#endif

#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_CALL('THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('THERMODYNAMICS      [FORWARD_STEP]',myThid)
        CALL THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('THERMODYNAMICS      [FORWARD_STEP]',myThid)

C--    if staggerTimeStep: end
      ENDIF
C---+--------+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
cph This is needed because convective_adjustment calls
cph find_rho which may use pressure()
CADJ STORE totphihyd  = comlev1, key = ikey_dynamics
#endif
C--   Cycle time-stepping Tracers arrays (T,S,+pTracers)
      CALL TIMER_START('TRC_CORRECTION_STEP [FORWARD_STEP]',myThid)
      CALL TRACERS_CORRECTION_STEP(myTime, myIter, myThid)
      CALL TIMER_STOP ('TRC_CORRECTION_STEP [FORWARD_STEP]',myThid)

#ifdef ALLOW_GCHEM
C     Add separate timestepping of chemical/biological/forcing
C     of ptracers here in GCHEM_FORCING_SEP
      IF ( useGCHEM ) THEN
#ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('GCHEM_FORCING_SEP',myThid)
#endif /* ALLOW_DEBUG */
         CALL TIMER_START('GCHEM_FORCING_SEP  [FORWARD_STEP]',myThid)
         CALL GCHEM_FORCING_SEP( myTime,myIter,myThid )
         CALL TIMER_STOP ('GCHEM_FORCING_SEP  [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_GCHEM */

C--   Do "blocking" sends and receives for tendency "overlap" terms
c     CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
c     CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )
c     CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

C--   Do "blocking" sends and receives for field "overlap" terms
      CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
      CALL DO_FIELDS_BLOCKING_EXCHANGES( myThid )
      CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
       CALL DO_STATEVARS_DIAGS( myTime, 2, myIter, myThid )
       CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_GRIDALT
      IF (useGRIDALT) THEN
         CALL GRIDALT_UPDATE(myThid)
      ENDIF
#endif

#ifdef ALLOW_FIZHI
      IF (useFIZHI) THEN
         CALL TIMER_START('FIZHI               [FORWARD_STEP]',myThid)
         CALL STEP_FIZHI_CORR ( myTime, myIter, myThid, dTtracerLev(1) )
         CALL TIMER_STOP ('FIZHI               [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_FLT
C--   Calculate float trajectories
      IF (useFLT) THEN
        CALL TIMER_START('FLOATS            [FORWARD_STEP]',myThid)
        CALL FLT_MAIN(myIter,myTime, myThid)
        CALL TIMER_STOP ('FLOATS            [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_TIMEAVE
C--   State-variables time-averaging
      CALL TIMER_START('DO_STATEVARS_TAVE   [FORWARD_STEP]',myThid)
      CALL DO_STATEVARS_TAVE( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_STATEVARS_TAVE   [FORWARD_STEP]',myThid)
#endif

#ifdef ALLOW_MONITOR
      IF ( .NOT.useOffLine ) THEN
C--   Check status of solution (statistics, cfl, etc...)
        CALL TIMER_START('MONITOR             [FORWARD_STEP]',myThid)
        CALL MONITOR( myIter, myTime, myThid )
        CALL TIMER_STOP ('MONITOR             [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_MONITOR */

#ifdef ALLOW_COST
C--     compare model with data and compute cost function
C--     this is done after exchanges to allow interpolation
      CALL TIMER_START('COST_TILE           [FORWARD_STEP]',myThid)
      CALL COST_TILE  ( myTime, myIter, myThid )
      CALL TIMER_STOP ('COST_TILE           [FORWARD_STEP]',myThid)
#endif

C--   Do IO if needed.
      CALL TIMER_START('DO_THE_MODEL_IO     [FORWARD_STEP]',myThid)
      CALL DO_THE_MODEL_IO( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_THE_MODEL_IO     [FORWARD_STEP]',myThid)

#ifdef HAVE_SIGREG
      IF ( useSIGREG ) THEN
        IF ( i_got_signal .GT. 0 ) THEN
          CALL PACKAGES_WRITE_PICKUP(
     I         .TRUE., myTime, myIter, myThid )
          CALL WRITE_PICKUP(
     I         .TRUE., myTime, myIter, myThid )
          STOP 'Checkpoint completed -- killed by signal handler'
        ENDIF
      ENDIF
#endif /* HAVE_SIGREG */

C--   Save state for restarts
      CALL TIMER_START('DO_WRITE_PICKUP     [FORWARD_STEP]',myThid)
      CALL DO_WRITE_PICKUP(
     I               .FALSE., myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_WRITE_PICKUP     [FORWARD_STEP]',myThid)

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_LEAVE('FORWARD_STEP',myThid)
#endif

      RETURN
      END
