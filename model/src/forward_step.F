C $Header: /u/gcmpack/MITgcm/model/src/forward_step.F,v 1.85 2004/03/03 20:42:05 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

cswdptr -- add --
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif
cswdptr -- end add ---

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
#include "FFIELDS.h"

#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif

#ifdef ALLOW_SHAP_FILT
#include "SHAP_FILT.h"
#endif
#ifdef ALLOW_ZONAL_FILT
#include "ZONAL_FILT.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"
# include "EOS.h"
# ifdef ALLOW_EXF
#  include "exf_fields.h"
#  include "exf_clim_fields.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "exf_constants.h"
#  endif
# endif
# ifdef ALLOW_OBCS
#  include "OBCS.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS.h"
# endif
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     !LOCAL VARIABLES:
C     == Routine arguments ==
C     note: under the multi-threaded model myiter and 
C           mytime are local variables passed around as routine 
C           arguments. Although this is fiddly it saves the need to 
C           impose additional synchronisation points when they are 
C           updated.
C     myiter - iteration counter for this thread
C     mytime - time counter for this thread
C     mythid - thread number for this instance of the routine.
      integer iloop
      integer mythid 
      integer myiter
      _RL     mytime

      integer i,L
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_ENTER('FORWARD_STEP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
C--   Reset the model iteration counter and the model time.
      myiter = nIter0 + (iloop-1)
      mytime = startTime + float(iloop-1)*deltaTclock
#endif

#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_AUTODIFF_MONITOR))
C     Include call to a dummy routine. Its adjoint will be 
C     called at the proper place in the adjoint code.
C     The adjoint routine will print out adjoint values 
C     if requested. The location of the call is important, 
C     it has to be after the adjoint of the exchanges 
C     (DO_GTERM_BLOCKING_EXCHANGES).
      CALL DUMMY_IN_STEPPING( myTime, myIter, myThid )
cph   I've commented this line since it may conflict with MITgcm's adjoint
cph   However, need to check whether that's still consistent
cph   with the ecco-branch (it should).
cph      CALL DO_FIELDS_BLOCKING_EXCHANGES( myThid )
#endif

#ifdef EXACT_CONSERV
      IF (exactConserv) THEN
C--   Update etaH(n+1) :
         CALL TIMER_START('UPDATE_ETAH        [FORWARD_STEP]',mythid)
         CALL UPDATE_ETAH( myTime, myIter, myThid )
         CALL TIMER_STOP ('UPDATE_ETAH        [FORWARD_STEP]',mythid)
      ENDIF
#endif /* EXACT_CONSERV */ 

#ifdef NONLIN_FRSURF
      IF ( select_rStar.NE.0 ) THEN
C--   r* : compute the future level thickness according to etaH(n+1) 
          CALL TIMER_START('CALC_R_STAR       [FORWARD_STEP]',mythid)
          CALL CALC_R_STAR(etaH, myTime, myIter, myThid )
          CALL TIMER_STOP ('CALC_R_STAR       [FORWARD_STEP]',mythid)
      ELSEIF ( nonlinFreeSurf.GT.0) THEN
C--   compute the future surface level thickness according to etaH(n+1) 
          CALL TIMER_START('CALC_SURF_DR      [FORWARD_STEP]',mythid)
          CALL CALC_SURF_DR(etaH, myTime, myIter, myThid )
          CALL TIMER_STOP ('CALC_SURF_DR      [FORWARD_STEP]',mythid)
      ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_AUTODIFF_TAMC
c**************************************
#include "checkpoint_lev1_directives.h"
c**************************************
#endif

C--   Call external forcing package
#ifdef ALLOW_BULK_FORCE
      IF ( useBulkForce ) THEN
#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('BULKF_FIELDS_LOAD',myThid)
#endif
       CALL TIMER_START('BULKF_FORCING      [FORWARD_STEP]',mythid)
C-    load all forcing fields at current time
       CALL BULKF_FIELDS_LOAD( myTime, myIter, myThid )
C-    calculate qnet and empmr (and wind stress)
       CALL BULKF_FORCING( myTime, myIter, myThid )
       CALL TIMER_STOP ('BULKF_FORCING      [FORWARD_STEP]',mythid)
      ELSE
#endif /* ALLOW_BULK_FORCE */

# ifdef ALLOW_EXF
#  ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('EXF_GETFORCING',myThid)
#  endif
      CALL TIMER_START('EXF_GETFORCING     [FORWARD_STEP]',mythid)
      CALL EXF_GETFORCING( mytime, myiter, mythid )
      CALL TIMER_STOP ('EXF_GETFORCING     [FORWARD_STEP]',mythid)
# else /* ALLOW_EXF undef */
cph The following IF-statement creates an additional dependency
cph for the forcing fields requiring additional storing.
cph Therefore, the IF-statement will be put between CPP-OPTIONS,
cph assuming that ALLOW_SEAICE has not yet been differentiated.
#  ifdef ALLOW_SEAICE
      IF ( .NOT. useSEAICE ) THEN
#  endif
#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('EXTERNAL_FIELDS_LOAD',myThid)
#endif
       CALL TIMER_START('EXTERNAL_FIELDS_LOAD[FORWARD_STEP]',mythid)
       CALL EXTERNAL_FIELDS_LOAD( mytime, myiter, mythid )
       CALL TIMER_STOP ('EXTERNAL_FIELDS_LOAD[FORWARD_STEP]',mythid)
#  ifdef ALLOW_SEAICE
      ENDIF
#  endif
# endif /* ALLOW_EXF */
#ifdef ALLOW_BULK_FORCE
C--   end of if/else block useBulfforce --
      ENDIF
#endif /* ALLOW_BULK_FORCE */

#ifdef ALLOW_AUTODIFF
c--   Add control vector for forcing and parameter fields
      if ( myiter .EQ. nIter0 )
     &     CALL CTRL_MAP_FORCING (mythid)
#endif

#ifdef ALLOW_THSICE
      IF (useThSIce) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL('THSICE_MAIN',myThid)
#endif
C--     Step forward Therm.Sea-Ice variables
C       and modify forcing terms including effects from ice
        CALL TIMER_START('THSICE_MAIN        [FORWARD_STEP]', myThid)
        CALL THSICE_MAIN( myTime, myIter, myThid )
        CALL TIMER_STOP( 'THSICE_MAIN        [FORWARD_STEP]', myThid)
      ENDIF
#endif /* ALLOW_THSICE */

# ifdef ALLOW_SEAICE
C--   Call sea ice model to compute forcing/external data fields.  In
C     addition to computing prognostic sea-ice variables and diagnosing the
C     forcing/external data fields that drive the ocean model, SEAICE_MODEL
C     also sets theta to the freezing point under sea-ice.  The implied
C     surface heat flux is then stored in variable surfaceTendencyTice,
C     which is needed by KPP package (kpp_calc.F and kpp_transport_t.F)
C     to diagnose surface buoyancy fluxes and for the non-local transport
C     term.  Because this call precedes model thermodynamics, temperature
C     under sea-ice may not be "exactly" at the freezing point by the time
C     theta is dumped or time-averaged.
      IF ( useSEAICE ) THEN
#ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('SEAICE_MODEL',myThid)
#endif
         CALL TIMER_START('SEAICE_MODEL       [FORWARD_STEP]',myThid)
         CALL SEAICE_MODEL( myTime, myIter, myThid )
         CALL TIMER_STOP ('SEAICE_MODEL       [FORWARD_STEP]',myThid)
      ENDIF
# endif /* ALLOW_SEAICE */

C--   Freeze water at the surface
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta = comlev1, key = ikey_dynamics
#endif
      IF ( allowFreezing .AND. .NOT. useSEAICE
     &                   .AND. .NOT. useThSIce ) THEN
        CALL FREEZE_SURFACE(  myTime, myIter, myThid )
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_PTRACERS
cph this replaces _bibj storing of ptracer within thermodynamics
CADJ STORE ptracer  = comlev1, key = ikey_dynamics
# endif
#endif

#ifdef ALLOW_PTRACERS
# ifdef ALLOW_GCHEM
         CALL GCHEM_FIELDS_LOAD( mytime, myiter, mythid )
# endif
#endif

#ifdef COMPONENT_MODULE
       IF ( useCoupler ) THEN
C      Post coupling data that I export.
C      Read in coupling data that I import.
         CALL TIMER_START('CPL_EXPORT-IMPORT  [FORWARD_STEP]',myThid)
         CALL CPL_EXPORT_MY_DATA(       myIter, myTime, myThid )
         CALL CPL_IMPORT_EXTERNAL_DATA( myIter, myTime, myThid )
         CALL TIMER_STOP ('CPL_EXPORT-IMPORT  [FORWARD_STEP]',myThid)
       ENDIF
#endif /* COMPONENT_MODULE */

#ifdef COMPONENT_MODULE
# ifndef ALLOW_AIM
C jmc: don't know precisely where to put this call. leave it here for now.
       IF ( useCoupler ) THEN
         CALL OCN_APPLY_IMPORT( myTime, myIter, myThid )
       ENDIF
# endif
#endif /* COMPONENT_MODULE */

C--     Step forward fields and calculate time tendency terms.

C AMM
#ifdef ALLOW_FIZHI
        if( useFIZHI) then
         CALL UPDATE_OCEAN_EXPORTS ( myTime, myIter, myThid )
         CALL UPDATE_EARTH_EXPORTS ( myTime, myIter, myThid )
         CALL UPDATE_CHEMISTRY_EXPORTS ( myTime, myIter, myThid )
         CALL TIMER_START('FIZHI_WRAPPER       [FORWARD_STEP]',mythid)
         CALL FIZHI_WRAPPER ( myTime, myIter, myThid )
         CALL TIMER_STOP ('FIZHI_WRAPPER       [FORWARD_STEP]',mythid)
         CALL STEP_FIZHI_FG ( myTime, myIter, myThid, deltaTtracer )
        endif
#endif
C AMM

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('THERMODYNAMICS      [FORWARD_STEP]',mythid)
        CALL THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('THERMODYNAMICS      [FORWARD_STEP]',mythid)

C--   do exchanges (needed for DYNAMICS) when using stagger time-step :
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('DO_STAGGER_FIELDS_EXCH.',myThid)
#endif
        CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
        CALL DO_STAGGER_FIELDS_EXCHANGES( myTime, myIter, myThid )
        CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT .AND. 
     &     staggerTimeStep .AND. shap_filt_TrStagg ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('SHAP_FILT_APPLY_TS',myThid)
#endif
        CALL TIMER_START('SHAP_FILT           [FORWARD_STEP]',myThid)
        CALL SHAP_FILT_APPLY_TS(gT,gS,myTime+deltaT,myIter+1,myThid)
        CALL TIMER_STOP ('SHAP_FILT           [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT .AND. 
     &     staggerTimeStep .AND. zonal_filt_TrStagg ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('ZONAL_FILT_APPLY_TS',myThid)
#endif
        CALL TIMER_START('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
        CALL ZONAL_FILT_APPLY_TS( gT, gS, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
      ENDIF
#endif   

C--   Step forward fields and calculate time tendency terms.
#ifndef ALLOW_AUTODIFF_TAMC
      IF ( momStepping ) THEN
#endif
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('DYNAMICS',myThid)
#endif
        CALL TIMER_START('DYNAMICS            [FORWARD_STEP]',mythid)
        CALL DYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('DYNAMICS            [FORWARD_STEP]',mythid)
#ifndef ALLOW_AUTODIFF_TAMC
      ENDIF
#endif

#ifdef ALLOW_NONHYDROSTATIC
C--   Step forward W field in N-H algorithm
      IF ( momStepping .AND. nonHydrostatic ) THEN
#ifdef ALLOW_DEBUG
          IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('CALC_GW',myThid)
#endif
         CALL TIMER_START('CALC_GW          [FORWARD_STEP]',myThid)
         CALL CALC_GW(myThid)
         CALL TIMER_STOP ('CALC_GW          [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef NONLIN_FRSURF
C--   update hfacC,W,S and recip_hFac according to etaH(n+1) :
      IF ( nonlinFreeSurf.GT.0) THEN
       IF ( select_rStar.GT.0 ) THEN
        CALL TIMER_START('UPDATE_R_STAR      [FORWARD_STEP]',myThid)
        CALL UPDATE_R_STAR( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_R_STAR      [FORWARD_STEP]',myThid)
       ELSE
        CALL TIMER_START('UPDATE_SURF_DR     [FORWARD_STEP]',myThid)
        CALL UPDATE_SURF_DR( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_SURF_DR     [FORWARD_STEP]',myThid)
       ENDIF
      ENDIF
C-    update also CG2D matrix (and preconditioner)
      IF ( momStepping .AND. nonlinFreeSurf.GT.2 ) THEN
        CALL TIMER_START('UPDATE_CG2D        [FORWARD_STEP]',myThid)
        CALL UPDATE_CG2D( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_CG2D        [FORWARD_STEP]',myThid)
      ENDIF
#endif

C--   Apply Filters to u*,v* before SOLVE_FOR_PRESSURE
#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT .AND. shap_filt_uvStar) THEN
        CALL TIMER_START('SHAP_FILT           [FORWARD_STEP]',myThid)
        IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
          CALL SHAP_FILT_APPLY_UV( uVel,vVel,
     &                             myTime+deltaT, myIter+1, myThid )
        ENDIF
        CALL SHAP_FILT_APPLY_UV( gU,gV,myTime+deltaT,myIter+1,myThid)
        CALL TIMER_STOP ('SHAP_FILT           [FORWARD_STEP]',myThid)
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT .AND. zonal_filt_uvStar) THEN
        CALL TIMER_START('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
        IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
          CALL ZONAL_FILT_APPLY_UV( uVel, vVel, myThid )
        ENDIF
        CALL ZONAL_FILT_APPLY_UV( gU, gV, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
      ENDIF
#endif  

C--   Solve elliptic equation(s).
C     Two-dimensional only for conventional hydrostatic or 
C     three-dimensional for non-hydrostatic and/or IGW scheme.
      IF ( momStepping ) THEN
      CALL TIMER_START('SOLVE_FOR_PRESSURE  [FORWARD_STEP]',myThid)
      CALL SOLVE_FOR_PRESSURE(myTime, myIter, myThid)
      CALL TIMER_STOP ('SOLVE_FOR_PRESSURE  [FORWARD_STEP]',myThid)
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
cph This is needed because convective_adjustment calls
cph find_rho which may use pressure()
CADJ STORE totphihyd  = comlev1, key = ikey_dynamics
#endif
C--   Correct divergence in flow field and cycle time-stepping
C     arrays (for all fields) ; update time-counter
      myIter = nIter0 + iLoop
      myTime = startTime + deltaTClock * float(iLoop)
      CALL TIMER_START('THE_CORRECTION_STEP [FORWARD_STEP]',myThid)
      CALL THE_CORRECTION_STEP(myTime, myIter, myThid)
      CALL TIMER_STOP ('THE_CORRECTION_STEP [FORWARD_STEP]',myThid)

C--   Do "blocking" sends and receives for tendency "overlap" terms
c     CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
c     CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )
c     CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

C--   Do "blocking" sends and receives for field "overlap" terms
      CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
      CALL DO_FIELDS_BLOCKING_EXCHANGES( myThid )
      CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

cswdptr -- add for seperate timestepping of chemical/biological/forcing
cswdptr    of ptracers ---
#ifdef ALLOW_GCHEM
ceh3 This is broken -- this ifdef should not be visible!
#ifdef PTRACERS_SEPARATE_FORCING
ceh3 needs an IF ( use GCHEM ) THEN
        call GCHEM_FORCING_SEP( myTime,myIter,myThid )
#endif /* PTRACERS_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */
cswdptr -- end add ---

C AMM
#ifdef ALLOW_GRIDALT
        if (useGRIDALT) CALL GRIDALT_UPDATE(myThid)
#endif
C AMM

C AMM
#ifdef ALLOW_FIZHI
        CALL STEP_FIZHI_CORR ( myTime, myIter, myThid )
#endif
C AMM

#ifdef ALLOW_FLT
C--   Calculate float trajectories
      IF (useFLT) THEN
        CALL TIMER_START('FLOATS            [FORWARD_STEP]',myThid)
        CALL FLT_MAIN(myIter,myTime, myThid)
        CALL TIMER_STOP ('FLOATS            [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_MONITOR
C--   Check status of solution (statistics, cfl, etc...)
      CALL TIMER_START('MONITOR             [FORWARD_STEP]',myThid)
      CALL MONITOR( myIter, myTime, myThid )
      CALL TIMER_STOP ('MONITOR             [FORWARD_STEP]',myThid)
#endif /* ALLOW_MONITOR */

C--   Do IO if needed.
      CALL TIMER_START('DO_THE_MODEL_IO     [FORWARD_STEP]',myThid)
      CALL DO_THE_MODEL_IO( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_THE_MODEL_IO     [FORWARD_STEP]',myThid)

C--   Save state for restarts
C     Note:    (jmc: is it still the case after ckp35 ?)
C     =====
C     Because of the ordering of the timestepping code and
C     tendency term code at end of loop model arrays hold
C     U,V,T,S  at "time-level" N but gu, gv, gs, gt, guNM1,... 
C     at "time-level" N+1/2 (guNM1 at "time-level" N+1/2 is 
C     gu at "time-level" N-1/2) and etaN at "time-level" N+1/2.
C      where N = I+timeLevBase-1
C     Thus a checkpoint contains U.0000000000, GU.0000000001 and 
C     etaN.0000000001 in the indexing scheme used for the model 
C     "state" files. This example is referred to as a checkpoint 
C     at time level 1 
      CALL TIMER_START('WRITE_CHECKPOINT    [FORWARD_STEP]',myThid)
      CALL PACKAGES_WRITE_PICKUP(
     I               .FALSE., myTime, myIter, myThid )
      CALL WRITE_CHECKPOINT(
     I               .FALSE., myTime, myIter, myThid )   
      CALL TIMER_STOP ('WRITE_CHECKPOINT    [FORWARD_STEP]',myThid)

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_LEAVE('FORWARD_STEP',myThid)
#endif

      RETURN
      END
