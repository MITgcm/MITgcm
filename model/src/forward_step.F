C $Header: /u/gcmpack/MITgcm/model/src/forward_step.F,v 1.37 2002/11/15 03:01:21 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
# ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
#  include "exf_fields.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "exf_constants.h"
#  endif
# endif
# ifdef ALLOW_OBCS
#  include "OBCS.h"
# endif
#endif

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
      INTEGER bi,bj

CEOP


#ifdef ALLOW_AUTODIFF_TAMC
C--     Reset the model iteration counter and the model time.
        myiter = nIter0 + (iloop-1)
        mytime = startTime + float(iloop-1)*deltaTclock
#endif

#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_AUTODIFF_MONITOR))
C       Include call to a dummy routine. Its adjoint will be 
C       called at the proper place in the adjoint code.
C       The adjoint routine will print out adjoint values 
C       if requested. The location of the call is important, 
C       it has to be after the adjoint of the exchanges 
C       (DO_GTERM_BLOCKING_EXCHANGES).
        CALL DUMMY_IN_STEPPING( myTime, myIter, myThid )
        CALL DO_FIELDS_BLOCKING_EXCHANGES( myThid )
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
C--   compute the future surface level thickness
C      according to etaH(n+1) 
        IF ( nonlinFreeSurf.GT.0) THEN
          CALL TIMER_START('CALC_SURF_DR      [FORWARD_STEP]',mythid)
          CALL CALC_SURF_DR(etaH, myTime, myIter, myThid )
          CALL TIMER_STOP ('CALC_SURF_DR      [FORWARD_STEP]',mythid)
        ENDIF
#endif /* NONLIN_FRSURF */

C--     Load forcing/external data fields.
#ifdef ALLOW_AUTODIFF_TAMC
c**************************************
#include "checkpoint_lev1_directives.h"
c**************************************
#endif
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
C--   Call external forcing package
        IF ( .not. useSEAICE ) THEN 
         CALL TIMER_START('EXF_GETFORCING     [FORWARD_STEP]',mythid)
         CALL EXF_GETFORCING( mytime, myiter, mythid )
         CALL TIMER_STOP ('EXF_GETFORCING     [FORWARD_STEP]',mythid)
        ENDIF
#else
        IF ( .not. useSEAICE ) THEN 
         CALL TIMER_START('EXTERNAL_FIELDS_LOAD[FORWARD_STEP]',mythid)
         CALL EXTERNAL_FIELDS_LOAD( mytime, myiter, mythid )
         CALL TIMER_STOP ('EXTERNAL_FIELDS_LOAD[FORWARD_STEP]',mythid)
        ENDIF
#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */

#ifdef ALLOW_SEAICE
C--   Call sea ice model to compute forcing/external data fields.
        IF ( useSEAICE ) THEN
         CALL TIMER_START('SEAICE_MODEL       [FORWARD_STEP]',myThid)
         CALL SEAICE_MODEL( myTime, myIter, myThid )
         CALL TIMER_STOP ('SEAICE_MODEL       [FORWARD_STEP]',myThid)
        ENDIF
#endif ALLOW_SEAICE

C--     Step forward fields and calculate time tendency terms.
        CALL TIMER_START('THERMODYNAMICS      [FORWARD_STEP]',mythid)
        CALL THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('THERMODYNAMICS      [FORWARD_STEP]',mythid)

C--   do exchanges (needed for DYNAMICS) when using stagger time-step :
        CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
        CALL DO_STAGGER_FIELDS_EXCHANGES( myTime, myIter, myThid )
        CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

#ifdef ALLOW_SHAP_FILT
       IF (useSHAP_FILT .AND. 
     &     staggerTimeStep .AND. shap_filt_TrStagg ) THEN
        CALL TIMER_START('SHAP_FILT           [FORWARD_STEP]',myThid)
        CALL SHAP_FILT_APPLY_TS( gT, gS, myTime, myIter, myThid )
        CALL TIMER_STOP ('SHAP_FILT           [FORWARD_STEP]',myThid)
       ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
       IF (useZONAL_FILT .AND. 
     &     staggerTimeStep .AND. zonal_filt_TrStagg ) THEN
        CALL TIMER_START('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
        CALL ZONAL_FILT_APPLY_TS( gT, gS, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
      ENDIF
#endif   

C--     Step forward fields and calculate time tendency terms.
        IF ( momStepping ) THEN
        CALL TIMER_START('DYNAMICS            [FORWARD_STEP]',mythid)
        CALL DYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('DYNAMICS            [FORWARD_STEP]',mythid)
        ENDIF

#ifdef ALLOW_NONHYDROSTATIC
C--   Step forward W field in N-H algorithm
        IF ( momStepping .AND. nonHydrostatic ) THEN
          CALL TIMER_START('CALC_GW          [FORWARD_STEP]',myThid)
          CALL CALC_GW(myThid)
          CALL TIMER_STOP ('CALC_GW          [FORWARD_STEP]',myThid)
        ENDIF
#endif

#ifdef NONLIN_FRSURF
C--   update hfacC,W,S and recip_hFac according to etaH(n+1) :
      IF ( nonlinFreeSurf.GT.0) THEN
        CALL TIMER_START('UPDATE_SURF_DR     [FORWARD_STEP]',mythid)
        CALL UPDATE_SURF_DR( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_SURF_DR     [FORWARD_STEP]',myThid)
      ENDIF
C-    update also CG2D matrix (and preconditioner)
      IF ( momStepping .AND. nonlinFreeSurf.GT.2 ) THEN
        CALL TIMER_START('UPDATE_CG2D        [FORWARD_STEP]',mythid)
        CALL UPDATE_CG2D( myTime, myIter, myThid )
        CALL TIMER_START('UPDATE_CG2D        [FORWARD_STEP]',mythid)
      ENDIF
#endif

C--   Apply Filters to u*,v* before SOLVE_FOR_PRESSURE
#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT .AND. shap_filt_uvStar) THEN
        CALL TIMER_START('SHAP_FILT           [FORWARD_STEP]',myThid)
        CALL SHAP_FILT_APPLY_UV( gUnm1,gVnm1, myTime,myIter,myThid )
        IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
          CALL SHAP_FILT_APPLY_UV( uVel,vVel, myTime,myIter,myThid )
        ENDIF
        CALL TIMER_STOP ('SHAP_FILT           [FORWARD_STEP]',myThid)
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT .AND. zonal_filt_uvStar) THEN
        CALL TIMER_START('ZONAL_FILT_APPLY    [FORWARD_STEP]',myThid)
        CALL ZONAL_FILT_APPLY_UV( gUnm1, gVnm1, myThid )
        IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
          CALL ZONAL_FILT_APPLY_UV( uVel, vVel, myThid )
        ENDIF
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
CADJ STORE pressure  = comlev1, key = ikey_dynamics
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

#ifdef ALLOW_FLT
C--   Calculate float trajectories
      IF (useFLT) THEN
        CALL TIMER_START('FLOATS            [FORWARD_STEP]',myThid)
        CALL FLT_MAIN(myIter,myTime, myThid)
        CALL TIMER_STOP ('FLOATS            [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifndef EXCLUDE_MONITOR
C--   Check status of solution (statistics, cfl, etc...)
      CALL TIMER_START('MONITOR             [FORWARD_STEP]',mythid)
      CALL MONITOR( myIter, myTime, myThid )
      CALL TIMER_STOP ('MONITOR             [FORWARD_STEP]',myThid)
#endif /* EXCLUDE_MONITOR */

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
      CALL WRITE_CHECKPOINT(
     &        .FALSE., myTime, myIter, myThid )
      CALL TIMER_STOP ('WRITE_CHECKPOINT    [FORWARD_STEP]',myThid)

      END
