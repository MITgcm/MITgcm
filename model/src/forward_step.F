C $Header: /u/gcmpack/MITgcm/model/src/forward_step.F,v 1.13 2001/07/20 15:53:10 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE FORWARD_STEP( iloop, myTime, myIter, myThid )
      IMPLICIT NONE

c     ==================================================================
c     SUBROUTINE forward_step
c     ==================================================================
c
c     o Run the ocean model and evaluate the specified cost function.
c
c     *the_main_loop* is the toplevel routine for the Tangent Linear and
c     Adjoint Model Compiler (TAMC). For this purpose the initialization
c     of the model was split into two parts. Those parameters that do
c     not depend on a specific model run are set in *initialise_fixed*,
c     whereas those that do depend on the specific realization are
c     initialized in *initialise_varia*. 

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "TR1.h"

#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "cost.h"
#endif

C     == routine arguments ==
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

C     == local variables ==


#ifdef ALLOW_AUTODIFF_TAMC
C--     Set the model iteration counter and the model time.
        myiter = nIter0 + (iloop-1)
        mytime = startTime + float(iloop-1)*deltaTclock

C       Include call to a dummy routine. Its adjoint will be 
C       called at the proper place in the adjoint code.
C       The adjoint routine will print out adjoint values 
C       if requested. The location of the call is important, 
C       it has to be after the adjoint of the exchanges 
C       (DO_GTERM_BLOCKING_EXCHANGES).
        CALL DUMMY_IN_STEPPING( myTime, myIter, myThid )
#endif

C--     Load forcing/external data fields.
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
C       NOTE, that although the exf package is part of the
C       distribution, it is not currently maintained, i.e.
C       exf is disabled by default in genmake.
        CALL EXF_GETFORCING( mytime, myiter, mythid )
#else
        CALL TIMER_START('EXTERNAL_FIELDS_LOAD[THE_MAIN_LOOP]',mythid)
        CALL EXTERNAL_FIELDS_LOAD( mytime, myiter, mythid )
        CALL TIMER_STOP ('EXTERNAL_FIELDS_LOAD[THE_MAIN_LOOP]',mythid)
#endif

C--     Step forward fields and calculate time tendency terms.
        CALL TIMER_START('DYNAMICS            [THE_MAIN_LOOP]',mythid)
        CALL DYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('DYNAMICS            [THE_MAIN_LOOP]',mythid)

#ifndef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_NONHYDROSTATIC
C--   Step forward W field in N-H algorithm
        IF ( nonHydrostatic ) THEN
          CALL TIMER_START('CALC_GW          [THE_MAIN_LOOP]',myThid)
          CALL CALC_GW(myThid)
          CALL TIMER_STOP ('CALC_GW          [THE_MAIN_LOOP]',myThid)
        ENDIF
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

C-- This block has been moved to the_correction_step(). We have
C   left this code here to indicate where the filters used to be
C   in the algorithm before JMC moved them to after the pressure
C   solver.
C
C#ifdef ALLOW_SHAP_FILT
CC--   Step forward all tiles, filter and exchange.
C      CALL TIMER_START('SHAP_FILT           [THE_MAIN_LOOP]',myThid)
C      CALL SHAP_FILT_APPLY( 
C     I                     gUnm1, gVnm1, gTnm1, gSnm1,
C     I                     myTime, myIter, myThid )
C      IF (implicDiv2Dflow.LT.1.) THEN
CC--   Explicit+Implicit part of the Barotropic Flow Divergence
CC      => Filtering of uVel,vVel is necessary
C         CALL SHAP_FILT_UV( uVel, vVel, myTime, myThid )
C      ENDIF
C      CALL TIMER_STOP ('SHAP_FILT           [THE_MAIN_LOOP]',myThid)
C#endif
C
C#ifdef ALLOW_ZONAL_FILT
C      IF (zonal_filt_lat.LT.90.) THEN 
C        CALL TIMER_START('ZONAL_FILT_APPLY    [THE_MAIN_LOOP]',myThid)
C        CALL ZONAL_FILT_APPLY(
C     U           gUnm1, gVnm1, gTnm1, gSnm1,
C     I           myThid )
C        CALL TIMER_STOP ('ZONAL_FILT_APPLY    [THE_MAIN_LOOP]',myThid)
C      ENDIF
C#endif

C--   Solve elliptic equation(s).
C     Two-dimensional only for conventional hydrostatic or 
C     three-dimensional for non-hydrostatic and/or IGW scheme.
      CALL TIMER_START('SOLVE_FOR_PRESSURE  [THE_MAIN_LOOP]',myThid)
      CALL SOLVE_FOR_PRESSURE( myThid )
      CALL TIMER_STOP ('SOLVE_FOR_PRESSURE  [THE_MAIN_LOOP]',myThid)

C--   Correct divergence in flow field and cycle time-stepping
C     arrays (for all fields) ; update time-counter
      myIter = nIter0 + iLoop
      myTime = startTime + deltaTClock * float(iLoop)
      CALL TIMER_START('THE_CORRECTION_STEP [THE_MAIN_LOOP]',myThid)
      CALL THE_CORRECTION_STEP(myTime, myIter, myThid)
      CALL TIMER_STOP ('THE_CORRECTION_STEP [THE_MAIN_LOOP]',myThid)

C--   Do "blocking" sends and receives for tendency "overlap" terms
c     CALL TIMER_START('BLOCKING_EXCHANGES  [THE_MAIN_LOOP]',myThid)
c     CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )
c     CALL TIMER_STOP ('BLOCKING_EXCHANGES  [THE_MAIN_LOOP]',myThid)

C--   Do "blocking" sends and receives for field "overlap" terms
      CALL TIMER_START('BLOCKING_EXCHANGES  [THE_MAIN_LOOP]',myThid)
      CALL DO_FIELDS_BLOCKING_EXCHANGES( myThid )
      CALL TIMER_STOP ('BLOCKING_EXCHANGES  [THE_MAIN_LOOP]',myThid)

#ifndef EXCLUDE_MONITOR
C--   Check status of solution (statistics, cfl, etc...)
      CALL MONITOR( myIter, myTime, myThid )
#endif /* EXCLUDE_MONITOR */

#ifndef ALLOW_AUTODIFF_TAMC
C--   Do IO if needed.
      CALL TIMER_START('DO_THE_MODEL_IO     [THE_MAIN_LOOP]',myThid)
      CALL DO_THE_MODEL_IO( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_THE_MODEL_IO     [THE_MAIN_LOOP]',myThid)

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
      CALL TIMER_START('WRITE_CHECKPOINT    [THE_MAIN_LOOP]',myThid)
      CALL WRITE_CHECKPOINT(
     &        .FALSE., myTime, myIter, myThid )
      CALL TIMER_STOP ('WRITE_CHECKPOINT    [THE_MAIN_LOOP]',myThid)

#endif /* ALLOW_AUTODIFF_TAMC */

      END
