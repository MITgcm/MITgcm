C $Header: /u/gcmpack/MITgcm/model/src/the_main_loop.F,v 1.10 2001/06/07 16:33:55 adcroft Exp $

#include "CPP_OPTIONS.h"


      subroutine the_main_loop( mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE the_main_loop
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
c     This routine is to be used in conjuction with the MITgcmuv 
c     checkpoint 37.
c
c     ==================================================================
c     SUBROUTINE the_main_loop
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "cost.h"
#endif

c     == routine arguments ==
c     note: under the multi-threaded model myiter and 
c           mytime are local variables passed around as routine 
c           arguments. Although this is fiddly it saves the need to 
c           impose additional synchronisation points when they are 
c           updated.
c     myiter - iteration counter for this thread
c     mytime - time counter for this thread
c     mythid - thread number for this instance of the routine.
      integer mythid 
      integer myiter
      _RL     mytime

c     == local variables ==

      integer iloop

#ifdef ALLOW_TAMC_CHECKPOINTING
      integer ilev_1
      integer ilev_2
      integer ilev_3
      integer max_lev2
      integer max_lev3
#endif

c--   == end of interface ==

#ifdef ALLOW_AUTODIFF_TAMC
c--   Initialize storage for the cost function evaluation.
CADJ  INIT dummytape = common, 1
c--   Initialize storage for the outermost loop.
CADJ  INIT tapelev3 = USER
#ifdef ALLOW_TAMC_CHECKPOINTING
       ikey_dynamics = 1
#endif
      CALL TIMER_START('ADJOINT SPIN-UP', mythid)
#endif

C--   Set initial conditions (variable arrays)
      CALL TIMER_START('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)
      CALL INITIALISE_VARIA( mythid )
      CALL TIMER_STOP ('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)

#ifndef ALLOW_AUTODIFF_TAMC
c--   Dump for start state.
      CALL TIMER_START('WRITE_STATE         [THE_MAIN_LOOP]', mythid)
      CALL WRITE_STATE( mytime, myiter, mythid )
      CALL TIMER_STOP ('WRITE_STATE         [THE_MAIN_LOOP]', mythid)
#endif

#ifndef EXCLUDE_MONITOR
C--   Check status of solution (statistics, cfl, etc...)
      CALL TIMER_START('MONITOR             [THE_MAIN_LOOP]', mythid)
      CALL MONITOR( myIter, myTime, myThid )
      CALL TIMER_STOP ('MONITOR             [THE_MAIN_LOOP]', mythid)
#endif /* EXCLUDE_MONITOR */

#ifdef ALLOW_COST_TEST
c--   Add control vector for forcing and parameter fields
      CALL CTRL_MAP_FORCING (mythid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      CALL TIMER_STOP ('ADJOINT SPIN-UP', mythid)
      _BARRIER
#endif

c--   Do the model integration.
      CALL TIMER_START('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STARTS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_TAMC_CHECKPOINTING
c--   Implement a three level checkpointing. For a two level
c--   checkpointing delete the middle loop; for n levels (n > 3)
c--   insert more loops.

c--   Check the choice of the checkpointing parameters in relation
c--   to nTimeSteps: (nchklev_1*nchklev_2*nchklev_3 .ge. nTimeSteps)
      if (nchklev_1*nchklev_2*nchklev_3 .lt. nTimeSteps) then
        print*
        print*, ' the_main_loop: TAMC checkpointing parameters'
        print*, '            nchklev_1*nchklev_2*nchklev_3 = ',
     &       nchklev_1*nchklev_2*nchklev_3
        print*, '            are not consistent with nTimeSteps = ',
     &       nTimeSteps
        stop    ' ... stopped in the_main_loop.'
      endif
      max_lev3=nTimeSteps/(nchklev_1*nchklev_2)+1
      max_lev2=nTimeSteps/nchklev_1+1

      do ilev_3 = 1,nchklev_3
         if(ilev_3.le.max_lev3) then
CADJ STORE gsnm1     = tapelev3, key = ilev_3
CADJ STORE gtnm1     = tapelev3, key = ilev_3
CADJ STORE gunm1     = tapelev3, key = ilev_3
CADJ STORE gvnm1     = tapelev3, key = ilev_3
CADJ STORE theta     = tapelev3, key = ilev_3
CADJ STORE salt      = tapelev3, key = ilev_3
CADJ STORE uvel      = tapelev3, key = ilev_3
CADJ STORE vvel      = tapelev3, key = ilev_3
CADJ STORE wvel      = tapelev3, key = ilev_3
CADJ STORE etan      = tapelev3, key = ilev_3
CADJ STORE etanm1    = tapelev3, key = ilev_3
#ifdef INCLUDE_CD_CODE
CADJ STORE uveld     = tapelev3, key = ilev_3
CADJ STORE vveld     = tapelev3, key = ilev_3
CADJ STORE unm1      = tapelev3, key = ilev_3
CADJ STORE vnm1      = tapelev3, key = ilev_3
#endif

c--     Initialise storage for the middle loop.
CADJ    INIT tapelev2 = USER

        do ilev_2 = 1,nchklev_2
         if(ilev_2.le.max_lev2) then
CADJ STORE gsnm1     = tapelev2, key = ilev_2
CADJ STORE gtnm1     = tapelev2, key = ilev_2
CADJ STORE gunm1     = tapelev2, key = ilev_2
CADJ STORE gvnm1     = tapelev2, key = ilev_2
CADJ STORE theta     = tapelev2, key = ilev_2
CADJ STORE salt      = tapelev2, key = ilev_2
CADJ STORE uvel      = tapelev2, key = ilev_2
CADJ STORE vvel      = tapelev2, key = ilev_2
CADJ STORE wvel      = tapelev2, key = ilev_2
CADJ STORE etan      = tapelev2, key = ilev_2
CADJ STORE etanm1    = tapelev2, key = ilev_2
#ifdef INCLUDE_CD_CODE
CADJ STORE uveld     = tapelev2, key = ilev_2
CADJ STORE vveld     = tapelev2, key = ilev_2
CADJ STORE unm1      = tapelev2, key = ilev_2
CADJ STORE vnm1      = tapelev2, key = ilev_2
#endif

c--       Initialize storage for the innermost loop.
c--       Always check common block sizes for the checkpointing!
CADJ INIT comlev1        = COMMON,nchklev_1
CADJ INIT comlev1_bibj   = COMMON,nchklev_1*nsx*nsy*nthreads_chkpt
CADJ INIT comlev1_bibj_k = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt
cphCADJ INIT comlev1_impl   = COMMON,nchklev_1*nsx*nsy*6
CADJ INIT comlev1_kpp    = COMMON,nchklev_1*nsx*nsy

C--   RG replace 2 by max of num_v_smooth_Ri
CADJ INIT comlev1_kpp_sm = COMMON,nchklev_1*nsx*nsy*2

          do ilev_1 = 1,nchklev_1

c--         The if-statement below introduces a some flexibility in the
c--         choice of the 3-tupel ( nchklev_1, nchklev_2, nchklev_3 ).
c--
c--         Requirement: nchklev_1*nchklev_2*nchklev_3 .ge. nTimeSteps .

            iloop = (ilev_3 - 1)*nchklev_2*nchklev_1 +
     &              (ilev_2 - 1)*nchklev_1           + ilev_1

            if ( iloop .le. nTimeSteps ) then

#else /* ALLOW_TAMC_CHECKPOINTING  undefined */
c--   Initialise storage for reference trajectory without TAMC check-
c--   pointing.
CADJ INIT history        = USER
CADJ INIT comlev1_bibj   = COMMON,nchklev_0*nsx*nsy*nthreads_chkpt
CADJ INIT comlev1_bibj_k = COMMON,nchklev_0*nsx*nsy*nr*nthreads_chkpt
CADJ INIT comlev1_impl   = COMMON,nchklev_0*nsx*nsy*6
CADJ INIT comlev1_impl_k = COMMON,nchklev_0*nsx*nsy*(nr-2)*6
CADJ INIT comlev1_kpp    = COMMON,nchklev_0*nsx*nsy

C--   RG replace 2 by max of num_v_smooth_Ri
CADJ INIT comlev1_kpp_sm = COMMON,nchklev_0*nsx*nsy*2

c--   Check the choice of the checkpointing parameters in relation
c--   to nTimeSteps: (nchklev_0 .ge. nTimeSteps)
      if (nchklev_0 .lt. nTimeSteps) then
        print*
        print*, ' the_main_loop: TAMC checkpointing parameter ',
     &       nchklev_0 = ',       nchklev_0
        print*, '                 not consistent with nTimeSteps = ', 
     &       nTimeSteps
        stop    ' ... stopped in the_main_loop.'
      endif

      DO iloop = 1, nTimeSteps

#endif /* ALLOW_TAMC_CHECKPOINTING */

#else /* ALLOW_AUTODIFF_TAMC  undefined */

c--   Start the main loop of adjoint_Objfunc. Automatic differentiation
c--   NOT enabled.
      DO iloop = 1, nTimeSteps

#endif /* ALLOW_AUTODIFF_TAMC */

c--     >>> Loop body start <<<

#ifdef ALLOW_AUTODIFF_TAMC
c--     Set the model iteration counter and the model time.
        myiter = nIter0 + (iloop-1)
        mytime = startTime + float(iloop-1)*deltaTclock
#endif

c--     Load forcing/external data fields.
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
c       NOTE, that although the exf package is part of the
c       distribution, it is not currently maintained, i.e.
c       exf is disabled by default in genmake.
        call exf_getforcing( mytime, myiter, mythid )
#else
        CALL TIMER_START('EXTERNAL_FIELDS_LOAD[THE_MAIN_LOOP]',mythid)
        CALL EXTERNAL_FIELDS_LOAD( mytime, myiter, mythid )
        CALL TIMER_STOP ('EXTERNAL_FIELDS_LOAD[THE_MAIN_LOOP]',mythid)
#endif

#ifdef ALLOW_TAMC_CHECKPOINTING
              ikey_dynamics = ilev_1
#endif
c--     Step forward fields and calculate time tendency terms.
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

#ifdef ALLOW_SHAP_FILT
C--   Step forward all tiles, filter and exchange.
      CALL TIMER_START('SHAP_FILT           [THE_MAIN_LOOP]',myThid)
      CALL SHAP_FILT_APPLY( 
     I                     gUnm1, gVnm1, gTnm1, gSnm1,
     I                     myTime, myIter, myThid )
      IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
         CALL SHAP_FILT_UV( uVel, vVel, myTime, myThid )
      ENDIF
      CALL TIMER_STOP ('SHAP_FILT           [THE_MAIN_LOOP]',myThid)
#endif

#ifdef ALLOW_ZONAL_FILT
      IF (zonal_filt_lat.LT.90.) THEN 
        CALL TIMER_START('ZONAL_FILT_APPLY    [THE_MAIN_LOOP]',myThid)
        CALL ZONAL_FILT_APPLY(
     U           gUnm1, gVnm1, gTnm1, gSnm1,
     I           myThid )
        CALL TIMER_STOP ('ZONAL_FILT_APPLY    [THE_MAIN_LOOP]',myThid)
      ENDIF
#endif

#endif /* ALLOW_AUTODIFF_TAMC */

C--   Solve elliptic equation(s).
C     Two-dimensional only for conventional hydrostatic or 
C     three-dimensional for non-hydrostatic and/or IGW scheme.
      CALL TIMER_START('SOLVE_FOR_PRESSURE  [THE_MAIN_LOOP]',myThid)
      CALL SOLVE_FOR_PRESSURE( myThid )
      CALL TIMER_STOP ('SOLVE_FOR_PRESSURE  [THE_MAIN_LOOP]',myThid)

#ifdef ALLOW_AUTODIFF_TAMC
c     Include call to a dummy routine. Its adjoint will be 
c     called at the proper place in the adjoint code.
c     The adjoint routine will print out adjoint values 
c     if requested. The location of the call is important, 
c     it has to be after the adjoint of the exchanges 
c     (DO_GTERM_BLOCKING_EXCHANGES).
      call dummy_in_stepping( myTime, myIter, myThid )
#endif

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

c--           >>> Loop body end <<<

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_TAMC_CHECKPOINTING
            endif
          enddo
          endif
        enddo
        endif
      enddo
#else
      enddo
#endif

#else
      enddo
#endif

      _BARRIER
      CALL TIMER_STOP ('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

#ifdef ALLOW_COST_TEST
      call timer_start('ADJOINT FINALIZE COST', mythid)
c--   Veronique's test case
      call timer_start('cost_test          [ADJOINT SPIN-DOWN]', mythid)
      call cost_test( myThid )
      call timer_stop ('cost_test          [ADJOINT SPIN-DOWN]', mythid)

c--   Sum all cost function contributions.
      call timer_start('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)
      call cost_Final( mythid )
      call timer_stop ('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)
      call timer_stop ('ADJOINT FINALIZE COST', mythid)
#endif


      END
