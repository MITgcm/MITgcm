C $Header: /u/gcmpack/MITgcm/model/src/the_main_loop.F,v 1.21 2001/09/28 02:26:57 adcroft Exp $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: THE_MAIN_LOOP
C     !INTERFACE:
      SUBROUTINE THE_MAIN_LOOP( mytime, myiter, mythid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE the_main_loop
C     | o Run the ocean model and evaluate the specified cost function.
C     *================================================================*
C     |
C     | THE_MAIN_LOOP is the toplevel routine for the Tangent Linear and
C     | Adjoint Model Compiler (TAMC). For this purpose the initialization
C     | of the model was split into two parts. Those parameters that do
C     | not depend on a specific model run are set in INITIALISE_FIXED,
C     | whereas those that do depend on the specific realization are
C     | initialized in INITIALISE_VARIA. 
C     | This routine is to be used in conjuction with the MITgcmuv 
C     | checkpoint 37.
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"
# include "DYNVARS.h"
# include "FFIELDS.h"
# include "GAD.h"
# ifdef ALLOW_PASSIVE_TRACER
#  include "TR1.h"
# endif
# ifdef ALLOW_NONHYDROSTATIC
#  include "CG3D.h"
# endif
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     note: under the multi-threaded model myiter and 
C           mytime are local variables passed around as routine 
C           arguments. Although this is fiddly it saves the need to 
C           impose additional synchronisation points when they are 
C           updated.
C     myIter - iteration counter for this thread
C     myTime - time counter for this thread
C     myThid - thread number for this instance of the routine.
      INTEGER myThid 
      INTEGER myIter
      _RL     myTime

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer iloop
#ifdef ALLOW_TAMC_CHECKPOINTING
      integer ilev_1
      integer ilev_2
      integer ilev_3
      integer max_lev2
      integer max_lev3
#endif
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
c--   Initialize storage for the cost function evaluation.
CADJ  INIT dummytape = common, 1
c--   Initialize storage for the outermost loop.
CADJ  INIT tapelev3 = USER
#ifdef ALLOW_TAMC_CHECKPOINTING
       nIter0 = INT( startTime/deltaTClock )
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

#ifdef ALLOW_ADJOINT_RUN
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
CADJ STORE gu        = tapelev3, key = ilev_3
CADJ STORE gv        = tapelev3, key = ilev_3
#ifdef INCLUDE_CD_CODE
CADJ STORE uveld     = tapelev3, key = ilev_3
CADJ STORE vveld     = tapelev3, key = ilev_3
CADJ STORE unm1      = tapelev3, key = ilev_3
CADJ STORE vnm1      = tapelev3, key = ilev_3
CADJ STORE gucd      = tapelev3, key = ilev_3
CADJ STORE gvcd      = tapelev3, key = ilev_3
#endif
#ifdef ALLOW_COST_TRACER
CADJ STORE tr1       = tapelev3, key = ilev_3
CADJ STORE gtr1nm1   = tapelev3, key = ilev_3
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
CADJ STORE gu        = tapelev2, key = ilev_2
CADJ STORE gv        = tapelev2, key = ilev_2
#ifdef INCLUDE_CD_CODE
CADJ STORE uveld     = tapelev2, key = ilev_2
CADJ STORE vveld     = tapelev2, key = ilev_2
CADJ STORE unm1      = tapelev2, key = ilev_2
CADJ STORE vnm1      = tapelev2, key = ilev_2
CADJ STORE gucd      = tapelev2, key = ilev_2
CADJ STORE gvcd      = tapelev2, key = ilev_2
#endif
#ifdef ALLOW_COST_TRACER
CADJ STORE tr1       = tapelev2, key = ilev_2
CADJ STORE gtr1nm1   = tapelev2, key = ilev_2
#endif

c--       Initialize storage for the innermost loop.
c--       Always check common block sizes for the checkpointing!
CADJ INIT comlev1        = COMMON,nchklev_1
CADJ INIT comlev1_bibj   = COMMON,nchklev_1*nsx*nsy*nthreads_chkpt
CADJ INIT comlev1_bibj_k = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt
CADJ INIT comlev1_kpp    = COMMON,nchklev_1*nsx*nsy
#ifndef DISABLE_MULTIDIM_ADVECTION
CADJ INIT comlev1_bibj_pass
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass
#endif /* DISABLE_MULTIDIM_ADVECTION */
#ifdef ALLOW_BULKFORMULAE
CADJ INIT comlev1_exf_1
CADJ &     = COMMON,nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
CADJ INIT comlev1_exf_2
CADJ &     = COMMON,niter_bulk*nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
#endif

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
CADJ INIT comlev1_kpp    = COMMON,nchklev_0*nsx*nsy

C--   RG replace 2 by max of num_v_smooth_Ri
CADJ INIT comlev1_kpp_sm = COMMON,nchklev_0*nsx*nsy*2

c--   Check the choice of the checkpointing parameters in relation
c--   to nTimeSteps: (nchklev_0 .ge. nTimeSteps)
      if (nchklev_0 .lt. nTimeSteps) then
        print*
        print*, ' the_main_loop: TAMC checkpointing parameter ',
     &       'nchklev_0 = ',       nchklev_0
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

#ifdef ALLOW_TAMC_CHECKPOINTING
        nIter0 = INT( startTime/deltaTClock )
        ikey_dynamics = ilev_1
#endif


        CALL TIMER_START('FORWARD_STEP        [THE_MAIN_LOOP]',mythid)
        CALL FORWARD_STEP( iloop, mytime, myiter, mythid )
        CALL TIMER_STOP ('FORWARD_STEP        [THE_MAIN_LOOP]',mythid)

#ifdef ALLOW_COST
C--     compare model with data and compute cost function
C--     this is done after exchanges to allow interpolation
        CALL TIMER_START('COST_TILE           [THE_MAIN_LOOP]',myThid)
        CALL COST_TILE  ( myThid )
        CALL TIMER_STOP ('COST_TILE           [THE_MAIN_LOOP]',myThid)
#endif

c--     >>> Loop body end <<<

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

#ifdef ALLOW_COST
c--   Sum all cost function contributions.
      call TIMER_START('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)
      call COST_FINAL ( mythid )
      call TIMER_STOP ('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)
#endif

      _BARRIER
      CALL TIMER_STOP ('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

      END
