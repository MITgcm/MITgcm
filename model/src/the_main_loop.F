C $Header: /u/gcmpack/MITgcm/model/src/the_main_loop.F,v 1.46 2003/11/20 02:26:18 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_OPTIONS.h"
#endif

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

c**************************************
#ifdef ALLOW_AUTODIFF_TAMC

c These includes are needed for 
c AD-checkpointing. 
c They provide the fields to be stored.

# include "GRID.h"
# include "DYNVARS.h"
# include "FFIELDS.h"
# include "EOS.h"
# include "GAD.h"
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_PASSIVE_TRACER
#  include "TR1.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS.h"
# endif
# ifdef ALLOW_NONHYDROSTATIC
#  include "CG3D.h"
# endif
# ifdef EXACT_CONSERV
#  include "SURFACE.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS.h"
# endif
# ifdef ALLOW_EXF
#  include "exf_fields.h"
#  include "exf_clim_fields.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "exf_constants.h"
#  endif
# endif /* ALLOW_EXF */
# ifdef ALLOW_SEAICE
#  include "SEAICE.h"
# endif
# ifdef ALLOW_DIVIDED_ADJOINT_MPI
#  include "mpif.h"
# endif

# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"

#endif /* ALLOW_AUTODIFF_TAMC */
c**************************************

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

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_MAIN_LOOP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
c--   Initialize storage for the cost function evaluation.
CADJ  INIT dummytape = common, 1
c--   Initialize storage for the outermost loop.
CADJ  INIT tapelev3 = USER
CADJ  INIT tapelev_ini_bibj_k   = USER
#ifdef ALLOW_TAMC_CHECKPOINTING
       nIter0 = INT( startTime/deltaTClock )
       ikey_dynamics = 1
#endif
      CALL TIMER_START('ADJOINT SPIN-UP', mythid)
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INITIALISE_VARIA',myThid)
#endif

C--   Set initial conditions (variable arrays)
      CALL TIMER_START('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)
      CALL INITIALISE_VARIA( mythid )
      CALL TIMER_STOP ('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)

c--   Dump for start state.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('WRITE_STATE',myThid)
#endif
      CALL TIMER_START('WRITE_STATE         [THE_MAIN_LOOP]', mythid)
      CALL WRITE_STATE( mytime, myiter, mythid )
      CALL TIMER_STOP ('WRITE_STATE         [THE_MAIN_LOOP]', mythid)

#ifdef ALLOW_MONITOR
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('MONITOR',myThid)
#endif
C--   Check status of solution (statistics, cfl, etc...)
      CALL TIMER_START('MONITOR             [THE_MAIN_LOOP]', mythid)
      CALL MONITOR( myIter, myTime, myThid )
      CALL TIMER_STOP ('MONITOR             [THE_MAIN_LOOP]', mythid)
#endif /* ALLOW_MONITOR */

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

c**************************************
#ifdef ALLOW_DIVIDED_ADJOINT
CADJ loop = divided
#endif
c**************************************

      do ilev_3 = 1,nchklev_3
         if(ilev_3.le.max_lev3) then
c**************************************
#include "checkpoint_lev3_directives.h"
c**************************************

c--     Initialise storage for the middle loop.
CADJ    INIT tapelev2 = USER

        do ilev_2 = 1,nchklev_2
         if(ilev_2.le.max_lev2) then
c**************************************
#include "checkpoint_lev2_directives.h"
c**************************************

c**************************************
#ifdef ALLOW_AUTODIFF_TAMC
c--       Initialize storage for the innermost loop.
c--       Always check common block sizes for the checkpointing!
c--
CADJ INIT comlev1        = COMMON,nchklev_1
CADJ INIT comlev1_bibj   = COMMON,nchklev_1*nsx*nsy*nthreads_chkpt
CADJ INIT comlev1_bibj_k = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt
c--
#ifdef ALLOW_KPP
CADJ INIT comlev1_kpp    = COMMON,nchklev_1*nsx*nsy
#endif /* ALLOW_KPP */
c--
#ifdef ALLOW_GMREDI
CADJ INIT comlev1_gmredi_k_gad
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass
#endif /* ALLOW_GMREDI */
c--
#ifdef ALLOW_PTRACERS
CADJ INIT comlev1_bibj_ptracers = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*NUMBER_OF_PTRACERS
#endif /* ALLOW_PTRACERS */
c--
#ifndef DISABLE_MULTIDIM_ADVECTION
CADJ INIT comlev1_bibj_k_gad
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass
CADJ INIT comlev1_bibj_k_gad_pass
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass*maxcube
#endif /* DISABLE_MULTIDIM_ADVECTION */
c--
#if (defined (ALLOW_EXF) && defined (ALLOW_BULKFORMULAE))
CADJ INIT comlev1_exf_1
CADJ &     = COMMON,nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
CADJ INIT comlev1_exf_2
CADJ &     = COMMON,niter_bulk*nchklev_1*snx*nsx*sny*nsy*nthreads_chkpt
#endif /* ALLOW_BULKFORMULAE */
c--
#ifdef ALLOW_SEAICE
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ INIT comlev1_lsr = COMMON,nchklev_1*2
# endif
#endif /* ALLOW_SEAICE */
c--
#endif /* ALLOW_AUTODIFF_TAMC */
c**************************************

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

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('FORWARD_STEP',myThid)
#endif

        CALL TIMER_START('FORWARD_STEP        [THE_MAIN_LOOP]',mythid)
        CALL FORWARD_STEP( iloop, mytime, myiter, mythid )
        CALL TIMER_STOP ('FORWARD_STEP        [THE_MAIN_LOOP]',mythid)

#ifdef ALLOW_COST
C--     compare model with data and compute cost function
C--     this is done after exchanges to allow interpolation
ceh3 perhaps needs an IF ( useCOST ) THEN
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

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_MAIN_LOOP',myThid)
#endif

      END
