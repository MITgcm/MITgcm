C $Header: /u/gcmpack/MITgcm/model/src/the_main_loop.F,v 1.70 2006/05/03 23:34:42 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THE_MAIN_LOOP
C     !INTERFACE:
      SUBROUTINE THE_MAIN_LOOP( myTime, myIter, myThid )

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
# include "MOM_FLUXFORM.h"
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS.h"
# endif
# if (defined (EXACT_CONSERV) || defined (NONLIN_FRSURF))
#  include "SURFACE.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS.h"
#  ifdef ALLOW_PTRACERS
#   include "OBCS_PTRACERS.h"
#  endif
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
# ifdef ALLOW_THSICE
#  include "THSICE_VARS.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
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

C     !FUNCTIONS:
C     == Functions ==
#ifdef ALLOW_RUNCLOCK
      LOGICAL RUNCLOCK_CONTINUE
      LOGICAL RC_CONT
#endif

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer iloop
#ifdef ALLOW_AUTODIFF_TAMC
      integer ilev_1
      integer ilev_2
      integer ilev_3
      integer ilev_4
      integer max_lev2
      integer max_lev3
      integer max_lev4
#endif
CEOP
#if defined(TIME_PER_TIMESTEP) || defined(USE_PAPI_FLOPS)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef TIME_PER_TIMESTEP
CCE107 common block for per timestep timing
C     !TIMING VARIABLES
C     == Timing variables ==
      REAL*8 utnew, utold, stnew, stold, wtnew, wtold
      DATA utnew, utold, stnew, stold, wtnew, wtold /6*0.0D0/
#endif
#ifdef USE_PAPI_FLOPS
CCE107 common block for PAPI summary performance
#include <fpapi.h>
      INTEGER*8 flpops
      DATA flpops /0/
      INTEGER check
      REAL real_time, proc_time, mflops
      DATA real_time, proc_time, mflops /3*0.0D0/
#endif
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_MAIN_LOOP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
c--   Initialize storage for the cost function evaluation.
CADJ  INIT dummytape = common, 1
c--   Initialize storage for the outermost loop.
CADJ  INIT tapelev_ini_bibj_k   = USER
CADJ  INIT tapelev_init   = USER
c
#if (defined (AUTODIFF_2_LEVEL_CHECKPOINT))
CADJ  INIT tapelev2 = USER
#elif (defined (AUTODIFF_4_LEVEL_CHECKPOINT))
CADJ  INIT tapelev4 = USER
#else
CADJ  INIT tapelev3 = USER
#endif

      nIter0 = NINT( (startTime-baseTime)/deltaTClock )
      ikey_dynamics = 1

      CALL TIMER_START('ADJOINT SPIN-UP', mythid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
CADJ STORE hFacC = tapelev_init, key = 1
# endif
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INITIALISE_VARIA',myThid)
#endif
C--   Set initial conditions (variable arrays)
      CALL TIMER_START('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)
      CALL INITIALISE_VARIA( mythid )
      CALL TIMER_STOP ('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)

#ifdef ALLOW_MONITOR
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('MONITOR',myThid)
#endif
C--   Check status of solution (statistics, cfl, etc...)
      CALL TIMER_START('MONITOR             [THE_MAIN_LOOP]', mythid)
      CALL MONITOR( myIter, myTime, myThid )
      CALL TIMER_STOP ('MONITOR             [THE_MAIN_LOOP]', mythid)
#endif /* ALLOW_MONITOR */

C--   Do IO if needed (Dump for start state).
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('DO_THE_MODEL_IO',myThid)
#endif

#ifdef ALLOW_OFFLINE
      CALL TIMER_START('OFFLINE_MODEL_IO     [FORWARD_STEP]',myThid)
      CALL OFFLINE_MODEL_IO( myTime, myIter, myThid )
      CALL TIMER_STOP ('OFFLINE_MODEL_IO     [FORWARD_STEP]',myThid)
#else
      CALL TIMER_START('DO_THE_MODEL_IO     [THE_MAIN_LOOP]', mythid)
      CALL DO_THE_MODEL_IO( myTime, myIter, mythid )
      CALL TIMER_STOP ('DO_THE_MODEL_IO     [THE_MAIN_LOOP]', mythid)
#endif


#ifdef ALLOW_AUTODIFF_TAMC
      CALL TIMER_STOP ('ADJOINT SPIN-UP', mythid)
      _BARRIER
#endif

#ifdef TIME_PER_TIMESTEP
CCE107 Initial call for timers
      _BEGIN_MASTER( myThid )
      CALL TIMER_GET_TIME( utold, stold, wtold )
      _END_MASTER( myThid )
#endif
#ifdef USE_PAPI_FLOPS
CCE107 Initial call for PAPI
      _BEGIN_MASTER( myThid )
      call PAPIF_flops(real_time, proc_time, flpops, mflops, check)
      _END_MASTER( myThid )
#endif

c--   Do the model integration.
      CALL TIMER_START('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STARTS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_TAMC_CHECKPOINTING

      max_lev4=nTimeSteps/(nchklev_1*nchklev_2*nchklev_3)+1
      max_lev3=nTimeSteps/(nchklev_1*nchklev_2)+1
      max_lev2=nTimeSteps/nchklev_1+1

c**************************************
#ifdef ALLOW_DIVIDED_ADJOINT
CADJ loop = divided
#endif
c**************************************

#ifdef AUTODIFF_4_LEVEL_CHECKPOINT
      do ilev_4 = 1,nchklev_4
         if(ilev_4.le.max_lev4) then
c**************************************
#include "checkpoint_lev4_directives.h"
c**************************************
c--     Initialise storage for the middle loop.
CADJ    INIT tapelev3 = USER
#endif /* AUTODIFF_4_LEVEL_CHECKPOINT */

#ifndef AUTODIFF_2_LEVEL_CHECKPOINT
      do ilev_3 = 1,nchklev_3
         if(ilev_3.le.max_lev3) then
c**************************************
#include "checkpoint_lev3_directives.h"
c**************************************
c--     Initialise storage for the middle loop.
CADJ    INIT tapelev2 = USER
#endif /* AUTODIFF_2_LEVEL_CHECKPOINT */

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
CADJ INIT comlev1_kpp_k  = COMMON,nchklev_1*nsx*nsy*nr
#endif /* ALLOW_KPP */
c--
#ifdef ALLOW_GMREDI
CADJ INIT comlev1_gmredi_k_gad
CADJ &    = COMMON,nchklev_1*nsx*nsy*nr*nthreads_chkpt*maxpass
#endif /* ALLOW_GMREDI */
c--
#ifdef ALLOW_PTRACERS
CADJ INIT comlev1_bibj_ptracers = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*PTRACERS_num
CADJ INIT comlev1_bibj_k_ptracers = COMMON,
CADJ &    nchklev_1*nsx*nsy*nthreads_chkpt*PTRACERS_num*nr
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

            iloop = (ilev_2 - 1)*nchklev_1                     + ilev_1
#ifndef AUTODIFF_2_LEVEL_CHECKPOINT
     &            + (ilev_3 - 1)*nchklev_2*nchklev_1
#endif
#ifdef AUTODIFF_4_LEVEL_CHECKPOINT
     &            + (ilev_4 - 1)*nchklev_3*nchklev_2*nchklev_1
#endif

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

#ifdef ALLOW_AUTODIFF_TAMC
        nIter0 = NINT( (startTime-baseTime)/deltaTClock )
        ikey_dynamics = ilev_1
        CALL AUTODIFF_INADMODE_UNSET( myThid )
#endif

#ifdef ALLOW_AUTODIFF_TAMC
        CALL AUTODIFF_INADMODE_UNSET( myThid )
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('FORWARD_STEP',myThid)
#endif
        CALL TIMER_START('FORWARD_STEP        [THE_MAIN_LOOP]',mythid)
        CALL FORWARD_STEP( iloop, mytime, myiter, mythid )
        CALL TIMER_STOP ('FORWARD_STEP        [THE_MAIN_LOOP]',mythid)

#ifdef ALLOW_AUTODIFF_TAMC
        CALL AUTODIFF_INADMODE_SET( myThid )
#endif

#ifdef ALLOW_RUNCLOCK
        IF (useRunClock) THEN
         RC_CONT=RUNCLOCK_CONTINUE( myThid )
         IF (.NOT.RC_CONT) RETURN
        ENDIF
#endif /* ALLOW_RUNCLOCK */
#ifdef TIME_PER_TIMESTEP
CCE107 Time per timestep information
      _BEGIN_MASTER( myThid )
      CALL TIMER_GET_TIME( utnew, stnew, wtnew )
      WRITE(msgBuf,'(A34,3F10.6,I8)')
     $        'User, system and wallclock time:', utnew - utold,
     $        stnew - stold, wtnew - wtold, iloop
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      utold = utnew
      stold = stnew
      wtold = wtnew
      _END_MASTER( myThid )
#endif
#ifdef USE_PAPI_FLOPS
CCE107 PAPI summary performance
      _BEGIN_MASTER( myThid )
      call PAPIF_flops(real_time, proc_time, flpops, mflops, check)
      WRITE(msgBuf,'(F10.6,A34,I8)')
     $        mflops, 'Mflop/s during timestep ', iloop
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      _END_MASTER( myThid )
#endif

c--     >>> Loop body end <<<

#ifdef ALLOW_AUTODIFF_TAMC
        CALL AUTODIFF_INADMODE_SET( myThid )
#ifdef ALLOW_TAMC_CHECKPOINTING
            endif
          enddo
          endif
        enddo
#ifndef AUTODIFF_2_LEVEL_CHECKPOINT
        endif
      enddo
#endif
#ifdef AUTODIFF_4_LEVEL_CHECKPOINT
       endif
      enddo
#endif
#else /* ndef ALLOW_TAMC_CHECKPOINTING */
      enddo
#endif /* ALLOW_TAMC_CHECKPOINTING */

#else /* ndef ALLOW_AUTODIFF_TAMC */
      enddo
#endif /* ALLOW_AUTODIFF_TAMC */

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
