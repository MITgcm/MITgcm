C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code_ad/Attic/the_main_loop.F,v 1.4 2005/12/10 17:44:23 utke Exp $
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
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS.h"
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
      integer leftOvers
      integer  uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4

      integer ilev_1
CEOP

      uCheckLev1=0
      uCheckLev2=0
      uCheckLev3=0
      uCheckLev4=0

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_MAIN_LOOP',myThid)
#endif

      nIter0 = NINT( (startTime-baseTime)/deltaTClock )
      ikey_dynamics = 1

      CALL TIMER_START('ADJOINT SPIN-UP', mythid)

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

c--   Do the model integration.
      CALL TIMER_START('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STARTS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<


c**************************************
c$openad INDEPENDENT(xx_theta_dummy)
c$openad DEPENDENT(xx_salt_dummy)

#ifndef ALLOW_TAMC_CHECKPOINTING 
#error "not set up for undefined ALLOW_TAMC_CHECKPOINTING "
#endif 

#ifndef ALLOW_AUTODIFF_TAMC 
#error "not set up for undefined ALLOW_AUTODIFF_TAMC "
#endif 
      
cju compute the actual checkpoint counts: 
      leftOvers = nTimeSteps
      if (leftOvers .lt. nchklev_1 ) then 
         uCheckLev1=leftOvers
      else
         uCheckLev1 = nchklev_1
      end if
      leftOvers = leftOvers - uCheckLev1

      if ((leftOvers/uCheckLev1) .lt. nchklev_2 ) then 
         uCheckLev2=(leftOvers/uCheckLev1)
      else
         uCheckLev2 = nchklev_2
      end if
      if (uCheckLev1*uCheckLev2 .gt. leftOvers) then 
         leftOvers=0
      else
         leftOvers = leftOvers - uCheckLev1*uCheckLev2
      end if

      if ((leftOvers/(uCheckLev1*uCkeckLev2)) .lt. nchklev_3 ) then 
         uCheckLev3=(leftOvers/(uCheckLev1*uCkeckLev2))
      else
         uCheckLev3 = nchklev_3
      end if
      if (uCheckLev1*uCheckLev2*uCheckLev3 .gt. leftOvers) then 
         leftOvers=0
      else
         leftOvers = leftOvers - uCheckLev1*uCheckLev2*uCheckLev3
      end if

      if ((leftOvers/(uCheckLev1*uCkeckLev2*uCkeckLev3)) 
     &.lt. nchklev_4 ) then 
         uCheckLev4=
     &(leftOvers/(uCheckLev1*uCkeckLev2*uCkeckLev3))
      else
         uCheckLev4 = nchklev_4
      end if
      leftOvers = leftOvers-uCheckLev1*uCheckLev2*uCheckLev3*uCheckLev4
      if (leftOvers .gt. 0) then 
         print *, 'JU: error, not enough checkpoints allowed'
      end if

      print* ,'JU: main_loop: ',uCheckLev1, uCheckLev2, 
     +uCheckLev3, uCheckLev4

      if (uCheckLev4 .gt. 0 ) then 
         do ilev_1 = 0, uCheckLev4-1

c--     >>> Loop body start <<<
cju not sure what the following two lines are good for:

            nIter0 = NINT( (startTime-baseTime)/deltaTClock )
            ikey_dynamics = ilev_1

#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('THE_FIRST_LEVEL_LOOP',myThid)
#endif

            CALL THE_FOURTH_LEVEL_LOOP( ilev_4, 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +myTime, myIter, myThid )

#ifdef ALLOW_RUNCLOCK
            IF (useRunClock) THEN
               RC_CONT=RUNCLOCK_CONTINUE( myThid )
               IF (.NOT.RC_CONT) RETURN
            ENDIF
#endif /* ALLOW_RUNCLOCK */

c--     >>> Loop body end <<<

         end do
      else
c--     >>> Loop body start <<
cju not sure what the following two lines are good for:

         nIter0 = NINT( (startTime-baseTime)/deltaTClock )
         ikey_dynamics = ilev_1

#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('THE_FIRST_LEVEL_PLAIN',myThid)
#endif

         CALL THE_FOURTH_LEVEL_PLAIN( 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +myTime, myIter, myThid )

#ifdef ALLOW_RUNCLOCK
         IF (useRunClock) THEN
            RC_CONT=RUNCLOCK_CONTINUE( myThid )
            IF (.NOT.RC_CONT) RETURN
         ENDIF
#endif /* ALLOW_RUNCLOCK */

c--     >>> Loop body end <<<

      end if

#ifdef ALLOW_COST
c--   Sum all cost function contributions.
      call TIMER_START('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)
      call COST_FINAL ( mythid )
      call TIMER_STOP ('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)
#endif

c$openad DEPENDENT(fc)

      _BARRIER
      CALL TIMER_STOP ('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_MAIN_LOOP',myThid)
#endif

      END

