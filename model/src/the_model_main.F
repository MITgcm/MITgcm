C $Header: /u/gcmpack/MITgcm/model/src/the_model_main.F,v 1.40 2001/07/30 20:39:18 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE THE_MODEL_MAIN(myThid)
C     /==========================================================\
C     | SUBROUTINE THE_MODEL_MAIN                                |
C     | o Master controlling routine for model using the MITgcm  |
C     |   UV parallel wrapper.                                   |
C     |==========================================================|
C     | THE_MODEL_MAIN is invoked by the MITgcm UV parallel      |
C     | wrapper with a single integer argument "myThid". This    |
C     | variable identifies the thread number of an instance of  |
C     | THE_MODEL_MAIN. Each instance of THE_MODEL_MAIN works    |
C     | on a particular region of the models domain and          |
C     | synchronises with other instances as necessary. The      |
C     | routine has to "understand" the MITgcm parallel          |
C     | environment and the numerical algorithm. Editing this    |
C     | routine is best done with some knowledge of both aspects.|
C     | Notes                                                    |
C     | =====                                                    |
C     | C*P* comments indicating place holders for which code is |
C     |      presently being developed.                          |
C     \==========================================================/
      IMPLICIT NONE
C
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "cost.h"
#include "adcost.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid  

C     == Local variables ==
C     Note: Under the multi-threaded model myCurrentIter and 
C           myCurrentTime are local variables passed around as routine 
C           arguments. Although this is fiddly it saves the need to 
C           impose additional synchronisation points when they are 
C           updated.
C     myCurrentTime - Time counter for this thread
C     myCurrentIter - Iteration counter for this thread
      INTEGER myCurrentIter
      _RL     myCurrentTime

C--   This timer encompasses the whole code
      CALL TIMER_START('ALL                [THE_MODEL_MAIN]',myThid)


C--   Set model configuration (fixed arrays)
      CALL TIMER_START('INITIALISE_FIXED   [THE_MODEL_MAIN]',myThid)
      CALL INITIALISE_FIXED( myThid )
      CALL TIMER_STOP ('INITIALISE_FIXED   [THE_MODEL_MAIN]',myThid)

      myCurrentTime = startTime
      myCurrentIter = nIter0

#if ( defined (ALLOW_ADJOINT_RUN) || \
      defined (ALLOW_GRADIENT_CHECK) || \
      defined (ALLOW_ECCO_OPTIMIZATION) )

      _BEGIN_MASTER( mythid )
      IF (myProcId .eq. 0) THEN
         CALL CTRL_UNPACK( mycurrentiter, mycurrenttime, mythid )
      ENDIF
      _END_MASTER( mythid )
      _BARRIER

      fc   = 0.0
      adfc = 1.0
      CALL TIMER_START('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
      CALL ADTHE_MAIN_LOOP ( mythid )
      CALL TIMER_STOP ('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
      _BARRIER

      _BEGIN_MASTER( mythid )
      IF (myProcId .eq. 0) THEN
         call CTRL_PACK( mycurrentiter, mycurrenttime, mythid )
      ENDIF
      _END_MASTER( mythid )
      _BARRIER

#ifdef ALLOW_GRADIENT_CHECK
      if (useGrdchk) then
         CALL TIMER_START('GRDCHK_MAIN          [ADJOINT RUN]', mythid)
         CALL GRDCHK_MAIN( mythid )
         CALL TIMER_STOP ('GRDCHK_MAIN          [ADJOINT RUN]', mythid)
         _BARRIER
      end if
#endif

#else /* ALLOW_ADJOINT_RUN undef */

C--   Call time stepping loop of full model
      CALL TIMER_START('THE_MAIN_LOOP      [THE_MODEL_MAIN]',myThid)
      CALL THE_MAIN_LOOP(myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('THE_MAIN_LOOP      [THE_MODEL_MAIN]',myThid)

#endif /* ALLOW_ADJOINT_RUN */

C--   Final checkpoint (incase the in-loop checkpoint was missed)
      CALL TIMER_START('WRITE_CHECKPOINT   [THE_MODEL_MAIN]',myThid)
      CALL WRITE_CHECKPOINT(
     &        .TRUE., myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('WRITE_CHECKPOINT   [THE_MODEL_MAIN]',myThid)

      CALL TIMER_STOP ('ALL                [THE_MODEL_MAIN]',myThid)

C--   Write timer statistics
      IF ( myThid .EQ. 1 ) THEN
       CALL TIMER_PRINTALL( myThid )
       CALL COMM_STATS
      ENDIF

      RETURN
      END
