C $Header: /u/gcmpack/MITgcm/model/src/the_model_main.F,v 1.36 2001/03/06 17:10:29 jmc Exp $
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
C     Call Tree
C     =========
C     
C      main ( eesupp )
C       |
C       .
C       .
C       . Generic environment initialisation ( see eesupp/src and
C       .                                      eesupp/inc )      
C       . multiple threads and/or processes are created in here
C       .
C       .
C       .
C       |
C       |-THE_MODEL_MAIN - Begin specific model. One instance
C       |  |               of this codes exists for each thread
C       |  |               and/or instance. Each instance manages
C       |  |               a specifc set of tiles.               
C       |  |
C       |  |--INITIALISE_FIXED
C       |  |   o Set model configuration (fixed arrays)
C       |  |     Topography, hydrography, timestep, grid, etc..
C       |  | 
C       |  |--THE_MAIN_LOOP
C       |  |  |
C       |  |  |--INITIALISE_VARIA
C       |  |  |   o Set initial conditions (variable arrays)
C       |  |  |
C  ==>  |  |  | ** Time stepping loop starts here **
C  |    |  |  |
C /|\   |  |  |
C  |    |  |  |--FORWARD_STEP
C /|\   |  |  |  |  o Does a single forward step of the model
C  |    |  |  |  | 
C /|\   |  |  |  |
C  |    |  |  |  |--LOAD_EXTERNAL_DATA
C /|\   |  |  |  |   o Load and/or set time dependent forcing fields
C  |    |  |  |  |
C /|\   |  |  |  |--DYNAMICS
C  |    |  |  |  |   o Evaluate "forward" terms
C /|\   |  |  |  |
C  |    |  |  |  |--SOLVE_FOR_PRESSURE
C /|\   |  |  |  |   o Find pressure field to keep flow non-divergent
C  |    |  |  |  |
C /|\   |  |  |  |--THE_CORRECTION_STEP
C  |    |  |  |  |   o Correct flow field with and cycle time-stepping arrays
C /|\   |  |  |  |
C  |    |  |  |  |--DO_GTERM_BLOCKING_EXCHANGES
C /|\   |  |  |  |   o Update overlap regions
C  |    |  |  |  |
C /|\   |  |  |  |--DO_THE_MODEL_IO
C  |    |  |  |  |   o Write model state
C /|\   |  |  |  |
C  |    |  |  |  |--WRITE_CHECKPOINT
C /|\   |  |  |  |   o Write restart file(s)
C  |    |  |  |
C /|\   |  |  |
C  |<== |  |  | ** Time stepping loop finishes here **
C       |  |  |
C       |  |  |--
C       |  |
C       |  |--WRITE_STATE
C       |  |--WRITE_CHECKPOINT
C       |
C       .
C       .
C       . Generic environment termination ( see eesupp/src and
C       .                                      eesupp/inc )      
C       .
C       .

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
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

C--   Call time stepping loop of full model
      CALL TIMER_START('THE_MAIN_LOOP      [THE_MODEL_MAIN]',myThid)
      CALL THE_MAIN_LOOP(myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('THE_MAIN_LOOP      [THE_MODEL_MAIN]',myThid)


      CALL TIMER_STOP ('ALL                [THE_MODEL_MAIN]',myThid)

C--   Write timer statistics
      IF ( myThid .EQ. 1 ) THEN
       CALL TIMER_PRINTALL( myThid )
       CALL COMM_STATS
      ENDIF

      RETURN
      END
