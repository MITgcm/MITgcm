C $Header: /u/gcmpack/MITgcm/model/src/the_model_main.F,v 1.17 1998/07/01 19:49:36 adcroft Exp $

#include "CPP_EEOPTIONS.h"

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
C       |  |--INITIALISE
C       |  |   o Set initial conditions and model configuration
C       |  |     Topography, hydrography, timestep, grid, etc..
C       |  | 
C  ==>  |  | ** Time stepping loop starts here **
C  |    |  |
C /|\   |  |
C  |    |  |--LOAD_EXTERNAL_DATA
C /|\   |  |   o Load and/or set time dependent forcing fields
C  |    |  |
C /|\   |  |--DYNAMICS
C  |    |  |   o Evaluate "forward" terms
C /|\   |  |
C  |    |  |--DO_THE_MODEL_IO
C /|\   |  |   o Write model state
C  |    |  |
C /|\   |  |--SOLVE_FOR_PRESSURE
C  |    |  |   o Find pressure field to keep flow non-divergent
C /|\   |  |
C  |    |  |--DO_GTERM_BLOCKING_EXCHANGES
C /|\   |  |   o Update overlap regions
C  |    |  |
C /|\   |  |--WRITE_CHECKPOINT
C  |    |  |   o Write restart file(s)
C /|\   |  |
C  |    |  |
C  |<== |  | ** Time stepping loop finishes here **
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
#include "CG2D.h"
#include "DYNVARS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid  

C     == Local variables ==
C     Note: Under the multi-threaded model myCurrentIter and 
C           myCurrentTime are local variables passed around as routine 
C           arguments. Although this is fiddly it saves the need to 
C           impose additional synchronisation points when they are 
C           updated.
C     myCurrentIter - Iteration counter for this thread
C     myCurrentTime - Time counter for this thread
C     I             - Loop counter
      INTEGER I, myCurrentIter
      REAL    myCurrentTime

C--   Set model initial conditions 
      CALL INITIALISE( myThid )
      myCurrentTime = startTime
      myCurrentIter = nIter0

C--   Dump for start state
      CALL WRITE_STATE( .TRUE., myCurrentTime, myCurrentIter, myThid )

C--   Begin time stepping loop
      DO I=1, nTimeSteps

C--    Load forcing/external data fields
       CALL LOAD_EXTERNAL_FIELDS( myCurrentTime, myCurrentIter, myThid )

C--    Step forward fields and calculate time tendency terms
       CALL DYNAMICS( myCurrentTime, myCurrentIter, myThid )

C--    Do time averages
#ifdef ALLOW_DIAGNOSTICS
       IF (taveFreq.GT.0.) THEN
        CALL WRITE_TIME_AVERAGES( myCurrentTime, myCurrentIter, myThid )
       ENDIF
#endif

C--    Do IO if needed.
C      Note:
C      =====
C      At this point model arrays hold U,V,T,S  at "time-level" N 
C      and cg2d_x at "time-level" N-1/2 where N = I+timeLevBase-1. 
C      By convention this is taken to be the model "state". 
       CALL DO_THE_MODEL_IO( myCurrentTime, myCurrentIter, myThid )

C--    Solve elliptic equation(s).
C      Two-dimensional only for conventional hydrostatic or 
C      three-dimensional for non-hydrostatic and/or IGW scheme.
       CALL SOLVE_FOR_PRESSURE( myThid )

C--    Do "blocking" sends and receives for tendency "overlap" terms
       CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )

       myCurrentIter = myCurrentIter + 1
       myCurrentTime = myCurrentTime + deltaTClock 

C--    Save state for restarts
C      Note:
C      =====
C      Because of the ordering of the timestepping code and
C      tendency term code at end of loop model arrays hold
C      U,V,T,S  at "time-level" N but gu, gv, gs, gt, guNM1,... 
C      at "time-level" N+1/2 (guNM1 at "time-level" N+1/2 is 
C      gu at "time-level" N-1/2) and cg2d_x at "time-level" N+1/2.
C       where N = I+timeLevBase-1
C      Thus a checkpoint contains U.0000000000, GU.0000000001 and 
C      cg2d_x.0000000001 in the indexing scheme used for the model 
C      "state" files. This example is referred to as a checkpoint 
C      at time level 1 
       CALL WRITE_CHECKPOINT( .FALSE., myCurrentTime, myCurrentIter, myThid )

      ENDDO

C--   Final checkpoint (incase the in-loop checkpoint was missed)
      CALL WRITE_CHECKPOINT( .TRUE., myCurrentTime, myCurrentIter, myThid )

C--   Step-forward U/V/Theta/Salt for purposes of final I/O dump
      CALL DYNAMICS( myCurrentTime, myCurrentIter, myThid )

C--   Do time averages
#ifdef ALLOW_DIAGNOSTICS
      IF (taveFreq.GT.0.) THEN
        CALL WRITE_TIME_AVERAGES( myCurrentTime, myCurrentIter, myThid )
      ENDIF
#endif

C--   Dump for end state
      CALL WRITE_STATE( .FALSE., myCurrentTime, myCurrentIter, myThid )


      RETURN
      END
