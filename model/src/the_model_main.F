C $Header: /u/gcmpack/MITgcm/model/src/the_model_main.F,v 1.9 1998/06/01 20:36:13 adcroft Exp $

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

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"
#include "DYNVARS.h"

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

C--   Initialise time-stepping (predictor variables are carried in G)
      CALL INIT_PREDICTOR( myThid )

C--   Begin time stepping loop
      DO I=1, nTimeSteps

C--    Load offline tracer fields
       IF ( MOD(myCurrentIter,numStepsPerPickup) .EQ. 1 ) THEN
C       I/O 
C       o Writes to arrays are performed by their own thread (to ensure
C         physical memory allocation will pair with thread).
C       o Thread 1 reads into a buffer.
C       CALL LOAD_OFFLINE_FIELDS( myCurrentTime, myCurrentIter, myThid )
       ENDIF

C--    Step forward fields and calculate time tendency terms
       CALL DYNAMICS( myCurrentTime, myCurrentIter, myThid )

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
C      Because of the ordereing of the timestepping code and
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

C--   Dump for end state and final checkpoint
      CALL WRITE_STATE( myCurrentTime, myCurrentIter, myThid )
      CALL WRITE_CHECKPOINT( .TRUE., myCurrentTime, myCurrentIter, myThid )


      RETURN
      END
