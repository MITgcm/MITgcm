C $Header: /u/gcmpack/MITgcm/model/src/the_model_main.F,v 1.5 1998/04/24 03:32:40 cnh Exp $

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
C     Note: Under the multi-threaded model myCurrentIter and myCurrentTime
C           are local variables passed around as routine arguments. Although
C           this is fiddly it saves the need to impose additional synchronisation
C           points when they are updated.
C     myCurrentIter - Iteration counter for this thread
C     myCurrentTime - Time counter for this thread
C     I             - Loop counter
      INTEGER I, myCurrentIter
      REAL    myCurrentTime

C--   Set model initial conditions 
      CALL INITIALISE( myThid )
      myCurrentTime = startTime
      myCurrentIter = nIter0

C--   Begin time stepping loop
      DO I=1, nTimeSteps

C--    Do IO if needed.
       CALL DO_THE_MODEL_IO( myCurrentIter, myThid )

C--    Load offline tracer fields
       IF ( MOD(myCurrentIter,numStepsPerPickup) .EQ. 1 ) THEN
C       I/O 
C       o Writes to arrays are performed by their own thread (to ensure
C         physical memory allocation will pair with thread).
C       o Thread 1 reads into a buffer.
C       CALL LOAD_OFFLINE_FIELDS( myCurrentTime, myCurrentIter, myThid )
       ENDIF

C--    Step forward fields and calculate time tendency terms
       CALL DYNAMICS( myThid )

C--    Solve elliptic equation(s).
C      Two-dimensional only for conventional hydrostatic or three-dimensional
C      for non-hydrostatic and/or IGW scheme.
       CALL SOLVE_FOR_PRESSURE( myThid )

C--    Do "blocking" sends and receives for tendency "overlap" terms
       CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )

       myCurrentIter = myCurrentIter + 1
       myCurrentTime = myCurrentTime  + deltaTtracer

C--    Save state for restarts
C*P*   CALL CHECKPOINT( myThid )

      ENDDO

      RETURN
      END
