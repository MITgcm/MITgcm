C $Header: /u/gcmpack/MITgcm/model/src/forward_step.F,v 1.4 2000/11/13 16:32:58 heimbach Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE FORWARD_STEP(
     I                    doHalfStep, iLoop,
     U                    myCurrentTime, myCurrentIter,
     &                    myThid)
C     /==========================================================\
C     | SUBROUTINE FORWARD_STEP                                  |
C     | o Does one instance of the model time stepping           |
C     |   The time stepping loop in THE_MAIN_LOOP() calls        |
C     |   this routine                                           |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
C
C     Call Tree
C     =========
C     
C      THE_MAIN_LOOP()
C       |
C  ==>  | ** Time stepping loop starts here **
C  |    |
C /|\   |-FORWARD_STEP
C  |    |  |
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
C /|\   |
C  |    |
C  |<== | ** Time stepping loop finishes here **
C

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif

C     == Routine arguments ==
C     doHalfStep - If .TRUE. then this is the last half step
C     iLoop         - Invocation count (counter in THE_MAIN_LOOP)
C     myCurrentIter - Iteration counter for this thread
C     myCurrentTime - Time counter for this thread
C     myThid - Thread number for this instance of the routine.
      LOGICAL doHalfStep
      INTEGER iLoop
      INTEGER myCurrentIter
      _RL     myCurrentTime
      INTEGER myThid  

C     == Local variables ==

C--   Load forcing/external data fields
      CALL TIMER_START('I/O (READ)         [FORWARD_STEP]',myThid)
      CALL EXTERNAL_FIELDS_LOAD( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('I/O (READ)         [FORWARD_STEP]',myThid)

#ifdef INCLUDE_SHAPIRO_FILTER_CODE
C--   Step forward all tiles, filter and exchange.
      CALL TIMER_START('SHAP_FILT          [FORWARD_STEP]',myThid)
      CALL SHAP_FILT( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('SHAP_FILT          [FORWARD_STEP]',myThid)
#endif

C--   Step forward fields and calculate time tendency terms
      CALL TIMER_START('DYNAMICS           [FORWARD_STEP]',myThid)
      CALL DYNAMICS( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('DYNAMICS           [FORWARD_STEP]',myThid)

#ifdef ALLOW_NONHYDROSTATIC
C--   Step forward W field in N-H algorithm
      IF ( nonHydrostatic ) THEN
       CALL TIMER_START('CALC_GW            [FORWARD_STEP]',myThid)
       CALL CALC_GW( myThid)
       CALL TIMER_STOP ('CALC_GW            [FORWARD_STEP]',myThid)
      ENDIF
#endif

C--   Do IO if needed.
C     Note:
C     =====
C     At this point model arrays hold U,V,T,S  at "time-level" N 
C     and cg2d_x at "time-level" N-1/2 where N = I+timeLevBase-1. 
C     By convention this is taken to be the model "state". 
      CALL TIMER_START('I/O (WRITE)        [FORWARD_STEP]',myThid)
      CALL DO_THE_MODEL_IO(
     &        doHalfStep, myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('I/O (WRITE)        [FORWARD_STEP]',myThid)


      IF (.NOT. doHalfStep) THEN

C--   Solve elliptic equation(s).
C     Two-dimensional only for conventional hydrostatic or 
C     three-dimensional for non-hydrostatic and/or IGW scheme.
      CALL TIMER_START('SOLVE_FOR_PRESSURE [FORWARD_STEP]',myThid)
      CALL SOLVE_FOR_PRESSURE( myThid )
      CALL TIMER_STOP ('SOLVE_FOR_PRESSURE [FORWARD_STEP]',myThid)

C--   Do "blocking" sends and receives for tendency "overlap" terms
      CALL TIMER_START('BLOCKING_EXCHANGES [FORWARD_STEP]',myThid)
      CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )
      CALL TIMER_STOP ('BLOCKING_EXCHANGES [FORWARD_STEP]',myThid)

c     myCurrentIter = myCurrentIter + 1
c     myCurrentTime = myCurrentTime + deltaTClock 
      myCurrentIter = nIter0 + iLoop
      myCurrentTime = startTime + deltaTClock * float(iLoop)

C--   Save state for restarts
C     Note:
C     =====
C     Because of the ordering of the timestepping code and
C     tendency term code at end of loop model arrays hold
C     U,V,T,S  at "time-level" N but gu, gv, gs, gt, guNM1,... 
C     at "time-level" N+1/2 (guNM1 at "time-level" N+1/2 is 
C     gu at "time-level" N-1/2) and cg2d_x at "time-level" N+1/2.
C      where N = I+timeLevBase-1
C     Thus a checkpoint contains U.0000000000, GU.0000000001 and 
C     cg2d_x.0000000001 in the indexing scheme used for the model 
C     "state" files. This example is referred to as a checkpoint 
C     at time level 1 
      CALL TIMER_START('I/O (WRITE)        [FORWARD_STEP]',myThid)
      CALL WRITE_CHECKPOINT(
     &        .FALSE., myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('I/O (WRITE)        [FORWARD_STEP]',myThid)

      ENDIF

      RETURN
      END
