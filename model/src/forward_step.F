C $Header: /u/gcmpack/MITgcm/model/src/forward_step.F,v 1.10 2001/03/06 16:51:02 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE FORWARD_STEP(
     I                    iLoop,
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
C /|\   |  |--LOAD_EXTERNAL_DATA
C  |    |  |   o Load and/or set time dependent forcing fields
C /|\   |  |
C  |    |  |--DYNAMICS
C /|\   |  |   o Evaluate "forward" terms
C  |    |  |
C /|\   |  |
C  |    |  |--SOLVE_FOR_PRESSURE
C /|\   |  |   o Find pressure field to keep flow non-divergent
C  |    |  |
C /|\   |  |
C  |    |  |--THE_CORRECTION_STEP
C /|\   |  |   o Correct flow field with pressure gradient
C  |    |  |     and cycle time-stepping arrays (for all fields)
C /|\   |  |
C  |    |  |--DO_GTERM_BLOCKING_EXCHANGES
C /|\   |  |   o Update overlap regions
C  |    |  |
C /|\   |  |
C  |    |  |--DO_THE_MODEL_IO
C /|\   |  |   o Write model state
C  |    |  |
C /|\   |  |
C  |    |  |--WRITE_CHECKPOINT
C /|\   |  |   o Write restart file(s)
C  |    |
C /|\   |
C  |<== | ** Time stepping loop finishes here **
C

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif

C     == Routine arguments ==
C     iLoop         - Invocation count (counter in THE_MAIN_LOOP)
C     myCurrentIter - Iteration counter for this thread
C     myCurrentTime - Time counter for this thread
C     myThid - Thread number for this instance of the routine.
      INTEGER iLoop
      INTEGER myCurrentIter
      _RL     myCurrentTime
      INTEGER myThid  

C     == Local variables ==

C--   Load forcing/external data fields
      CALL TIMER_START('I/O (READ)         [FORWARD_STEP]',myThid)
      CALL EXTERNAL_FIELDS_LOAD( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('I/O (READ)         [FORWARD_STEP]',myThid)

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

#ifdef ALLOW_SHAP_FILT
C--   Step forward all tiles, filter and exchange.
      CALL TIMER_START('SHAP_FILT          [FORWARD_STEP]',myThid)
      CALL SHAP_FILT_APPLY( 
     I                     gUnm1, gVnm1, gTnm1, gSnm1,
     I                     myCurrentTime, myCurrentIter, myThid )
      IF (implicDiv2Dflow.LT.1.) THEN
C--   Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
         CALL SHAP_FILT_UV( uVel, vVel, myCurrentTime, myThid )
      ENDIF
      CALL TIMER_STOP ('SHAP_FILT          [FORWARD_STEP]',myThid)
#endif

#ifdef ALLOW_ZONAL_FILT
      IF (zonal_filt_lat.LT.90.) THEN 
      CALL ZONAL_FILT_APPLY(
     U           gUnm1, gVnm1, gTnm1, gSnm1,
     I           myThid )
      ENDIF
#endif

C--   Solve elliptic equation(s).
C     Two-dimensional only for conventional hydrostatic or 
C     three-dimensional for non-hydrostatic and/or IGW scheme.
      CALL TIMER_START('SOLVE_FOR_PRESSURE [FORWARD_STEP]',myThid)
      CALL SOLVE_FOR_PRESSURE( myThid )
      CALL TIMER_STOP ('SOLVE_FOR_PRESSURE [FORWARD_STEP]',myThid)

C--   Correct divergence in flow field and cycle time-stepping
C     arrays (for all fields) ; update time-counter
      myCurrentIter = nIter0 + iLoop
      myCurrentTime = startTime + deltaTClock * float(iLoop)
      CALL THE_CORRECTION_STEP(myCurrentTime, myCurrentIter, myThid)

C--   Do "blocking" sends and receives for tendency "overlap" terms
c     CALL TIMER_START('BLOCKING_EXCHANGES [FORWARD_STEP]',myThid)
c     CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )
c     CALL TIMER_STOP ('BLOCKING_EXCHANGES [FORWARD_STEP]',myThid)

C--   Do "blocking" sends and receives for field "overlap" terms
      CALL TIMER_START('BLOCKING_EXCHANGES [FORWARD_STEP]',myThid)
      CALL DO_FIELDS_BLOCKING_EXCHANGES( myThid )
      CALL TIMER_STOP ('BLOCKING_EXCHANGES [FORWARD_STEP]',myThid)

C--   Do IO if needed.
      CALL TIMER_START('I/O (WRITE)        [FORWARD_STEP]',myThid)
      CALL DO_THE_MODEL_IO(.FALSE., 
     &                     myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('I/O (WRITE)        [FORWARD_STEP]',myThid)

C--   Save state for restarts
C     Note:    (jmc: is it still the case after ckp35 ?)
C     =====
C     Because of the ordering of the timestepping code and
C     tendency term code at end of loop model arrays hold
C     U,V,T,S  at "time-level" N but gu, gv, gs, gt, guNM1,... 
C     at "time-level" N+1/2 (guNM1 at "time-level" N+1/2 is 
C     gu at "time-level" N-1/2) and etaN at "time-level" N+1/2.
C      where N = I+timeLevBase-1
C     Thus a checkpoint contains U.0000000000, GU.0000000001 and 
C     etaN.0000000001 in the indexing scheme used for the model 
C     "state" files. This example is referred to as a checkpoint 
C     at time level 1 
      CALL TIMER_START('I/O (WRITE)        [FORWARD_STEP]',myThid)
      CALL WRITE_CHECKPOINT(
     &        .FALSE., myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('I/O (WRITE)        [FORWARD_STEP]',myThid)


      RETURN
      END
