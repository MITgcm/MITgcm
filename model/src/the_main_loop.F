C $Header: /u/gcmpack/MITgcm/model/src/the_main_loop.F,v 1.1 2000/04/05 19:04:14 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE THE_MAIN_LOOP(myThid)
C     /==========================================================\
C     | SUBROUTINE THE_MAIN_LOOP                                 |
C     | o This contains the principal time loop                  |
C     |==========================================================|
C     | THE_MAIN_LOOP is invoked by THE_MODEL_MAIN               |
C     \==========================================================/
      IMPLICIT NONE
C
C     Call Tree
C     =========
C     
C       |-THE_MODEL_MAIN
C       |  |
C       |  |-THE_MAIN_LOOP
C       |  |  |
C       |  |  |--INITIALISE_VARIA
C       |  |  |   o Set initial conditions (variable arrays)
C       |  |  |
C  ==>  |  |  | ** Time stepping loop starts here **
C  |    |  |  |
C /|\   |  |  |
C  |    |  |  |--FORWARD_STEP
C /|\   |  |  | 
C  |    |  |  |
C  |<== |  |  | ** Time stepping loop finishes here **
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
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid  

C     == Local variables ==
C     Note: Under the multi-threaded model myCurrentIter and 
C           myCurrentTime are local variables passed around as routine 
C           arguments. Although this is fiddly it saves the need to 
C           impose additional synchronisation points when they are 
C           updated.
C     ILOOP         - Loop counter
C     myCurrentIter - Iteration counter for this thread
C     myCurrentTime - Time counter for this thread
      INTEGER ILOOP
      INTEGER  myCurrentIter
      _RL     myCurrentTime

      CALL TIMER_START('SPIN-UP            [THE_MAIN_LOOP]',myThid)

      myCurrentTime = startTime
      myCurrentIter = nIter0

C--   Set initial conditions (variable arrays)
      CALL TIMER_START('INITIALISE_VARIA   [THE_MAIN_LOOP]',myThid)
      CALL INITIALISE_VARIA( myThid )
      CALL TIMER_STOP ('INITIALISE_VARIA   [THE_MAIN_LOOP]',myThid)

C--   Dump starting state
      CALL TIMER_START('I/O (WRITE)        [THE_MAIN_LOOP]',myThid)
      CALL WRITE_STATE( .TRUE., myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('I/O (WRITE)        [THE_MAIN_LOOP]',myThid)

      CALL TIMER_STOP ('SPIN-UP            [THE_MAIN_LOOP]',myThid)

C     ===========================================================
C--   Begin time stepping loop
      CALL TIMER_START('MAIN LOOP          [THE_MAIN_LOOP]',myThid)
      DO ILOOP=1, nTimeSteps

C--    Step forward fields and calculate time tendency terms
       CALL TIMER_START('FORWARD_STEP       [THE_MAIN_LOOP]',myThid)
       CALL FORWARD_STEP( .FALSE., ILOOP,
     &                    myCurrentTime, myCurrentIter, myThid )
       CALL TIMER_STOP ('FORWARD_STEP       [THE_MAIN_LOOP]',myThid)

      ENDDO
      CALL TIMER_STOP ('MAIN LOOP          [THE_MAIN_LOOP]',myThid)
C     ===========================================================

      CALL TIMER_START('SPIN-DOWN          [THE_MAIN_LOOP]',myThid)

C--   Final checkpoint (incase the in-loop checkpoint was missed)
      CALL TIMER_START('I/O (WRITE)        [THE_MAIN_LOOP]',myThid)
      CALL WRITE_CHECKPOINT(
     &        .TRUE., myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('I/O (WRITE)        [THE_MAIN_LOOP]',myThid)

C--   Step forward fields for purposes of final I/O processing
      CALL TIMER_START('FORWARD_STEP       [THE_MAIN_LOOP]',myThid)
      CALL FORWARD_STEP( .TRUE., ILOOP,
     &                   myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('FORWARD_STEP       [THE_MAIN_LOOP]',myThid)

      CALL TIMER_STOP ('SPIN-DOWN          [THE_MAIN_LOOP]',myThid)

      RETURN
      END
