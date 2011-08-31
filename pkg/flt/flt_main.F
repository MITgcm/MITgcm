C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_main.F,v 1.9 2011/08/31 21:33:50 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

C     ==================================================================
C
C     Float Package for the MIT Model
C
C     Main Routines:
C
C     o flt_main       - Integrates the floats forward and stores
C                        positions and vertical profiles at specific
C                        time intervals.
C     o flt_readparms  - Read parameter file
C     o flt_init_fixed - Initialise fixed
C     o flt_init_varia - Initialise the floats
C     o flt_restart    - Writes restart data to file (=> renamed: flt_write_pickup)
C
C     Second Level Subroutines:
C
C     o flt_runga2     - Second order Runga-Kutta inetgration (default)
C     o flt_exchg      - Does a new distribution of floats over tiles
C                        after every integration step.
C     o flt_up         - moves float to the surface (if flag is set)
C                        and stores profiles to file
C     o flt_down       - moves float to its target depth (if flag is set)
C     o flt_traj       - stores positions and data to file
C     o flt_interp_linear  - contains blinear interpolation scheme
C     o flt_mapping        - contains mapping functions & subroutine
C     o flt_mdsreadvector  - modified mdsreadvector to read files
C
C     ToDo:
C
C     o avoid exchanges when arrays empty
C     o 3D advection of floats
C
C     ==================================================================
C
C     Documentation:
C
C     To be made....
C
C
C     started: Arne Biastoch abiastoch@ucsd.edu 10-Jan-2000
C              (adopted from version written by Detlef Stammer
C               for the old model code)
C
C     changed: Arne Biastoch abiastoch@ucsd.edu 21-JUN-2001
C
C     ==================================================================

CBOP 0
C !ROUTINE: FLT_MAIN

C !INTERFACE:
      SUBROUTINE FLT_MAIN (
     I                      myTime, myIter, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     SUBROUTINE FLT_MAIN
C     ==================================================================
C     o This routine steps floats forward in time and samples the model
C       state at float position every flt_int_traj time steps.
C       Also moves the float up and down and samples vertical profiles.
C
C     o Uses  2nd or fourth order runga-kutta
C     o Spatial interpolation is bilinear close to boundaries and otherwise
C       a polynomial interpolation.
C     o Particles are kept in grid space (with position of dp taken as
C       x(south), y(east) grid cell point)
C     o Calls profile every flt_int_prof time steps; in that event the
C       profile over the whole water column is written to file and the
C       float might be moved upwards to the surface (depending on its
C       configuration).
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FLT_SIZE.h"
#include "FLT.h"

C     !INPUT PARAMETERS:
C     myTime :: current time in simulation
C     myIter :: current iteration number
C     myThid :: my Thread Id number
      _RL myTime
      INTEGER myIter, myThid

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER( 'FLT_MAIN', myThid )
#endif

C--   integration of the float trajectories
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('FLT_RUNGA*', myThid )
#endif
#ifdef FLT_SECOND_ORDER_RUNGE_KUTTA
c         WRITE(0,*) ' bf call flt_runga2', myIter
         CALL TIMER_START('FLOATS RUNGA2      [FLT  LOOP]',myThid)
         CALL FLT_RUNGA2( myTime, myIter, myThid )
         CALL TIMER_STOP ('FLOATS RUNGA2      [FLT  LOOP]',myThid)
c         WRITE(0,*) ' af call flt_runga2', myIter
#else
c         WRITE(0,*) ' bf call flt_runga4', myIter
         CALL TIMER_START('FLOATS RUNGA4      [FLT  LOOP]',myThid)
         CALL FLT_RUNGA4( myTime, myIter, myThid )
         CALL TIMER_STOP ('FLOATS RUNGA4      [FLT  LOOP]',myThid)
c         WRITE(0,*) ' af call flt_runga4', myIter
#endif

C--   do exchanges between tiles if necessary

#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('FLT_EXCH*', myThid )
#endif
         CALL TIMER_START('FLOATS EXCHG       [FLT  LOOP]',myThid)
#ifdef ALLOW_EXCH2
         CALL FLT_EXCH2( myTime, myIter, myThid )
#else
         CALL FLT_EXCHG( myTime, myIter, myThid )
#endif
         CALL TIMER_STOP ('FLOATS EXCHG       [FLT  LOOP]',myThid)

C--   store profiles every flt_int_prof time steps and move floats up and down

         IF (  DIFFERENT_MULTIPLE( flt_int_prof, myTime, deltaTClock )
     &      ) THEN
#ifdef ALLOW_DEBUG
           IF (debugMode) CALL DEBUG_CALL('FLT_UP', myThid )
#endif
           CALL TIMER_START('FLOATS UP          [FLT  LOOP]',myThid)
           CALL FLT_UP( myTime, myIter, myThid )
           CALL TIMER_STOP ('FLOATS UP          [FLT  LOOP]',myThid)
         ENDIF

#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('FLT_DOWN', myThid )
#endif
c         WRITE(0,*) ' bf call flt_down', myIter
           CALL TIMER_START('FLOATS DOWN        [FLT  LOOP]',myThid)
           CALL FLT_DOWN( myTime, myIter, myThid )
           CALL TIMER_STOP ('FLOATS DOWN        [FLT  LOOP]',myThid)
c         WRITE(0,*) ' af call flt_down', myIter

C--   store particles every flt_int_traj timesteps:

         IF (  DIFFERENT_MULTIPLE( flt_int_traj, myTime, deltaTClock )
     &      ) THEN
#ifdef ALLOW_DEBUG
           IF (debugMode) CALL DEBUG_CALL('FLT_TRAJ', myThid )
#endif
c         WRITE(0,*) ' bf call flt_traj', myIter
           CALL TIMER_START('FLOATS TRAJ        [FLT  LOOP]',myThid)
           CALL FLT_TRAJ( myTime, myIter, myThid )
           CALL TIMER_STOP ('FLOATS TRAJ        [FLT  LOOP]',myThid)
c         WRITE(0,*) ' af call flt_traj', myIter
         ENDIF

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE( 'FLT_MAIN', myThid )
#endif

      RETURN
      END
