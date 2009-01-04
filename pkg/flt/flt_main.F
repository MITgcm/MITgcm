C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_main.F,v 1.3 2009/01/04 00:58:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

C     ==================================================================
C
C     Float Package for the MIT Model
C
C     Main Routines:
C
C     o flt_main      - Integrates the floats forward and stores
C                       positions and vertical profiles at specific
C                       time intervals.
C     o flt_init      - Initialise the floats
C     o flt_restart   - Writes restart data to file (=> renamed: flt_write_pickup)
C
C     Second Level Subroutines:
C
C     o flt_runga2    - Second order Runga-Kutta inetgration (default)
C     o flt_exchg     - Does a new distribution of floats over tiles
C                       after every integration step.
C     o flt_up        - moves float to the surface (if flag is set)
C                       and stores profiles to file
C     o flt_down      - moves float to its target depth (if flag is set)
C     o flt_traj      - stores positions and data to file
C     o flt_bilinear  - contains blinear interpolation scheme
C     o flt_functions - contains some functions
C     o flt_mdsreadvector - modified mdsreadvector to read files
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


      SUBROUTINE FLT_MAIN (
     I                      myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE FLT_MAIN
C     ==================================================================
C
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
Cc
C     ==================================================================
C     SUBROUTINE FLT_MAIN
C     ==================================================================

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FLT.h"

C     == routine arguments ==

C     myThid - thread number for this instance of the routine.

      _RL myTime
      INTEGER myIter, myThid

C     integration of the float trajectories

c         WRITE(0,*) ' bf call flt_runga2', myIter
         CALL TIMER_START('FLOATS RUNGA2      [FLT  LOOP]',myThid)
         CALL FLT_RUNGA2( myTime, myIter, myThid )
         CALL TIMER_STOP ('FLOATS RUNGA2      [FLT  LOOP]',myThid)
c         WRITE(0,*) ' af call flt_runga2', myIter

C     check if exchanges between tiles are necessary

         IF (Nx .NE. sNx .OR. Ny .NE. sNy) THEN
            CALL TIMER_START('FLOATS EXCHG       [FLT  LOOP]',myThid)
            CALL FLT_EXCHG( myTime, myIter, myThid )
            CALL TIMER_STOP ('FLOATS EXCHG       [FLT  LOOP]',myThid)
         ENDIF

C     store profiles every flt_int_prof time steps:
C     and move floats up and down

         IF ( MOD(myTime,flt_int_prof).EQ.0. ) THEN
            CALL TIMER_START('FLOATS UP          [FLT  LOOP]',myThid)
            CALL FLT_UP( myTime, myIter, myThid )
            CALL TIMER_STOP ('FLOATS UP          [FLT  LOOP]',myThid)
         ENDIF

c         WRITE(0,*) ' bf call flt_down', myIter
            CALL TIMER_START('FLOATS DOWN        [FLT  LOOP]',myThid)
            CALL FLT_DOWN( myTime, myIter, myThid )
            CALL TIMER_STOP ('FLOATS DOWN        [FLT  LOOP]',myThid)
c         WRITE(0,*) ' af call flt_down', myIter

C     store particles every flt_int_traj timesteps:

         IF ( MOD(myTime,flt_int_traj).EQ.0. ) THEN
c         WRITE(0,*) ' bf call flt_traj', myIter
            CALL TIMER_START('FLOATS TRAJ        [FLT  LOOP]',myThid)
            CALL FLT_TRAJ( myTime, myIter, myThid )
            CALL TIMER_STOP ('FLOATS TRAJ        [FLT  LOOP]',myThid)
c         WRITE(0,*) ' af call flt_traj', myIter
         ENDIF

      RETURN
      END
