C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_main.F,v 1.2 2008/12/03 01:35:42 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

c     ==================================================================
c
c     Float Package for the MIT Model
c
c     Main Routines:
c
c     o flt_main      - Integrates the floats forward and stores
c                       positions and vertical profiles at specific
c                       time intervals.
c     o flt_init      - Initialise the floats
c     o flt_restart   - Writes restart data to file.
c
c     Second Level Subroutines:
c
c     o flt_runga2    - Second order Runga-Kutta inetgration (default)
c     o flt_exchg     - Does a new distribution of floats over tiles
c                       after every integration step.
c     o flt_up        - moves float to the surface (if flag is set)
c                       and stores profiles to file
c     o flt_down      - moves float to its target depth (if flag is set)
c     o flt_traj      - stores positions and data to file
c     o flt_bilinear  - contains blinear interpolation scheme
c     o flt_functions - contains some functions
c     o flt_mdsreadvector - modified mdsreadvector to read files
c
c     ToDo:
c
c     o avoid exchanges when arrays empty
c     o 3D advection of floats
c
c     ==================================================================
c
c     Documentation:
c
c     To be made....
c
c
c     started: Arne Biastoch abiastoch@ucsd.edu 10-Jan-2000
c              (adopted from version written by Detlef Stammer
c               for the old model code)
c
c     changed: Arne Biastoch abiastoch@ucsd.edu 21-JUN-2001
c
c     ==================================================================


      SUBROUTINE FLT_MAIN (
     I                      myIter,
     I                      myTime,
     I                      myThid
     &                     )

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

C     mythid - thread number for this instance of the routine.

      INTEGER myIter, myThid
      _RL myTime

C     integration of the float trajectories

c         write(0,*) ' bf call flt_runga2', myIter
         CALL TIMER_START('FLOATS RUNGA2      [FLT  LOOP]',myThid)
         call flt_runga2( myIter, myTime, myThid )
         CALL TIMER_STOP ('FLOATS RUNGA2      [FLT  LOOP]',myThid)
c         write(0,*) ' af call flt_runga2', myIter

C     check if exchanges between tiles are necessary

         if (Nx .ne. sNx .or. Ny .ne. sNy) then
            CALL TIMER_START('FLOATS EXCHG       [FLT  LOOP]',myThid)
            call flt_exchg( myIter, myTime, myThid )
            CALL TIMER_STOP ('FLOATS EXCHG       [FLT  LOOP]',myThid)
         endif

C     store profiles every flt_int_prof time steps:
C     and move floats up and down

         if (mod(myTime,flt_int_prof).eq.0.) then
            CALL TIMER_START('FLOATS UP          [FLT  LOOP]',myThid)
            call flt_up( myIter, myTime, myThid )
            CALL TIMER_STOP ('FLOATS UP          [FLT  LOOP]',myThid)
         endif

c         write(0,*) ' bf call flt_down', myIter
            CALL TIMER_START('FLOATS DOWN        [FLT  LOOP]',myThid)
            call flt_down( myIter, myTime, myThid )
            CALL TIMER_STOP ('FLOATS DOWN        [FLT  LOOP]',myThid)
c         write(0,*) ' af call flt_down', myIter

C     store particles every flt_int_traj timesteps:

         if (mod(myTime,flt_int_traj).eq.0.) then
c         write(0,*) ' bf call flt_traj', myIter
            CALL TIMER_START('FLOATS TRAJ        [FLT  LOOP]',myThid)
            call flt_traj( myIter, myTime, myThid )
            CALL TIMER_STOP ('FLOATS TRAJ        [FLT  LOOP]',myThid)
c         write(0,*) ' af call flt_traj', myIter
         endif

      RETURN
      END
