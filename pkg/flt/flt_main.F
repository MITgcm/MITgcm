C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_main.F,v 1.1 2001/09/13 17:43:55 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"

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


      subroutine flt_main (
     I                      myCurrentIter, 
     I                      myCurrentTime, 
     I                      myThid
     &                     )

c     ==================================================================
c     SUBROUTINE flt_main
c     ==================================================================
c
c     o This routine steps floats forward in time and samples the model
c       state at float position every flt_int_traj time steps.
c       Also moves the float up and down and samples vertical profiles.
c
c     o Uses  2nd or fourth order runga-kutta 
c     o Spatial interpolation is bilinear close to boundaries and otherwise
c       a polynomial interpolation.
c     o Particles are kept in grid space (with position of dp taken as 
c       x(south), y(east) grid cell point)
c     o Calls profile every flt_int_prof time steps; in that event the 
c       profile over the whole water column is written to file and the
c       float might be moved upwards to the surface (depending on its
c       configuration).
cc
c     ==================================================================
c     SUBROUTINE flt_main
c     ==================================================================

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FLT.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      INTEGER myCurrentIter, myThid
      _RL myCurrentTime

c     integration of the float trajectories
c
         CALL TIMER_START('FLOATS RUNGA2      [FLT  LOOP]',myThid)
         call flt_runga2(myCurrentIter,myCurrentTime,myThid)
         CALL TIMER_STOP ('FLOATS RUNGA2      [FLT  LOOP]',myThid)

c     check if exchanges between tiles are necessary
c
         if (Nx .ne. sNx .or. Ny .ne. sNy) then
            CALL TIMER_START('FLOATS EXCHG       [FLT  LOOP]',myThid)
            call flt_exchg(myCurrentIter,myCurrentTime,myThid)
            CALL TIMER_STOP ('FLOATS EXCHG       [FLT  LOOP]',myThid)
         endif

c     store profiles every flt_int_prof time steps:
c     and move floats up and down 
c

         if (mod(myCurrentTime,flt_int_prof).eq.0.) then
            CALL TIMER_START('FLOATS UP          [FLT  LOOP]',myThid)
            call flt_up(myCurrentIter,myCurrentTime,myThid)
            CALL TIMER_STOP ('FLOATS UP          [FLT  LOOP]',myThid)
         endif

            CALL TIMER_START('FLOATS DOWN        [FLT  LOOP]',myThid)
            call flt_down(MyCurrentIter,myCurrentTime,myThid)
            CALL TIMER_STOP ('FLOATS DOWN        [FLT  LOOP]',myThid)

c     store particles every flt_int_traj timesteps:
c
         if (mod(myCurrentTime,flt_int_traj).eq.0.) then
            CALL TIMER_START('FLOATS TRAJ        [FLT  LOOP]',myThid)
            call flt_traj(myCurrentIter,myCurrentTime,myThid)
            CALL TIMER_STOP ('FLOATS TRAJ        [FLT  LOOP]',myThid)
         endif

      return
      end
