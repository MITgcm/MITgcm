
Introduction
------------

This section describes the :filelink:`pkg/flt` package. Just below is a
reformatted and slightly updated version of the original documentation,
from `Arne Biastoch` and `Alistair Adcroft` back in the Summer of 2001,
found in `pkg/flt/README`.

This package allows the advection of floats during a model run.
Although originally intended to simulate PALACE floats
(floats that drift in at depth and to the surface at a defined
time interval) it can also run ALACE floats (non-profiling)
and surface drifters as well as sample moorings (simply a
non-advective, profiling float).
The stepping of the float advection is done using a second
order Runga-Kutta scheme (Press et al., 1992, Numerical
Recipes), whereby velocities and positions are bilinear
interpolated between the grid points.
The package has only few interfaces to the model. Despite a
general introduction of the flag useFLT and an initialization in
packages_init_fixed.F the interfaces are in:

- forward_step.F calls `flt_main`
- write_checkpoint.F calls `flt_write_pickup`

1. Compile-time options in `FLT_OPTIONS.h` include:

#define FLT_NOISE
   to add white noise to the advection velocity
#undef ALLOW_3D_FLT
   to allow three-dimensional float advection (not tested yet!) instead of drifting on a pre-defined (integer) vertical level.

2. Compile-time options in `FLT.h` include:

parameter (`max_npart_tile` = 300)
   is the maximum number of floats per tile. Should be smaller
   than the total number of floats when running on a parallel
   environment but as small as possible to avoid too large
   arrays. The model will stop if the number of floats per tile
   exceeds max_npart_tile at any time.
parameter (`max_npart_exch` = 50)
   is the maximum number of floats per tile that can be exchanged
   with other tiles to one side (there are 4 arrays) in one
   timestep. Should be generally small because only few floats
   leave the tile exactly at the same time.

3. Run-time options in `data.flt` include

`flt_int_traj`
   is the time interval in seconds to sample float position and dynamic variables (T,S,U,V,Eta).
   To capture the whole profile cycle of a PALACE float this has to be at least as small as the shortest surface time

`flt_int_prof`
   is the time interval in seconds to sample a whole profile of T,S,U,V (as well as
   positions and Eta). This has to chosen at least as small as the shortest profiling interval.

*Notes:* All profiling intervals have to be an integer multiple of this interval.
The profile is always taken over the whole water column.
For example, let's assume that one wants a first set of floats with
5 days profiling interval and 24 hours surface time, and another one
with 10 days profiling interval and 12 hours surface time.
To capture all of the floats motions, one then would have to set
`flt_int_traj=43200` and `flt_int_prof=432000`.

`flt_noise`
	If `FLT_NOISE` is defined then this is the amplitude that is added to the advection velocity by the random number generator.

`flt_file`
   is the base filename of the float positions without tile information and ending (e.g. `float_pos`)

3. Input files

The initialization is written in a way that it first looks for a
global file (e.g. `float_pos.data`). A global file is mainly used
for first-time initialization. If that not exists the routine looks
for local files (e.g. `float_pos.001.001.data`, etc.) that have
been used for storing the float positions for restart (note that
float restarts are ALWAYS local files).
The structure of the file is always the same. Each float contains
a 9 element double precision record of a
direct access file. The records are:

::

	npart   A unique float identifier (1,2,3,...)
	tstart  start date of integration of float (in s)
          - If tstart=-1 floats are integrated right from the beginning
	xpart   x position of float (in units of XC)
	ypart   y position of float (in units of YC)
	kpart   actual vertical level of float
	kfloat  target level of float
	       - should be the same as kpart at the beginning
	iup     flag if the float
          - should profile   ( >  0 = return cycle (in s) to surface)
          - remain at depth  ( =  0 )
          - is a 3D float    ( = -1 ).
          - should be advected WITHOUT additional noise ( = -2 ).
            (This implies that the float is non-profiling)
          - is a mooring     ( = -3 ), i.e. the float is not advected
	itop    time of float the surface (in s)
	tend    end date of integration of float (in s)
          - If tend=-1 floats are integrated till the end of the integration

In addition the first line of the file contains a record with

- the number of floats on that tile in the first record
- the total number of floats in the sixth record

At first-time initialization in a global file both fields should be the same.
An example how to write a float file (`write_float.F`) is included in the
verification experiment (see below).

4. Output/Visualization

The output always consists of 3 series of local files:

- files with last positions of floats that can be used for restart
- files with trajectories of floats and actual values at depth
- files with profiles throughout the whole water column

Examples and conversion routines for the second and third series
into NetCDF are included in `verification/flt_example/aux/`.

5. Verification Experiment

The verification experiment is based on `exp4` (flow over a
Gaussian in a channel). There are, however, two main differences
to the original experiment:

- The domain has closed boundaries. Currently the float package
  is not able to treat floats that leave the domain via open boundaries
- There is an additional wind forcing to speed up the currents
  to get significant advection rates in time

Package Folder Contents
-----------------------

A summary of included fortran files is provided inside `flt_main.F`:

::

	Main Routines:
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

The main computation is done by `flt_main.F` which steps floats forward in time
and samples the model state at float position every flt_int_traj time steps.
The code can also moves the float up and down and samples vertical profiles.
The original developers, in the early 2000s, noted that:

- Uses 2nd or fourth order Runga-Kutta
- Spatial interpolation is bilinear close to boundaries and otherwise a polynomial interpolation.
- Particles are kept in grid space (with position of dp taken as x(south), y(east) grid cell point)
- Calls profile every `flt_int_prof` time steps; in that event the profile over the whole water column is written to file and the float might be moved upwards to the surface (depending on its configuration).

A summary of what `flt_main.F` currently does is as follows:

::

		CALL FLT_RUNGA4
		  CALL FLT_TRILINEAR
		  or CALL FLT_BILINEAR
		or CALL FLT_RUNGA2
		  CALL FLT_TRILINEAR
		  or CALL FLT_BILINEAR
		CALL FLT_EXCH2
		  CALL EXCH2_SEND_PUT_VEC_RL
		  CALL EXCH2_RECV_GET_VEC_RL
		or CALL FLT_EXCHG
		  CALL EXCH_SEND_PUT_VEC_X_RL
		  CALL EXCH_RECV_GET_VEC_X_RL
		  CALL EXCH_SEND_PUT_VEC_Y_RL
		  CALL EXCH_RECV_GET_VEC_Y_RL
		CALL FLT_UP
		CALL FLT_DOWN
		CALL FLT_TRAJ


`verification/flt_example/`
---------------------------

This verification experiment has been used to test `pkg/flt`. It also contains
a few utility and documentation pieces that seem worth mentioning here. These
were supposedly used to prepare input for `pkg/flt` and / or visualize its
output. Not sure if any of these has recently been tested.

::

	extra/cvfloat.F90
	extra/cvprofiles.F
	extra/Makefile
	extra/write_float.F
	input/convert_ini.m
	input/read_flt_traj.m
