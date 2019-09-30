
Introduction
------------

This section describes the :filelink:`pkg/flt` package and is largely based on the original documentation provided by `Arne Biastoch` and `Alistair Adcroft` circa 2001.
:filelink:`pkg/flt` computes float trajectories and simulates the behavior of profiling floats during a model run.
Profiling floats (e.g.) Argo) typically drift at depth and go back to the surface at pre-defined time intervals.
However, :filelink:`pkg/flt` can also simulate observing devices such as non-profiling floats or surface drifters.

The package's core functionalities are operated by the `flt_main` call in `forward_step` (see below for details). Checkpointing is supported via `flt_write_pickup` called in `packages_write_pickup`.

Time-stepping of float locations is based on a second- or fourth-order Runga-Kutta scheme (Press et al., 1992, Numerical Recipes).
Velocities and positions are interpolated between grid points to the simulated device location, and various types of noise can be added the simulated displacements.
Spatial interpolation is bilinear close to boundaries and otherwise a polynomial interpolation. Float positions are expressed in local grid index space.


Compile-time options in `FLT_OPTIONS.h`
---------------------------------------

.. tabularcolumns:: |\Y{.4}|L|L|


+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| CPP Flag Name                                 | Default | Description                                                                                                          |
+===============================================+=========+======================================================================================================================+
| :varlink:`ALLOW_3D_FLT`                       | #define | allow three-dimensional float displacements                                                                          |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`USE_FLT_ALT_NOISE`                  | #define | use alternative method of adding random noise                                                                        |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`ALLOW_FLT_3D_NOISE`                 | #define | add noise also to the vertical velocity of 3D floats                                                                 |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`FLT_SECOND_ORDER_RUNGE_KUTTA`       | #undef  | revert to old second-order Runge-Kutta                                                                               |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`FLT_WITHOUT_X_PERIODICITY`          | #undef  | prevent floats to re-enter the opposite side of a periodic domain                                                    |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`FLT_WITHOUT_Y_PERIODICITY`          | #undef  | prevent floats to re-enter the opposite side of a periodic domain                                                    |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`DEVEL_FLT_EXCH2`                    | #undef  | allow experimentation with pkg/flt + exch2 despite incomplete implementation                                         |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

Compile-time parameters in `FLT_SIZE.h` include:
------------------------------------------------

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

Run-time options in `data.flt` include:
---------------------------------------

`flt_int_traj`
   is the time interval in seconds to sample float position and dynamic variables (T,S,U,V,Eta).
   To capture the whole profile cycle of a PALACE float this has to be at least as small as the shortest surface time

`flt_int_prof`
   is the time interval in seconds to sample a whole profile of T,S,U,V (as well as
   positions and Eta). This has to chosen at least as small as the shortest profiling interval.

`flt_noise`
	If `FLT_NOISE` is defined then this is the amplitude that is added to the advection velocity by the random number generator.

`flt_file`
   is the base filename of the float positions without tile information and ending (e.g. `float_pos`)

`flt_selectTrajOutp`
   selects variables to output following float trajectories (=0 : none ; =1 : position only ; =2 : +p,u,v,t,s)

flt_selectProfOutp`
   selects variables to output when floats profile (=0 : none ; =1 : position only ; =2 : +p,u,v,t,s)

`flt_deltaT`
	 is equal to `deltaTClock` by default

`FLT_Iter0`
   is the time step when floats are initialized

`mapIniPos2Index`
   converts float initial positions to local, fractional indices (`.TRUE.` by default)

*Notes:* `flt_int_prof` is the time between getting profiles, not the the return  cycle of the float to the surface. The latter can be specified individually for every float. Because the mechanism
for returning to the surface is called in the profiling routine flt_int_prof has to be the minimum of all iup(max_npart). The subsampling of profiles can be done later in the analysis.

*Notes:* All profiling intervals have to be an integer multiple of `flt_int_prof`. The profile is always taken over the whole water column.
For example, let's assume that one wants a first set of floats with 5 days profiling interval and 24 hours surface time, and another one with 10 days profiling interval and 12 hours surface time.
To capture all of the floats motions, one then would have to set `flt_int_traj=43200` and `flt_int_prof=432000`.

Input Files
-----------

If `nIter0.EQ.FLT_Iter0` then `flt_init_varia` first looks for a global file (e.g. `float_pos.data`).
If that file does not exists then `flt_init_varia` looks for local files (e.g. `float_pos.001.001.data`, etc.)
or for local pickup files that have been generated during a previous model run (e.g. `pickup_flt.ckptA.001.001.data`, etc.).


The first line of these input file provides:

- the number of floats on that tile in the first record
- the total number of floats in the sixth record

*Notes:* when using a global file at first-time initialization both fields should be the same.

Afterwards the input files contain one 9-element double-precision record for each float:

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

*Notes:* an example how to write a float file (`write_float.F`) is included in the verification experiment documented below.

Output Files
------------

The output consists of 3 sets of local files:

- `pickup_flt*` : last positions of floats that can be used for restart
- `float_trajectories*` : trajectories of floats and actual values at depth
- `float_profiles*` : profiles throughout the whole water column

Verification Experiment
-----------------------

The verification experiment is based on `exp4` (flow over a Gaussian in a channel). The two main difference is that an additional wind forcing was introduced to speed up the currents.

A few utilities are included that were supposedly used to prepare input for `pkg/flt` and / or visualize its output:

::

	extra/cvfloat.F90
	extra/cvprofiles.F
	extra/write_float.F
	input/convert_ini.m
	input/read_flt_traj.m

Algorithm details
-----------------

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
