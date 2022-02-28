.. _sec_eg_baro:

Barotropic Ocean Gyre
=====================

  (in directory  :filelink:`verification/tutorial_barotropic_gyre/`)

This example experiment demonstrates using the MITgcm to simulate a barotropic, wind-forced, ocean gyre circulation.
The experiment is a numerical rendition of the gyre circulation problem described analytically
by Stommel in 1948  :cite:`stommel:48` and Munk in 1950 :cite:`munk:50`, and numerically in Bryan (1963) :cite:`bryan:63`.
Note this tutorial assumes a basic familiarity with ocean dynamics and geophysical fluid dynamics; readers new to the field
may which to consult one of the standard texts on these subjects,
such as Vallis (2017) :cite:`vallis:17` or Cushman-Roisin and Beckers (2011) :cite:`cushmanroisin:11`.

In this experiment the model is configured to represent a rectangular enclosed box of fluid, :math:`1200 \times 1200` km
in lateral extent. The fluid depth :math:`H =`  5 km. The fluid is forced by a zonal wind stress, :math:`\tau_x`, that varies
sinusoidally in the north-south direction and is constant in time. Topologically the grid is Cartesian and the Coriolis parameter :math:`f` is
defined according to a mid-latitude beta-plane equation

.. math::
    f(y) = f_{0}+\beta y

where :math:`y` is the distance along the 'north-south' axis of the simulated domain. For this experiment :math:`f_{0}` is
set to :math:`10^{-4}\text{s}^{-1}` and :math:`\beta = 10^{-11}\text{s}^{-1}\text{m}^{-1}`.

The sinusoidal wind-stress variations are defined according to

.. math::
   \tau_x(y) = -\tau_{0} \cos\left(\pi \frac{y}{L_y}\right)

where :math:`L_{y}` is the lateral domain extent and
:math:`\tau_0` is set to :math:`0.1 \text{N m}^{-2}`.

:numref:`baro_simulation_config` summarizes the configuration simulated.

  .. figure:: figs/new_barotropic_gyre.png
      :width: 100%
      :align: center
      :alt: barotropic gyre configuration
      :name: baro_simulation_config

      Schematic of simulation domain and wind-stress forcing function for barotropic gyre numerical experiment. The domain is enclosed by solid walls at :math:`x=` 0, 1200 km and at :math:`y=` 0, 1200 km.

Equations Solved
----------------

The model is configured in hydrostatic form (the MITgcm default). The implicit free surface form of the
pressure equation described in Marshall et al. (1997) :cite:`marshall:97a` is employed.
A horizontal Laplacian operator :math:`\nabla_{h}^2` provides viscous
dissipation. The wind-stress momentum input is added to the momentum equation
for the 'zonal flow', :math:`u`. This effectively yields an active set of
equations for this configuration as follows:

.. math::
   \frac{Du}{Dt} - fv + g\frac{\partial \eta}{\partial x} - A_{h}\nabla_{h}^2u
   = \frac{\tau_{x}}{\rho_{c}H}
   :label: baro_model_eq_u

.. math::
   \frac{Dv}{Dt} + fu + g\frac{\partial \eta}{\partial y} - A_{h}\nabla_{h}^2v
   = 0
   :label: baro_model_eq_v

.. math::
    \frac{\partial \eta}{\partial t} +  \nabla _{h}\cdot (H \vec{\bf u})
    = 0
    :label: baro_model_eq_eta

where :math:`u` and :math:`v` are the :math:`x` and :math:`y` components of the
flow vector :math:`\vec{\bf u}`, :math:`\eta` is the free surface height,
:math:`A_{h}` the horizontal Laplacian viscosity, :math:`\rho_{c}` is the fluid density,
and :math:`g` the acceleration due to gravity.

.. _sec_baro_num_config:

Discrete Numerical Configuration
--------------------------------

The domain is discretized with a uniform grid spacing in the horizontal set to :math:`\Delta x=\Delta y=20` km,
so that there are sixty ocean grid cells in the :math:`x` and :math:`y` directions. The numerical
domain includes a border row of "land" cell surrounding the ocean cells, so the numerical grid size is 62\ :math:`\times`\ 62
(if these land cells were not included, the domain would be periodic in both
the :math:`x` and :math:`y` directions).

Vertically the model is configured using a single layer in depth, :math:`\Delta z`, of 5000 m.

.. _barotropic_gyre_stab_crit:

Numerical Stability Criteria
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let’s start with our choice for the model’s time step. To minimize the amount of required computational resources, typically one
opts for as large a time step as possible while keeping the model solution stable. The advective
Courant–Friedrichs–Lewy (CFL) condition (see Adcroft 1995 :cite:`adcroft:95`) for an extreme
maximum horizontal flow speed is:

.. math::
    :label: eq_baro_cfl_stability

    S_{\rm adv} = 2 \left( \frac{ |u| \Delta t}{ \Delta x} \right) < 0.5 \text{ for stability}

The 2 factor on the left is because we have a 2-D problem
(in contrast with the more familiar 1-D canonical stability analysis); the right hand side is 0.5
due to our default use of Adams-Bashforth2 (see :numref:`adams-bashforth`) rather than the more familiar
value of 1 that one would obtain using a forward Euler scheme.
In our configuration, let’s assume our solution will achieve a maximum :math:`| u | = 1` ms\ :sup:`--1`
(in reality, current speeds in our solution will be much smaller). To keep :math:`\Delta t` safely
below the stability threshold, let’s choose :math:`\Delta t` = 1200 s (= 20 minutes), which
results in :math:`S_{\rm adv}` = 0.12.

The numerical stability for inertial oscillations using Adams-Bashforth II (Adcroft 1995 :cite:`adcroft:95`)

.. math::
    :label: eq_baro_inertial_stability

    S_{\rm inert} = f {\Delta t} < 0.5 \text{ for stability}

evaluates to 0.12 for our choice of :math:`\Delta t`, which is below the stability threshold.

There are two general rules in choosing a horizontal Laplacian eddy viscosity :math:`A_{h}`:

  - the resulting Munk layer width should be at least as large (preferably, larger) than the lateral grid spacing;
  - the viscosity should be sufficiently small that the model is stable for horizontal friction, given the time step.

Let’s use this first rule to make our choice for :math:`A_{h}`, and check this value using the second rule.
The theoretical Munk boundary layer width (as defined by the solution
zero-crossing, see Pedlosky 1987 :cite:`pedlosky:87`) is given by:

.. math::
    :label: baro_munk_layer

    M = \frac{2\pi}{\sqrt{3}} \left( \frac { A_{h} }{ \beta } \right) ^{\frac{1}{3}}

For our configuration we will choose to resolve a boundary layer of :math:`\approx` 100 km,
or roughly across five grid cells, so we set :math:`A_{h} = 400` m\ :sup:`2` s\ :sup:`--1`
(more precisely, this sets the full width at :math:`M` = 124 km). This choice ensures
that the frictional boundary layer is well resolved.

Given our choice of :math:`\Delta t`, the stability
parameter for the horizontal Laplacian friction (Adcroft 1995 :cite:`adcroft:95`)

.. math::
    :label: baro_laplacian_stability

    S_{\rm Lh} = 2 \left( 4 \frac{A_{h} \Delta t}{{\Delta x}^2} \right)  < 0.6 \text{ for stability}

evaluates to 0.0096, which is well below the stability threshold. As in :eq:`eq_baro_cfl_stability` the above criteria
is for a 2D problem using Adams-Bashforth2 time stepping, with the 0.6 value on the right replacing the more familiar 1
that is obtained using a forward Euler scheme.

See :numref:`adams-bashforth` for additional details on Adams-Bashforth time-stepping and numerical stability criteria.

.. _sec_eg_baro_code_config:

Configuration
-------------

The model configuration for this experiment resides under the directory :filelink:`verification/tutorial_barotropic_gyre/`.

The experiment files

 - :filelink:`verification/tutorial_barotropic_gyre/code/SIZE.h`
 - :filelink:`verification/tutorial_barotropic_gyre/input/data`
 - :filelink:`verification/tutorial_barotropic_gyre/input/data.pkg`
 - :filelink:`verification/tutorial_barotropic_gyre/input/eedata`
 - verification/tutorial_barotropic_gyre/input/bathy.bin
 - verification/tutorial_barotropic_gyre/input/windx_cosy.bin

contain the code customizations and parameter settings for this
experiment. Below we describe these customizations in detail.

Note: MITgcm’s defaults are configured to simulate an ocean rather than an atmosphere, with vertical :math:`z`-coordinates.
To model the ocean using pressure coordinates using MITgcm, additional parameter changes are required; see tutorial ocean_in_p.
To switch parameters to model an atmosphere, see tutorial Held_Suarez.

Compile-time Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~

File :filelink:`code/SIZE.h <verification/tutorial_barotropic_gyre/code/SIZE.h>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_barotropic_gyre/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/code/SIZE.h

Here we show a modified :filelink:`model/inc` source code file, customizing MITgcm’s array sizes to our model domain.
This file must be uniquely configured for any model setup; using the MITgcm default
:filelink:`model/inc/SIZE.h`  will in fact cause a compilation error.
Note that MITgcm's storage arrays are allocated as `static variables <https://en.wikipedia.org/wiki/Static_variable>`_
(hence their size must be declared in the source code), in contrast to some model codes which declare array sizes dynamically,
i.e., through runtime (namelist) parameter settings.

For this first tutorial, our setup and run environment is the most simple possible: we run on a single process
(i.e., NOT  `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ and NOT multi-threaded)
using a single model :ref:`"tile" <tile_description>`. For a more complete explanation of the parameter choices to use multiple tiles,
see the tutorial Baroclinic Gyre.

- These lines set parameters :varlink:`sNx` and :varlink:`sNy`, the number of grid points in the :math:`x` and :math:`y` directions, respectively.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/code/SIZE.h
       :start-at: sNx =
       :end-at: sNy =
       :lineno-match:

- These lines set parameters :varlink:`OLx` and :varlink:`OLy` in the :math:`x` and :math:`y` directions, respectively.
  These values are the overlap extent of a model tile, the purpose of which will be explained in later tutorials.
  Here, we simply specify the required minimum value (2)
  in both :math:`x` and :math:`y`.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/code/SIZE.h
       :start-at: OLx =
       :end-at: OLy =
       :lineno-match:

- These lines set parameters :varlink:`nSx`, :varlink:`nSy`, :varlink:`nPx`, and :varlink:`nPy`,
  the number of model tiles and the number of processes in the :math:`x` and :math:`y` directions, respectively.
  As discussed above, in this tutorial we configure a single model tile on
  a single process, so these parameters are all set to the value one.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/code/SIZE.h
       :start-at: nSx =
       :end-at: nPy =
       :lineno-match:

- This line sets parameter :varlink:`Nr`, the number of points in the vertical dimension. Here we use just a single vertical level.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/code/SIZE.h
       :start-at: Nr  =
       :end-at: Nr  =
       :lineno-match:

- Note these lines summarize the horizontal size of the model domain (**NOT** to be edited).

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/code/SIZE.h
       :start-at: sNx*nSx*nPx
       :end-at: sNy*nSy*nPy
       :lineno-match:

Further information and examples about how to configure :filelink:`model/inc/SIZE.h`
are given in :numref:`specify_decomp`.

Run-time Configuration
~~~~~~~~~~~~~~~~~~~~~~

.. _baro_gyre_data:

File :filelink:`input/data <verification/tutorial_barotropic_gyre/input/data>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/data

This file, reproduced completely above, specifies the main parameters
for the experiment. The parameters that are significant for this configuration
(shown with line numbers to left) are as follows.

PARM01 - Continuous equation parameters
#######################################

- This line sets parameter :varlink:`viscAh`, the horizontal Laplacian viscosity, to :math:`400` m\ :sup:`2` s\ :sup:`--1`.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: viscAh
       :end-at: viscAh
       :lineno-match:

- These lines set :math:`f_0` and :math:`\beta` (the Coriolis parameter :varlink:`f0` and
  the gradient of the Coriolis parameter :varlink:`beta`) for our beta-plane to
  :math:`1 \times 10^{-4}` s\ :sup:`--1` and :math:`1 \times 10^{-11}` m\ :sup:`--1`\ s\ :sup:`--1`, respectively.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: f0
       :end-at: beta
       :lineno-match:

- This line sets parameter :varlink:`rhoConst`, the Boussinesq reference density
  :math:`\rho_c` in :eq:`baro_model_eq_u`, to 1000 kg/m\ :sup:`3`.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: rhoConst
       :end-at: rhoConst
       :lineno-match:

- This line sets parameter :varlink:`gBaro`, the acceleration due to gravity :math:`g` (in the free surface terms
  in :eq:`baro_model_eq_u` and :eq:`baro_model_eq_v`), to  9.81 m/s\ :sup:`2`.
  This is the MITgcm default value, i.e., the value used if this line were not
  included in ``data``. One might alter this parameter for a reduced gravity model, or to simulate a different planet, for example.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: gBaro
       :end-at: gBaro
       :lineno-match:

- These lines set parameters :varlink:`rigidLid` and :varlink:`implicitFreeSurface` in order to
  suppress the rigid lid formulation of the surface pressure inverter and activate the implicit free surface formulation.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: rigidLid
       :end-at: implicitFreeSurface
       :lineno-match:

- This line sets parameter :varlink:`momAdvection` to suppress the (non-linear) momentum of advection
  terms in the momentum equations. However, note the ``#`` in column 1: this
  “comments out” the line, so using the above :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`
  file verbatim will in fact include the momentum advection terms (i.e., MITgcm default for this parameter is TRUE).
  We’ll explore the linearized solution (i.e., by removing the leading ``#``) in :numref:`barotropic_gyre_solution`.
  Note the ability to comment out a line in a namelist file is not part of standard Fortran,
  but this feature is implemented for all MITgcm namelist files.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: momAdvection
       :end-at: momAdvection
       :lineno-match:

- These lines set parameters :varlink:`tempStepping` and :varlink:`saltStepping` to
  suppress MITgcm’s forward time integration of temperature and salt in the tracer equations,
  as these prognostic variables are not relevant for the model solution in this configuration.
  By default, MITgcm solves equations governing these two (active) tracers; later tutorials will
  demonstrate how additional passive tracers can be included in the solution.
  The advantage of NOT solving the temperature and salinity equations is to
  eliminate many unnecessary computations. In most typical configurations however, one will want the model to
  compute a solution for :math:`T` and :math:`S`, which
  typically comprises the majority of MITgcm’s processing time.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: tempStepping
       :end-at: saltStepping
       :lineno-match:

PARM02 - Elliptic solver parameters
###################################

- The first line sets the tolerance (parameter :varlink:`cg2dTargetResidual`) that the 2-D conjugate gradient solver,
  the iterative method used in the pressure method algorithm, will use to test for convergence.
  The second line sets parameter :varlink:`cg2dMaxIters`, the maximum
  number of iterations.
  The solver will iterate until the residual falls below this target value
  (here, set to :math:`1 \times 10^{-7}`) or until this maximum number of solver iterations is reached
  (here, set to a maximum 1000 iterations). Typically, the solver will converge in far fewer than 1000 iterations, but
  it does not hurt to allow for a large number. The chosen value for the target residual
  happens to be the MITgcm default, and will serve well
  in most model configurations.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: cg2dTargetResidual
       :end-at: cg2dMaxIters
       :lineno-match:

.. _baro_time_stepping_parms:

PARM03 - Time stepping parameters
#################################

- This line sets the starting (integer) iteration number for the run. Here we set the value to zero, which starts the model from a new, initialized state.
  If :varlink:`nIter0` is non-zero, the model would require appropriate pickup files (i.e., restart files) in order to continue integration of an existing run.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: nIter0
       :end-at: nIter0
       :lineno-match:

- This line sets parameter :varlink:`nTimeSteps`, the (integer) number of timesteps the model will integrate forward. Below,
  we have set this to integrate for just 10 time steps, for MITgcm automated testing purposes (:numref:`code_testing_protocols`).
  To integrate the solution to near steady state,
  uncomment the line further down where we set the value to 77760 time steps. When you make this change,
  be sure to also uncomment the next line that sets :varlink:`monitorFreq` (see below).

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: nTimeSteps=10
       :end-at: nTimeSteps=10
       :lineno-match:

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: for longer
       :end-at: Freq=864
       :lineno-match:

- This line sets parameter :varlink:`deltaT`, the timestep used in stepping forward the model, to 1200 seconds.
  In combination with the larger value of :varlink:`nTimeSteps` mentioned above,
  we have effectively set the model to integrate forward for :math:`77760  \times 1200 \text{ s} = 3.0` years (based on 360-day years), long enough for the solution to approach equilibrium.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: deltaT
       :end-at: deltaT
       :lineno-match:

.. _barot_describe_checkp:

- These lines control the frequency at which restart (a.k.a. pickup) files are dumped by MITgcm.
  Here the value of :varlink:`pChkptFreq` is set to 31,104,000 seconds (=1.0 years) of model time;
  this controls the frequency of “permanent” checkpoint pickup files. With permanent files,
  the model’s iteration number is part of the file name (as the filename “suffix”; see :numref:`other_output`)
  in order to save it as a labelled, permanent, pickup state.
  The value of :varlink:`ChkptFreq` is set to 15,552,000 seconds (=0.5 years); the pickup files
  written at this frequency but will NOT include the iteration number in the filename,
  instead toggling between ``ckptA`` and ``ckptB`` in the filename, and thus these
  files will be overwritten with new data every 2 :math:`\times` 15,552,000 seconds.
  Temporary checkpoint files can be written more frequently without requiring additional disk space,
  for example to peruse (or re-run) the model state prior to an instability,
  or restart following a computer crash, etc.
  Either type of checkpoint file can be used to restart the model.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: pChkptFreq
       :end-at: chkptFreq
       :lineno-match:

- This line sets parameter :varlink:`dumpFreq`, frequency of writing model state
  snapshot diagnostics (of relevance in this setup: variables :math:`u`, :math:`v`, and :math:`\eta`).
  Here, we opt for a snapshot of model state every 15,552,000 seconds (=0.5 years), or after every 12960 time steps of integration.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: dumpFreq
       :end-at: dumpFreq
       :lineno-match:

- These lines are set to dump monitor output (see :numref:`pkg_monitor`) every 1200 seconds (i.e., every time step) to standard output.
  While this monitor frequency is needed for MITgcm automated testing, this is too much output for our tutorial run. Comment out this line
  and uncomment the line where :varlink:`monitorFreq` is set to 864,000 seconds, i.e., output every 10 days.
  Parameter :varlink:`monitorSelect` is set to 2 here to reduce output of non-applicable quantities for this simple example.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: monitorFreq
       :end-at: monitorSelect
       :lineno-match:

PARM04 - Gridding parameters
############################

- This line sets parameter :varlink:`usingCartesianGrid`, which specifies that the simulation will use a Cartesian coordinate system.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: usingCartesianGrid
       :end-at: usingCartesianGrid
       :lineno-match:

- These lines set the horizontal grid spacing of the model grid, as vectors :varlink:`delX` and :varlink:`delY`
  (i.e., :math:`\Delta x` and :math:`\Delta y` respectively).
  This syntax indicates that we specify 62 values in both the :math:`x` and :math:`y` directions, which matches the
  domain size as specified in :filelink:`SIZE.h <verification/tutorial_barotropic_gyre/code/SIZE.h>`.
  Grid spacing is set to :math:`20 \times 10^{3}` m (=20 km).

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: delX
       :end-at: delY
       :lineno-match:

- The cartesian grid default origin is (0,0) so here we set the origin with
  parameters :varlink:`xgOrigin` and :varlink:`ygOrigin` to (-20000,-20000),
  accounting for the bordering solid wall.
  The centers of the grid boxes will thus be at -10 km, 10 km, 30 km, 50 km, ...,
  in both :math:`x` and :math:`y` directions.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: xgOrigin
       :end-at: ygOrigin
       :lineno-match:

- This line sets parameter :varlink:`delR`, the vertical grid spacing in the :math:`z`-coordinate (i.e., :math:`\Delta z`), to 5000 m.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: delR
       :end-at: delR
       :lineno-match:

PARM05 - Input datasets
#######################

- This line sets parameter :varlink:`bathyFile`, the name of the bathymetry file.
  See :numref:`baro_gyre_bathy_file` for information about the file format.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: bathyFile
       :end-at: bathyFile
       :lineno-match:

- These lines specify the names of the files from which the surface wind stress is read.
  There is a separate file for the :math:`x`-direction (:varlink:`zonalWindFile`) and the :math:`y`-direction (:varlink:`meridWindFile`).
  Note, here we have left the latter parameter blank, as there is no meridional wind stress forcing in our example.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
       :start-at: zonalWindFile
       :end-at: meridWindFile
       :lineno-match:

File :filelink:`input/data.pkg <verification/tutorial_barotropic_gyre/input/data.pkg>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data.pkg
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/data.pkg

This file does not set any namelist parameters, yet is necessary to run --  only standard packages
(i.e., those compiled in MITgcm by default) are required for this setup, so no other customization is necessary.
We will demonstrate how to include additional packages
in other tutorial experiments.

File :filelink:`input/eedata <verification/tutorial_barotropic_gyre/input/eedata>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/eedata
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/eedata

This file uses standard default values (i.e., MITgcm default is single-threaded) and does not contain
customizations for this experiment.

.. _baro_gyre_bathy_file:

File ``input/bathy.bin``
^^^^^^^^^^^^^^^^^^^^^^^^

This file is a 2-D(:math:`x,y`) map of bottom bathymetry, specified as the :math:`z`-coordinate of the solid bottom boundary.
Here, the value is set to -5000 m everywhere except along the N, S, E, and W edges of the array, where the
value is set to 0 (i.e., “land”).   As discussed in :numref:`sec_baro_num_config`, the domain in MITgcm is assumed doubly periodic
(i.e., periodic in both :math:`x`- and :math:`y`-directions), so boundary walls
are necessary to set up our enclosed box domain.
The matlab program :filelink:`verification/tutorial_barotropic_gyre/input/gendata.m`
was used to generate this bathymetry file (alternatively, see python equivalent
:filelink:`gendata.py <verification/tutorial_barotropic_gyre/input/gendata.py>`). By default, this file is assumed to
contain 32-bit (single precision) binary numbers.
See :numref:`sec_mitgcm_inp_file_format` for additional information on MITgcm input data file format specifications.

.. _baro_gyre_windx_cosy:

File ``input/windx_cosy.bin``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similar to file ``input/bathy.bin``, this file is a 2-D(:math:`x,y`)
map of :math:`\tau_{x}` wind stress values, formatted in the same manner.
The units are Nm\ :sup:`--2`. Although :math:`\tau_{x}` is only a function of :math:`y` in this experiment,
this file must still define a complete 2-D map in order
to be compatible with the standard code for loading forcing fields
in MITgcm. The matlab program :filelink:`verification/tutorial_barotropic_gyre/input/gendata.m`
was used to generate this wind stress file (alternatively, see python equivalent
:filelink:`gendata.py <verification/tutorial_barotropic_gyre/input/gendata.py>`). 
To run the barotropic jet variation of this tutorial example (see :numref:`baro_jet_solution`),
you will in fact need to run one of these
programs to generate the file ``input/windx_siny.bin``.

.. _baro_gyre_build_run:

Building and running the model
------------------------------

To configure and compile the code (following the procedure described in :numref:`building_quickstart`):

::

  cd build
  ../../../tools/genmake2 -mods ../code ««-of my_platform_optionFile»»
  make depend
  make
  cd ..

To run the model (following the procedure in :numref:`run_the_model`):

::

  cd run
  ln -s ../input/* .
  ln -s ../build/mitgcmuv .
  ./mitgcmuv > output.txt

.. _barotropic_gyre_std_out:

Standard output
~~~~~~~~~~~~~~~

Your run’s standard output file should be similar to :filelink:`verification/tutorial_barotropic_gyre/results/output.txt`.
The standard output is essentially a log file of the model run. The following information is included (in rough order):

- startup information including MITgcm checkpoint release number and other execution environment information,
  and a list of activated packages (including all default packages, as well as optional packages).

- the text from all ``data.*`` and other critical files (in our example here,
  :filelink:`eedata <verification/tutorial_barotropic_gyre/input/eedata>`,
  :filelink:`SIZE.h <verification/tutorial_barotropic_gyre/code/SIZE.h>`,
  :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`,
  and :filelink:`data.pkg <verification/tutorial_barotropic_gyre/input/data.pkg>`).

- information about the grid and bathymetry, including dumps of all grid variables (only if Cartesian or spherical polar coordinates used, as is the case here).

- all runtime parameter choices used by the model, including all model defaults as well as user-specified parameters.

- monitor statistics at regular intervals (as specified by parameter
  :varlink:`monitorFreq` in :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`. See :numref:`pkg_monitor`).

- output from the 2-D conjugate gradient solver. More specifically, statistics from the right-hand side of the elliptic
  equation -- for our linear free-surface, see eq. :eq:`elliptic-backward-free-surface` -- are dumped for every model time step. If the model solution
  blows up, these statistics will increase to infinity, so one can see exactly when the problem occurred (i.e., to aid in debugging). Additional
  solver variables, such as number of iterations and residual, are included with the monitor statistics.

- a summary of end-of-run execution information, including user-, wall- and system-time elapsed during execution,
  and tile communication statistics.
  These statistics are provided for the overall run, and also broken down by time spent in various subroutines.

Different setups using non-standard packages and/or different parameter choices will include
additional or different output as part of the standard output. It is also possible to select more or less output
by changing the parameter :varlink:`debugLevel` in :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`; see (missing doc for pkg debug).

``STDERR.0000`` - if errors (or warnings) occurred during the run, helpful warning and/or error message(s) would appear in this file.

.. _other_output:

Other output files
~~~~~~~~~~~~~~~~~~

In addition to raw binary data files with ``.data`` extension, each binary file has a corresponding ``.meta`` file. These plain-text files include
information about the array size, precision (i.e., ``float32`` or ``float64``), and if relevant, time information  and/or
a list of these fields included in the binary file.  The ``.meta`` files are used by MITgcm :filelink:`utils` when binary data are read.

The following output files are generated:

**Grid Data**: see :numref:`spatial_discret_dyn_eq` for definitions and description of the `Arakawa C-grid <https://en.wikipedia.org/wiki/Arakawa_grids>`_
staggering of model variables.

- ``XC``, ``YC`` - grid cell center point locations
- ``XG``, ``YG`` - locations of grid cell vertices
- ``RC``, ``RF`` - vertical cell center and cell faces positions
- ``DXC``, ``DYC`` - grid cell center point separations (:numref:`hgrid-abcd` b)
- ``DXG``, ``DYG`` - separation of grid cell vertices (:numref:`hgrid-abcd` a)
- ``DRC``, ``DRF`` - separation of vertical cell centers and faces, respectively
- ``RAC``, ``RAS``, ``RAW``, ``RAZ`` - areas of the grid “tracer cells”, “southern cells”,
  “western cells” and “vorticity cells”, respectively (:numref:`hgrid-abcd`)
- ``hFacC``, ``hFacS``, ``hFacW`` - fractions of the grid cell in the vertical which are “open” as defined
  in the center and on the southern and western boundaries, respectively.
  These variables effectively contain the configuration bathymetric (or topographic) information.
- ``Depth`` - bathymetry depths

All these files contain 2-D(:math:`x,y`) data except ``RC``, ``RF``, ``DRC``, ``DRF``, which are 1-D(:math:`z`),
and ``hFacC``, ``hFacS``, ``hFacW``, which contain 3D(:math:`x,y,z`) data. Units for the grid files depends on one's choice of model grid;
here, they are all in given in meters (or :math:`\text{m}^2` for areas).

.. _tut_barotropic_tilenaming:

All the 2-D grid data files contain ``.001.001`` in their filename, e.g., ``DXC.001.001.data`` -- this is the tile number in ``.XXX.YYY`` format.
Here, we have just a single tile in both x and y, so both tile numbers are ``001``.
Using multiple tiles, the default is that the local tile grid information
would be output separately for each tile (as an example, see the baroclinic gyre tutorial,
which is set up using multiple tiles), producing multiple files for each 2-D grid variable.

**State Variable Snapshot Data**:

``Eta.0000000000.001.001.data, Eta.0000000000.001.001.meta`` - this is
a binary data snapshot of model dynamic variable
:varlink:`etaN` (the free-surface height) and its meta file, respectively.
Note the tile number is included in the filename, as is the iteration number ``0000000000``, which is simply the time step
(the iteration number here  is referred to as the “suffix” in
MITgcm parlance; there are options to change this suffix to something other than iteration number).
In other words, this is a dump of the free-surface height from the initialized state,
iteration 0; if you load up this data file,
you will see it is all zeroes. More interesting is the free-surface
height after some time steps have occurred. Snapshots are written according
to our parameter choice :varlink:`dumpFreq`, here set to 15,552,000 seconds, which is every 12960 time steps.
We will examine the model solutions in :numref:`barotropic_gyre_solution`.
The free-surface height is a 2-D(:math:`x,y`) field.

Snapshot files exist for other prognostic model variables, in particular
filenames starting with ``U`` (:varlink:`uVel`),
``V`` (:varlink:`uVel`), ``T`` (:varlink:`theta`), and ``S`` (:varlink:`salt`);
given our setup, these latter two fields
remain uniform in space and time, thus not very interesting until we
explore a baroclinic gyre setup in tutorial_baroclinic_gyre.
These are all 3-D(:math:`x,y,z`) fields. The format for the file names is similar
to the free-surface height files. Also dumped are snapshots
of diagnosed vertical velocity ``W`` (:varlink:`wVel`) (note that in non-hydrostatic
simulations, ``W`` is a fully prognostic model variable).

**Checkpoint Files**:

The following pickup files are generated:

- ``pickup.0000025920.001.001.data``, ``pickup.0000025920.001.001.meta``, etc. - written at frequency set by :varlink:`pChkptFreq`
- ``pickup.ckptA.001.001.data``, ``pickup.ckptA.001.001.meta``, ``pickup.ckptB.001.001.data``,
  ``pickup.ckptB.001.001.meta`` - written at frequency set by :varlink:`ChkptFreq`

**Other Model Output Data**: Model output related to reference density and hydrostatic pressure, in files ``Rhoref``, ``PHrefC``, ``PHrefF``, ``PH``, and ``PHL``,
is discussed in depth :ref:`here <phi_hyd_discussion>` in tutorial :ref:`tutorial_baroclinic_gyre` (as these data are not terribly interesting in this single-layer setup).

.. _barotropic_gyre_solution:

Model Solution
--------------

After running the model for 77,760 time steps (3.0 years), the solution is near equilibrium.
Given an approximate timescale of one month for barotropic Rossby waves
to cross our model domain, one might expect the solution to require several years to achieve an equilibrium state.
The model solution of free-surface
height :math:`\eta` (proportional to streamfunction) at :math:`t=` 3.0 years is shown in :numref:`barotropic_nl_soln`.
For further details on this solution, particularly examining the effect of the non-linear terms with increasing Reynolds number,
the reader is referred to Pedlosky (1987) :cite:`pedlosky:87` section 5.11.

  .. figure:: figs/full_solution.*
      :width: 80%
      :align: center
      :alt: barotropic gyre full solution
      :name: barotropic_nl_soln

      MITgcm solution to the barotropic gyre example after :math:`t=` 3.0 years of model integration. Free surface height is shown in meters.

Using matlab for example, visualizing output using the :filelink:`utils/matlab/rdmds.m` utility to load the
binary data in ``Eta.0000077760.001.001.data`` is as simple as:

::

   addpath ../../../utils/matlab/
   XC=rdmds('XC');
   YC=rdmds('YC');
   Eta=rdmds('Eta', 77760);
   contourf(XC/1000, YC/1000, Eta, [-.04:.01:.04])
   colorbar
   colormap((flipud(hot)))
   set(gca, 'XLim', [0 1200])
   set(gca, 'YLim', [0 1200])

or using python (you will need to install the MITgcmutils package, see :numref:`MITgcmutils`):

::

   from MITgcmutils import mds
   import numpy as np
   import matplotlib.pyplot as plt
   XC = mds.rdmds('XC')
   YC = mds.rdmds('YC')
   Eta = mds.rdmds('Eta', 77760)
   plt.contourf(XC/1000, YC/1000, Eta, np.linspace(-0.02, 0.05,8), cmap='hot_r')
   plt.colorbar()
   plt.show()

(for a more involved example with detailed explanations how to read in model output, 
perform calculations using these data, and make plots, see tutorial :ref:`Baroclinic Ocean Gyre <baroclinic_gyre_solution>`)

Let’s simplify the example by considering the linear problem where we neglect the advection of momentum terms.
In other words, replace :math:`\frac{Du}{Dt}` and :math:`\frac{Dv}{Dt}` with
:math:`\frac{\partial u}{\partial t}` and :math:`\frac{\partial v}{\partial t}`, respectively,
in in :eq:`baro_model_eq_u` and :eq:`baro_model_eq_v`.
To do so, we uncomment (i.e., remove the leading ``#``) in the
line  ``# momAdvection=.FALSE.,`` in file ``data`` and re-run the model. Any existing output files will be overwritten.

For the linearized equations, the Munk layer (equilibrium) analytical solution is given by:

.. math::
   \eta(x,y) = \frac{\tau_o}{\rho_c g H} \frac{f}{\beta} \left(1 - \frac{x}{L_x}\right) \pi \sin\left(\pi \frac{y}{L_y}\right)
   \left[1 - e^{-x/(2\delta_m)} \left(\cos\frac{\sqrt{3}x}{2\delta_m} + \frac{1}{\sqrt{3}} \sin\frac{\sqrt{3}x}{2\delta_m} \right) \right]

where :math:`\delta_m = (A_{h} / \beta)^{\frac{1}{3}}`. :numref:`lin_anal_soln`
displays the MITgcm output after switching off momentum advection vs.
the analytical solution to the linearized equations. Success!

  .. figure:: figs/lin_anal_soln.*
      :width: 100%
      :align: center
      :alt: barotropic gyre linearized solution
      :name: lin_anal_soln

      Comparison of free surface height for the near-equilibrium MITgcm solution (:math:`t=` 3.0 years) with momentum advection switched off (left) and the analytical equilibrium solution to the linearized equation (right).

Finally, let’s examine one additional simulation where we change the cosine profile of wind stress forcing to a sine profile.
First, run the matlab script :filelink:`verification/tutorial_barotropic_gyre/input/gendata.m` to generate the alternate sine
profile wind stress, and place a copy in your run directory. Then,
in file :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`,
replace the line ``zonalWindFile='windx_cosy.bin’,`` with ``zonalWindFile='windx_siny.bin’,``.

  .. figure:: figs/baro_jet_solution.*
      :width: 80%
      :align: center
      :alt: barotropic jet linearized solution
      :name: baro_jet_solution

      MITgcm equilibrium solution to the barotropic setup with alternate sine profile of wind stress forcing, producing a barotropic jet.

The free surface solution given this forcing is shown in :numref:`baro_jet_solution`. Two “half gyres” are separated by a strong jet.
We’ll look more at the solution to this “barotropic jet” setup in later tutorial examples.
