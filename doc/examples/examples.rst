.. _chap_modelExamples:

MITgcm Tutorial Example Experiments
***********************************

The full MITgcm distribution comes with a set of pre-configured
numerical experiments.  Some of these example experiments are tests of
individual parts of the model code, but many are fully fledged
numerical simulations. Full tutorials exist for a few of the examples,
and are documented in sections :numref:`sec_eg_baro` -
:numref:`sec_eg_tank`. The other examples follow the same general
structure as the tutorial examples. However, they only include brief
instructions in text file README.  The examples are
located in subdirectories under the directory :filelink:`verification`.
Each example is briefly described below.

.. _sec_eg_baro:

Barotropic Gyre MITgcm Example
==============================

  (in directory  :filelink:`verification/tutorial_barotropic_gyre/`)


This example experiment demonstrates using the MITgcm to simulate a barotropic, wind-forced, ocean gyre circulation.
The experiment is a numerical rendition of the gyre circulation problem described analytically
by Stommel in 1948  :cite:`stommel:48` and Munk in 1950 :cite:`munk:50`, and numerically in Bryan (1963) :cite:`bryan:63`.

In this experiment the model is configured to represent a rectangular enclosed box of fluid, :math:`1200 \times 1200` km
in lateral extent. The fluid depth :math:`D =`  5 km. The fluid is forced by a zonal wind stress, :math:`\tau_x`, that varies
sinusoidally in the north-south direction and is constant in time. Topologically the grid is Cartesian and the Coriolis parameter :math:`f` is
defined according to a mid-latitude beta-plane equation

.. math::
    f(y) = f_{0}+\beta y

where :math:`y` is the distance along the 'north-south' axis of the simulated domain. For this experiment :math:`f_{0}` is
set to :math:`10^{-4}\text{s}^{-1}` and :math:`\beta = 10^{-11}\text{s}^{-1}\text{m}^{-1}`.


The sinusoidal wind-stress variations are defined according to 

.. math:: 
   \tau_x(y) = -\tau_{0}\cos(\pi \frac{y}{L_y})

 
where :math:`L_{y}` is the lateral domain extent and
:math:`\tau_0` is set to :math:`0.1 \text{N m}^{-2}`. 


:numref:`baro_simulation_config` summarizes the configuration simulated.


  .. figure:: barotropic_gyre/figs/new_barotropic_gyre.*
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
   = \frac{\tau_{x}}{\rho_{c}D}
   :label: baro_model_eq_u

.. math::
   \frac{Dv}{Dt} + fu + g\frac{\partial \eta}{\partial y} - A_{h}\nabla_{h}^2v
   = 0
   :label: baro_model_eq_v

.. math::
    \frac{\partial \eta}{\partial t} + \nabla_{h}\cdot \vec{u}
    = 0
    :label: baro_model_eq_eta


where :math:`u` and :math:`v` are the :math:`x` and :math:`y` components of the
flow vector :math:`\vec{u}`, :math:`\eta` is the free surface height,
and :math:`A_{h}` the horizontal Laplacian viscosity. 


Discrete Numerical Configuration
--------------------------------

The domain is discretized with a uniform grid spacing in the horizontal set to :math:`\Delta x=\Delta y=20` km, so that there are sixty grid cells in the :math:`x` and :math:`y` directions. Vertically the model is configured with a single layer with depth, :math:`\Delta z`, of 5000 m. 


Numerical Stability Criteria
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let’s start with our choice for the model’s time step. To minimize the amount of required computational resources, typically one
opts for as large a time step as possible while keeping the model solution stable. The advective
Courant–Friedrichs–Lewy (CFL) condition (see Adcroft 1995 :cite:`adcroft:95`) for an extreme
maximum horizontal flow speed is:

.. math::
    :label: eq_baro_cfl_stability

    S_{a} = \frac{| \vec{u} | \delta t}{ \Delta x} < 0.5 \text{ for stability}

In our configuration, let’s assume our solution will achieve a maximum :math:`{|\vec{u}|}=2` ms\ :sup:`--1`. To keep :math:`\delta t` safely
below the stability criteria, let’s choose :math:`\delta t` = 1200 s (= 20 minutes), which 
results in :math:`S_{a}` = 0.12.

The numerical stability for inertial oscillations (Adcroft 1995 :cite:`adcroft:95`)

.. math::
    :label: eq_baro_inertial_stability

    S_{i} = f^{2} {\delta t}^2 < 0.5 \text{ for stability}


evaluates to 0.014 for our choice of :math:`\delta t`, which is well below the stability criteria.

There are two general rules in choosing a lateral Laplacian eddy viscosity :math:`A_{h}`:

  - the resulting Munk layer width should be at least as large (preferably, larger) than the lateral grid spacing;
  - the viscosity should be sufficiently large that the model is stable, given the time step. 

Let’s use this first rule to dictate our choice of :math:`A_{h}`.
The theoretical Munk boundary layer width (as defined by the solution
zero-crossing, see Pedlosky 1987 :cite:`pedlosky:87`) is given by: 

.. math::
    :label: baro_munk_layer
    
    M_{w} = \frac{2\pi}{\sqrt{3}} ( \frac { A_{h} }{ \beta } )^{\frac{1}{3}}

For our configuration we will choose to resolve a boundary layer of :math:`\approx` 100 km,
or roughly across five grid cells, so we set :math:`A_{h} = 400` m\ :sup:`2` s\ :sup:`--1`
(more precisely, this sets the full width at :math:`M_{w}` = 124 km). This choice ensures
that the frictional boundary layer is well resolved.

Given our choice of :math:`\delta t`, the stability 
parameter to the horizontal Laplacian friction (Adcroft 1995 :cite:`adcroft:95`)

.. math::
    :label: baro_laplacian_stability

    S_{l} = 4 \frac{A_{h} \delta t}{{\Delta x}^2}  < 0.3 \text{ for stability}


evaluates to 0.012, which is well also below the stability criteria.

.. _sec_eg_baro_code_config:

Code Configuration
------------------

The model configuration for this experiment resides under the directory :filelink:`verification/tutorial_barotropic_gyre/`.

The experiment files

 - :filelink:`verification/tutorial_barotropic_gyre/code/SIZE.h`
 - :filelink:`verification/tutorial_barotropic_gyre/input/data`
 - :filelink:`verification/tutorial_barotropic_gyre/input/data.pkg`
 - :filelink:`verification/tutorial_barotropic_gyre/input/eedata`
 - verification/tutorial_barotropic_gyre/input/Swall.box32
 - verification/tutorial_barotropic_gyre/input/Wwall.box32
 - verification/tutorial_barotropic_gyre/input/windx.cos_y
 
contain the code customizations and parameter settings for this 
experiments. Below we describe the customizations
to these files associated with this experiment. 

Note: MITgcm’s defaults are configured to simulate an ocean rather than an atmosphere, with vertical :math:`z`-coordinates.
To model the ocean using pressure coordinates using MITgcm, additional parameter changes are required; see tutorial ocean_in_p. 
To switch parameters to model an atmosphere, see tutorial Held_Suarez.

File :filelink:`code/SIZE.h <verification/tutorial_barotropic_gyre/code/SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../verification/tutorial_barotropic_gyre/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/code/SIZE.h


Here we show a modified :filelink:`model/inc` source code file, customizing MITgcm’s array sizes to our model domain.
This file must be uniquely configured for any model setup; using the MITgcm default
:filelink:`model/inc/SIZE.h`  will in fact cause a compilation error.
Since FORTRAN77 lacks dynamical memory allocation capabilities, this must be specified by parameter choices in a source code
``.h`` file rather than as a runtime (namelist) parameter settings,
as done with :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`,
:filelink:`data.pkg <verification/tutorial_barotropic_gyre/input/data.pkg>`, etc.


- These lines set parameters :varlink:`sNx` and :varlink:`sNy`, the number of grid points in the :math:`x` and :math:`y` directions, respectively,
  for a single model “tile”.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre/code/SIZE.h
       :start-at: sNx =
       :end-at: sNy =
       :lineno-match:

- These lines set parameters :varlink:`OLx` and :varlink:`OLy` in the :math:`x` and :math:`y` directions, respectively.
  These values are the overlap extent of a model tile, or in other words the number of grid cells on the border of each tile that are duplicated
  in neighboring tiles. The minimum model overlap is 2 in both :math:`x` and :math:`y`. Some horizontal advection schemes and other parameter and setup choices
  may require a larger overlap setting (provide ref).
  In our configuration, we are using a second-order center-differences advection scheme (the MITgcm default)
  which does not requires setting a overlap beyond the MITgcm minimum 2. Note these constraints on :varlink:`OLx` and
  :varlink:`OLy` size exist even if using a single tile, as in this setup.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/code/SIZE.h
       :start-at: OLx =
       :end-at: OLy =
       :lineno-match:

- These lines set parameters :varlink:`nSx` and :varlink:`nSy`, the number of model tiles in the :math:`x` and :math:`y` directions, respectively,
  which execute on a single process.
  In our configuration, we are using a single model tile; as such, parameters :varlink:`sNx` and :varlink:`sNy`,
  set to 60 grid points each, span the full model domain of 1200 km. For a multi-threaded setup
  (i.e., the process splits execution into multiple threads, taking advantage of multiple processor cores if available),
  one would set one or both of these to something larger than one.
  There are also numerical, computational, and/or grid-dictated reasons why one
  might opt to break the domain into multiple tiles in some setups.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/code/SIZE.h
       :start-at: nSx =
       :end-at: nSy =
       :lineno-match:

- These lines set parameters :varlink:`nPx` and :varlink:`nPy`, the number of processes to use in the :math:`x` and :math:`y` directions, respectively.
  Each process would solve the model equations for a separate MITgcm tile (or several, depending on your :varlink:`nSx` and :varlink:`nSy` setting).
  Given the relatively small domain size and minimal computational resources
  required to run this example, we have configured the model to run on a 
  single processor, thus both parameters are set to one. 

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/code/SIZE.h
       :start-at: nPx =
       :end-at: nPy =
       :lineno-match:

- This line sets parameter :varlink:`Nr`, the number of points in the vertical dimension. Here we use just a single vertical level.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/code/SIZE.h
       :start-at: Nr  =
       :end-at: Nr  =
       :lineno-match:

Further information and examples about how to configure :filelink:`model/inc/SIZE.h`
are given in :numref:`specify_decomp`.


.. _baro_gyre_data:

File :filelink:`input/data <verification/tutorial_barotropic_gyre/input/data>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/data

This file, reproduced completely above, specifies the main parameters 
for the experiment. The parameters that are significant for this configuration
(shown with line numbers to left) are as follows.

PARM01 - Continuous equation parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

- This line sets parameter :varlink:`viscAh`, the horizontal Laplacian viscosity, to :math:`400` m\ :sup:`2` s\ :sup:`--1`.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: viscAh
       :end-at: viscAh
       :lineno-match:

- These lines set :math:`f_0` and :math:`\beta` (the Coriolis parameter :varlink:`f0` and
  the gradient of the Coriolis parameter :varlink:`beta`) for our beta-plane to 
  :math:`1 \times 10^{-4}` s\ :sup:`--1` and :math:`1 \times 10^{-11}` m\ :sup:`--1`\ s\ :sup:`--1`, respectively.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: f0
       :end-at: beta
       :lineno-match:

- This line sets parameter :varlink:`rhonil`, a reference density which will also be used
  as :math:`rho_c` (parameter :varlink:`rhoConst`) in :eq:`baro_model_eq_u`, to 1000 kg/m\ :sup:`3`.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: rhonil
       :end-at: rhonil
       :lineno-match:

- This line sets parameter :varlink:`gBaro`, the acceleration due to gravity :math:`g` (in the free surface terms
  in :eq:`baro_model_eq_u` and :eq:`baro_model_eq_v`), to  9.81 m/s\ :sup:`2`.
  This is the MITgcm default value, i.e., the value used if this line were not
  included in ``data``. One might alter this parameter for a reduced gravity model, or to simulate a different planet, for example.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: gBaro
       :end-at: gBaro
       :lineno-match:

- These lines set parameters :varlink:`rigidLid` and :varlink:`implicitFreeSurface` in order to
  suppress the rigid lid formulation of the surface pressure inverter and activate the implicit free surface formulation.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: rigidLid
       :end-at: implicitFreeSurface
       :lineno-match:

- This line sets parameter :varlink:`momAdvection` to suppress the momentum of advection
  terms in the momentum equations. However, note the ``#`` in column 1: this
  “comments out” the line, so using this :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`
  file as is will in fact include the momentum advection terms (i.e., MITgcm default for this parameter is TRUE).
  We’ll explore the linearized solution (i.e., by removing the leading ``#``) in :numref:`barotropic_gyre_solution`.
  Note the ability to comment out a line in a namelist file is not part of standard Fortran,
  but this feature is implemented for all MITgcm namelist files.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: momAdvection
       :end-at: momAdvection
       :lineno-match:

- These lines set parameters :varlink:`tempStepping` and :varlink:`saltStepping` to
  suppress MITgcm’s forward time integration of temperature and salt in the tracer equations,
  as these prognostic variables are not relevant for the model solution in this configuration.
  The advantage of doing so is to
  eliminate many unnecessary computations. In most typical configurations however, one will want the model to
  compute a solution for :math:`T` and :math:`S` (the MITgcm default), which
  typically comprises the majority of MITgcm’s processing time. 

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: tempStepping
       :end-at: saltStepping
       :lineno-match:

PARM02 - Elliptic solver parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

- The first line sets the tolerance (parameter :varlink:`cg2dTargetResidual`) that the 2D conjugate gradient solver,
  the iterative method used in the pressure method algorithm, will use to test for convergence.
  The second line sets parameter :varlink:`cg2dMaxIters` the maximum
  number of iterations.
  The solver will iterate until the residual falls below this target value
  (here, set to :math:`1 \times 10^{-7}`) or until this maximum number of solver iterations is reached
  (here, set to a maximum 1000 iterations). Typically, the solver will converge in far fewer than 1000 iterations, but
  it does not hurt to allow for a large number. The chosen value for the target residual
  happens to be the MITgcm default, and will serve well
  in most model configurations.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: cg2dTargetResidual
       :end-at: cg2dMaxIters
       :lineno-match:


PARM03 - Time stepping parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- This line sets the starting (integer) iteration number for the run. Here we set the value to zero, which starts the model from a new, initialized state.
  If :varlink:`nIter0` is non-zero, the model would require appropriate pickup files (i.e., restart files) in order to continue integration of an existing run.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: nIter0
       :end-at: nIter0
       :lineno-match:

- This line sets parameter :varlink:`nTimeSteps`, the (integer) number of timesteps the model will integrate forward. Below,
  we have set this to integrate for just 10 time steps, for testing purposes. To integrate the solution to near steady state,
  uncomment the line where we set the value to 60,000 time steps.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: nTimeSteps=10
       :end-at: nTimeSteps=60
       :lineno-match:

- This line sets parameter :varlink:`deltaT`, the timestep used in stepping forward the model, to 1200 seconds. In combination with the larger value of :varlink:`nTimeSteps` above,
  we have effectively set the model to integrate forward for :math:`60000 \times 1200 \text{ s} = 2.28` years, long enough for the solution to reach equilibrium.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: deltaT
       :end-at: deltaT
       :lineno-match:

- These lines control the frequency at which restart (a.k.a. pickup) files are dumped by MITgcm.
  Here the value of :varlink:`pChkptFreq` is set to 36,000,000 seconds (=1.14 years) of model time;
  this controls the frequency of “permanent” checkpoint pickup files. In permanent files,
  the model’s iteration number is included in the file name so as to save it as a labelled, permanent, pickup state.
  The value of :varlink:`ChkptFreq` is set to 12,000,000 seconds (=0.38 years); the pickup files
  written at this frequency but will NOT include the iteration number in the filename
  (as the filename “suffix”; see :numref:`other_output`),
  instead toggling between ``ckptA`` and ``ckptB`` in the filename, and thus these
  files will be overwritten with new data every 2 :math:`\times` 12,000,000 seconds.
  Generally, one opts to dump permanent checkpoints at intervals for which one desires a permanent restart state, or perhaps would
  rather not have to repeat computational machine time (typically, wall times of hours to a days). Temporary
  checkpoint files can be written more frequently without requiring additional disk space,
  for example to peruse (or re-run) the model state prior to an instability, should such occur.
  Either type of checkpoint file can be used to restart the model.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: pChkptFreq
       :end-at: chkptFreq
       :lineno-match:

- This line sets parameter :varlink:`dumpFreq`, frequency of writing model state
  snapshot diagnostics (of relevance in this setup: :math:`u`, :math:`v`, and :math:`\eta`).
  Here, we opt for a snapshot of model state every 24,000,000 seconds (=0.76 years), or after every 20000 time steps of integration.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: dumpFreq
       :end-at: dumpFreq
       :lineno-match:



PARM04 - Gridding parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

- This line sets parameter :varlink:`usingCartesianGrid`, which specifies that the simulation will use a Cartesian coordinate system.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: usingCartesianGrid
       :end-at: usingCartesianGrid
       :lineno-match:

- These lines sets the horizontal grid spacing in the discrete grid, parameters :varlink:`delX` and :varlink:`delY`.
  The syntax indicates that the discrete grid should be comprise of 60 grid lines,
  each separated by :math:`20 \times 10^{3}` m (=20 km), in both the :math:`x`- and :math:`y`-coordinate.
  The centers of the grid boxes will thus be at 10 km, 30 km, 50 km, ...,
  in both :math:`x` and :math:`y` directions; the default origin is (0,0).

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: delX
       :end-at: delY
       :lineno-match:

- This line sets parameter :varlink:`delR`, the vertical grid spacing in the :math:`z`-coordinate, to 5000m.

  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: delR
       :end-at: delR
       :lineno-match:
 
 
PARM05 - Input datasets
^^^^^^^^^^^^^^^^^^^^^^^

- These lines sets parameters :varlink:`addSwallFile` and :varlink:`addWwallFile`, the names of the files
  which effectively set “thin walls” at the southern and western faces of our model domain, respectively.
  The domain in both :math:`x`- and :math:`y`-directions is periodic in MITgcm, so these
  walls are necessary to set up our confined box domain. See :numref:`baro_gyre_wall_files` for information about the file format.
  A more typical ocean setup would employ a bathymetry file rather than setting thin walls; we’ll show how to do this in :numref:`tutorial_baroclinic_gyre`.


  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: Swall
       :end-at: Wwall
       :lineno-match:
 
- These lines specifies the names of the files from which the surface wind stress is read.
  There is a separate file for the :math:`x`-direction (:varlink:`zonalWindFile`) and the :math:`y`-direction (:varlink:`meridWindFile`).
  Note, here we have left the latter parameter blank, as there is no meridional wind stress forcing in our example.
 
  .. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data
       :start-at: zonalWindFile
       :end-at: meridWindFile
       :lineno-match:


File :filelink:`input/data.pkg <verification/tutorial_barotropic_gyre/input/data.pkg>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data.pkg
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/data.pkg

This file does not set any parameters, yet is necessary to run --  only standard packages
(i.e., those compiled in MITgcm by default) are required for this setup, so no other customization is necessary.
We will demonstrate how to include additional packages
in parts II and II of this tutorial.


File :filelink:`input/eedata <verification/tutorial_barotropic_gyre/input/eedata>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/eedata
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/data.pkg

This file uses standard default values (i.e., MITgcm default is single-threaded) and does not contain
customizations for this experiment.

.. _baro_gyre_wall_files:

Files ``input/Swall.box32`` and ``input/Wwall.box32``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These files are 2D(:math:`x,y`) maps of 0s and 1s indicating the position of “thin walls”. 
The points are ordered from low to high coordinates in both axes (varying fastest in :math:`x`), as a raw binary
stream of data that is enumerated in the same way as standard MITgcm 2D horizontal arrays.
Where there is a 1.0 in the file ``input/Swall.box32``, a thin wall is set at the southern-most face of the grid cell.
For file  ``input/Wwall.box32``, a 1.0 sets a thin wall at the western-most face of the grid cell. 
Given that the domain is assumed
to be doubly periodic (i.e., in **BOTH** the :math:`x` and :math:`y` dimensions),
placing a wall on the south and west boundaries of our box domain (as specified here)
effectively bounds all lateral sides of the box.
By default, this file is assumed to
contain 32-bit (single precision) binary numbers.
The matlab program :filelink:`verification/tutorial_barotropic_gyre/input/gendata.m`
was used to generate this bathymetry file.


.. _baro_gyre_windx_cos_y:

File ``input/windx.cos_y``
~~~~~~~~~~~~~~~~~~~~~~~~~~

Similar to files `input/Swall.box32` and `input/Wwall.box32`, this file is a 2D(:math:`x,y`)
map of :math:`\tau_{x}` wind stress values, formatted in the same manner.
The units are Nm\ :sup:`--2`. Although :math:`\tau_{x}` is only a function of :math:`y` in this experiment,
this file must still define a complete 2D map in order
to be compatible with the standard code for loading forcing fields 
in MITgcm. The matlab program :filelink:`verification/tutorial_barotropic_gyre/input/gendata.m`
was used to generate this wind stress file.



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

- information about the grid and bathymetry, including dumps of all grid variables (if Cartesian or spherical polar coordinates used, as is the case here).

- all runtime parameter choices used by the model, including all model defaults as well as user-specified parameters.

- monitor statistics at regular intervals (note we did not specify parameter
  :varlink:`monitorFreq` in :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`,
  so MITgcm defaulted to the value of parameter :varlink:`dumpFreq` for the monitor interval). See :numref:`pkg_monitor`.

- output from the 2D conjugate gradient solver. More specifically, statistics from the right-hand side of the elliptic
  equation -- for our linear free-surface, see eq. :eq:`elliptic-backward-free-surface` -- are dumped for every model time step. If the model solution
  blows up, these statistics will increase to infinity, so one can see exactly when the problem occurred (i.e., to aid in debugging). Additional
  solver variables, such as number of iterations and residual, are included with the monitor statistics.

- a summary of end-of-run execution information, including user-, wall- and system-time elapsed during execution
  (for the overall run, and broken down by time spent in various subroutines), and tile communication statistics.

Different setups using non-standard packages and/or different parameter choices will include
additional or different output as part of the standard output. It is also possible to select more or less output
by changing the parameter :varlink:`debugLevel` in :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`; see (missing doc for pkg debug).

``STDERR.0000`` - if errors (or warnings) occurred during the run, helpful warning and/or error message(s) would appear in this file.

.. _other_output:

Other output files
~~~~~~~~~~~~~~~~~~

In addition to raw binary data files with ``.data`` extension, each binary file has a corresponding ``.meta`` file. These plain-text files include
information about the array size, precision (i.e., ``float32`` or ``float64``), and if relevant, time information  and/or 
a list of these fields included in the binary file.  The ``.meta`` files are used by MITgcm :filelink:`utils` when binary data is read.

The following output files are generated:

**Grid Data**: see :numref:`spatial_discret_dyn_eq` for definitions and description of the C-grid staggering of these variables.

- ``XC``, ``YC`` - grid cell center point locations
- ``XG``, ``YG`` - grid cell velocity point locations
- ``RC``, ``RF`` - vertical cell center and cell faces positions
- ``DXC``, ``DYC`` - grid cell center point separations (fig 2.6b)
- ``DXG``, ``DYG`` - grid cell velocity point separations (fig 2.6a)
- ``DRC``, ``DRF`` - separation of vertical cell center points and between faces
- ``RAC``, ``RAS``, ``RAW``, ``RAZ`` - areas of the grid “tracer cells”, “southern cells”, “western cells” and “vorticity cells” (see fig 2.6)
- ``hFacC``, ``hFacS``, ``hFacW`` - fractions of the grid cell in the vertical which are “open” as defined
  in the center and on the southern and western boundary. These variables effectively contain the configuration bathymetric (or topographic) information.
- ``Depth`` - bathymetry depths

All these files contain 2D(:math:`x,y`) data except ``RC``, ``RF``, ``DRC``, ``DRF``, which are 1D(:math:`z`), 
and ``hFacC``, ``hFacS``, ``hFacW``, which contain 3D(:math:`x,y,z`) data.

All the 2D grid data files contain ``.001.001`` in their name, e.g., ``DXC.001.001.data`` -- this is the tile number in ``.XXX.YYY`` format.
Here, we have just a single tile in both x and y, so both tile numbers are ``001``. Using multiple tiles, by default the local tile grid information
would be output separately for each tile (as an example, see the :ref:`baroclinic gyre tutorial <tutorial_baroclinic_gyre>`,
which is set up using multiple tiles), producing multiple files for each 2D grid variable.


**State Variable Snapshot Data**:

``Eta.0000000000.001.001.data, Eta.0000000000.001.001.meta`` - this is
the a binary data snapshot of model dynamic variable
:varlink:`etaN` (the free-surface height) and its meta files, respectively.
Note the tile number is included in the filename, as is the iteration number ``0000000000``
(the iteration number, i.e. the text after the variable name, is referred to as the “suffix” in
MITgcm parlance; there are options to change this “suffix” to something other than iteration number).
In other words, this is a dump of the free-surface height from the initialized state,
iteration 0; if you load up this data file,
you will see it is all zeroes. More interesting is the free-surface
height after some time steps have occurred; snapshots are written according
to our parameter choice :varlink:`dumpFreq`, here set to 20,000.
We will examine the model solutions in :numref:`barotropic_gyre_solution`.
The free-surface height is a 2D(:math:`x,y`) field.

Similar snapshot files exist for other prognostic model variables,
filename starting with ``U`` (:varlink:`uVel`),
``V`` (:varlink:`uVel`), ``T`` (:varlink:`theta`), and ``S`` (:varlink:`salt`);
given our setup, these latter two fields
remain uniform in space and time, thus not very interesting until we
explore a baroclinic gyre setup in :numref:`tutorial_baroclinic_gyre`.
These are all 3D(:math:`x,y,z`) fields. The format for the file names is similar
to the free-surface height files. Also dumped are snapshots
of diagnosed vertical velocity W (:varlink:`wVel`) (note in non-hydrostatic
simulations, W is a fully prognostic model variable).


**Checkpoint Files**:

In addition, the following pickup files are generated:

- ``pickup.0000030000.001.001.data``, ``pickup.0000030000.001.001.meta``, etc., - written at frequency set by :varlink:`pChkptFreq`
- ``pickup.ckptA.001.001.data``, ``pickup.ckptA.001.001.meta``, ``pickup.ckptB.001.001.data``,
  ``pickup.ckptB.001.001.meta`` - written at frequency set by :varlink:`ChkptFreq`


**Other Model Output Data**: For completeness, here we list the remaining default output files produced by MITgcm (despite being not particularly informative for this simple setup).

``RhoRef.data, RhoRef.meta`` - this is a 1D(:math:`z`) array of reference density. Here we have a single level and have not specified an equation of state relation, thus
the file simply contains our prescribed value :varlink:`rhonil`.

``PHrefC.data, PHrefC.meta, PHrefF.data, PHrefF.meta`` - these are 1D(:math:`z`) arrays containing reference
hydrostatic “pressure potential” :math:`\phi = p/\rho_c` (see :numref:`finding_the_pressure_field`),
computed at the (vertical grid) cell centers and cell faces, respectively.
In our setup here, ``PHrefC`` is simply :math:`\frac{\rho_c*g*D/2}{\rho_c}`,
i.e., computed at the midpoint of our single vertical cell.

``PH``, ``PHL`` files - these are a 3D(:math:`x,y,z`) field of hydrostatic
:math:`\phi’` (including free-surface contribution) at cell centers
and a 2D(:math:`x,y`) field of ocean bottom :math:`\phi’`, respectively, as a function of time.
To obtain full :math:`\phi(t)` values, ``PHrefC`` should be added to ``PH``, and ``PHrefF``\ (z=bottom) should be added to ``PHL``.



.. _barotropic_gyre_solution:

Model Solution
--------------


After running the model for 60,000 time steps (2.28 years), the solution is near equilibrium.
This equilibrium timescale is consistent with barotropic Rossby waves
requiring order one month to cross our model domain. The model solution of free-surface
height :math:`\eta` (proportional to streamfunction) at :math:`t=` 2.28 years is shown in :numref:`barotropic_nl_soln`.

  .. figure:: barotropic_gyre/figs/full_solution.*
      :width: 80%
      :align: center
      :alt: barotropic gyre full solution
      :name: barotropic_nl_soln

      MITgcm solution to the barotropic gyre example after :math:`t=` 2.28 years of model integration. Free surface height is shown in meters.

Using matlab for example, visualizing output using the :filelink:`utils/matlab/rdmds.m` utility to load the
binary data in ``Eta.0000060000.001.001.data`` is as simple as:

::

   addpath ../../../utils/matlab/
   XC=rdmds('XC'); YC=rdmds('YC');
   Eta=rdmds('Eta',60000); 
   contourf(XC/1000,YC/1000,Eta,[-.04:.01:.04]); colorbar; set(gca,'Clim',[-.04 .04]);

or using python (you will need to copy :filelink:`utils/python/MITgcmutils/MITgcmutils/mds.py` to your run directory before proceeding):

::

   import mds
   import matplotlib.pyplot as plt
   XC = mds.rdmds('XC’); YC = mds.rdmds('YC')
   Eta = mds.rdmds('Eta', 6000)
   plt.contourf(XC, YC, Eta[:,:]); plt.colorbar(); plt.show()


Let’s simplify the example by considering the linear problem where we neglect the advection of momentum terms.
In other words, replace :math:`\frac{Du}{Dt}` and :math:`\frac{Dv}{Dt}` with
:math:`\frac{\partial u}{\partial t}` and :math:`\frac{\partial v}{\partial t}`, respectively, in in :eq:`baro_model_eq_u` and :eq:`baro_model_eq_v`.
To do so, we uncomment (i.e., remove the leading ``#``) in the
line  ``# momAdvection=.FALSE.,`` in file ``data`` and re-run the model. Any existing output files will be overwritten.

For the linearized equations, the Munk layer analytical solution is given by:

.. math:: 
   \eta(x,y) = \frac{\tau_o}{\rho_c g D} \frac{f}{\beta} \left(1 - \frac{x}{L_x}\right) \pi \sin(\pi \frac{y}{L_y})
   \left[1 - \exp({\frac{-x}{2\delta_m}}) \left(\cos\frac{\sqrt{3}x}{2\delta_m} + \frac{1}{\sqrt{3}} \sin\frac{\sqrt{3}x}{2\delta_m} \right) \right]
 

:numref:`lin_anal_soln` displays the MITgcm output after switching off momentum advection vs.
the analytical solution to the linearized equations. Success!


  .. figure:: barotropic_gyre/figs/lin_anal_soln.*
      :width: 100%
      :align: center
      :alt: barotropic gyre linearized solution
      :name: lin_anal_soln

      Comparison of free surface height for the equilibrium MITgcm solution with momentum advectum switched off (left) and the analytical solution to the linearized equation (right).

Finally, let’s examine one additional simulation where we change the cosine profile of wind stress forcing to a sine profile.
In file :filelink:`data <verification/tutorial_barotropic_gyre/input/data>`,
replace the line ``zonalWindFile='windx.cos_y32’,`` with ``zonalWindFile='windx.sin_y32’,``.

  .. figure:: barotropic_gyre/figs/baro_jet_solution.*
      :width: 80%
      :align: center
      :alt: barotropic jet linearized solution
      :name: baro_jet_solution

      MITgcm equilibrium solution to the barotropic setup with alternate sine profile of wind stress forcing, producing a barotropic jet.

The free surface solution given this forcing is shown in :numref:`baro_jet_solution`. Two “half gyres” are separated by a strong jet.
We’ll look more at the solution to this “barotropic jet” setup in later tutorial examples.


.. _tutorial_baroclinic_gyre:

Fifteen Layer Baroclinic Ocean Gyre In Spherical Coordinates
============================================================


(in directory: :filelink:`verification/tutorial_baroclinic_gyre`)

This section describes an example experiment using MITgcm to simulate a
baroclinic ocean gyre in spherical polar coordinates using 15 vertical layers.


This example experiment demonstrates using the MITgcm to simulate a
baroclinic, wind-forced, ocean double-gyre circulation. The experiment is a
numerical rendition of the gyre circulation problem similar to the
problems described analytically by Stommel in 1966
:cite:`Stommel66` and numerically in Holland et. al
:cite:`Holland75`.
In this experiment the model is configured to represent a mid-latitude
enclosed sector of fluid on a sphere,
:math:`60^{\circ} \times 60^{\circ}` in lateral extent. The fluid is
:math:`2` km deep and is forced by a constant in time zonal wind
stress, :math:`\tau_{\lambda}`, that varies sinusoidally in the
north-south direction. Topologically the simulated domain is a sector
on a sphere and the Coriolis parameter, :math:`f`, is defined
according to latitude, :math:`\varphi`

.. math::

   f(\varphi) = 2 \Omega \sin( \varphi )

with the rotation rate, :math:`\Omega` set to :math:`\frac{2 \pi}{86400s}`.
The sinusoidal wind-stress variations are defined according to

.. math::

   \tau_{\lambda}(\varphi) = \tau_{0}\sin(\pi \frac{\varphi}{L_{\varphi}})

where :math:`L_{\varphi}` is the lateral domain extent
(:math:`60^{\circ}`) and :math:`\tau_0` is set to :math:`0.1N m^{-2}`.
:numref:`baroclinic_gyre_config` summarizes the
configuration simulated. In contrast to the example in section
[sec:eg-baro], the current experiment simulates a spherical polar
domain. As indicated by the axes in the lower left of the figure the
model code works internally in a locally orthogonal coordinate
:math:`(x,y,z)`. For this experiment description the local orthogonal
model coordinate :math:`(x,y,z)` is synonymous with the coordinates
:math:`(\lambda,\varphi,r)` shown in figure
[fig:spherical-polar-coord]
The experiment has four levels in the vertical, each of equal
thickness, :math:`\Delta z = 500` m. Initially the fluid is stratified
with a reference potential temperature profile,
:math:`\theta_{250}=20^{\circ}` C, :math:`\theta_{750}=10^{\circ}` C,
:math:`\theta_{1250}=8^{\circ}` C, :math:`\theta_{1750}=6^{\circ}` C.
The equation of state used in this experiment is linear

.. math::

   \rho = \rho_{0} ( 1 - \alpha_{\theta}\theta^{'} )

which is implemented in the model as a density anomaly equation

.. math::

   \rho^{'} = -\rho_{0}\alpha_{\theta}\theta^{'}

with :math:`\rho_{0}=999.8\,{\rm kg\,m}^{-3}` and
:math:`\alpha_{\theta}=2\times10^{-4}\,{\rm degrees}^{-1}`. Integrated
forward in this configuration the model state variable :varlink:`theta` is
equivalent to either in-situ temperature, :math:`T`, or potential
temperature, :math:`\theta`. For consistency with later examples, in
which the equation of state is non-linear, we use :math:`\theta` to
represent temperature here. This is the quantity that is carried in the
model core equations.


  .. figure:: baroclinic_gyre/figs/baroclinic_gyre_config.png
      :width: 90%
      :align: center
      :alt: baroclinic gyre configuration
      :name: baroclinic_gyre_config

      Schematic of simulation domain and wind-stress forcing function for baroclinic gyre numerical experiment. The domain is enclosed by solid walls at.


Equations solved
----------------

For this problem the implicit free surface, **HPE** (see section
[sec:hydrostatic\_and\_quasi-hydrostatic\_forms]) form of the equations
described in Marshall et. al :cite:`marshall:97a` are
employed. The flow is three-dimensional with just temperature,
:math:`\theta`, as an active tracer. The equation of state is linear. A
horizontal Laplacian operator :math:`\nabla_{h}^2` provides viscous
dissipation and provides a diffusive sub-grid scale closure for the
temperature equation. A wind-stress momentum forcing is added to the
momentum equation for the zonal flow, :math:`u`. Other terms in the
model are explicitly switched off for this experiment configuration (see
section [sec:eg\_fourl\_code\_config] ). This yields an active set of
equations solved in this configuration, written in spherical polar
coordinates as follows

.. math::

   \begin{aligned}
   \label{eq:eg-fourlayer-model_equations}
   \frac{Du}{Dt} - fv + 
     \frac{1}{\rho}\frac{\partial p^{\prime}}{\partial \lambda} - 
     A_{h}\nabla_{h}^2u - A_{z}\frac{\partial^{2}u}{\partial z^{2}} 
   & = &
   \cal{F}_{\lambda}
   \\
   \frac{Dv}{Dt} + fu + 
     \frac{1}{\rho}\frac{\partial p^{\prime}}{\partial \varphi} - 
     A_{h}\nabla_{h}^2v - A_{z}\frac{\partial^{2}v}{\partial z^{2}} 
   & = &
   0
   \\
   \frac{\partial \eta}{\partial t} + \frac{\partial H \widehat{u}}{\partial \lambda} +
   \frac{\partial H \widehat{v}}{\partial \varphi}
   &=&
   0
   \label{eq:fourl_example_continuity}
   \\
   \frac{D\theta}{Dt} -
    K_{h}\nabla_{h}^2\theta  - K_{z}\frac{\partial^{2}\theta}{\partial z^{2}} 
   & = &
   0
   \label{eq:eg_fourl_theta}
   \\
   p^{\prime} & = &
   g\rho_{0} \eta + \int^{0}_{-z}\rho^{\prime} dz
   \\
   \rho^{\prime} & = &- \alpha_{\theta}\rho_{0}\theta^{\prime}
   \\
   {\cal F}_{\lambda} |_{s} & = & \frac{\tau_{\lambda}}{\rho_{0}\Delta z_{s}}
   \\
   {\cal F}_{\lambda} |_{i} & = & 0\end{aligned}

where :math:`u` and :math:`v` are the components of the horizontal flow
vector :math:`\vec{u}` on the sphere
(:math:`u=\dot{\lambda},v=\dot{\varphi}`). The terms
:math:`H\widehat{u}` and :math:`H\widehat{v}` are the components of the
vertical integral term given in equation [eq:free-surface] and explained
in more detail in section [sec:pressure-method-linear-backward].
However, for the problem presented here, the continuity relation
(equation [eq:fourl\_example\_continuity]) differs from the general form
given in section [sec:pressure-method-linear-backward], equation
[eq:linear-free-surface=P-E], because the source terms
:math:`{\cal P}-{\cal E}+{\cal R}` are all :math:`0`.

The pressure field, :math:`p^{\prime}`, is separated into a barotropic
part due to variations in sea-surface height, :math:`\eta`, and a
hydrostatic part due to variations in density, :math:`\rho^{\prime}`,
integrated through the water column.

The suffices :math:`{s},{i}` indicate surface layer and the interior of
the domain. The windstress forcing, :math:`{\cal F}_{\lambda}`, is
applied in the surface layer by a source term in the zonal momentum
equation. In the ocean interior this term is zero.

In the momentum equations lateral and vertical boundary conditions for
the :math:`\nabla_{h}^{2}` and
:math:`\frac{\partial^{2}}{\partial z^{2}}` operators are specified when
the numerical simulation is run - see section
[sec:eg\_fourl\_code\_config]. For temperature the boundary condition is
“zero-flux” e.g. :math:`\frac{\partial \theta}{\partial \varphi}=
\frac{\partial \theta}{\partial \lambda}=\frac{\partial \theta}{\partial z}=0`.

Discrete Numerical Configuration
--------------------------------

The domain is discretised with a uniform grid spacing in latitude and
longitude :math:`\Delta \lambda=\Delta \varphi=1^{\circ}`, so that there
are sixty grid cells in the zonal and meridional directions. Vertically
the model is configured with four layers with constant depth,
:math:`\Delta z`, of :math:`500` m. The internal, locally orthogonal,
model coordinate variables :math:`x` and :math:`y` are initialized from
the values of :math:`\lambda`, :math:`\varphi`, :math:`\Delta \lambda`
and :math:`\Delta \varphi` in radians according to

.. math::

   \begin{aligned}
   x=r\cos(\varphi)\lambda,~\Delta x & = &r\cos(\varphi)\Delta \lambda \\
   y=r\varphi,~\Delta y &= &r\Delta \varphi\end{aligned}

The procedure for generating a set of internal grid variables from a
spherical polar grid specification is discussed in section
[sec:spatial\_discrete\_horizontal\_grid].

| 
| As described in [sec:tracer\_equations], the time evolution of
  potential temperature, :math:`\theta`, (equation
  [eq:eg\_fourl\_theta]) is evaluated prognostically. The centered
  second-order scheme with Adams-Bashforth time stepping described in
  section [sec:tracer\_equations\_abII] is used to step forward the
  temperature equation. Prognostic terms in the momentum equations are
  solved using flux form as described in section
  [sec:flux-form\_momentum\_equations]. The pressure forces that drive
  the fluid motions, ( :math:`\frac{\partial p^{'}}{\partial \lambda}`
  and :math:`\frac{\partial p^{'}}{\partial \varphi}`), are found by
  summing pressure due to surface elevation :math:`\eta` and the
  hydrostatic pressure. The hydrostatic part of the pressure is
  diagnosed explicitly by integrating density. The sea-surface height,
  :math:`\eta`, is diagnosed using an implicit scheme. The pressure
  field solution method is described in sections
  [sec:pressure-method-linear-backward] and
  [sec:finding\_the\_pressure\_field].

Numerical Stability Criteria
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Laplacian viscosity coefficient, :math:`A_{h}`, is set to
:math:`400 m s^{-1}`. This value is chosen to yield a Munk layer width,

.. math::

   \begin{aligned}
   \label{eq:eg-fourlayer-munk_layer}
   M_{w} = \pi ( \frac { A_{h} }{ \beta } )^{\frac{1}{3}}\end{aligned}

| of :math:`\approx 100`\ km. This is greater than the model resolution
  in mid-latitudes
  :math:`\Delta x=r \cos(\varphi) \Delta \lambda \approx 80~{\rm km}` at
  :math:`\varphi=45^{\circ}`, ensuring that the frictional boundary
  layer is well resolved.
| The model is stepped forward with a time step
  :math:`\delta t=1200`\ secs. With this time step the stability
  parameter to the horizontal Laplacian friction

.. math::

   \begin{aligned}
   \label{eq:eg-fourlayer-laplacian_stability}
   S_{l} = 4 \frac{A_{h} \delta t}{{\Delta x}^2}\end{aligned}

| evaluates to 0.012, which is well below the 0.3 upper limit for
  stability for this term under ABII time-stepping.
| The vertical dissipation coefficient, :math:`A_{z}`, is set to
  :math:`1\times10^{-2} {\rm m}^2{\rm s}^{-1}`. The associated stability
  limit

.. math::

   \begin{aligned}
   \label{eq:eg-fourlayer-laplacian_stability_z}
   S_{l} = 4 \frac{A_{z} \delta t}{{\Delta z}^2}\end{aligned}

| evaluates to :math:`4.8 \times 10^{-5}` which is again well below the
  upper limit. The values of :math:`A_{h}` and :math:`A_{z}` are also
  used for the horizontal (:math:`K_{h}`) and vertical (:math:`K_{z}`)
  diffusion coefficients for temperature respectively.
| The numerical stability for inertial oscillations

.. math::

   \begin{aligned}
   \label{eq:eg-fourlayer-inertial_stability}
   S_{i} = f^{2} {\delta t}^2\end{aligned}

| evaluates to :math:`0.0144`, which is well below the :math:`0.5` upper
  limit for stability.
| The advective CFL for a extreme maximum horizontal flow speed of
  :math:` | \vec{u} | = 2 ms^{-1}`

.. math::

   \begin{aligned}
   \label{eq:eg-fourlayer-cfl_stability}
   C_{a} = \frac{| \vec{u} | \delta t}{ \Delta x}\end{aligned}

| evaluates to :math:`5 \times 10^{-2}`. This is well below the
  stability limit of 0.5.
| The stability parameter for internal gravity waves propagating at
  :math:`2~{\rm m}~{\rm s}^{-1}`

.. math::

   \begin{aligned}
   \label{eq:eg-fourlayer-igw_stability}
   S_{c} = \frac{c_{g} \delta t}{ \Delta x}\end{aligned}

evaluates to :math:`\approx 5 \times 10^{-2}`. This is well below the
linear stability limit of 0.25.

Code Configuration
------------------

The model configuration for this experiment resides under the directory
*verification/tutorial\_barotropic\_gyre/*. The experiment files

-  *input/data*

-  *input/data.pkg*

-  *input/eedata*,

-  *input/windx.sin\_y*,

-  *input/topog.box*,

-  *code/CPP\_EEOPTIONS.h*

-  *code/CPP\_OPTIONS.h*,

-  *code/SIZE.h*.

contain the code customizations and parameter settings for this
experiment. Below we describe the customizations to these files
associated with this experiment.

File *input/data*
~~~~~~~~~~~~~~~~~

This file, reproduced completely below, specifies the main parameters
for the experiment. The parameters that are significant for this
configuration are

-  Line 4,

   ::

        tRef=20.,10.,8.,6., 

   this line sets the initial and reference values of potential
   temperature at each model level in units of
   :math:`^{\circ}\mathrm{C}`. The entries are ordered from surface to
   depth. For each depth level the initial and reference profiles will
   be uniform in :math:`x` and :math:`y`. The values specified here are
   read into the variable in the model code, by procedure

-  Line 6,

   ::

        viscAz=1.E-2, 

   this line sets the vertical Laplacian dissipation coefficient to
   :math:`1
   \times 10^{-2} {\rm m^{2}s^{-1}}`. Boundary conditions for this
   operator are specified later. The variable is read in the routine and
   is copied into model general vertical coordinate variable At each
   time step, the viscous term contribution to the momentum equations is
   calculated in routine

-  Line 7,

   ::

       viscAh=4.E2,

   this line sets the horizontal laplacian frictional dissipation
   coefficient to :math:`1 \times 10^{-2} {\rm m^{2}s^{-1}}`. Boundary
   conditions for this operator are specified later. The variable is
   read in the routine and applied in routine .

-  Line 8,

   ::

       no_slip_sides=.FALSE.

   this line selects a free-slip lateral boundary condition for the
   horizontal laplacian friction operator e.g. :math:`\frac{\partial
   u}{\partial y}`\ =0 along boundaries in :math:`y` and
   :math:`\frac{\partial
   v}{\partial x}`\ =0 along boundaries in :math:`x`. The variable
   is read in the routine and the boundary condition is evaluated in
   routine

-  Lines 9,

   ::

       no_slip_bottom=.TRUE.

   this line selects a no-slip boundary condition for bottom boundary
   condition in the vertical laplacian friction operator e.g.
   :math:`u=v=0` at :math:`z=-H`, where :math:`H` is the local depth of
   the domain. The variable is read in the routine and is applied in the
   routine .

-  Line 10,

   ::

       diffKhT=4.E2,

   this line sets the horizontal diffusion coefficient for temperature
   to :math:`400\,{\rm m^{2}s^{-1}}`. The boundary condition on this
   operator is
   :math:`\frac{\partial}{\partial x}=\frac{\partial}{\partial y}=0` at
   all boundaries. The variable is read in the routine and used in
   routine .

-  Line 11,

   ::

       diffKzT=1.E-2,

   this line sets the vertical diffusion coefficient for temperature to
   :math:`10^{-2}\,{\rm m^{2}s^{-1}}`. The boundary condition on this
   operator is :math:`\frac{\partial}{\partial z}` = 0 on all
   boundaries. The variable is read in the routine . It is copied into
   model general vertical coordinate variable which is used in routine .

-  Line 13,

   ::

       tAlpha=2.E-4,

   This line sets the thermal expansion coefficient for the fluid to
   :math:`2 \times 10^{-4}\,{\rm degrees}^{-1}` The variable is read in the
   routine . The routine makes use of **tAlpha**.

-  Line 18,

   ::

       eosType='LINEAR'

   This line selects the linear form of the equation of state. The
   variable is read in the routine . The values of **eosType** sets
   which formula in routine *FIND\_RHO* is used to calculate density.

-  Line 40,

   ::

       usingSphericalPolarGrid=.TRUE.,

   This line requests that the simulation be performed in a spherical
   polar coordinate system. It affects the interpretation of grid input
   parameters, for example **delX** and **delY** and causes the grid
   generation routines to initialize an internal grid based on spherical
   polar geometry. The variable is read in the routine . When set to
   **.TRUE.** the settings of **delX** and **delY** are taken to be in
   degrees. These values are used in the routine

-  Line 41,

   ::

       ygOrigin=0.,

   This line sets the southern boundary of the modeled domain to
   :math:`0^{\circ}` latitude. This value affects both the generation of
   the locally orthogonal grid that the model uses internally and
   affects the initialization of the coriolis force. Note - it is not
   required to set a longitude boundary, since the absolute longitude
   does not alter the kernel equation discretisation. The variable is
   read in the routine and is used in routine

-  Line 42,

   ::

       delX=60*1.,

   This line sets the horizontal grid spacing between each y-coordinate
   line in the discrete grid to :math:`1^{\circ}` in longitude. The
   variable is read in the routine and is used in routine

-  Line 43,

   ::

       delY=60*1.,

   This line sets the horizontal grid spacing between each y-coordinate
   line in the discrete grid to :math:`1^{\circ}` in latitude. The
   variable is read in the routine and is used in routine

-  Line 44,

   ::

       delZ=500.,500.,500.,500.,

   This line sets the vertical grid spacing between each z-coordinate
   line in the discrete grid to :math:`500\,{\rm m}`, so that the total
   model depth is :math:`2\,{\rm km}`. The variable is read in the
   routine . It is copied into the internal model coordinate variable
   which is used in routine

-  Line 47,

   ::

       bathyFile='topog.box'

   This line specifies the name of the file from which the domain
   bathymetry is read. This file is a two-dimensional (:math:`x,y`) map
   of depths. This file is assumed to contain 64-bit binary numbers
   giving the depth of the model at each grid cell, ordered with the x
   coordinate varying fastest. The points are ordered from low
   coordinate to high coordinate for both axes. The units and
   orientation of the depths in this file are the same as used in the
   MITgcm code. In this experiment, a depth of :math:`0m` indicates a
   solid wall and a depth of :math:`-2000m` indicates open ocean. The
   matlab program *input/gendata.m* shows an example of how to generate
   a bathymetry file. The variable is read in the routine . The
   bathymetry file is read in the routine

-  Line 50,

   ::

       zonalWindFile='windx.sin_y'

   This line specifies the name of the file from which the x-direction
   (zonal) surface wind stress is read. This file is also a
   two-dimensional (:math:`x,y`) map and is enumerated and formatted in
   the same manner as the bathymetry file. The matlab program
   *input/gendata.m* includes example code to generate a valid
   **zonalWindFile** file. The variable is read in the routine . The
   wind-stress file is read in the routine

other lines in the file *input/data* are standard values.

<PRE>

</PRE>

File *input/data.pkg*
~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customisations for this experiment.

File *input/eedata*
~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customisations for this experiment.

File *input/windx.sin\_y*
~~~~~~~~~~~~~~~~~~~~~~~~~

The *input/windx.sin\_y* file specifies a two-dimensional (:math:`x,y`)
map of wind stress ,\ :math:`\tau_{x}`, values. The units used are
:math:`Nm^{-2}` (the default for MITgcm). Although :math:`\tau_{x}` is
only a function of latitude, :math:`y`, in this experiment this file
must still define a complete two-dimensional map in order to be
compatible with the standard code for loading forcing fields in MITgcm
(routine *EXTERNAL\_FIELDS\_LOAD*. The included matlab program
*input/gendata.m* gives a complete code for creating the
*input/windx.sin\_y* file.

File *input/topog.box*
~~~~~~~~~~~~~~~~~~~~~~

The *input/topog.box* file specifies a two-dimensional (:math:`x,y`) map
of depth values. For this experiment values are either :math:`0~{\rm m}`
or :math:`-2000\,{\rm m}`, corresponding respectively to a wall or to
deep ocean. The file contains a raw binary stream of data that is
enumerated in the same way as standard MITgcm two-dimensional,
horizontal arrays. The included matlab program *input/gendata.m* gives a
complete code for creating the *input/topog.box* file.

File *code/SIZE.h*
~~~~~~~~~~~~~~~~~~

Two lines are customized in this file for the current experiment

-  Line 39,

   ::

        sNx=60, 

   this line sets the lateral domain extent in grid points for the axis
   aligned with the x-coordinate.

-  Line 40,

   ::

        sNy=60, 

   this line sets the lateral domain extent in grid points for the axis
   aligned with the y-coordinate.

-  Line 49,

   ::

        Nr=4,   

   this line sets the vertical domain extent in grid points.

File *code/CPP\_OPTIONS.h*
~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customisations for this experiment.

File *code/CPP\_EEOPTIONS.h*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customisations for this experiment.

Other Files 
~~~~~~~~~~~~

Other files relevant to this experiment are

-  *model/src/ini\_cori.F*. This file initializes the model coriolis
   variables **fCorU** and **fCorV**.

-  *model/src/ini\_spherical\_polar\_grid.F* This file initializes the
   model grid discretisation variables **dxF, dyF, dxG, dyG, dxC, dyC**.

-  *model/src/ini\_parms.F*.

Running The Example
-------------------

Code Download
~~~~~~~~~~~~~

In order to run the examples you must first download the code
distribution. Instructions for downloading the code can be found in
section [sec:obtainingCode].

Experiment Location
~~~~~~~~~~~~~~~~~~~

This example experiments is located under the release sub-directory

Running the Experiment
~~~~~~~~~~~~~~~~~~~~~~

To run the experiment

#. Set the current directory to *input/*

   ::

       % cd input

#. Verify that current directory is now correct

   ::

       % pwd

   You should see a response on the screen ending in

   *verification/exp2/input*

#. Run the genmake script to create the experiment *Makefile*

   ::

       % ../../../tools/genmake -mods=../code

#. Create a list of header file dependencies in *Makefile*

   ::

       % make depend

#. Build the executable file.

   ::

       % make

#. Run the *mitgcmuv* executable

   ::

       % ./mitgcmuv

(-----------------------------------------------------save)

Running with MPI
----------------

In this example’s :filelink:`verification/tutorial_barotropic_gyre/code` directory
there is a alternate file :filelink:`verification/tutorial_barotropic_gyre/code/SIZE.h_mpi`.
Overwrite :filelink:`verification/tutorial_barotropic_gyre/code/SIZE.h` with this file
and re-compile the model following instructions detailed in
:numref:`build_mpi`. Several lines in :filelink:`verification/tutorial_barotropic_gyre/code/SIZE.h_mpi` are different.
Instead of a tile size :math:`60 \times 60`, we’re now using using a tile size of :math:`30 \times 30`:

   .. literalinclude:: ../../verification/tutorial_barotropic_gyre/code/SIZE.h_mpi
       :start-at: sNx =
       :end-at: sNy =
       :lineno-match:

To cover the full domain, we need four of these smaller tiles,
two tiles in the both :math:`x`- and :math:`y`-directions.
The following change will result in each tile running on a separate MPI process:

   .. literalinclude:: ../../verification/tutorial_barotropic_gyre/code/SIZE.h_mpi
       :start-at: nPx =
       :end-at: nPy =
       :lineno-match:

When running the model (using ``mpirun`` or the specific syntax required by your local openmpi implementation),
it is no longer necessary to redirect standard output
to a file such as ``output.txt``; rather, separate ``STDOUT.xxxx`` and ``STDERR.xxxx``
files are created by each process, where ``xxxx`` is the process number (starting from ``0000``).
Other than some additional MPI-related information and different grid information
(related to the different tile size), the standard output content is identical to
that from the single-process run. Note that reported monitor statistics and cg2d
information are evaluated over the global domain.

Output files ``grid``, ``state``, ``phiHyd``, and ``phiHydLow`` are now written separately for each tile;
``t001`` in the file name is tile 1, ``t002`` in the file name is tile 2, etc.
The pickup files are also written separately for each tile, but
for raw binary the naming scheme is somewhat different:
tile 1 includes ``.001.001`` in the file name, tile 2 ``.002.001``, tile 3 ``.001.002``, and tile 4 ``.002.002``.

Running with OpenMP
-------------------

[At this time, MNC is automatically disabled for multi-threaded.
So either we change this first tutorial to use mdsio, or we nix this
variant here and do in baroclinic gyre, which will use mdsio.]

(if included: show change to SIZE.h, eedata, and type env OMPfoo env vars
and then run normally w/redirected standard output. Output files in tiles.
single STDERR file (given a single process, albeit multi-threaded).

File `input/data.pkg`
~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data.pkg
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/data.pkg

This file sets the (boolean) parameter :varlink:`useMNC` to activate package :filelink:`pkg/mnc`
(i.e. `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ output; see :numref:`pkg_mnc`);
by default, MITgcm writes output in binary files, using package :filelink:`pkg/mdsio` (see :numref:`pkg_mdsio`). 
In :numref:`baro_gyre_data` we specified a non-zero :varlink:`dumpFreq`, which will
write the model state at a specified frequency in `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
format for easy persusal of the model solution in :numref:`barotropic_gyre_solution`.
Otherwise, only standard packages (i.e., those compiled in MITgcm by default) are required for this setup, so no other customization is necessary.


File `input/data.mnc`
~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../verification/tutorial_barotropic_gyre_jrs/input/data.mnc
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/data.mnc

This file is to specify parameter settings which affect package :filelink:`pkg/mnc` behavior.  Here, we are using default
settings except for parameter :varlink:`monitor_mnc`: we are specifying NOT to create separate `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
output files for :filelink:`pkg/monitor` output, but rather to include this monitor output in the standard output file (see :numref:`baro_gyre_build_run`).
See :numref:`pkg_mnc_inputs` for a complete listing of :filelink:`pkg/mnc` namelist parameters and their default settings.

``grid.t001.nc`` - includes all the model grid variables used by MITgcm.
This includes the grid cell center points and separation (:varlink:`XC`, :varlink:`YC`, :varlink:`dxC`, :varlink:`dyC`),
corner point locations and separation (:varlink:`XG`, :varlink:`YG`, :varlink:`dxG`, :varlink:`dyG`),
the separation between velocity points (:varlink:`dyU`, :varlink:`dxV`),
vertical coordinate location and separation (:varlink:`RC`, :varlink:`RF`, :varlink:`drC`, :varlink:`drF`),
grid cell areas (:varlink:`rA`, :varlink:`rAw`, :varlink:`rAs`, :varlink:`rAz`),
and bathymetry information (:varlink:`Depth`, :varlink:`HFacC`, :varlink:`HFacW`, :varlink:`HFacS)`.
See :numref:`spatial_discret_dyn_eq` for definitions and description of the C grid staggering of these variables.
There are also grid variables in vector form that are not used in the MITgcm source code
(X, Y, Xp1, Yp1, Z, Zp1, Zu, Zl); see description in  ``grid.t001.nc``.
Note the file name does NOT contain any iteration number, but does include
a tile number ``t001``  (in our setup we have a single tile, so only one file is generated).


``state.0000000000.t001.nc`` - includes snapshots of state variables U, V, W, Temp, S, and Eta
at model times T (variable iter(T) stores the model iteration corresponding with these model times).
Also included are vector forms of grid variables X, Y, Z, Xp1, Yp1, and Zl.
Note part of the file name ``0000000000`` is the parameter :varlink:`nIter0` for the model run
(recall, we initialized our model with :varlink:`nIter0` =0).
Snapshots of model state are written for model iterations 0, 1000, 2000, ..., 60000
according to our parameter choice :varlink:`dumpFreq`. The tile number ``t001`` is also included in the file name.


``phiHyd.0000000000.t001.nc``, ``phiHydLow.0000000000.t001.nc`` - these are a 3D field of hydrostatic
pressure potential anomaly (:math:`p/\rho_c`, see :numref:`finding_the_pressure_field`)
and a 2D field of bottom hydrostatic pressure potential anomaly, respectively.
These are not MITgcm state variables per se, and are computed during the time step,
ergo they are not included in file ``state.0000000000.t001.nc``. Like ``state.0000000000.t001.nc``
these fields are written at interval according to
:varlink:`dumpFreq`,  however are not written out at time :varlink:`nIter0` (i.e., have one time
record less than ``state.0000000000.t001.nc``). File name convention is
similar to that of ``state.0000000000.t001.nc``.


 
In this experiment, a depth of 0 m indicates a solid wall and a depth of -5000 m
(the value of -\ :varlink:`delR`) indicates open ocean. By default, the domain is assumed
to be doubly periodic (i.e., in **BOTH** the :math:`x` and :math:`y` dimensions).
Thus, placing a wall on the north and east boundaries (as specified here)
effectively bounds all lateral sides of our box domain.


(-------------------------------------------------endsave)


.. _sec_eg_tank:

A Rotating Tank in Cylindrical Coordinates
==========================================

  (in directory: :filelink:`verification/rotating_tank/`)

This example configuration demonstrates using the MITgcm to simulate a
laboratory demonstration using a differentially heated rotating
annulus of water.  The simulation is configured for a laboratory scale
on a :math:`3^{\circ}\times1\mathrm{cm}` cyclindrical grid with twenty-nine
vertical levels of 0.5cm each.  This is a typical laboratory setup for
illustration principles of GFD, as well as for a laboratory data
assimilation project.


example illustration from GFD lab here
 

.. toctree::
   :maxdepth: 1

   rotating_tank/rotating_tank.rst


