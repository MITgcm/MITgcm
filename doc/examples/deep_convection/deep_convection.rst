.. _sec_deep_convection:

Deep Convection
===============

(in directory: :filelink:`verification/tutorial_deep_convection/`)

   .. figure:: figs/simulation_config.png
       :width: 80%
       :align: center
       :alt: deep convection setup
       :name: tut_deep_cvct_config

       Schematic of simulation domain for the surface driven convection experiment. The domain is doubly periodic with an initially uniform temperature of 20 :sup:`o`\ C.

This experiment, :numref:`tut_deep_cvct_config`, showcasing
MITgcm’s non-hydrostatic capability, was designed to explore the
temporal and spatial characteristics of convection plumes as they might
exist during a period of oceanic deep convection. It is

-  non-hydrostatic

-  doubly-periodic with cubic geometry

-  discretized with 50 m resolution in :math:`x, y, z`

-  Cartesian

-  on an :math:`f`-plane

-  using a linear equation of state

Overview
--------

The model domain consists of an approximately 3 km square by 1 km deep
box of initially unstratified, resting fluid. The domain is doubly
periodic.

The experiment has 20 levels in the vertical, each of equal thickness
:math:`\Delta z =` 50 m (the horizontal resolution is also 50 m). The
fluid is initially unstratified with a uniform reference potential
temperature :math:`\theta =` 20 :sup:`o`\ C. The equation of state
used in this experiment is linear

.. math::
    \rho = \rho_{0} ( 1 - \alpha_{\theta}\theta^{'} )
    :label: eg-bconv-linear1_eos

which is implemented in the model as a density anomaly equation

.. math::
    \rho^{'} = -\rho_{0}\alpha_{\theta}\theta^{'}
   :label: eg-bconv-linear1_eos_pert

with :math:`\rho_{0}=1000\,{\rm kg\,m}^{-3}` and
:math:`\alpha_{\theta}=2\times10^{-4}\,{\rm degrees}^{-1}`. Integrated
forward in this configuration, the model state variable :varlink:`theta` is
equivalent to either in-situ temperature, :math:`T`, or potential
temperature, :math:`\theta`. For consistency with other examples, in
which the equation of state is non-linear, we use :math:`\theta` to
represent temperature here. This is the quantity that is carried in the
model core equations.

As the fluid in the surface layer is cooled (at a mean rate of 800
Wm\ :math:`^2`), it becomes convectively unstable and overturns, at
first close to the grid-scale, but, as the flow matures, on larger
scales (:numref:`tut_deep_cvct_vert_section` and
:numref:`tut_deep_cvct_surf_section`), under the influence of rotation
(:math:`f_o = 10^{-4}` s\ :math:`^{-1}`).

   .. figure:: figs/verticalsection.png
       :width: 80%
       :align: center
       :alt: vertical section deep cvct exp
       :name: tut_deep_cvct_vert_section

       Vertical section

  .. figure:: figs/surfacesection.png
       :width: 80%
       :align: center
       :alt: surface section deep cvct exp
       :name: tut_deep_cvct_surf_section

       Surface section

Model parameters are specified in file :filelink:`input/data <verification/tutorial_deep_convection/input/data>`. The grid dimensions
are prescribed in :filelink:`code/SIZE.h <verification/tutorial_deep_convection/code/SIZE.h>`. The forcing (file ``input/Qsurf.bin``) is
specified in a binary data file generated using the Matlab script
:filelink:`input/gendata.m <verification/tutorial_deep_convection/input/gendata.m>`.

Equations solved
----------------

The model is configured in non-hydrostatic form, that is, all terms in
the Navier Stokes equations are retained and the pressure field is
found, subject to appropriate boundary conditions, through inversion of
a 3-D elliptic equation.

The implicit free surface form of the pressure equation described in
Marshall et. al (1997) :cite:`marshall:97a` is employed. A
horizontal Laplacian operator :math:`\nabla_{h}^2` provides viscous
dissipation. The thermodynamic forcing appears as a sink in the
equation for potential temperature :math:`\theta`. This produces a set of equations
solved in this configuration as follows:

.. math::

   \begin{aligned}
   \frac{Du}{Dt} - fv +
     \frac{1}{\rho}\frac{\partial p^{'}}{\partial x} -
     \nabla_{h}\cdot A_{h}\nabla_{h}u -
     \frac{\partial}{\partial z}A_{z}\frac{\partial u}{\partial z}
    & =
   \begin{cases}
   0 & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}
   \\
   \frac{Dv}{Dt} + fu +
     \frac{1}{\rho}\frac{\partial p^{'}}{\partial y} -
     \nabla_{h}\cdot A_{h}\nabla_{h}v -
     \frac{\partial}{\partial z}A_{z}\frac{\partial v}{\partial z}
   & =
   \begin{cases}
   0 & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}
   \\
   \frac{Dw}{Dt} + g \frac{\rho^{'}}{\rho} +
     \frac{1}{\rho}\frac{\partial p^{'}}{\partial z} -
     \nabla_{h}\cdot A_{h}\nabla_{h}w -
     \frac{\partial}{\partial z}A_{z}\frac{\partial w}{\partial z}
   & =
   \begin{cases}
   0 & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}
   \\
   \frac{\partial u}{\partial x} +
   \frac{\partial v}{\partial y} +
   \frac{\partial w}{\partial z} +
   &=
   0
   \\
   \frac{D\theta}{Dt} -
    \nabla_{h}\cdot K_{h}\nabla_{h}\theta
    - \frac{\partial}{\partial z}K_{z}\frac{\partial\theta}{\partial z}
   & =
   \begin{cases}
   {\cal F}_\theta & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}
   \end{aligned}

where :math:`u=\frac{Dx}{Dt}`, :math:`v=\frac{Dy}{Dt}` and
:math:`w=\frac{Dz}{Dt}` are the components of the flow vector in
directions :math:`x`, :math:`y` and :math:`z`. The pressure is
diagnosed through inversion (subject to appropriate boundary
conditions) of a 3-D elliptic equation derived from the divergence of
the momentum equations and continuity (see :numref:`finding_the_pressure_field`).

Discrete numerical configuration
--------------------------------

The domain is discretized with a uniform grid spacing in each direction.
There are 64 grid cells in directions :math:`x` and :math:`y` and 20
vertical levels thus the domain comprises a total of just over 80,000
gridpoints.

Numerical stability criteria and other considerations
-----------------------------------------------------

For a heat flux of 800 Wm\ :math:`^2` and a rotation rate of
:math:`10^{-4}` s\ :math:`^{-1}` the plume-scale can be expected to be a
few hundred meters guiding our choice of grid resolution. This in turn
restricts the timestep we can take. It is also desirable to minimize the
level of diffusion and viscosity we apply.

For this class of problem it is generally the advective time-scale which
restricts the timestep.

For an extreme maximum flow speed of :math:`| \vec{u} | = 1 ms^{-1}`,
at a resolution of 50 m, the implied maximum timestep for stability,
:math:`\delta t_u` is

.. math::
    \delta t_u = \frac{\Delta x}{| \vec{u} |} = 50 s

The choice of :math:`\delta t = 10` s is a safe 20 percent of this
maximum.

Interpreted in terms of a mixing-length hypothesis, a magnitude of
Laplacian diffusion coefficient :math:`\kappa_h (=
\kappa_v) = 0.1` m\ :math:`^2`\ s\ :math:`^{-1}` is consistent with an
eddy velocity of 2 mm s\ :math:`^{-1}` correlated over 50 m.

Experiment configuration
------------------------

The model configuration for this experiment resides under the directory
*verification/convection/*. The experiment files

-  :filelink:`code/CPP_OPTIONS.h <verification/tutorial_deep_convection/code/CPP_OPTIONS.h>`
-  :filelink:`code/SIZE.h <verification/tutorial_deep_convection/code/SIZE.h>`
-  :filelink:`input/data <verification/tutorial_deep_convection/input/data>`
-  :filelink:`input/data.pkg <verification/tutorial_deep_convection/input/data.pkg>`
-  :filelink:`input/eedata <verification/tutorial_deep_convection/input/eedata>`
-  ``input/Qsurf.bin``,

contain the code customizations and parameter settings for this
experiment. Below we describe these experiment-specific customizations.

File :filelink:`code/CPP_OPTIONS.h <verification/tutorial_deep_convection/code/CPP_OPTIONS.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File :filelink:`code/SIZE.h <verification/tutorial_deep_convection/code/SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_deep_convection/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_deep_convection/code/SIZE.h

Three lines are customized in this file. These prescribe the domain grid
dimensions.

-  Line 45,

   ::

        sNx=50,

   this line sets the lateral domain extent in grid points for the axis
   aligned with the :math:`x`-coordinate.

-  Line 46,

   ::

        sNy=50,

   this line sets the lateral domain extent in grid points for the axis
   aligned with the :math:`y`-coordinate.

-  Line 55,

   ::

        Nr=50,

   this line sets the vertical domain extent in grid points.

File :filelink:`input/data <verification/tutorial_deep_convection/input/data>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_deep_convection/input/data
    :linenos:
    :caption: verification/tutorial_deep_convection/input/data

This file specifies the main parameters
for the experiment. The parameters that are significant for this
configuration are

-  Line 7,

   ::

      tRef=20*20.0,

   this line sets the initial and reference values of potential
   temperature at each model level in units of
   :math:`^{\circ}\mathrm{C}`. Here the value is arbitrary since, in
   this case, the flow evolves independently of the absolute magnitude
   of the reference temperature. For each depth level the initial and
   reference profiles will be uniform in :math:`x` and :math:`y`.

-  Line 8,

   ::

      sRef=20*35.0,

   this line sets the initial and reference values of salinity at each
   model level in units of ppt. In this case salinity is set to an
   (arbitrary) uniform value of 35.0 ppt. However since, in this
   example, density is independent of salinity, an appropriately defined
   initial salinity could provide a useful passive tracer. For each
   depth level the initial and reference profiles will be uniform in
   :math:`x` and :math:`y`.

-  Line 9,

   ::

      viscAh=0.1,

   this line sets the horizontal Laplacian dissipation coefficient to
   0.1 :math:`{\rm m^{2}s^{-1}}`. Boundary conditions for this operator
   are specified later.

-  Line 10,

   ::

      viscAz=0.1,

   this line sets the vertical Laplacian frictional dissipation
   coefficient to 0.1 :math:`{\rm m^{2}s^{-1}}`. Boundary conditions for
   this operator are specified later.

-  Line 11,

   ::

      no_slip_sides=.FALSE.

   this line selects a free-slip lateral boundary condition for the
   horizontal Laplacian friction operator e.g.
   :math:`\frac{\partial u}{\partial y}`\ =0 along boundaries in
   :math:`y` and :math:`\frac{\partial v}{\partial x}`\ =0 along
   boundaries in :math:`x`.

-  Lines 12,

   ::

       no_slip_bottom=.TRUE.

   this line selects a no-slip boundary condition for the bottom
   boundary condition in the vertical Laplacian friction operator e.g.,
   :math:`u=v=0` at :math:`z=-H`, where :math:`H` is the local depth of
   the domain.

-  Line 13,

   ::

       diffKhT=0.1,

   this line sets the horizontal diffusion coefficient for temperature
   to 0.1 :math:`\rm m^{2}s^{-1}`. The boundary condition on this
   operator is
   :math:`\frac{\partial}{\partial x}=\frac{\partial}{\partial y}=0` at
   all boundaries.

-  Line 14,

   ::

       diffKzT=0.1,

   this line sets the vertical diffusion coefficient for temperature to
   0.1 :math:`{\rm m^{2}s^{-1}}`. The boundary condition on this
   operator is :math:`\frac{\partial}{\partial z}` = 0 on all
   boundaries.

-  Line 15,

   ::

       f0=1E-4,

   this line sets the Coriolis parameter to :math:`1 \times 10^{-4}`
   s\ :math:`^{-1}`. Since :math:`\beta = 0.0` this value is then
   adopted throughout the domain.

-  Line 16,

   ::

       beta=0.E-11,

   this line sets the the variation of Coriolis parameter with latitude
   to :math:`0`.

-  Line 17,

   ::

       tAlpha=2.E-4,

   This line sets the thermal expansion coefficient for the fluid to
   :math:`2 \times 10^{-4}` :sup:`o`\ C\ :math:`^{-1}`.

-  Line 18,

   ::

       sBeta=0,

   This line sets the saline expansion coefficient for the fluid to
   :math:`0`, consistent with salt’s passive role in this example.

-  Line 23-24,

   ::

       rigidLid=.FALSE.,
       implicitFreeSurface=.TRUE.,

   Selects the barotropic pressure equation to be the implicit free
   surface formulation.

-  Line 26,

   ::

       eosType='LINEAR',

   Selects the linear form of the equation of state.

-  Line 27,

   ::

       nonHydrostatic=.TRUE.,

   Selects for non-hydrostatic code.

-  Line 33,

   ::

       cg2dMaxIters=1000,

   Inactive - the pressure field in a non-hydrostatic simulation is
   inverted through a 3-D elliptic equation.

-  Line 34,

   ::

       cg2dTargetResidual=1.E-9,

   Inactive - the pressure field in a non-hydrostatic simulation is
   inverted through a 3-D elliptic equation.

-  Line 35,

   ::

       cg3dMaxIters=40,

   This line sets the maximum number of iterations the
   3-D conjugate gradient solver will use to 40,
   **irrespective of the convergence criteria being met**.

-  Line 36,

   ::

       cg3dTargetResidual=1.E-9,

   Sets the tolerance which the 3-D conjugate gradient
   solver will use to test for convergence in equation :eq:`phi-nh` to
   :math:`1 \times 10^{-9}`. The solver will iterate until the tolerance
   falls below this value or until the maximum number of solver
   iterations is reached.

-  Line43,

   ::

       nTimeSteps=8640.,

   Sets the number of timesteps at which this simulation will terminate
   (in this case 8640 timesteps or 1 day or :math:`\delta t = 10` s). At
   the end of a simulation a checkpoint file is automatically written so
   that a numerical experiment can consist of multiple stages.

-  Line 44,

   ::

       deltaT=10,

   Sets the timestep :math:`\delta t` to 10 s.

-  Line 57,

   ::

       dXspacing=50.0,

   Sets horizontal (:math:`x`-direction) grid interval to 50 m.

-  Line 58,

   ::

       dYspacing=50.0,

   Sets horizontal (:math:`y`-direction) grid interval to 50 m.

-  Line 59,

   ::

       delZ=20*50.0,

   Sets vertical grid spacing to 50 m. Must be consistent with
   :filelink:`code/SIZE.h <verification/tutorial_deep_convection/code/SIZE.h>`.
   Here, 20 corresponds to the number of vertical levels.

-  Line64,

   ::

       surfQfile='Qsurf.bin'

   This line specifies the name of the file from which the surface heat
   flux is read. This file is a 2-D (:math:`x,y`) map. It is
   assumed to contain 64-bit binary numbers giving the value of
   :math:`Q` (W m\ :math:`^2`) to be applied in each surface grid cell,
   ordered with the :math:`x` coordinate varying fastest. The points are
   ordered from low coordinate to high coordinate for both axes. The
   matlab program :filelink:`input/gendata.m <verification/tutorial_deep_convection/input/gendata.m>`
   shows how to generate the surface
   heat flux file used in the example.

File :filelink:`input/data.pkg <verification/tutorial_deep_convection/input/data.pkg>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File :filelink:`input/eedata <verification/tutorial_deep_convection/input/eedata>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File ``input/Qsurf.bin``
~~~~~~~~~~~~~~~~~~~~~~~~

The file ``input/Qsurf.bin`` specifies a 2-D (:math:`x,y`) map
of heat flux values where
:math:`Q = Q_o \times ( 0.5 + \mbox{random number between 0 and 1})`.

In the example :math:`Q_o = 800` W m\ :math:`^{-2}` so that values of
:math:`Q` lie in the range 400 to 1200 W m\ :math:`^{-2}` with a mean of
:math:`Q_o`. Although the flux models a loss, because it is directed
upwards, according to the model’s sign convention, :math:`Q` is
positive.

