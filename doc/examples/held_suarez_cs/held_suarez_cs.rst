.. _sec_held_suarez_cs:

Held-Suarez Atmosphere
======================

(in directory: :filelink:`verification/tutorial_held_suarez_cs/`)

This example illustrates the use of the MITgcm as an atmospheric GCM,
using simple Held and Suarez (1994) :cite:`held-suar:94` forcing to simulate
atmospheric dynamics on global scale. The set-up uses the rescaled
pressure coordinate (:math:`p^*`) of Adcroft and Campin (2004) :cite:`adcroft:04a` in
the vertical direction, with 20 equally-spaced levels, and the conformal
cube-sphere grid (C32) described in Adcroft et al. (2004) :cite:`adcroft:04b`.

Overview
--------

This example demonstrates using the MITgcm to simulate the planetary
atmospheric circulation, with flat orography and simplified forcing. In
particular, only dry air processes are considered and radiation effects
are represented by a simple Newtonian cooling, Thus, this example does
not rely on any particular atmospheric physics package. This kind of
simplified atmospheric simulation has been widely used in GFD-type
experiments and in intercomparison projects of AGCM dynamical cores
(Held and Suarez 1994 :cite:`held-suar:94`).

The horizontal grid is obtain from the projection of a uniform gridded
cube to the sphere. Each of the 6 faces has the same resolution, with
32 :math:`\times` 32 grid points. The equator coincides with a grid
line and crosses through the middle in 4 of the 6 faces, leaving 2 faces
for the northern and southern polar regions. This curvilinear grid
requires the use of the 2\ :sup:`nd` generation exchange topology (:filelink:`pkg/exch2`)
to connect tile and face edges, but without any limitation on the number
of processors.

The use of the :math:`p^*` coordinate with 20 equally spaced levels
(20 :math:`\times` 50 mb, from :math:`p^*=1000` mb to
:math:`0` at the top of the atmosphere) follows the choice of
Held and Suarez (1994) :cite:`held-suar:94`. Note that without topography, the
:math:`p^*` coordinate and the normalized pressure coordinate
(:math:`\sigma_p`) coincide exactly. Both viscosity and diffusivity are
set to zero here, but an 8\ :sup:`th` order Shapiro (1970) :cite:`shapiro:70`
filter is applied to both momentum and potential temperature, to remove
selectively grid scale noise. Apart from the horizontal grid, this
experiment is made very similar to the grid-point model case used in
the Held and Suarez (1994) :cite:`held-suar:94` study.

At this resolution, the configuration can be integrated forward for
many years on a single processor desktop computer.

Forcing
-------

The model is forced by relaxation to a radiative equilibrium temperature
from Held and Suarez (1994) :cite:`held-suar:94`. A linear frictional drag
(Rayleigh damping) is applied in the lower part of the atmosphere and
accounts for surface friction and momentum dissipation in the boundary
layer. Altogether, this yields the following forcing from
Held and Suarez (1994) :cite:`held-suar:94` that is applied to the fluid:

.. math::
   \vec{\boldsymbol{\cal F}_\mathbf{v}} = -k_\mathbf{v}(p)\vec{\mathbf{v}}_h
   :label: eg-hs-global_forcing_fv

.. math::
   {\cal F}_{\theta} = -k_{\theta}(\varphi,p)[\theta-\theta_{eq}(\varphi,p)]
  :label: eg-hs-global_forcing_ft

where :math:`\vec{\boldsymbol{\cal F}_\mathbf{v}}`, :math:`{\cal F}_{\theta}`, are
the forcing terms in the zonal and meridional momentum and in the
potential temperature equations, respectively. The term
:math:`k_\mathbf{v}` in :eq:`eg-hs-global_forcing_fv`
applies a Rayleigh damping that is active within the planetary boundary
layer. It is defined so as to decay as pressure decreases according to

.. math::

   \begin{aligned}
   \label{eq:eg-hs-define_kv}
   k_\mathbf{v} & = k_{f}~\max[0,~(p^*/P^{0}_{s}-\sigma_{b})/(1-\sigma_{b})]
   \\
   \sigma_{b} & = 0.7 ~~{\rm and}~~
   k_{f}  =  1/86400 ~{\rm s}^{-1}\end{aligned}

where :math:`p^*` is the pressure level of the cell center and
:math:`P^{0}_{s}` is the pressure at the base of the atmospheric column,
which is constant and uniform here (:math:`= 10^5 {\rm Pa}`), in the
absence of topography.

The equilibrium temperature :math:`\theta_{eq}` and relaxation time
scale :math:`k_{\theta}` are set to:

.. math::
   \theta_{eq}(\varphi,p^*)  = \max \{ 200 (P^{0}_{s}/p^*)^\kappa,
   \nonumber \hspace{2mm} 315 - \Delta T_y~\sin^2(\varphi)
     - \Delta \theta_z \cos^2(\varphi) \log(p^*/P^{0}_s) \}
   :label: eg-hs-define_Teq

.. math::
   k_{\theta}(\varphi,p^*) =
   k_a + (k_s -k_a)~\cos^4(\varphi)~\max \{ 0,\nonumber \hspace{2mm} (p^*/P^{0}_{s}-\sigma_{b})/(1-\sigma_{b}) \}
   :label: eg-hs-define_kT

with:

.. math::

   \begin{aligned}
    \Delta T_y = 60 \text{ K, }  k_a &= 1/(40 \cdot 86400) ~{\rm s}^{-1}\\
   \Delta \theta_z = 10 \text{ K, }  k_s &= 1/(4 \cdot 86400) ~{\rm s}^{-1}\end{aligned}

Initial conditions correspond to a resting state with horizontally
uniform stratified fluid. The initial temperature profile is simply the
horizontal average of the radiative equilibrium temperature.

Set-up description
------------------

The model is configured in hydrostatic form, using non-Boussinesq
:math:`p^*` coordinate. The vertical resolution is uniform,
:math:`\Delta p^* = 50 \times 10^2` Pa, with 20 levels, from
:math:`p^*=10^5` Pa to :math:`0` at the top. The domain is discretized
using the C32 cube-sphere grid (see Adcroft et al. 2004 :cite:`adcroft:04b`) that covers
the whole sphere with a relatively uniform grid spacing. The resolution
at the equator or along the Greenwich meridian is similar to a
128 :math:`\times` 64 equally spaced longitude-latitude grid, but
requires 25% less grid points. Grid spacing and grid-point
location are not computed by the model, but instead read from files.

The vector-invariant form of the momentum equation (see :numref:`vec_invar_mom_eqs`)
is used so that no explicit metrics are necessary.

Applying the vector-invariant discretization to the atmospheric
equations :eq:`atmos-prime`, and adding the forcing terms
:eq:`eg-hs-global_forcing\_fv`, :eq:`eg-hs-global_forcing_ft` on the
right-hand-side, leads to the set of equations that are solved in this
configuration:

.. math::
   \frac{\partial \vec{\mathbf{v}}_h}{\partial t}
   +(f + \zeta)\hat{\boldsymbol{k}} \times \vec{\mathbf{v}}_h
   + \nabla_{p} (\mathrm{KE})
   + \omega \frac{\partial \vec{\mathbf{v}}_h }{\partial p}
   + \nabla_p \Phi ^{\prime }
   = -k_\mathbf{v} \vec{\mathbf{v}}_h
   :label: eg-hs-model_equations

.. math::
   \frac{\partial \Phi ^{\prime }}{\partial p}
   +\frac{\partial \Pi }{\partial p}\theta ^{\prime } =0

.. math::
    \nabla _{p}\cdot \vec{\mathbf{v}}_h+\frac{\partial \omega }{
   \partial p} =0

.. math::
   \frac{\partial \theta }{\partial t}
   +  \nabla _{p}\cdot (\theta \vec{\mathbf{v}}_h)
   + \frac{\partial (\theta \omega)}{\partial p}
   = -k_{\theta}[\theta-\theta_{\rm eq}]

where :math:`\vec{\mathbf{v}}_h` and :math:`\omega = \frac{Dp}{Dt}` are
the horizontal velocity vector and the vertical velocity in pressure
coordinate, :math:`\zeta` is the relative vorticity and :math:`f` the
Coriolis parameter, :math:`\hat{\boldsymbol{k}}` is the vertical unity
vector, :math:`\mathrm{KE}` is the kinetic energy, :math:`\Phi` is the geopotential, and
:math:`\Pi` the Exner function
(:math:`\Pi = C_p (p/p_c)^\kappa` with :math:`p_c = 10^5` Pa). Primed variables
correspond to anomaly from the resting, uniformly
stratified state.

As described in :numref:`discret_algorithm`, the continuity equation is integrated vertically,
to give a prognostic equation for the surface pressure :math:`p_s`:

.. math::
   \frac{\partial p_s}{\partial t} + \nabla_{h}\cdot \int_{0}^{p_s} \vec{\mathbf{v}}_h dp = 0

The implicit free surface form of the pressure equation described in
Marshall et al. (1997) :cite:`marshall:97a` is employed to solve for :math:`p_s`;
Vertically integrating the hydrostatic balance gives the geopotential
:math:`\Phi'` and allows one to step forward the momentum equation
:eq:`eg-hs-model_equations`. The potential temperature,
:math:`\theta`, is stepped forward using the new velocity field
(see :numref:`adams-bashforth-staggered`).

Numerical Stability Criteria
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The numerical stability for inertial oscillations
(see Adcroft 1995 :cite:`adcroft:95`):

.. math::
    S_{\rm inert} = f^{2} {\Delta t}^2
    :label: eg-hs-inertial_stability

evaluates to :math:`4 \times10^{-3}` at the poles, for
:math:`f=2\Omega\sin(\pi / 2) = 1.45\times10^{-4}~{\rm s}^{-1}`, which
is well below the :math:`S_{\rm inert} < 1` upper limit for stability.
The advective CFL (Adcroft 1995 :cite:`adcroft:95`) for a extreme maximum
horizontal flow speed of :math:`| \vec{\mathbf{u}} | = 90 {\rm m/s}`  and the
smallest horizontal grid spacing :math:`\Delta x = 1.1\times10^5 {\rm m}`:

.. math::
   S_{\rm adv} = \frac{| \vec{\mathbf{u}} | \Delta t}{ \Delta x}
   :label: eg-hs-cfl_stability

evaluates to 0.37, which is close to the stability limit of 0.5.
The stability parameter for internal gravity waves propagating with a
maximum speed of :math:`c_{g}=100~{\rm m/s}` (Adcroft 1995 :cite:`adcroft:95`)

.. math::
   S_{c} = \frac{c_{g} \Delta t}{ \Delta x}
   :label: eg-hs-gfl_stability

evaluates to :math:`4 \times 10^{-1}`. This is close to the linear
stability limit of 0.5.

Experiment Configuration
------------------------

The model configuration for this experiment resides under the directory
:filelink:`verification/tutorial_held_suarez_cs/`. The experiment files

-  :filelink:`verification/tutorial_held_suarez_cs/input/data`

-  :filelink:`verification/tutorial_held_suarez_cs/input/data.pkg`

-  :filelink:`verification/tutorial_held_suarez_cs/input/data.shap`

-  :filelink:`verification/tutorial_held_suarez_cs/input/eedata`

-  :filelink:`verification/tutorial_held_suarez_cs/code/packages.conf`

-  :filelink:`verification/tutorial_held_suarez_cs/code/CPP_OPTIONS.h`

-  :filelink:`verification/tutorial_held_suarez_cs/code/SIZE.h`

-  :filelink:`verification/tutorial_held_suarez_cs/code/DIAGNOSTICS_SIZE.h`

-  :filelink:`verification/tutorial_held_suarez_cs/code/apply_forcing.F`,

contain the code customizations and parameter settings for these
experiments. Below we describe the customizations to these files
associated with this experiment.

File :filelink:`input/data <verification/tutorial_held_suarez_cs/input/data>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_held_suarez_cs/input/data
    :linenos:
    :caption: verification/tutorial_held_suarez_cs/input/data

This file specifies the main parameters
for the experiment. The parameters that are significant for this
configuration are:

-  Lines 7-8,

   ::

        tRef=295.2, 295.5, 295.9, 296.3, 296.7, 297.1, 297.6, 298.1, 298.7, 299.3,
             300.0, 300.7, 301.9, 304.1, 308.0, 315.1, 329.5, 362.3, 419.2, 573.8,

   set reference values for potential temperature (in kelvins) at
   each model level. The entries are ordered like model level, from
   surface up to the top. Density is calculated from anomalies at each
   level evaluated with respect to the reference values set here.

-  Line 10,

   ::

        no_slip_sides=.FALSE.,

   this line selects a free-slip lateral boundary condition for the
   horizontal Laplacian friction operator, e.g.,
   :math:`\frac{\partial u}{\partial y}`\ =0 along boundaries in
   :math:`y` and :math:`\frac{\partial v}{\partial x}`\ =0 along
   boundaries in :math:`x`.

-  Line 11,

   ::

        no_slip_bottom=.FALSE.,

   this line selects a free-slip boundary condition at the top, in the
   vertical Laplacian friction operator, e.g.,
   :math:`\frac{\partial u}{\partial p} = \frac{\partial v}{\partial p} = 0`.

-  Line 12,

   ::

        buoyancyRelation='ATMOSPHERIC',

   this line sets the type of fluid and the type of vertical coordinate
   to use, which, in this case, is air with a pressure-like coordinate
   (:math:`p` or :math:`p^*`).

-  Line 13,

   ::

        eosType='IDEALG',

   Selects the ideal gas equation of state.

-  Line 15,

   ::

        implicitFreeSurface=.TRUE.,

   Selects the way the barotropic equation is solved, here using the
   implicit free-surface formulation.

-  Line 16,

   ::

        exactConserv=.TRUE.,

   Explicitly calculate (again) the surface pressure changes from the
   divergence of the vertically integrated horizontal flow, after the
   implicit free surface solver and filters are applied.

-  Lines 17-18,

   ::

        nonlinFreeSurf=4,
        select_rStar=2,

   Select the non-linear free surface formulation, using :math:`r^*`
   vertical coordinate (here :math:`p^*`). Note that, except for the
   default (= 0), other values of those two parameters are only
   permitted for testing/debugging purpose.

-  Line 21,

   ::

        uniformLin_PhiSurf=.FALSE.,

   Select the linear relation between surface geopotential anomaly and
   surface pressure anomaly to be evaluated from
   :math:`\frac{\partial \Phi_s}{\partial p_s} = 1/\rho(\theta_{\rm ref})`
   (see :numref:`nonlinear-freesurface`). Note that using the default
   (``=.TRUE.``), the constant :math:`1/\rho_0` is used instead, and is not
   necessarily consistent with other parts of the geopotential that
   rely on :math:`\theta_{\rm ref}`.

-  Line 23-24,

   ::

        saltStepping=.FALSE.,
        momViscosity=.FALSE.,

   Do not step forward water vapor and do not compute viscous terms.
   This saves computer time.

-  Line 25,

   ::

        vectorInvariantMomentum=.TRUE.,

   Select the vector-invariant form to solve the momentum equation.

-  Line 26,

   ::

        staggerTimeStep=.TRUE.,

   Select the staggered time-stepping (rather than synchronous time
   stepping).

-  Lines 27-28,

   ::

        readBinaryPrec=64,
        writeBinaryPrec=64,

   Sets format for reading binary input datasets and writing output
   fields to use 64-bit representation for floating-point numbers.

-  Line 33,

   ::

        cg2dMaxIters=200,

   Sets maximum number of iterations the 2-D conjugate
   gradient solver will use, **irrespective of convergence criteria
   being met**.

-  Line 35,

   ::

        cg2dTargetResWunit=1.E-17,

   Sets the tolerance (in units of :math:`\omega`) which the
   2-D conjugate gradient solver will use to test for
   convergence in equation :eq:`elliptic-backward-free-surface` to
   :math:`1 \times 10^{-17}` Pa/s. Solver will iterate until tolerance
   falls below this value or until the maximum number of solver
   iterations is reached.

-  Line 40,

   ::

        deltaT=450.,

   Sets the timestep :math:`\Delta t` used in the model to
   450 seconds (= 1/8 hour).

-  Line 42,

   ::

        startTime=124416000.,

   Sets the starting time, in seconds, for the model time counter. A
   non-zero starting time requires the initial state read from a
   pickup file. By default the pickup file is named according to the
   integer number (:varlink:`nIter0`) of time steps in the :varlink:`startTime` value
   (nIter0 = startTime / deltaT).

-  Line 44,

   ::

       #nTimeSteps=69120,

   A commented out setting for the length of the simulation (in number
   of timesteps) that corresponds to 1-year simulation.

-  Lines 54-55,

   ::

        nTimeSteps=16,
        monitorFreq=1.,

   Sets the length of the simulation (in number of timesteps) and the
   frequency (in seconds) for “monitor” output to 16 iterations and 1
   seconds respectively. This choice corresponds to a short simulation
   test.

-  Line 48,

   ::

        pChkptFreq=31104000.,

   Sets the time interval, in seconds, between 2 consecutive “permanent”
   pickups (“permanent checkpoint frequency”) that are used to restart
   the simulation, to 1 year.

-  Line 48,

   ::

        chkptFreq=2592000.,

   Sets the time interval, in seconds, between two consecutive “temporary”
   pickups (“checkpoint frequency”) to one month. The “temporary” pickup
   file name is alternatively “ckptA” and “ckptB”; these pickups (as
   opposed to the permanent ones) are designed to be over-written by the
   model as the simulation progresses.

-  Line 50,

   ::

        dumpFreq=2592000.,

   Set the frequency (in seconds) for the snapshot output to 1 month.

-  Line 51,

   ::

       #monitorFreq=43200.,

   A commented out line setting the frequency (in seconds) for the
   “monitor” output to 12 h. This frequency fits better with the longer
   simulation of one year.

-  Line 60,

   ::

        usingCurvilinearGrid=.TRUE.,

   Set the horizontal type of grid to curvilinear grid.

-  Line 61,

   ::

        horizGridFile='grid_cs32',

   Set the root for the grid file name to ``grid_cs32``. The
   grid-file names are derived from the root, adding a suffix with the
   face number (e.g., ``.face001.bin``, ``.face002.bin`` :math:`\cdots` )

-  Lines 63,

   ::

        delR=20*50.E2,

   This line sets the increments in pressure
   units to 20 equally thick levels of
   :math:`50 \times 10^2` Pa each.
   This defines the origin (interface :math:`k=1`) of the vertical
   pressure axis, with decreasing pressure as the level index
   :math:`k` increases.

-  Line 68,

   ::

       #topoFile='topo.cs.bin'

   This commented out line would set the file name of a 2-D
   orography file, in units of meters, to ``topo.cs.bin``.

Other lines in the file :filelink:`input/data <verification/tutorial_held_suarez_cs/input/data>`
are standard values that are
described in :numref:`chap_getting_started`..

File :filelink:`input/data.pkg <verification/tutorial_held_suarez_cs/input/data.pkg>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_held_suarez_cs/input/data.pkg
    :linenos:
    :caption: verification/tutorial_held_suarez_cs/input/data.pkg

This file specifies the additional
packages that the model uses for the experiment. Note that some packages
are used by default (e.g., :filelink:`pkg/generic_advdiff`) and some others are
selected according to parameter in :filelink:`input/data.pkg <verification/tutorial_held_suarez_cs/input/data.pkg>` (e.g.,
:filelink:`pkg/mom_vecinv`). The additional packages that are used for this
configuration are

-  Line 3,

   ::

        useSHAP_FILT=.TRUE.,

   This line selects the Shapiro filter (Shapiro 1970 :cite:`shapiro:70`)
   (:filelink:`pkg/shap_filt`) to be used in this experiment.

-  Line 4,

   ::

        useDiagnostics=.TRUE.,

   This line selects :filelink:`pkg/diagnostics` to be used in this
   experiment.

-  Line 5,

   ::

       #useMNC=.TRUE.,

   This line  would select :filelink:`pkg/mnc` for I/O but is commented out.

File :filelink:`input/data.shap <verification/tutorial_held_suarez_cs/input/data.shap>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_held_suarez_cs/input/data.shap
    :linenos:
    :caption: verification/tutorial_held_suarez_cs/input/data.shap

This file specifies the parameters that
the model uses for the Shapiro filter package
(Shapiro 1970 :cite:`shapiro:70`), see :numref:`shapiro_filter`. The
parameters that are significant for this configuration are:

-  Line 5,

   ::

        Shap_funct=2,

   This line selects which Shapiro filter function to use, here S2, for this
   experiment (see :numref:`shapiro_filter`).

-  Lines 6-7,

   ::

        nShapT=0,
        nShapUV=4,

   These lines select the order of the Shapiro filter for active tracers
   (:math:`\theta` and :math:`q`) and momentum (:math:`u,v`)
   respectively. In this case, no filter is applied to active tracers.
   Regarding the momentum, this sets the integer parameter :math:`n` to
   4, in the equations of :numref:`shapiro_filter`, which
   corresponds to a 8th order filter.

-  Line 9,

   ::

        nShapUVPhys=4,

   This line selects the order of the physical space filter (filter
   function S2g, see :numref:`shapiro_filter`) that applies to
   :math:`u,v`. The difference :varlink:`nShapUV` - :varlink:`nShapUVPhys` corresponds to
   the order of the computational filter (filter function S2c, see :numref:`shapiro_filter`).

-  Lines 12-13,

   ::

       #Shap_Trtau=5400.,
       #Shap_uvtau=1800.,

   These commented lines would have set the time scale of the filter (in
   seconds), for :math:`\theta`, :math:`q` and for :math:`u`,
   :math:`v` respectively, to 5400 s (90 min) and 1800 s (30 min).
   Without explicitly setting those timescales, the
   default is used, which corresponds to the model timestep.

File :filelink:`input/eedata <verification/tutorial_held_suarez_cs/input/eedata>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_held_suarez_cs/input/eedata
    :linenos:
    :caption: verification/tutorial_held_suarez_cs/input/eedata

This file uses standard default values except line 6:

::

     useCubedSphereExchange=.TRUE.,

This line selects the cubed-sphere specific exchanges to to connect
tiles and faces edges.

File :filelink:`code/SIZE.h <verification/tutorial_held_suarez_cs/code/SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_held_suarez_cs/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_held_suarez_cs/code/SIZE.h

Four lines are customized in this file for the current experiment

-  Line 45,

   ::

        sNx=32,

   sets the lateral domain extent in grid points along the :math:`x`-direction,
   for one face.

-  Line 46,

   ::

        sNy=32,

   sets the lateral domain extent in grid points along the :math:`y`-direction,
   for one face.

-  Line 49,

   ::

        nSx=6,

   sets the number of tiles in the :math:`x`-direction, for the model domain
   decomposition. In this simple case (single processor, with one tile per
   face), this number corresponds to the total number of faces.

-  Line 55,

   ::

        Nr=20,

   sets the vertical domain extent in grid points.

File :filelink:`code/packages.conf <verification/tutorial_held_suarez_cs/code/packages.conf>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_held_suarez_cs/code/packages.conf
    :linenos:
    :caption: verification/tutorial_held_suarez_cs/input/code/packages.conf

This file specifies the packages that are
compiled and made available for this experiment. The additional packages
that are used for this configuration are

-  Line 1,

   ::

        gfd

   This line selects the standard set of packages that are used by
   default.

-  Line 2,

   ::

        shap_filt

   This line makes the Shapiro filter package available for this
   experiment.

-  Line 3,

   ::

        exch2

   This line selects :filelink:`pkg/exch2` to be compiled and used in this
   experiment. Note that at present, no such
   parameter ``useEXCH2`` exists and therefore this package is
   always used when it is compiled.

-  Line 4,

   ::

        diagnostics

   This line selects  :filelink:`pkg/diagnostics` to be compiled, and makes it
   available for this experiment.

-  Line 5,

   ::

        mnc

   This line selects the :filelink:`pkg/mnc` to be compiled, and makes it
   available for this experiment.

File :filelink:`code/CPP_OPTIONS.h <verification/tutorial_held_suarez_cs/code/CPP_OPTIONS.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses the standard default except for:

::

    #define NONLIN_FRSURF

This line enables the non-linear free-surface part of the code,
which is required for the :math:`p^*` coordinate formulation.

Other Files
~~~~~~~~~~~~

Other files relevant to this experiment are

-  :filelink:`code/apply_forcing.F <verification/tutorial_held_suarez_cs/code/apply_forcing.F>`

-  ``input/grid_cs32.face00[n].bin``, with :math:`n=1,2,3,4,5,6`

contain the code customizations and binary input files for this experiment.
The file :filelink:`apply_forcing.F <verification/tutorial_held_suarez_cs/code/apply_forcing.F>`
contains four subroutines that
calculate the forcing terms (i.e., right-hand side terms) in the momentum
equation :eq:`eg-hs-global_forcing_fv`, :varlink:`EXTERNAL_FORCING_U`
and :varlink:`EXTERNAL_FORCING_V` and in the potential temperature equation
:eq:`eg-hs-global_forcing_ft`, :varlink:`EXTERNAL_FORCING_T`. The
water-vapor forcing subroutine (:varlink:`EXTERNAL_FORCING_S`) is left
empty for this experiment.
The grid-files ``input/grid_cs32.face00[n].bin``, with
:math:`n=1,2,3,4,5,6`, are binary files (direct-access, big-endian
64 bit reals) that contains all the cubed-sphere grid lengths, areas
and grid-point positions, with one file per face. Each file contains
18 2-D arrays (dimension 33 :math:`\times` 33) that correspond to the
model variables: *XC YC DXF DYF RA XG YG DXV DYU RAZ DXC DYC RAW RAS
DXG DYG AngleCS AngleSN* (see :filelink:`model/inc/GRID.h`)
