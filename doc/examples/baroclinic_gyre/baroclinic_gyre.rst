
.. _tutorial_baroclinic_gyre:

Baroclinic Ocean Gyre
=====================

(in directory: :filelink:`verification/tutorial_baroclinic_gyre`)

This section describes an example experiment using MITgcm to simulate a
baroclinic, wind and buoyancy-forced, double-gyre ocean circulation. Unlike tutorial
:ref:`Barotropic Ocean Gyre <sec_eg_baro>`, which used a Cartesian grid and a single vertical
layer, here the grid employs spherical polar coordinates with 15 vertical layers.
The configuration is similar to the double-gyre setup first solved numerically
in Cox and Bryan (1984) :cite:`cox:84`: the model is configured to
represent an enclosed sector of fluid on a sphere, spanning the tropics to mid-latitudes,
:math:`60^{\circ} \times 60^{\circ}` in lateral extent.
The fluid is :math:`1.8` km deep and is forced by a zonal wind
stress which is constant in time, :math:`\tau_{\lambda}`, varying sinusoidally in the
north-south direction. The Coriolis parameter, :math:`f`, is defined
according to latitude :math:`\varphi`

.. math::
   f(\varphi) = 2 \Omega \sin( \varphi )

with the rotation rate, :math:`\Omega` set to :math:`\frac{2 \pi}{86164} \text{s}^{-1}` (i.e., corresponding the to standard Earth rotation rate).
The sinusoidal wind-stress variations are defined according to

.. math::
   \tau_{\lambda}(\varphi) = -\tau_{0}\cos \left(2 \pi \frac{\varphi-\varphi_o}{L_{\varphi}} \right)

where :math:`L_{\varphi}` is the lateral domain extent
(:math:`60^{\circ}`), :math:`\varphi_o` is set to :math:`15^{\circ} \text{N}` and :math:`\tau_0` is :math:`0.1 \text{ N m}^{-2}`.
:numref:`baroclinic_gyre_config` summarizes the
configuration simulated. As indicated by the axes in the lower left of the figure, the
model code works internally in a locally orthogonal coordinate
:math:`(x,y,z)`. For this experiment description the local orthogonal
model coordinate :math:`(x,y,z)` is synonymous with the coordinates
:math:`(\lambda,\varphi,r)` shown in :numref:`sphere_coor`.
Initially the fluid is stratified
with a reference potential temperature profile that varies from :math:`\theta=30 \text{ } ^{\circ}`\ C
in the surface layer to :math:`\theta=2 \text{ } ^{\circ}`\ C in the bottom layer.
The equation of state used in this experiment is linear:

.. math::
   \rho = \rho_{0} ( 1 - \alpha_{\theta}\theta^{\prime} )
  :label: rho_lineareos

which is implemented in the model as a density anomaly equation

.. math::
   \rho^{\prime} = -\rho_{0}\alpha_{\theta}\theta^{\prime}
   :label: rhoprime_lineareos

with :math:`\rho_{0}=999.8\,{\rm kg\,m}^{-3}` and
:math:`\alpha_{\theta}=2\times10^{-4}\,{\rm K}^{-1}`.
Given the linear equation of state, in this configuration the model state variable for temperature is
equivalent to either in-situ temperature, :math:`T`, or potential
temperature, :math:`\theta`. For consistency with later examples, in
which the equation of state is non-linear, here we use the variable :math:`\theta` to
represent temperature.

  .. figure:: figs/baroclinic_gyre_config.png
      :width: 95%
      :align: center
      :alt: baroclinic gyre configuration
      :name: baroclinic_gyre_config

      Schematic of simulation domain and wind-stress forcing function for baroclinic gyre numerical experiment. The domain is enclosed by solid walls.

Temperature is restored in the surface layer to a linear profile:

.. math::
   {\cal F}_\theta = - \frac{1}{\tau_{\theta}} (\theta-\theta^*), \phantom{WWW}
   \theta^* = \frac{\theta_{\rm max} - \theta_{\rm min}}{L_\varphi} (\varphi - \varphi_o)
   :label: baroc_restore_theta

where the relaxation timescale :math:`\tau_{\theta} = 30` days and :math:`\theta_{\rm max}=30^{\circ}` C, :math:`\theta_{\rm min}=0^{\circ}` C.

.. _baroc_eq_solved:

Equations solved
----------------

For this problem the implicit free surface, **HPE**
form of the equations (see :numref:`hydro_and_quasihydro`; :numref:`press_meth_linear`)
described in Marshall et al. (1997) :cite:`marshall:97a` are
employed. The flow is three-dimensional with just temperature,
:math:`\theta`, as an active tracer.
The viscous and diffusive terms provides viscous dissipation
and a diffusive sub-grid scale closure for the momentum and temperature equations, respectively.
A wind-stress momentum forcing is added to the
momentum equation for the zonal flow, :math:`u`. Other terms in the
model are explicitly switched off for this experiment configuration (see
:numref:`sec_eg_baroclinic_code_config`). This yields an active set of
equations solved in this configuration, written in spherical polar
coordinates as follows:

.. math::
   \frac{Du}{Dt} - fv -\frac{uv}{a}\tan{\varphi} +
   \frac{1}{\rho_c a \cos{\varphi}}\frac{\partial p^{\prime}}{\partial \lambda} +
    \nabla _h \cdot (-A_{h}  \nabla _h u) + \frac{\partial}{\partial z} \left( -A_{z}\frac{\partial u}{\partial z} \right)
   =  \mathcal{F}_u
   :label: baroc_gyre_umom

.. math::
   \frac{Dv}{Dt} + fu + \frac{u^2}{a}\tan{\varphi} +
   \frac{1}{\rho_c a}\frac{\partial p^{\prime}}{\partial \varphi} +
    \nabla _h \cdot (-A_{h}  \nabla _h v) + \frac{\partial}{\partial z} \left( -A_{z}\frac{\partial v}{\partial z} \right)
   = \mathcal{F}_v
   :label: baroc_gyre_vmom

.. math::
   \frac{\partial \eta}{\partial t} + \frac{1}{a \cos{\varphi}}  \left( \frac{\partial H \widehat{u}}{\partial \lambda} +
   \frac{\partial H \widehat{v} \cos{\varphi}}{\partial \varphi} \right) = 0
   :label: baroc_gyre_cont

.. math::
   \frac{D\theta}{Dt} +  \nabla _h \cdot (-\kappa_{h}  \nabla _h \theta)
   + \frac{\partial}{\partial z} \left( -\kappa_{z}\frac{\partial \theta}{\partial z} \right)
   = \mathcal{F}_\theta
   :label: barooc_gyre_theta

.. math::
   p^{\prime} =    g\rho_{c} \eta + \int^{0}_{z} g \rho^{\prime} dz
   :label: baroc_gyre_press

where :math:`u` and :math:`v` are the components of the horizontal flow
vector :math:`\vec{u}` on the sphere
(:math:`u=\dot{\lambda},v=\dot{\varphi}`), :math:`a` is the distance from the center of the Earth,
:math:`\rho_c` is a fluid density (which appears in the momentum equations,
and can be set differently than :math:`\rho_0` in :eq:`rhoprime_lineareos`),
:math:`A_h` and :math:`A_v` are horizontal and vertical viscosity, and
:math:`\kappa_h` and :math:`\kappa_v` are horizontal and vertical diffusivity, respectively.
The terms :math:`H\widehat{u}` and :math:`H\widehat{v}` are the components of the
vertical integral term given in equation :eq:`free-surface` and explained
in more detail in :numref:`press_meth_linear`.
However, for the problem presented here, the continuity relation
:eq:`baroc_gyre_cont` differs from the general form
given in :numref:`press_meth_linear`, equation :eq:`linear-free-surface=P-E`
because the source terms
:math:`{\mathcal P}-{\mathcal E}+{\mathcal R}` are all zero.

The forcing terms :math:`\mathcal{F}_u`, :math:`\mathcal{F}_v`, and :math:`\mathcal{F}_\theta` are
applied as source terms in the model surface layer and are zero in the interior.
The windstress forcing, :math:`{\mathcal F}_u` and :math:`{\mathcal F}_v`, is
applied in the zonal and meridional momentum
equations, respectively; in this configuration, :math:`\mathcal{F}_u = \tau_x / (\rho_c\Delta z_s)`
(where :math:`\Delta z_s` is the depth of the surface model gridcell), and
:math:`\mathcal{F}_v = 0`. Similarly, :math:`\mathcal{F}_\theta` is applied in the temperature equation,
as given by :eq:`baroc_restore_theta`.

In :eq:`baroc_gyre_press` the pressure field, :math:`p^{\prime}`, is separated into a barotropic
part due to variations in sea-surface height, :math:`\eta`, and a
hydrostatic part due to variations in density, :math:`\rho^{\prime}`,
integrated through the water column. Note the :math:`g` in the first term on the right hand side is
MITgcm parameter :varlink:`gBaro` whereas in the seond term :math:`g` is parameter :varlink:`gravity`;
allowing for different gravity constants here is useful, for example, if one wanted to slow down external gravity waves.

In the momentum equations, lateral and vertical boundary conditions for
the :math:`\nabla_{h}^{2}` and :math:`\partial_z^2` operators are specified in the
runtime configuration - see :numref:`sec_eg_baroclinic_code_config`.
For temperature, the boundary condition along the bottom and sidewalls is zero-flux.

Discrete Numerical Configuration
--------------------------------

The domain is discretized with a uniform grid spacing in latitude and
longitude :math:`\Delta \lambda=\Delta \varphi=1^{\circ}`, so that there
are 60 active ocean grid cells in the zonal and meridional directions. As in tutorial
:ref:`Barotropic Ocean Gyre <sec_eg_baro>`, a border row of land cells surrounds the
ocean domain, so the full numerical grid size is 62\ :math:`\times`\ 62 in the horizontal.
The domain has 15 levels in the vertical, varying from :math:`\Delta z = 50` m deep in the surface layer
to 190 m deep in the bottom layer, as shown by the faint red lines in :numref:`baroclinic_gyre_config`.
The internal, locally orthogonal,
model coordinate variables :math:`x` and :math:`y` are initialized from
the values of :math:`\lambda`, :math:`\varphi`, :math:`\Delta \lambda`
and :math:`\Delta \varphi` in radians according to:

.. math::
   \begin{aligned} x &= a\cos(\varphi)\lambda, \phantom{WWW} \Delta x = a\cos(\varphi)\Delta \lambda \\
   y &= a\varphi, \phantom{WWWWWW} \Delta y =  a\Delta \varphi \end{aligned}

See :numref:`operators` for additional description of spherical coordinates.

As described in :numref:`tracer_eqns`, the time evolution of
potential temperature :math:`\theta` in :eq:`barooc_gyre_theta`
is evaluated prognostically. The centered
second-order scheme with Adams-Bashforth II time stepping described in
:numref:`sub_tracer_eqns_ab` is used to step forward the
temperature equation.

Prognostic terms in the momentum equations are
solved using flux form as described in :numref:`flux-form_momentum_equations`.
The pressure forces that drive
the fluid motions, :math:`\partial_{\lambda} p^\prime`
and :math:`\partial_{\varphi} p^\prime`, are found by
summing pressure due to surface elevation :math:`\eta` and the
hydrostatic pressure, as discussed in :numref:`baroc_eq_solved`.
The hydrostatic part of the pressure is
diagnosed explicitly by integrating density.
The sea-surface height is found by solving implicitly the 2-D (elliptic) surface pressure equation
(see :numref:`press_meth_linear`).

Numerical Stability Criteria
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The analysis in this section is similar to that discussed in
tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`,
albeit with some added wrinkles. In this experiment, we not only have a larger model domain extent, with greater variation
in the Coriolis parameter between the southernmost and northernmost gridpoints, but also significant variation
in the grid :math:`\Delta x` spacing.

In order to choose an appropriate time step, note that our smallest gridcells (i.e., in the far north)
have :math:`\Delta x \approx 29` km, which
is similar to our grid spacing in tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`. Thus, using the advective
CFL condition, first assuming our solution will achieve maximum horizontal advection :math:`|c_{\rm max}|` ~ 1 ms\ :sup:`-1`)

.. math::
   S_{\rm adv} = 2 \left( \frac{ |c_{\rm max}| \Delta t}{ \Delta x} \right) < 0.5 \text{ for stability}
   :label: eq_baroc_cfl_stability

we choose the same time step as in tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`,
:math:`\Delta t` = 1200 s (= 20 minutes), resulting in :math:`S_{\rm adv} = 0.08`.
Also note this time step is stable for propagation of internal gravity waves:
approximating the propagation speed as :math:`\sqrt{g' h}` where :math:`g'` is reduced gravity (our maximum
:math:`\Delta \rho` using our linear equation of state is
:math:`\rho_{0} \alpha_{\theta} \Delta \theta = 6` kg/m\ :sup:`3`) and :math:`h` is the upper layer depth
(we'll assume 150 m), produces an estimated propagation speed generally less than :math:`|c_{\rm max}| = 3` ms\ :sup:`--1`
(see Adcroft 1995 :cite:`adcroft:95` or Gill 1982 :cite:`gill:82`), thus still comfortably below the threshold.

Using our chosen value of :math:`\Delta t`, numerical stability for inertial oscillations using Adams-Bashforth II

.. math::
   S_{\rm inert} = f {\Delta t} < 0.5 \text{ for stability}
   :label: eq_baroc_inertial_stability

evaluates to 0.17 for the largest :math:`f` value in our domain (:math:`1.4\times10^{-4}` s\ :sup:`--1`),
below the stability threshold.

To choose a horizontal Laplacian eddy viscosity :math:`A_{h}`, note that the largest :math:`\Delta x`
value in our domain (i.e., in the south) is :math:`\approx 110` km. With the Munk boundary width as follows,

.. math::
   M = \frac{2\pi}{\sqrt{3}}  \left( \frac { A_{h} }{ \beta } \right) ^{\frac{1}{3}}
   :label: baroc_munk_layer

in order to to have a well resolved boundary current in the subtropical gyre we will set
:math:`A_{h} = 5000` m\ :sup:`2` s\ :sup:`--1`. This results in a boundary current
resolved across two to three grid cells in the southern portion of the domain.

Given that our choice for :math:`A_{h}` in this experiment is an order of magnitude larger than in
tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`,
let's re-examine the stability of horizontal Laplacian friction:

.. math::
   S_{\rm Lh} = 2 \left( 4 \frac{A_{h} \Delta t}{{\Delta x}^2} \right)  < 0.6 \text{ for stability}
   :label: baroc_laplacian_stability

evaluates to 0.057 for our smallest :math:`\Delta x`, which is below the stability threshold.
Note this same stability test also applies to horizontal Laplacian diffusion of tracers, with :math:`\kappa_{h}` replacing
:math:`A_{h}`, but we will choose :math:`\kappa_{h} \ll A_{h}` so this should not pose any stability issues.

Finally, stability of vertical diffusion of momentum:

.. math::
   S_{\rm Lv} = 4 \frac{A_{v} \Delta t}{{\Delta z}^2} < 0.6 \text{ for stability}
   :label: baroc_laplacian_v_stability

Here we will choose :math:`A_{v} = 1\times10^{-2}` m\ :sup:`2` s\ :sup:`--1`,
so :math:`S_{lv}` evaluates to 0.02 for our minimum :math:`\Delta z`,
well below the stability threshold. Note if we were to use Adams Bashforth II for diffusion of tracers
the same check would apply, with :math:`\kappa_{v}` replacing :math:`A_{v}`. However, we will instead choose
an implicit scheme for computing vertical diffusion of tracers (see :numref:`baroc_input_data`), which is unconditionally stable.

.. _sec_eg_baroclinic_code_config:

Configuration
-------------

The model configuration for this experiment resides under the directory :filelink:`verification/tutorial_baroclinic_gyre/`.

The experiment files

 - :filelink:`verification/tutorial_baroclinic_gyre/code/packages.conf`
 - :filelink:`verification/tutorial_baroclinic_gyre/code/SIZE.h`
 - :filelink:`verification/tutorial_baroclinic_gyre/code/DIAGNOSTICS_SIZE.h`
 - :filelink:`verification/tutorial_baroclinic_gyre/input/data`
 - :filelink:`verification/tutorial_baroclinic_gyre/input/data.pkg`
 - :filelink:`verification/tutorial_baroclinic_gyre/input/data.mnc`
 - :filelink:`verification/tutorial_baroclinic_gyre/input/data.diagnostics`
 - :filelink:`verification/tutorial_baroclinic_gyre/input/eedata`
 - verification/tutorial_baroclinic_gyre/input/bathy.bin
 - verification/tutorial_baroclinic_gyre/input/windx_cosy.bin
 - verification/tutorial_baroclinic_gyre/input/SST_relax.bin

contain the code customizations, parameter settings, and input data files for this
experiment. Below we describe these customizations in detail.

.. _tut_baroc_code_config:

Compile-time Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~

File :filelink:`code/packages.conf <verification/tutorial_baroclinic_gyre/code/packages.conf>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/packages.conf
    :linenos:
    :caption: verification/tutorial_baroclinic_gyre/code/packages.conf

Here we specify which MITgcm packages we want to include in our configuration. ``gfd``
is a pre-defined "package group" (see :ref:`using_packages`)
of standard packages necessary for most setups; it is also the :ref:`default compiled packages <default_pkg_list>` setting
and the minimum set of packages necessary for GFD-type setups.
In addition to package group ``gfd`` we include two additional packages (individual packages, not package groups), :filelink:`mnc </pkg/mnc>`
and :filelink:`diagnostics </pkg/diagnostics>`. Package :filelink:`mnc </pkg/mnc>` is required
for output to be dumped in `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ format. Package :filelink:`diagnostics </pkg/diagnostics>`
allows one to choose output from a extensive list of model diagnostics, and specify output frequency, with multiple time averaging or snapshot options available.
Without this package enabled, output is limited to a small number of snapshot output fields. Subsequent tutorial experiments will explore the use
of packages which expand the physical and scientific capabilities of MITgcm, e.g., such as physical parameterizations or modeling capabilities
for tracers, ice, etc., that are not compiled unless specified.

.. _baroc_code_size:

File :filelink:`code/SIZE.h <verification/tutorial_baroclinic_gyre/code/SIZE.h>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_baroclinic_gyre/code/SIZE.h

For this second tutorial, we will break the model domain into multiple tiles. Although initially we will
run the model on a single processor, a multi-tiled setup
is required when we demonstrate how to run the model using either
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ or using multiple threads.

The following lines calculate the horizontal size of the global model domain (NOT to be edited).
Our values for :filelink:`SIZE.h <verification/tutorial_baroclinic_gyre/code/SIZE.h>` parameters
below must multiply so that our horizontal model domain is 62\ :math:`\times`\ 62:

 .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/SIZE.h
       :start-at: sNx*nSx
       :end-at: sNy*nSy
       :lineno-match:

Now let's look at all individual :filelink:`SIZE.h <verification/tutorial_baroclinic_gyre/code/SIZE.h>` parameter settings.

- Although our model domain is 62\ :math:`\times`\ 62, here we specify the size of a single tile to be one-half that
  in both :math:`x` and :math:`y`. Thus, the model requires four of these tiles to cover the full ocean sector domain
  (see below, where we set :varlink:`nSx` and :varlink:`nSy`). Note that the grid
  can only be subdivided into tiles in the horizontal dimensions, not in the vertical.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/SIZE.h
       :start-at: sNx =
       :end-at: sNy =
       :lineno-match:

- As in tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`, here we set the overlap extent of a model tile
  to the value 2 in both :math:`x` and :math:`y`. In other words, although our model tiles are sized 31\ :math:`\times`\ 31,
  in MITgcm array storage there are an additional 2 border rows surrounding
  each tile which contain model data from neighboring tiles.
  Some horizontal advection schemes and other parameter and setup choices
  require a larger overlap setting (see :numref:`adv_scheme_summary`).
  In our configuration, we are using a second-order center-differences advection scheme (the MITgcm default)
  which does not requires setting a overlap beyond the MITgcm minimum 2.

  .. literalinclude:: ../../../verification/tutorial_barotropic_gyre/code/SIZE.h
       :start-at: OLx =
       :end-at: OLy =
       :lineno-match:

- These lines set parameters :varlink:`nSx` and :varlink:`nSy`,
  the number of model tiles in the :math:`x` and :math:`y` directions, respectively,
  which execute on a single process. Initially, we will run the model on a single core,
  thus both :varlink:`nSx` and :varlink:`nSy` are set to 2 so that all :math:`2 \times 2 = 4` tiles are integrated forward in time.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/SIZE.h
       :start-at: nSx =
       :end-at: nSy =
       :lineno-match:

- These lines set parameters :varlink:`nPx` and :varlink:`nPy`, the number of processes
  to use in the :math:`x` and :math:`y` directions, respectively.
  As noted, initially we will run using a single process, so for now these parameters are both set to 1.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/SIZE.h
       :start-at: nPx =
       :end-at: nPy =
       :lineno-match:

- Here we tell the model we are using 15 vertical levels.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/SIZE.h
       :start-at: Nr  =
       :end-at: Nr  =
       :lineno-match:

File :filelink:`code/DIAGNOSTICS_SIZE.h <verification/tutorial_baroclinic_gyre/code/DIAGNOSTICS_SIZE.h>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/DIAGNOSTICS_SIZE.h
    :linenos:
    :caption: verification/tutorial_baroclinic_gyre/code/DIAGNOSTICS_SIZE.h

In the default version :filelink:`/pkg/diagnostics/DIAGNOSTICS_SIZE.h` the storage array for diagnostics is purposely
set quite small, in other words forcing the user to assess how many diagnostics will be computed and thus choose an appropriate
size for a storage array. In the above file we have modified the value of parameter :varlink:`numDiags`:

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/DIAGNOSTICS_SIZE.h
       :start-at: numDiags =
       :end-at: numDiags =
       :lineno-match:

from its default value ``1*Nr``, which would only allow a single 3-D diagnostic to be computed and saved, to ``20*Nr``,
which will permit up to some combination of up to 20 3-D diagnostics or 300 2-D diagnostic fields.

Run-time Configuration
~~~~~~~~~~~~~~~~~~~~~~

.. _baroc_input_data:

File :filelink:`input/data <verification/tutorial_baroclinic_gyre/input/data>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
    :linenos:
    :caption: verification/tutorial_baroclinic_gyre/input/data

Parameters for this configuration
are set as follows.

PARM01 - Continuous equation parameters
#######################################

- These lines set parameters :varlink:`viscAh` and :varlink:`viscAr`, the horizontal and vertical Laplacian viscosities respectively,
  to :math:`5000` m\ :sup:`2` s\ :sup:`--1` and :math:`1 \times 10^{-2}` m\ :sup:`2` s\ :sup:`--1`. Note the subscript :math:`r`
  is used for the vertical, reflecting MITgcm's generic :math:`r`-vertical coordinate capability (i.e., the model is capable of
  using either a :math:`z`-coordinate or a :math:`p`-coordinate system).

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: viscAh
       :end-at: viscAr
       :lineno-match:

- These lines set parameters to specify the boundary conditions for momentum on the model domain sidewalls and bottom.
  Parameter :varlink:`no_slip_sides` is set to ``.TRUE.``, i.e., no-slip lateral boundary conditions (the default), which will yield a Munk (1950) :cite:`munk:50` western boundary solution.
  Parameter :varlink:`no_slip_bottom` is set to ``.FALSE.``, i.e., free-slip bottom boundary condition (default is true).
  If instead of a Munk layer we desired a Stommel (1948) :cite:`stommel:48` western boundary layer solution, we would opt for free-slip lateral boundary conditions and no-slip conditions along the bottom.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: slip_sides
       :end-at: slip_bottom
       :lineno-match:

- These lines set parameters :varlink:`diffKhT` and :varlink:`diffKrT`,
  the horizontal and vertical Laplacian temperature diffusivities respectively,
  to :math:`1000` m\ :sup:`2` s\ :sup:`--1` and :math:`1 \times 10^{-5}` m\ :sup:`2` s\ :sup:`--1`.The boundary condition on this
  operator is zero-flux at all boundaries.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: diffKhT
       :end-at: diffKrT
       :lineno-match:

- By default, MITgcm does not apply any parameterization to mix statically unstable columns of water. In a coarse resolution, hydrostatic
  configuration, typically such a parameterization is desired. We recommend a scheme which
  simply applies (presumably, large) vertical diffusivity between statically unstable grid cells in the vertical. This vertical diffusivity
  is set by parameter :varlink:`ivdc_kappa`, which here we set to :math:`1.0` m\ :sup:`2` s\ :sup:`--1`. This scheme requires that
  :varlink:`implicitDiffusion` is set to ``.TRUE.`` (see :numref:`implicit-backward-stepping`;
  more specifically, applying a large vertical diffusivity to represent convective mixing
  requires the use of an implicit
  time-stepping method for vertical diffusion, rather than Adams Bashforth II).
  Alternatively, a traditional convective adjustment scheme is available; this can be activated
  through the :varlink:`cAdjFreq` parameter, see :numref:`ocean_convection_parms`.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: ivdc
       :end-at: implicitDiff
       :lineno-match:

.. _tut_baroc_linear_eos:

- The following parameters tell the model to use a linear equation of state.
  Note a list of :varlink:`Nr` (=15, from :filelink:`SIZE.h <verification/tutorial_baroclinic_gyre/code/SIZE.h>`)
  potential temperature values in :sup:`o`\ C is specified for parameter :varlink:`tRef`, ordered from surface to depth.
  :varlink:`tRef` is used for two purposes here.
  First, anomalies in density are computed using this reference :math:`\theta`, :math:`\theta'(x,y,z) = \theta(x,y,z) - \theta_{\rm ref}(z)`;
  see use in :eq:`rho_lineareos` and :eq:`rhoprime_lineareos`.
  Second, the model will use these reference temperatures for its initial state, as we are not providing a pickup file
  nor specifying an initial temperature hydrographic file (in later tutorials we will demonstrate how to do so).
  For each depth level the initial and reference profiles will be uniform in :math:`x` and :math:`y`.
  Note when checking static stability or computing :math:`N^2`, the density gradient resulting from these specified reference levels
  is added to :math:`\partial \rho' / \partial z` from :eq:`rhoprime_lineareos`.
  Finally, we set the thermal expansion coefficient :math:`\alpha_{\theta}` (:varlink:`tAlpha`)
  as used in :eq:`rho_lineareos` and :eq:`rhoprime_lineareos`, while setting
  the haline contraction coefficient (:varlink:`sBeta`) to zero (see :eq:`rho_lineareos`, which omits a salinity contribution to the
  linear equation of state; like tutorial :ref:`Barotropic Ocean Gyre <sec_eg_baro>`,
  salinity is not included as a tracer in this very idealized model setup).

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: eosType
       :end-at: sBeta
       :lineno-match:

- This line sets parameter :math:`\rho_0` (:varlink:`rhoNil`) to 999.8 kg/m\ :sup:`3`, the surface reference density for our linear equation of state,
  i.e., the density of water at tRef(k=1). This value will also be used
  as :math:`\rho_c` (parameter :varlink:`rhoConst`) in :eq:`baroc_gyre_umom`-:eq:`baroc_gyre_press`,
  lacking a separate explicit assignment of :varlink:`rhoConst` in ``data``.
  Note this value is the model default value for :varlink:`rhoNil`.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: rhoNil
       :end-at: rhoNil
       :lineno-match:

- This line sets parameter :varlink:`gravity`, the acceleration due to gravity :math:`g` in :eq:`baroc_gyre_press`, and this value will also
  be used to set :varlink:`gBaro`, the barotopic (i.e., free surface-related)
  gravity parameter which we set in tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`.
  This is the MITgcm default value.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: gravity
       :end-at: gravity
       :lineno-match:

- These lines set parameters which prescribe the linearized free surface formulation,
  similar to tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`. Note
  we have added parameter :varlink:`exactConserv`, set to ``.TRUE.``: this instructs the model to
  recompute divergence after the pressure solver step, ensuring volume conservation of the free surface solution
  (the model default is NOT to recompute divergence, but given the small numerical cost, we typically recommend doing so).

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: rigidLid
       :end-at: exactConserv
       :lineno-match:

- As in tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`, we
  suppress MITgcm’s forward time integration of salt in the tracer equations.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: saltStepping
       :end-at: saltStepping
       :lineno-match:

PARM02 - Elliptic solver parameters
###################################

These parameters are unchanged from tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`.

.. _baroc_parm03:

PARM03 - Time stepping parameters
#################################

- In tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>` we specified a starting iteration number :varlink:`nIter0`
  and a number of time steps to integrate, :varlink:`nTimeSteps`. Here we opt to use another approach to control run start and duration:
  we set a :varlink:`startTime`  and :varlink:`endTime`, both in units of seconds. Given a starting time of 0.0, the model starts
  from rest using specified initial values of temperature (here, as previously noted, from the :varlink:`tRef` parameter) rather than attempting
  to restart from a saved checkpoint file. The specified value for :varlink:`endTime`, 12000.0 seconds
  is equivalent to 10 time steps, set for testing purposes.
  To integrate over a longer, more physically relevant period of time, uncomment the :varlink:`endTime` and :varlink:`monitorFreq` lines
  located near the end of this parameter block. Note, for simplicity, our units for these time choices assume a 360-day "year"
  and 30-day "month" (although lacking a seasonal cycle in our forcing, defining a "year" is immaterial; we will demonstrate how to apply time-varying
  forcings in later tutorials).

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: startTime
       :end-at: endTime
       :lineno-match:

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: for longer
       :end-at: Freq=2592
       :lineno-match:

- Remaining time stepping parameter choices (specifically, :math:`\Delta t`,
  checkpoint frequency, output frequency, and monitor settings)
  are described in tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`;
  refer to the description :ref:`here <baro_time_stepping_parms>`.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: deltaT
       :end-at: monitorSelect
       :lineno-match:

- The parameter :varlink:`tauThetaClimRelax` sets the time scale, in seconds,
  for restoring potential temperature in the model's top surface layer (see :eq:`baroc_restore_theta`).
  Our choice here of 2,592,000 seconds is equal to 30 days.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: tauTheta
       :end-at: tauTheta
       :lineno-match:

PARM04 - Gridding parameters
############################

- This line sets parameter :varlink:`usingSphericalPolarGrid`, which specifies that the simulation will use spherical polar coordinates
  (and affects the interpretation of other grid coordinate parameters).

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: usingSpherical
       :end-at: usingSpherical
       :lineno-match:

- These lines set the horizontal grid spacing, as vectors :varlink:`delX` and :varlink:`delY`
  (i.e., :math:`\Delta x` and :math:`\Delta y` respectively), with units of degrees
  as dictated by our choice :varlink:`usingSphericalPolarGrid`.
  As before, this syntax indicates that we specify 62 values in both the :math:`x` and :math:`y` directions, which matches the
  global domain size as specified in :filelink:`SIZE.h <verification/tutorial_barotropic_gyre/code/SIZE.h>`.
  Our ocean sector domain starts at :math:`0^\circ` longitude and :math:`15^\circ` N; accounting for a surrounding land
  row of cells, we thus set the origin in longitude to :math:`-1.0^\circ` and in latitude to :math:`14.0^\circ`.
  Again note that our origin specifies the southern and western edges of the gridcell, not the cell center location.
  Setting the origin in latitude is critical given that it affects the Coriolis parameter :math:`f`
  (which appears in :eq:`baroc_gyre_umom` and :eq:`baroc_gyre_vmom`); the default value for :varlink:`ygOrigin` is :math:`0.0^\circ`.
  Note that setting :varlink:`xgOrigin` is optional, given that absolute longitude does not appear in the equation discretization.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: delX
       :end-at: ygOrigin
       :lineno-match:

- This line sets parameter :varlink:`delR`, the vertical grid spacing in the :math:`z`-coordinate (i.e., :math:`\Delta z`),
  to a vector of 15 depths (in meters), from 50 m in the surface layer to a bottom layer depth of 190 m. The sum of these
  specified depths equals 1800 m, the full depth :math:`H` of our idealized ocean sector.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: delR
       :end-at: delR
       :lineno-match:

PARM05 - Input datasets
#######################

- Similar to tutorial :ref:`Barotropic Ocean Gyre <barotropic_gyre_stab_crit>`, these lines specify filenames for bathymetry
  and surface wind stress forcing files.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: bathyFile
       :end-at: zonalWindFile
       :lineno-match:

- This line specifies parameter :varlink:`thetaClimFile`, the filename for the (2-D) restoring temperature field.

  .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data
       :start-at: thetaClimFile
       :end-at: thetaClimFile
       :lineno-match:

File :filelink:`input/data.pkg <verification/tutorial_baroclinic_gyre/input/data.pkg>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data.pkg
    :linenos:
    :caption: verification/tutorial_baroclinic_gyre/input/data.pkg

Here we activate two MITgcm packages that are not included with the model by default:
package :filelink:`mnc <pkg/mnc>` (see :numref:`pkg_mnc`) specifies that model output should be written in `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_  format,
and package :filelink:`diagnostics <pkg/diagnostics>` (see :numref:`sub_outp_pkg_diagnostics`) allows user-selectable diagnostic output.
The boolean parameters set are :varlink:`useMNC` and :varlink:`useDiagnostics`, respectively.
Note these add-on packages also need to be specified when the model is compiled, see :numref:`tut_baroc_code_config`.
Apart from these two additional packages, only standard packages (i.e., those compiled in MITgcm by default) are required for this setup.

.. _baroc_datamnc:

File :filelink:`input/data.mnc <verification/tutorial_baroclinic_gyre/input/data.mnc>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data.mnc
    :linenos:
    :caption: verification/tutorial_baroclinic_gyre/input/data.mnc

This file sets parameters which affect package :filelink:`pkg/mnc` behavior; in fact, with :filelink:`pkg/mnc` enabled, it is required
(many packages look for file ``data.«PACKAGENAME»`` and will terminate if not present).
By setting the parameter :varlink:`monitor_mnc` to ``.FALSE.``
we are specifying NOT to create separate `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
output files for :filelink:`pkg/monitor` output, but rather to include this monitor output in the standard output file
(see :numref:`baro_gyre_build_run`). See :numref:`pkg_mnc_inputs` for a complete
listing of :filelink:`pkg/mnc` namelist parameters and their default settings.

Unlike raw binary output, which overwrites any existing files, when using mnc output the model will create new directories if the parameters
:varlink:`mnc_use_outdir` and :varlink:`mnc_outdir_str` are set, as above; the model will append a 4-digit number to :varlink:`mnc_outdir_str`,
starting at 0001, incrementing as needed if existing directories already exist.
If these parameters are NOT set, the model will terminate with an error if one attempts
to overwrite an existing  ``.nc`` file (in other words, to re-run in an previous run directory,
one must delete all ``*.nc`` files before restarting). Note that our subdirectory name choice ``mnc_test_`` is required
by :ref:`MITgcm automated testing <code_testing_protocols>` protocols, and can be changed to something more mnemonic, if desired.

In general, it is good practice to write diagnostic output into subdirectories, to keep the top run directory
less "cluttered"; some unix file systems do not respond well when very large numbers of files are produced, which can
occur in setups divided into many tiles and/or when many diagnostics are selected for output.

.. _baroc_diags_list:

File :filelink:`input/data.diagnostics <verification/tutorial_baroclinic_gyre/input/data.diagnostics>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data.diagnostics
    :linenos:
    :caption: verification/tutorial_baroclinic_gyre/input/data.diagnostics

.. _baroc_diags_parms:

DIAGNOSTICS_LIST - Diagnostic Package Choices
#############################################

In this section we specify what diagnostics we want to compute, how frequently to compute them, and the name of output files.
Multiple diagnostic fields can be grouped into individual files (i.e., an individual output file here is associated with a 'list' of diagnostics).

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data.diagnostics
    :start-at: fields(1:3,1
    :end-at: frequency(1
    :lineno-match:

The above lines tell MITgcm that our first list will consist of three diagnostic variables:

  - ETAN - the linearized free surface height (m)
  - TRELAX - the heat flux entering the ocean due to surface temperature relaxation (W/m\ :sup:`2`)
  - MXLDEPTH - the depth of the mixed layer (m), as defined here by a given magnitude decrease
    in density from the surface (we'll use the model default for :math:`\Delta  \rho`)

Note that all these diagnostic
fields are 2-D output. **2-D and 3-D diagnostics CANNOT be mixed in a diagnostics list.**
These variables are specified in parameter :varlink:`fields`: the first index is specified as ``1:«NUMBER_OF_DIAGS»``, the second index
designates this for diagnostics list 1. Next, the output filename for diagnostics list 1 is specified  in variable :varlink:`fileName`. Finally,
for this list we specify variable :varlink:`frequency` to provide time-averaged output every 31,104,000 seconds,
i.e., once per year. Had we entered
a negative value for :varlink:`frequency`, MITgcm would have instead written snapshot data at this interval.
Next, we set up a second diagnostics list for several 3-D diagnostics.

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data.diagnostics
    :start-at: fields(1:5,2
    :end-at: frequency(2
    :lineno-match:

The diagnostics in list 2 are:

  - THETA - potential temperature (:sup:`o`\ C )
  - PHYHYD - hydrostatic pressure potential anomaly (m\ :sup:`2`/s\ :sup:`2`)
  - UVEL, VVEL, WVEL - the zonal, meridional, and vertical velocity components respectively (m/s)

Here we did not specify parameter :varlink:`levels`, so all depth levels will be included in the output.
An example of syntax to limit which depths are output is ``levels(1:5,2) = 1.,2.,3.,``, which would dump just the top three levels.
We again specify an output file name via parameter :varlink:`fileName`, and specify a time-average period of one year
through parameter :varlink:`frequency`.

.. _baroc_stat_diags:

DIAG_STATIS_PARMS - Diagnostic Per Level Statistics
###################################################

It is also possible to request output statistics averaged for global mean and by level average (for 3-D diagnostics) over the full domain,
and/or for a pre-defined :math:`(x,y)` region  of the model grid. The statistics computed for each diagnostic are as follows:

  - (area weighted) mean (in both space and time, if time-averaged frequency is selected)
  - (area weighted) standard deviation
  - minimum value
  - maximum value
  - volume of the area used in the calculation (multiplied by the number of time steps if time-averaged).

While these statistics could in theory also be calculated (by the user) from 2-D and 3-D :varlink:`DIAGNOSTICS_LIST` output, the advantage
is that much higher frequency statistical output can be  achieved without filling up copious amounts of disk space.

Options for namelist :varlink:`DIAG_STATIS_PARMS` are set as follows:

.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/data.diagnostics
    :start-at: stat_fields(1
    :end-at: stat_freq
    :lineno-match:

The syntax here is analogous with :varlink:`DIAGNOSTICS_LIST` namelist parameters, except the parameter names begin with ``stat``
(here, :varlink:`stat_fields`, :varlink:`stat_fName`, :varlink:`stat_freq`). Frequency can be set to snapshot or time-averaged output,
and multiple lists of diagnostics (i.e., separate output files) can be specified. The only major difference from
:varlink:`DIAGNOSTICS_LIST` syntax is that 2-D and 3-D diagnostics can be mixed in a list.
As noted, it is possible to select limited horizontal regions of interest, in addition to the full domain calculation.

File :filelink:`input/eedata <verification/tutorial_baroclinic_gyre/input/eedata>`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/eedata
    :linenos:
    :caption: verification/tutorial_baroclinic_gyre/input/eedata

As shown, this file is configured for a single-threaded run, but will be modified later in this tutorial for a multi-threaded setup
(:numref:`baroc_openmp`).

.. _baroc_gyre_bathy_file:

Files ``input/bathy.bin``, ``input/windx_cosy.bin``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The purpose and format of these files is similar to tutorial :ref:`Barotropic Ocean Gyre <baro_gyre_bathy_file>`,
and were generated by matlab script :filelink:`verification/tutorial_baroclinic_gyre/input/gendata.m`
(alternatively, python script :filelink:`gendata.py <verification/tutorial_baroclinic_gyre/input/gendata.py>`).
See :numref:`sec_mitgcm_inp_file_format` for additional information on MITgcm input data file format specifications.

File ``input/SST_relax.bin``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This file specifies a 2-D(:math:`x,y`) map of surface relaxation temperature values,
as generated by :filelink:`verification/tutorial_baroclinic_gyre/input/gendata.m` or
:filelink:`gendata.py <verification/tutorial_baroclinic_gyre/input/gendata.py>`.

.. _building_tutorial_baroc:

Building and running the model
------------------------------

To build and run the model on a single processor, follow the procedure outlined in :numref:`baro_gyre_build_run`.
To run the model for a longer period (i.e., to obtain a reasonable solution; for testing purposes,
by default the model is set to run only a few time steps) uncomment the lines in ``data`` which specify
larger numbers for parameters :varlink:`endTime` and :varlink:`monitorFreq`. This will run the model for 100 years, which
will likely take several hours on a single processor (depending on your computer specs); below we also give instructions for running the model
in parallel either using `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
or multi-threaded (`OpenMP <https://en.wikipedia.org/wiki/OpenMP>`_), which
will cut down run time significantly.

Output Files
~~~~~~~~~~~~

As in tutorial :ref:`sec_eg_baro`, standard output is produced (redirected into
file ``output.txt`` as specified in :numref:`baro_gyre_build_run`); like before, this file
includes model startup information, parameters, etc. (see :numref:`barotropic_gyre_std_out`).
And because we set :varlink:`monitor_mnc` ``=.FALSE.`` in :ref:`data.mnc <baroc_datamnc>`,
our standard output file will include all monitor statistics output. Note monitor statistics and cg2d
information are evaluated over the global domain, despite the bifurcation of the grid into four separate tiles.
As before, the file ``STDERR.0000`` will contain a log of any run-time errors.

With :filelink:`pkg/mnc` compiled and activated in ``data.pkg``, other output is
in `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ format: grid information,
snapshot output specified in ``data``, diagnostics output specified in ``data.diagnostics``
and separate files containing hydrostatic pressure data (see below).
There are two notable differences from standard binary output. Recall that we specified
that the grid was subdivided into four separate tiles (in :ref:`SIZE.h <baroc_code_size>`);
instead of a ``.XXX.YYY.`` file naming scheme for different tiles (as discussed :ref:`here <tut_barotropic_tilenaming>`),
with :filelink:`pkg/nmc` the file names contain ``.t«nnn».`` where «nnn» is the
tile number. Secondly, model data from multiple
time snapshots (or periods) is included in a single file. Although an iteration
number is still part of the file name (here, ``0000000000``),
this is the iteration number at the start of the run (instead of
marking the specific iteration number for the data contained in the file, as the case
for standard binary output). Note that if you dump data frequently, standard binary can produce
huge quantities of separate files, whereas using `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
will greatly reduce the number of files. On the other hand, the
`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ files created can instead become quite large.

To more easily process and plot our results as a single array over the full domain,
we will first reassemble the individual tiles into new `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ format global data files.
To accomplish this, we will make use of utility script :filelink:`utils/python/MITgcmutils/scripts/gluemncbig`.
From the output run (top) directory, type:

.. _baroc_gluemnc_steps:

::

    % ln -s ../../../utils/python/MITgcmutils/scripts/gluemncbig .
    % ./gluemncbig -o grid.nc mnc_test_*/grid.t*.nc
    % ./gluemncbig -o state.nc mnc_test_*/state*.t*.nc
    % ./gluemncbig -o dynDiag.nc mnc_test_*/dynDiag*.t*.nc
    % ./gluemncbig -o surfDiag.nc mnc_test_*/surfDiag*.t*.nc
    % ./gluemncbig -o phiHyd.nc mnc_test_*/phiHyd*.t*.nc
    % ./gluemncbig -o phiHydLow.nc mnc_test_*/phiHydLow*.t*.nc
    % ln -s mnc_test_0001/dynStDiag.0000000000.t001.nc dynStDiag.nc

For help using this utility, type ``gluemncbig --help`` (note, this utility requires python).
The files ``grid.nc``, ``state.nc``, etc. are concatenated from the separate ``t001``, ``t002``, ``t003``, ``t004`` files
into global grid files of horizontal dimension 62\ :math:`\times`\ 62. ``gluemncbig`` is a fairly intelligent script, and by inserting the wildcards
in the path/filename specification, it will grab the most recent run (in case you have started up runs multiple times in this directory,
thus having ``mnc_test_0001``, ``mnc_test_0002``, etc. directories present; see :numref:`baroc_datamnc`).
Note that the last line above is simply making
a link to a file in the ``mnc_test_0001`` output subdirectory; this is the :ref:`statistical-dynamical diagnostics <baroc_stat_diags>`
output, which is already assembled over the global domain (and also note here we are required to be specific which ``mnc_test_`` directory to link from).
For convenience we simply place the link at the top level
of the run directory, where the other assembled ``.nc`` files are saved by ``gluemncbig``.

Let's proceed through the netcdf output that is produced.

  - ``grid.nc`` - includes all the model grid variables used by MITgcm.
    This includes the grid cell center points and separation (:varlink:`XC`, :varlink:`YC`, :varlink:`dxC`, :varlink:`dyC`),
    corner point locations and separation (:varlink:`XG`, :varlink:`YG`, :varlink:`dxG`, :varlink:`dyG`),
    the separation between velocity points (:varlink:`dyU`, :varlink:`dxV`),
    vertical coordinate location and separation (:varlink:`RC`, :varlink:`RF`, :varlink:`drC`, :varlink:`drF`),
    grid cell areas (:varlink:`rA`, :varlink:`rAw`, :varlink:`rAs`, :varlink:`rAz`),
    and bathymetry information (:varlink:`Depth`, :varlink:`HFacC`, :varlink:`HFacW`, :varlink:`HFacS)`.
    See :numref:`spatial_discret_dyn_eq` for definitions and description of the C grid staggering of these variables.
    There are also grid variables in vector form that are not used in the MITgcm source code
    (X, Y, Xp1, Yp1, Z, Zp1, Zu, Zl); see description in  ``grid.nc``. The variables named p1 include an additional data point
    and are dimensioned +1 larger than the standard array size; for example, ``Xp1`` is the longitude of the gridcell left corner, and
    includes an extra data point for the last gridcell's right corner longitude.

  - ``state.nc`` - includes snapshots of state variables U, V, W, Temp, S, and Eta
    at model times T in seconds (variable iter(T) stores the model iteration corresponding with these model times).
    Also included are vector forms of grid variables X, Y, Z, Xp1, Yp1, and Zl.
    As mentioned, in model output-by-tile files, e.g., ``state.0000000000.t001.nc``, the iteration number ``0000000000`` is the parameter :varlink:`nIter0` for the model run
    (recall, we initialized our model with :varlink:`nIter0` =0).
    Snapshots of model state are written for model iterations 0, 25920, 51840, ...
    according to our ``data`` file parameter choice :varlink:`dumpFreq` (:varlink:`dumpFreq`/:varlink:`deltaT` = 25920).

  - ``surfDiag.nc`` - includes output diagnostics as specified from list 1 in
    :ref:`data.diagnostics <baroc_diags_list>`.
    Here we specified that list 1 include 2-D diagnostics ``ETAN``, ``TRELAX``, and ``MXLDEPTH``.
    Also includes an array of model times corresponding to the end of the time-average period, the iteration
    number corresponding to these model times, and vector forms of grid variables which describe these data.
    A Z index is included in the output arrays, even though
    its dimension is one (given that this list contains only 2-D fields).

  - ``dynDiag.nc`` - similar to ``surfDiag.nc`` except this file contains the time-averaged 3-D diagnostics
    we specified in list 2 of :ref:`data.diagnostics <baroc_diags_list>`:
    ``THETA``, ``PHIHYD``, ``UVEL``, ``VVEL``, ``WVEL``.

  - ``dynStDiag.nc`` - includes output statistical-dynamical diagnostics as specified in the ``DIAG_STATIS_PARMS`` section of
    :filelink:`data.diagnostics <verification/tutorial_baroclinic_gyre/input/data.diagnostics>`. Like ``surfDiag.nc`` it also
    includes an array of model times and corresponding iteration numbers for each time-average period end. Output variables are 3-D:
    (time, region, depth). In :filelink:`data.diagnostics <verification/tutorial_baroclinic_gyre/input/data.diagnostics>`,
    we have not defined any additional regions (and by default only global output is produced, "region 1"). Depth-integrated statistics
    are computed (in which case the depth subscript has a range of one element; this is also the case for surface diagnostics such as ``TRELAX``),
    but output is also tabulated at each depth for some variables (i.e., the depth subscript will range from 1 to :varlink:`Nr`).

  - ``phiHyd.nc``, ``phiHydLow.nc`` - these files contain a snapshot 3-D field of hydrostatic
    pressure potential anomaly (:math:`p'/\rho_c`, see :numref:`finding_the_pressure_field`)
    and a snapshot 2-D field of bottom hydrostatic pressure potential anomaly, respectively.
    These are technically not MITgcm state variables, as they are computed `during` the time step
    (normal snapshot state variables are dumped `after` the time step),
    ergo they are not included in file ``state.nc``. Like ``state.nc`` output however
    these fields are written at interval according to
    :varlink:`dumpFreq`, except are not written out at time :varlink:`nIter0` (i.e., have one time
    record fewer than ``state.nc``). Also note when writing standard binary output, these filenames begin as ``PH`` and ``PHL`` respectively.

.. _phi_hyd_discussion:

The hydrostatic pressure potential anomaly :math:`\phi'` is computed as follows:

.. math:: \phi' = \frac{1}{\rho_c} \left( \rho_c g \eta + \int_{z}^{0} (\rho - \rho_0) g dz \right)

following :eq:`rho_lineareos`, :eq:`rhoprime_lineareos` and :eq:`baroc_gyre_press`. Note that with the linear free surface approximation,
the contribution of the free surface position :math:`\eta` to :math:`\phi'` involves the constant density :math:`\rho_c`
and not the density anomaly :math:`\rho'`, in contrast with contributions from below :math:`z=0`.

Several additional files are output in standard binary format. These are:

``RhoRef.data, RhoRef.meta`` - this is a 1-D (k=1...\ :varlink:`Nr`) array of reference density, defined as:

.. math:: \rho_{\rm ref}(k) = \rho_0  \big[ 1 - \alpha_{\theta} (\theta_{\rm ref}(k) - \theta_{\rm ref}(1)) \big]

``PHrefC.data, PHrefC.meta, PHrefF.data, PHrefF.meta`` - these are 1-D (k=1...\ :varlink:`Nr` for PHrefC and
k=1...\ :varlink:`Nr`\ +1 for PHrefF) arrays containing a reference
hydrostatic “pressure potential” :math:`\phi = p/\rho_c` (see :numref:`finding_the_pressure_field`).
Using a linear equation of state, ``PHrefC`` is simply :math:`\frac{\rho_c g |z|}{\rho_c}`,
with output computed at the midpoint of each vertical cell, whereas ``PHrefF``
is computed at the surface and bottom of each vertical cell.
Note that these quantities are not especially useful when using a linear equation of state
(to compute the full hydrostatic pressure potential, one would use ``RhoRef`` and
integrate downward, and add ``phiHyd``, rather than use these fields),
but are of greater utility using a non-linear equation of state.

``pickup.ckptA.001.001.data``, ``pickup.ckptA.001.001.meta``, ``pickup.0000518400.001.001.data``, ``pickup.0000518400.001.001.meta`` etc. - as
described in detail in :ref:`tutorial Barotropic Gyre <barot_describe_checkp>`,
these are temporary and permanent checkpoint files,  output in binary format. Note that separate checkpoint files are written for each model tile.

And finally, because we are using the diagnostics package, upon startup the file ``available_diagnostics.log``
will be generated. This (plain text) file contains a list of all diagnostics available for output in this setup, including a description of each diagnostic and its units,
and the number of levels for which the diagnostic is available (i.e., 2-D or 3-D field).  This list of available diagnostics will change based
on what packages are included in the setup. For example, if your setup includes a seaice package, many seaice diagnostics
will be listed in ``available_diagnostics.log`` that are not available for the :ref:`tutorial Baroclinic Gyre <tutorial_baroclinic_gyre>` setup.

.. _baroc_mpi:

Running with MPI
----------------

In the :filelink:`verification/tutorial_baroclinic_gyre/code` directory
there is a alternate file :filelink:`verification/tutorial_baroclinic_gyre/code/SIZE.h_mpi`.
Overwrite :filelink:`verification/tutorial_baroclinic_gyre/code/SIZE.h` with this file
and re-compile the model from scratch (the most simple approach is to create a new build directory ``build_mpi``;
if instead you wanted to re-compile in your existing build directory, you should
``make CLEAN`` first, which will delete any existing files and dependencies you had created previously):

  ::

     % ../../../tools/genmake2 -mods ../code -mpi -of=«/PATH/TO/OPTFILE»
     % make depend
     % make

Note we have added the option ``-mpi`` to the :filelink:`genmake2 <tools/genmake2>` command that generates the makefile.
A successful build requires MPI libraries installed on your system, and you may need to add to your ``$PATH`` environment variable
and/or set environment variable ``$MPI_INC_DIR`` (for more details, see :numref:`build_mpi`). If there is a problem
finding `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_  libraries, :filelink:`genmake2 <tools/genmake2>` output will complain.

Several lines in :filelink:`verification/tutorial_baroclinic_gyre/code/SIZE.h_mpi` are different from the standard version.
First, we change :varlink:`nSx` and :varlink:`nSy` to 1, so that each process integrates the model for a single tile.

   .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/SIZE.h_mpi
       :start-at: nSx =
       :end-at: nSy =
       :lineno-match:

Next, we we change :varlink:`nPx` and :varlink:`nPy` so that we use two processes in each dimension, for a total of :math:`2*2 = 4` processes.
Effectively, we have subdivided the model grid into four separate tiles, and the model equations are solved in parallel on four separate processes
(presumably, on a unique physical processor or core). Because of the overlap regions
(i.e., gridpoints along the tile edges are duplicated in two or more tiles), and limitations
in the transfer speed of data between processes, the model will not run 4\ :math:`\times` faster, but should be at least 2-3\ :math:`\times` faster than running
on a single process.

   .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/code/SIZE.h_mpi
       :start-at: nPx =
       :end-at: nPy =
       :lineno-match:

Finally, to run the model (from your run directory), using four processes running in parallel:

::

     % mpirun -np 4 ../build_mpi/mitgcmuv

On some systems the `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
run command (and the subsequent command-line option ``-np``) might be something other than ``mpirun``; ask your local system administrator.
When using a large `HPC <https://en.wikipedia.org/wiki/Supercomputer>`_ cluster,
prior steps might be required to allocate four processor cores to your job, and/or it might be necessary to
write this command within a batch scheduler script; again, check with your local system documentation or system administrator.
If four cores are not available when you execute the above ``mpirun`` command, an error will occur.

When running in parallel, :filelink:`pkg/mnc` output will create separate output subdirectories for each
process, assuming option :varlink:`mnc_use_outdir`
is set to ``TRUE`` (here, by specifying ``-np 4`` four directories will be created, one for each
tile --  ``mnc_test_00001`` through ``mnc_test_00004`` -- the first time
the model is run). The (global) statistical-dynamical diagnostics output file will be written in only the first of these directories.
The ``gluemncbig`` steps outlined :ref:`above <baroc_gluemnc_steps>` remain unchanged (if in doubt, one can always
tell ``gluemncbig`` which specific directories to read,
e.g., in bash ``mnc_test_{0009..0012}`` will grab only directories 0009, 0010, 0011, 0012).
Also note it is no longer necessary to redirect standard output
to a file such as ``output.txt``; rather, separate ``STDOUT.xxxx`` and ``STDERR.xxxx``
files are created by each process, where ``xxxx`` is the process number (starting from ``0000``).
Other than some additional `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_-related information,
the standard output content is similar to that from the single-process run.

.. _baroc_openmp:

Running with OpenMP
-------------------

To run multi-threaded (using shared memory, `OpenMP <https://en.wikipedia.org/wiki/OpenMP>`_),
the original :filelink:`SIZE.h <verification/tutorial_baroclinic_gyre/code/SIZE.h>` file is used.
In our example, for compatibility with MITgcm :ref:`testing protocols <code_testing_protocols>`, we will
run using two separate threads, but the user should feel free to experiment using four threads if their local machine contains four cores.
Like the :ref:`previous section <baroc_mpi>` we must first re-compile the executable from scratch,
using a special command line option (for this configuration, ``-omp``). However it is not necessary to specify
how many threads at compile-time (unlike `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_, which requires specific processor count
information to be set in :filelink:`SIZE.h <verification/tutorial_baroclinic_gyre/code/SIZE.h>`).
Create and navigate into a new build directory ``build_openmp`` and type:

  ::

     % ../../../tools/genmake2 -mods ../code -omp -of=«/PATH/TO/OPTFILE»
     % make depend
     % make

In a run directory, overwrite the contents of :filelink:`eedata <verification/tutorial_baroclinic_gyre/input/eedata>` with file
:filelink:`verification/tutorial_baroclinic_gyre/input/eedata.mth`. The parameter :varlink:`nTy` is changed; we now specify to
use two threads across the :math:`y`-domain. Since our model domain is subdivided into four tiles, each thread will now
integrate two tiles in the :math:`x`-domain. Alternatively, to run a multi-threaded example using four threads, both lines should be set to 2.

   .. literalinclude:: ../../../verification/tutorial_baroclinic_gyre/input/eedata.mth
       :start-at: nTx=
       :end-at: nTy=
       :lineno-match:

To run the model, we first need to set two `environment variables <https://en.wikipedia.org/wiki/Environment_variable>`_, before invoking the executable:

  ::

     % export OMP_STACKSIZE=400M
     % export OMP_NUM_THREADS=2
     % ../build_openmp/mitgcmuv >output.txt

Your system's `environment variables <https://en.wikipedia.org/wiki/Environment_variable>`_ may differ from above;
see :numref:`running_openmp` and/or ask your system administrator
(also note, above is `bash shell <https://en.wikipedia.org/wiki/Bash_(Unix_shell)>`_ syntax;
different syntax is required for `C shell <https://en.wikipedia.org/wiki/C_shell>`_).  The important point to note is that
we must tell the operating system environment how many threads will be used, prior to running the executable.
The total number of threads set in ``OMP_NUM_THREADS`` must match :varlink:`nTx` * :varlink:`nTy` as specified in file ``eedata``.
Moreover, the model domain must be subdivided into sufficient number of tiles in :filelink:`SIZE.h <verification/tutorial_baroclinic_gyre/code/SIZE.h>`
through the choices of :varlink:`nSx` and :varlink:`nSy`: the number of tiles (:varlink:`nSx` * :varlink:`nSy`) must be equal to or greater than the number of threads.
More specifically, :varlink:`nSx` must be equal to or an integer multiple of :varlink:`nTx`, and :varlink:`nSy` must be equal to or an integer multiple of :varlink:`nTy`.

Also note that at this time, :filelink:`pkg/mnc` is automatically disabled for multi-threaded setups, so output
is dumped in standard binary format (i.e., using :filelink:`pkg/msdio`). You will receive a gentle warning message if you run
this multi-threaded setup and keep :varlink:`useMNC` set to ``.TRUE.`` in ``data.pkg``.
The full filenames for grid variables (e.g., ``XC``, ``YC``, etc.), snapshot output (e.g., ``Eta``, ``T``, ``PHL``)
and :filelink:`pkg/diagnostics` output (e.g., ``surfDiag``, ``oceStDiag``, etc.) include a suffix
that contains the time iteration number and tile identification (tile 001 includes ``.001.001`` in the filename,
tile 002 ``.002.001``, tile 003 ``.001.002``, and tile 004 ``.002.002``).
Unfortunately there is no analogous script
to :filelink:`utils/python/MITgcmutils/scripts/gluemncbig` to concatenate raw binary files, but it is relatively straightforward
to do so in matlab (reading in files using  :filelink:`utils/matlab/rdmds.m`), or equally simple in python -- or, one could simply set
:varlink:`globalFiles` to ``.TRUE.`` and the model will output global files for you (note, this global option is not available for :filelink:`pkg/mnc` output).
One additional difference between :filelink:`pkg/msdio` and
:filelink:`pkg/mnc` is that :ref:`Diagnostics Per Level Statistics <baroc_stat_diags>` are written in plain text, not binary, with :filelink:`pkg/msdio`.

.. _baroclinic_gyre_solution:

Model solution
--------------

In this section, we will examine details of the model solution,
using monthly and annual mean time average data provided in diagnostics files ``dynStDiag.nc``, ``dynDiag.nc``, and ``surfDiag.nc``.
See companion :filelink:`matlab <verification/tutorial_baroclinic_gyre/analysis/matlab_plots.m>`
or :filelink:`python <verification/tutorial_baroclinic_gyre/analysis/python_plots.py>`
(or :filelink:`python using xarray <verification/tutorial_baroclinic_gyre/analysis/python_xr_plots.py>`)
script which shows the code to read output `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ files
and create figures shown in this section.

Our ocean sector model is forced mechanically by wind stress and thermodynamically
though temperature relaxation at the surface. As such,
we expect our solution to not only exhibit wind-driven gyres in the upper layers,
but also include a deep, overturning circulation. Our focus in this section
will be on the former; this component of the solution equilibrates on a time scale of decades,
more or less, whereas the deep cell depends on a slower, diffusive timescale.
We will begin by examining some of our :ref:`Diagnostics Per Level Statistics <baroc_stat_diags>` output, to assess
how close we are to equilibration at different ocean model levels. Recall we've requested
these statistics to be computed monthly.

Load diagnostics ``TRELAX_ave``, ``THETA_lv_avg``, and ``THETA_lv_std`` from file ``dynStDiag.nc``.
In :numref:`baroclinic_gyre_trelax_timeseries`\a
we plot the global model surface mean heat flux (``TRELAX_ave``) as a function of time.
At the beginning of the run,
we observe that the ocean is cooling dramatically; this is mainly because our ocean surface layer is
initialized to a uniform :math:`30^{\circ}` C (as specified :ref:`here <tut_baroc_linear_eos>`), which results in
very strong relaxation initially in the northern portion of ocean model, where the
restoring temperature is just above :math:`0^{\circ}` C.
(As an aside comment, such large initialization shocks are often best avoided
if possible, as they may cause model instability, which
may necessitate smaller time steps at model onset and/or more realistic initial conditions.)
However, this initial burst of cooling quickly diminishes over the first decade
of integration, as the surface layer approaches temperature values close to the specified profile;
see  :numref:`baroclinic_gyre_trelax_timeseries`\b
where the mean temperature at surface, thermocline, and abyssal depth are plotted as a function of time.
Note that while the total heat flux shows that the global heat content is slowly decreasing,
even after 100 years, the temperature of the deepest water is slowly warming.
In :numref:`baroclinic_gyre_trelax_timeseries`\c we plot standard deviation of temperature
(by level) over time. Given that each level is initialized at uniform temperature,
initially the standard deviation is zero, but should tend to level off at some non-zero
value over time, as the solution at each depth equilibrates.
Not surprisingly, the largest gradients in temperature exist at the surface, whereas in the
abyss the differences in temperature are quite small.
In summary, we conclude that while the surface appears to approach equilibrium rapidly,
even after 100 years there are changes occurring in deep circulation, presumably related
to the meridional overturning circulation.
We leave it as an exercise to the reader to
integrate the solution further and/or examine and calculate the meridional overturning circulation strength over time.

  .. figure:: figs/trelax_theta_timeseries.png
      :width: 100%
      :align: center
      :alt: baroclinic gyre surface heat flux
      :name: baroclinic_gyre_trelax_timeseries

      a\) Surface heat flux due to temperature restoring, negative values indicate heat flux out of the ocean;
      b) and c) potential temperature mean and standard deviation by level, respectively.

Next, let's examine the effect of wind stress on the ocean's upper layers.
Given the orientation of the wind stress and its variation over a full sine wave as shown in :numref:`baroclinic_gyre_config`
(crudely mimicking easterlies in the tropics, mid-latitude westerlies, and polar easterlies),
we anticipate a double-gyre solution, with a subtropical gyre and a subpolar gyre.
Let's begin by examining the free surface solution (load diagnostics ``ETAN`` and ``TRELAX`` from file ``surfDiag.nc``).
In :numref:`baroclinic_gyre_trelax_freesurface` we show contours of free surface height
(``ETAN``; this is what we plotted in our :ref:`barotropic gyre tutorial solution <barotropic_gyre_solution>`)
overlaying a 2-D color plot of ``TRELAX``
(blue is where heat is released from the ocean, red where heat enters the ocean), averaged over year 100.
Note that a subtropical gyre is readily apparent, as suggested by geostropic currents in balance with
the free surface elevation (not shown, but the reader is encouraged to load diagnostics ``UVEL`` and ``VVEL``
and plot the circulation at various levels). Heat is entering the ocean mainly along the southern boundary,
where upwelling of cold water is occurring, but also along the boundary current between :math:`50^{\circ}`\ N and :math:`65^{\circ}`\ N, where we
would expect southward flow (i.e., advecting water that is colder than the local restoring temperature).
Heat is exiting the ocean where the western boundary current transports warm water northward,
before turning eastward into the basin
at :math:`40^{\circ}`\ N, and also weakly throughout the higher latitude bands,
where deeper mixed layers occur (not shown, but variations in mixed layer
depth can be easily visualized by loading diagnostic ``MXLDEPTH``).

 .. figure:: figs/trelax_freesurface.png
      :width: 80%
      :align: center
      :alt: baroclinic gyre free surface and relaxation
      :name: baroclinic_gyre_trelax_freesurface

      Contours of free surface height (m) averaged over year 100; shading is surface heat flux due to
      temperature restoring (W/m\ :sup:`2`), blue indicating cooling.

So what happened to our model solution subpolar gyre? Let's compute depth-integrated velocity :math:`U_{\rm bt}, V_{\rm bt}`
(units: m\ :sup:`2` s\ :sup:`-1`) and use it calculate the barotropic transport streamfunction:

.. math:: U_{\rm bt} = - \frac{\partial \Psi}{\partial y}, \phantom{WW} V_{\rm bt} = \frac{\partial \Psi}{\partial x}

Compute :math:`U_{\rm bt}` by summing the diagnostic ``UVEL`` multiplied by gridcell depth
(``grid.nc`` variable :varlink:`drF`, i.e.,
the separation between gridcell faces in the vertical). Now do a cumulative sum of
:math:`-U_{\rm bt}` times the gridcell spacing the in the :math:`y` direction (you
will need to load ``grid.nc`` variable :varlink:`dyG`, the separation between gridcell faces in :math:`y`).
A plot of the resulting :math:`\Psi` field is shown in :numref:`baroclinic_gyre_psi`.
Note one could also cumulative sum :math:`V_{\rm bt}` times the grid spacing in the :math:`x`-direction and obtain a similar result.

 .. figure:: figs/baroc_psi.png
      :width: 80%
      :align: center
      :alt: baroclinic gyre barotropic streamfunction
      :name: baroclinic_gyre_psi

      Barotropic streamfunction (Sv) as computed over year 100.

When velocities are integrated over depth, the subpolar gyre is readily apparent,
as might be expected given our wind stress forcing profile. The pattern in :numref:`baroclinic_gyre_psi` in fact resembles
the double-gyre free surface solution we observed in :numref:`baro_jet_solution`
from tutorial :ref:`sec_eg_baro`, when our model grid was only a single layer in the vertical.

Is the magnitude of :math:`\Psi`
we obtain in our solution reasonable? To check this, consider the Sverdrup transport:

.. math:: \rho v_{\rm bt} = \hat{\boldsymbol{k}} \cdot \frac{ \nabla  \times \vec{\boldsymbol{\tau}}}{\beta}

If we plug in a typical mid-latitude value for :math:`\beta` (:math:`2 \times 10^{-11}` m\ :sup:`-1` s\ :sup:`-1`)
and note that :math:`\tau` varies by :math:`0.1` Nm\ :sup:`—2` over :math:`15^{\circ}` latitude,
and multiply by the width of our ocean sector, we obtain an estimate of approximately 20 Sv.
This estimate agrees reasonably well with the strength of the circulation in :numref:`baroclinic_gyre_psi`.

Finally, let's examine the model solution potential temperature field averaged over year 100.
Read in diagnostic ``THETA`` from the file ``dynDiag.nc``. :numref:`baroclinic_gyre_thetaplots`\a shows a plan view of temperature
at 220 m depth (vertical level k=4). :numref:`baroclinic_gyre_thetaplots`\b shows a slice in the :math:`xz` plane at :math:`28.5^{\circ}`\ N
(:math:`y`-dimension j=15), through the center of the subtropical gyre.

 .. figure:: figs/baroc_thetaplots.png
      :width: 100%
      :align: center
      :alt: baroclinic gyre plots of theta
      :name: baroclinic_gyre_thetaplots

      Contour plot of potential temperature at year 100 a) at a depth of 220 m and b) through a section at :math:`28.5^{\circ}`\ N. Contour interval is 2K.

The dynamics of the subtropical gyre are governed by
Ventilated Thermocline Theory (see, for example, Pedlosky (1996) :cite:`pedlosky:96` or
Vallis (2017) :cite:`vallis:17`). Note the presence of warm "mode water" on the western side of the basin;
the contours of the warm water in the southern half of the sector crudely align with the free surface
heights we observed in :numref:`baroclinic_gyre_psi`. In :numref:`baroclinic_gyre_thetaplots`\b note
the presence of a thermocline, i.e., the bunching up of the contours
between 200 m and 400 m depth, with weak stratification below the thermocline.
What sets the penetration depth of the subtropical gyre? Following a simple advective scaling argument
(see Vallis (2017) :cite:`vallis:17` or Cushman-Roisin and Beckers (2011) :cite:`cushmanroisin:11`;
this scaling is obtained via thermal wind and the linearized barotropic vorticity equation),
the depth of the thermocline :math:`h` should scale as:

.. math:: h = \left( \frac{w_{\rm Ek} f^2 L_x}{\beta \Delta b} \right) ^2 = \left( \frac{(\tau / L_y) f L_x}{\beta \rho'} \right) ^2

where :math:`w_{\rm Ek}` is a representive value for Ekman pumping, :math:`\Delta b = g \rho' / \rho_0`
is the variation in buoyancy across the gyre,
and :math:`L_x` and :math:`L_y` are length scales in the
:math:`x` and :math:`y` directions, respectively.
Plugging in applicable values at :math:`30^{\circ}`\ N,
we obtain an estimate for :math:`h` of 200 m, which agrees quite well with that observed in :numref:`baroclinic_gyre_thetaplots`\b.

