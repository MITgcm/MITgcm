.. _sec_global_oce_latlon:

Global Ocean Simulation
=======================

  (in directory: :filelink:`verification/tutorial_global_oce_latlon/`)

This example experiment demonstrates using the MITgcm to simulate the
planetary ocean circulation. The simulation is configured with
realistic geography and bathymetry on a
:math:`4^{\circ} \times 4^{\circ}` spherical polar grid. Fifteen levels are used in the
vertical, ranging in thickness from 50 m at the surface to 690 m at depth, giving a
maximum model depth of 5200 m. Different time-steps are
used to accelerate the convergence to equilibrium (see Bryan 1984 :cite:`bryan:84`)
so that, at this resolution, the
configuration can be integrated forward for thousands of years on a
single processor desktop computer.

Overview
--------

The model is forced with climatological wind stress data from Trenberth (1990)
:cite:`trenberth:90` and NCEP surface flux data from Kalnay et al. (1996)
:cite:`kalnay:96`. Climatological data (Levitus and Boyer 1994a,b :cite:`levitus:94a,levitus:94b`)
is used to initialize the model
hydrography. Levitus and Boyer seasonal climatology
data is also used throughout the calculation to provide additional
air-sea fluxes. These fluxes are combined with the NCEP climatological
estimates of surface heat flux, resulting in a mixed boundary condition
of the style described in Haney (1971) :cite:`haney:71`. Altogether, this
yields the following forcing applied in the model surface layer.

.. math::
   {\cal F}_{u} = \frac{\tau_{x}}{\rho_{0} \Delta z_{s}}
   :label: eg-global_forcing-Fu

.. math::
   {\cal F}_{v} = \frac{\tau_{y}}{\rho_{0} \Delta z_{s}}
   :label: eg-global_forcing-Fv

.. math::
   {\cal F}_{\theta} = - \lambda_{\theta} ( \theta - \theta^{\ast} )
    - \frac{1}{C_{p} \rho_{0} \Delta z_{s}}{\cal Q}
   :label: eg-global_forcing-Ftheta

.. math::
   {\cal F}_{s} = - \lambda_{s} ( S - S^{\ast} )
    + \frac{S_{0}}{\Delta z_{s}}({\cal E} - {\cal P} - {\cal R})
   :label: eg-global_forcing-Fs

where :math:`{\cal F}_{u}`, :math:`{\cal F}_{v}`,
:math:`{\cal F}_{\theta}`, :math:`{\cal F}_{s}` are the forcing terms in
the zonal and meridional momentum and in the potential temperature and
salinity equations respectively. The term :math:`\Delta z_{s}`
represents the top ocean layer thickness in meters. It is used in
conjunction with a reference density, :math:`\rho_{0}` (here set to
999.8 kg m\ :sup:`-3`), a reference salinity, :math:`S_{0}`
(here set to 35 ppt), and a specific heat capacity, :math:`C_{p}` (here
set to 4000 J kg\ :sup:`-1` K\ :sup:`-1`), to
convert input dataset values into time tendencies of potential
temperature (with units of :sup:`o`\ C s\ :sup:`-1`),
salinity (with units ppt s\ :sup:`-1`) and velocity (with units
m s\ :sup:`-2`). The externally supplied forcing fields
used in this experiment are :math:`\tau_{x}`, :math:`\tau_{y}`,
:math:`\theta^{\ast}`, :math:`S^{\ast}`, :math:`\cal{Q}` and
:math:`\mathcal{E}-\mathcal{P}-\mathcal{R}`. The wind stress fields (:math:`\tau_x`,
:math:`\tau_y`) have units of N m\ :sup:`-2`. The
temperature forcing fields (:math:`\theta^{\ast}` and :math:`Q`) have
units of :sup:`o`\ C and W m\ :sup:`-2`
respectively. The salinity forcing fields (:math:`S^{\ast}` and
:math:`\cal{E}-\cal{P}-\cal{R}`) have units of ppt and
m s\ :sup:`-1` respectively. The source files and
procedures for ingesting this data into the simulation are described in
the experiment configuration discussion in section
:numref:`sec_eg-global-clim_ocn_examp_exp_config`.

Discrete Numerical Configuration
--------------------------------

The model is configured in hydrostatic form. The domain is discretized
with a uniform grid spacing in latitude and longitude on the sphere
:math:`\Delta \phi=\Delta \lambda=4^{\circ}`, so that there are 90
grid cells in the zonal and 40 in the meridional direction. The
internal model coordinate variables :math:`x` and :math:`y` are
initialized according to

.. math::

   x &= r\cos(\phi), &\Delta x & = r\cos(\Delta \phi)

   y &= r\lambda, &\Delta y &= r\Delta \lambda

Arctic polar regions are not included in this experiment. Meridionally
the model extends from 80\ :sup:`o`\ S to
80\ :sup:`o`\ N. Vertically the model is configured with
fifteen layers with the following thicknesses:

  |    :math:`\Delta z_{1}` = 50 m
  |    :math:`\Delta z_{2}` = 70 m
  |    :math:`\Delta z_{3}` = 100 m
  |    :math:`\Delta z_{4}` = 140 m
  |    :math:`\Delta z_{5}` = 190 m
  |    :math:`\Delta z_{6}` = 240 m
  |    :math:`\Delta z_{7}` = 290 m
  |    :math:`\Delta z_{8}` = 340 m
  |    :math:`\Delta z_{9}` = 390 m
  |    :math:`\Delta z_{10}` = 440 m
  |    :math:`\Delta z_{11}` = 490 m
  |    :math:`\Delta z_{12}` = 540 m
  |    :math:`\Delta z_{13}` = 590 m
  |    :math:`\Delta z_{14}` = 640 m
  |    :math:`\Delta z_{15}` = 690 m

(here the numeric subscript indicates the model level index number,
:math:`{\tt k}`) to give a total depth, :math:`H`, of
-5200 m. The implicit free surface form of the pressure
equation described in Marshall et al. (1997) :cite:`marshall:97a` is employed. A
Laplacian operator, :math:`\nabla^2`, provides viscous dissipation.
Thermal and haline diffusion is also represented by a Laplacian
operator.

Wind-stress forcing is added to the momentum equations in
:eq:`eg-global-model_equations_uv` for both the zonal
flow :math:`u` and the meridional flow :math:`v`, according to
equations :eq:`eg-global_forcing-Fu` and :eq:`eg-global_forcing-Fv`. Thermodynamic
forcing inputs are added to the equations in
:eq:`eg-global-model_equations_ts` for potential
temperature, :math:`\theta`, and salinity, :math:`S`, according to equations
:eq:`eg-global_forcing-Ftheta` and :eq:`eg-global_forcing-Fs`.  This produces a set
of equations solved in this configuration as follows:

.. math::
   :label: eg-global-model_equations_uv

   \frac{Du}{Dt} - fv +
     \frac{1}{\rho}\frac{\partial p'}{\partial x} -
      \nabla _h \cdot (A_{h} \nabla _h u) -
     \frac{\partial}{\partial z}\left(A_{z}\frac{\partial u}{\partial z}\right)
   &=
   \begin{cases}
     \mathcal{F}_u & \text{(surface)} \\
     0 & \text{(interior)}
   \end{cases}
   \\
   \frac{Dv}{Dt} + fu +
     \frac{1}{\rho}\frac{\partial p'}{\partial y} -
      \nabla _h \cdot (A_{h} \nabla _h v) -
     \frac{\partial}{\partial z}\left(A_{z}\frac{\partial v}{\partial z}\right)
   &=
   \begin{cases}
     \mathcal{F}_v & \text{(surface)} \\
     0 & \text{(interior)}
   \end{cases}

.. math::
      \frac{\partial \eta}{\partial t} +  \nabla _h \cdot \vec{\bf u} = 0

.. math::
   :label: eg-global-model_equations_ts

   \frac{D\theta}{Dt} -
     \nabla _h \cdot (K_{h} \nabla _h \theta)
    - \frac{\partial}{\partial z}\left(\Gamma(K_{z})\frac{\partial\theta}{\partial z}\right)
   &=
   \begin{cases}
   {\cal F}_\theta & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}
   \\
   \frac{D S}{Dt} -
     \nabla _h \cdot (K_{h} \nabla _h S)
    - \frac{\partial}{\partial z}\left(\Gamma(K_{z})\frac{\partial S}{\partial z}\right)
   &=
   \begin{cases}
   {\cal F}_S & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}
   \\

.. math::
   g\rho_{0} \eta + \int^{0}_{-z}\rho' dz = p'

where :math:`u=\frac{Dx}{Dt}=r \cos(\phi)\frac{D \lambda}{Dt}` and
:math:`v=\frac{Dy}{Dt}=r \frac{D \phi}{Dt}` are the zonal and
meridional components of the flow vector, :math:`\vec{\bf u}`, on the
sphere. As described in :numref:`discret_algorithm`, the time evolution of
potential temperature :math:`\theta` equation is solved
prognostically. The total pressure :math:`p` is diagnosed by summing
pressure due to surface elevation :math:`\eta` and the hydrostatic
pressure.

Numerical Stability Criteria
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Laplacian dissipation coefficient, :math:`A_{h}`, is set to
:math:`5 \times 10^5` m s\ :sup:`-1`. This value is chosen to yield a Munk
layer width (see Adcroft 1995 :cite:`adcroft:95`),

.. math::
   M = \pi ( \frac { A_{h} }{ \beta } )^{\frac{1}{3}}
   :label: eq:eg-global-munk_layer

of ~600 km. This is greater than
the model resolution in low-latitudes,
:math:`\Delta x \approx` 400 km, ensuring that the frictional
boundary layer is adequately resolved.

The model is stepped forward with a time step
:math:`\Delta
t_{\theta}` = 24 hours for thermodynamic variables and
:math:`\Delta t_{v}` = 30 minutes for momentum terms. With this time step, the
stability parameter to the horizontal Laplacian friction
(Adcroft 1995 :cite:`adcroft:95`)

.. math::
   S_{\rm Lh} = 4 \frac{A_{h} \Delta t_{v}}{{\Delta x}^2}
   :label: eq:eg-global-laplacian_stability

evaluates to 0.6 at a latitude of
:math:`\phi` = 80\ :sup:`o`, which is above the 0.3 upper limit for
stability, but the zonal grid spacing :math:`\Delta x` is smallest at
:math:`\phi` = 80\ :sup:`o` where :math:`\Delta
x=r\cos(\phi)\Delta \phi\approx` 77 km and the stability criterion
is already met one grid cell equatorwards (at :math:`\phi` = 76\ :sup:`o`).

The vertical dissipation coefficient,
:math:`A_{z}`, is set to :math:`1\times10^{-3}` m\ :sup:`2` s\ :sup:`-1`.
The associated stability limit

.. math::
   S_{\rm Lv} = 4 \frac{A_{z} \Delta t_{v}}{{\Delta z}^2}
   :label: eg-global-laplacian_stability_z

evaluates to 0.0029 for the smallest
model level spacing (:math:`\Delta z_{1}` = 50 m) which is well
below the upper stability limit.

The numerical stability for inertial
oscillations (Adcroft 1995 :cite:`adcroft:95`)

.. math::
   S_{\rm inert} = f^{2} {\Delta t_v}^2
   :label: eg-global-inertial_stability

evaluates to 0.07 for
:math:`f=2\omega\sin(80^{\circ})=1.43\times10^{-4}` s\ :sup:`-1`,
which is below the :math:`S_{i} < 1` upper limit for stability.

The advective CFL (Adcroft 1995 :cite:`adcroft:95`)
for a extreme maximum horizontal flow
speed of :math:`| \vec{\bf u} |` = 2 m s\ :sup:`-1`

.. math::
   S_{\rm adv} = \frac{| \vec{\bf u} | \Delta t_{v}}{ \Delta x}
   :label: eg-global-cfl_stability

evaluates to :math:`5 \times 10^{-2}`. This is
well below the stability limit of 0.5.

The stability parameter for internal gravity
waves propagating with a maximum speed of
:math:`c_{g}` = 10 m s\ :sup:`-1` (Adcroft 1995 :cite:`adcroft:95`)

.. math::
   S_{c} = \frac{c_{g} \Delta t_{v}}{ \Delta x}
   :label: eg-global-gfl_stability

evaluates to :math:`2.3 \times 10^{-1}`. This is
close to the linear stability limit of 0.5.

.. _sec_eg-global-clim_ocn_examp_exp_config:

Experiment Configuration
------------------------

The experiment files

-  :filelink:`verification/tutorial_global_oce_latlon/input/data`

-  :filelink:`verification/tutorial_global_oce_latlon/input/data.pkg`

-  :filelink:`verification/tutorial_global_oce_latlon/input/eedata`

-  ``verification/tutorial_global_oce_latlon/input/trenberth_taux.bin``

-  ``verification/tutorial_global_oce_latlon/input/trenberth_tauy.bin``

-  ``verification/tutorial_global_oce_latlon/input/lev_s.bin``

-  ``verification/tutorial_global_oce_latlon/input/lev_t.bin``

-  ``verification/tutorial_global_oce_latlon/input/lev_sss.bin``

-  ``verification/tutorial_global_oce_latlon/input/lev_sst.bin``

-  ``verification/tutorial_global_oce_latlon/input/bathymetry.bin``

-  :filelink:`verification/tutorial_global_oce_latlon/code/SIZE.h`

contain the code customizations and parameter settings for these
experiments. Below we describe the customizations to these files
associated with this experiment.

Driving Datasets
~~~~~~~~~~~~~~~~

:numref:`fig_sim_config_tclim`-:numref:`fig_sim_config_emp`
show the relaxation temperature (:math:`\theta^{\ast}`) and salinity
(:math:`S^{\ast}`) fields, the wind stress components (:math:`\tau_x`
and :math:`\tau_y`), the heat flux (:math:`Q`) and the net fresh water
flux (:math:`{\cal E} - {\cal P} - {\cal R}`) used in equations
:eq:`eg-global_forcing-Fu`-:eq:`eg-global_forcing-Fs`.
The figures also indicate the lateral extent and coastline used in the
experiment. Figure (*— missing figure —* ) shows the depth contours of
the model domain.

  .. figure:: figs/sst.png
       :width: 94%
       :align: center
       :alt: restoring sst field
       :name: fig_sim_config_tclim

       Annual mean of relaxation temperature (:sup:`o`\ C)

  .. figure:: figs/sss.png
       :width: 90%
       :align: center
       :alt: restoring sss field
       :name: fig_sim_config_sclim

       Annual mean of relaxation salinity (g/kg)

  .. figure:: figs/tx.png
       :width: 90%
       :align: center
       :alt: forcing tau_x field
       :name: fig_sim_config_taux

       Annual mean of zonal wind stress component (N m\ :sup:`-2`)

  .. figure:: figs/ty.png
       :width: 90%
       :align: center
       :alt: forcing tau_y field
       :name: fig_sim_config_tauy

       Annual mean of meridional wind stress component (N m\ :sup:`-2`)

  .. figure:: ../global_oce_in_p/figs/qnet.png
       :width: 90%
       :align: center
       :alt: forcing qnet field
       :name: fig_sim_config_qnet

       Annual mean heat flux (W m\ :sup:`-2`)

  .. figure:: ../global_oce_in_p/figs/emp.png
       :width: 90%
       :align: center
       :alt: forcing emp field
       :name: fig_sim_config_emp

       Annual mean freshwater flux (Evaporation-Precipitation) (m s\ :sup:`-1`)

File :filelink:`input/data <verification/tutorial_global_oce_latlon/input/data>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_global_oce_latlon/input/data
    :linenos:
    :caption: verification/tutorial_global_oce_latlon/input/data

This file specifies the main parameters
for the experiment. The parameters that are significant for this
configuration are

-  Lines 7-8,

   ::

       tRef= 15*20.,
       sRef= 15*35.,

   set reference values for potential temperature and salinity at each
   model level in units of :sup:`o`\ C and
   ppt. The entries are ordered from surface to depth.
   Density is calculated from anomalies at each level evaluated with
   respect to the reference values set here.

-  Line 9,

   ::

       viscAr=1.E-3,

   this line sets the vertical Laplacian dissipation coefficient to
   :math:`1 \times 10^{-3}` m\ :sup:`2` s\ :sup:`-1`. Boundary conditions for
   this operator are specified later.

-  Line 10,

   ::

       viscAh=5.E5,

   this line sets the horizontal Laplacian frictional dissipation
   coefficient to :math:`5 \times 10^{5}` m\ :sup:`2` s\ :sup:`-1`. Boundary
   conditions for this operator are specified later.

-  Lines 11, 13,

   ::

       diffKhT=0.,
       diffKhS=0.,

   set the horizontal diffusion coefficient for temperature and salinity
   to 0, since :filelink:`pkg/gmredi` is used.

-  Lines 12, 14,

   ::

       diffKrT=3.E-5,
       diffKrS=3.E-5,

   set the vertical diffusion coefficient for temperature and salinity
   to :math:`3 \times 10^{-5}`  m\ :sup:`2` s\ :sup:`-1`. The boundary
   condition on this operator is :math:`\frac{\partial}{\partial z}=0`
   at both the upper and lower boundaries.

-  Lines 15-17,

   ::

       rhoConst=1035.,
       rhoConstFresh=1000.,
       eosType = 'JMD95Z',

   set the reference densities for sea water and fresh water, and
   selects the equation of state (Jackett and McDougall 1995 :cite:`jackett:95`)

-  Lines 18-19,

   ::

        ivdc_kappa=100.,
        implicitDiffusion=.TRUE.,

   specify an “implicit diffusion” scheme with increased vertical
   diffusivity of 100 m\ :sup:`2`/s in case of instable
   stratification.

-  Line 28,

   ::

       readBinaryPrec=32,

   Sets format for reading binary input datasets containing model fields
   to use 32-bit representation for floating-point numbers.

-  Line 33,

   ::

       cg2dMaxIters=500,

   Sets maximum number of iterations the two-dimensional, conjugate
   gradient solver will use, **irrespective of convergence criteria
   being met**.

-  Line 34,

   ::

       cg2dTargetResidual=1.E-13,

   Sets the tolerance which the 2-D conjugate gradient
   solver will use to test for convergence in
   :eq:`elliptic-backward-free-surface` to :math:`1 \times 10^{-13}`.
   Solver will iterate until tolerance falls below this value or until
   the maximum number of solver iterations is reached.

-  Line 39,

   ::

       nIter0=0,

   Sets the starting time for the model internal time counter. When set
   to non-zero this option implicitly requests a checkpoint file be read
   for initial state. By default the checkpoint file is named according
   to the integer number of time step value :varlink:`nIter0`. The internal
   time counter works in seconds. Alternatively, :varlink:`startTime` can be
   set.

-  Line 40,

   ::

       nTimeSteps=20,

   Sets the time step number at which this simulation will terminate. At
   the end of a simulation a checkpoint file is automatically written so
   that a numerical experiment can consist of multiple stages.
   Alternatively :varlink:`endTime` can be set.

-  Line 44,

   ::

       deltaTmom=1800.,

   Sets the timestep :math:`\Delta t_{v}` used in the momentum equations
   to 30 minutes. See :numref:`time_stepping`.

-  Line 45,

   ::

       tauCD=321428.,

   Sets the D-grid to C-grid coupling time scale :math:`\tau_{CD}` used
   in the momentum equations.

-  Lines 46-48,

   ::

       deltaTtracer=86400.,
       deltaTClock = 86400.,
       deltaTfreesurf= 86400.,

   Sets the default timestep, :math:`\Delta t_{\theta}`, for tracer
   equations and implicit free surface equations to
   24 hours. See :numref:`time_stepping`.

-  Line 76,

   ::

       bathyFile='bathymetry.bin'

   This line specifies the name of the file from which the domain
   bathymetry is read. This file is a 2-D (:math:`x,y`) map
   of depths. This file is assumed to contain 32-bit binary numbers
   giving the depth of the model at each grid cell, ordered with the :math:`x`
   coordinate varying fastest. The points are ordered from low
   coordinate to high coordinate for both axes. The units and
   orientation of the depths in this file are the same as used in the
   MITgcm code. In this experiment, a depth of 0 m indicates a
   solid wall and a depth of <0 m indicates open ocean.

-  Lines 79-80,

   ::

       zonalWindFile='trenberth_taux.bin'
       meridWindFile='trenberth_tauy.bin'

   These lines specify the names of the files from which the :math:`x`- and :math:`y`-
   direction surface wind stress is read. These files are also
   3-D (:math:`x,y,time`) maps and are enumerated and
   formatted in the same manner as the bathymetry file.

Other lines in the file :filelink:`input/data <verification/tutorial_global_oce_latlon/input/data>`
are standard values that are described in the :numref:`customize_model`.

File :filelink:`input/data.pkg <verification/tutorial_global_oce_latlon/input/data.pkg>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File :filelink:`input/eedata <verification/tutorial_global_oce_latlon/input/eedata>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

Files ``input/trenberth_taux.bin`` and ``input/trenberth_tauy.bin``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``input/trenberth_taux.bin`` and ``input/trenberth_tauy.bin`` files
specify 3-D (:math:`x,y,time`) maps of wind stress
:math:`(\tau_{x},\tau_{y})`, based on values from Treberth et al. (1990) :cite:`trenberth:90`.
The units are N m\ :sup:`-2`.

File ``input/bathymetry.bin``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``input/bathymetry.bin`` file specifies a 2-D
(:math:`x,y`) map of depth values. For this experiment values range
between 0 and -5200 m, and have been derived
from `ETOPO5 <https://www.ngdc.noaa.gov/mgg/global/etopo5.HTML>`_. The file contains a raw binary stream of data that is
enumerated in the same way as standard MITgcm 2-D horizontal arrays.

.. _tut_global_oce_latlon_code_size:

File :filelink:`code/SIZE.h <verification/tutorial_global_oce_latlon/code/SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_global_oce_latlon/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_global_oce_latlon/code/SIZE.h

Four lines are customized in this file for the current experiment

-  Line 45,

   ::

       sNx=45,

   this line sets the number of grid points of each tile (or sub-domain)
   along the :math:`x`-coordinate axis.

-  Line 46,

   ::

       sNy=40,

   this line sets the number of grid points of each tile (or sub-domain)
   along the :math:`y`-coordinate axis.

-  Lines 49,51,

   ::

       nSx=2,
       nPx=1,

   these lines set, respectively, the number of tiles per process and the number of processes
   along the :math:`x`-coordinate axis. Therefore,
   the total number of grid points along the :math:`x`-coordinate axis
   corresponding to the full domain extent is :math:`Nx=sNx*nSx*nPx=90`.

-  Line 55,

   ::

       Nr=15

   this line sets the vertical domain extent in grid points.

