.. _sec_global_oce_in_p_coor:

Global Ocean Simulation in Pressure Coordinates
===============================================

  (in directory: :filelink:`verification/tutorial_global_oce_in_p/`)

This example experiment demonstrates using MITgcm to simulate the
planetary ocean circulation in pressure coordinates, that is, without
making the Boussinesq approximations. The simulation is configured as a near
copy of tutorial_global_oce_latlon
(:numref:`sec_global_oce_latlon`). with realistic geography and
bathymetry on a :math:`4^{\circ} \times
4^{\circ}` spherical polar grid. Fifteen levels are used in the
vertical, ranging in thickness from
50.4089 dbar :math:`\approx` 50 m at the surface to
710.33 dbar :math:`\approx` 690 m at depth, giving a
maximum model depth of
5302.3122 dbar :math:`\approx` 5200 m. At this
resolution, the configuration can be integrated forward for thousands of
years on a single processor desktop computer.

Overview
--------

The model is forced with climatological wind stress data from
Trenberth (1990) :cite:`trenberth:90` and surface flux data from Jiang et al. (1999)
:cite:`jiang:99`. Climatological data (Levitus and Boyer 1994a,b :cite:`levitus:94a,levitus:94b`)
is used to initialize the model
hydrography. Levitus and Boyer seasonal climatology
data is also used throughout the calculation to provide additional
air-sea fluxes. These fluxes are combined with the Jiang et al. climatological
estimates of surface heat flux, resulting in a mixed boundary condition
of the style described in Haney (1971) :cite:`haney:71`. Altogether, this
yields the following forcing applied in the model surface layer.

.. math::
   {\cal F}_{u} = g\frac{\tau_{x}}{\Delta p_{s}}
  :label: eg-global_forcing_fu_pcoord

.. math::
   {\cal F}_{v} = g\frac{\tau_{y}}{\Delta p_{s}}
   :label: eg-global_forcing_fv_pcoord

.. math::
   {\cal F}_{\theta} = - g\lambda_{\theta} ( \theta - \theta^{\ast} )
    - \frac{1}{C_{p} \Delta p_{s}}{\cal Q}
   :label: eg-global_forcing_ft_pcoord

.. math::
   {\cal F}_{s} =
    + g\rho_{FW}\frac{S}{\rho\Delta p_{s}}({\cal E} - {\cal P} - {\cal R})
   :label: eg-global_forcing_fs_pcoord

where :math:`{\cal F}_{u}`, :math:`{\cal F}_{v}`,
:math:`{\cal F}_{\theta}`, :math:`{\cal F}_{s}` are the forcing terms in
the zonal and meridional momentum and in the potential temperature and
salinity equations respectively. The term :math:`\Delta p_{s}`
represents the top ocean layer thickness in Pa. It is used in
conjunction with a reference density, :math:`\rho_{FW}` (here set to
999.8 kg m\ :sup:`-3`), the surface salinity, :math:`S`, and a
specific heat capacity, :math:`C_{p}` (here set to
4000 J kg\ :sup:`-1` K\ :sup:`-1`), to convert
input dataset values into time tendencies of potential temperature (with
units of :sup:`o`\ C s\ :sup:`-1`), salinity (with units
ppt s\ :sup:`-1`) and velocity (with units m s\ :sup:`-2`).
The externally supplied forcing fields used in this experiment are
:math:`\tau_{x}`, :math:`\tau_{y}`, :math:`\theta^{\ast}`,
:math:`\cal{Q}` and :math:`\cal{E}-\cal{P}-\cal{R}`. The wind stress
fields (:math:`\tau_x`, :math:`\tau_y`) have units of
N m\ :sup:`-2`. The temperature forcing fields
(:math:`\theta^{\ast}` and :math:`Q`) have units of
:sup:`o`\ C and W m\ :sup:`-2` respectively.
The salinity forcing fields (:math:`\cal{E}-\cal{P}-\cal{R}`) has units of
m s\ :sup:`-1` respectively. The source files and
procedures for ingesting these data into the simulation are described in
the experiment configuration discussion in section
:numref:`sec_eg-global-clim_ocn_examp_exp_config`.

Discrete Numerical Configuration
--------------------------------

Due to the pressure coordinate, the model can only be hydrostatic (de Szoeke and Samelson 2002
:cite:`deszoeke:02`). The domain is discretized with a uniform
grid spacing in latitude and longitude on the sphere
:math:`\Delta \phi=\Delta
\lambda=4^{\circ}`, so that there are 90 grid cells in the zonal and
40 in the meridional direction. The internal model coordinate
variables :math:`x` and :math:`y` are initialized according to

.. math::

   \begin{aligned}
   x=r\cos(\phi),~\Delta x & = r\cos(\Delta \phi) \\
   y=r\lambda,~\Delta y & = r\Delta \lambda \end{aligned}

Arctic polar regions are not included in this experiment. Meridionally
the model extends from 80\ :sup:`o`\ S to
80\ :sup:`o`\ N. Vertically the model is configured with
fifteen layers with the following thicknesses

  |    :math:`\Delta p_{1}` = 7103300.720021 Pa
  |    :math:`\Delta p_{2}` = 6570548.440790 Pa
  |    :math:`\Delta p_{3}` = 6041670.010249 Pa
  |    :math:`\Delta p_{4}` = 5516436.666057 Pa
  |    :math:`\Delta p_{5}` = 4994602.034410 Pa
  |    :math:`\Delta p_{6}` = 4475903.435290 Pa
  |    :math:`\Delta p_{7}` = 3960063.245801 Pa
  |    :math:`\Delta p_{8}` = 3446790.312651 Pa
  |    :math:`\Delta p_{9}` = 2935781.405664 Pa
  |    :math:`\Delta p_{10}` = 2426722.705046 Pa
  |    :math:`\Delta p_{11}` = 1919291.315988 Pa
  |    :math:`\Delta p_{12}` = 1413156.804970 Pa
  |    :math:`\Delta p_{13}` = 1008846.750166 Pa
  |    :math:`\Delta p_{14}` = 705919.025481 Pa
  |    :math:`\Delta p_{15}` = 504089.693499 Pa

(here the numeric subscript indicates the model level index number,
:math:`{\tt k}`; note that the surface layer has the highest index
number 15) to give a total depth, :math:`H`, of -5200 m. In
pressure, this is :math:`p_{b}^{0}` = 53023122.566084 Pa. The
implicit free surface form of the pressure equation described in
Marshall et al. (1997) :cite:`marshall:97a` with the nonlinear extension by Campin et al. (2004)
:cite:`cam:04` is employed. A Laplacian operator,
:math:`\nabla^2`, provides viscous dissipation. Thermal and haline
diffusion is also represented by a Laplacian operator.

Wind-stress forcing is added to the momentum equations in
:eq:`eg-global-model_equations_pcoord_uv` for both the
zonal flow, :math:`u` and the meridional flow :math:`v`, according to
equations :eq:`eg-global_forcing_fu_pcoord` and :eq:`eg-global_forcing_fv_pcoord`. Thermodynamic
forcing inputs are added to the equations in
:eq:`eg-global-model_equations_pcoord_ts` for potential
temperature, :math:`\theta`, and salinity, :math:`S`, according to
equations :eq:`eg-global_forcing_ft_pcoord` and :eq:`eg-global_forcing_fs_pcoord`. This produces a set
of equations solved in this configuration as follows:

.. math::
   :label: eg-global-model_equations_pcoord_uv

   \frac{Du}{Dt} - fv +
     \frac{1}{\rho}\frac{\partial \Phi^\prime}{\partial x} -
      \nabla _h \cdot ( A_{h} \nabla _h u )-
     (g\rho_0)^2\frac{\partial}{\partial p}\left( A_{r}\frac{\partial u}{\partial p}\right)
    &=
   \begin{cases}
   {\cal F}_u & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}
   \\
   \frac{Dv}{Dt} + fu +
     \frac{1}{\rho}\frac{\partial \Phi^\prime}{\partial y} -
      \nabla _h \cdot ( A_{h} \nabla _h v) -
     (g\rho_0)^2\frac{\partial}{\partial p}\left( A_{r}\frac{\partial v}{\partial p}\right)
   &=
   \begin{cases}
   {\cal F}_v & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}

.. math::
  \frac{\partial p_{b}}{\partial t} +  \nabla _h \cdot \vec{\bf u} = 0

.. math::
   :label: eg-global-model_equations_pcoord_ts

   \frac{D\theta}{Dt} -
     \nabla _h \cdot (K_{h} \nabla _h \theta)
    - (g\rho_0)^2\frac{\partial}{\partial p}\left( \Gamma(K_{r})\frac{\partial\theta}{\partial p}\right)
   &=
   \begin{cases}
   {\cal F}_\theta & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}
   \\
   \frac{D S}{Dt} -
     \nabla _h \cdot (K_{h} \nabla _h S)
    - (g\rho_0)^2\frac{\partial}{\partial p}\left( \Gamma(K_{r})\frac{\partial S}{\partial p}\right)
   &=
   \begin{cases}
   {\cal F}_S & \text{(surface)} \\
   0 & \text{(interior)}
   \end{cases}

.. math::
   \Phi_{-H}'^{(0)} + \alpha_{0}p_{b}+ \int^{p}_{0}\alpha' dp = \Phi'

where :math:`u=\frac{Dx}{Dt}=r \cos(\phi)\frac{D \lambda}{Dt}` and
:math:`v=\frac{Dy}{Dt}=r \frac{D \phi}{Dt}` are the zonal and meridional
components of the flow vector, :math:`\vec{\bf u}`, on the sphere. As
described in :numref:`discret_algorithm`, the time evolution of potential
temperature :math:`\theta` equation is solved prognostically. The full
geopotential height :math:`\Phi` is diagnosed by summing the
geopotential height anomalies :math:`\Phi'` due to bottom pressure
:math:`p_{b}` and density variations. The integration of the hydrostatic
equation is started at the bottom of the domain. The condition of
:math:`p=0` at the sea surface requires a time-independent integration
constant for the height anomaly due to density variations
:math:`\Phi_{-H}'^{(0)}`, which is provided as an input field.

.. _sec_eg-globalpressure-config:

Experiment Configuration
------------------------

The experiment files

-  :filelink:`verification/tutorial_global_oce_in_p/input/data`

-  :filelink:`verification/tutorial_global_oce_in_p/input/data.pkg`

-  :filelink:`verification/tutorial_global_oce_in_p/input/eedata`

-  ``verification/tutorial_global_oce_in_p/input/topog.bin``

-  ``verification/tutorial_global_oce_in_p/input/deltageopotjmd95.bin``

-  ``verification/tutorial_global_oce_in_p/input/lev_s.bin``

-  ``verification/tutorial_global_oce_in_p/input/lev_t.bin``

-  ``verification/tutorial_global_oce_in_p/input/trenberth_taux.bin``

-  ``verification/tutorial_global_oce_in_p/input/trenberth_tauy.bin``

-  ``verification/tutorial_global_oce_in_p/input/lev_sst.bin``

-  ``verification/tutorial_global_oce_in_p/input/shi_qnet.bin``

-  ``verification/tutorial_global_oce_in_p/input/shi_empmr.bin``

-  :filelink:`verification/tutorial_global_oce_in_p/code/CPP_OPTIONS.h`

-  :filelink:`verification/tutorial_global_oce_in_p/code/SIZE.h`

contain the code customizations and parameter settings for these
experiments. Below we describe the customizations to these files
associated with this experiment.

Driving Datasets
~~~~~~~~~~~~~~~~

:numref:`fig_sim_config_tclim_pcoord`-:numref:`fig_sim_config_empmr_pcoord`
show the relaxation temperature (:math:`\theta^{\ast}`) and salinity
(:math:`S^{\ast}`) fields, the wind stress components (:math:`\tau_x`
and :math:`\tau_y`), the heat flux (:math:`Q`) and the net fresh water
flux (:math:`{\cal E} - {\cal P} - {\cal R}`) used in equations
:eq:`eg-global_forcing_fu_pcoord` - :eq:`eg-global_forcing_fs_pcoord`.
The figures also indicate the lateral extent and coastline used in the
experiment. :numref:`fig_model_bathymetry_pcoord`
shows the depth contours of the model domain.

  .. figure:: ../global_oce_latlon/figs/sst.png
       :width: 90%
       :align: center
       :alt: restoring sst field
       :name: fig_sim_config_tclim_pcoord

       Annual mean of relaxation temperature (:sup:`o`\ C)

  .. figure:: ../global_oce_latlon/figs/sss.png
       :width: 90%
       :align: center
       :alt: restoring sss field
       :name: fig_sim_config_sclim_pcoord

       Annual mean of relaxation salinity (g/kg)

  .. figure:: ../global_oce_latlon/figs/tx.png
       :width: 90%
       :align: center
       :alt: forcing tau_x field
       :name: fig_sim_config_taux_pcoord

       Annual mean of zonal wind stress component (N m\ :sup:`-2`)

  .. figure:: ../global_oce_latlon/figs/ty.png
       :width: 90%
       :align: center
       :alt: forcing tau_y field
       :name: fig_sim_config_tauy_pcoord

       Annual mean of meridional wind stress component (N m\ :sup:`-2`)

  .. figure:: figs/qnet.png
       :width: 90%
       :align: center
       :alt: forcing qnet field
       :name: fig_sim_config_qnet_pcoord

       Annual mean heat flux (W m\ :sup:`-2`)

  .. figure:: figs/emp.png
       :width: 90%
       :align: center
       :alt: forcing emp field
       :name: fig_sim_config_empmr_pcoord

       Annual mean freshwater flux (Evaporation-Precipitation) (m s\ :sup:`-1`)

  .. figure:: figs/pb0.png
       :width: 90%
       :align: center
       :alt: model bathymetry in pressure
       :name: fig_model_bathymetry_pcoord

       Model bathymetry in pressure units (Pa)

File :filelink:`input/data <verification/tutorial_global_oce_in_p/input/data>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_global_oce_in_p/input/data
    :linenos:
    :caption: verification/tutorial_global_oce_oce_in_p/input/data

This file specifies the main parameters
for the experiment. The parameters that are significant for this
configuration are

-  Line 9–10,

   ::

       viscAh=3.E5,
       no_slip_sides=.TRUE.

   these lines set the horizontal Laplacian frictional dissipation
   coefficient to :math:`3 \times 10^{5}` m\ :sup:`2` s\ :sup:`-1` and specify
   a no-slip boundary condition for this operator, i.e., :math:`u=0`
   along boundaries in :math:`y` and :math:`v=0` along boundaries in
   :math:`x`.

-  Lines 11-13,

   ::

        viscAr =1.721611620915750e5,
       #viscAz =1.67E-3,
        no_slip_bottom=.FALSE.,

   These lines set the vertical Laplacian frictional dissipation
   coefficient to :math:`1.721611620915750 \times 10^{5}` Pa\ :sup:`2` s\ :sup:`-1`,
   which corresponds to
   :math:`1.67\times10^{-3}` m\ :sup:`2` s\ :sup:`-1` in the commented
   line, and specify a free slip boundary condition for this operator, i.e.,
   :math:`\frac{\partial u}{\partial p}=\frac{\partial v}{\partial p}=0`
   at :math:`p=p_{b}^{0}`, where :math:`p_{b}^{0}` is the local bottom
   pressure of the domain at rest. Note that the factor
   :math:`(g\rho)^2` needs to be included in this value.

-  Line 14,

   ::

        diffKhT=1.E3,

   this line sets the horizontal diffusion coefficient for temperature
   to 1000 m\ :sup:`2` s\ :sup:`-1`. The boundary condition on this
   operator is
   :math:`\frac{\partial}{\partial x}=\frac{\partial}{\partial y}=0`
   on all boundaries.

-  Line 15–16,

   ::

        diffKrT=5.154525811125000e3,
       #diffKzT=0.5E-4,

   this line sets the vertical diffusion coefficient for temperature to
   :math:`5.154525811125 \times 10^{3}` Pa\ :sup:`2` s\ :sup:`-1`, which
   corresponds to :math:`5\times10^{-4}` m\ :sup:`2` s\ :sup:`-1` in the
   commented line. Note that the factor :math:`(g\rho)^2` needs to be
   included in this value. The boundary condition on this operator is
   :math:`\frac{\partial}{\partial p}=0` at both the upper and lower
   boundaries.

-  Line 17–19,

   ::

        diffKhS=1.E3,
        diffKrS=5.154525811125000e3,
       #diffKzS=0.5E-4,

   These lines set the diffusion coefficients for
   salinity to the same value as for temperature.

-  Line 21–23,

   ::

        implicitDiffusion=.TRUE.,
        ivdc_kappa=1.030905162225000E9,
       #ivdc_kappa=10.0,

   Select implicit diffusion as a convection scheme and set coefficient
   for implicit vertical diffusion to
   :math:`1.030905162225\times10^{9}` Pa\ :sup:`2` s\ :sup:`-1`, which corresponds to
   10 m\ :sup:`2` s\ :sup:`-1`.

-  Line 24,

   ::

        gravity=9.81,

   This line sets the gravitational acceleration coefficient to
   9.81 m s\ :sup:`-1`.

-  Line 25,

   ::

        rhoConst=1035.,

   sets the reference density of sea water to 1035 kg m\ :sup:`-3`.

-  Line 29,

   ::

        eosType='JMD95P',

   Selects the full equation of state according to Jackett and McDougall (1995)
   :cite:`jackett:95`. Note that the only other sensible choice here could be
   the equation of state by McDougall et al. (2003) :cite:`mcdougall:03`, ``MDJFW``.
   Other model choices for equations of state do not make sense in this
   configuration.

-  Line 28-29,

   ::

         implicitFreeSurface=.TRUE.,

   Selects the barotropic pressure equation to be the implicit free
   surface formulation.

-  Line 32,

   ::

        exactConserv=.TRUE.,

   Select a more accurate conservation of properties at the surface
   layer by including the horizontal velocity divergence to update the
   free surface.

-  Line 33–35

   ::

        nonlinFreeSurf=3,
        hFacInf=0.2,
        hFacSup=2.0,

   Select the nonlinear free surface formulation and set lower and upper
   limits for the free surface excursions.

-  Line 39-40,

   ::

        readBinaryPrec=64,
        writeBinaryPrec=64,

   Sets format for reading binary input datasets and writing binary
   output datasets containing model fields to use 64-bit representation
   for floating-point numbers.

-  Line 45,

   ::

        cg2dMaxIters=200,

   Sets maximum number of iterations the 2-D conjugate
   gradient solver will use, **irrespective of convergence criteria
   being met**.

-  Line 46,

   ::

       cg2dTargetResidual=1.E-13,

   Sets the tolerance which the 2-D conjugate gradient
   solver will use to test for convergence in
   :eq:`elliptic-backward-free-surface` to
   :math:`1 \times 10^{-9}`. Solver will iterate until tolerance falls
   below this value or until the maximum number of solver iterations
   is reached.

-  Line 51,

   ::

       startTime=0,

   Sets the starting time for the model internal time counter. When set
   to non-zero, this option implicitly requests a checkpoint file be read
   for initial state. By default the checkpoint file is named according
   to the integer number of time steps in the :varlink:`startTime` value. The
   internal time counter works in seconds.

-  Line 52–54,

   ::

        endTime=8640000.,
        # after 100 years of intergration, one gets a reasonable flow field
        #endTime=3110400000.,

   Sets the time (in seconds) at which this simulation will terminate.
   At the end of a simulation a checkpoint file is automatically written
   so that a numerical experiment can consist of multiple stages. The
   commented out setting for endTime is for a 100 year simulation.

-  Line 55–57,

   ::

        deltaTmom      =   1200.0,
        deltaTtracer   = 172800.0,
        deltaTfreesurf = 172800.0,

   Sets the timestep :math:`\delta t_{v}` used in the momentum
   equations to 20 minutes and the timesteps
   :math:`\delta t_{\theta}` in the tracer equations and
   :math:`\delta t_{\eta}` in the implicit free surface equation to
   48 hours. See :numref:`time_stepping`.

-  Line 60,

   ::

        pChkptFreq  =3110400000.,

   write a pickup file every 100 years of integration.

-  Line 61-63,

   ::

        dumpFreq    = 3110400000.,
        taveFreq    = 3110400000.,
        monitorFreq =   1.,

   write model output and time-averaged model output every 100 years,
   and monitor statistics every model time step (this is set for testing purposes; change to a
   larger number for long integrations).

-  Line 64–66,

   ::

        periodicExternalForcing=.TRUE.,
        externForcingPeriod=2592000.,
        externForcingCycle=31104000.,

   Allow periodic external forcing: set one month forcing period during which
   a single time slice of data is valid, and the repeat cycle to one
   year. Thus, external forcing files will contain twelve periods of forcing data.

-  Line 67,

   ::

        tauThetaClimRelax=5184000.0,

   Set the restoring timescale to 2 months.

-  Line 59,

   ::

        abEps=0.1,

   Adams-Bashforth factor (see :numref:`adams-bashforth`).

-  Line 72,

   ::

        usingSphericalPolarGrid=.TRUE.,

   Select spherical grid.

-  Line 73–74,

   ::

        dXspacing=4.,
        dYspacing=4.,

   Set the horizontal grid spacing in degrees spherical distance.

-  Line 77–81,

   ::

        delR=7103300.720021, ...

   set the layer thickness in pressure units, starting with the bottom
   layer.

-  Line 87–96,

   ::

        bathyFile='topog.box'
        ploadFile='deltageopotjmd95.bin'
        hydrogThetaFile='lev_t.bin',
        hydrogSaltFile ='lev_s.bin',
        zonalWindFile  ='trenberth_taux.bin',
        meridWindFile  ='trenberth_tauy.bin',
        thetaClimFile  ='lev_sst.bin',
        surfQFile      ='shi_qnet.bin',
        EmPmRFile      ='shi_empmr.bin',

   These lines specify the names of the files holding the bathymetry
   data set, the time-independent geopotential height anomaly at the
   bottom, initial conditions of temperature and salinity, wind stress
   forcing fields, sea surface temperature climatology, heat flux, and
   fresh water flux (evaporation minus precipitation minus runoff) at
   the surface. See file descriptions in section
   :numref:`sec_eg-globalpressure-config`.

Other lines in the file :filelink:`input/data <verification/tutorial_global_oce_in_p/input/data>`
are standard values that are described in the :numref:`customize_model`.

File :filelink:`input/data.pkg <verification/tutorial_global_oce_in_p/input/data.pkg>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File :filelink:`input/eedata <verification/tutorial_global_oce_in_p/input/eedata>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File ``input/topog.bin``
~~~~~~~~~~~~~~~~~~~~~~~~

This file is a 2-D (:math:`x,y`) map of depths. This file is
assumed to contain 64-bit binary numbers giving the depth of the model
at each grid cell, ordered with the :math:`x` coordinate varying fastest. The
points are ordered from low coordinate to high coordinate for both axes.
The units and orientation of the depths in this file are the same as
used in the MITgcm code (Pa for this experiment). In this experiment, a
depth of 0 Pa indicates a land point (wall) and a depth of
>0 Pa indicates open ocean.

File ``input/deltageopotjmd95.box``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The file contains twelve identical 2-D maps (:math:`x,y`) of
geopotential height anomaly at the bottom at rest. The values have been
obtained by vertically integrating the hydrostatic equation with the
initial density field (using ``input/lev_t.bin`` and ``input/lev_s.bin``). This file has to be
consistent with the temperature and salinity field at rest and the choice
of equation of state!

Files ``input/lev_t.bin`` and ``input/lev_s.bin``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The files ``input/lev_t.bin`` and ``input/lev_s.bin`` specify the initial conditions for
temperature and salinity for every grid point in a 3-D
array (:math:`x,y,z`). The data are obtain by interpolating monthly mean
values using Levitus and Boyer (1994a,b) :cite:`levitus:94a,levitus:94b` for January onto the model grid.
Keep in mind that the first index corresponds to the bottom layer and
highest index to the surface layer.

Files ``input/trenberth_taux.bin`` and ``input/trenberth_tauy.bin``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The files ``input/trenberth_taux.bin`` and  ``input/trenberth_tauy.bin`` contain twelve
2-D (:math:`x,y`) maps of zonal and meridional wind stress
values, :math:`\tau_{x}` and :math:`\tau_{y}`, respectively, in 3-D arrays (:math:`x,y,t`).
These are monthly mean
values from Trenberth et al. (1990) :cite:`trenberth:90`, units of N m\ :sup:`-2`.

File ``input/lev_sst.bin``
~~~~~~~~~~~~~~~~~~~~~~~~~~

The file ``input/lev_sst.bin`` contains twelve monthly surface temperature
climatologies from Levitus and Boyer (1994b) :cite:`levitus:94b` in a 3-D
arrays (:math:`x,y,t`).

Files ``input/shi_qnet.bin`` and ``input/shi_empmr.bin``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The files ``input/shi_qnet.bin`` and ``input/shi_empmr.bin`` contain twelve monthly surface fluxes
of heat (qnet) and freshwater (empmr) from Jiang et al. (1999) :cite:`jiang:99` in
3-D arrays (:math:`x,y,t`). Both fluxes are normalized so
that the total forcing over one year results in no net flux into the ocean (note, the freshwater
flux is actually constant in time).

File :filelink:`code/SIZE.h <verification/tutorial_global_oce_in_p/code/SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The file :filelink:`code/SIZE.h <verification/tutorial_global_oce_in_p/code/SIZE.h>` is identical to that
described in :ref:`tutorial global ocean simulation <sec_global_oce_latlon>`, for more specifics see :numref:`tut_global_oce_latlon_code_size`.

File :filelink:`code/CPP_OPTIONS.h <verification/tutorial_global_oce_in_p/code/CPP_OPTIONS.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values except for:

-  ``#define`` :varlink:`ATMOSPHERIC_LOADING`

   enables pressure loading which is abused to include the initial
   geopotential height anomaly.

-  ``#define`` :varlink:`EXACT_CONSERV`

   enables more accurate conservation properties to include the
   horizontal mass divergence in the free surface.

-  ``#define`` :varlink:`NONLIN_FRSURF`

   enables the nonlinear free surface.

