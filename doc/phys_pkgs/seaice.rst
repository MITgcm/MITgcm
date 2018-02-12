.. _sub_phys_pkg_seaice:

SEAICE Package
--------------


Authors: Martin Losch, Dimitris Menemenlis, An Nguyen, Jean-Michel
Campin, Patrick Heimbach, Chris Hill and Jinlun Zhang

.. _ssub_phys_pkg_seaice_intro:

Introduction
++++++++++++

Package “seaice” provides a dynamic and thermodynamic interactive
sea-ice model.

CPP options enable or disable different aspects of the package
(Section :numref:`para_phys_pkg_seaice_compile`). Run-Time options, flags, filenames and
field-related dates/times are set in data.seaice (Section :numref:`para_phys_pkg_seaice_runtime`).
A description of key subroutines is given in Section
:numref:`para_phys_pkg_seaice_subroutines`. Input fields, units and sign conventions
are summarized in Section [sec:pkg:seaice:fields:sub:`u`\ nits], and
available diagnostics output is listed in Section
[sec:pkg:seaice:diagnostics].

SEAICE configuration, compiling & running
+++++++++++++++++++++++++++++++++++++++++


.. _para_phys_pkg_seaice_compile:

Compile-time options 
####################

 

As with all MITgcm packages, SEAICE can be turned on or off at compile
time

-  using the ``packages.conf`` file by adding ``seaice`` to it,

-  or using ``genmake2`` adding ``-enable=seaice`` or ``-disable=seaice`` switches

-  *required packages and CPP options*:
   SEAICE requires the external forcing package ``exf`` to be enabled; no
   additional CPP options are required.

(see Section [sec:buildingCode]).

Parts of the SEAICE code can be enabled or disabled at compile time via
CPP preprocessor flags. These options are set in ``SEAICE_OPTIONS.h``. :numref:`tab_phys_pkg_seaice_cpp` summarizes the most important ones. For more
options see the default ``pkg/seaice/SEAICE_OPTIONS.h``.

.. table:: Some of the most relevant CPP preporocessor flags in the ``seaice``-package. 
  :name: tab_phys_pkg_seaice_cpp

  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | **CPP option**                     | **Description**                                                                                          |
  +====================================+==========================================================================================================+
  | ``SEAICE_DEBUG``                   | Enhance STDOUT for debugging                                                                             |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_ALLOW_DYNAMICS``          | sea-ice dynamics code                                                                                    |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_CGRID``                   | LSR solver on C-grid (rather than original B-grid)                                                       |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_ALLOW_EVP``               | enable use of EVP rheology solver                                                                        |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_ALLOW_JFNK``              | enable use of JFNK rheology solver                                                                       |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_EXTERNAL_FLUXES``         | use EXF-computed fluxes as starting point                                                                |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_ZETA_SMOOTHREG``          | use differentialable regularization for viscosities                                                      |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_VARIABLE_FREEZING_POINT`` | enable linear dependence of the freezing point on salinity (by default undefined)                        |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``ALLOW_SEAICE_FLOODING``          | enable snow to ice conversion for submerged sea-ice                                                      |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_VARIABLE_SALINITY``       | enable sea-ice with variable salinity (by default undefined)                                             |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_SITRACER``                | enable sea-ice tracer package (by default undefined)                                                     |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``SEAICE_BICE_STRESS``             | B-grid only for backward compatiblity: turn on ice-stress on ocean                                       |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+
  | ``EXPLICIT_SSH_SLOPE``             | B-grid only for backward compatiblity: use ETAN for tilt computations rather than geostrophic velocities |
  +------------------------------------+----------------------------------------------------------------------------------------------------------+


.. _para_phys_pkg_seaice_runtime:

Run-time parameters 
###################

Run-time parameters (see :numref:`tab_phys_pkg_seaice_runtimeparms`) are set in
files `data.pkg` (read in `packages_readparms.F`), and `data.seaice` (read in `seaice_readparms.F`).

Enabling the package
^^^^^^^^^^^^^^^^^^^^

A package is switched on/off at run-time by setting (e.g. for SEAICE `useSEAICE = .TRUE.` in `data.pkg`).

General flags and parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:numref:`tab_phys_pkg_seaice_runtimeparms` lists most run-time parameters.


.. table:: Run-time parameters and default values
  :name: tab_phys_pkg_seaice_runtimeparms

  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   **Name**                   |     **Default value**        | **Description**                                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICEwriteState           |     T                        | write sea ice state to file                                                                    |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICEuseDYNAMICS          |     T                        | use dynamics                                                                                   |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICEuseJFNK              |     F                        | use the JFNK-solver                                                                            |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICEuseTEM               |     F                        | use truncated ellipse method                                                                   |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICEuseStrImpCpl         |     F                        | use strength implicit coupling in LSR/JFNK                                                     |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICEuseMetricTerms       |     T                        | use metric terms in dynamics                                                                   |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICEuseEVPpickup         |     T                        | use EVP pickups                                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICEuseFluxForm          |     F                        | use flux form for 2nd central difference advection scheme                                      |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   SEAICErestoreUnderIce      |     F                        | enable restoring to climatology under ice                                                      |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   useHB87stressCoupling      |     F                        | turn on ice-ocean stress coupling following                                                    |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   usePW79thermodynamics      |     T                        | flag to turn off zero-layer-thermodynamics for testing                                         |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEadvHeff/Area/Snow/Salt | T                            | flag to turn off advection of scalar state variables                                           |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEuseFlooding            | T                            | use flood-freeze algorithm                                                                     |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_no_slip               | F                            | switch between free-slip and no-slip boundary conditions                                       |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_deltaTtherm           | dTracerLev(1)                | thermodynamic timestep                                                                         |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_deltaTdyn             | dTracerLev(1)                | dynamic timestep                                                                               |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_deltaTevp             | 0                            | EVP sub-cycling time step, values :math:`>` 0 turn on EVP                                      |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEuseEVPstar             | F                            | use modified EVP\* instead of EVP                                                              |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEuseEVPrev              | F                            | use yet another variation on EVP\*                                                             |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEnEVPstarSteps          | UNSET                        | number of modified EVP\* iteration                                                             |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_evpAlpha              | UNSET                        | EVP\* parameter                                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_evpBeta               | UNSET                        | EVP\* parameter                                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEaEVPcoeff              | UNSET                        | aEVP parameter                                                                                 |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEaEVPcStar              | 4                            | aEVP parameter   :cite:`kimmritz16`                                                            |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEaEVPalphaMin           | 5                            | aEVP parameter   :cite:`kimmritz16`                                                            |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_elasticParm           | :math:`\frac{1}{3}`          | EVP paramter :math:`E_0`                                                                       |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_evpTauRelax           | :math:`\Delta{t}_{EVP}`      | relaxation time scale :math:`T` for EVP waves                                                  |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEnonLinIterMax          | 10                           |  maximum number of JFNK-Newton iterations (non-linear)                                         |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICElinearIterMax          | 10                           | maximum number of JFNK-Krylov iterations (linear)                                              |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_JFNK_lsIter           | (off)                        | start line search after “lsIter” Newton iterations                                             |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEnonLinTol              | 1.0E-05                      | non-linear tolerance parameter for JFNK solver                                                 |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | JFNKgamma_lin_min/max        | 0.10/0.99                    | tolerance parameters for linear JFNK solver                                                    |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | JFNKres_tFac                 | UNSET                        | tolerance parameter for FGMRES residual                                                        |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_JFNKepsilon           | 1.0E-06                      | step size for the FD-Jacobian-times-vector                                                     |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_dumpFreq              | dumpFreq                     | dump frequency                                                                                 |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_taveFreq              | taveFreq                     | time-averaging frequency                                                                       |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_dump_mdsio            | T                            | write snap-shot using MDSIO                                                                    |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_tave_mdsio            | T                            | write TimeAverage using MDSIO                                                                  |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_dump_mnc              | F                            | write snap-shot using MNC                                                                      |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_tave_mnc              | F                            | write TimeAverage using MNC                                                                    |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_initialHEFF           | 0.00000E+00                  | initial sea-ice thickness                                                                      |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_drag                  | 2.00000E-03                  | air-ice drag coefficient                                                                       |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | OCEAN_drag                   | 1.00000E-03                  | air-ocean drag coefficient                                                                     |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_waterDrag             | 5.50000E+00                  | water-ice drag                                                                                 |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_dryIceAlb             | 7.50000E-01                  | winter albedo                                                                                  |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_wetIceAlb             | 6.60000E-01                  | summer albedo                                                                                  |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_drySnowAlb            | 8.40000E-01                  | dry snow albedo                                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_wetSnowAlb            | 7.00000E-01                  | wet snow albedo                                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_waterAlbedo           | 1.00000E-01                  | water albedo                                                                                   |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_strength              | 2.75000E+04                  | sea-ice strength :math:`P^{*}`                                                                 |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_cStar                 | 20.0000E+00                  | sea-ice strength paramter :math:`C^{*}`                                                        |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_rhoAir                | 1.3 (or value)               | density of air (kg/m:math:`^3`)                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_cpAir                 | 1004 (or value)              | specific heat of air (J/kg/K)                                                                  |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_lhEvap                | 2,500,000 (or val    ue)     | latent heat of evaporation                                                                     |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_lhFusion              | 334,000 (or value    )       | latent heat of fusion                                                                          |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_lhSublim              | 2,834,000                    | latent heat of sublimation                                                                     |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_dalton                | 1.75E-03                     | sensible heat transfer coefficient                                                             |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_iceConduct            | 2.16560E+00                  | sea-ice conductivity                                                                           |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_snowConduct           | 3.10000E-01                  | snow conductivity                                                                              |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_emissivity            | 5.50000E-08                  | Stefan-Boltzman                                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_snowThick             | 1.50000E-01                  | cutoff snow thickness                                                                          |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_shortwave             | 3.00000E-01                  | penetration shortwave radiation                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_freeze                | -1.96000E+00                 | freezing temp. of sea water                                                                    |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_saltFrac              | 0.0                          | salinity newly formed ice (fraction of ocean surface salinity)                                 |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_frazilFrac            | 0.0                          | Fraction of surface level negative heat content anomalies (relative to the local freezing poin |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICEstressFactor           | 1.00000E+00                  | scaling factor for ice-ocean stress                                                            |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | Heff/Area/HsnowFile/Hsalt    | UNSET                        | initial fields for variables HEFF/AREA/HSNOW/HSALT                                             |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | LSR_ERROR                    | 1.00000E-04                  | sets accuracy of LSR solver                                                                    |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | DIFF1                        | 0.0                          | parameter used in advect.F                                                                     |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | HO                           | 5.00000E-01                  | demarcation ice thickness (AKA lead closing paramter :math:`h_0`)                              |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | MAX_HEFF                     | 1.00000E+01                  | maximum ice thickness                                                                          |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | MIN_ATEMP                    | -5.00000E+01                 | minimum air temperature                                                                        |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | MIN_LWDOWN                   | 6.00000E+01                  | minimum downward longwave                                                                      |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | MAX_TICE                     | 3.00000E+01                  | maximum ice temperature                                                                        |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | MIN_TICE                     | -5.00000E+01                 | minimum ice temperature                                                                        |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | IMAX_TICE                    | 10                           | iterations for ice heat budget                                                                 |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_EPS                   | 1.00000E-10                  | reduce derivative singularities                                                                |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_area_reg              | 1.00000E-5                   | minimum concentration to regularize ice thickness                                              |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_hice_reg              | 0.05 m                       | minimum ice thickness for regularization                                                       |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_multDim               | 1                            | number of ice categories for thermodynamics                                                    |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | SEAICE_useMultDimSnow        | F                            | use SEAICE_multDim snow categories                                                             |
  +------------------------------+------------------------------+------------------------------------------------------------------------------------------------+


Input fields and units
^^^^^^^^^^^^^^^^^^^^^^

 - `HeffFile`: Initial sea ice thickness averaged over grid cell in meters; initializes variable `HEFF`;

 - `AreaFile`: Initial fractional sea ice cover, range :math:`[0,1]`; initializes variable `AREA`;

 - `HsnowFile`: Initial snow thickness on sea ice averaged over grid cell in meters; initializes variable `HSNOW`;

 - `HsaltFile`: Initial salinity of sea ice averaged over grid cell in g/m\ :math:`^2`; initializes variable `HSALT`;


.. _ssub_phys_pkg_seaice_descr:

Description
+++++++++++

[TO BE CONTINUED/MODIFIED]

The MITgcm sea ice model (MITgcm/sim) is based on a variant of the
viscous-plastic (VP) dynamic-thermodynamic sea ice model :cite:`zhang97` first
introduced by :cite:`hib79,hib80`. In order to adapt this model to the requirements of
coupled ice-ocean state estimation, many important aspects of the
original code have been modified and improved :cite:`losch10:_mitsim`:

-  the code has been rewritten for an Arakawa C-grid, both B- and C-grid
   variants are available; the C-grid code allows for no-slip and
   free-slip lateral boundary conditions;

-  three different solution methods for solving the nonlinear momentum
   equations have been adopted: LSOR :cite:`zhang97`, EVP :cite:`hun97`, JFNK :cite:`lemieux10,losch14:_jfnk`;

-  ice-ocean stress can be formulated as in :cite:`hibler87` or as in :cite:`cam08`;

-  ice variables are advected by sophisticated, conservative advection
   schemes with flux limiting;

-  growth and melt parameterizations have been refined and extended in
   order to allow for more stable automatic differentiation of the code.

The sea ice model is tightly coupled to the ocean compontent of the
MITgcm. Heat, fresh water fluxes and surface stresses are computed from
the atmospheric state and – by default – modified by the ice model at
every time step.

The ice dynamics models that are most widely used for large-scale
climate studies are the viscous-plastic (VP) model :cite:`hib79`, the cavitating
fluid (CF) model :cite:`fla92`, and the elastic-viscous-plastic (EVP) model :cite:`hun97`.
Compared to the VP model, the CF model does not allow ice shear in
calculating ice motion, stress, and deformation. EVP models approximate
VP by adding an elastic term to the equations for easier adaptation to
parallel computers. Because of its higher accuracy in plastic solution
and relatively simpler formulation, compared to the EVP model, we
decided to use the VP model as the default dynamic component of our ice
model. To do this we extended the line successive over relaxation (LSOR)
method of :cite:`zhang97` for use in a parallel configuration. An EVP model and a
free-drift implemtation can be selected with runtime flags.


.. _para_phys_pkg_seaice_thsice:

Compatibility with ice-thermodynamics ``thsice`` package
########################################################

Note, that by default the ``seaice``-package includes the orginial so-called
zero-layer thermodynamics following with a snow cover as in . The
zero-layer thermodynamic model assumes that ice does not store heat and,
therefore, tends to exaggerate the seasonal variability in ice
thickness. This exaggeration can be significantly reduced by using ’s []
three-layer thermodynamic model that permits heat storage in ice.
Recently, the three-layer thermodynamic model has been reformulated by .
The reformulation improves model physics by representing the brine
content of the upper ice with a variable heat capacity. It also improves
model numerics and consumes less computer time and memory.

The Winton sea-ice thermodynamics have been ported to the MIT GCM; they currently reside under ``pkg/seaice``. The package ``thsice`` is described in section [sec:pkg:thsice]; it is fully compatible with the packages ``seaice`` and ``exf``.  When turned on together with ``seaice``, the zero-layer thermodynamics are replaced by the Winton thermodynamics. In order to use the ``seaice``-package with the thermodynamics of ``thsice``, compile both packages and turn both package on in ``data.pkg``; see an example in ``global_ocean.cs32x15/input.icedyn``. Note, that once ``thsice`` is turned on, the variables and diagnostics associated to the default thermodynamics are meaningless, and the diagnostics of ``thsice`` have to be used instead.

.. _para_phys_pkg_seaice_surfaceforcing:

Surface forcing
###############

|  

The sea ice model requires the following input fields: 10-m winds, 2-m air temperature and specific humidity, downward longwave and shortwave radiations, precipitation, evaporation, and river and glacier runoff. The sea ice model also requires surface temperature from the ocean model and the top level horizontal velocity. Output fields are surface wind stress, evaporation minus precipitation minus runoff, net surface heat flux, and net shortwave flux. The sea-ice model is global: in ice-free regions bulk formulae are used to estimate oceanic forcing from the atmospheric fields.

.. _para_phys_pkg_seaice_dynamics:

Dynamics
########

|  

The momentum equation of the sea-ice model is

.. math::
   :label: eq_momseaice
	   
     m \frac{D\mathbf{u}}{Dt} = -mf\mathbf{k}\times\mathbf{u} +
     \mathbf{\tau}_{air} + \mathbf\tau}_{ocean}
     - m \nabla{\phi(0)} + \mathbf{F},

where :math:`m=m_{i}+m_{s}` is the ice and snow mass per unit area;
:math:`\mathbf{u}=u\mathbf{i}+v\mathbf{j}`
is the ice velocity vector; :math:`\mathbf{i}`,
:math:`\mathbf{j}`, and
:math:`\mathbf{k}` are unit vectors in the
:math:`x`, :math:`y`, and :math:`z` directions, respectively; :math:`f`
is the Coriolis parameter;
:math:`\mathbf{\tau}_{air}` and
:math:`\mathbf{\tau}_{ocean}` are the
wind-ice and ocean-ice stresses, respectively; :math:`g` is the gravity
accelation; :math:`\nabla\phi(0)` is the gradient (or tilt) of the sea
surface height; :math:`\phi(0) = g\eta + p_{a}/\rho_{0} + mg/\rho_{0}`
is the sea surface height potential in response to ocean dynamics
(:math:`g\eta`), to atmospheric pressure loading
(:math:`p_{a}/\rho_{0}`, where :math:`\rho_{0}` is a reference density)
and a term due to snow and ice loading ; and
:math:`\mathbf{F}=\nabla\cdot\sigma` is the
divergence of the internal ice stress tensor :math:`\sigma_{ij}`.
Advection of sea-ice momentum is neglected. The wind and ice-ocean
stress terms are given by

.. math::

   \begin{aligned}
     \mathbf{\tau}_{air}   = & \rho_{air}  C_{air}
     |\mathbf{U}_{air} -\mathbf{u}|  R_{air}  (\mathbf{U}_{air}
     -\mathbf{u}), \\
     \mathbf{\tau}_{ocean} = & \rho_{ocean}C_{ocean}
     |\mathbf{U}_{ocean}-\mathbf{u}|
     R_{ocean}(\mathbf{U}_{ocean}-\mathbf{u}),
   \end{aligned}

where :math:`\mathbf{U}_{air/ocean}` are the
surface winds of the atmosphere and surface currents of the ocean,
respectively; :math:`C_{air/ocean}` are air and ocean drag coefficients;
:math:`\rho_{air/ocean}` are reference densities; and
:math:`R_{air/ocean}` are rotation matrices that act on the wind/current
vectors.

.. _para_phys_pkg_seaice_VPrheology:

Viscous-Plastic (VP) Rheology
#############################

|  

For an isotropic system the stress tensor :math:`\sigma_{ij}`
(:math:`i,j=1,2`) can be related to the ice strain rate and strength 
by a nonlinear viscous-plastic (VP) constitutive law :

.. math::
   :label: eq_vpequation
	   
     \sigma_{ij}=2\eta(\dot{\epsilon}_{ij},P)\dot{\epsilon}_{ij} 
     + \left[\zeta(\dot{\epsilon}_{ij},P) -
       \eta(\dot{\epsilon}_{ij},P)\right]\dot{\epsilon}_{kk}\delta_{ij}  
     - \frac{P}{2}\delta_{ij}.

The ice strain rate is given by

.. math::

   \dot{\epsilon}_{ij} = \frac{1}{2}\left( 
       \frac{\partial{u_{i}}}{\partial{x_{j}}} +
       \frac{\partial{u_{j}}}{\partial{x_{i}}}\right).

The maximum ice pressure :math:`P_{\max}`, a measure of ice strength,
depends on both thickness :math:`h` and compactness (concentration)
:math:`c`:

.. math::

   P_{\max} = P^{*}c\,h\,\exp\{-C^{*}\cdot(1-c)\},
   \label{eq:icestrength}

with the constants :math:`P^{*}` (run-time parameter ``SEAICE_strength``) and
:math:`C^{*}=20`. The nonlinear bulk and shear viscosities :math:`\eta`
and :math:`\zeta` are functions of ice strain rate invariants and ice
strength such that the principal components of the stress lie on an
elliptical yield curve with the ratio of major to minor axis :math:`e`
equal to :math:`2`; they are given by:

.. math::

   \begin{aligned}
     \zeta =& \min\left(\frac{P_{\max}}{2\max(\Delta,\Delta_{\min})},
      \zeta_{\max}\right) \\
     \eta =& \frac{\zeta}{e^2} \\
     & \text{with the abbreviation} \\
     \Delta = & \left[
       \left(\dot{\epsilon}_{11}^2+\dot{\epsilon}_{22}^2\right)
       (1+e^{-2}) +  4e^{-2}\dot{\epsilon}_{12}^2 + 
       2\dot{\epsilon}_{11}\dot{\epsilon}_{22} (1-e^{-2})
     \right]^{\frac{1}{2}}.\end{aligned}

The bulk viscosities are bounded above by imposing both a minimum
:math:`\Delta_{\min}` (for numerical reasons, run-time parameter
``SEAICE_EPS`` with a default value of :math:`10^{-10}\text{\,s}^{-1}`)
and a maximum :math:`\zeta_{\max} = P_{\max}/\Delta^*`, where
:math:`\Delta^*=(5\times10^{12}/2\times10^4)\text{\,s}^{-1}`. (There
is also the option of bounding :math:`\zeta` from below by setting
run-time parameter ``SEAICE_zetaMin`` :math:`>0`, but this is generally not
recommended). For stress tensor computation the replacement pressure
:math:`P = 2\,\Delta\zeta` is used so that the stress state always
lies on the elliptic yield curve by definition.

Defining the CPP-flag ``SEAICE_ZETA_SMOOTHREG`` in ``SEAICE_OPTIONS.h`` before compiling replaces the method for
bounding :math:`\zeta` by a smooth (differentiable) expression:

.. math::

   \label{eq:zetaregsmooth}
     \begin{split}
     \zeta &= \zeta_{\max}\tanh\left(\frac{P}{2\,\min(\Delta,\Delta_{\min})
         \,\zeta_{\max}}\right)\\
     &= \frac{P}{2\Delta^*}
     \tanh\left(\frac{\Delta^*}{\min(\Delta,\Delta_{\min})}\right) 
     \end{split}

where :math:`\Delta_{\min}=10^{-20}\text{\,s}^{-1}` is chosen to avoid
divisions by zero.

.. _para_phys_pkg_seaice_LSRJFNK:

LSR and JFNK solver
###################

|  

In the matrix notation, the discretized momentum equations can be
written as

.. math::
   :label: eq_matrixmom
	   
     \mathbf{A}(\mathbf{x})\,\mathbf{x} = \mathbf{b}(\mathbf{x}).

The solution vector :math:`\mathbf{x}` consists of the two velocity
components :math:`u` and :math:`v` that contain the velocity variables
at all grid points and at one time level. The standard (and default)
method for solving Eq. :eq:`eq_matrixmom` in the sea ice component of
the MITgcm, as in many sea ice models, is an iterative Picard solver: in the
:math:`k`-th iteration a linearized form
:math:`\mathbf{A}(\mathbf{x}^{k-1})\,\mathbf{x}^{k} =
\mathbf{b}(\mathbf{x}^{k-1})` is solved (in the case of the MITgcm it
is a Line Successive (over) Relaxation (LSR) algorithm ). Picard
solvers converge slowly, but generally the iteration is terminated
after only a few non-linear steps and the calculation continues with
the next time level. This method is the default method in the
MITgcm. The number of non-linear iteration steps or pseudo-time steps
can be controlled by the runtime parameter ``SEAICEnonLinIterMax``
(default is 2).

In order to overcome the poor convergence of the Picard-solver,
introduced a Jacobian-free Newton-Krylov solver for the sea ice momentum
equations. This solver is also implemented in the MITgcm . The Newton
method transforms minimizing the residual
:math:`\mathbf{F}(\mathbf{x}) = \mathbf{A}(\mathbf{x})\,\mathbf{x} -
\mathbf{b}(\mathbf{x})` to finding the roots of a multivariate Taylor
expansion of the residual :math:`\mathbf{F}` around the previous
(:math:`k-1`) estimate :math:`\mathbf{x}^{k-1}`:

.. math::
   :label: eq_jfnktaylor

      \mathbf{F}(\mathbf{x}^{k-1}+\delta\mathbf{x}^{k}) =
      \mathbf{F}(\mathbf{x}^{k-1}) + \mathbf{F}'(\mathbf{x}^{k-1})
      \,\delta\mathbf{x}^{k}

with the Jacobian
:math:`\mathbf{J}\equiv\mathbf{F}'`.
The root
:math:`\mathbf{F}(\mathbf{x}^{k-1}+\delta\mathbf{x}^{k})=0`
is found by solving

.. math::
   :label: eq_jfnklin
	   
      \mathbf{J}(\mathbf{x}^{k-1})\,\delta\mathbf{x}^{k} =
      -\mathbf{F}(\mathbf{x}^{k-1})

for :math:`\delta\mathbf{x}^{k}`. The next
(:math:`k`-th) estimate is given by
:math:`\mathbf{x}^{k}=\mathbf{x}^{k-1}+a\,\delta\mathbf{x}^{k}`.
In order to avoid overshoots the factor :math:`a` is iteratively reduced
in a line search
(:math:`a=1, \frac{1}{2}, \frac{1}{4}, \frac{1}{8}, \ldots`) until
:math:`\|\mathbf{F}(\mathbf{x}^k)\| <  \|\mathbf{F}(\mathbf{x}^{k-1})\|`,
where :math:`\|\cdot\|=\int\cdot\,dx^2` is the :math:`L_2`-norm. In
practice, the line search is stopped at :math:`a=\frac{1}{8}`. The line
search starts after ``SEAICE_JFNK_lsIter`` non-linear
Newton iterations (off by default).

Forming the Jacobian :math:`\mathbf{J}` explicitly is
often avoided as “too error prone and time consuming” . Instead, Krylov
methods only require the action of :math:`\mathbf{J}` on an arbitrary
vector :math:`\mathbf{w}` and hence allow a matrix free algorithm
for solving Eq. :eq:`eq_jfnklin`. The action of :math:`\mathbf{J}` can be
approximated by a first-order Taylor series expansion:

.. math::
   :label: eq_jfnkjacvecfd

	   \mathbf{J}(\mathbf{x}^{k-1})\,\mathbf{w} \approx
	   \frac{\mathbf{F}(\mathbf{x}^{k-1}+\epsilon\mathbf{w})
	   - \mathbf{F}(\mathbf{x}^{k-1})} \epsilon

or computed exactly with the help of automatic differentiation (AD)
tools. ``SEAICE_JFNKepsilon`` sets the step size :math:`\epsilon`.

We use the Flexible Generalized Minimum RESidual method with
right-hand side preconditioning to solve Eq. :eq:`eq_jfnklin`
iteratively starting from a first guess of
:math:`\delta\mathbf{x}^{k}_{0} = 0`. For the preconditioning matrix
:math:`\mathbf{P}` we choose a simplified form of the system matrix
:math:`\mathbf{A}(\mathbf{x}^{k-1})` where :math:`\mathbf{x}^{k-1}` is
the estimate of the previous Newton step :math:`k-1`. The transformed
equation :eq:`eq_jfnklin` becomes

.. math::
   :label: eq_jfnklinpc

   \mathbf{J}(\mathbf{x}^{k-1})\,\mathbf{P}^{-1}\delta\mathbf{z} =
   -\mathbf{F}(\mathbf{x}^{k-1}), \quad\text{with} \quad
   \delta{\mathbf{z}} = \mathbf{P}\delta\mathbf{x}^{k}.

The Krylov method iteratively improves the approximate solution
to Eq. :eq:`eq_jfnklinpc` in subspace
(:math:`\mathbf{r}_0`, :math:`\mathbf{J}\mathbf{P}^{-1}\mathbf{r}_0`,
:math:`(\mathbf{J}\mathbf{P}^{-1})^2\mathbf{r}_0`, 
:math:`\dots`, 
:math:`(\mathbf{J}\mathbf{P}^{-1})^m\mathbf{r}_0`)
with increasing :math:`m`;
:math:`\mathbf{r}_0 = -\mathbf{F}(\mathbf{x}^{k-1})      -\mathbf{J}(\mathbf{x}^{k-1})\,\delta\mathbf{x}^{k}_{0}`
is the initial residual of Eq. :eq:`eq_jfnklin`;
:math:`\mathbf{r}_0=-\mathbf{F}(\mathbf{x}^{k-1})`
with the first guess
:math:`\delta\mathbf{x}^{k}_{0}=0`. We allow a
Krylov-subspace of dimension \ :math:`m=50` and we do not use restarts.
The preconditioning operation involves applying
:math:`\mathbf{P}^{-1}` to the basis vectors
:math:`\mathbf{v}_0, \mathbf{v}_1, \mathbf{v}_2, \ldots, \mathbf{v}_m`
of the Krylov subspace. This operation is approximated by solving the
linear system
:math:`\mathbf{P}\,\mathbf{w}=\mathbf{v}_i`.
Because :math:`\mathbf{P} \approx \mathbf{A}(\mathbf{x}^{k-1})`, we
can use the LSR-algorithm already implemented in the Picard solver. Each
preconditioning operation uses a fixed number of 10 LSR-iterations
avoiding any termination criterion. More details and results can be
found in .

To use the JFNK-solver set ``SEAICEuseJNFK = .TRUE.,`` in the namelist file
``data.seaice``; ``SEAICE_ALLOW_JFNK`` needs to be defined in ``SEAICE_OPTIONS.h`` and we recommend using a smooth regularization of :math:`\zeta` by defining ``SEAICE_ZETA_SMOOTHREG`` (see above) for better convergence. The non-linear Newton iteration is terminated when the :math:`L_2`-norm of the residual is reduced by :math:`\gamma_{\mathrm{nl}}` (runtime parameter ``SEAICEnonLinTol = 1.E-4,`` will already lead to expensive simulations) with respect to the initial norm: :math:`\|\mathbf{F}(\mathbf{x}^k)\| <
\gamma_{\mathrm{nl}}\|\mathbf{F}(\mathbf{x}^0)\|`.
Within a non-linear iteration, the linear FGMRES solver is terminated
when the residual is smaller than :math:`\gamma_k\|\mathbf{F}(\mathbf{x}^{k-1})\|` where :math:`\gamma_k` is determined by

.. math::
   :label: eq_jfnkgammalin

	   \gamma_k = 
      \begin{cases} 
	   \gamma_0 &\text{for $\|\mathbf{F}(\mathbf{x}^{k-1})\| \geq r$},  \\ 
       \max\left(\gamma_{\min},
       \frac{\|\mathbf{F}(\mathbf{x}^{k-1})\|}
       {\|\mathbf{F}(\mathbf{x}^{k-2})\|}\right)  
       &\text{for $\|\mathbf{F}(\mathbf{x}^{k-1})\| < r$,}
     \end{cases}

so that the linear tolerance parameter :math:`\gamma_k` decreases with
the non-linear Newton step as the non-linear solution is approached.
This inexact Newton method is generally more robust and
computationally more efficient than exact methods . Typical parameter
choices are :math:`\gamma_0` = ``JFNKgamma_lin_max`` = 0.99,
:math:`\gamma_{\min}` = ``JFNKgamma_lin_min`` = 0.1, and :math:`r` =
``JFNKres_tFac``
:math:`\times\|\mathbf{F}(\mathbf{x}^{0})\|` with
``JFNKres_tFac`` = 0.5. We recommend a maximum number of
non-linear iterations ``SEAICEnewtonIterMax`` = 100 and a maximum number
of Krylov iterations ``SEAICEkrylovIterMax`` = 50, because the Krylov
subspace has a fixed dimension of 50.

Setting ``SEAICEuseStrImpCpl = .TRUE.,`` turns on “strength implicit
coupling” :cite:`hutchings04` in the LSR-solver and in the LSR-preconditioner for the JFNK-solver. In this mode, the different contributions of the stress
divergence terms are re-ordered in order to increase the diagonal dominance of the system matrix. Unfortunately, the convergence rate of the LSR solver is increased only slightly, while the JFNK-convergence appears to be unaffected.

.. _para_phys_pkg_seaice_EVPdynamics:

Elastic-Viscous-Plastic (EVP) Dynamics
######################################

:cite:`hun97` introduced an elastic contribution to the strain rate in
order to regularize :eq:`eq_vpequation` in such a way that the
resulting elastic-viscous-plastic (EVP) and VP models are identical at steady state,

.. math::
   :label: eq_evpequation

   \frac{1}{E}\frac{\partial\sigma_{ij}}{\partial{t}} +
     \frac{1}{2\eta}\sigma_{ij} 
     + \frac{\eta - \zeta}{4\zeta\eta}\sigma_{kk}\delta_{ij}  
     + \frac{P}{4\zeta}\delta_{ij}
     = \dot{\epsilon}_{ij}.

The EVP-model uses an explicit time stepping scheme with a short timestep. According to the recommendation of :cite:`hun97`, the EVP-model should be stepped forward in time 120 times (``SEAICE_deltaTevp`` = ``SEAICIE_deltaTdyn``/120) within the physical ocean model time step (although this parameter is under debate), to allow for elastic waves to disappear. Because the scheme does not require a matrix inversion it is fast in spite of the small internal timestep and simple to implement on parallel computers .
For completeness, we repeat the equations for the components of the
stress tensor :math:`\sigma_{1} =
\sigma_{11}+\sigma_{22}`, :math:`\sigma_{2}= \sigma_{11}-\sigma_{22}`,
and :math:`\sigma_{12}`. Introducing the divergence :math:`D_D =
\dot{\epsilon}_{11}+\dot{\epsilon}_{22}`, and the horizontal tension and
shearing strain rates, :math:`D_T =
\dot{\epsilon}_{11}-\dot{\epsilon}_{22}` and :math:`D_S =
2\dot{\epsilon}_{12}`, respectively, and using the above abbreviations,
the equations :eq:`eq_evpequation` can be written as:

.. math::

   \begin{aligned}
     \label{eq:evpstresstensor1}
     \frac{\partial\sigma_{1}}{\partial{t}} + \frac{\sigma_{1}}{2T} +
     \frac{P}{2T} &= \frac{P}{2T\Delta} D_D \\
     \label{eq:evpstresstensor2}
     \frac{\partial\sigma_{2}}{\partial{t}} + \frac{\sigma_{2} e^{2}}{2T}
     &= \frac{P}{2T\Delta} D_T \\
     \label{eq:evpstresstensor12}
     \frac{\partial\sigma_{12}}{\partial{t}} + \frac{\sigma_{12} e^{2}}{2T}
     &= \frac{P}{4T\Delta} D_S \end{aligned}

Here, the elastic parameter :math:`E` is redefined in terms of a damping
timescale :math:`T` for elastic waves

.. math:: E=\frac{\zeta}{T}.

:math:`T=E_{0}\Delta{t}` with the tunable parameter :math:`E_0<1` and
the external (long) timestep :math:`\Delta{t}`.
:math:`E_{0} = \frac{1}{3}` is the default value in the code and close
to what and recommend.

To use the EVP solver, make sure that both ``SEAICE_CGRID`` and
``SEAICE_ALLOW_EVP`` are defined in ``SEAICE_OPTIONS.h``
(default). The solver is turned on by setting the sub-cycling time
step ``SEAICE_deltaTevp`` to a value larger than zero. The choice of
this time step is under debate.  :cite:`hun97` recommend order(120)
time steps for the EVP solver within one model time step
:math:`\Delta{t}` (``deltaTmom``). One can also choose order(120) time
steps within the forcing time scale, but then we recommend adjusting
the damping time scale :math:`T` accordingly, by setting either ``SEAICE_elasticPlarm`` (:math:`E_{0}`), so that :math:`E_{0}\Delta{t}=` forcing time scale, or directly ``SEAICE_evpTauRelax`` (:math:`T`) to the forcing time scale. (NOTE: with the improved EVP variants of the next section, the above recommendations are obsolete. Use mEVP or aEVP instead.)

.. _para_phys_pkg_seaice_EVPstar:

More stable variants of Elastic-Viscous-Plastic Dynamics: EVP\* , mEVP, and aEVP
################################################################################

The genuine EVP schemes appears to give noisy solu tions :cite:`hun01,lemieux12,bouillon13`. This has lead to a modified EVP or EVP\* :cite:`lemieux12,bouillon13,kimmritz15`; here, we refer to these variants by modified EVP (mEVP) and adaptive EVP (aEVP) :cite:`kimmritz16`. The main idea is to modify the “natural” time-discretization of the momentum equations:

.. math::
   :label: eq_evpstar
	   
     m\frac{D\mathbf{u}}{Dt} \approx
     m\frac{\mathbf{u}^{p+1}-\mathbf{u}^{n}}{\Delta{t}} +
     \beta^{*}\frac{\mathbf{u}^{p+1}-\mathbf{u}^{p}}{\Delta{t}_{\mathrm{EVP}}}

where :math:`n` is the previous time step index, and :math:`p` is the
previous sub-cycling index. The extra “intertial” term
:math:`m\,(\mathbf{u}^{p+1}-\mathbf{u}^{n})/\Delta{t})` allows the
definition of a residual :math:`|\mathbf{u}^{p+1}-\mathbf{u}^{p}|`
that, as :math:`\mathbf{u}^{p+1} \rightarrow \mathbf{u}^{n+1}`,
converges to :math:`0`. In this way EVP can be re-interpreted as a
pure iterative solver where the sub-cycling has no association with
time-relation (through :math:`\Delta{t}_{\mathrm{EVP}}`) . Using the
terminology of , the evolution equations of stress :math:`\sigma_{ij}`
and momentum :math:`\mathbf{u}` can be written as:

.. math::

   \begin{aligned}
     \label{eq:evpstarsigma}
     \sigma_{ij}^{p+1}&=\sigma_{ij}^p+\frac{1}{\alpha}
     \Big(\sigma_{ij}(\mathbf{u}^p)-\sigma_{ij}^p\Big),
     \phantom{\int}\\
     \label{eq:evpstarmom}
     \mathbf{u}^{p+1}&=\mathbf{u}^p+\frac{1}{\beta}
     \Big(\frac{\Delta t}{m}\nabla \cdot{\bf \sigma}^{p+1}+
     \frac{\Delta t}{m}\mathbf{R}^{p}+\mathbf{u}_n
     -\mathbf{u}^p\Big).\end{aligned}

:math:`\mathbf{R}` contains all terms in the momentum equations except
for the rheology terms and the time derivative; :math:`\alpha` and
:math:`\beta` are free parameters (``SEAICE_evpAlpha``, ``SEAICE_evpBeta``) that replace the time stepping parameters ``SEAICE_deltaTevp`` (:math:`\Delta{T}_{\mathrm{EVP}}`), ``SEAICE_elasticParm`` (:math:`E_{0}`), or ``SEAICE_evpTauRelax`` (:math:`T`). :math:`\alpha` and :math:`\beta` determine the speed of convergence and the stability. Usually, it makes sense to use
:math:`\alpha = \beta`, and ``SEAICEnEVPstarSteps`` :math:`\gg (\alpha,\,\beta)` :cite:`kimmritz15`. Currently,
there is no termination criterion and the number of mEVP iterations is
fixed to ``SEAICEnEVPstarSteps``.

In order to use mEVP in the MITgcm, set ``SEAICEuseEVPstar = .TRUE.,``
in ``data.seaice``. If ``SEAICEuseEVPrev =.TRUE.,`` the actual form of
equations ([eq:evpstarsigma]) and ([eq:evpstarmom]) is used with fewer
implicit terms and the factor of :math:`e^{2}` dropped in the stress
equations ([eq:evpstresstensor2]) and
([eq:evpstresstensor12]). Although this modifies the original
EVP-equations, it turns out to improve convergence :cite:`bouillon13`.

Another variant is the aEVP scheme :cite:`kimmritz16`, where the value
of :math:`\alpha` is set dynamically based on the stability criterion

.. math::

   \label{eq:aevpalpha}
     \alpha = \beta = \max\left( \tilde{c}\pi\sqrt{c \frac{\zeta}{A_{c}}
       \frac{\Delta{t}}{\max(m,10^{-4}\text{\,kg})}},\alpha_{\min} \right)

with the grid cell area :math:`A_c` and the ice and snow mass :math:`m`.
This choice sacrifices speed of convergence for stability with the
result that aEVP converges quickly to VP where :math:`\alpha` can be
small and more slowly in areas where the equations are stiff. In
practice, aEVP leads to an overall better convergence than mEVP :cite:`kimmritz16`. To use aEVP in the MITgcm set ``SEAICEaEVPcoeff`` :math:`= \tilde{c}`; this also sets the default values of ``SEAICEaEVPcStar`` (:math:`c=4`) and ``SEAICEaEVPalphaMin`` (:math:`\alpha_{\min}=5`). Good convergence has been obtained with setting these values :cite:`kimmritz16`:
``SEAICEaEVPcoeff = 0.5, SEAICEnEVPstarSteps = 500, SEAICEuseEVPstar = .TRUE., SEAICEuseEVPrev = .TRUE.``

Note, that probably because of the C-grid staggering of velocities and
stresses, mEVP may not converge as successfully as in :cite:`kimmritz15`, and that convergence at very high resolution (order 5km) has not been studied yet.

.. _para_phys_pkg_seaice_TEM:

Truncated ellipse method (TEM) for yield curve
##############################################

In the so-called truncated ellipse method the shear viscosity :math:`\eta` is capped to suppress any tensile stress:

.. math::

   \label{eq:etatem}
     \eta = \min\left(\frac{\zeta}{e^2},
     \frac{\frac{P}{2}-\zeta(\dot{\epsilon}_{11}+\dot{\epsilon}_{22})}
     {\sqrt{\max(\Delta_{\min}^{2},(\dot{\epsilon}_{11}-\dot{\epsilon}_{22})^2
         +4\dot{\epsilon}_{12}^2})}\right).

To enable this method, set ``#define SEAICE_ALLOW_TEM`` in
``SEAICE_OPTIONS.h`` and turn it on with ``SEAICEuseTEM`` in ``data.seaice``.

.. _para_phys_pkg_seaice_iceoceanstress:

Ice-Ocean stress
################

Moving sea ice exerts a stress on the ocean which is the opposite of
the stress :math:`\mathbf{\tau}_{ocean}` in
Eq. :eq:`eq_momseaice`. This stess is applied directly to the surface
layer of the ocean model. An alternative ocean stress formulation is
given by :cite:`hibler87`. Rather than applying
:math:`\mathbf{\tau}_{ocean}` directly, the stress is derived from
integrating over the ice thickness to the bottom of the oceanic
surface layer. In the resulting equation for the *combined* ocean-ice
momentum, the interfacial stress cancels and the total stress appears
as the sum of windstress and divergence of internal ice stresses:
:math:`\delta(z) (\mathbf{\tau}_{air} + \mathbf{F})/\rho_0`, see alse
Eq. 2 of :cite:`hibler87`. The disadvantage of this formulation is
that now the velocity in the surface layer of the ocean that is used
to advect tracers, is really an average over the ocean surface
velocity and the ice velocity leading to an inconsistency as the ice
temperature and salinity are different from the oceanic variables. To
turn on the stress formulation of :cite:`hibler87`, set
``useHB87StressCoupling=.TRUE.``, in ``data.seaice``.

.. _para_phys_pkg_seaice_discretization:


Finite-volume discretization of the stress tensor divergence
############################################################

On an Arakawa C grid, ice thickness and concentration and thus ice
strength :math:`P` and bulk and shear viscosities :math:`\zeta` and
:math:`\eta` are naturally defined a C-points in the center of the grid
cell. Discretization requires only averaging of :math:`\zeta` and
:math:`\eta` to vorticity or Z-points (or :math:`\zeta`-points, but here
we use Z in order avoid confusion with the bulk viscosity) at the bottom
left corner of the cell to give :math:`\overline{\zeta}^{Z}` and
:math:`\overline{\eta}^{Z}`. In the following, the superscripts indicate
location at Z or C points, distance across the cell (F), along the cell
edge (G), between :math:`u`-points (U), :math:`v`-points (V), and
C-points (C). The control volumes of the :math:`u`- and
:math:`v`-equations in the grid cell at indices :math:`(i,j)` are
:math:`A_{i,j}^{w}` and :math:`A_{i,j}^{s}`, respectively. With these
definitions (which follow the model code documentation except that
:math:`\zeta`-points have been renamed to Z-points), the strain rates
are discretized as:

.. math::

   \begin{aligned}
     \dot{\epsilon}_{11} &= \partial_{1}{u}_{1} + k_{2}u_{2} \\ \notag
     => (\epsilon_{11})_{i,j}^C &= \frac{u_{i+1,j}-u_{i,j}}{\Delta{x}_{i,j}^{F}} 
      + k_{2,i,j}^{C}\frac{v_{i,j+1}+v_{i,j}}{2} \\ 
     \dot{\epsilon}_{22} &= \partial_{2}{u}_{2} + k_{1}u_{1} \\\notag
     => (\epsilon_{22})_{i,j}^C &= \frac{v_{i,j+1}-v_{i,j}}{\Delta{y}_{i,j}^{F}} 
      + k_{1,i,j}^{C}\frac{u_{i+1,j}+u_{i,j}}{2} \\ 
      \dot{\epsilon}_{12} = \dot{\epsilon}_{21} &= \frac{1}{2}\biggl(
      \partial_{1}{u}_{2} + \partial_{2}{u}_{1} - k_{1}u_{2} - k_{2}u_{1}
      \biggr) \\ \notag
     => (\epsilon_{12})_{i,j}^Z &= \frac{1}{2}
     \biggl( \frac{v_{i,j}-v_{i-1,j}}{\Delta{x}_{i,j}^V} 
      + \frac{u_{i,j}-u_{i,j-1}}{\Delta{y}_{i,j}^U} \\\notag
     &\phantom{=\frac{1}{2}\biggl(}
      - k_{1,i,j}^{Z}\frac{v_{i,j}+v_{i-1,j}}{2}
      - k_{2,i,j}^{Z}\frac{u_{i,j}+u_{i,j-1}}{2}
      \biggr),\end{aligned}

so that the diagonal terms of the strain rate tensor are naturally
defined at C-points and the symmetric off-diagonal term at Z-points.
No-slip boundary conditions (:math:`u_{i,j-1}+u_{i,j}=0` and
:math:`v_{i-1,j}+v_{i,j}=0` across boundaries) are implemented via
“ghost-points”; for free slip boundary conditions
:math:`(\epsilon_{12})^Z=0` on boundaries.

For a spherical polar grid, the coefficients of the metric terms are
:math:`k_{1}=0` and :math:`k_{2}=-\tan\phi/a`, with the spherical radius
:math:`a` and the latitude :math:`\phi`;
:math:`\Delta{x}_1 = \Delta{x} = a\cos\phi
\Delta\lambda`, and :math:`\Delta{x}_2 = \Delta{y}=a\Delta\phi`. For a
general orthogonal curvilinear grid, :math:`k_{1}` and :math:`k_{2}` can
be approximated by finite differences of the cell widths:

.. math::

   \begin{aligned}
     k_{1,i,j}^{C} &= \frac{1}{\Delta{y}_{i,j}^{F}}
     \frac{\Delta{y}_{i+1,j}^{G}-\Delta{y}_{i,j}^{G}}{\Delta{x}_{i,j}^{F}} \\
     k_{2,i,j}^{C} &= \frac{1}{\Delta{x}_{i,j}^{F}}
     \frac{\Delta{x}_{i,j+1}^{G}-\Delta{x}_{i,j}^{G}}{\Delta{y}_{i,j}^{F}} \\
     k_{1,i,j}^{Z} &= \frac{1}{\Delta{y}_{i,j}^{U}}
     \frac{\Delta{y}_{i,j}^{C}-\Delta{y}_{i-1,j}^{C}}{\Delta{x}_{i,j}^{V}} \\
     k_{2,i,j}^{Z} &= \frac{1}{\Delta{x}_{i,j}^{V}}
     \frac{\Delta{x}_{i,j}^{C}-\Delta{x}_{i,j-1}^{C}}{\Delta{y}_{i,j}^{U}}\end{aligned}

The stress tensor is given by the constitutive viscous-plastic relation
:math:`\sigma_{\alpha\beta} = 2\eta\dot{\epsilon}_{\alpha\beta} +
[(\zeta-\eta)\dot{\epsilon}_{\gamma\gamma} - P/2
]\delta_{\alpha\beta}` . The stress tensor divergence
:math:`(\nabla\sigma)_{\alpha} = \partial_\beta\sigma_{\beta\alpha}`, is
discretized in finite volumes . This conveniently avoids dealing with
further metric terms, as these are “hidden” in the differential cell
widths. For the :math:`u`-equation (:math:`\alpha=1`) we have:

.. math::

   \begin{aligned}
     (\nabla\sigma)_{1}: \phantom{=}&
     \frac{1}{A_{i,j}^w}
     \int_{\mathrm{cell}}(\partial_1\sigma_{11}+\partial_2\sigma_{21})\,dx_1\,dx_2
     \\\notag
     =& \frac{1}{A_{i,j}^w} \biggl\{
     \int_{x_2}^{x_2+\Delta{x}_2}\sigma_{11}dx_2\biggl|_{x_{1}}^{x_{1}+\Delta{x}_{1}}
     + \int_{x_1}^{x_1+\Delta{x}_1}\sigma_{21}dx_1\biggl|_{x_{2}}^{x_{2}+\Delta{x}_{2}}
     \biggr\} \\ \notag
     \approx& \frac{1}{A_{i,j}^w} \biggl\{
     \Delta{x}_2\sigma_{11}\biggl|_{x_{1}}^{x_{1}+\Delta{x}_{1}}
     + \Delta{x}_1\sigma_{21}\biggl|_{x_{2}}^{x_{2}+\Delta{x}_{2}}
     \biggr\} \\ \notag
     =& \frac{1}{A_{i,j}^w} \biggl\{
     (\Delta{x}_2\sigma_{11})_{i,j}^C -
     (\Delta{x}_2\sigma_{11})_{i-1,j}^C 
     \\\notag
     \phantom{=}& \phantom{\frac{1}{A_{i,j}^w} \biggl\{}
     + (\Delta{x}_1\sigma_{21})_{i,j+1}^Z - (\Delta{x}_1\sigma_{21})_{i,j}^Z
     \biggr\}\end{aligned}

with

.. math::

   \begin{aligned}
     (\Delta{x}_2\sigma_{11})_{i,j}^C =& \phantom{+}
     \Delta{y}_{i,j}^{F}(\zeta + \eta)^{C}_{i,j}
     \frac{u_{i+1,j}-u_{i,j}}{\Delta{x}_{i,j}^{F}} \\ \notag
     &+ \Delta{y}_{i,j}^{F}(\zeta + \eta)^{C}_{i,j}
     k_{2,i,j}^C \frac{v_{i,j+1}+v_{i,j}}{2} \\ \notag
     \phantom{=}& + \Delta{y}_{i,j}^{F}(\zeta - \eta)^{C}_{i,j}
     \frac{v_{i,j+1}-v_{i,j}}{\Delta{y}_{i,j}^{F}} \\ \notag
     \phantom{=}& + \Delta{y}_{i,j}^{F}(\zeta - \eta)^{C}_{i,j}
     k_{1,i,j}^{C}\frac{u_{i+1,j}+u_{i,j}}{2} \\ \notag
     \phantom{=}& - \Delta{y}_{i,j}^{F} \frac{P}{2} \\
     (\Delta{x}_1\sigma_{21})_{i,j}^Z =& \phantom{+}
     \Delta{x}_{i,j}^{V}\overline{\eta}^{Z}_{i,j}
     \frac{u_{i,j}-u_{i,j-1}}{\Delta{y}_{i,j}^{U}} \\ \notag
     & + \Delta{x}_{i,j}^{V}\overline{\eta}^{Z}_{i,j}
     \frac{v_{i,j}-v_{i-1,j}}{\Delta{x}_{i,j}^{V}} \\ \notag
     & - \Delta{x}_{i,j}^{V}\overline{\eta}^{Z}_{i,j} 
     k_{2,i,j}^{Z}\frac{u_{i,j}+u_{i,j-1}}{2} \\ \notag
     & - \Delta{x}_{i,j}^{V}\overline{\eta}^{Z}_{i,j} 
     k_{1,i,j}^{Z}\frac{v_{i,j}+v_{i-1,j}}{2}\end{aligned}

Similarly, we have for the :math:`v`-equation (:math:`\alpha=2`):

.. math::

   \begin{aligned}
     (\nabla\sigma)_{2}: \phantom{=}&
     \frac{1}{A_{i,j}^s}
     \int_{\mathrm{cell}}(\partial_1\sigma_{12}+\partial_2\sigma_{22})\,dx_1\,dx_2 
     \\\notag
     =& \frac{1}{A_{i,j}^s} \biggl\{
     \int_{x_2}^{x_2+\Delta{x}_2}\sigma_{12}dx_2\biggl|_{x_{1}}^{x_{1}+\Delta{x}_{1}}
     + \int_{x_1}^{x_1+\Delta{x}_1}\sigma_{22}dx_1\biggl|_{x_{2}}^{x_{2}+\Delta{x}_{2}}
     \biggr\} \\ \notag
     \approx& \frac{1}{A_{i,j}^s} \biggl\{
     \Delta{x}_2\sigma_{12}\biggl|_{x_{1}}^{x_{1}+\Delta{x}_{1}}
     + \Delta{x}_1\sigma_{22}\biggl|_{x_{2}}^{x_{2}+\Delta{x}_{2}}
     \biggr\} \\ \notag
     =& \frac{1}{A_{i,j}^s} \biggl\{
     (\Delta{x}_2\sigma_{12})_{i+1,j}^Z - (\Delta{x}_2\sigma_{12})_{i,j}^Z
     \\ \notag
     \phantom{=}& \phantom{\frac{1}{A_{i,j}^s} \biggl\{}
     + (\Delta{x}_1\sigma_{22})_{i,j}^C - (\Delta{x}_1\sigma_{22})_{i,j-1}^C
     \biggr\} \end{aligned}

with

.. math::

   \begin{aligned}
     (\Delta{x}_1\sigma_{12})_{i,j}^Z =& \phantom{+}
     \Delta{y}_{i,j}^{U}\overline{\eta}^{Z}_{i,j}
     \frac{u_{i,j}-u_{i,j-1}}{\Delta{y}_{i,j}^{U}} 
     \\\notag &
     + \Delta{y}_{i,j}^{U}\overline{\eta}^{Z}_{i,j}
     \frac{v_{i,j}-v_{i-1,j}}{\Delta{x}_{i,j}^{V}} \\\notag
     &- \Delta{y}_{i,j}^{U}\overline{\eta}^{Z}_{i,j}
     k_{2,i,j}^{Z}\frac{u_{i,j}+u_{i,j-1}}{2} 
     \\\notag &
     - \Delta{y}_{i,j}^{U}\overline{\eta}^{Z}_{i,j}
     k_{1,i,j}^{Z}\frac{v_{i,j}+v_{i-1,j}}{2} \\ \notag
     (\Delta{x}_2\sigma_{22})_{i,j}^C =& \phantom{+}
     \Delta{x}_{i,j}^{F}(\zeta - \eta)^{C}_{i,j}
     \frac{u_{i+1,j}-u_{i,j}}{\Delta{x}_{i,j}^{F}} \\ \notag
     &+ \Delta{x}_{i,j}^{F}(\zeta - \eta)^{C}_{i,j}
     k_{2,i,j}^{C} \frac{v_{i,j+1}+v_{i,j}}{2} \\ \notag
     & + \Delta{x}_{i,j}^{F}(\zeta + \eta)^{C}_{i,j}
     \frac{v_{i,j+1}-v_{i,j}}{\Delta{y}_{i,j}^{F}} \\ \notag
     & + \Delta{x}_{i,j}^{F}(\zeta + \eta)^{C}_{i,j}
     k_{1,i,j}^{C}\frac{u_{i+1,j}+u_{i,j}}{2} \\ \notag
     & -\Delta{x}_{i,j}^{F} \frac{P}{2}\end{aligned}

Again, no slip boundary conditions are realized via ghost points and
:math:`u_{i,j-1}+u_{i,j}=0` and :math:`v_{i-1,j}+v_{i,j}=0` across
boundaries. For free slip boundary conditions the lateral stress is set
to zeros. In analogy to :math:`(\epsilon_{12})^Z=0` on boundaries, we
set :math:`\sigma_{21}^{Z}=0`, or equivalently :math:`\eta_{i,j}^{Z}=0`,
on boundaries.

.. _para_phys_pkg_seaice_thermodynamics:

Thermodynamics
##############

| ``**NOTE: THIS SECTION IS TERRIBLY OUT OF DATE**``

In its original formulation the sea ice model uses simple
thermodynamics following the appendix of :cite:`sem76`. This
formulation does not allow storage of heat, that is, the heat capacity
of ice is zero. Upward conductive heat flux is parameterized assuming
a linear temperature profile and together with a constant ice
conductivity. It is expressed as :math:`(K/h)(T_{w}-T_{0})`, where
:math:`K` is the ice conductivity, :math:`h` the ice thickness, and
:math:`T_{w}-T_{0}` the difference between water and ice surface
temperatures. This type of model is often refered to as a “zero-layer”
model. The surface heat flux is computed in a similar way to that of
and .

The conductive heat flux depends strongly on the ice thickness
:math:`h`. However, the ice thickness in the model represents a mean
over a potentially very heterogeneous thickness distribution. In order
to parameterize a sub-grid scale distribution for heat flux
computations, the mean ice thickness :math:`h` is split into :math:`N`
thickness categories :math:`H_{n}` that are equally distributed between
:math:`2h` and a minimum imposed ice thickness of :math:`5\text{\,cm}`
by :math:`H_n= \frac{2n-1}{7}\,h` for :math:`n\in[1,N]`. The heat fluxes
computed for each thickness category is area-averaged to give the total
heat flux :cite:`hibler84`. To use this thickness category parameterization set ``SEAICE_multDim`` to the number of desired categories in ``data.seaice`` (7 is a good guess, for anything larger than 7 modify ``SEAICE_SIZE.h``); note that this requires different restart files and switching this flag on in the middle of an integration is not advised. In order to include the same distribution for snow, set ``SEAICE_useMultDimSnow = .TRUE.``; only then, the parameterization of always having a fraction of thin ice is efficient and generally thicker ice is produce :cite:`castro-morales14`.

The atmospheric heat flux is balanced by an oceanic heat flux from
below. The oceanic flux is proportional to
:math:`\rho\,c_{p}\left(T_{w}-T_{fr}\right)` where :math:`\rho` and
:math:`c_{p}` are the density and heat capacity of sea water and
:math:`T_{fr}` is the local freezing point temperature that is a
function of salinity. This flux is not assumed to instantaneously melt
or create ice, but a time scale of three days (run-time parameter ``SEAICE_gamma_t``) is used to relax :math:`T_{w}` to the freezing point. The parameterization of lateral and vertical growth of sea ice follows that of  :cite:`hib79,hib80`; the so-called lead closing parameter :math:`h_{0}` (run-time parameter ``HO``) has
a default value of 0.5 meters.

On top of the ice there is a layer of snow that modifies the heat flux
and the albedo :cite:`zha98a`. Snow modifies the effective conductivity according to

.. math:: \frac{K}{h} \rightarrow \frac{1}{\frac{h_{s}}{K_{s}}+\frac{h}{K}},

where :math:`K_s` is the conductivity of snow and :math:`h_s` the snow
thickness. If enough snow accumulates so that its weight submerges the
ice and the snow is flooded, a simple mass conserving parameterization
of snowice formation (a flood-freeze algorithm following Archimedes’
principle) turns snow into ice until the ice surface is back at
:math:`z=0` :cite:`leppaeranta83`. The flood-freeze algorithm is enabled with the CPP-flag ``SEAICE_ALLOW_FLOODDING`` and turned on with run-time parameter ``SEAICEuseFlooding=.TRUE.``.

.. _para_phys_pkg_seaice_advection:

Advection of thermodynamic variables
####################################

Effective ice thickness (ice volume per unit area, :math:`c\cdot{h}`),
concentration :math:`c` and effective snow thickness
(:math:`c\cdot{h}_{s}`) are advected by ice velocities:

.. math::

   \label{eq:advection}
     \frac{\partial{X}}{\partial{t}} = - \nabla\cdot\left({{\vec{\mathbf{u}}}}\,X\right) +
     \Gamma_{X} + D_{X}

where :math:`\Gamma_X` are the thermodynamic source terms and
:math:`D_{X}` the diffusive terms for quantities
:math:`X=(c\cdot{h}), c, (c\cdot{h}_{s})`. From the various advection
scheme that are available in the MITgcm, we recommend flux-limited
schemes to preserve sharp gradients and edges that are typical of sea
ice distributions and to rule out unphysical over- and undershoots
(negative thickness or concentration). These schemes conserve volume and
horizontal area and are unconditionally stable, so that we can set
:math:`D_{X}=0`. Run-timeflags: ``SEAICEadvScheme ``(default=2, is the historic 2nd-order, centered difference scheme), ``DIFF`` = :math:`D_{X}/\Delta{x}` (default=0.004).

The MITgcm sea ice model provides the option to use the thermodynamics
model of :cite:`win00`, which in turn is based on the 3-layer model of
:cite:`sem76` and which treats brine content by means of enthalpy
conservation; the corresponding package ``thsice`` is described in
section [sec:pkg:thsice]. This scheme requires additional state
variables, namely the enthalpy of the two ice layers (instead of
effective ice salinity), to be advected by ice velocities. The
internal sea ice temperature is inferred from ice enthalpy. To avoid
unphysical (negative) values for ice thickness and concentration, a
positive 2nd-order advection scheme with a SuperBee flux limiter
:cite:`roe:85` should be used to advect all sea-ice-related quantities
of the :cite:`win00` thermodynamic model (runtime flag
``thSIceAdvScheme=77`` and ``thSIce_diffK`` =\ :math:`D_{X}`\ =0 in
``data.ice``, defaults are 0). Because of the non-linearity of the
advection scheme, care must be taken in advecting these quantities:
when simply using ice velocity to advect enthalpy, the total energy
(i.e., the volume integral of enthalpy) is not
conserved. Alternatively, one can advect the energy content (i.e.,
product of ice-volume and enthalpy) but then false enthalpy extrema
can occur, which then leads to unrealistic ice temperature. In the
currently implemented solution, the sea-ice mass flux is used to
advect the enthalpy in order to ensure conservation of enthalpy and to
prevent false enthalpy extrema.

.. _para_phys_pkg_seaice_subroutines:

Key subroutines
###############

Top-level routine: ``seaice_model.F``

::


    C     !CALLING SEQUENCE:
    c ...
    c  seaice_model (TOP LEVEL ROUTINE)
    c  |
    c  |-- #ifdef SEAICE_CGRID
    c  |     SEAICE_DYNSOLVER
    c  |     |
    c  |     |-- < compute proxy for geostrophic velocity >
    c  |     |
    c  |     |-- < set up mass per unit area and Coriolis terms >
    c  |     |
    c  |     |-- < dynamic masking of areas with no ice >
    c  |     |
    c  |     |
    c  |   #ELSE
    c  |     DYNSOLVER
    c  |   #ENDIF
    c  |
    c  |-- if ( useOBCS ) 
    c  |     OBCS_APPLY_UVICE
    c  |
    c  |-- if ( SEAICEadvHeff .OR. SEAICEadvArea .OR. SEAICEadvSnow .OR. SEAICEadvSalt )
    c  |     SEAICE_ADVDIFF
    c  |
    c  |   SEAICE_REG_RIDGE
    c  |
    c  |-- if ( usePW79thermodynamics ) 
    c  |     SEAICE_GROWTH
    c  |
    c  |-- if ( useOBCS ) 
    c  |     if ( SEAICEadvHeff ) OBCS_APPLY_HEFF
    c  |     if ( SEAICEadvArea ) OBCS_APPLY_AREA
    c  |     if ( SEAICEadvSALT ) OBCS_APPLY_HSALT
    c  |     if ( SEAICEadvSNOW ) OBCS_APPLY_HSNOW
    c  |
    c  |-- < do various exchanges >
    c  |
    c  |-- < do additional diagnostics >
    c  |
    c  o

.. _para_phys_pkg_seaice_diagnostics:

SEAICE diagnostics
##################

Diagnostics output is available via the diagnostics package (see Section
[sec:pkg:diagnostics]). Available output fields are summarized in Table
[tab:pkg:seaice:diagnostics].

Experiments and tutorials that use seaice
#########################################

- Labrador Sea experiment in ``lab_sea`` verification directory. }
- ``seaice_obcs``, based on ``lab_sea``
- ``offline_exf_seaice/input.seaicetd``, based on ``lab_sea``
- ``global_ocean.cs32x15/input.icedyn`` and ``global_ocean.cs32x15/input.seaice``, global cubed-sphere-experiment with combinations of ``seaice`` and ``thsice``


