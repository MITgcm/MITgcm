.. _sub_phys_pkg_seaice:

SEAICE Package
**************

Authors: Martin Losch, Dimitris Menemenlis, An Nguyen, Jean-Michel
Campin, Patrick Heimbach, Chris Hill, Jinlun Zhang, and Damien Ringeisen

.. _ssub_phys_pkg_seaice_intro:

Introduction
============

Package :filelink:`seaice <pkg/seaice>` provides a dynamic and thermodynamic
interactive sea ice model.

CPP options enable or disable different aspects of the package
(:numref:`ssub_phys_pkg_seaice_config`). Run-time options, flags, filenames and
field-related dates/times are set in ``data.seaice``
(:numref:`ssub_phys_pkg_seaice_runtime`).  A description of key subroutines is
given in :numref:`ssub_phys_pkg_seaice_subroutines`.  Available diagnostics
output is listed in :numref:`ssub_phys_pkg_seaice_diagnostics`.

.. _ssub_phys_pkg_seaice_config:

SEAICE configuration and compiling
==================================

Compile-time options
--------------------

As with all MITgcm packages, SEAICE can be turned on or off at compile time
(see :numref:`building_code`)

- using the ``packages.conf`` file by adding ``seaice`` to it

- or using :filelink:`genmake2 <tools/genmake2>` adding ``-enable=seaice`` or
  ``-disable=seaice`` switches

- **required packages and CPP options**:
  :filelink:`seaice <pkg/seaice>` requires the external forcing package
  :filelink:`pkg/exf` to be enabled; no additional CPP options are required.


Parts of the :filelink:`seaice <pkg/seaice>` code can be enabled or disabled at
compile time via CPP preprocessor flags. These options are set in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>`.
:numref:`tab_phys_pkg_seaice_cpp` summarizes the most important ones. For more
options see :filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>`.

.. tabularcolumns:: |\Y{.375}|\Y{.1}|\Y{.55}|

.. csv-table:: Some of the most relevant CPP preprocessor flags in the :filelink:`seaice <pkg/seaice>` package.
   :header: "CPP option", "Default", Description"
   :widths: 30, 10, 60
   :name: tab_phys_pkg_seaice_cpp

   :varlink:`SEAICE_DEBUG`, #undef, enhance STDOUT for debugging
   :varlink:`SEAICE_ALLOW_DYNAMICS`, #define, sea ice dynamics code
   :varlink:`SEAICE_CGRID`, #define, LSR solver on C-grid (rather than original B-grid)
   :varlink:`SEAICE_ALLOW_EVP`, #define, enable use of EVP rheology solver
   :varlink:`SEAICE_ALLOW_JFNK`, #define, enable use of JFNK rheology solver
   :varlink:`SEAICE_ALLOW_KRYLOV`, #define, enable use of Krylov rheology solver
   :varlink:`SEAICE_ALLOW_TEM`, #undef, enable use of the truncated ellipse method (TEM) and coulombic yield curve
   :varlink:`SEAICE_ALLOW_MCS`, #undef, enable use of Mohr-Coulomb yield curve with shear flow rule
   :varlink:`SEAICE_ALLOW_MCE`, #undef, enable use of Mohr-Coulomb yield curve with elliptical plastic potential
   :varlink:`SEAICE_ALLOW_TD`, #undef, enable use of teardrop and parabolic Lens yield curves with normal flow rules
   :varlink:`SEAICE_LSR_ZEBRA`, #undef, use a coloring method for LSR solver
   :varlink:`SEAICE_EXTERNAL_FLUXES`, #define, use :filelink:`pkg/exf`-computed fluxes as starting point
   :varlink:`SEAICE_ZETA_SMOOTHREG`, #define, use differentiable regularization for viscosities
   :varlink:`SEAICE_DELTA_SMOOTHREG`, #undef, use differentiable regularization for :math:`1/\Delta`
   :varlink:`SEAICE_ALLOW_BOTTOMDRAG`, #undef, enable grounding parameterization for improved fastice in shallow seas
   :varlink:`SEAICE_ITD`, #undef, run with dynamical sea Ice Thickness Distribution (ITD)
   :varlink:`SEAICE_VARIABLE_SALINITY`, #undef, enable sea ice with variable salinity
   :varlink:`SEAICE_CAP_ICELOAD`, #undef, enable to limit seaice load (:varlink:`siceLoad`) on the sea surface
   :varlink:`ALLOW_SITRACER`, #undef, enable sea ice tracer package
   :varlink:`SEAICE_BICE_STRESS`, #undef, B-grid only for backward compatiblity: turn on ice-stress on ocean
   :varlink:`EXPLICIT_SSH_SLOPE`, #undef, B-grid only for backward compatiblity: use ETAN for tilt computations rather than geostrophic velocities

.. _ssub_phys_pkg_seaice_runtime:

Run-time parameters
===================

Run-time parameters (see :numref:`tab_phys_pkg_seaice_runtimeparms`) are set in
``data.seaice`` (read in :filelink:`pkg/seaice/seaice_readparms.F`).

Enabling the package
--------------------

:filelink:`seaice <pkg/seaice>` package is switched on/off at run-time by
setting :varlink:`useSEAICE` ``= .TRUE.,`` in ``data.pkg``.

General flags and parameters
----------------------------

:numref:`tab_phys_pkg_seaice_runtimeparms` lists most run-time parameters.

.. tabularcolumns:: |\Y{.275}|\Y{.20}|\Y{.525}|

.. table:: Run-time parameters and default values
  :class: longtable
  :name: tab_phys_pkg_seaice_runtimeparms

  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  |   Name                             |      Default value           |   Description                                                           |
  +====================================+==============================+=========================================================================+
  | :varlink:`SEAICEwriteState`        |     FALSE                    | write sea ice state to file                                             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseDYNAMICS`       |     TRUE                     | use dynamics                                                            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseJFNK`           |     FALSE                    | use the JFNK-solver                                                     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseTEM`            |     FALSE                    | use truncated ellipse method or coulombic yield curve                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseMCS`            |     FALSE                    | use the Mohr-Coulomb yield curve with shear flow rule                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseMCE`            |     FALSE                    | use the Mohr-Coulomb yield curve with elliptical plastic potential      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseTD`             |     FALSE                    | use the teardrop yield curve with normal flow rule                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEusePL`             |     FALSE                    | use the parabolic Lens yield curve with normal flow rule                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseStrImpCpl`      |     FALSE                    | use strength implicit coupling in LSR/JFNK                              |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseMetricTerms`    |     TRUE                     | use metric terms in dynamics                                            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseEVPpickup`      |     TRUE                     | use EVP pickups                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseFluxForm`       |     TRUE                     | use flux form for 2nd central difference advection scheme               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICErestoreUnderIce`   |     FALSE                    | enable restoring to climatology under ice                               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEupdateOceanStress` |     TRUE                     | update ocean surface stress accounting for sea ice cover                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEscaleSurfStress`   |     TRUE                     | scale atmosphere and ocean-surface stress on ice by concentration (AREA)|
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEaddSnowMass`       |     TRUE                     | in computing seaiceMass, add snow contribution                          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`useHB87stressCoupling`   |     FALSE                    | turn on ice-ocean stress coupling following                             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`usePW79thermodynamics`   |     TRUE                     | flag to turn off zero-layer-thermodynamics for testing                  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEadvHeff`           |     TRUE                     | flag to turn off advection of scalar variable :varlink:`HEFF`           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEadvArea`           |     TRUE                     | flag to turn off advection of scalar variable :varlink:`AREA`           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEadvSnow`           |     TRUE                     | flag to turn off advection of scalar variable :varlink:`HSNOW`          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEadvSalt`           |     TRUE                     | flag to turn off advection of scalar variable :varlink:`HSALT`          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEadvScheme`         | 77                           | set advection scheme for seaice scalar state variables                  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseFlooding`       | TRUE                         | use flood-freeze algorithm                                              |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_no_slip`          | FALSE                        | use no-slip boundary conditions instead of free-slip                    |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_deltaTtherm`      | :varlink:`dTtracerLev` (1)   | time step for seaice thermodynamics (s)                                 |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_deltaTdyn`        | :varlink:`dTtracerLev` (1)   | time step for seaice dynamics (s)                                       |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_deltaTevp`        | 0.0                          | EVP sub-cycling time step (s); values :math:`>` 0 turn on EVP           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseEVPstar`        | TRUE                         | use modified EVP\* instead of EVP, following :cite:`lemieux:12`         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseEVPrev`         | TRUE                         | "revisited form" variation on EVP\*, following :cite:`bouillon:13`      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEnEVPstarSteps`     | unset                        | number of modified EVP\* iterations                                     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_evpAlpha`         | unset                        | EVP\* parameter (non-dim.), to replace                                  |
  |                                    |                              | 2*\ :varlink:`SEAICE_evpTauRelax`\ /\ :varlink:`SEAICE_deltaTevp`       |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_evpBeta`          | unset                        | EVP\* parameter (non-dim.), to replace                                  |
  |                                    |                              | :varlink:`SEAICE_deltaTdyn`\ /\ :varlink:`SEAICE_deltaTevp`             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEaEVPcoeff`         | unset                        | largest stabilized frequency for adaptive EVP (non-dim.)                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEaEVPcStar`         | 4.0                          | aEVP multiple of stability factor (non-dim.), see :cite:`kimmritz:16`   |
  |                                    |                              | :math:`\alpha * \beta = c^\ast * \gamma`                                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEaEVPalphaMin`      | 5.0                          | aEVP lower limit of alpha and beta (non-dim.), see :cite:`kimmritz:16`  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_elasticParm`      | 0.33333333                   | EVP parameter :math:`E_0` (non-dim.), sets relaxation timescale         |
  |                                    |                              | :varlink:`SEAICE_evpTauRelax` =                                         |
  |                                    |                              | :varlink:`SEAICE_elasticParm` * :varlink:`SEAICE_deltaTdyn`             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_evpTauRelax`      | :varlink:`dTtracerLev` (1) * | relaxation time scale :math:`T` for EVP waves (s)                       |
  |                                    | :varlink:`SEAICE_elasticParm`|                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_OLx`              | :varlink:`OLx` - 2           | overlap for LSR-solver or preconditioner, :math:`x`-dimension           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_OLy`              | :varlink:`OLy` - 2           | overlap for LSR-solver or preconditioner, :math:`y`-dimension           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEnonLinIterMax`     | 2/10                         |  maximum number of non-linear (outer loop) iterations (LSR/JFNK)        |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICElinearIterMax`     | 1500/10                      | maximum number of linear iterations (LSR/JFNK)                          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_JFNK_lsIter`      | (off)                        | start line search after “lsIter” Newton iterations                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_JFNK_lsLmax`      | 4                            | maximum number of line search steps                                     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_JFNK_lsGamma`     | 0.5                          | line search step size parameter                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEnonLinTol`         | 1.0E-05                      | non-linear tolerance parameter for JFNK solver                          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`JFNKgamma_lin_min`       | 0.10                         | minimum tolerance parameter for linear JFNK solver                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`JFNKgamma_lin_max`       | 0.99                         | maximum tolerance parameter for linear JFNK solver                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`JFNKres_tFac`            | unset                        | tolerance parameter for FGMRES residual                                 |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_JFNKepsilon`      | 1.0E-06                      | step size for the FD-gradient in s/r seaice_jacvec                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_dumpFreq`         | dumpFreq                     | dump frequency (s)                                                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_dump_mdsio`       | TRUE                         | write snapshot using :filelink:`/pkg/mdsio`                             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_dump_mnc`         | FALSE                        | write snapshot using :filelink:`/pkg/mnc`                               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_initialHEFF`      | 0.0                          | initial sea ice thickness averaged over grid cell (m)                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_drag`             | 1.0E-03                      | air-ice drag coefficient (non-dim.)                                     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`OCEAN_drag`              | 1.0E-03                      | air-ocean drag coefficient (non-dim.)                                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_waterDrag`        | 5.5E-03                      | water-ice drag coefficient (non-dim.)                                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_dryIceAlb`        | 0.75                         | winter sea ice albedo                                                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_wetIceAlb`        | 0.66                         | summer sea ice albedo                                                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_drySnowAlb`       | 0.84                         | dry snow albedo                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_wetSnowAlb`       | 0.70                         | wet snow albedo                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_waterAlbedo`      | 0.10                         | water albedo (not used if #define :varlink:`SEAICE_EXTERNAL_FLUXES`)    |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_strength`         | 2.75E+04                     | sea ice strength constant :math:`P^{\ast}`  (N/m\ :sup:`2`)             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_cStar`            | 20.0                         | sea ice strength constant :math:`C^{\ast}`  (non-dim.)                  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_eccen`            | 2.0                          | VP rheology ellipse aspect ratio :math:`e`                              |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_eccfr`            | = :varlink:`SEAICE_eccen`    | sea ice plastic potential ellipse aspect ratio :math:`e_G`              |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEmcMU`              | 1.0                          | slope of the Mohr-Coulomb yield curve                                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEpressReplFac`      | 1.0                          | use replacement pressure (0.0-1.0)                                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_tensilFac`        | 0.0                          | tensile factor for the yield curve                                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_rhoAir`           | 1.3  (or                     | density of air (kg/m\ :sup:`3`)                                         |
  |                                    | :filelink:`pkg/exf` value)   |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_cpAir`            | 1004.0 (or                   | specific heat of air (J/kg/K)                                           |
  |                                    | :filelink:`pkg/exf` value)   |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_lhEvap`           | 2.5E+06 (or                  | latent heat of evaporation (J/kg)                                       |
  |                                    | :filelink:`pkg/exf` value)   |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_lhFusion`         | 3.34E+05 (or                 | latent heat of fusion (J/kg)                                            |
  |                                    | :filelink:`pkg/exf` value)   |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_dalton`           | 1.75E-03                     | ice-ocean transfer coefficient for latent and sensible heat (non-dim.)  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`useMaykutSatVapPoly`     | FALSE                        | use Maykut polynomial to compute saturation vapor pressure              |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_iceConduct`       | 2.16560E+00                  | sea ice conductivity  (W m\ :sup:`-1` K\ :sup:`-1`)                     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_snowConduct`      | 3.10000E-01                  | snow conductivity (W m\ :sup:`-1` K\ :sup:`-1`)                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_emissivity`       | 0.970018 (or                 | longwave ocean surface emissivity (non-dim.)                            |
  |                                    | :filelink:`pkg/exf` value)   |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_snowThick`        | 0.15                         | cutoff snow thickness to use snow albedo (m)                            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_shortwave`        | 0.30                         | ice penetration shortwave radiation factor (non-dim.)                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_saltFrac`         | 0.0                          | salinity newly formed ice (as fraction of ocean surface salinity)       |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_frazilFrac`       | 1.0  (or                     | frazil to sea ice conversion rate, as fraction                          |
  |                                    | computed from other parms)   | (relative to the local freezing point of sea ice water)                 |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEstressFactor`      | 1.0                          | scaling factor for ice area in computing total ocean stress (non-dim.)  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`HeffFile`                | unset                        | filename for initial sea ice eff. thickness field :varlink:`HEFF` (m)   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`AreaFile`                | unset                        | filename for initial fraction sea ice cover :varlink:`AREA` (non-dim.)  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`HsnowFile`               | unset                        | filename for initial eff. snow thickness field :varlink:`HSNOW` (m)     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`HsaltFile`               | unset                        | filename for initial eff. sea ice salinity field :varlink:`HSALT`       |
  |                                    |                              | (g/m\ :sup:`2`)                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`LSR_ERROR`               | 1.0E-05                      | sets accuracy of LSR solver                                             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`DIFF1`                   | 0.0                          | parameter used in advect.F                                              |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`HO`                      | 0.5                          | lead closing parameter :math:`h_0` (m); demarcation thickness between   |
  |                                    |                              | thick and thin ice which determines partition between vertical and      |
  |                                    |                              | lateral ice growth                                                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`MIN_ATEMP`               | -50.0                        | minimum air temperature (:sup:`o`\ C)                                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`MIN_LWDOWN`              | 60.0                         | minimum downward longwave (W/m\ :sup:`2`)                               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`MIN_TICE`                | -50.0                        | minimum ice temperature (:sup:`o`\ C)                                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`IMAX_TICE`               | 10                           | number of iterations for ice surface temperature solution               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_EPS`              | 1.0E-10                      | a "small number" used in various routines                               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_area_reg`         | 1.0E-5                       | minimum concentration to regularize ice thickness                       |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_hice_reg`         | 0.05                         | minimum ice thickness (m) for regularization                            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_multDim`          | 1                            | number of ice categories for thermodynamics                             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_useMultDimSnow`   | TRUE                         | use same fixed pdf for snow as for multi-thickness-category ice         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+


The following dynamical ice thickness distribution and ridging parameters in
:numref:`tab_phys_pkg_seaice_ridging` are only active with #define
:varlink:`SEAICE_ITD`.  All parameters are non-dimensional unless indicated.

.. tabularcolumns:: |\Y{.275}|\Y{.20}|\Y{.525}|

.. table:: Thickness distribution and ridging parameters
  :name: tab_phys_pkg_seaice_ridging


  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  |   Name                             |      Default value           |   Description                                                           |
  +====================================+==============================+=========================================================================+
  | :varlink:`useHibler79IceStrength`  | TRUE                         | use :cite:`hibler:79` ice strength; do not use :cite:`rothrock:75`      |
  |                                    |                              | with #define :varlink:`SEAICE_ITD`                                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEsimpleRidging`     | TRUE                         | use simple ridging a la :cite:`hibler:79`                               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICE_cf`               | 17.0                         | scaling parameter of :cite:`rothrock:75` ice strength parameterization  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEpartFunc`          | 0                            | use partition function of :cite:`thorndike:75`                          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEredistFunc`        | 0                            | use redistribution function of :cite:`hibler:80`                        |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEridgingIterMax`    | 10                           | maximum number of ridging sweeps                                        |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEshearParm`         | 0.5                          | fraction of shear to be used for ridging                                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEgStar`             | 0.15                         | max. ice conc. that participates in ridging :cite:`thorndike:75`        |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEhStar`             | 25.0                         | ridging parameter for :cite:`thorndike:75`, :cite:`lipscomb:07`         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEaStar`             | 0.05                         | similar to :varlink:`SEAICEgStar` for                                   |
  |                                    |                              | :cite:`lipscomb:07` participation function                              |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEmuRidging`         | 3.0                          | similar to :varlink:`SEAICEhStar` for                                   |
  |                                    |                              | :cite:`lipscomb:07` ridging function                                    |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEmaxRaft`           | 1.0                          | regularization parameter for rafting                                    |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEsnowFracRidge`     | 0.5                          | fraction of snow that remains on ridged ice                             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`SEAICEuseLinRemapITD`    | TRUE                         | use linear remapping scheme of :cite:`lipscomb:01`                      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`Hlimit`                  | unset                        | nITD+1-array of ice thickness category limits (m)                       |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`Hlimit_c1`,              | 3.0,                         | when :varlink:`Hlimit` is not set, then these parameters                |
  | :varlink:`Hlimit_c2`,              | 15.0,                        | determine :varlink:`Hlimit` from a simple function                      |
  | :varlink:`Hlimit_c3`               | 3.0                          | following :cite:`lipscomb:01`                                           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+


.. _ssub_phys_pkg_seaice_descr:

Description
===========

The MITgcm sea ice model is based on a variant of the viscous-plastic (VP)
dynamic-thermodynamic sea ice model (Zhang and Hibler 1997 :cite:`zhang:97`)
first introduced in Hibler (1979) and Hibler (1980)
:cite:`hibler:79,hibler:80`.  In order to adapt this model to the requirements
of coupled ice-ocean state estimation, many important aspects of the original
code have been modified and improved, see Losch et al. (2010) :cite:`losch:10`:

-  the code has been rewritten for an Arakawa C-grid, both B- and C-grid
   variants are available; the C-grid code allows for no-slip and free-slip
   lateral boundary conditions;

-  three different solution methods for solving the nonlinear momentum
   equations have been adopted: LSOR (Zhang and Hibler 1997 :cite:`zhang:97`),
   EVP (Hunke and Dukowicz 1997 :cite:`hunke:97`),
   JFNK (Lemieux et al. 2010 :cite:`lemieux:10`, Losch et al. 2014
   :cite:`losch:14`);

-  ice-ocean stress can be formulated as in Hibler and Bryan (1987)
   :cite:`hibler:87` or as in Campin et al. (2008) :cite:`campin:08`;

-  ice variables are advected by sophisticated, conservative advection
   schemes with flux limiting;

-  growth and melt parameterizations have been refined and extended in
   order to allow for more stable automatic differentiation of the code.

The sea ice model is tightly coupled to the ocean compontent of the
MITgcm. Heat, fresh water fluxes and surface stresses are computed from the
atmospheric state and, by default, modified by the ice model at every time
step.

The ice dynamics models that are most widely used for large-scale climate
studies are the viscous-plastic (VP) model (Hilber 1979 :cite:`hibler:79`), the
cavitating fluid (CF) model (Flato and Hibler 1992 :cite:`flato:92`), and the
elastic-viscous-plastic (EVP) model (Hunke and Dukowicz 1997 :cite:`hunke:97`).
Compared to the VP model, the CF model does not allow ice shear in calculating
ice motion, stress, and deformation. EVP models approximate VP by adding an
elastic term to the equations for easier adaptation to parallel
computers. Because of its higher accuracy in plastic solution and relatively
simpler formulation, compared to the EVP model, we decided to use the VP model
as the default dynamic component of our ice model. To do this we extended the
line successive over relaxation (LSOR) method of Zhang and Hibler (1997)
:cite:`zhang:97` for use in a parallel configuration. An EVP model and a
free-drift implementation can be selected with run-time flags.


.. _para_phys_pkg_seaice_thsice:

Compatibility with ice-thermodynamics package :filelink:`pkg/thsice`
--------------------------------------------------------------------

By default :filelink:`pkg/seaice` includes the original so-called zero-layer
thermodynamics with a snow cover as in the appendix of Semtner (1976)
:cite:`semtner:76`. The zero-layer thermodynamic model assumes that ice does
not store heat and, therefore, tends to exaggerate the seasonal variability in
ice thickness. This exaggeration can be significantly reduced by using Winton's
(Winton 2000 :cite:`winton:00`) three-layer thermodynamic model that permits
heat storage in ice.

The Winton (2000) sea-ice thermodynamics have been ported to MITgcm; they
currently reside under :filelink:`pkg/thsice`, described in
:numref:`sub_phys_pkg_thsice`.  It is fully compatible with the packages
:filelink:`seaice <pkg/seaice>` and :filelink:`exf <pkg/exf>`.  When turned on
together with :filelink:`seaice <pkg/seaice>`, the zero-layer thermodynamics
are replaced by the Winton thermodynamics. In order to use package
:filelink:`seaice <pkg/seaice>` with the thermodynamics of
:filelink:`pkg/thsice`, compile both packages and turn both package on in
``data.pkg``; see an example in
:filelink:`verification/global_ocean.cs32x15/input.icedyn`. Note, that once
:filelink:`thsice <pkg/thsice>` is turned on, the variables and diagnostics
associated to the default thermodynamics are meaningless, and the diagnostics
of :filelink:`thsice <pkg/thsice>` must be used instead.

.. _para_phys_pkg_seaice_surfaceforcing:

Surface forcing
---------------

The sea ice model requires the following input fields: 10 m winds, 2 m air
temperature and specific humidity, downward longwave and shortwave radiations,
precipitation, evaporation, and river and glacier runoff. The sea ice model
also requires surface temperature from the ocean model and the top level
horizontal velocity. Output fields are surface wind stress, evaporation minus
precipitation minus runoff, net surface heat flux, and net shortwave flux.  The
sea-ice model is global: in ice-free regions bulk formulae (by default computed
in package :filelink:`exf <pkg/exf>`) are used to estimate oceanic forcing from
the atmospheric fields.

.. _para_phys_pkg_seaice_dynamics:

Dynamics
--------


The momentum equation of the sea-ice model is

.. math::
   m \frac{D\mathbf{u}}{Dt} = -mf\hat{\mathbf{k}}\times\mathbf{u} +
   \mathbf{\tau}_\mathrm{air} + \mathbf{\tau}_\mathrm{ocean}
   - m \nabla{\phi(0)} + \mathbf{F}
   :label: eq_momseaice

where :math:`m=m_{i}+m_{s}` is the ice and snow mass per unit area;
:math:`\mathbf{u}=u\hat{\mathbf{i}}+v\hat{\mathbf{j}}` is the ice velocity vector;
:math:`\hat{\mathbf{i}}`, :math:`\hat{\mathbf{j}}`, and :math:`\hat{\mathbf{k}}` are unit vectors
in the :math:`x`, :math:`y`, and :math:`z` directions, respectively; :math:`f`
is the Coriolis parameter; :math:`\mathbf{\tau}_\mathrm{air}` and
:math:`\mathbf{\tau}_\mathrm{ocean}` are the wind-ice and ocean-ice stresses,
respectively; :math:`g` is the gravity accelation; :math:`\nabla\phi(0)` is the
gradient (or tilt) of the sea surface height; :math:`\phi(0) = g\eta +
p_{a}/\rho_{0} + mg/\rho_{0}` is the sea surface height potential in response
to ocean dynamics (:math:`g\eta`), to atmospheric pressure loading
(:math:`p_{a}/\rho_{0}`, where :math:`\rho_{0}` is a reference density) and a
term due to snow and ice loading ; and :math:`\mathbf{F}= \nabla  \cdot\sigma` is
the divergence of the internal ice stress tensor :math:`\sigma_{ij}`.
Advection of sea-ice momentum is neglected. The wind and ice-ocean stress terms
are given by

.. math::
   \begin{aligned}
     \mathbf{\tau}_\mathrm{air}   = & \rho_\mathrm{air}  C_\mathrm{air}
     |\mathbf{U}_\mathrm{air} -\mathbf{u}|  R_\mathrm{air}
     (\mathbf{U}_\mathrm{air} - \mathbf{u}) \\
     \mathbf{\tau}_\mathrm{ocean} = & \rho_\mathrm{ocean}C_\mathrm{ocean}
     |\mathbf{U}_\mathrm{ocean}-\mathbf{u}|
     R_\mathrm{ocean}(\mathbf{U}_\mathrm{ocean} - \mathbf{u})
   \end{aligned}

where :math:`\mathbf{U}_\mathrm{air/ocean}` are the surface winds of the
atmosphere and surface currents of the ocean, respectively;
:math:`C_\mathrm{air/ocean}` are air and ocean drag coefficients;
:math:`\rho_\mathrm{air/ocean}` are reference densities; and
:math:`R_\mathrm{air/ocean}` are rotation matrices that act on the wind/current
vectors.

.. _para_phys_pkg_seaice_VPrheology:

Viscous-Plastic (VP) Rheology
-----------------------------

For an isotropic system the stress tensor :math:`\sigma_{ij}` (:math:`i,j=1,2`)
can be related to the ice strain rate and strength by a nonlinear
viscous-plastic (VP) constitutive law:

.. math::
   \sigma_{ij}=2\eta(\dot{\epsilon}_{ij},P)\dot{\epsilon}_{ij}
   + \left[\zeta(\dot{\epsilon}_{ij},P) -
       \eta(\dot{\epsilon}_{ij},P)\right]\dot{\epsilon}_{kk}\delta_{ij}
   - \frac{P}{2}\delta_{ij}
   :label: eq_vpequation

The ice strain rate is given by

.. math::
   \dot{\epsilon}_{ij} = \frac{1}{2}\left(
       \frac{\partial{u_{i}}}{\partial{x_{j}}} +
       \frac{\partial{u_{j}}}{\partial{x_{i}}}\right)

The maximum ice pressure :math:`P_{\max}` (variable :varlink:`PRESS0` in the
code), a measure of ice strength, depends on both thickness :math:`h` and
compactness (concentration) :math:`c`:

.. math::
   :label: eq_icestrength

   P_{\max} = P^{\ast}c\,h\,\exp\{-C^{\ast}\cdot(1-c)\},

with the constants :math:`P^{\ast}` (run-time parameter
:varlink:`SEAICE_strength`) and :math:`C^{\ast}` (run-time parameter
:varlink:`SEAICE_cStar`). By default, :math:`P` (variable :varlink:`PRESS` in
the code) is the replacement pressure

 .. math::
    :label: eq_pressrepl

    P = (1-k_t)\,P_{\max} \left( (1 - f_{r})
    + f_{r} \frac{\Delta}{\Delta_{\rm reg}}  \right)

where :math:`f_{r}` is run-time parameter :varlink:`SEAICEpressReplFac`
(default = 1.0), and :math:`\Delta_{\rm reg}` is a regularized form of
:math:`\Delta = \left[ \left(\dot{\epsilon}_{11}+\dot{\epsilon}_{22}\right)^2 +
e^{-2}\left( \left(\dot{\epsilon}_{11}-\dot{\epsilon}_{22} \right)^2 +
\dot{\epsilon}_{12}^2 \right) \right]^{\frac{1}{2}}`, for example
:math:`\Delta_{\rm reg} = \max(\Delta,\Delta_{\min})`.

The tensile strength factor :math:`k_t` (run-time parameter
:varlink:`SEAICE_tensilFac`) determines the ice tensile strength :math:`T =
k_t\cdot P_{\max}`, as defined by König Beatty and Holland (2010)
:cite:`konig:10`. :varlink:`SEAICE_tensilFac` is zero by default.

Different VP rheologies can be used to model sea ice dynamics. The different
rheologies are characterized by different definitions of the bulk and shear
viscosities :math:`\zeta` and :math:`\eta` in :eq:`eq_vpequation`.  The
following :numref:`tab_phys_pkg_seaice_rheologies` is a summary of the
available choices with recommended (sensible) parameter values. All the
rheologies presented here depend on the ice strength :math:`P`
:eq:`eq_pressrepl`.

.. tabularcolumns:: |\Y{.275}|\Y{.450}|\Y{.275}|

.. table:: Overview over availabe sea ice viscous-plastic rheologies
  :class: longtable
  :name: tab_phys_pkg_seaice_rheologies

  +---------------------------------------+---------------------------------------+----------------------------------------------------+
  |   Name                                | CPP flags                             | Run-time flags (recommended value)                 |
  +=======================================+=======================================+====================================================+
  |   :ref:`rheologies_ellnfr`            |   None (default)                      | - :varlink:`SEAICE_eccen` (= 2.0)                  |
  |                                       |                                       | - :varlink:`SEAICE_tensilFac` (= 0.0)              |
  +---------------------------------------+---------------------------------------+----------------------------------------------------+
  |   :ref:`rheologies_ellnnfr`           |   None                                | - :varlink:`SEAICE_eccen` (= 2.0)                  |
  |                                       |                                       | - :varlink:`SEAICE_eccfr` (< 2.0)                  |
  |                                       |                                       | - :varlink:`SEAICE_tensilFac` (= 0.0)              |
  +---------------------------------------+---------------------------------------+----------------------------------------------------+
  |   :ref:`rheologies_TEM`               |   :varlink:`SEAICE_ALLOW_TEM`         | - :varlink:`SEAICEuseTEM` (=.TRUE.)                |
  |                                       |                                       | - :varlink:`SEAICE_eccen` (= 1.4)                  |
  |                                       |                                       | - :varlink:`SEAICE_eccfr` (< 1.4)                  |
  |                                       |                                       | - :varlink:`SEAICE_tensilFac` (= 0.05)             |
  |                                       |                                       | - :varlink:`SEAICEmcMU` (= 0.6 to 0.8)             |
  +---------------------------------------+---------------------------------------+----------------------------------------------------+
  |   :ref:`rheologies_MCE`               |   :varlink:`SEAICE_ALLOW_MCE`         | - :varlink:`SEAICEuseMCE` (=.TRUE.)                |
  |                                       |                                       | - :varlink:`SEAICE_eccen`  (= 1.4)                 |
  |                                       |                                       | - :varlink:`SEAICE_eccfr`  (< 1.4)                 |
  |                                       |                                       | - :varlink:`SEAICE_tensilFac` (= 0.05)             |
  |                                       |                                       | - :varlink:`SEAICEmcMU` (= 0.6 to 0.8)             |
  +---------------------------------------+---------------------------------------+----------------------------------------------------+
  |   :ref:`rheologies_MCS`               |   :varlink:`SEAICE_ALLOW_MCS`         | - :varlink:`SEAICEuseMCS` (=.TRUE.)                |
  |                                       |                                       | - :varlink:`SEAICE_tensilFac` (= 0.05)             |
  |                                       |                                       | - :varlink:`SEAICEmcMU` (= 0.6 to 0.8)             |
  +---------------------------------------+---------------------------------------+----------------------------------------------------+
  |   :ref:`rheologies_TD`                |   :varlink:`SEAICE_ALLOW_TD`          | - :varlink:`SEAICEuseTD` (=.TRUE.)                 |
  |                                       |                                       | - :varlink:`SEAICE_tensilFac` (= 0.025)            |
  +---------------------------------------+---------------------------------------+----------------------------------------------------+
  |   :ref:`rheologies_PL`                |   :varlink:`SEAICE_ALLOW_TD`          |  - :varlink:`SEAICEusePL` (=.TRUE.)                |
  |                                       |                                       |  - :varlink:`SEAICE_tensilFac` (= 0.025)           |
  +---------------------------------------+---------------------------------------+----------------------------------------------------+


**Note:** With the exception of the default rheology and the TEM (with
:varlink:`SEAICEmcMU` : :math:`\mu=1.0`), these rheologies are not implemented
in EVP (:numref:`para_phys_pkg_seaice_EVPdynamics`).

.. _rheologies_ellnfr:

Elliptical yield curve with normal flow rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The default rheology in the sea ice module of the MITgcm implements the widely
used elliptical yield curve with a normal flow rule :cite:`hibler:79`.  For
this yield curve, the nonlinear bulk and shear viscosities :math:`\zeta` and
:math:`\eta` are functions of ice strain rate invariants and ice strength such
that the principal components of the stress lie on an elliptical yield curve
with the ratio of major to minor axis :math:`e = 2.0` (run-time parameter
:varlink:`SEAICE_eccen`); they are given by:

.. math::
   \begin{aligned}
     \zeta =& \min\left(\frac{(1+k_t)P_{\max}}{2\max(\Delta,\Delta_{\min})},
      \zeta_{\max}\right) \\
     \eta =& \frac{\zeta}{e^2}
   \end{aligned}
   :label: eq_zetareg


with the abbreviation

 .. math::
    \Delta =  \left[
    \left(\dot{\epsilon}_{11}+\dot{\epsilon}_{22}\right)^2
    + e^{-2}\left( \left(\dot{\epsilon}_{11}-\dot{\epsilon}_{22} \right)^2
      + \dot{\epsilon}_{12}^2 \right)
    \right]^{\frac{1}{2}}

The bulk viscosities are bounded above by imposing both a minimum
:math:`\Delta_{\min}` (for numerical reasons, run-time parameter
:varlink:`SEAICE_deltaMin` is set to a default value of
:math:`10^{-10}\,\text{s}^{-1}`, the value of :varlink:`SEAICE_EPS`) and a
maximum :math:`\zeta_{\max} = P_{\max}/(2\Delta^\ast)`, where
:math:`\Delta^\ast=(2\times10^4/5\times10^{12})\,\text{s}^{-1}` :math:`=
2\times10^{-9}\,\text{s}^{-1}`.  Obviously, this corresponds to regularizing
:math:`\Delta` with the typical value of :varlink:`SEAICE_deltaMin` :math:`=
2\times10^{-9}`. Clearly, some of this regularization is redundant.  (There is
also the option of bounding :math:`\zeta` from below by setting run-time
parameter :varlink:`SEAICE_zetaMin` :math:`>0`, but this is generally not
recommended). For stress tensor computation the replacement pressure :math:`P =
2\,\Delta\zeta` is used so that the stress state always lies on the elliptic
yield curve by definition.

Defining the CPP-flag :varlink:`SEAICE_ZETA_SMOOTHREG` in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` before compiling
replaces the method for bounding :math:`\zeta` by a smooth (differentiable)
expression:

.. math::
   \begin{split}
   \zeta &= \zeta_{\max}\tanh\left(\frac{(1+k_t)P_{\max}}{2\,
         \min(\Delta,\Delta_{\min}) \,\zeta_{\max}}\right)\\
   &= \frac{(1+k_t)P_{\max}}{2\Delta^\ast}
   \tanh\left(\frac{\Delta^\ast}{\min(\Delta,\Delta_{\min})}\right)
   \end{split}
   :label: eq_zetaregsmooth

where :math:`\Delta_{\min}=10^{-20}\,\text{s}^{-1}` should be chosen to avoid
divisions by zero.

In this default formulation the yield curve does not allow isotropic tensile
stress, that is, sea ice can be "pulled apart" without any effort.  Setting the
parameter :math:`k_t` (:varlink:`SEAICE_tensilFac`) to a small value larger
than zero, extends the yield curve into a region where the divergence of the
stress :math:`\sigma_{11}+\sigma_{22} > 0` to allow some tensile stress.

Besides this commonly used default rheology, a number of a alternative
rheologies are implemented.  Some of these are experiemental and should be used
with caution.

.. _rheologies_ellnnfr:

Elliptical yield curve with non-normal flow rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defining the run-time parameter :varlink:`SEAICE_eccfr` with a value different
from :varlink:`SEAICE_eccen` allows one to use an elliptical yield curve with a
non-normal flow rule as described in Ringeisen et al. (2020)
:cite:`ringeisen:20`.  In this case the viscosities are functions of
:math:`e_F` (:varlink:`SEAICE_eccen`) and :math:`e_G`
(:varlink:`SEAICE_eccfr`):

.. math::
   \begin{aligned}
     \zeta &= \frac{P_{\max}(1+k_t)}{2\Delta} \\
     \eta &= \frac{\zeta}{e_G^2} = \frac{P_{\max}(1+k_t)}{2e_G^2\Delta}
   \end{aligned}

with the abbreviation

.. math::
     \Delta = \sqrt{(\dot{\epsilon}_{11}-\dot{\epsilon}_{22})^2
       +\frac{e_F^2}{e_G^4}((\dot{\epsilon}_{11}
       -\dot{\epsilon}_{22})^2+4\dot{\epsilon}_{12}^2)}.

Note that if :math:`e_G=e_F=e`, these formulae reduce to the normal flow rule.

.. _rheologies_TEM:

Truncated ellipse method (TEM) for elliptical yield curve
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the so-called truncated ellipse method, the shear viscosity :math:`\eta` is
capped to suppress any tensile stress:

.. math::
   \eta = \min\left(\frac{\zeta}{e^2},
   \frac{\frac{(1+k_t)\,P_{\max}}{2}-\zeta(\dot{\epsilon}_{11}+\dot{\epsilon}_{22})}
   {\sqrt{\max(\Delta_{\min}^{2},(\dot{\epsilon}_{11}-\dot{\epsilon}_{22})^2
   +4\dot{\epsilon}_{12}^2})}\right).
   :label: eq_etatem

To enable this method, set ``#define`` :varlink:`SEAICE_ALLOW_TEM` in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` and turn it on with
:varlink:`SEAICEuseTEM` ``=.TRUE.,`` in ``data.seaice``. This parameter
combination implies the default of :varlink:`SEAICEmcMU` :math:`= 1.0`.

Instead of an ellipse that is truncated by constant slope coulombic limbs, this
yield curve can also be seen as a Mohr-Coulomb yield curve with elliptical flow
rule that is truncated for high :math:`P` by an ellipse. As a consequence, the
Mohr-Coulomb slope :varlink:`SEAICEmcMU` can be set in ``data.seaice`` to
values :math:`\ne 1.0`. This defines a coulombic yield curve similar to the
ones shown in Hibler and Schulson (2000) :cite:`hibler:00` and Ringeisen et
al. (2019) :cite:`ringeisen:19`.

For this rheology, it is recommended to use a non-zero tensile strength, so set
:varlink:`SEAICE_tensilFac` :math:`=k_{t}>0` in ``data.seaice``, e.g., :math:`=
0.05` or 5%.

.. _rheologies_MCE:

Mohr-Coulomb yield curve with elliptical plastic potential
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use a Mohr-Coulomb rheology, set ``#define`` :varlink:`SEAICE_ALLOW_MCE` in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` and
:varlink:`SEAICEuseMCE` ``= .TRUE.,`` in ``data.seaice``.  This Mohr-Coulomb
yield curve uses an elliptical plastic potential to define the flow rule.  The
slope of the Mohr-Coulomb yield curve is defined by :varlink:`SEAICEmcMU` in
``data.seaice``, and the plastic potential ellipse aspect ratio is set by
:varlink:`SEAICE_eccfr` in ``data.seaice``.  For details of this rheology, see
https://doi.org/10.26092/elib/380, Chapter 2.

For this rheology, it is recommended to use a non-zero tensile strength, so set
:varlink:`SEAICE_tensilFac` :math:`>0` in ``data.seaice``, e.g., :math:`= 0.05`
or 5%.

.. _rheologies_MCS:

Mohr-Coulomb yield curve with shear flow rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use the specifc Mohr-Coulomb rheology as defined first by Ip et al. (1991)
:cite:`ip:91`, set ``#define`` :varlink:`SEAICE_ALLOW_MCS` in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` and
:varlink:`SEAICEuseMCS` ``= .TRUE.,`` in ``data.seaice``.  The slope of the
Mohr-Coulomb yield curve is defined by :varlink:`SEAICEmcMU` in
``data.seaice``.  For details of this rheology, including the tensile strength,
see https://doi.org/10.26092/elib/380, Chapter 2.

For this rheology, it is recommended to use a non-zero tensile strength, so set
:varlink:`SEAICE_tensilFac` :math:`>0` in ``data.seaice``, e.g., :math:`= 0.05`
or 5%.

**WARNING: This rheology is known to be unstable. Use with caution!**

.. _rheologies_TD:

Teardrop yield curve with normal flow rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The teardrop rheology was first described in Zhang and Rothrock (2005)
:cite:`zha:05`.  Here we implement a slightly modified version (See
https://doi.org/10.26092/elib/380, Chapter 2).

To use this rheology, set ``#define`` :varlink:`SEAICE_ALLOW_TEARDROP` in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` and
:varlink:`SEAICEuseTD` ``= .TRUE.,`` in ``data.seaice``. The size of the yield
curve can be modified by changing the tensile strength, using
:varlink:`SEAICE_tensFac` in ``data.seaice``.

For this rheology, it is recommended to use a non-zero tensile strength, so set
:varlink:`SEAICE_tensilFac` :math:`>0` in ``data.seaice``, e.g., :math:`=
0.025` or 2.5%.

.. _rheologies_PL:

Parabolic lens yield curve with normal flow rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The parabolic lens rheology was first described in Zhang and Rothrock (2005)
:cite:`zha:05`.  Here we implement a slightly modified version (See
https://doi.org/10.26092/elib/380, Chapter 2).

To use this rheology, set ``#define`` :varlink:`SEAICE_ALLOW_TEARDROP` in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` and
:varlink:`SEAICEusePL` ``= .TRUE.,`` in ``data.seaice``. The size of the yield
curve can be modified by changing the tensile strength, using
:varlink:`SEAICE_tensFac` in ``data.seaice``.

For this rheology, it is recommended to use a non-zero tensile strength, so set
:varlink:`SEAICE_tensilFac` :math:`>0` in ``data.seaice``, e.g., :math:`=
0.025` or 2.5%.

.. _para_phys_pkg_seaice_LSRJFNK:

LSR and JFNK solver
-------------------

In matrix notation, the discretized momentum equations can be written as

.. math::
   :label: eq_matrixmom

     \mathbf{A}(\mathbf{x})\,\mathbf{x} = \mathbf{b}(\mathbf{x}).

The solution vector :math:`\mathbf{x}` consists of the two velocity components
:math:`u` and :math:`v` that contain the velocity variables at all grid points
and at one time level. The standard (and default) method for solving
Eq. :eq:`eq_matrixmom` in the sea ice component of MITgcm is an iterative
Picard solver: in the :math:`k`-th iteration a linearized form
:math:`\mathbf{A}(\mathbf{x}^{k-1})\,\mathbf{x}^{k} =
\mathbf{b}(\mathbf{x}^{k-1})` is solved (in the case of MITgcm it is a Line
Successive (over) Relaxation (LSR) algorithm). Picard solvers converge slowly,
but in practice the iteration is generally terminated after only a few
nonlinear steps and the calculation continues with the next time level. This
method is the default method in MITgcm. The number of nonlinear iteration steps
or pseudo-time steps can be controlled by the run-time parameter
:varlink:`SEAICEnonLinIterMax`.  This parameter's default is 2, but using a
number of at least 10 is recommended for better solutions that are converged at
least in an energy norm sense (Zhang and Hibler 1997) :cite:`zhang:97`.

In order to overcome the poor convergence of the Picard solver, Lemieux et
al. (2010) :cite:`lemieux:10` introduced a Jacobian-free Newton-Krylov solver
for the sea ice momentum equations. This solver is also implemented in MITgcm
(see Losch et al. 2014 :cite:`losch:14`). The Newton method transforms
minimizing the residual :math:`\mathbf{F}(\mathbf{x}) =
\mathbf{A}(\mathbf{x})\,\mathbf{x} - \mathbf{b}(\mathbf{x})` to finding the
roots of a multivariate Taylor expansion of the residual :math:`\mathbf{F}`
around the previous (:math:`k-1`) estimate :math:`\mathbf{x}^{k-1}`:

.. math::
   \mathbf{F}(\mathbf{x}^{k-1}+\delta\mathbf{x}^{k}) =
   \mathbf{F}(\mathbf{x}^{k-1}) + \mathbf{F}'(\mathbf{x}^{k-1})
   \,\delta\mathbf{x}^{k}
   :label: eq_jfnktaylor

with the Jacobian :math:`\mathbf{J}\equiv\mathbf{F}'`.  The root
:math:`\mathbf{F}(\mathbf{x}^{k-1}+\delta\mathbf{x}^{k})=0` is found by solving

.. math::
   \mathbf{J}(\mathbf{x}^{k-1})\,\delta\mathbf{x}^{k} =
   -\mathbf{F}(\mathbf{x}^{k-1})
   :label: eq_jfnklin

for :math:`\delta\mathbf{x}^{k}`. The next (:math:`k`-th) estimate is given by
:math:`\mathbf{x}^{k}=\mathbf{x}^{k-1}+(1-\gamma_{\mathrm{LS}})^{l}
\,\delta\mathbf{x}^{k}`.

By default :math:`l=0`, but in order to avoid overshoots, the step size factor
:math:`(1-\gamma_{\mathrm{LS}})^{l}` with :math:`\gamma_{\mathrm{LS}}<1` can be
iteratively reduced in a line search with :math:`l=0,1,2,\ldots` until
:math:`\|\mathbf{F}(\mathbf{x}^k)\| < \|\mathbf{F}(\mathbf{x}^{k-1})\|`, where
:math:`\|\cdot\|=\int\cdot\,dx^2` is the :math:`L_2`-norm. The line search
starts after :varlink:`SEAICE_JFNK_lsIter` nonlinear Newton iterations (off by
default) to allow for full Newton steps at the beginning of the iteration. If
the line search is turned on by setting :varlink:`SEAICE_JFNK_lsIter` to a
non-negative value in ``data.seaice``, by default, the line search with
:math:`\gamma_\mathrm{LS}=\frac{1}{2}` (runtime parameter
:varlink:`SEAICE_JFNK_lsGamma`) is stopped after :math:`L_{\max}=4` (runtime
parameter :varlink:`SEAICE_JFNK_lsLmax`) steps.

Forming the Jacobian :math:`\mathbf{J}` explicitly is often avoided as “too
error prone and time consuming”. Instead, Krylov methods only require the
action of :math:`\mathbf{J}` on an arbitrary vector :math:`\mathbf{w}` and
hence allow a matrix free algorithm for solving :eq:`eq_jfnklin`. The action of
:math:`\mathbf{J}` can be approximated by a first-order Taylor series
expansion:

.. math::
	 \mathbf{J}(\mathbf{x}^{k-1})\,\mathbf{w} \approx
	 \frac{\mathbf{F}(\mathbf{x}^{k-1}+\epsilon\mathbf{w})
	 - \mathbf{F}(\mathbf{x}^{k-1})} \epsilon
   :label: eq_jfnkjacvecfd

or computed exactly with the help of automatic differentiation (AD)
tools. :varlink:`SEAICE_JFNKepsilon` sets the step size :math:`\epsilon`.

We use the Flexible Generalized Minimum RESidual (FMGRES) method with
right-hand side preconditioning to solve :eq:`eq_jfnklin` iteratively starting
from a first guess of :math:`\delta\mathbf{x}^{k}_{0} = 0`. For the
preconditioning matrix :math:`\mathbf{P}` we choose a simplified form of the
system matrix :math:`\mathbf{A}(\mathbf{x}^{k-1})` where
:math:`\mathbf{x}^{k-1}` is the estimate of the previous Newton step
:math:`k-1`. The transformed equation :eq:`eq_jfnklin` becomes

.. math::
   \mathbf{J}(\mathbf{x}^{k-1})\,\mathbf{P}^{-1}\delta\mathbf{z} =
   -\mathbf{F}(\mathbf{x}^{k-1}), \quad\text{with} \quad
   \delta{\mathbf{z}} = \mathbf{P}\delta\mathbf{x}^{k}
   :label: eq_jfnklinpc

The Krylov method iteratively improves the approximate solution to
:eq:`eq_jfnklinpc` in subspace (:math:`\mathbf{r}_0`,
:math:`\mathbf{J}\mathbf{P}^{-1}\mathbf{r}_0`,
:math:`(\mathbf{J}\mathbf{P}^{-1})^2\mathbf{r}_0`, :math:`\dots`,
:math:`(\mathbf{J}\mathbf{P}^{-1})^m\mathbf{r}_0`) with increasing :math:`m`;
:math:`\mathbf{r}_0 = -\mathbf{F}(\mathbf{x}^{k-1})
-\mathbf{J}(\mathbf{x}^{k-1})\,\delta\mathbf{x}^{k}_{0}` is the initial
residual of :eq:`eq_jfnklin`;
:math:`\mathbf{r}_0=-\mathbf{F}(\mathbf{x}^{k-1})` with the first guess
:math:`\delta\mathbf{x}^{k}_{0}=0`. We allow a Krylov subspace of dimension \
:math:`m=50` and we do allow restarts for more than 50 Krylov iterations.  The
preconditioning operation involves applying :math:`\mathbf{P}^{-1}` to the
basis vectors :math:`\mathbf{v}_0, \mathbf{v}_1, \mathbf{v}_2, \ldots,
\mathbf{v}_m` of the Krylov subspace. This operation is approximated by solving
the linear system :math:`\mathbf{P}\,\mathbf{w}=\mathbf{v}_i`.  Because
:math:`\mathbf{P} \approx \mathbf{A}(\mathbf{x}^{k-1})`, we can use the
LSR algorithm already implemented in the Picard solver. Each preconditioning
operation uses a fixed number of 10 LSR iterations avoiding any termination
criterion. More details and results can be found in Losch et al. (2014)
:cite:`losch:14`).

To use the JFNK solver set :varlink:`SEAICEuseJFNK` ``= .TRUE.,`` in the
namelist file ``data.seaice``; ``#define`` :varlink:`SEAICE_ALLOW_JFNK` in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` and we recommend
using a smooth regularization of :math:`\zeta` by ``#define``
:varlink:`SEAICE_ZETA_SMOOTHREG` (see above) for better convergence. The
nonlinear Newton iteration is terminated when the :math:`L_2`-norm of the
residual is reduced by :math:`\gamma_{\mathrm{nl}}` (run-time parameter
:varlink:`SEAICEnonLinTol` ``= 1.E-4,`` will already lead to expensive
simulations) with respect to the initial norm:
:math:`\|\mathbf{F}(\mathbf{x}^k)\| <
\gamma_{\mathrm{nl}}\|\mathbf{F}(\mathbf{x}^0)\|`.  Within a nonlinear
iteration, the linear FGMRES solver is terminated when the residual is smaller
than :math:`\gamma_k\|\mathbf{F}(\mathbf{x}^{k-1})\|` where :math:`\gamma_k` is
determined by

.. math::
 	 \gamma_k =
      \begin{cases}
	 \gamma_0 &\text{for $\|\mathbf{F}(\mathbf{x}^{k-1})\| \geq r$},  \\
    \max\left(\gamma_{\min},
    \frac{\|\mathbf{F}(\mathbf{x}^{k-1})\|}
    {\|\mathbf{F}(\mathbf{x}^{k-2})\|}\right)
   &\text{for $\|\mathbf{F}(\mathbf{x}^{k-1})\| < r$,}
    \end{cases}
   :label: eq_jfnkgammalin

so that the linear tolerance parameter :math:`\gamma_k` decreases with the
nonlinear Newton step as the nonlinear solution is approached.  This inexact
Newton method is generally more robust and computationally more efficient than
exact methods. Typical parameter choices are :math:`\gamma_0 =`
:varlink:`JFNKgamma_lin_max` :math:`= 0.99`, :math:`\gamma_{\min} =`
:varlink:`JFNKgamma_lin_min` :math:`= 0.1`, and :math:`r =`
:varlink:`JFNKres_tFac` :math:`\times\|\mathbf{F}(\mathbf{x}^{0})\|` with
:varlink:`JFNKres_tFac` :math:`= 0.5`. We recommend a maximum number of
nonlinear iterations :varlink:`SEAICEnewtonIterMax` :math:`= 100` and a maximum
number of Krylov iterations :varlink:`SEAICEkrylovIterMax` :math:`= 50`,
because the Krylov subspace has a fixed dimension of 50 (but restarts are
allowed for :varlink:`SEAICEkrylovIterMax` :math:`> 50`).

Setting :varlink:`SEAICEuseStrImpCpl` to ``.TRUE.`` turns on “strength implicit
coupling” (see Hutchings et al. 2004 :cite:`hutchings:04`) in the LSR solver
and in the LSR preconditioner for the JFNK solver. In this mode, the different
contributions of the stress divergence terms are reordered so as to increase
the diagonal dominance of the system matrix.  Unfortunately, the convergence
rate of the LSR solver is increased only slightly, while the JFNK convergence
appears to be unaffected.

.. _para_phys_pkg_seaice_EVPdynamics:

Elastic-Viscous-Plastic (EVP) Dynamics
--------------------------------------

Hunke and Dukowicz (1997) :cite:`hunke:97` introduced an elastic contribution
to the strain rate in order to regularize :eq:`eq_vpequation` in such a way
that the resulting elastic-viscous-plastic (EVP) and VP models are identical at
steady state,

.. math::
   \frac{1}{E}\frac{\partial\sigma_{ij}}{\partial{t}} +
    \frac{1}{2\eta}\sigma_{ij}
    + \frac{\eta - \zeta}{4\zeta\eta}\sigma_{kk}\delta_{ij}
    + \frac{P}{4\zeta}\delta_{ij}
    = \dot{\epsilon}_{ij}.
   :label: eq_evpequation

The EVP model uses an explicit time stepping scheme with a short timestep.
According to the recommendation in Hunke and Dukowicz (1997) :cite:`hunke:97`,
the EVP-model should be stepped forward in time 120 times
(:varlink:`SEAICE_deltaTevp` = :varlink:`SEAICE_deltaTdyn` /120) within the
physical ocean model time step (although this parameter is under debate), to
allow for elastic waves to disappear. Because the scheme does not require a
matrix inversion it is fast in spite of the small internal timestep and simple
to implement on parallel computers. For completeness, we repeat the equations
for the components of the stress tensor :math:`\sigma_{1} =
\sigma_{11}+\sigma_{22}`, :math:`\sigma_{2}= \sigma_{11}-\sigma_{22}`, and
:math:`\sigma_{12}`. Introducing the divergence :math:`D_D =
\dot{\epsilon}_{11}+\dot{\epsilon}_{22}`, and the horizontal tension and
shearing strain rates, :math:`D_T = \dot{\epsilon}_{11}-\dot{\epsilon}_{22}`
and :math:`D_S = 2\dot{\epsilon}_{12}`, respectively, and using the above
abbreviations, the equations :eq:`eq_evpequation` can be written as:

.. math::
   \frac{\partial\sigma_{1}}{\partial{t}} + \frac{\sigma_{1}}{2T} +
   \frac{P}{2T} = \frac{P}{2T\Delta} D_D
   :label: eq_evpstresstensor1

.. math::
   \frac{\partial\sigma_{2}}{\partial{t}} + \frac{\sigma_{2} e^{2}}{2T}
   = \frac{P}{2T\Delta} D_T
  :label: eq_evpstresstensor2

.. math::
  \frac{\partial\sigma_{12}}{\partial{t}} + \frac{\sigma_{12} e^{2}}{2T}
  = \frac{P}{4T\Delta} D_S
  :label: eq_evpstresstensor12

Here, the elastic parameter :math:`E` is redefined in terms of a damping
timescale :math:`T` for elastic waves

.. math:: E=\frac{\zeta}{T}

:math:`T=E_{0}\Delta{t}` with the tunable parameter :math:`E_0<1` and the
external (long) timestep :math:`\Delta{t}`.  :math:`E_{0} = \frac{1}{3}` is the
default value in the code and close to what Hunke and Dukowicz (1997)
:cite:`hunke:97` recommend.

We do not recommend to use the EVP solver in its original form. Instead, use
mEVP or aEVP instead (see :numref:`para_phys_pkg_seaice_EVPstar`). If you
really need to use the original EVP solver, make sure that both ``#define``
:varlink:`SEAICE_CGRID` and ``#define`` :varlink:`SEAICE_ALLOW_EVP` are set in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` (both are defined by
default). By default, the runtime parameters :varlink:`SEAICEuseEVPstar` and
:varlink:`SEAICEuseEVPrev` are set to ``.TRUE.``, which already improves the
behavoir of EVP, but for the original EVP they should be set to ``.FALSE.``.  The
solver is turned on by setting the sub-cycling time step
:varlink:`SEAICE_deltaTevp` to a value larger than zero. The choice of this
time step is under debate.  Hunke and Dukowicz (1997) :cite:`hunke:97` recommend
order 120 time steps for the EVP solver within one model time step
:math:`\Delta{t}` (:varlink:`deltaTmom`). One can also choose order 120 time
steps within the forcing time scale, but then we recommend adjusting the
damping time scale :math:`T` accordingly, by setting either
:varlink:`SEAICE_elasticParm` (:math:`E_{0}`), so that :math:`E_{0}\Delta{t}=`
forcing time scale, or directly :varlink:`SEAICE_evpTauRelax` (:math:`T`) to
the forcing time scale. (**NOTE**: with the improved EVP variants of the next
section, the above recommendations are obsolete. Use mEVP or aEVP instead.)

.. _para_phys_pkg_seaice_EVPstar:

More stable variants of Elastic-Viscous-Plastic Dynamics: EVP\*, mEVP, and aEVP
-------------------------------------------------------------------------------

The genuine EVP scheme appears to give noisy solutions (see Hunke 2001, Lemieux
et al. 2012, Bouillon et a1. 2013
:cite:`hunke:01,lemieux:12,bouillon:13`). This has led to a modified EVP or
EVP\* (Lemieux et al. 2012, Bouillon et a1. 2013, Kimmritz et al. 2015
:cite:`lemieux:12,bouillon:13,kimmritz:15`); here, we refer to these variants
by modified EVP (mEVP) and adaptive EVP (aEVP).  The main idea is to modify the
“natural” time-discretization of the momentum equations:

.. math::
   m\frac{D\mathbf{u}}{Dt} \approx
   m\frac{\mathbf{u}^{p+1}-\mathbf{u}^{n}}{\Delta{t}} +
   \beta^{\ast}\frac{\mathbf{u}^{p+1}-\mathbf{u}^{p}}{\Delta{t}_{\mathrm{EVP}}}
   :label: eq_evpstar

where :math:`n` is the previous time step index, and :math:`p` is the previous
sub-cycling index. The extra “intertial” term
:math:`m\,(\mathbf{u}^{p+1}-\mathbf{u}^{n})/\Delta{t})` allows the definition
of a residual :math:`|\mathbf{u}^{p+1}-\mathbf{u}^{p}|` that, as
:math:`\mathbf{u}^{p+1} \rightarrow \mathbf{u}^{n+1}`, converges to
:math:`0`. In this way EVP can be re-interpreted as a pure iterative solver
where the sub-cycling has no association with time-relation (through
:math:`\Delta{t}_{\mathrm{EVP}}`). With the setting of
:varlink:`SEAICEuseEVPstar` to  ``.TRUE.`` (default), this form of EVP is used.
Using the terminology of Kimmritz et al. 2015 :cite:`kimmritz:15`, the evolution
equations of stress :math:`\sigma_{ij}` and momentum :math:`\mathbf{u}` can be
written as:

.. math::
   \sigma_{ij}^{p+1}=\sigma_{ij}^p+\frac{1}{\alpha}
   \Big(\sigma_{ij}(\mathbf{u}^p)-\sigma_{ij}^p\Big),
   \phantom{\int}
   :label: eq_evpstarsigma

.. math::
   \mathbf{u}^{p+1}=\mathbf{u}^p+\frac{1}{\beta}
   \Big(\frac{\Delta t}{m} \nabla  \cdot\boldsymbol{\sigma}^{p+1}+
   \frac{\Delta t}{m}\mathbf{R}^{p}+\mathbf{u}_n
     -\mathbf{u}^p\Big)
   :label: eq_evpstarmom

:math:`\mathbf{R}` contains all terms in the momentum equations except for the
rheology terms and the time derivative; :math:`\alpha` and :math:`\beta` are
free parameters (:varlink:`SEAICE_evpAlpha`, :varlink:`SEAICE_evpBeta`) that
replace the time stepping parameters :varlink:`SEAICE_deltaTevp`
(:math:`\Delta{t}_{\mathrm{EVP}}`), :varlink:`SEAICE_elasticParm`
(:math:`E_{0}`), or :varlink:`SEAICE_evpTauRelax` (:math:`T`). :math:`\alpha`
and :math:`\beta` determine the speed of convergence and the
stability. Usually, it makes sense to use :math:`\alpha = \beta`, and
:varlink:`SEAICEnEVPstarSteps` :math:`\gg (\alpha,\,\beta)` (Kimmritz et
al. 2015 :cite:`kimmritz:15`). Currently, there is no termination criterion and
the number of mEVP iterations is fixed to :varlink:`SEAICEnEVPstarSteps`.

In order to use mEVP in MITgcm, compile with both ``#define``
:varlink:`SEAICE_CGRID` and ``#define`` :varlink:`SEAICE_ALLOW_EVP` in
:filelink:`SEAICE_OPTIONS.h <pkg/seaice/SEAICE_OPTIONS.h>` (default) and make
sure that :varlink:`SEAICEuseEVPstar` ``= .TRUE.,`` (default) in ``data.seaice``.
By default :varlink:`SEAICEuseEVPrev` is set to ``.TRUE.`` and the
actual form of equations :eq:`eq_evpstarsigma` and :eq:`eq_evpstarmom` is used
with fewer implicit terms and the factor of :math:`e^{2}` dropped in the stress
equations :eq:`eq_evpstresstensor2` and :eq:`eq_evpstresstensor12`. Although
this modifies the original EVP equations, it turns out to improve convergence
(Bouillon et al. 2013 :cite:`bouillon:13`).

The aEVP scheme is an enhanced variant of mEVP (Kimmritz et al. 2016
:cite:`kimmritz:16`), where the value of :math:`\alpha` is set dynamically based
on the stability criterion

.. math::
   \alpha = \beta = \max\left( \tilde{c} \pi\sqrt{c \frac{\zeta}{A_{c}}
   \frac{\Delta{t}}{\max(m,10^{-4}\,\text{kg})}},\alpha_{\min} \right)
   :label: eq_aevpalpha

with the grid cell area :math:`A_c` and the ice and snow mass :math:`m`.  This
choice sacrifices speed of convergence for stability with the result that aEVP
converges quickly to VP where :math:`\alpha` can be small and more slowly in
areas where the equations are stiff. In practice, aEVP leads to an overall
better convergence than mEVP (Kimmritz et al. 2016 :cite:`kimmritz:16`). To use
aEVP in MITgcm set :varlink:`SEAICEaEVPcoeff` :math:`= \tilde{c}`
(see :eq:`eq_aevpalpha`; default is unset); this also
sets the default values of :varlink:`SEAICEaEVPcStar` (:math:`c=4`) and
:varlink:`SEAICEaEVPalphaMin` (:math:`\alpha_{\min}=5`). Good convergence has
been obtained with these values (Kimmritz et al. 2016 :cite:`kimmritz:16`):

::

   SEAICEaEVPcoeff      = 0.5,
   SEAICEnEVPstarSteps  = 500,
   # The following two parameters are required by mEVP and aEVP,
   # but they are TRUE by default:
   SEAICEuseEVPstar     = .TRUE.,
   SEAICEuseEVPrev      = .TRUE.,

Because of the C-grid staggering of velocities and
stresses, mEVP may not converge as successfully as in Kimmritz et al. (2015)
:cite:`kimmritz:15`, see also Kimmritz et al. (2016) :cite:`kimmritz:16`.
Convergence at very high resolution (order 5 km) has not yet been studied.

.. _para_phys_pkg_seaice_iceoceanstress:

Ice-Ocean stress
----------------

Moving sea ice exerts a stress on the ocean which is the opposite of the stress
:math:`\mathbf{\tau}_\mathrm{ocean}` in :eq:`eq_momseaice`. This stress is
applied directly to the surface layer of the ocean model. An alternative ocean
stress formulation is given by Hibler and Bryan (1987)
:cite:`hibler:87`. Rather than applying :math:`\mathbf{\tau}_\mathrm{ocean}`
directly, the stress is derived from integrating over the ice thickness to the
bottom of the oceanic surface layer. In the resulting equation for the
*combined* ocean-ice momentum, the interfacial stress cancels and the total
stress appears as the sum of windstress and divergence of internal ice
stresses: :math:`\delta(z) (\mathbf{\tau}_\mathrm{air} + \mathbf{F})/\rho_0`,
see also Eq. (2) of Hibler and Bryan (1987) :cite:`hibler:87`. The disadvantage
of this formulation is that now the velocity in the surface layer of the ocean
that is used to advect tracers, is really an average over the ocean surface
velocity and the ice velocity leading to an inconsistency as the ice
temperature and salinity are different from the oceanic variables. To turn on
the stress formulation of Hibler and Bryan (1987) :cite:`hibler:87`, set
:varlink:`useHB87StressCoupling` ``=.TRUE.,``, in ``data.seaice``.

.. _para_phys_pkg_seaice_discretization:


Finite-volume discretization of the stress tensor divergence
------------------------------------------------------------

On an Arakawa C grid, ice thickness and concentration and thus ice strength
:math:`P` and bulk and shear viscosities :math:`\zeta` and :math:`\eta` are
naturally defined a C-points in the center of the grid cell. Discretization
requires only averaging of :math:`\zeta` and :math:`\eta` to vorticity or
Z-points (or :math:`\zeta`-points, but here we use Z in order avoid confusion
with the bulk viscosity) at the bottom left corner of the cell to give
:math:`\overline{\zeta}^{Z}` and :math:`\overline{\eta}^{Z}`. In the following,
the superscripts indicate location at Z or C points, distance across the cell
(F), along the cell edge (G), between :math:`u`-points (U), :math:`v`-points
(V), and C-points (C). The control volumes of the :math:`u`- and
:math:`v`-equations in the grid cell at indices :math:`(i,j)` are
:math:`A_{i,j}^{w}` and :math:`A_{i,j}^{s}`, respectively. With these
definitions (which follow the model code documentation except that
:math:`\zeta`-points have been renamed to Z-points), the strain rates are
discretized as:

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
      \biggr),
      \end{aligned}

so that the diagonal terms of the strain rate tensor are naturally defined at
C-points and the symmetric off-diagonal term at Z-points.  No-slip boundary
conditions (:math:`u_{i,j-1}+u_{i,j}=0` and :math:`v_{i-1,j}+v_{i,j}=0` across
boundaries) are implemented via “ghost-points”; for free slip boundary
conditions :math:`(\epsilon_{12})^Z=0` on boundaries.

For a spherical polar grid, the coefficients of the metric terms are
:math:`k_{1}=0` and :math:`k_{2}=-\tan\phi/a`, with the spherical radius
:math:`a` and the latitude :math:`\phi`; :math:`\Delta{x}_1 = \Delta{x} =
a\cos\phi \Delta\lambda`, and :math:`\Delta{x}_2 = \Delta{y}=a\Delta\phi`. For
a general orthogonal curvilinear grid, :math:`k_{1}` and :math:`k_{2}` can be
approximated by finite differences of the cell widths:

.. math::
   \begin{aligned}
     k_{1,i,j}^{C} &= \frac{1}{\Delta{y}_{i,j}^{F}}
     \frac{\Delta{y}_{i+1,j}^{G}-\Delta{y}_{i,j}^{G}}{\Delta{x}_{i,j}^{F}} \\
     k_{2,i,j}^{C} &= \frac{1}{\Delta{x}_{i,j}^{F}}
     \frac{\Delta{x}_{i,j+1}^{G}-\Delta{x}_{i,j}^{G}}{\Delta{y}_{i,j}^{F}} \\
     k_{1,i,j}^{Z} &= \frac{1}{\Delta{y}_{i,j}^{U}}
     \frac{\Delta{y}_{i,j}^{C}-\Delta{y}_{i-1,j}^{C}}{\Delta{x}_{i,j}^{V}} \\
     k_{2,i,j}^{Z} &= \frac{1}{\Delta{x}_{i,j}^{V}}
     \frac{\Delta{x}_{i,j}^{C}-\Delta{x}_{i,j-1}^{C}}{\Delta{y}_{i,j}^{U}}
     \end{aligned}

The stress tensor is given by the constitutive viscous-plastic relation
:math:`\sigma_{\alpha\beta} = 2\eta\dot{\epsilon}_{\alpha\beta} +
[(\zeta-\eta)\dot{\epsilon}_{\gamma\gamma} - P/2 ]\delta_{\alpha\beta}` . The
stress tensor divergence :math:`(\nabla\sigma)_{\alpha} =
\partial_\beta\sigma_{\beta\alpha}`, is discretized in finite volumes . This
conveniently avoids dealing with further metric terms, as these are “hidden” in
the differential cell widths. For the :math:`u`-equation (:math:`\alpha=1`) we
have:

.. math::
   \begin{aligned}
     (\nabla\sigma)_{1}: \phantom{=}&
     \frac{1}{A_{i,j}^w}
     \int_{\mathrm{cell}}(\partial_1\sigma_{11}+\partial_2\sigma_{21})
     \,dx_1\,dx_2  \\\notag
     =& \frac{1}{A_{i,j}^w} \biggl\{
     \int_{x_2}^{x_2+\Delta{x}_2}\sigma_{11}dx_2\biggl|_{x_{1}}^{x_{1}
     +\Delta{x}_{1}}
     + \int_{x_1}^{x_1+\Delta{x}_1}\sigma_{21}dx_1\biggl|_{x_{2}}^{x_{2}
     +\Delta{x}_{2}}
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
     \biggr\}
     \end{aligned}

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
     k_{1,i,j}^{Z}\frac{v_{i,j}+v_{i-1,j}}{2}
     \end{aligned}

Similarly, we have for the :math:`v`-equation (:math:`\alpha=2`):

.. math::
   \begin{aligned}
     (\nabla\sigma)_{2}: \phantom{=}&
     \frac{1}{A_{i,j}^s}
     \int_{\mathrm{cell}}(\partial_1\sigma_{12}+\partial_2\sigma_{22})
     \,dx_1\,dx_2 \\\notag
     =& \frac{1}{A_{i,j}^s} \biggl\{
     \int_{x_2}^{x_2+\Delta{x}_2}\sigma_{12}dx_2\biggl|_{x_{1}}^{x_{1}
     +\Delta{x}_{1}}
     + \int_{x_1}^{x_1+\Delta{x}_1}\sigma_{22}dx_1\biggl|_{x_{2}}^{x_{2}
     +\Delta{x}_{2}}
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

Again, no-slip boundary conditions are realized via ghost points and
:math:`u_{i,j-1}+u_{i,j}=0` and :math:`v_{i-1,j}+v_{i,j}=0` across
boundaries. For free-slip boundary conditions the lateral stress is set to
zeros. In analogy to :math:`(\epsilon_{12})^Z=0` on boundaries, we set
:math:`\sigma_{21}^{Z}=0`, or equivalently :math:`\eta_{i,j}^{Z}=0`, on
boundaries.

.. _para_phys_pkg_seaice_thermodynamics:

Thermodynamics
--------------

**NOTE: THIS SECTION IS STILL NOT COMPLETE**

In its original formulation the sea ice model uses simple 0-layer
thermodynamics following the appendix of Semtner (1976)
:cite:`semtner:76`. This formulation neglects storage of heat, that is, the
heat capacity of ice is zero, and all internal heat sources so that the heat
equation reduces to a constant conductive heat flux. This constant upward
conductive heat flux together with a constant ice conductivity implies a linear
temperature profile. The boundary conditions for the heat equations are: at the
bottom of the ice :math:`T|_{\rm bottom} = T_{\rm fr}` (freezing point temperature of
sea water), and at the surface: :math:`Q_{\rm top} =
\frac{\partial{T}}{\partial{z}} = (K/h)(T_{0}-T_{\rm fr})`, where :math:`K` is the
ice conductivity, :math:`h` the ice thickness, and :math:`T_{0}-T_{\rm fr}` the
difference between the ice surface temperature and the water temperature at the
bottom of the ice (at the freezing point). The surface heat flux
:math:`Q_{\rm top}` is computed in a similar way to that of Parkinson and
Washington (1979) :cite:`parkinson:79` and Manabe et al. (1979)
:cite:`manabe:79`. The resulting equation for surface temperature is

.. math::
   \begin{aligned}
   \frac{K}{h}(T_{0}-T_{\rm fr}) &= Q_{\rm SW\downarrow}(1-\mathrm{albedo}) \\
   & + \epsilon Q_{\rm LW\downarrow} - Q_{\rm LW\uparrow}(T_{0}) \\
   & + Q_{\rm LH}(T_{0}) + Q_{\rm SH}(T_{0}),
   \end{aligned}
   :label: eq_zerolayerheatbalance

where :math:`\epsilon` is the emissivity of the surface (snow or ice),
:math:`Q_{\rm S/LW\downarrow}` the downwelling shortwave and longwave radiation to
be prescribed, and :math:`Q_{\rm LW\uparrow}=\epsilon\sigma_B T_{0}^4` the emitted
long wave radiation with the Stefan-Boltzmann constant :math:`\sigma_B`. With
explicit expressions in :math:`T_0` for the turbulent fluxes of latent and
sensible heat

.. math::
   \begin{aligned}
   Q_{\rm LH} &= \rho_\mathrm{air} C_E (\Lambda_v + \Lambda_f)
   |\mathbf{U}_\mathrm{air}|
   \left[ q_\mathrm{air} - q_\mathrm{sat}(T_0)\right] \\
   Q_{\rm SH} &= \rho_\mathrm{air} c_p C_E |\mathbf{U}_\mathrm{air}|
   \left[ T_\mathrm{10m} - T_{0} \right],
   \end{aligned}

:eq:`eq_zerolayerheatbalance` can be solved for :math:`T_0` with an iterative
Ralphson-Newton method, which usually converges very quickly in less that 10
iterations. In these equations, :math:`\rho_\mathrm{air}` is the air density
(parameter :varlink:`SEAICE_rhoAir`), :math:`C_E` is the ice-ocean transfer
coefficient for sensible and latent heat (parameter :varlink:`SEAICE_dalton`),
:math:`\Lambda_v` and :math:`\Lambda_f` are the latent heat of vaporization and
fusion, respectively (parameters :varlink:`SEAICE_lhEvap` and
:varlink:`SEAICE_lhFusion`), and :math:`c_p` is the specific heat of air
(parameter :varlink:`SEAICE_cpAir`). For the latent heat :math:`Q_{\rm LH}` a
choice can be made between the old polynomial expression for saturation
humidity :math:`q_\mathrm{sat}(T_0)` (by setting
:varlink:`useMaykutSatVapPoly` to ``.TRUE.``) and the default exponential
relation approximation that is more accurate at low temperatures.

In the zero-layer model of Semtner (1976) :cite:`semtner:76`, the conductive
heat flux depends strongly on the ice thickness :math:`h`. However, the ice
thickness in the model represents a mean over a potentially very heterogeneous
thickness distribution. In order to parameterize a sub-grid scale distribution
for heat flux computations, the mean ice thickness :math:`h` is split into
:math:`N` thickness categories :math:`H_{n}` that are equally distributed
between :math:`2h` and a minimum imposed ice thickness of :math:`5\,\text{cm}`
by :math:`H_n= \frac{2n-1}{7}\,h` for :math:`n\in[1,N]`. The heat fluxes
computed for each thickness category are area-averaged to give the total heat
flux (see Hibler 1984 :cite:`hibler:84`). To use this thickness category
parameterization set :varlink:`SEAICE_multDim` to the number of desired
categories in ``data.seaice`` (7 is a good guess, for anything larger than 7
modify :filelink:`SEAICE_SIZE.h <pkg/seaice/SEAICE_SIZE.h>`).  Note that this
requires different restart files and switching this flag on in the middle of an
integration is not advised. As an alternative to the flat distribution, the
run-time parameter :varlink:`SEAICE_PDF` (1D-array of lenght :varlink:`nITD`)
can be used to prescribe an arbitrary distribution of ice thicknesses, for
example derived from observed distributions (Castro-Morales et al. 2014
:cite:`castro-morales:14`). In order to include the ice thickness distribution
also for snow, set :varlink:`SEAICE_useMultDimSnow` to ``.TRUE.`` (this is the
default); only then, the parameterization of always having a fraction of thin
ice is efficient and generally thicker ice is produced (see Castro-Morales et
al. 2014 :cite:`castro-morales:14`).

The atmospheric heat flux is balanced by an oceanic heat flux from below. The
oceanic flux is proportional to :math:`\rho\,c_{p}\left(T_{w}-T_{fr}\right)`
where :math:`\rho` and :math:`c_{p}` are the density and heat capacity of sea
water and :math:`T_{\rm fr}` is the local freezing point temperature that is a
function of salinity. This flux is not assumed to instantaneously melt or
create ice, but a time scale of three days (run-time parameter
:varlink:`SEAICE_gamma_t`) is used to relax :math:`T_{w}` to the freezing
point. The parameterization of lateral and vertical growth of sea ice follows
that of Hibler (1979) and Hibler (1980) :cite:`hibler:79,hibler:80`; the
so-called lead closing parameter :math:`h_{0}` (run-time parameter
:varlink:`HO`) has a default value of 0.5 meters.

On top of the ice there is a layer of snow that modifies the heat flux and the
albedo (Zhang et al. 1998 :cite:`zha:98`). Snow modifies the effective
conductivity according to

.. math:: \frac{K}{h} \rightarrow \frac{1}{\frac{h_{s}}{K_{s}}+\frac{h}{K}},

where :math:`K_s` is the conductivity of snow and :math:`h_s` the snow
thickness. If enough snow accumulates so that its weight submerges the ice and
the snow is flooded, a simple mass conserving parameterization of snowice
formation (a flood-freeze algorithm following Archimedes’ principle) turns snow
into ice until the ice surface is back at :math:`z=0` (see Leppäranta 1983
:cite:`leppaeranta:83`).  The flood-freeze algorithm is turned on with run-time
parameter :varlink:`SEAICEuseFlooding` set to ``.TRUE.``.

.. _para_phys_pkg_seaice_advection:

Advection of thermodynamic variables
------------------------------------

Effective ice thickness (ice volume per unit area, :math:`c h`),
concentration :math:`c` and effective snow thickness (:math:`c h_s`)
are advected by ice velocities:

.. math::
   \frac{\partial{X}}{\partial{t}} =
	 -  \nabla  \cdot\left(\mathbf{u}\,X\right) + \Gamma_{X} + D_{X}
   :label: eq_advection

where :math:`\Gamma_X` are the thermodynamic source terms and :math:`D_{X}` the
diffusive terms for quantities :math:`X= c h, c, c h_s`. From
the various advection schemes that are available in MITgcm, we recommend
flux-limited schemes to preserve sharp gradients and edges that are typical of
sea ice distributions and to rule out unphysical over- and undershoots
(negative thickness or concentration). These schemes conserve volume and
horizontal area and are unconditionally stable, so that we can set
:math:`D_{X}=0`. Run-time flags: :varlink:`SEAICEadvScheme` (default=77, is a
2nd-order flux limited scheme), :varlink:`DIFF1` = :math:`D_{X}/\Delta{x}`
(default=0).

The MITgcm sea ice model provides the option to use the thermodynamics model of
Winton (2000) :cite:`winton:00`, which in turn is based on the 3-layer model of
Semtner (1976) :cite:`semtner:76` which treats brine content by means of
enthalpy conservation; the corresponding package :filelink:`thsice
<pkg/thsice>` is described in section :numref:`sub_phys_pkg_thsice`. This
scheme requires additional state variables, namely the enthalpy of the two ice
layers (instead of effective ice salinity), to be advected by ice
velocities. The internal sea ice temperature is inferred from ice enthalpy. To
avoid unphysical (negative) values for ice thickness and concentration, a
positive 2nd-order advection scheme with a SuperBee flux limiter (Roe 1985
:cite:`roe:85`) should be used to advect all sea-ice-related quantities of the
Winton (2000) :cite:`winton:00` thermodynamic model (run-time flag
:varlink:`thSIceAdvScheme` :math:`= 77` and :varlink:`thSIce_diffK` :math:`=
D_{X} = 0` in ``data.ice``, defaults are 0). Because of the nonlinearity of the
advection scheme, care must be taken in advecting these quantities: when simply
using ice velocity to advect enthalpy, the total energy (i.e., the volume
integral of enthalpy) is not conserved. Alternatively, one can advect the
energy content (i.e., product of ice-volume and enthalpy) but then false
enthalpy extrema can occur, which then leads to unrealistic ice temperature. In
the currently implemented solution, the sea-ice mass flux is used to advect the
enthalpy in order to ensure conservation of enthalpy and to prevent false
enthalpy extrema.

.. _para_phys_pkg_seaice_itd:

Dynamical Ice Thickness Distribution (ITD)
------------------------------------------

The ice thickness distribution model used by MITgcm follows the implementation
in the Los Alamos sea ice model CICE (https://github.com/CICE-Consortium/CICE).
There are two parts to it that are closely connected: the participation and
ridging functions that determine which thickness classes take part in ridging
and which thickness classes receive ice during ridging based on Thorndike et
al. (1975) :cite:`thorndike:75`, and the ice strength parameterization by
Rothrock (1975) :cite:`rothrock:75` which uses this information.  The following
description is slightly modified from Ungermann et al. (2017)
:cite:`ungermann:17`.  Verification experiment :filelink:`seaice_itd
<verification/seaice_itd>` uses the ITD model.

Distribution, participation and redistribution functions in ridging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When :varlink:`SEAICE_ITD` is defined in :filelink:`SEAICE_OPTIONS.h
<pkg/seaice/SEAICE_OPTIONS.h>`, the ice thickness is described by the ice
thickness distribution :math:`g(h,\mathbf{x},t)` for the subgrid-scale (see
Thorndike et al. 1975 :cite:`thorndike:75`), a probability density function for
thickness :math:`h` following the evolution equation


.. math::
   \frac{\partial g}{\partial t} = -  \nabla  \cdot (\mathbf{u} g) - \frac{\partial}{\partial h}(fg) + \Psi.
   :label: eq_itd


Here :math:`f=\frac{\mathrm{d} h}{\mathrm{d} t}` is the thermodynamic growth
rate and :math:`\Psi` a function describing the mechanical redistribution of
sea ice during ridging or lead opening.

The mechanical redistribution function :math:`\Psi` generates open water in
divergent motion and creates ridged ice during convergent motion. The ridging
process depends on total strain rate and on the ratio between shear (run-time
parameter :varlink:`SEAICEshearParm`) and divergent strain.  In the single
category model, ridge formation is treated implicitly by limiting the ice
concentration to a maximum of one (see Hibler 1979 :cite:`hibler:79`), so that
further volume increase in convergent motion leads to thicker ice. (This is
also the default for ITD models; to change from the default, set run-time
parameter :varlink:`SEAICEsimpleRidging` ``=.FALSE.,`` in ``data.seaice``).  For
the ITD model, the ridging mode in convergence

.. math::
   \omega_r(h)= \frac{-a(h)+n(h)}{N}

gives the effective change for the ice volume with thickness between :math:`h`
and :math:`h+\textrm{d} h` as the normalized difference between the ice
:math:`n(h)` generated by ridging and the ice :math:`a(h)` participating in
ridging.

The participation function :math:`a(h) = b(h)g(h)` can be computed either
following Thorndike et al. (1975) :cite:`thorndike:75` (run-time parameter
:varlink:`SEAICEpartFunc` =0) or Lipscomb et al. (2007) :cite:`lipscomb:07`
(:varlink:`SEAICEpartFunc` =1), and similarly the ridging function :math:`n(h)`
can be computed following Hilber (1980) :cite:`hibler:80` (run-time parameter
:varlink:`SEAICEredistFunc` =0) or Lipscomb et al. (2007) :cite:`lipscomb:07`
(:varlink:`SEAICEredistFunc` =1). As an example, we show here the functions
that Lipscomb et al. (2007) :cite:`lipscomb:07` suggested to avoid noise in the
solutions. These functions are smooth and avoid non-differentiable
discontinuities, but so far we did not find any noise issues as in Lipscomb et
al. (2007) :cite:`lipscomb:07`.

With :varlink:`SEAICEpartFunc` ``= 1,`` in ``data.seaice``, the participation
function with the relative amount of ice of thickness :math:`h` weighted by an
exponential function

.. math::
   b(h) = b_0 \exp [ -G(h)/a^*]

where :math:`G(h)=\int_0^h g(h) \textrm{d} h` is the cumulative thickness
distribution function, :math:`b_0` a normalization factor, and :math:`a^*`
(:varlink:`SEAICEaStar`) the exponential constant that determines which
relative amount of thicker and thinner ice take part in ridging.

With :varlink:`SEAICEredistFunc` ``= 1,`` in ``data.seaice``, the ice generated by
ridging is calculated as

.. math::
   n(h) = \int_0^\infty  a(h_1)\gamma(h_1,h) \textrm{d} h_1

where the density function :math:`\gamma(h_1,h)` of resulting thickness
:math:`h` for ridged ice with an original thickness of :math:`h_1` is taken as

.. math::
   \gamma(h_1, h) = \frac{1}{k \lambda}
   \exp\left[{\frac{-(h-h_{\min})}{\lambda}}\right]

for :math:`h \geq h_{\min}`, with :math:`\gamma(h_1,h)=0` for :math:`h <
h_{\min}`.  In this parameterization, the normalization factor
:math:`k=\frac{h_{\min} + \lambda}{h_1}`, the e-folding scale :math:`\lambda =
\mu h_1^{1/2}` and the minimum ridge thickness :math:`h_{\min}=\min(2h_1,h_1 +
h_{\textrm{raft}})` all depend on the original thickness :math:`h_1`.  The
maximal ice thickness allowed to raft :math:`h_{\textrm{raft}}` is constant
(:varlink:`SEAICEmaxRaft`, default =1 m) and :math:`\mu`
(:varlink:`SEAICEmuRidging`) is a tunable parameter.

In the numerical model these equations are discretized into a set of :math:`n`
(:varlink:`nITD` defined in :filelink:`SEAICE_SIZE.h
<pkg/seaice/SEAICE_SIZE.h>`) thickness categories employing the delta function
scheme of Bitz et al. (2001) :cite:`bitz:01`.  For each thickness category in
an ITD configuration, the volume conservation equation :eq:`eq_advection` is
evaluated using the heat flux with the category-specific values for ice and
snow thickness, so there are no conceptual differences in the thermodynamics
between the single category and ITD configurations.  The only difference is
that only in the thinnest category the creation of new ice of thickness
:math:`H_0` (run-time parameter :varlink:`HO`) is possible, all other
categories are limited to basal growth.  The conservation of ice area is
replaced by the evolution equation of the ITD :eq:`eq_itd` that is discretized
in thickness space with :math:`n+1` category limits given by run-time parameter
:varlink:`Hlimit`.  If :varlink:`Hlimit` is not set in ``data.seaice``, a
simple recursive formula following Lipscomb (2001) :cite:`lipscomb:01` is used
to compute :varlink:`Hlimit`:

.. math::
   H_\mathrm{limit}(k) = H_\mathrm{limit}(k-1) + \frac{c_1}{n}
   + \frac{c_1 c_2}{n} [ 1 + \tanh c_3 (\frac{k-1}{n} - 1) ]

with :math:`H_\mathrm{limit}(0)=0` m and
:math:`H_\mathrm{limit}(n)=999.9` m. The three constants are the
run-time parameters :varlink:`Hlimit_c1`, :varlink:`Hlimit_c2`, and
:varlink:`Hlimit_c3`.  The total ice concentration and volume can then be
calculated by summing up the values for each category.

Ice strength parameterization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the default approach of equation :eq:`eq_icestrength`, the ice strength is
parameterized following Hibler (1979) :cite:`hibler:79` and :math:`P` depends
only on average ice concentration and thickness per grid cell and the constant
ice strength parameters :math:`P^{\ast}` (:varlink:`SEAICE_strength`) and
:math:`C^{\ast}` (:varlink:`SEAICE_cStar`).  With an ice thickness
distribution, it is possible to use a different parameterization following
Rothrock (1975) :cite:`rothrock:75`

.. math::
    P = C_f C_p \int_0^\infty h^2 \omega_r(h) \textrm{d}h
   :label: eq_rothrock

by considering the production of potential energy and the frictional energy
loss in ridging.  The physical constant :math:`C_p = \rho_i (\rho_w - \rho_i)
\hat{g} / (2 \rho_w)` is a combination of the gravitational acceleration
:math:`\hat{g}` and the densities :math:`\rho_i`, :math:`\rho_w` of ice and
water, and :math:`C_f` (:varlink:`SEAICE_cf`) is a scaling factor relating the
amount of work against gravity necessary for ridging to the amount of work
against friction.  To calculate the integral, this parameterization needs
information about the ITD in each grid cell, while the default
parameterization :eq:`eq_icestrength` can be used for both ITD and single
thickness category models.  In contrast to :eq:`eq_icestrength`, which is based
on the plausible assumption that thick and compact ice is stronger than thin
and loose drifting ice, this parameterization :eq:`eq_rothrock` clearly
contains the more physical assumptions about energy conservation.  For that
reason alone this parameterization is often considered to be more physically
realistic than :eq:`eq_icestrength`, but in practice, the success is not so
clear (Ungermann et al. 2007 :cite:`ungermann:17`).  Ergo, the default is to
use :eq:`eq_icestrength`; set :varlink:`useHibler79IceStrength` ``=.FALSE.,`` in
``data.seaice`` to change this behavior.

Known issues and work-arounds
=============================

- An often encountered problem in long simulations with sea ice models is
  (local) perpetually increasing sea ice (plus snow) height; this is
  problematic when using a non-linear free surface and
  :varlink:`useRealFreshWaterFlux` set to ``.TRUE.``, because the mass of the sea ice
  places a load on the sea surface, which if too large, can cause the surface
  cells of the model to become too thin so that the model eventually stops with
  an error message. Usually this problem occurs because of dynamical ice growth
  (i.e., convergence and ridging of ice) or simply too much net precipitation
  with insufficient summer surface melting. If the problem is dynamical in
  nature (e.g., caused by ridging in a deep inlet), the first step to try is to
  turn off the replacement pressure method (:varlink:`SEAICEpressReplFac` = 0;
  in :numref:`para_phys_pkg_seaice_VPrheology`); turning this off provides
  resistance against additional growth due to further ridging, because the ice
  pressure :math:`P` is no longer reduced as :math:`\Delta\rightarrow 0` in
  nearly motionless thick ice :eq:`eq_pressrepl`. If this does not solve the
  problem, a somewhat more radical yet effective approach is simply to cap the
  sea ice load on the free surface by defining the CPP option
  :varlink:`SEAICE_CAP_ICELOAD`. This option effectively limits the sea ice
  load (variable :varlink:`sIceLoad`) to a mass of 1/5 of the the top grid cell
  depth.  If desired, this limit can be changed in routine
  :filelink:`seaice_growth.F <pkg/seaice/seaice_growth.F>` where variable
  :varlink:`heffTooHeavy` is assigned.

.. _ssub_phys_pkg_seaice_subroutines:

Key subroutines
===============

Top-level routine: :filelink:`pkg/seaice/seaice_model.F`

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

.. _ssub_phys_pkg_seaice_diagnostics:

SEAICE diagnostics
==================

Diagnostics output is available via the diagnostics package (see
:numref:`sub_outp_pkg_diagnostics`).  Available output fields are summarized in
the following table:

.. code-block:: text

    ---------+----------+----------------+-----------------
     <-Name->|<- grid ->|<--  Units   -->|<- Tile (max=80c)
    ---------+----------+----------------+-----------------
     sIceLoad|SM      U1|kg/m^2          |sea-ice loading (in Mass of ice+snow / area unit)
    ---
    SEA ICE STATE:
    ---
     SIarea  |SM      M1|m^2/m^2         |SEAICE fractional ice-covered area [0 to 1]
     SIheff  |SM      M1|m               |SEAICE effective ice thickness
     SIhsnow |SM      M1|m               |SEAICE effective snow thickness
     SIhsalt |SM      M1|g/m^2           |SEAICE effective salinity
     SIuice  |UU      M1|m/s             |SEAICE zonal ice velocity, >0 from West to East
     SIvice  |VV      M1|m/s             |SEAICE merid. ice velocity, >0 from South to North
    ---
    ATMOSPHERIC STATE AS SEEN BY SEA ICE:
    ---
     SItices |SM  C   M1|K               |Surface Temperature over Sea-Ice (area weighted)
     SIuwind |UM      U1|m/s             |SEAICE zonal 10-m wind speed, >0 increases uVel
     SIvwind |VM      U1|m/s             |SEAICE meridional 10-m wind speed, >0 increases uVel
     SIsnPrcp|SM      U1|kg/m^2/s        |Snow precip. (+=dw) over Sea-Ice (area weighted)
    ---
    FLUXES ACROSS ICE-OCEAN INTERFACE (ATMOS to OCEAN FOR ICE-FREE REGIONS):
    ---
     SIfu    |UU      U1|N/m^2           |SEAICE zonal surface wind stress, >0 increases uVel
     SIfv    |VV      U1|N/m^2           |SEAICE merid. surface wind stress, >0 increases vVel
     SIqnet  |SM      U1|W/m^2           |Ocean surface heatflux, turb+rad, >0 decreases theta
     SIqsw   |SM      U1|W/m^2           |Ocean surface shortwave radiat., >0 decreases theta
     SIempmr |SM      U1|kg/m^2/s        |Ocean surface freshwater flux, > 0 increases salt
     SIqneto |SM      U1|W/m^2           |Open Ocean Part of SIqnet, turb+rad, >0 decr theta
     SIqneti |SM      U1|W/m^2           |Ice Covered Part of SIqnet, turb+rad, >0 decr theta
    ---
    FLUXES ACROSS ATMOSPHERE-ICE INTERFACE (ATMOS to OCEAN FOR ICE-FREE REGIONS):
    ---
     SIatmQnt|SM      U1|W/m^2           |Net atmospheric heat flux, >0 decreases theta
     SIatmFW |SM      U1|kg/m^2/s        |Net freshwater flux from atmosphere & land (+=down)
     SIfwSubl|SM      U1|kg/m^2/s        |Freshwater flux of sublimated ice, >0 decreases ice
    ---
    THERMODYNAMIC DIAGNOSTICS:
    ---
     SIareaPR|SM      M1|m^2/m^2         |SIarea preceeding ridging process
     SIareaPT|SM      M1|m^2/m^2         |SIarea preceeding thermodynamic growth/melt
     SIheffPT|SM      M1|m               |SIheff preceeeding thermodynamic growth/melt
     SIhsnoPT|SM      M1|m               |SIhsnow preceeeding thermodynamic growth/melt
     SIaQbOCN|SM      M1|m/s             |Potential HEFF rate of change by ocean ice flux
     SIaQbATC|SM      M1|m/s             |Potential HEFF rate of change by atm flux over ice
     SIaQbATO|SM      M1|m/s             |Potential HEFF rate of change by open ocn atm flux
     SIdHbOCN|SM      M1|m/s             |HEFF rate of change by ocean ice flux
     SIdSbATC|SM      M1|m/s             |HSNOW rate of change by atm flux over sea ice
     SIdSbOCN|SM      M1|m/s             |HSNOW rate of change by ocean ice flux
     SIdHbATC|SM      M1|m/s             |HEFF rate of change by atm flux over sea ice
     SIdHbATO|SM      M1|m/s             |HEFF rate of change by open ocn atm flux
     SIdHbFLO|SM      M1|m/s             |HEFF rate of change by flooding snow
     SIdAbATO|SM      M1|m^2/m^2/s       |Potential AREA rate of change by open ocn atm flux
     SIdAbATC|SM      M1|m^2/m^2/s       |Potential AREA rate of change by atm flux over ice
     SIdAbOCN|SM      M1|m^2/m^2/s       |Potential AREA rate of change by ocean ice flux
     SIdA    |SM      M1|m^2/m^2/s       |AREA rate of change (net)
    ---
    DYNAMIC/RHEOLOGY DIAGNOSTICS:
    ---
     SIpress |SM      M1|N/m             |SEAICE strength (with upper and lower limit)
     SIzeta  |SM      M1|kg/s            |SEAICE nonlinear bulk viscosity
     SIeta   |SM      M1|kg/s            |SEAICE nonlinear shear viscosity
     SIsig1  |SM      M1|no units        |SEAICE normalized principle stress, component one
     SIsig2  |SM      M1|no units        |SEAICE normalized principle stress, component two
     SIshear |SM      M1|1/s             |SEAICE shear deformation rate
     SIdelta |SM      M1|1/s             |SEAICE Delta deformation rate
     SItensil|SM      M1|N/m             |SEAICE maximal tensile strength
    ---
    ADVECTIVE/DIFFUSIVE FLUXES OF SEA ICE variables:
    ---
     ADVxHEFF|UU      M1|m.m^2/s         |Zonal      Advective Flux of eff ice thickn
     ADVyHEFF|VV      M1|m.m^2/s         |Meridional Advective Flux of eff ice thickn
     SIuheff |UU      M1|m^2/s           |Zonal      Transport of eff ice thickn (centered)
     SIvheff |VV      M1|m^2/s           |Meridional Transport of eff ice thickn (centered)
     DFxEHEFF|UU      M1|m^2/s           |Zonal      Diffusive Flux of eff ice thickn
     DFyEHEFF|VV      M1|m^2/s           |Meridional Diffusive Flux of eff ice thickn
     ADVxAREA|UU      M1|m^2/m^2.m^2/s   |Zonal      Advective Flux of fract area
     ADVyAREA|VV      M1|m^2/m^2.m^2/s   |Meridional Advective Flux of fract area
     DFxEAREA|UU      M1|m^2/m^2.m^2/s   |Zonal      Diffusive Flux of fract area
     DFyEAREA|VV      M1|m^2/m^2.m^2/s   |Meridional Diffusive Flux of fract area
     ADVxSNOW|UU      M1|m.m^2/s         |Zonal      Advective Flux of eff snow thickn
     ADVySNOW|VV      M1|m.m^2/s         |Meridional Advective Flux of eff snow thickn
     DFxESNOW|UU      M1|m.m^2/s         |Zonal      Diffusive Flux of eff snow thickn
     DFyESNOW|VV      M1|m.m^2/s         |Meridional Diffusive Flux of eff snow thickn
     ADVxSSLT|UU      M1|(g/kg).m^2/s    |Zonal      Advective Flux of seaice salinity
     ADVySSLT|VV      M1|(g/kg).m^2/s    |Meridional Advective Flux of seaice salinity
     DFxESSLT|UU      M1|(g/kg).m^2/s    |Zonal      Diffusive Flux of seaice salinity
     DFyESSLT|VV      M1|(g/kg).m^2/s    |Meridional Diffusive Flux of seaice salinity


Experiments and tutorials that use seaice
=========================================

- :filelink:`verification/lab_sea`: Labrador Sea experiment
- :filelink:`verification/seaice_obcs`, based on :filelink:`lab_sea <verification/lab_sea>`
- :filelink:`verification/offline_exf_seaice`, idealized topography in a zonally re-entrant channel, tests solvers and rheologies
- :filelink:`verification/seaice_itd`, based on :filelink:`offline_exf_seaice <verification/offline_exf_seaice>`, tests ice thickness distribution
- :filelink:`verification/global_ocean.cs32x15`, global cubed-sphere-experiment with combinations of :filelink:`pkg/seaice` and :filelink:`pkg/thsice`
- :filelink:`verification/1D_ocean_ice_column`, just thermodynamics
