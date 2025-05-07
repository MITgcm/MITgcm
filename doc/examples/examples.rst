.. _chap_modelExamples:

MITgcm Tutorial Example Experiments
***********************************

The full MITgcm distribution comes with a set of pre-configured
numerical experiments.  Some of these example experiments are tests of
individual parts of the model code, but many are fully fledged
numerical simulations. Full tutorials exist for a few of the examples,
and are documented in sections :numref:`sec_eg_baro` -
:numref:`sec_eg_tank`. The other examples follow the same general
structure as the tutorial examples, see below.  All example experiments are
located in subdirectories under the directory :filelink:`verification`.
A list of additional experiments (i.e, not documented as full tutorials), with brief description,
is provided in :numref:`subsec_add_expts_fwd` and :numref:`subsec_add_expts_adj`.

Each example experiment directory has the following subdirectories:

-  ``code``: contains code specific to the example. At a minimum,
   this directory includes the following files:

   -  ``code/packages.conf``: declares the list of packages or package
      groups to be used. If not included, the default set of packages is located
      in :filelink:`pkg/pkg_groups`. Package groups are simply convenient
      collections of commonly used packages which are defined in
      :filelink:`pkg/pkg_groups` (see :ref:`using_packages`).
      Some packages may require other packages or
      may require their absence (that is, they are incompatible) and
      these package dependencies are listed in :filelink:`pkg/pkg_depend`.

   -  ``code/SIZE.h``: declares the size of underlying computational grid.
      This file is compiled instead of the MITgcm repository version :filelink:`model/inc/SIZE.h`.

   -  The ``code/`` directory may include other files and subroutines specific to the experiment,
      i.e., containing changes from the standard repository version.
      For example, some experiments contains CPP header options files to enable or disable
      some parts of the code at compile time; the most common ones would be :filelink:`model/inc/CPP_OPTIONS.h`
      for core model options  and «PKG»_OPTIONS.h for individual packages.

-  ``input``: contains the input data files required to run the example.
   At a minimum, the ``input`` directory contains the following files:

   -  ``input/data``: this file, written as a namelist, specifies the
      main parameters for the experiment.

   -  ``input/data.pkg``: contains parameters relative to the packages
      used in the experiment.

   -  ``input/eedata``: this file contains “execution environment” data.
      This consists of a specification of the number of
      threads to use in :math:`x` and :math:`y`. For multi-threaded execution,these will be set to numbers greater than 1.

   - Forcing and topography file(s), as well as files describing the initial state of
     the experiment and any other supporting data.
     Required support files vary from experiment to experiment, depending on the setup.

-  ``results``: this directory contains the output file ``output.txt``
   produced by the simulation example. This file is useful for
   comparison with your own output when you run the experiment.

-  ``build``: this directory is initially empty and should be used to compile
   the model and generate the executable.

-  ``run``: this directory is initially empty and should be used to run the
   executable. From the (empty) run directory, link files from ``input``
   using the command ``ln -s ../input/* .``, then execute the file ``../input/prepare_run`` if it exists.
   If you are running one of the experiment variations, i.e., using ``input.«OTHER»``,
   first link files from ``input.«OTHER»`` (running ``../input.«OTHER»/prepare_run`` if it exists) and next link files from ``input``
   (and run ``../input/prepare_run``). Following this procedure, file links
   from ``input.«OTHER»`` will NOT be overwritten by identically named files in ``input``.

.. raw:: html

   <h2> The tutorial experiments are as follows:</h2>

.. toctree::
   :maxdepth: 1

   barotropic_gyre/barotropic_gyre.rst

.. only:: html

   In directory :filelink:`tutorial_barotropic_gyre <verification/tutorial_barotropic_gyre>`:
   Single layer ocean gyre (barotropic with free-surface), using a Cartesian grid. If you are new to MITgcm, start here,
   as no prior experience with MITgcm is assumed. Introduces building and running the model, with description of model output files and
   simple methods to load and plot model output.

.. toctree::
   :maxdepth: 1

   baroclinic_gyre/baroclinic_gyre.rst

.. only:: html

   In directory :filelink:`tutorial_baroclinic_gyre <verification/tutorial_baroclinic_gyre>`:
   Ocean double-gyre using spherical coordinates (i.e., latitude-longitude coordinates) with 15 vertical layers. Second introductory tutorial, assumes you have read through
   :ref:`sec_eg_baro`. Introduces using `NetCDF <http://www.unidata.ucar.edu/software/netcdf>`_  for model output,
   and how to use the :ref:`diagnostics package <sub_outp_pkg_diagnostics>` to customize output and its writing frequency.

.. toctree::
   :maxdepth: 1

   reentrant_channel/reentrant_channel.rst

.. only:: html

   In directory :filelink:`tutorial_reentrant channel <verification/tutorial_reentrant_channel>`:
   Reentrant channel in the Southern Ocean with idealized topography, 49 vertical layers. Tutorial compares solution using coarse horizontal resolution,
   with and without GM parameterization (:filelink:`pkg/gmredi`),
   versus a high-resolution, eddy-permitting configuration. Third and final introductory tutorial,
   assumes reader is familiar with MITgcm basics described in tutorials :ref:`sec_eg_baro` and :ref:`tutorial_baroclinic_gyre`.
   Also introduces :filelink:`pkg/layers` and :filelink:`pkg/rbcs`.

.. toctree::
   :maxdepth: 1

   advection_in_gyre/advection_in_gyre.rst

.. only:: html

   In directory :filelink:`tutorial_advection_in_gyre <verification/tutorial_advection_in_gyre>`:
   Short tutorial comparing the results using different advection schemes in a single-layer, ocean double-gyre.
   Demonstrates the importance of carefully selecting an advection scheme for a specific setup.

.. toctree::
   :maxdepth: 1

   global_oce_latlon/global_oce_latlon.rst

.. only:: html

   In directory :filelink:`tutorial_global_oce_latlon <verification/tutorial_global_oce_latlon>`:
   Coarse resolution (4\ :sup:`o`\ x4\ :sup:`o`) global ocean simulation, using a spherical ocean grid with 15 vertical layers.
   Monthly climatological forcing of of wind stress, heat and freshwater fluxes is employed, with surface restoring
   of temperature and salinity. Simulates the large-scale ocean circulation.

.. toctree::
   :maxdepth: 1

   global_oce_in_p/global_oce_in_p.rst

.. only:: html

   In directory :filelink:`tutorial_global_oce_in_p <verification/tutorial_global_oce_in_p>`:
   Global ocean simulation with a similar configuration as :ref:`sec_global_oce_latlon` except pressure is used as the vertical coordinate
   instead of the traditional height coordinate (exploiting MITgcm's height–pressure coordinate coding isomorphism).
   In this configuration the model does **NOT** make the  Boussinesq approximation.

.. toctree::
   :maxdepth: 1

   held_suarez_cs/held_suarez_cs.rst

.. only:: html

   In directory :filelink:`tutorial_held_suarez_cs <verification/tutorial_held_suarez_cs>`:
   Simulates (dry) 3-D atmosphere dynamics using Held and Suarez forcing on a (global) cubed sphere grid. The vertical coordinate
   is a rescaled pressure coordinate (:math:`p^*`) with 20 levels; orography is flat. Radiation effects are represented by Newtonian cooling.

.. toctree::
   :maxdepth: 1

   deep_convection/deep_convection.rst

.. only:: html

   In directory :filelink:`tutorial_deep_convection <verification/tutorial_deep_convection>`:
   Non-uniformly surface-forced ocean deep convection in a doubly periodic box. This tutorial showcases MITgcm's non-hydrostatic
   capability in a spatially small domain (3 km x 3 km x 1 km deep), exploring the temporal and spatial characteristics of convection
   plumes as they might exist during a period of oceanic deep convection.

.. toctree::
   :maxdepth: 1

   plume_on_slope/plume_on_slope.rst

.. only:: html

   In directory :filelink:`tutorial_plume_on_slope <verification/tutorial_plume_on_slope>`:
   Non-hydrostatic simulation of a non-rotating gravity plume descending down a continental slope, forced by surface cooling. Model domain is 2-D with
   open boundaries conditions used in a the deep-water end of the domain.

.. toctree::
   :maxdepth: 1

   global_oce_biogeo/global_oce_biogeo.rst

.. only:: html

   In directory :filelink:`tutorial_global_oce_biogeo <verification/tutorial_global_oce_biogeo>`:
   Global ocean simulation (similar to tutorial :ref:`sec_global_oce_latlon` except using 2.8\ :sup:`o`\ x2.8\ :sup:`o` resolution) which includes
   a dissolved inorganic carbon biogeochemistry model. The biogeochemical model considers the coupled cycles of carbon, oxygen, phosphorus and alkalinity, which are
   included as passive tracers. A simplified parameterization of biological production is also included.

.. toctree::
   :maxdepth: 1

   global_oce_optim/global_oce_optim.rst

.. only:: html

   In directory :filelink:`tutorial_global_oce_optim <verification/tutorial_global_oce_optim>`:
   This tutorial illustrates the optimization capacity of the MITgcm,
   running the adjoint of a global ocean simulation (model setup similar to :ref:`sec_global_oce_latlon`).
   This adjoint run optimizes a time-independent surface heat flux (i.e., the control variable) which brings
   the model climatology closest to observed climatology, using a cost function
   based on gridpoint error in temperature. TAF and OpenAD adjoint setups.

.. toctree::
   :maxdepth: 1

   tracer_adjsens/tracer_adjsens.rst

.. only:: html

   In directory :filelink:`tutorial_tracer_adjsens <verification/tutorial_tracer_adjsens>`:
   A second experiment demonstrating MITgcm's adjoint capabilities, here examining the sensitivity of surface outgassing of a passive tracer
   as a function of tracer injection site location within the ocean interior. The global (4\ :sup:`o`\ x4\ :sup:`o`)
   ocean setup from tutorial :ref:`sec_global_oce_latlon`
   is again used for this tutorial. TAF and OpenAD adjoint setups.

.. toctree::
   :maxdepth: 1

   cfc_offline/cfc_offline.rst

.. only:: html

   In directory :filelink:`tutorial_cfc_offline <verification/tutorial_cfc_offline>`:
   This tutorial contains an experiment which uses MITgcm in offline mode (i.e., with prescribed ocean dynamics terms,
   from a prior forward integration of MITgcm), simulating
   the penetration of CFCs into the ocean interior during the last century. The model domain is global with 2.8\ :sup:`o`\ x2.8\ :sup:`o` resolution.

.. toctree::
   :maxdepth: 1

   rotating_tank/rotating_tank.rst

.. only:: html

   In directory :filelink:`tutorial_rotating_tank <verification/tutorial_rotating_tank>`:
   Laboratory rotating tank simulation,
   using a cylindrical coordinate system at laboratory scale of 46 cm diameter and 14.5 cm deep.
   This is a typical laboratory setup for illustrating principles of geophysical fluid mechanics. An annulus of fluid is heated differentially
   on the interior and exterior walls of the tank.

.. _subsec_add_expts_fwd:

Additional Example Experiments: Forward Model Setups
====================================================

For many experiments, additional information is provided in a ``README`` file located in the respective experiment's subdirectory.

#. :filelink:`1D_ocean_ice_column <verification/1D_ocean_ice_column>` - Oceanic column with seaice on top.

#. :filelink:`adjustment.128x64x1 <verification/adjustment.128x64x1>` - Barotropic adjustment problem on latitude-longitude
   grid with 128x64 grid points (2.8\ :sup:`o` resolution).

#. :filelink:`adjustment.cs-32x32x1 <verification/adjustment.cs-32x32x1>` -
   Barotropic adjustment problem on cube sphere grid with 32x32 points per face
   (roughly 2.8\ :sup:`o` resolution) with a rectangular island at the equator.
   Note that "blank tiles" are used in the MPI test
   (:filelink:`data.exch2.mpi <verification/adjustment.cs-32x32x1/input/data.exch2.mpi>`).
   Also contains a non-linear free-surface, atmospheric like, barotropic
   adjustment version (:filelink:`input.nlfs <verification/adjustment.cs-32x32x1/input.nlfs>`).

#. :filelink:`advect_cs <verification/advect_cs>` - 2-D passive advection test on cube sphere
   grid (32x32 grid points per face, roughly 2.8\ :sup:`o` resolution).

#. :filelink:`advect_xy <verification/advect_xy>` - 2-D (horizontal plane) passive
   advection test on Cartesian grid. Also contains an additional setup using Adams-Bashforth 3
   (:filelink:`input.ab3_c4 <verification/advect_xy/input.ab3_c4>`).

#. :filelink:`advect_xz <verification/advect_xz>` - 2-D (vertical plane) passive advection
   test on Cartesian grid. Also contains an additional setup using non-linear free-surface
   with divergent barotropic flow and implicit vertical advection (:filelink:`input.nlfs <verification/advect_xz/input.nlfs>`),
   and a setup using piecewise quartic ("mono" and "weno" limiter)
   advection schemes (:filelink:`input.pqm <verification/advect_xz/input.pqm>`).

#. :filelink:`aim.5l_cs <verification/aim.5l_cs>` - 5-level intermediate atmospheric physics, global
   configuration on cube sphere grid (32x32 grid points per face,
   roughly 2.8\ :sup:`o` resolution).
   Also contains an additional setup with a slab-ocean and
   thermodynamic sea ice (:filelink:`input.thSI <verification/aim.5l_cs/input.thSI>`).

#. :filelink:`aim.5l_Equatorial_Channel <verification/aim.5l_Equatorial_Channel>` - 5-level intermediate atmospheric
   physics, 3-D equatorial channel configuration.

#. :filelink:`aim.5l_LatLon <verification/aim.5l_LatLon>` - 5-level intermediate atmospheric physics, global
   configuration, on latitude-longitude grid with 128x64x5 grid points
   (2.8\ :sup:`o` resolution).

#. :filelink:`atm_gray <verification/atm_gray>` - gray atmospheric physics configuration using
   :filelink:`atm_phys <pkg/atm_phys>` package, on cube sphere grid (32x32 grid points per face)
   with 26 pressure levels. This aquaplanet-like experiment has interactive SST with a prescribed,
   time-invariant Q-flux.
   Also contains a secondary setup (:filelink:`input.ape <verification/atm_gray/input.ape>`)
   with prescribed idealized SST from Aqua-Planet Experiment
   (APE) project (Neale and Hoskins, 2001 :cite:`neale:01`).

#. :filelink:`cfc_example <verification/cfc_example>` - Global ocean with online computation and advection
   of CFC11 and CFC12.

#. :filelink:`cheapAML_box <verification/cheapAML_box>` - Example using cheap atmospheric mixed layer
   (:filelink:`cheapaml <pkg/cheapaml>`) package.

#. :filelink:`cpl_aim+ocn <verification/cpl_aim+ocn>` - Coupled ocean-atmosphere realistic configuration
   on cubed-sphere cs32 horizontal grid, using intermediate atmospheric
   physics (:filelink:`pkg/aim_v23`) thermodynamic seaice (:filelink:`pkg/thsice`) and land
   packages.
   Also contains an additional setup with seaice dynamics
   (:filelink:`input_cpl.icedyn  <verification/cpl_aim+ocn/input_cpl.icedyn>`,
   :filelink:`input_atm.icedyn  <verification/cpl_aim+ocn/input_atm.icedyn>`,
   :filelink:`input_ocn.icedyn  <verification/cpl_aim+ocn/input_ocn.icedyn>`).

#. :filelink:`deep_anelastic <verification/deep_anelastic>` - Convection simulation on a giant
   planet: relaxes both the Boussinesq approximation (anelastic) and the thin atmosphere
   approximation (deep atmosphere).

#. :filelink:`dome <verification/dome>` - Idealized 3-D test of a density-driven bottom current
   (Denmark Overflow Mixing and Entrainment experiment).

#. :filelink:`exp2 <verification/exp2>` - Old version of the global ocean experiment (no GM, no
   partial-cells). Also contains an additional setup with rigid lid
   (:filelink:`input.rigidLid <verification/exp2/input.rigidLid>`).

#. :filelink:`exp4 <verification/exp4>` - Flow over a Gaussian bump in open-water or channel
   with open boundaries. Also contains additional setups:

   - using non-linear free-surface (:filelink:`input.nlfs <verification/exp4/input.nlfs>`).

   - using Stevens (1990) :cite:`stevens:90` boundary conditions
     (:filelink:`input.stevens <verification/exp4/input.stevens>`).

   - a simple example using float (:filelink:`flt <pkg/flt>`) package
     (:filelink:`input.with_flt <verification/exp4/input.with_flt>`,
     formerly :filelink:`flt_example <verification/flt_example>`).

#. :filelink:`fizhi-cs-32x32x40 <verification/fizhi-cs-32x32x40>` - Global atmospheric simulation with realistic
   topography, 40 vertical levels, a cubed sphere grid and the full atmospheric physics package.

#. :filelink:`fizhi-cs-aqualev20 <verification/fizhi-cs-aqualev20>` - Global atmospheric simulation on an aqua
   planet with full atmospheric physics. Run is perpetual March with an
   analytical SST distribution. This is the configuration used for the
   `Aqua-Planet Experiment Project (APE) <http://www.met.reading.ac.uk/~mike/ape/>`_ ,
   see also Neale and Hoskins (2001) :cite:`neale:01`.

#. :filelink:`fizhi-gridalt-hs <verification/fizhi-gridalt-hs>` - Global atmospheric simulation Held-Suarez
   (1994) :cite:`held-suar:94` forcing, with the physical forcing and the dynamical forcing
   running on different vertical grids.

#. :filelink:`front_relax <verification/front_relax>` - Relaxation of an 2-D (:math:`y-z`) ocean thermal front (test of
   Gent and McWilliams scheme). Also contains additional setups:

   - using the identical setup except with pressure as the vertical coordinate instead of height
     (:filelink:`input.in_p <verification/front_relax/input.in_p>`)

   - using the boundary-value problem method (Ferrari et al. 2010 :cite:`ferrari:10`)
     (:filelink:`input.bvp <verification/front_relax/input.bvp>`)

   - with mixed-layer eddy parameterization (Ferrari and McWilliams
     2008 :cite:`ferrari:08`) (:filelink:`input.mxl <verification/front_relax/input.mxl>`)

   - with dry-cell at the top and a sloping bottom
     (:filelink:`input.top <verification/front_relax/input.top>`).

#. :filelink:`global_ocean.90x40x15 <verification/global_ocean.90x40x15>` -
   Global ocean simulation at 4\ :sup:`o`\ x4\ :sup:`o` resolution.
   Similar to :ref:`tutorial_global_oce_latlon <sec_global_oce_latlon>`, but
   using :math:`z^*` coordinates with quasi-non-hydrostatic and non-hydrostatic metric
   terms. This experiment illustrates the use of :filelink:`sbo </pkg/sbo>` package.
   Note that "blank tiles" are used in the MPI test (:filelink:`data.exch2.mpi <verification/global_ocean.90x40x15/input/data.exch2.mpi>`).
   Also contains additional setups:

   - using :filelink:`down-slope package <pkg/down_slope>` (:filelink:`input.dwnslp <verification/global_ocean.90x40x15/input.dwnslp>`)

   - using package :filelink:`ggl90 <pkg/ggl90>` scheme (Gaspar et al. 1990 :cite:`gas-eta:90`) with parameterized tidal and wind
     energy input into vertical mixing (:filelink:`input.idemix <verification/global_ocean.90x40x15/input.idemix>`).

#. :filelink:`global_ocean.cs32x15 <verification/global_ocean.cs32x15>` - Global ocean experiment on the cubed
   sphere grid. Also contains additional setups:

   - non-hydrostatic with biharmonic viscosity (:filelink:`input.viscA4 <verification/global_ocean.cs32x15/input.viscA4>`)

   - using thermodynamic sea ice and bulk force (:filelink:`input.thsice <verification/global_ocean.cs32x15/input.thsice>`)

   - using both thermodynamic (:filelink:`pkg/thsice`) and dynamic (:filelink:`pkg/seaice`) sea ice packages
     with :filelink:`exf <pkg/exf>` package (:filelink:`input.icedyn <verification/global_ocean.cs32x15/input.icedyn>`)

   - using thermodynamic and dynamic (:filelink:`pkg/seaice`) sea ice with :filelink:`exf <pkg/exf>` package
     package (:filelink:`input.seaice <verification/global_ocean.cs32x15/input.seaice>`).

   - using pressure as vertical coordinate, with :filelink:`ggl90 <pkg/ggl90>` scheme (Gaspar et al. 1990 :cite:`gas-eta:90`)
     and dynamic and thermodynamic seaice (:filelink:`pkg/seaice`) package and :filelink:`exf <pkg/exf>`
     package (:filelink:`input.in_p <verification/global_ocean.cs32x15/input.in_p>`)

#. :filelink:`global_ocean_ebm <verification/global_ocean_ebm>` - Global ocean experiment on a lat-lon grid,
   similar to :ref:`tutorial_global_oce_latlon <sec_global_oce_latlon>` experiment
   but using other surface forcing pkg, such as from a zonally averaged atmospheric energy balance model
   (:filelink:`ebm <pkg/ebm>` package). Also contains additional setups:

   - using the :filelink:`exf <pkg/exf>` package with :filelink:`exf <pkg/exf>` interpolation
     (:filelink:`input.w_exf <verification/global_ocean_ebm/input.w_exf>`,
     formerly :filelink:`global_with_exf <verification/global_with_exf>`).

   - same as above with yearly :filelink:`exf <pkg/exf>` fields
     (:filelink:`input.yearly <verification/global_ocean_ebm/input.yearly>`,
     formerly in :filelink:`global_with_exf <verification/global_with_exf>`).


#. :filelink:`global_oce_biogeo_bling  <verification/global_oce_biogeo_bling >` - Global ocean biogeochemistry simulation,
   based on :ref:`sub_global_oce_biogeo` but using package :filelink:`bling <pkg/bling>` instead of the :ref:`DIC <sub_pkg_dic>` package.

#. :filelink:`halfpipe_streamice<verification/halfpipe_streamice>` - Example using package :filelink:`streamice <pkg/streamice>`.

#. :filelink:`hs94.128x64x5 <verification/hs94.128x64x5>` - 3-D atmosphere dynamics on lat-lon grid, using
   Held and Suarez (1994) :cite:`held-suar:94` forcing.

#. :filelink:`hs94.1x64x5 <verification/hs94.1x64x5>` - Zonal averaged atmosphere dynamics using Held and
   Suarez (1994) :cite:`held-suar:94` forcing.

#. :filelink:`hs94.cs-32x32x5 <verification/hs94.cs-32x32x5>` - 3-D atmosphere dynamics using Held and Suarez
   (1994) :cite:`held-suar:94` forcing on the cubed sphere, similar to
   :ref:`tutorial_held_suarez_cs <sec_held_suarez_cs>` experiment but using linear free-surface
   and only 5 levels. Also contains an additional setup with implicit internal gravity
   waves treatment and Adams-Bashforth 3 (:filelink:`input.impIGW <verification/hs94.cs-32x32x5/input.impIGW>`).

#. :filelink:`ideal_2D_oce <verification/ideal_2D_oce>` - Idealized 2-D global ocean simulation on an aqua
   planet.

#. :filelink:`internal_wave <verification/internal_wave>` - Ocean internal wave forced by open boundary
   conditions. Also contains an additional setup using :filelink:`pkg/kl10` (see :numref:`sub_phys_pkg_kl10`,
   Klymak and Legg 2010 :cite:`klymaklegg10`) (:filelink:`input.kl10 <verification/internal_wave/input.kl10>`).

#. :filelink:`inverted_barometer <verification/inverted_barometer>` - Simple test of atmospheric pressure loading with
   radially symmetric Bessel-function geometry in a quadratic domain.

#. :filelink:`isomip <verification/isomip>` - ISOMIP-like setup (Ice Shelf Ocean Model Intercomparison Project experiment 0)
   including ice-shelf cavities (:filelink:`pkg/shelfice`). Also contains additional setups:

   - with “htd” (Hellmer's thermodynamics, Hellmer 1989 :cite:`hellmer:89`) (:filelink:`input.htd <verification/isomip/input.htd>`).

   - using package :filelink:`icefront <pkg/icefront>` (:filelink:`input.icefront <verification/isomip/input.icefront>`)

   - using package :ref:`OBCS <sub_phys_pkg_obcs>` enabled to balance surface mass (freshwater and ice shelf mass flux)
     input through open boundaries (:filelink:`input.obcs <verification/isomip/input.obcs>`).

#. :filelink:`lab_sea <verification/lab_sea>` - Regional (2\ :sup:`o`\ x2\ :sup:`o`) Labrador Sea simulation on a lat-lon grid
   using :filelink:`pkg/seaice`. Also contains additional setups:

   - using the simple “free-drift” assumption for sea ice (:filelink:`input.fd <verification/lab_sea/input.fd>`)

   - using :ref:`aEVP dynamics <para_phys_pkg_seaice_EVPstar>` (instead of :ref:`LSR solver <para_phys_pkg_seaice_LSRJFNK>`)
     and Hibler and Bryan (1987) :cite:`hibler:87` sea ice ocean stress (:filelink:`input.hb87 <verification/lab_sea/input.hb87>`)

   - using package :filelink:`salt_plume <pkg/salt_plume>` (:filelink:`input.salt_plume <verification/lab_sea/input.salt_plume>`).

   - ice-free eastern subtropical North Atlantic box (:filelink:`input.natl_box <verification/lab_sea/input.natl_box>`,
     formerly :filelink:`natl_box <verification/natl_box>`).

   - same as above with passive tracers (:filelink:`pkg/ptracers`) using package :filelink:`longstep <pkg/longstep>`
     to speed up integration time (:filelink:`input.longstep <verification/lab_sea/input.longstep>`,
     formerly in :filelink:`natl_box <verification/natl_box>`).

#. :filelink:`matrix_example <verification/matrix_example>` - Test of experimental method to accelerate
   convergence towards equilibrium.

#. :filelink:`MLAdjust <verification/MLAdjust>` - Simple tests of different viscosity formulations
   in a zonally reentrant, flat-bottom channel. Contains additional setups; see
   :filelink:`verification/MLAdjust/README` for a listing of different viscosity settings in these experiments:

   - :filelink:`input.A4FlxF <verification/MLAdjust/input.A4FlxF>`

   - :filelink:`input.AhFlxF <verification/MLAdjust/input.AhFlxF>`

   - :filelink:`input.AhVrDv <verification/MLAdjust/input.AhVrDv>`

   - :filelink:`input.AhStTn <verification/MLAdjust/input.AhStTn>`

   - :filelink:`input.QGLeith <verification/MLAdjust/input.QGLeith>`

   - :filelink:`input.QGLthGM <verification/MLAdjust/input.QGLthGM>`.

#. :filelink:`offline_exf_seaice <verification/offline_exf_seaice>` - Sea ice on top of oceanic surface layer in
   an idealized channel. Forcing is computed by bulk-formulae
   (:filelink:`pkg/exf`) with temperature relaxation to prescribed SST (i.e., no momentum timestepping in ocean,
   so ocean is "offline", not to be confused with :filelink:`pkg/offline`).
   Also contains additional setups:

   - sea ice dynamics-only using :ref:`JFNK solver <para_phys_pkg_seaice_LSRJFNK>`
     and (:filelink:`pkg/thsice`) advection
     (:filelink:`input.dyn_jfnk <verification/offline_exf_seaice/input.dyn_jfnk>`)

   - sea ice dynamics-only using :ref:`LSR solver <para_phys_pkg_seaice_LSRJFNK>`
     and (:filelink:`pkg/seaice`) advection
     (:filelink:`input.dyn_lsr <verification/offline_exf_seaice/input.dyn_lsr>`)

   - sea ice dynamics-only using :ref:`LSR solver <para_phys_pkg_seaice_LSRJFNK>`,
     elliptical yield curve with :ref:`non-normal flow rule <rheologies_ellnnfr>`
     and (:filelink:`pkg/seaice`) advection
     (:filelink:`input.dyn_ellnnfr <verification/offline_exf_seaice/input.dyn_ellnnfr>`
     and :filelink:`input.dyn_lsr <verification/offline_exf_seaice/input.dyn_lsr>`)

   - sea ice dynamics-only using :ref:`LSR solver <para_phys_pkg_seaice_LSRJFNK>`,
     :ref:`Mohr-Coulomb yieldcurve with elliptical plastic potential <rheologies_MCE>`
     and (:filelink:`pkg/seaice`) advection
     (:filelink:`input.dyn_mce <verification/offline_exf_seaice/input.dyn_mce>` and
     :filelink:`input.dyn_lsr <verification/offline_exf_seaice/input.dyn_lsr>`)

   - sea ice dynamics-only using :ref:`Picard (KRYLOV) solver <para_phys_pkg_seaice_LSRJFNK>`,
     :ref:`parabolic lens yieldcurve <rheologies_PL>`
     and (:filelink:`pkg/thsice`) advection
     (:filelink:`input.dyn_paralens <verification/offline_exf_seaice/input.dyn_paralens>`
     and :filelink:`input.dyn_jfnk <verification/offline_exf_seaice/input.dyn_jfnk>`)

   - sea ice dynamics-only using :ref:`JFNK solver <para_phys_pkg_seaice_LSRJFNK>`, :ref:`teardrop yieldcurve <rheologies_TD>`
     and (:filelink:`pkg/thsice`) advection
     (:filelink:`input.dyn_teardrop <verification/offline_exf_seaice/input.dyn_teardrop>`
     and :filelink:`input.dyn_jfnk <verification/offline_exf_seaice/input.dyn_jfnk>`)

   - sea ice thermodynamics-only using (:filelink:`pkg/seaice`) (:filelink:`input.thermo <verification/offline_exf_seaice/input.thermo>`)

   - sea ice thermodynamics-only using (:filelink:`pkg/thsice`) (:filelink:`input.thsice <verification/offline_exf_seaice/input.thsice>`).

#. :filelink:`seaice_itd <verification/seaice_itd>` - Seaice example using ice thickness distribution (ITD); otherwise
   very similar to :filelink:`offline_exf_seaice <verification/offline_exf_seaice>`.
   Also contains additional setups; see
   :filelink:`verification/seaice_itd/README` for details of these setups:

   - :filelink:`input.thermo <verification/seaice_itd/input.thermo>`

   - :filelink:`input.lipscomb07 <verification/seaice_itd/input.lipscomb07>`.

#. :filelink:`seaice_obcs <verification/seaice_obcs>` - Similar to :filelink:`lab_sea <verification/lab_sea>`
   (:filelink:`input.salt_plume <verification/lab_sea/input.salt_plume>`)
   experiment with only a fraction of the domain and open boundary
   conditions derived from :filelink:`lab_sea <verification/lab_sea>` experiment.
   Also contains additional setups:

   - includes relaxation of seaice variables (:filelink:`input.seaiceSponge <verification/seaice_obcs/input.seaiceSponge>`)

   - includes tidal velocity forcing (:filelink:`input.tides <verification/seaice_obcs/input.tides>`).

#. :filelink:`shelfice_2d_remesh <verification/shelfice_2d_remesh>` - Simple experiment to test (:filelink:`pkg/shelfice`)
   vertical remeshing code in 2-D idealized-geometry setup.

#. :filelink:`short_surf_wave <verification/short_surf_wave>` - Short surface wave adjustment (non-hydrostatic)
   in homogeneous 2-D vertical section (:math:`x-z`).

#. :filelink:`so_box_biogeo <verification/so_box_biogeo>` - Open boundary Southern Ocean box around Drake
   Passage, using same model parameters and forcing as experiment
   :ref:`tutorial_global_oce_biogeo <sub_global_oce_biogeo>` from which initial conditions and open boundary
   conditions have been extracted. Also contains additional setups:

   - using the SolveSAPHE algorithm (Munhoven 2013 :cite:`munhoven:13`) to determine oceanic
     pH (:filelink:`input.saphe <verification/so_box_biogeo/input.saphe>`)

   - using the calcite-saturation code (with 3-D pH and silica input file) with the original pH solver
     (:filelink:`input.caSat0 <verification/so_box_biogeo/input.caSat0>`)

   - using the calcite-saturation code with the Munhoven "FAST" solver (:filelink:`input.caSat3 <verification/so_box_biogeo/input.caSat3>`)

#. :filelink:`solid-body.cs-32x32x1 <verification/solid-body.cs-32x32x1>` - Solid body rotation test for cube sphere
   grid.

#. :filelink:`tutorial_deep_convection <verification/tutorial_deep_convection>` - Experiment as described
   in :numref:`sec_deep_convection`, also contains an additional setup
   using the Smagorinisky (1963) :cite:`smag:63` viscosity scheme
   (:filelink:`input.smag3d <verification/tutorial_deep_convection/input.smag3d>`).

#. :filelink:`tutorial_plume_on_slope <verification/tutorial_plume_on_slope>` - Experiment as described in
   :numref:`tutorial_plume_on_slope`, also contains an additional setup
   using the logarithmic law of the wall to compute the bottom drag coefficient
   (:filelink:`input.roughBot <verification/tutorial_plume_on_slope/input.roughBot>`).

#. :filelink:`vermix <verification/vermix>` - Simple test in a small domain (3 columns) for ocean
   vertical mixing schemes. The standard setup (:filelink:`input <verification/vermix/input>`) uses the :ref:`KPP scheme <sub_phys_pkg_kpp>`
   Large et al. (1994) :cite:`lar-eta:94`. Also contains additional setups:

   - with double diffusion scheme from :ref:`KPP <sub_phys_pkg_kpp>` (:filelink:`input.dd <verification/vermix/input.dd>`)

   - with package :filelink:`ggl90 <pkg/ggl90>` scheme (Gaspar et al. 1990 :cite:`gas-eta:90`) scheme
     (:filelink:`input.ggl90 <verification/vermix/input.ggl90>`)

   - with :filelink:`ggl90 <pkg/ggl90>` package and parameterized Langmuir circulation effects
     (:varlink:`useLANGMUIR` ``= .TRUE.`` in ``data.ggl90``,
     :filelink:`input.gglLC <verification/vermix/input.gglLC>`)

   - with Mellor and Yamada (1982) :cite:`mellor:82` level 2 (:filelink:`pkg/my82`)
     scheme (:filelink:`input.my82 <verification/vermix/input.my82>`)

   - with Paluszkiewicz and Romea (1997) :cite:`pal-rom:97` (:filelink:`pkg/opps`) scheme
     (:filelink:`input.opps <verification/vermix/input.opps>`)

   - with Pacanowski and Philander (1981) :cite:`pacanowski:81` (:filelink:`pkg/pp81`)
     scheme (:filelink:`input.pp81 <verification/vermix/input.pp81>`).

.. _subsec_add_expts_adj:

Additional Example Experiments: Adjoint Model Setups
====================================================

Unless stated otherwise, the physical setup of the adjoint run is identical to the forward run, see description above.
TAF adjoint setups require building with directory ``code_ad`` with input directory ``input_ad``, whereas OpenAD requires
directories ``code_oad`` and ``input_oad`` respectively.

#. :filelink:`1D_ocean_ice_column <verification/1D_ocean_ice_column>` - Based on standard forward experiment,
   TAF adjoint setup, uses package :filelink:`ecco <pkg/ecco>`.

#. :filelink:`bottom_ctrl_5x5 <verification/bottom_ctrl_5x5>` - TAF adjoint
   test using the bottom topography as the control parameter, uses package
   :filelink:`ecco <pkg/ecco>` and "not self-adjoint" version of cg2d:
   :filelink:`cg2d_nsa.F <model/src/cg2d_nsa.F>`.
   Also contains an additional TAF adjoint setup
   that uses default :filelink:`cg2d.F <model/src/cg2d.F>` with a hand-written
   full (manual) adjoint routine :filelink:`cg2d_mad.F
   <pkg/autodiff/cg2d_mad.F>` (:filelink:`input_ad.facg2d
   <verification/bottom_ctrl_5x5/input_ad.facg2d>`)

#. :filelink:`global_ocean.90x40x15 <verification/global_ocean.90x40x15>` - Based on standard forward experiment,
   TAF and OpenAD adjoint setups. Also contains additional TAF adjoint setups:

   - with bottom drag as a control and manual adjoint :filelink:`cg2d_mad.F
     <pkg/autodiff/cg2d_mad.F>` (:filelink:`input_ad.bottomdrag <verification/global_ocean.90x40x15/input_ad.bottomdrag>`)

   - with :math:`\kappa_{GM}` as a control (:filelink:`input_ad.kapgm <verification/global_ocean.90x40x15/input_ad.kapgm>`)

   - with :math:`\kappa_{Redi}` as a control (:filelink:`input_ad.kapredi <verification/global_ocean.90x40x15/input_ad.kapredi>`).

#. :filelink:`global_ocean.cs32x15 <verification/global_ocean.cs32x15>` - Based on standard forward experiment, TAF adjoint setup.
   Also contains additional TAF adjoint setups:

   - using thermodynamic-dynamic sea ice (:filelink:`input_ad.seaice <verification/global_ocean.cs32x15/input_ad.seaice>`).

   - same as above but without adjoint sea ice dynamics
     (:filelink:`input_ad.seaice_dynmix <verification/global_ocean.cs32x15/input_ad.seaice_dynmix>`).

   - using thermodynamic sea ice from :filelink:`pkg/thsice`
     (:filelink:`input_ad.thsice <verification/global_ocean.cs32x15/input_ad.thsice>`).

#. :filelink:`global_ocean_ebm <verification/global_ocean_ebm>` - Based on standard forward experiment, TAF adjoint setup.

#. :filelink:`global_oce_biogeo_bling <verification/global_oce_biogeo_bling>` - Based on standard forward experiment,
   TAF adjoint setup, uses package :filelink:`ecco <pkg/ecco>`.

#. :filelink:`global_oce_latlon <verification/global_oce_latlon>` - Simple adjoint experiment (used also to test OpenAD compiler), TAF and OpenAD adjoint setups.
   Also contains additional TAF and OpenAD adjoint setups:

   - using package :filelink:`ggl90 <pkg/ggl90>`
     (:filelink:`input_ad.ggl90 <verification/global_oce_latlon/input_ad.ggl90>`,
     :filelink:`input_oad.ggl90 <verification/global_oce_latlon/input_oad.ggl90>`).

   - using package :filelink:`kpp <pkg/kpp>` (:filelink:`input_oad.kpp <verification/global_oce_latlon/input_oad.kpp>`).

   - using package :filelink:`exf <pkg/exf>` (:filelink:`input_ad.w_exf <verification/global_oce_latlon/input_ad.w_exf>`,
     formerly :filelink:`global_with_exf <verification/global_with_exf>`)

#. :filelink:`halfpipe_streamice<verification/halfpipe_streamice>` - Based on standard forward experiment, TAF and OpenAD adjoint setups.

#. :filelink:`hs94.1x64x5 <verification/hs94.1x64x5>` - Based on standard forward experiment, TAF and OpenAD adjoint setups.

#. :filelink:`isomip <verification/isomip>` - Based on standard forward experiment, TAF and OpenAD adjoint setups.
   Also contains additional TAF adjoint setup
   with “htd” (Hellmer's thermodynamics, Hellmer 1989 :cite:`hellmer:89`) (:filelink:`input_ad.htd <verification/isomip/input_ad.htd>`).

#. :filelink:`lab_sea <verification/lab_sea>` - Based on standard forward experiment, TAF adjoint setup, uses
   package :filelink:`ecco <pkg/ecco>` and :ref:`divided adjoint (DIVA) <sec_autodiff_diva>`.
   Also contains additional TAF adjoint setups:

   - without seaice dynamics (:filelink:`input_ad.noseaicedyn <verification/lab_sea/input_ad.noseaicedyn>`).

   - without seaice altogether (:filelink:`input_ad.noseaice <verification/lab_sea/input_ad.noseaice>`).

#. :filelink:`obcs_ctrl <verification/obcs_ctrl>` - Adjoint test using open boundary conditions as
   control parameters, uses package :filelink:`ecco <pkg/ecco>`.

#. :filelink:`offline_exf_seaice <verification/offline_exf_seaice>` - Based on standard forward experiment, TAF adjoint setup.
   Also contains additional TAF adjoint setup
   with sea ice thermodynamics-only using :filelink:`pkg/thsice` (:filelink:`input_ad.thsice <verification/offline_exf_seaice/input_ad.thsice>`).

#. :filelink:`tutorial_dic_adjoffline <verification/tutorial_dic_adjoffline>` - TAF adjoint setup of offline form of passive tracers coupled
   to the dissolved inorganic carbon biogeochemistry model (currently NOT documented as a tutorial experiment).

#. :filelink:`tutorial_global_oce_biogeo <verification/tutorial_global_oce_biogeo>` - Based on forward experiment described
   in :numref:`sub_global_oce_biogeo`, TAF and OpenAD adjoint setups.

#. :filelink:`tutorial_tracer_adjsens <verification/tutorial_tracer_adjsens>` - Based on adjoint experiment
   described in :numref:`sec_tracer_adj_sens`, contains an additional TAF setup using Second Order Moment (SOM)
   advection scheme (:filelink:`input_ad.som81 <verification/tutorial_tracer_adjsens/input_ad.som81>`).
