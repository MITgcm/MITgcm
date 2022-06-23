.. _sub_phys_pkg_streamice:

STREAMICE Package
-----------------


Author: Daniel Goldberg

.. _ssub_phys_pkg_streamice_intro:

Introduction
~~~~~~~~~~~~

Package :filelink:`STREAMICE <pkg/streamice>` provides a dynamic land ice model for MITgcm.
It was created primarily to develop a TAF- and OpenAD-generated ice model adjoint
and to provide synchronous ice-ocean coupling through the :filelink:`SHELFICE <pkg/shelfice>` package.
It solves a set of dynamic equations appropriate for floating ice-shelf
flow as well as ice-stream and slower ice-sheet flow. It has been tested
at the scale of one or several ice streams, but has not been tested at the continental scale.


.. _ssub_phys_pkg_streamice_config:
  
STREAMICE configuration
~~~~~~~~~~~~~~~~~~~~~~~

Compile-time options
^^^^^^^^^^^^^^^^^^^^

:filelink:`pkg/streamice` can be included on at compile
time in the ``packages.conf`` file by adding a line ``streamice``  (see :numref:`using_packages`).

Parts of the :filelink:`pkg/streamice`  code can be enabled or disabled at compile time via
CPP flags. These options are set in :filelink:`STREAMICE_OPTIONS.h <pkg/streamice/STREAMICE_OPTIONS.h>`. 


.. tabularcolumns:: |\Y{.475}|\Y{.1}|\Y{.45}|
.. table:: CPP flags used by :filelink:`pkg/streamice`.
   :name: tab_phys_pkg_streamice_cpp

   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | CPP Flag Name                                 | Default | Description                                                                                                          |
   +===============================================+=========+======================================================================================================================+
   | :varlink:`STREAMICE_CONSTRUCT_MATRIX`         | #define | explicit construction of matrix for Picard iteration for velocity                                                    |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`STREAMICE_HYBRID_STRESS`            | #undef  | use L1L2 formulation for stress balance (default shallow shelf approx.)                                              |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`USE_ALT_RLOW`                       | #undef  | use package array for rLow rather than model                                                                         |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`STREAMICE_GEOM_FILE_SETUP`          | #undef  | use files rather than parameters in :varlink:`STREAMICE_PARM03` to configure boundaries                              |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_PETSC`                        | #undef  | enable interface to PETSc for velocity solver matrix solve                                                           |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`STREAMICE_COULOMB_SLIDING`          | #undef  | enable basal sliding of the form :eq:`coul_eqn`                                                                      |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

.. | :varlink:`STREAMICE_SMOOTH_FLOATATION`        | #undef  | subgrid parameterization of transition across the grounding line                                                     |
.. +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
  

.. _ssub_phys_pkg_streamice_runtime:

Enabling the package
^^^^^^^^^^^^^^^^^^^^

Once it has been compiled, :filelink:`pkg/streamice` is switched on/off at run-time by setting :varlink:`useSTREAMICE` to ``.TRUE.`` in file ``data.pkg``.

Runtime parmeters: general flags and parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Run-time parameters are set in file ``data.streamice`` (read in :filelink:`streamice_readparms.F <pkg/streamice/streamice_readparms.F>`).
General :filelink:`pkg/streamice` parameters are set under :varlink:`STREAMICE_PARM01` as described in :numref:`tab_phys_pkg_streamice_runtimeparms`.


.. tabularcolumns:: |\Y{.3}|\Y{.125}|\Y{.6}|
.. table:: Run-time parameters and default values (defined under :varlink:`STREAMICE_PARM01` namelist)
   :name: tab_phys_pkg_streamice_runtimeparms

   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | Parameter                                 | Default                      | Description                                                                                                        |
   +===========================================+==============================+====================================================================================================================+
   | :varlink:`streamice_density`              |     910                      | the (uniform) density of land ice (kg/m\ :sup:`3`)                                                                 |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_density_ocean_avg`    |     1024                     | the (uniform) density of ocean (kg/m\ :sup:`3`)                                                                    |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`n_glen`                         |     3                        | Glen's Flow Law exponent (non-dim.)                                                                                |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`eps_glen_min`                   |     1e-12                    | minimum strain rate in Glen's Law (:math:`\varepsilon_0`, yr\ :sup:`-1`)                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`eps_u_min`                      |     1e-6                     | minimum speed in nonlinear sliding law (:math:`u_0`, m/yr)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`n_basal_friction`               |     0                        | exponent in nonlinear sliding law (non-dim.)                                                                       |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_cg_tol`               |     1e-6                     | tolerance of conjugate gradient of linear solve of Picard iteration for velocity                                   |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_lower_cg_tol`         |     TRUE                     | lower CG tolerance when nonlinear residual decreases by fixed factor                                               |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_max_cg_iter`          |     2000                     | maximum iterations in linear solve                                                                                 |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_maxcgiter_cpl`        |     0                        | as above when coupled with :filelink:`pkg/shelfice`                                                                |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_nonlin_tol`           |     1e-6                     | tolerance of nonlinear residual for velocity (relative to initial)                                                 |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_max_nl_iter`          |     100                      | maximum Picard iterations in solve for velocity                                                                    |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_maxnliter_cpl`        |     0                        | as above when coupled with :filelink:`pkg/shelfice`                                                                |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_nonlin_tol_fp`        |     1e-6                     | tolerance of relative change for velocity iteration (relative to magnitude)                                        |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_err_norm`             |    0                         | type of norm evaluated for error (:math:`p` in :math:`p`-norm; 0 is :math:`\infty`)                                |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_chkfixedptconvergence`|    FALSE                     | terminate velocity iteration based on relative change per iteration                                                |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_chkresidconvergence`  |    TRUE                      | terminate velocity iteration based on residual                                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicethickInit`             |    FILE                      | method by which to initialize thickness (``FILE`` or ``PARAM``)                                                    |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicethickFile`             |    :kbd:`' '`                | thickness initialization file, in meters (rather than parameters in :varlink:`STREAMICE_PARM03`)                   |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_move_front`           |    FALSE                     | allow ice shelf front to advance                                                                                   |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_calve_to_mask`        |    FALSE                     | if :varlink:`streamice_move_front` TRUE do not allow to advance beyond :varlink:`streamice_calve_mask`             |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`STREAMICE_use_log_ctrl`         |    FALSE                     | specify :math:`C` and :math:`B` via their logarithm rather than square root                                        | 
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicecalveMaskFile`         |    :kbd:`' '`                | file to initialize :varlink:`streamice_calve_mask`                                                                 |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_diagnostic_only`      |    FALSE                     | do not update ice thickness (velocity solve only)                                                                  |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_CFL_factor`           |    0.5                       | CFL factor which determine maximum time step for thickness sub-cycling                                             |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_adjDump`              |    0.0                       | frequency (s) of writing of adjoint fields to file (TAF only)                                                      |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicebasalTracConfig`       |    UNIFORM                   | method by which to initialize basal traction (``FILE`` or ``UNIFORM``)                                             |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicebasalTracFile`         |    :kbd:`' '`                | basal trac initialization file (see :ref:`ssub_phys_pkg_streamice_units` for units)                                |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`C_basal_fric_const`             |    31.71                     | uniform basal traction value (see :ref:`ssub_phys_pkg_streamice_units` for units)                                  |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceGlenConstConfig`       |    UNIFORM                   | method by which to initialize Glen's constant (``FILE`` or ``UNIFORM``)                                            |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceGlenConstFile`         |    :kbd:`' '`                | Glen's constant initialization file (see :ref:`ssub_phys_pkg_streamice_units` for units)                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`B_glen_isothermal`              |    9.461e-18                 | uniform Glen's constant value (see :ref:`ssub_phys_pkg_streamice_units` for units)                                 |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceBdotFile`              |    :kbd:`' '`                | file to initialize time-indep melt rate (m/yr)                                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceBdotTimeDepFile`       |   :kbd:`' '`                 | file to initialize time-varying melt rate (m/yr), based on :varlink:`streamice_forcing_period`                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceTopogFile`             |    :kbd:`' '`                | topography initialization file (m); requires #define :varlink:`USE_ALT_RLOW`                                       |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceHmaskFile`             |   :kbd:`' '`                 | :varlink:`streamice_hmask` initialization file; requires #define :varlink:`STREAMICE_GEOM_FILE_SETUP`              |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceuFaceBdryFile`         |     :kbd:`' '`               | :varlink:`streamice_ufacemask_bdry` initialization file; requires #define :varlink:`STREAMICE_GEOM_FILE_SETUP`     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicevFaceBdryFile`         |     :kbd:`' '`               | :varlink:`streamice_vfacemask_bdry` initialization file; requires #define :varlink:`STREAMICE_GEOM_FILE_SETUP`     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceuMassFluxFile`         |     :kbd:`' '`               | mass flux at :math:`u`-faces init. file (m\ :sup:`2`\ /yr); requires #define :varlink:`STREAMICE_GEOM_FILE_SETUP`  |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicevMassFluxFile`         |     :kbd:`' '`               | mass flux at :math:`v`-faces init. file (m\ :sup:`2`\ /yr); requires #define :varlink:`STREAMICE_GEOM_FILE_SETUP`  |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceuFluxTimeDepFile`      |     :kbd:`' '`               | time-depend. mass flux at :math:`u`-faces file (m\ :sup:`2`\ /yr);                                                 |
   |                                           |                              | requires #define :varlink:`STREAMICE_GEOM_FILE_SETUP`                                                              |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicevFluxTimeDepFile`      |     :kbd:`' '`               | time-depend. mass flux at :math:`v`-faces file (m\ :sup:`2`\ /yr);                                                 |
   |                                           |                              | requires #define :varlink:`STREAMICE_GEOM_FILE_SETUP`                                                              |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceuNormalStressFile`     |     :kbd:`' '`               | calving front normal stress parm along :math:`u`-faces (non-dim.; see :ref:`ssub_streamice_boundary_stress`)       |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicevNormalStressFile`     |     :kbd:`' '`               | calving front normal stress parm along :math:`v`-faces (non-dim.; see :ref:`ssub_streamice_boundary_stress`)       |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceuShearStressFile`      |     :kbd:`' '`               | calving front normal stress parm along :math:`u`-faces (non-dim.; see :ref:`ssub_streamice_boundary_stress`)       |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicevShearStressFile`      |     :kbd:`' '`               | calving front normal stress parm along :math:`v`-faces (non-dim.; see :ref:`ssub_streamice_boundary_stress`)       |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceuNormalTimeDepFile`    |     :kbd:`' '`               | time-dependent version of :varlink:`streamiceuNormalStressFile`                                                    |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicevNormalTimeDepFile`    |     :kbd:`' '`               | time-dependent version of :varlink:`streamicevNormalStressFile`                                                    |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamiceuShearTimeDepFile`     |     :kbd:`' '`               | time-dependent version of :varlink:`streamiceuShearStressFile`                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamicevShearTimeDepFile`     |     :kbd:`' '`               | time-dependent version of :varlink:`streamicevShearStressFile`                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_adot_uniform`         |   0                          | time/space uniform surface accumulation rate (m/yr)                                                                |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_forcing_period`       |   0                          | file input frequency for streamice time-dependent forcing fields (s)                                               |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_smooth_gl_width`      |   0                          | thickness range parameter in basal traction smoothing across grounding line  (m)                                   |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`streamice_allow_reg_coulomb`    |   FALSE                      | use regularized Coulomb sliding :eq:`coul_eqn`. Requires :varlink:`STREAMICE_COULOMB_SLIDING` CPP option.          |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`STREAMICE_vel_ext`              |   FALSE                      | over-ride velocity calculation with binary file                                                                    |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`STREAMICE_vel_ext`              |   FALSE                      | over-ride velocity calculation with binary file, with velocities applied directly to C-grid.                       |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`STREAMICE_uvel_ext_file`        |   FALSE                      | file to initialise `x`-velocity component (m/a)                                                                    |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`STREAMICE_vvel_ext_file`        |   FALSE                      | file to initialise `y`-velocity component (m/a)                                                                    | 
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
 
.. _ssub_phys_pkg_streamice_domain_setup:

Configuring domain through files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :varlink:`STREAMICE_GEOM_FILE_SETUP` CPP option allows versatility in defining the domain.
With this option, the array :varlink:`streamice_hmask` must be initialized through a file (:varlink:`streamiceHmaskFile`)
as must :varlink:`streamice_ufacemask_bdry` and :varlink:`streamice_vfacemask_bdry`
(through :varlink:`streamiceuFaceBdryFile` and :varlink:`streamicevFaceBdryFile`)
as well as :varlink:`u_flux_bdry_SI` and :varlink:`v_flux_bdry_SI`, volume flux at the boundaries,
where appropriate (through :varlink:`streamiceuMassFluxFile` and :varlink:`streamicevMassFluxFile`).
Thickness must be initialized through a file as well (:varlink:`streamicethickFile`); :varlink:`streamice_hmask`
is set to zero where ice thickness is zero, and boundaries between in-domain and out-of-domain cells
(according to :varlink:`streamice_hmask`) are no-slip by default.

When using this option, it is important that for all internal boundaries,
:varlink:`streamice_ufacemask_bdry` and :varlink:`streamice_vfacemask_bdry` are -1
(this will not be the case if :varlink:`streamiceuFaceBdryFile` and :varlink:`streamicevFaceBdryFile` are undefined). 

In fact, if :varlink:`streamice_hmask` is configured correctly, :varlink:`streamice_ufacemask_bdry`
and :varlink:`streamice_vfacemask_bdry` can be set uniformly to -1, UNLESS there are no-stress or
flux-condition boundaries in the domain. Where :varlink:`streamice_ufacemask_bdry` and :varlink:`streamice_vfacemask_bdry`
are set to -1, they will be overridden at (a) boundaries where :varlink:`streamice_hmask` changes from 1 to -1
(which become no-slip boundaries), and (b) boundaries where :varlink:`streamice_hmask` changes from 1 to 0 (which become calving front boundaries).

An example of domain configuration through files can be found in :filelink:`verification/halfpipe_streamice`.
By default, :filelink:`verification/halfpipe_streamice` is compiled with :varlink:`STREAMICE_GEOM_FILE_SETUP` undefined,
but the user can modify this option. The file :filelink:`verification/halfpipe_streamice/input/data.streamice_geomSetup`
represents an alternative version of :filelink:`verification/halfpipe_streamice/input/data.streamice`
in which the appropriate binary files are specified.

Configuring domain through parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For a very specific type of domain the boundary conditions and initial thickness can be set
via parameters in ``data.streamice``.
Such a domain will be rectangular. In order to use this option, the :varlink:`STREAMICE_GEOM_FILE_SETUP` CPP flag should be undefined.

There are different boundary condition types (denoted within the parameter names) that can be set:

-  ``noflow``: :math:`x`- and :math:`y`-velocity will be zero along this boundary.

-  ``nostress``: velocity normal to boundary will be zero; there will be no tangential stress along the boundary.

-  ``fluxbdry``: a mass volume flux is specified along this boundary, which becomes a boundary condition
   for the thickness advection equation (see :ref:`ssub_phys_pkg_streamice_eqns`). Velocities will be zero.
   The corresponding parameters :varlink:`flux_bdry_val_NORTH`, :varlink:`flux_bdry_val_SOUTH`, 
   :varlink:`flux_bdry_val_EAST` and  :varlink:`flux_bdry_val_WEST` then set the values.

-  ``CFBC``: calving front boundary condition, a Neumann condition based on ice thickness and bed depth,
   is imposed at this boundary (see :ref:`ssub_phys_pkg_streamice_eqns`).
  
Note the above only apply if there is dynamic ice in the cells at the boundary in question.
The boundary conditions are then set by specifying the above conditions over ranges of each
(north/south/east/west) boundary. The division of each boundary should be exhaustive and the ranges should not overlap.
Parameters to initialize boundary conditions (defined under :varlink:`STREAMICE_PARM03` namelist) are listed in :numref:`tab_phys_pkg_streamice_domainparms`.

.. table:: Parameters to initialize boundary conditions (defined under :varlink:`STREAMICE_PARM03` namelist)
   :name: tab_phys_pkg_streamice_domainparms
  
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | Parameter                                 | Default                      | Description                                                                                                        |
   +===========================================+==============================+====================================================================================================================+
   | :varlink:`min_x_noflow_NORTH`             |   0                          | western limit of no-flow region on northern boundary (m)                                                           | 
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_x_noflow_NORTH`             |   0                          | eastern limit of no-flow region on northern boundary (m)                                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_x_noflow_SOUTH`             |   0                          | western limit of no-flow region on southern boundary (m)                                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_x_noflow_SOUTH`             |   0                          | eastern limit of no-flow region on southern boundary (m)                                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_y_noflow_EAST`              |   0                          | southern limit of no-flow region on eastern boundary (m)                                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_y_noflow_EAST`              |   0                          | northern limit of no-flow region on eastern boundary (m)                                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_y_noflow_WEST`              |   0                          | southern limit of no-flow region on western boundary (m)                                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_y_noflow_WEST`              |   0                          | northern limit of no-flow region on eastern boundary (m)                                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_x_nostress_NORTH`           |   0                          | western limit of no-stress region on northern boundary (m)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_x_nostress_NORTH`           |   0                          | eastern limit of no-stress region on northern boundary (m)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_x_nostress_SOUTH`           |   0                          | western limit of no-stress region on southern boundary (m)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_x_nostress_SOUTH`           |   0                          | eastern limit of no-stress region on southern boundary (m)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_y_nostress_EAST`            |   0                          | southern limit of no-stress region on eastern boundary (m)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_y_nostress_EAST`            |   0                          | northern limit of no-stress region on eastern boundary (m)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_y_nostress_WEST`            |   0                          | southern limit of no-stress region on western boundary (m)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_y_nostress_WEST`            |   0                          | northern limit of no-stress region on eastern boundary (m)                                                         |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_x_fluxbdry_NORTH`           |   0                          | western limit of flux-boundary region on northern boundary (m)                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_x_fluxbdry_NORTH`           |   0                          | eastern limit of flux-boundary region on northern boundary (m)                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_x_fluxbdry_SOUTH`           |   0                          | western limit of flux-boundary region on southern boundary (m)                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_x_fluxbdry_SOUTH`           |   0                          | eastern limit of flux-boundary region on southern boundary (m)                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_y_fluxbdry_EAST`            |   0                          | southern limit of flux-boundary region on eastern boundary (m)                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_y_fluxbdry_EAST`            |   0                          | northern limit of flux-boundary region on eastern boundary (m)                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_y_fluxbdry_WEST`            |   0                          | southern limit of flux-boundary region on western boundary (m)                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_y_fluxbdry_WEST`            |   0                          | northern limit of flux-boundary region on eastern boundary (m)                                                     |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_x_CFBC_NORTH`               |   0                          | western limit of calving front condition region on northern boundary (m)                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_x_CFBC_NORTH`               |   0                          | eastern limit of calving front condition region on northern boundary (m)                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_x_CFBC_SOUTH`               |   0                          | western limit of calving front condition region on southern boundary (m)                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_x_CFBC_SOUTH`               |   0                          | eastern limit of calving front condition region on southern boundary (m)                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_y_CFBC_EAST`                |   0                          | southern limit of calving front condition region on eastern boundary  (m)                                          |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_y_CFBC_EAST`                |   0                          | northern limit of calving front condition region on eastern boundary (m)                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`min_y_CFBC_WEST`                |   0                          | southern limit of calving front condition region on western boundary (m)                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`max_y_CFBC_WEST`                |   0                          | northern limit of calving front condition region on eastern boundary (m)                                           |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`flux_bdry_val_SOUTH`            |   0                          | volume flux per width entering at flux-boundary on southern boundary (m\ :sup:`2`\ /a)                             |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`flux_bdry_val_NORTH`            |   0                          | volume flux per width entering at flux-boundary on southern boundary (m\ :sup:`2`\ /a)                             |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`flux_bdry_val_EAST`             |   0                          | volume flux per width entering at flux-boundary on southern boundary (m\ :sup:`2`\ /a)                             |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+
   | :varlink:`flux_bdry_val_WEST`             |   0                          | volume flux per width entering at flux-boundary on southern boundary (m\ :sup:`2`\ /a)                             |
   +-------------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------+

  
.. _ssub_phys_pkg_streamice_descr:
  
Description
~~~~~~~~~~~

.. _ssub_phys_pkg_streamice_eqns:

Equations Solved
^^^^^^^^^^^^^^^^

The model solves for 3 dynamic variables: :math:`x`-velocity
(:math:`u`), :math:`y`-velocity (:math:`v`), and thickness (:math:`h`).
There is also a variable that tracks coverage of fractional cells,
discussed in :ref:`ssub_phys_pkg_streamice_advance`.

By default the model solves the "shallow shelf approximation" (SSA) for
velocity. The SSA is appropriate for floating ice (ice shelf) or ice
flowing over a low-friction bed (e.g., Macayeal (1989) :cite:`Macayeal:89`). The SSA consists
of the :math:`x`-momentum balance:

.. math::
   \partial_x(h\nu(4\dot{\varepsilon}_{xx}+2\dot{\varepsilon}_{yy})) +
   \partial_y(2h\nu\dot{\varepsilon}_{xy}) - \tau_{bx} = \rho g h \frac{\partial s}{\partial x}
   :label: mom_x

the :math:`y`-momentum balance:

.. math::
   \partial_x(2h\nu\dot{\varepsilon}_{xy}) +
   \partial_y(h\nu(4\dot{\varepsilon}_{yy}+2\dot{\varepsilon}_{xx})) - \tau_{by} =
   \rho g h \frac{\partial s}{\partial y}
   :label: mom_y

where :math:`\rho` is ice density, :math:`g` is gravitational acceleration, and :math:`s` is surface elevation. :math:`\nu`,
:math:`\tau_{bi}` and :math:`\dot{\varepsilon}_{ij}` are ice viscosity, basal drag, and the strain rate tensor, respectively, all explained below.

From the velocity field, thickness evolves according to the continuity
equation:

.. math::
   h_t +  \nabla  \cdot(h\vec{u}) = \dot{a}-\dot{b}
   :label: adv_eqn

Where :math:`\dot{b}` is a basal mass balance (e.g., melting due to
contact with the ocean), positive where there is melting. This is a field that can be specified through a file. At the moment surface mass
balance :math:`\dot{a}` can only be set as uniform. Where ice is grounded,
surface elevation is given by

.. math:: s = R + h

where :math:`R(x,y)` is the bathymetry, and the basal elevation
:math:`b` is equal to :math:`R`. If ice is floating, then the assumption
of hydrostasy and constant density gives

.. math:: s = (1-\frac{\rho}{\rho_w}) h,

where :math:`\rho_w` is a representative ocean density, and
:math:`b=-(\rho/\rho_w)h`. Again by hydrostasy, floation is assumed
wherever

.. math:: h \leq -\frac{\rho_w}{\rho}R

is satisfied. Floatation criteria is stored in :varlink:`float_frac_streamice`,
equal to 1 where ice is grounded, and equal to 0 where ice is floating.

The strain rates :math:`\varepsilon_{ij}` are generalized to the case of
orthogonal curvilinear coordinates, to include the "metric" terms that
arise when casting the equations of motion on a sphere or projection on
to a sphere (see :ref:`para_phys_pkg_seaice_discretization`).
Thus

.. math::
   \begin{aligned}
   \dot{\varepsilon}_{xx} = & u_x + k_1 v, \notag \\
   \dot{\varepsilon}_{yy} = & v_y + k_1 u, \notag \\ 
   \dot{\varepsilon}_{xy} = & \frac{1}{2}(u_y+v_x) + k_1 u + k_2 v. \notag \end{aligned}

:math:`\nu` has the form arising from Glen's law

.. math::
   \nu =
   \frac{1}{2}A^{-\frac{1}{n}}\left(\dot{\varepsilon}_{xx}^2+\dot{\varepsilon}_{yy}
   ^2+\dot{\varepsilon}_{xx}\dot{\varepsilon}_{yy}+\dot{\varepsilon}_{xy}^2+\dot{
   \varepsilon}_{\min}^2\right)^{\frac{1-n}{2n}}
   :label: visc_eqn

though the form is slightly different if a hybrid formulation is used. 

Whether :math:`\tau_b` is nonzero depends on whether the floatation
condition is satisfied. Currently this is determined simply on an
instantaneous cell-by-cell basis (unless subgrid interpolation is used),
as is the surface elevation :math:`s`, but possibly this should be
rethought if the effects of tides are to be considered.
:math:`\vec{\tau}_b` has the form

.. math::
   \vec{\tau}_b = C (|\vec{u}|^2+u_{\min}^2)^{\frac{m-1}{2}}\vec{u}.
   :label: tau_eqn
 
Again, the form is slightly different if a hybrid formulation is to be
used, and the velocity refers to sliding velocity (:math:`u_b`).

An alternative to the above "power law" sliding parameterization can be used by
defining the :varlink:`STREAMICE_COULOMB_SLIDING` CPP option and setting 
:varlink:`streamice_allow_reg_coulomb` to ``.TRUE.``:

.. math::
   \vec{\tau}_b = C\frac{|u|^{m}N}{2\left[C^{1/m}|u|+(0.5N)^{1/m}\right]^{m}}u^{-1}\vec{u}
   :label: coul_eqn

where :math:`u` is shorthand for the regularized norm in :eq:`tau_eqn` (or for :math:`u_b` if a hybrid formulation is used). 
:math:`m` is the same exponent as in :eq:`tau_eqn`. :math:`N` is effective pressure:

.. math::
   N = \rho g (h - h_f),
   :label: eff_press

with :math:`h_f` the floatation thickness 

.. math::
   h_f = \max\left(0,-\frac{\rho_w}{\rho}R\right),

where :math:`R` is bed elevation. This formulation was used in the MISMIP+ intercomparison tests :cite:`asay-davis:16`.
:eq:`eff_press` assumes complete hydraulic connectivity to the ocean throughout 
the domain, which is likely only true within a few tens of kilometers of the 
grounding line. With this sliding relation, Coulomb sliding is predominant near the grounding line, with 
the yield strength proportional to height above floatation. Further inland sliding transitions to 
the power law relation in :eq:`tau_eqn`.

The momentum equations are solved together with appropriate boundary
conditions, discussed below. In the case of a calving front boundary
condition (CFBC), the boundary condition has the following form:

.. math::
   (h\nu(4\dot{\varepsilon}_{xx}+2\dot{\varepsilon}_{yy}))n_x +
   (2h\nu\dot{\varepsilon}_{xy})n_y = \frac{1}{2}g \left(\rho h^2 - \rho_w
   b^2\right)n_x   
   :label: cfbc_x

.. math::
   (2h\nu\dot{\varepsilon}_{xy})n_x +
   (h\nu(4\dot{\varepsilon}_{yy}+2\dot{\varepsilon}_{xx}))n_y = \frac{1}{2}g
   \left(\rho h^2 - \rho_w b^2\right)n_y. 
   :label: cfbc_y
 
Here :math:`\vec{n}` is the normal to the boundary, and :math:`b`
is ice base.

Hybrid SIA-SSA stress balance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The SSA does not take vertical shear stress or strain rates (e.g.,
:math:`\sigma_{xz}`, :math:`\partial u/\partial z`) into account.
Although there are other terms in the stress tensor, studies have found
that in all but a few cases, vertical shear and longitudinal stresses
(represented by the SSA) are sufficient to represent glaciological flow.
:filelink:`pkg/streamice` can allow for representation of vertical shear, although the
approximation is made that longitudinal stresses are depth-independent.
The stress balance is referred to as "hybrid" because it is a joining of
the SSA and the "shallow ice approximation" (SIA), which accounts
only for vertical shear. Such hybrid formulations have been shown to be
valid over a larger range of conditions than SSA (Goldberg 2011) :cite:`goldberg:2011`.

In the hybrid formulation, :math:`\overline{u}` and
:math:`\overline{v}`, the depth-averaged :math:`x-` and :math:`y-`
velocities, replace :math:`u` and :math:`v` in :eq:`mom_x`, :eq:`mom_y`, and :eq:`adv_eqn`, and gradients
such as :math:`u_x` are replaced by :math:`(\overline{u})_x`. Viscosity
becomes

.. math::
   \nu =
   \frac{1}{2}A^{-\frac{1}{n}}\left(\dot{\varepsilon}_{xx}^2+\dot{\varepsilon}_{yy}
   ^2+\dot{\varepsilon}_{xx}\dot{\varepsilon}_{yy}+\dot{\varepsilon}_{xy}^2+\frac{1
   }{4}u_z^2+\frac{1}{4}v_z^2+\dot{\varepsilon}_{\min}^2\right)^{\frac{1-n}{2n}}

In the formulation for :math:`\tau_b`, :math:`u_b`, the horizontal
velocity at :math:`u_b` is used instead. The details are given in Goldberg (2011)
:cite:`goldberg:2011`.

.. _ssub_phys_pkg_streamice_advance:

Ice front advance
^^^^^^^^^^^^^^^^^

By default all mass flux across calving boundaries is considered lost. However, it is possible to account
for this flux and potential advance of the ice shelf front. If :varlink:`streamice_move_front` is TRUE, then a partial-area formulation is used.

The algorithm is based on Albrecht et al. (2011) :cite:`Albrecht:2011`. In this scheme,
for empty or partial cells adjacent to a calving front, a **reference** thickness
:math:`h_{\rm ref}` is found, defined as an average over the thickness
of all neighboring cells that flow into the cell. The total volume input over a time step
is added to the volume of ice already in the cell, whose partial area coverage is then updated
based on the volume and reference thickness. If the area coverage reaches 100% in a time step,
then the additional volume is cascaded into adjacent empty or partial cells.

If :varlink:`streamice_calve_to_mask` is TRUE, this sets a limit to how far the front can
advance, even if advance is allowed. The front will not advance into
cells where the array :varlink:`streamice_calve_mask` is not equal to 1. This mask must
be set through a binary input file to allow the front to advance past its initial position.

No calving parameterization is implemented in :filelink:`pkg/streamice`. However,
front advancement is a precursor for such a development to be added.

.. _ssub_phys_pkg_streamice_units:

Units of input files
^^^^^^^^^^^^^^^^^^^^

The inputs for basal traction (:varlink:`streamicebasalTracFile`, :varlink:`C_basal_fric_const`)
and ice stiffness (:varlink:`streamiceGlenConstFile`, :varlink:`B_glen_isothermal`) require specific units.
For ice stiffness (`A` in :eq:`visc_eqn`), :math:`B=A^{-1/n}` is specified; or, more accurately,
its square root :math:`A^{-1/(2n)}` is specified (this is to ensure positivity of `B` by squaring the input).
The units of :varlink:`streamiceGlenConstFile` and :varlink:`B_glen_isothermal` are
:math:`\mathrm{Pa}^{1/2}\ \mathrm{yr}^{1/(2n)}`
where :math:`n` is :varlink:`n_glen`.

:varlink:`streamicebasalTracFile` and :varlink:`C_basal_fric_const` initialize the basal traction
(`C` in :eq:`tau_eqn`). Again :math:`C^{1/2}` is directly specified rather than `C` to ensure positivity. The units are
:math:`\mathrm{Pa}^{1/2} (\mathrm{m }\ \mathrm{yr}^{-1})^{n_b}`
where :math:`n_b` is :varlink:`n_basal_friction`.

Numerical Details
~~~~~~~~~~~~~~~~~

.. figure:: figs/stencil.*
   :width: 50%
   :align: center
   :alt: STREAMICE stencil
   :name: figstencil

   Grid locations of thickness (`h`), velocity (`u,v`), area, and various masks.
   
.. figure:: figs/mask_cover.*
   :width: 50%
   :align: center
   :alt: STREAMICE masks
   :name: figmask_cover

   Hypothetical configuration, detailing the meaning of thickness and velocity
   masks and their role in controlling boundary conditions.

The momentum balance is solved via iteration on viscosity (Goldberg 2011 :cite:`goldberg:2011`). At each iteration,
a linear elliptic differential equation is solved via a finite-element method using bilinear basis functions.
The velocity solution "lives" on cell corners, while thickness "lives" at cell centers (:numref:`figstencil`).
The cell-centered thickness is then evolved using a second-order slope-limited finite-volume scheme,
with the velocity field from the previous solve. To represent the flow of floating ice, basal stress
terms are multiplied by an array :varlink:`float_frac_streamice`, a cell-centered array which determines
where ice meets the floation condition.

The computational domain of :filelink:`pkg/streamice` (which may be smaller than the array/grid as
defined by :filelink:`SIZE.h <model/inc/SIZE.h>` and :filelink:`GRID.h <model/inc/GRID.h>`)
is determined by a number of mask
arrays within :filelink:`pkg/streamice`. They are

-  :math:`hmask` (:varlink:`streamice_hmask`): equal to 1 (ice-covered), 0
   (open ocean), 2 (partly-covered), or -1 (out of domain)

-  :math:`umask` (:varlink:`streamice_umask`): equal to 1 (an "active" velocity
   node), 3 (a Dirichlet node), or 0 (zero velocity)

-  :math:`vmask` (:varlink:`streamice_vmask`): similar to umask

-  :math:`ufacemaskbdry` (:varlink:`streamice_ufacemask_bdry`): equal to -1
   (interior face), 0 (no-slip), 1 (no-stress), 2 (calving stress
   front), or 4 (flux input boundary); when 4, then
   :varlink:`u_flux_bdry_SI` must be initialized, through binary or parameter
   file

-  :math:`vfacemaskbdry` (:varlink:`streamice_vfacemask_bdry`): similar to
   :math:`ufacemaskbdry`

:math:`hmask` is defined at cell centers, like :math:`h`. :math:`umask`
and :math:`vmask` are defined at cell nodes, like velocities.
:math:`ufacemaskbdry` and :math:`vfacemaskbdry` are defined at cell
faces, like velocities in a C-grid - but unless one sets 
``#define`` :varlink:`STREAMICE_GEOM_FILE_SETUP`  in
:filelink:`STREAMICE_OPTIONS.h <pkg/streamice/STREAMICE_OPTIONS.h>`,
the values are only relevant at the boundaries of the grid.

The values of :math:`umask` and :math:`vmask` determine which nodal
values of :math:`u` and :math:`v` are involved in the solve for
velocities. These masks are not configured directly by the user, but are re-initialized based
on :varlink:`streamice_hmask`,   :varlink:`streamice_ufacemask_bdry` and :varlink:`streamice_vfacemask_bdry`
at each time step.  :numref:`figmask_cover`
demonstrates how these values are set in various cells.

With :math:`umask` and :math:`vmask` appropriately initialized, subroutine
:filelink:`streamice_vel_solve.F <pkg/streamice/streamice_vel_solve.F>` can proceed rather generally.
Contributions are only evaluated if :math:`hmask=1` in a given cell, and a given nodal
basis function is only considered if :math:`umask=1` or :math:`vmask=1`
at that node.


Additional Features
~~~~~~~~~~~~~~~~~~~

.. Grounding line parameterization
.. ###############################

.. Representing grounding line movement (change of boundary between grounded and floating ice) is problematic in ice sheet models due to the high resolution required. It has been found that sub-grid treatment of the grounding line can partially alleviate this requirement (Gladstone et al, 2011). STREAMICE implements a simple "smoothing" of the floatation condition. By default, ``float_frac_streamice`` is equal to 0 in cells that satisfy the floatation condition, and 1 elsewhere. If the compile option ``STREAMICE_SMOOTH_FLOATATION2`` is defined, then the array varies smoothly between 0 and 1 in cells where :math:`|h-h_f| < w_{smooth}/2`, where

.. .. math::

..  h_f = -\frac{\rho}{\rho_w}R

.. and :math:`w_{smooth}` is specified by ``streamice_smooth_gl_width``. This modification then smooths the transition from grounded to floating ice with respect to basal stress. It is found that this parameterisation is necessary in order to achieve grounding line reversibility in the MISMIP3D intercomparison experiment (Pattyn et al, 2013).

PETSc
^^^^^

There is an option to use PETSc for the matrix solve component of the velocity solve,
and this has been observed to give a 3- or 4-fold improvement in performance over the
inbuilt conjugate gradient solver in a number of cases. To use this option, the CPP option :varlink:`ALLOW_PETSC` must be defined,
and MITgcm must be compiled with the ``-mpi`` flag (see :numref:`build_mpi`).
However, often a system-specific installation of PETSc is required.
If you wish to use PETSc with :filelink:`pkg/streamice`, please contact the author.

.. _ssub_streamice_boundary_stress:

Boundary Stresses
^^^^^^^^^^^^^^^^^

The calving front boundary conditions :eq:`cfbc_x` and :eq:`cfbc_y` are intended for ice fronts bordering open ocean.
However, there may be reasons to apply different Neumann conditions at these locations, e.g., one might want to
represent force associated with ice melange, or to represent parts of the ice shelf that are not resolved,
as in Goldberg et al. (2015) :cite:`Goldberg:2015`. The user can then modify these boundary conditions in the form

.. math::
   (h\nu(4\dot{\varepsilon}_{xx}+2\dot{\varepsilon}_{yy}))n_x +
   (2h\nu\dot{\varepsilon}_{xy})n_y = \frac{1}{2}g \left(\rho h^2 - \rho_w
   b^2\right)n_x + \sigma n_x + \tau n_y 

.. math::
  (2h\nu\dot{\varepsilon}_{xy})n_x +
   (h\nu(4\dot{\varepsilon}_{yy}+2\dot{\varepsilon}_{xx}))n_y = \frac{1}{2}g
   \left(\rho h^2 - \rho_w b^2\right)n_y + \sigma n_y + \tau n_x 

In these equations, :math:`\sigma` and :math:`\tau` represent normal and shear stresses at the boundaries of cells.
They are not specified directly, but through coefficients :math:`\gamma_{\sigma}` and :math:`\gamma_{\tau}`:

.. math::
   \sigma = \frac{1}{2}g \left(\rho h^2 - \rho_w
   b^2\right)\gamma_{\sigma}

.. math::
   \tau = \frac{1}{2}g \left(\rho h^2 - \rho_w
   b^2\right)\gamma_{\tau}

:math:`\gamma_{\sigma}` is specified through :varlink:`streamiceuNormalStressFile`,  :varlink:`streamicevNormalStressFile`,
:varlink:`streamiceuNormalTimeDepFile`, :varlink:`streamicevNormalTimeDepFile` and :math:`\gamma_{\tau}`
is specified through :varlink:`streamiceuShearStressFile`,  :varlink:`streamicevShearStressFile`,
:varlink:`streamiceuShearTimeDepFile`, and :varlink:`streamicevShearTimeDepFile`.
Within the file names, the  ``u`` and ``v`` determine whether the values are specified
along horizontal (:math:`u`-) faces and vertical (:math:`v`-) faces. The values will only
have an effect if they are specified along calving front boundaries (see :ref:`ssub_phys_pkg_streamice_domain_setup`).

Adjoint
~~~~~~~

The STREAMICE package is adjointable using both TAF (Goldberg et al. 2013 :cite:`goldberg_heimbach:2013`)
and OpenAD (Goldberg et al. 2016 :cite:`goldberg_openad_fixed:2016`). In OpenAD, the fixed-point method of
:cite:`christianson:94` is implemented, greatly reducing the memory requirements and also improving performance when PETSc is used.

Verification experiments with both OpenAD and TAF are located in the :filelink:`verification/halfpipe_streamice` (see below).

Key Subroutines
~~~~~~~~~~~~~~~

Top-level routine: :filelink:`streamice_timestep.F <pkg/streamice/streamice_timestep.F>` (called from :filelink:`model/src/do_oceanic_phys.F`)

::

    CALLING SEQUENCE
 ...
  streamice_timestep (called from DO_OCEANIC_PHYS)
  |
  |-- #ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
  |    STREAMICE_FIELDS_LOAD
  |   #endif
  |
  |--#if (defined (ALLOW_STREAMICE_OAD_FP))
  |    STREAMICE_VEL_SOLVE_OPENAD
  |  #else
  |    STREAMICE_VEL_SOLVE
  |    |
  |    |-- STREAMICE_DRIVING_STRESS
  |    |
  |    | [ITERATE ON FOLLOWING]
  |    |
  |    |-- STREAMICE_CG_WRAPPER
  |    |   |
  |    |   |-- STREAMICE_CG_SOLVE
  |    |       #ifdef ALLOW_PETSC
  |    |        STREAMICE_CG_SOLVE_PETSC
  |    |       #endif
  |    |
  |    |-- #ifdef STREAMICE_HYBRID_STRESS
  |         STREAMICE_VISC_BETA_HYBRID 
  |        #else
  |         STREAMICE_VISC_BETA 
  |        #endif
  |
  |-- STREAMICE_ADVECT_THICKNESS
  |   |
  |   |-- STREAMICE_ADV_FRONT  
  |
  |-- STREAMICE_UPD_FFRAC_UNCOUPLED
  |


STREAMICE diagnostics
~~~~~~~~~~~~~~~~~~~~~

Diagnostics output is available via the diagnostics package (:ref:`outp_pack`). Available output fields are summarized in the
following table:

.. code-block:: text

    ----------------------------------------------------------------------------
    <-Name->|Levs|  mate |<- code ->|<--  Units   -->|<- Tile (max=80c)
    ----------------------------------------------------------------------------
    SI_Uvel |  1 |       |UZ      L1|m/a             |Ice stream x-velocity
    SI_Vvel |  1 |       |VZ      L1|m/a             |Ice stream y-velocity
    SI_Thick|  1 |       |SM      L1|m               |Ice stream thickness
    SI_area |  1 |       |SM      L1|m^2             |Ice stream cell area coverage
    SI_float|  1 |       |SM      L1|none            |Ice stream grounding indicator
    SI_hmask|  1 |       |SM      L1|none            |Ice stream thickness mask
    SI_usurf|  1 |       |SM      L1|none            |Ice stream surface x-vel
    SI_vsurf|  1 |       |SM      L1|none            |Ice stream surface y-vel
    SI_ubase|  1 |       |SM      L1|none            |Ice stream basal x-vel
    SI_vbase|  1 |       |SM      L1|none            |Ice stream basal y-vel
    SI_taubx|  1 |       |SM      L1|none            |Ice stream basal x-stress
    SI_tauby|  1 |       |SM      L1|none            |Ice stream basal y-stress
    SI_selev|  1 |       |SM      L1|none            |Ice stream surface elev

Experiments and tutorials that use streamice
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The :filelink:`verification/halfpipe_streamice` experiment uses :filelink:`pkg/streamice`.

.. other references

.. Gladstone, Payne and Cornford (2010). Parameterising the grounding line in flow-line ice sheet models. The Cryosphere, 4, 605–619.

.. Pattyn, F. and others (2013). Grounding-line migration in plan-view marine ice-sheet models: results of the ice2sea MISMIP3d intercomparison. J of Glaciology, 59 (215), 410-422
