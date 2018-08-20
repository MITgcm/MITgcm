.. _sub_phys_pkg_streamice:

STREAMICE Package
-----------------


Author: Daniel Goldberg

.. _ssub_phys_pkg_streamice_intro:

Introduction
++++++++++++

Package STREAMICE provides a dynamic land ice model for MITgcm. It was created primarily to develop a TAF- and OpenAD-generated ice model adjoint and to provide synchronous ice-ocean coupling through the SHELFICE package. It solves a set of dynamic equations appropriate for floating ice-shelf flow as well as ice-stream and slower ice-sheet flow. It has been tested at the scale of one or several ice streams, but has not been tested at the continental scale.


.. _ssub_phys_pkg_streamice_config:
  
STREAMICE configuration and compiling
+++++++++++++++++++++++++++++++++++++

Compile-time options
####################

As with all MITgcm packages, SEAICE can be turned on or off at compile
time

-  using the ``packages.conf`` file by adding ``streamice`` to it,

(see Section :numref:`building_code`).

Parts of the STREAMICE code can be enabled or disabled at compile time via
CPP preprocessor flags. These options are set in ``STREAMICE_OPTIONS.h``. :numref:`tab_phys_pkg_streamice_cpp` summarizes the most important ones. For more
options see the default ``pkg/seaice/STREAMICE_OPTIONS.h``.

.. csv-table:: Some of the most relevant CPP preporocessor flags in the ``streamice``-package.
   :header: "CPP option", "Description"
   :widths: 40, 60
   :name: tab_phys_pkg_streamice_cpp

   "``STREAMICE_CONSTRUCT_MATRIX``", "Explicit construction of matrix for Picard iteration for velocity"
   "``STREAMICE_HYBRID_STRESS``", "Use L1L2 formulation for stress balance (default Shallow Shelf Approx)"
   "``USE_ALT_RLOW``", "Use package array for rLow rather than model"
   "``STREAMICE_GEOM_FILE_SETUP``", "Use files rather than parameters in ``STREAMICE_PARM03`` to configure boundaries"
   "``STREAMICE_SMOOTH_FLOATATION2``", "Subgrid parameterization of transition across the grounding line"
   "``ALLOW_PETSC``", "Enable interface to PETSc for velocity solver matrix solve"
   

.. _ssub_phys_pkg_streamice_runtime:

Run-time parameters 
+++++++++++++++++++

Run-time parameters (see :numref:`tab_phys_pkg_streamice_runtimeparms`) are set in
files `data.pkg` (read in `packages_readparms.F`), and `data.streamice` (read in `streamice_readparms.F`).

Enabling the package
####################

A package is switched on/off at run-time by setting (e.g. for STREAMICE `useSTREAMICE = .TRUE.` in `data.pkg`).

General flags and parameters
############################

:numref:`tab_phys_pkg_streamice_runtimeparms` lists most run-time parameters.


.. table:: Run-time parameters and default values (defined under STREAMICE_PARM01 Namelist)
  :name: tab_phys_pkg_streamice_runtimeparms

  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  |   **Name**                      |     **Default value**        | **Description**                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEdensity                |     910                      | the (uniform) density of land ice                                                              |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEdensity_ocean_avg      |     1024                     | the (uniform) density of ocean                                                                 |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | n_glen                          |     3                        | Glen's Flow Law exponent                                                                       |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | eps_glen_min                    |     1e-12                    | minimum strain rate in Glen's Law (:math:`\varepsilon_0`)                                      |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | eps_u_min                       |     1e-6                     | minimum speed in nonlinear sliding law (:math:`u_0`)                                           |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | n_basal_friction                |     1                        | exponent in nonlinear sliding law                                                              |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_cg_tol                |     1e-6                     | tolerance of conjugate gradient of linear solve of Picard iteration for velocity               |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_lower_cg_tol          |     .true.                   | lower CG tolerance when nonlinear residual decreases by fixed factor                           |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_max_cg_iter           |     2000                     | maximum iterations in linear solve                                                             |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_maxcgiter_cpl         |     0                        | as above when coupled with SHELFICE                                                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_nonlin_tol            |     1e-6                     | tolerance of nonlinear residual for velocity (relative to initial)                             |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_max_nl_iter           |     100                      | maximum Picard iterations in solve for velocity                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_maxnliter_cpl         |     0                        | as above when coupled with SHELFICE                                                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_nonlin_tol_fp         |     1e-6                     | tolerance of relative change for velocity iteration (relative to magnitude)                    |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_err_norm              |    0                         | type of norm evaluated for error (:math:`p` in :math:`p`-norm; 0 is :math: `infty`             |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICE_chkfixedptconvergence |    .false.                   | terminate velocity iteration based on relative change per iteration                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICE_chkresidconvergence   |    .true.                    | terminate velocity iteration based on residual                                                 |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEthickInit              |    'FILE'                    | method by which to initialise thickness ('FILE' or 'PARAM')                                    |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEthickFile              |                              | thickness initialisation file (rather than parameters in ``STREAMICE_PARM03``)                 |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICE_move_front            |    .false.                   | allow ice shelf front to advance                                                               |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICE_calve_to_mask         |    .false.                   | (if STREAMICE_move_front=.true.) do not allow to advance beyond ``STREAMICE_calve_mask``       |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEcalveMaskFile          |                              | file to initialise ``STREAMICE_calve_mask``                                                    |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICE_diagnostic_only       |    .false.                   | do not update ice thickness (velocity solve only)                                              |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_CFL_factor            |    0.5                       | CFL factor which determine maximum time step for thickness sub-cycling                         |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamice_adjDump               |    0.                        | frequency (s) of writing of adjoint fields to file (TAF only)                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamicebasalTracConfig        |    'UNIFORM'                 | method by which to initialise basal traction ('FILE' or 'UNIFORM')                             |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamicebasalTracFile          |                              | basal trac initialisation file (see :ref:`ssub_phys_pkg_streamice_units` for units)            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | C_basal_fric_const              |    31.71                     | uniform basal traction value (see :ref:`ssub_phys_pkg_streamice_units` for units)              |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamiceGlenConstConfig        |    'UNIFORM'                 | method by which to initialise Glen's constant ('FILE' or 'UNIFORM')                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamiceGlenConstFile          |                              | Glen's constant initialisation file (see :ref:`ssub_phys_pkg_streamice_units` for units)       |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | B_glen_isothermal               |                              | uniform Glen's constant value (see :ref:`ssub_phys_pkg_streamice_units` for units)             |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamiceBdotFile               |                              | File to initialise time-indep melt rate (m/year)                                               |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamiceBdotTimeDepFile        |                              | File to initialise time-varying melt rate (m/year), based on ``streamice_forcing_period``      |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamiceTopogFile              |                              | topography initialisation file (if ``USE_ALT_RLOW`` defined)                                   |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | streamiceHmaskFile              |                              | ``STREAMICE_hmask`` initialisation file (if ``STREAMICE_GEOM_FILE_SETUP`` defined)             |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEuFaceBdryFile          |                              | ``STREAMICE_ufacemask_bdry`` initialisation file (if ``STREAMICE_GEOM_FILE_SETUP`` defined)    |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEvFaceBdryFile          |                              | ``STREAMICE_vfacemask_bdry`` initialisation file (if ``STREAMICE_GEOM_FILE_SETUP`` defined)    |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEuMassFluxFile          |                              | mass flux at `u`-faces initialisation file (if ``STREAMICE_GEOM_FILE_SETUP`` defined)          |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEvMassFluxFile          |                              | mass flux at `v`-faces  initialisation file (if ``STREAMICE_GEOM_FILE_SETUP`` defined)         |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEuFluxTimeDepFile       |                              | time-dep mass flux at `u`-faces file (if ``STREAMICE_GEOM_FILE_SETUP`` defined)                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | STREAMICEvFluxTimeDepFile       |                              | time-dep mass flux at `v`-faces file (if ``STREAMICE_GEOM_FILE_SETUP`` defined)                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | STREAMICEuNormalStressFile      |                              |                                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEvNormalStressFile      |                              |                                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | STREAMICEuShearStressFile       |                              |                                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | STREAMICEvShearStressFile       |                              |                                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | STREAMICEuNormalTimeDepFile     |                              |                                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | STREAMICEvNormalTimeDepFile     |                              |                                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | STREAMICEuShearTimeDepFile      |                              |                                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | STREAMICEvShearTimeDepFile      |                              |                                                                                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | streamice_smooth_gl_width       |   0                          |  (meters) thickness range parameter in basal traction smoothing across grounding line          |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | streamice_adot_uniform          |   0                          |  time/space uniform surface accumulation rate (m/year)                                         |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | streamice_forcing_period        |   0                          | (seconds) File input frequency for STREAMICE time-dependent forcing fields                     |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
 

  
.. _ssub_phys_pkg_streamice_descr:
  
Description
+++++++++++

.. _ssub_phys_pkg_streamice_eqns:

Equations Solved
################

The model solves for 3 dynamic variables: :math:`x`-velocity
(:math:`u`), :math:`y`-velocity (:math:`v`), and thickness (:math:`h`).
There is also a variable that tracks coverage of fractional cells,
discussed...

By default the model solves the Shallow Shelf approximation (SSA) for
velocity. The SSA is appropriate for floating ice (ice shelf) or ice
flowing over a low-friction bed (e.g. MacAyeal, 1989). The SSA consists
of the :math:`x`-momentum balance:

.. math::

   \label{eq:xmom}
    \partial_x(h\nu(4\dot{\varepsilon}_{xx}+2\dot{\varepsilon}_{yy})) +
   \partial_y(2h\nu\dot{\varepsilon}_{xy}) - \tau_{bx} = \rho g h s_x

the :math:`y`-momentum balance:

.. math::

   \label{eq:ymom}
    \partial_x(2h\nu\dot{\varepsilon}_{xy}) +
   \partial_y(h\nu(4\dot{\varepsilon}_{yy}+2\dot{\varepsilon}_{xx})) - \tau_{by} =
   \rho g h s_y.

From the velocity field, thickness evolves according to the continuity
equation:

.. math::

   \label{eq:cont}
    h_t + \nabla\cdot(h\vec{u}) = \dot{a}-\dot{b},

Where :math:`\dot{b}` is a basal mass balance (e.g. melting due to
contact with the ocean), positive where there is melting. This is a field that can be specified through a file. At the moment surface mass
balance :math:`\dot{a}` can only be set as uniform. Where ice is grounded,
surface elevation is given by

.. math:: s = R + h,

where :math:`R(x,y)` is the bathymetry, and the basal elevation
:math:`b` is equal to :math:`R`. If ice is floating, then the assumption
of hydrostasy and constant density gives

.. math:: s = (1-\frac{\rho}{\rho_w} h,

where :math:`\rho_w` is a representative ocean density, and
:math:`b=-(\rho/\rho_w)h`. Again by hydrostasy, floation is assumed
wherever

.. math:: h \leq -\frac{\rho_w}{\rho}R

is satisfied. Floatation criteria is stored in ``float_frac_streamice``,
equal to 1 where ice is at floatation.

The strain rates :math:`\varepsilon_{ij}` are generalized to the case of
orthogonal curvilinear coordinates, to include the "metric" terms that
arise when casting the equations of motion on a sphere or projection on
to a sphere (see pkg/SEAICE, 6.6.2.4.8 of the MITgcm documentation).
Thus

.. math::

   \begin{aligned}
    \dot{\varepsilon}_{xx} = & u_x + k_1 v, \notag \\
    \dot{\varepsilon}_{yy} = & v_y + k_1 u, \notag \\ 
    \dot{\varepsilon}_{xy} = & \frac{1}{2}(u_y+v_x) + k_1 u + k_2 v. \notag \end{aligned}

:math:`\nu` has the form arising from Glen's law



.. math::
   :label: visc_eqn

   \nu =
   \frac{1}{2}A^{-\frac{1}{n}}\left(\dot{\varepsilon}_{xx}^2+\dot{\varepsilon}_{yy}
   ^2+\dot{\varepsilon}_{xx}\dot{\varepsilon}_{yy}+\dot{\varepsilon}_{xy}^2+\dot{
   \varepsilon}_{min}^2\right)^{\frac{1-n}{2n}},

though the form is slightly different if a hybrid formulation is used. 

Whether :math:`\tau_b` is nonzero depends on whether the floatation
condition is satisfied. Currently this is determined simply on an
instantaneous cell-by-cell basis (unless subgrid interpolation is used),
as is the surface elevation :math:`s`, but possibly this should be
rethought if the effects of tides are to be considered.
:math:`\vec{\tau}_b` has the form

.. math::
   :label: tau_eqn

   \label{eq:sliding_law}
    \vec{\tau}_b = C (|\vec{u}|^2+u_{min}^2)^{\frac{m-1}{2}}\vec{u}.

Again, the form is slightly different if a hybrid formulation is to be
used. The scalar term multiplying :math:`\vec{u}` is referred to as
:math:`\beta` below.

The momentum equations are solved together with appropriate boundary
conditions, discussed below. In the case of a calving front boundary
condition (CFBC), the boundary condition has the following form:

.. math::

   \label{eq:cfbcx}
    (h\nu(4\dot{\varepsilon}_{xx}+2\dot{\varepsilon}_{yy}))n_x +
   (2h\nu\dot{\varepsilon}_{xy})n_y = \frac{1}{2}g \left(\rho h^2 - \rho_w
   b^2\right)n_x   

.. math::
   \label{eq:cfbcy}  
   (2h\nu\dot{\varepsilon}_{xy})n_x +
   (h\nu(4\dot{\varepsilon}_{yy}+2\dot{\varepsilon}_{xx}))n_y = \frac{1}{2}g
   \left(\rho h^2 - \rho_w b^2\right)n_y. 

Here :math:`\vec{n}` is the normal to the boundary, and :math:`R(x,y)`
is the bathymetry.

Hybrid SIA-SSA stress balance
#############################

The SSA does not take vertical shear stress or strain rates (e.g.,
:math:`\sigma_{xz}`, :math:`\partial u/\partial z`) into account.
Although there are other terms in the stress tensor, studies have found
that in all but a few cases, vertical shear and longitudinal stresses
(represented by the SSA) are sufficient to represent glaciological flow.
streamice can allow for representation of vertical shear, although the
approximation is made that longitudinal stresses are depth-independent.
The stress balance is referred to as "hybrid" because it is a joining of
the SSA and the Shallow Ice Approximation (SIA), which only accounts
only for vertical shear. Such hybrid formulations have been shown to be
valid over a larger range of conditions than SSA (*Goldberg* 2011).

In the hybrid formulation, :math:`\overline{u}` and
:math:`\overline{v}`, the depth-averaged :math:`x-` and :math:`y-`
velocities, replace :math:`u` and :math:`v` in , , and , and gradients
such as :math:`u_x` are replaced by :math:`(\overline{u})_x`. Viscosity
becomes

.. math::

   \nu =
   \frac{1}{2}A^{-\frac{1}{n}}\left(\dot{\varepsilon}_{xx}^2+\dot{\varepsilon}_{yy}
   ^2+\dot{\varepsilon}_{xx}\dot{\varepsilon}_{yy}+\dot{\varepsilon}_{xy}^2+\frac{1
   }{4}u_z^2+\frac{1}{4}v_z^2+\dot{\varepsilon}_{min}^2\right)^{\frac{1-n}{2n}}.

In the formulation for :math:`\tau_b`, :math:`u_b`, the horizontal
velocity at :math:`u_b` is used instead. The details are given in
*Goldberg* (2011).

Ice front advance
#################

By default all mass flux across calving boundaries is considered lost. However, it is possible to account for this flux and potential advance of the ice shelf front. If ``STREAMICE_move_front=.true.``, then a partial-area formulation is used.

The algorithm is based on *Albrecht* (2011). In this scheme, for empty or partial cells adjacent to a calving front, a **reference** thickness
:math:`h_{ref}` is found, defined as an average over the thickness
of all neighboring cells with that flow into the cell. The total volume input over a time step to
is added to the volume of ice already in the cell, whose partial area coverage is then updated based on the volume and reference thickness. If the area coverage reaches 100% in a time step, then the additional volume is cascaded into adjacent empty or partial cells.

If ``calve_to_mask=.true.``, this sets a limit to how far the front can
advance, even if advance is allowed. The front will not advance into
cells where the array ``calve_mask`` is not equal to 1. This mask must
be set through a binary input file to allow front advance past its initial position.

No calving parameterisation is implemented in ``STREAMICE``. However,
front advancement is a precursor for such a development to be added.

.. _ssub_phys_pkg_streamice_units:

Units of input files
####################

The inputs for basal traction (``streamicebasalTracFile``, ``C_basal_fric_const``) and ice stiffness (``streamiceGlenConstFile``, ``B_glen_isothermal``) require specific units. For ice stiffness (`A` in Eqn :eq:`visc_eqn`), :math:`B=A^{-1/n}` is specified; or, more accurately, its square root :math:`A^{-1/(2n)}` is specified. (This is to ensure positivity of `B` by squaring the input.) The units of ``streamiceGlenConstFile`` and ``B_glen_isothermal`` are

:math:`\mathrm{Pa}^{1/2}\ \mathrm{yr}^{1/(2n)}`

where `n` is ``n_glen``.

``streamicebasalTracFile`` and ``C_basal_fric_const`` initialise the basal traction (`C` in Eqn :eq:`tau_eqn`). Again :math:`C^{1/2}` is directly specified rather than `C` to ensure positivity. The units are

:math:`\mathrm{Pa}^{1/2} (\mathrm{m }\ \mathrm{yr}^{-1})^{n_b}`

where :math:`n_b` is ``n_basal_friction``.

Numerical Details
+++++++++++++++++

.. figure:: figs/stencil.*
   :width: 40%
   :align: center
   :alt: STREAMICE stencil
   :name: figstencil

   Grid locations of thickness (`h`), velocity (`u,v`), area, and various masks.
   
.. figure:: figs/mask_cover.*
   :width: 40%
   :align: center
   :alt: STREAMICE masks
   :name: figmask_cover

   Hypothetical configuration, detailing the meaning of thickness and velocity masks and their role in controlling boundary conditions.

The momentum balance is solved via iteration on viscosity (*Goldberg* 2011). At each iteration, a linear elliptic differential equation is solved via a finite-element method using bilinear basis functions. The velocity solution "lives" on cell corners, while thickness "lives" at cell centers (Fig. :numref:`figstencil`). The cell-centered thickness is then evolved using a second-order slope-limited finite-volume scheme, with the velocity field from the previous solve. To represent the flow of floating ice, basal stress terms are multiplied by an array ``float_frac_streamice``, a cell-centered array which determines where ice meets the floation condition.

The computational domain of ``STREAMICE`` (which may be smaller than the array/grid as
defined by ``SIZE.h`` and ``GRID.h``) is determined by a number of mask
arrays within the ``STREAMICE`` package. They are

-  :math:`hmask` (``STREAMICE_hmask``): equal to 1 (ice-covered), 0
   (open ocean), 2 (partly-covered), or -1 (out of domain)

-  :math:`umask` (``STREAMICE_umask``): equal to 1 (an "active" velocity
   node), 3 (a Dirichlet node), or 0 (zero velocity)

-  :math:`vmask` (``STREAMICE_vmask``): similar to umask

-  :math:`ufacemaskbdry` (``STREAMICE_ufacemask_bdry``): equal to -1
   (interior face), 0 (no-slip), 1 (no-stress), 2 (calving stress
   front), or 4 (flux input boundary); when 4, then
   ``u_flux_bdry_SI`` must be initialized, through binary or parameter
   file

-  :math:`vfacemaskbdry` (``STREAMICE_vfacemask_bdry``): similar to
   ufacemaskbdry

:math:`hmask` is defined at cell centers, like :math:`h`. :math:`umask`
and :math:`vmask` are defined at cell nodes, like velocities.
:math:`ufacemask_bdry` and :math:`vfacemask_bdry` are defined at cell
faces, like velocities in a :math:`C`-grid - but unless
``STREAMICE_GEOM_FILE_SETUP`` is ``#define``\ d in
``STREAMICE_OPTIONS.h``, the values are only relevant at the boundaries
of the grid.

The values of :math:`umask` and :math:`vmask` determine which nodal
values of :math:`u` and :math:`v` are involved in the solve for
velocities. These masks are not configured directly by the user, but are re-initialized based on ``STREAMICE_hmask`` and ``STREAMICE_u/vfacemask_bdry`` at each time step. Fig. :numref:`figmask_cover` demonstrates how these values are set in various cells.

With :math:`umask` and :math:`vmask` appropriately initialized,
``STREAMICE_VEL_SOLVE`` can proceed rather generally. Contributions to
are only evaluated if :math:`hmask=1` in a given cell, and a given nodal
basis function is only considered if :math:`umask=1` or :math:`vmask=1`
at that node.

Configuring domain through files
################################

The ``STREAMICE_GEOM_FILE_SETUP`` compile option allows versatility in defining the domain. With this option, the array ``STREAMICE_hmask`` must be initialised through a file (``streamiceHmaskFile``) as must ``STREAMICE_ufacemask_bdry`` and ``STREAMICE_vfacemask_bdry`` (through ``STREAMICEuFaceBdryFile`` and ``STREAMICEvFaceBdryFile``) as well as ``u_flux_bdry_SI`` and ``v_flux_bdry_SI``, volume flux at the boundaries, where appropriate (through ``STREAMICEuMassFluxFile`` and ``STREAMICEvMassFluxFile``). Thickness must be initialised through a file as well (``STREAMICEthickFile``); ``STREAMICE_hmask`` is set to zero where ice thickness is zero, and boundaries between in-domain and out-of-domain cells (according to ``STREAMICE_hmask``) are no-slip by default.

When using this option, it is important that for all internal boundaries, ``STREAMICE_ufacemask_bdry`` and ``STREAMICE_vfacemask_bdry`` are -1. (This will not be the case if ``STREAMICEuFaceBdryFile`` and ``STREAMICEvFaceBdryFile`` are undefined.)

An example of domain configuration through files can be found in the ``halfpipe_streamice`` verification folder. By default, ``halfpipe_streamice`` is compiled with ``STREAMICE_GEOM_FILE_SETUP`` undefined, but the user can modify this option. The file ``data.streamice_geomSetup`` represents an alternative version of ``data.streamice`` in which the appropriate binary files are specified.

Configuring domain through parameters
#####################################

For a very specific type of domain the boundary conditions and initial thickness can be set via parameters in ``data.streamice``. Such a domain will be rectangular. In order to use this option, the ``STREAMICE_GEOM_FILE_SETUP`` compile flag should be undefined.

There are different boundary condition types that can be set:

-  ``noflow``: `x`- and `y`-velocity will be zero along this boundary.

-  ``nostress``: velocity normal to boundary will be zero; there will be no tangential stress along the boundary.

-  ``fluxbdry``: a mass volume flux is specified along this boundary, which becomes a boundary condition for the thickness advection equation (see :ref:`ssub_phys_pkg_streamice_eqns`). velocities will be zero. The corresponing parameter ``flux_val_bdry_X`` then sets the value.

-  ``CFBC``: calving front boundary condition, a neumann condition based on ice thickness and bed depth, is imposed at this boundary (see :ref:`ssub_phys_pkg_streamice_eqns`).
  
 Note the above only apply if there is dynamic ice in the cells at the boundary in question. The boundary conditions are then set by specifying the above conditions over ranges of each (north/south/east/west) boundary. The division of each boundary should be exhaustive and the ranges should not overlap.

.. table:: Parameters to initialise boundary conditions (defined under STREAMICE_PARM03 Namelist)
  :name: tab_phys_pkg_streamice_domainparms
  
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_noflow_NORTH              |   0                          | (meters) western limit of no-flow region on northern boundary                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_noflow_NORTH              |   0                          | (meters) eastern limit of no-flow region on northern boundary                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_noflow_SOUTH              |   0                          | (meters) western limit of no-flow region on Southern boundary                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_noflow_SOUTH              |   0                          | (meters) eastern limit of no-flow region on Southern boundary                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_noflow_EAST               |   0                          | (meters) southern limit of no-flow region on eastern boundary                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_noflow_EAST               |   0                          | (meters) northern limit of no-flow region on eastern boundary                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_noflow_WEST               |   0                          | (meters) southern limit of no-flow region on western boundary                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_noflow_WEST               |   0                          | (meters) northern limit of no-flow region on eastern boundary                                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_nostress_NORTH            |   0                          | (meters) western limit of no-stress region on northern boundary                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_nostress_NORTH            |   0                          | (meters) eastern limit of no-stress region on northern boundary                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_nostress_SOUTH            |   0                          | (meters) western limit of no-stress region on Southern boundary                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_nostress_SOUTH            |   0                          | (meters) eastern limit of no-stress region on Southern boundary                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_nostress_EAST             |   0                          | (meters) southern limit of no-stress region on eastern boundary                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_nostress_EAST             |   0                          | (meters) northern limit of no-stress region on eastern boundary                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_nostress_WEST             |   0                          | (meters) southern limit of no-stress region on western boundary                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_nostress_WEST             |   0                          | (meters) northern limit of no-stress region on eastern boundary                                |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_fluxbdry_NORTH            |   0                          | (meters) western limit of flux-boundary region on northern boundary                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_fluxbdry_NORTH            |   0                          | (meters) eastern limit of flux-boundary region on northern boundary                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_fluxbdry_SOUTH            |   0                          | (meters) western limit of flux-boundary region on Southern boundary                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_fluxbdry_SOUTH            |   0                          | (meters) eastern limit of flux-boundary region on Southern boundary                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_fluxbdry_EAST             |   0                          | (meters) southern limit of flux-boundary region on eastern boundary                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_fluxbdry_EAST             |   0                          | (meters) northern limit of flux-boundary region on eastern boundary                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_fluxbdry_WEST             |   0                          | (meters) southern limit of flux-boundary region on western boundary                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_fluxbdry_WEST             |   0                          | (meters) northern limit of flux-boundary region on eastern boundary                            |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_CFBC_NORTH                |   0                          | (meters) western limit of calving front condition region on northern boundary                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_CFBC_NORTH                |   0                          | (meters) eastern limit of calving front condition region on northern boundary                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_CFBC_SOUTH                |   0                          | (meters) western limit of calving front condition region on Southern boundary                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_CFBC_SOUTH                |   0                          | (meters) eastern limit of calving front condition region on Southern boundary                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_CFBC_EAST                 |   0                          | (meters) southern limit of calving front condition region on eastern boundary                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_CFBC_EAST                 |   0                          | (meters) northern limit of calving front condition region on eastern boundary                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | min_x_CFBC_WEST                 |   0                          | (meters) southern limit of calving front condition region on western boundary                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | max_x_CFBC_WEST                 |   0                          | (meters) northern limit of calving front condition region on eastern boundary                  |
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+
  | flux_val_bdry_south             |   0                          | (m^2/a) volume flux per width entering at flux-boundary on southern boundary                   |  
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | flux_val_bdry_north             |   0                          | (m^2/a) volume flux per width entering at flux-boundary on southern boundary                   |  
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | flux_val_bdry_east              |   0                          | (m^2/a) volume flux per width entering at flux-boundary on southern boundary                   |  
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 
  | flux_val_bdry_west              |   0                          | (m^2/a) volume flux per width entering at flux-boundary on southern boundary                   |  
  +---------------------------------+------------------------------+------------------------------------------------------------------------------------------------+ 

Additional Features
+++++++++++++++++++

Grounding line parameterization
###############################

Representing grounding line movement (change of boundary between grounded and floating ice) is problematic in ice sheet models due to the high resolution required. It has been found that sub-grid treatment of the grounding line can partially alleviate this requirement (Gladstone et al, 2011). STREAMICE implements a simple "smoothing" of the floatation condition. By default, ``float_frac_streamice`` is equal to 0 in cells that satisfy the floatation condition, and 1 elsewhere. If the compile option ``STREAMICE_SMOOTH_FLOATATION2`` is defined, then the array varies smoothly between 0 and 1 in cells where :math:`|h-h_f| < w_{smooth}/2`, where

.. math::

  h_f = -\frac{\rho}{\rho_w}R

and :math:`w_{smooth}` is specified by ``streamice_smooth_gl_width``. This modification then smooths the transition from grounded to floating ice with respect to basal stress. It is found that this parameterisation is necessary in order to achieve grounding line reversibility in the MISMIP3D intercomparison experiment (Pattyn et al, 2013).

PETSc
#####

There is an option to use PETSc for the matrix solve component of the velocity solve, and this has been observed to give a 3- or 4-fold improvement in performance over the inbuilt Conjugate Gradient solver in a number of cases. To use this option, the compile option ``ALLOW_PETSC`` must be defined, and MITgcm must be compiled with the -mpi flag. However, often a system-specific installation of PETSc is required. If you wish to use PETSc with STREAMICE, please contact the author.

Adjoint
+++++++

The STREAMICE package is adjoinable using both TAF (Goldberg and Heimbach, 2013) and OpenAD (Goldberg et al, 2016). In OpenAD, the fixed-point method of Christianson (1994) is implemented, greatly reducing the memory requirements and also improving performance when PETSc is used.

Verification experiments with both OpenAD and TAF are located in the ``halfpipe_streamice`` verification folder (see below).

Key Subroutines
+++++++++++++++

Top-level routine: ``streamice_timestep.F`` (called from ``do_oceanic_phys.F``)

::

 C    CALLING SEQUENCE
 c ...
 c  streamice_timestep (called from DO_OCEANIC_PHYS)
 c  |
 c  |-- #ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
 c  |    STREAMICE_FIELDS_LOAD
 c  |   #endif
 c  |
 c  |--#if (defined (ALLOW_STREAMICE_OAD_FP))
 c  |    STREAMICE_VEL_SOLVE_OPENAD
 c  |  #else
 c  |    STREAMICE_VEL_SOLVE
 c  |    |
 c  |    |-- STREAMICE_DRIVING_STRESS
 c  |    |
 c  |    | [ITERATE ON FOLLOWING]
 c  |    |
 c  |    |-- STREAMICE_CG_WRAPPER
 c  |    |   |
 c  |    |   |-- STREAMICE_CG_SOLVE
 c  |    |       #ifdef ALLOW_PETSC
 c  |    |        STREAMICE_CG_SOLVE_PETSC
 c  |    |       #endif
 c  |    |
 c  |    |-- #ifdef STREAMICE_HYBRID_STRESS
 c  |         STREAMICE_VISC_BETA_HYBRID 
 c  |        #else
 c  |         STREAMICE_VISC_BETA 
 c  |        #endif
 c  |
 c  |-- STREAMICE_ADVECT_THICKNESS
 c  |   |
 c  |   |-- STREAMICE_ADV_FRONT  
 c  |
 c  |-- STREAMICE_UPD_FFRAC_UNCOUPLED
 c  |


STREAMICE diagnostics
+++++++++++++++++++++

Diagnostics output is available via the diagnostics package (see Section
[sec:pkg:diagnostics]). Available output fields are summarized in the
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
++++++++++++++++++++++++++++++++++++++++++++

The ``halfpipe_streamice`` verification experiment uses STREAMICE.

References
++++++++++

Gladstone, Payne and Cornford (2010). Parameterising the grounding line in flow-line ice sheet models. The Cryosphere, 4, 605–619.

Goldberg, D N (2011). A variationally-derived, depth-integrated approximation to the Blatter/Pattyn balance. J. of Glaciology, 57, 157-170.

Goldberg, D N and P Heimbach (2013). Parameter and state estimation with a time-dependent adjoint marine ice sheet model. The Cryosphere, 7, 1659-1678 

Goldberg, D., Narayanan, S. H. K., Hascoet, L. & Utke, J. (2016). An optimized treatment for algorithmic differentiation of an important glaciological fixed-point problem. Geosci. Model Dev., 9, 1891-1904.

Pattyn, F. and others (2013). Grounding-line migration in plan-view marine ice-sheet models: results of the ice2sea MISMIP3d intercomparison. J of Glaciology, 59 (215), 410-422
