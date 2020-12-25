.. _sub_phys_remesh:

SHELFICE Remeshing
******************

Author: James Jordan and Daniel Goldberg

.. _ssub_phys_remesh_intro:

Introduction
============

This code works the nonlinear free surface feature of the ocean
model to allow continuous updating of the ice shelf draft in a mass-, salt- and heat-conservative fashion.
Note 'Shelfice Remeshing' is not a separate physical package as such, but works in conjunction
with :filelink:`pkg/shelfice`, and all code is contained within :filelink:`pkg/shelfice`.
However, as a separate line of development its application and use warrants
its own entry in the documentation.
By using :filelink:`pkg/streamice` at the same time, remeshing can allow synchronous coupling between ocean and ice shelf.

.. _ssub_phys_remeshing_config:

REMESHING configuration and compiling
=====================================

Compile-time options
--------------------

Shelfice remeshing requires that :filelink:`pkg/shelfice` be enabled, which is done by adding ``shelfice`` to ``packages.conf``
(see Section :numref:`building_code`). Nonlinear free surface is required, which is enabled by adding ``#define``  :varlink:`NONLIN_FRSURF`
to :filelink:`CPP_OPTIONS.h <model/inc/CPP_OPTIONS.h>`. Additionally,

-  ``#define`` :varlink:`ALLOW_SHELFICE_REMESHING` must be added to :filelink:`SHELFICE_OPTIONS.h <pkg/shelfice/SHELFICE_OPTIONS.h>`;

-  If :varlink:`SHI_ALLOW_GAMMAFRICT` is defined in :filelink:`SHELFICE_OPTIONS.h <pkg/shelfice/SHELFICE_OPTIONS.h>`
   we recommend also setting run-time parameter :varlink:`SHI_withBL_uStarTopDz` to ``.true.``, which will limit spurious features
   in the melt rate as explained in :numref:`ssub_phys_remesh_topdr`.

.. _ssub_phys_remeshing_runtime:

Run-time parameters
===================

:numref:`tab_phys_remeshing_runtimeparms` lists run-time parameters in ``data.shelfice`` relevant
to shelfice remeshing. In addition, :varlink:`nonlinFreeSurf`\ ``=4`` should be set, and :varlink:`select_rstar` should be zero (the model default) in file ``data``.

.. table:: Run-time parameters and default values
  :name: tab_phys_remeshing_runtimeparms

  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  |     Name                              |      Default value           |  Description                                                                                                             |
  +=======================================+==============================+==========================================================================================================================+
  | :varlink:`SHI_withBL_realFWflux`      |   FALSE                      | Necessary for mass/volume-conservative freezing/melting when :varlink:`SHELFICEboundaryLayer` ``= .true.``               |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  | :varlink:`SHI_withBL_uStarTopDz`      |   FALSE                      | With :varlink:`SHELFICEboundaryLayer` ``= .true.`` compute :math:`u^*` from uVel,vVel                                    |
  |                                       |                              | averaged over top :math:`\Delta z` thickness                                                                             |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  | :varlink:`SHELFICEmassFile`           |   :kbd:`' '`                 | Initialization file for ice shelf mass (kg m\ :sup:`-2`)                                                                 |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  | :varlink:`SHELFICEMassStepping`       |   FALSE                      | Enables ice mass to change in time                                                                                       |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  | :varlink:`SHELFICEMassDynTendFile`    |   :kbd:`' '`                 | Input file to specify non-thermodynamic ice mass change rate in kg/s (overridden when :filelink:`pkg/streamice` enabled) |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  | :varlink:`SHELFICEDynMassOnly`        |   FALSE                      | Update :varlink:`shelficeMass` via non-thermodynamic change only (overridden when :filelink:`pkg/streamice` enabled      |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  | :varlink:`SHELFICERemeshFrequency`    |   2592000                    | Frequency of remeshing operation (seconds)                                                                               |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  | :varlink:`SHELFICESplitThreshold`     |   1.25                       | Maximum allowed :varlink:`hFacC` for a cell                                                                              |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+
  | :varlink:`SHELFICEMergeThreshold`     |   0.26                       | Minimum allowed :varlink:`hFacC` for a cell                                                                              |
  +---------------------------------------+------------------------------+--------------------------------------------------------------------------------------------------------------------------+

.. _ssub_phys_remesh_descr:

Description
===========

When :filelink:`pkg/shelfice` is enabled, the elevation of the free surface in a grid cell is
determined by the mass of the ice shelf in that cell. In general use of :filelink:`shelfice <pkg/shelfice>`
this mass is held constant, but if it is allowed to change the free surface adjusts if :varlink:`implicitFreeSurface` ``= .true.``
through adjustment of the thickness of the top-level cell (:numref:`figremesh1`). If :varlink:`nonlinFreeSurf`\ ``=4``
these changes are fully accounted for in the ocean dynamics and tracer transport. However:

-  in the case of ice thinning, the numerical approximation to the governing equations will break down if the top-level cell becomes too thick, and

-  in the case of ice thickening, the top-level cell thickness cannot become negative or the model will fail.

Remeshing addresses these issues. At predefined intervals (set by :varlink:`SHELFICERemeshFrequency`),
the code checks every column and flags those where the top-level cell is too thick (:math:`h_c` is
larger than :varlink:`SHELFICESplitThreshold`) or where it is too thin (:math:`h_c` is smaller
than :varlink:`SHELFICEMergeThreshold`). In the former case, the cell is split into two cells as described
in :numref:`figremesh2`. In the latter case, the top cells is "merged" with the one below it. In both cases,
splitting and merging conserves mass, heat, and salt. Momentum is conserved where possible but
this is more difficult because velocities live at cell edges.

Ice shelf basal melt and freezing add and remove water from the ocean. :varlink:`useRealFreshWaterFlux`\  ``= .true.`` will cause
the ocean volume to be updated - unless :varlink:`SHELFICEboundaryLayer`\ `` = .true.``, which inputs heat
and salt fluxes over a distance :math:`\Delta z` from the ice-ocean interface, instead of inputting directly
into the top cell (:numref:`figremesh1`). In this case, an additional option :varlink:`SHI_withBL_realFWflux`\ ``=.true.``
can be set in ``data.shelfice`` to allow volume conservation.

.. figure:: figs/remesh1.*
   :width: 80%
   :align: center
   :alt: Remeshing schematic 1
   :name: figremesh1

   Schematic representation of (a) reference ice-shelf depth, d, vertical position of the ice-ocean interface, :math:`z_{surf}`,
   and the distance between the two, :math:`\eta`, and (b) the extent of the ice-shelf boundary layer used to calculate velocities,
   Bv (red), and tracers, B :math:`_\chi` (blue), used in the melt rate calculation.
   The model grid is represented by dashed lines with the actual sizeof the cells represented by the solid lines.
   From Jordan et al. (2018) :cite:`jordan:18`.

.. figure:: figs/remesh2.*
   :width: 80%
   :align: center
   :alt: Remeshing schematic 2
   :name: figremesh2

   Schematic representation of dimensionless vertical grid size, :math:`h_c`, and reference ice-shelf depth, `d`,
   at i=2 in (a) a "normal" case, (b) a cell with :math:`h_c` > :math:`h_{max}` at i=2, k=2 just before a model
   remesh check, and (c) the same cell just after a model remesh has occurred. From Jordan et al. (2018) :cite:`jordan:18`.

.. _ssub_phys_remesh_topdr:

Alternate boundary layer formulation
====================================

If :varlink:`SHELFICEboundaryLayer`\ ``= .true.``, then salt and temperature are averaged over a distance :math:`\Delta z`
from the ice-ocean interface in order to calculate melt rates, as described in Losch (2008) :cite:`losch:08`.
When :varlink:`SHI_ALLOW_GAMMAFRICT` is defined and :varlink:`SHELFICEuseGammaFrict`\ ``= .true.``,
near-ice velocities are used to calculate exchange coefficients of heat and salt, which can lead to spurious
features where there is a change in the level of the top fluid cell. In the default formulation velocities
(or rather square velocities) are first averaged horizontally from cell faces to cell centers, and then
vertically over a distance :math:`\Delta z`. The run-time parameter :varlink:`SHI_withBL_uStarTopDz`\ ``= .true.`` reverses
this order: velocities are first averaged vertically, and then horizontally. In some cases this has been found
to give less spurious variability, but either can be used.

Coupling with :filelink:`pkg/streamice`
=======================================

Shelfice remeshing can be used on its own (i.e. without coupling to an ice sheet model), with the effects of ice
dynamics specified via :varlink:`SHELFICEMassDynTendFile`. Alternatively it can be coupled to the :varlink:`pkg/streamice`.
To allow this, :varlink:`pkg/streamice` must be enabled. Please see the :filelink:`verification/shelfice_2d_remesh` for an example. (Documentation on SHELFICE is under construction)

.. Sea level restoring
.. ===================

.. When the grounded part of an ice sheet represented by ``STREAMICE`` or the calved mass of an ice shelf is not accounted for,
.. the amount of water displaced by the ice changes, which could lead to sea level change in open ocean. The latter of these is
.. the case in the shelfice_remeshing verification experiment. In a small domain, this has large effects on open-ocean sea level.
.. If these effects are unwanted, the ``conserve_ssh`` flag can be used. This feature takes advantage of the balance feature
.. of the OBCS (:numref:`sub_phys_pkg_obcs`) package. To use this, OBCS must be enabled, as well as the CPP directive ``OBCS_BALANCE_FLOW``
.. and the runtime parameter ``useOBCSbalance`` must be set to .TRUE. This remeshing feature calculates at each time step
.. the average sea level :math:`z_{sl}` of non-ice shelf covered ocean, and the OBCS balance flow then acts as if there is an
.. additional flux of :math:`\frac{z_{sl}}{\Delta t}` into the domain which must be balanced.

Diagnostics
===========

In addition to the diagnostics from :filelink:`pkg/SHELFICE`, remeshing adds one additional diagnostic: ``SHIRshel``,
the "reference" elevation of the ice shelf base (`d` in :numref:`figremesh1`).

Experiments that use Remeshing
==============================

-  :filelink:`verification/shelfice_2d_remesh`

