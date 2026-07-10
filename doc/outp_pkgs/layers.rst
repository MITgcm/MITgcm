.. _pkg_layers:

pkg/layers - Diagnosing Water Mass Transformation
=================================================

Introduction
------------

This packages computes the horizontal fluxes of volume/mass within the layers
specified by either THETA (potential temperature or conservative temperature),
salinity, or potential density referenced to a specific k-level. (Unfortunately
we cannot specify a reference pressure that is independent of the position of
the vertical grid points. For example, if one really wants to have the
potential density defined relative to 2000 dbar, then one needs to make sure
that the vertical grid is chosen in a way that has a tracer point at exactly
2000 dbar (= :varlink:`gravity`:varlink:`rhoConst`:varlink:`rC(k)`).)

For this code to work, and to compute a density layers averaged meridional
overturning circulation from the results, the CPP-flags
:varlink:`LAYERS_UFLUX`, :varlink:`LAYERS_VFLUX`, :varlink:`LAYERS_THICKNESS`
need to be defined (default).

The :varlink:`LAYERS_THERMODYNAMICS` code is described in :cite:`abernathey16`:
Water-mass transformation is calculated as (their eq.1)

.. math::
   \Omega(\sigma,t) = \frac{\partial}{\partial\sigma}
   \int_{\sigma'<\sigma} \left(
   \frac{\partial\sigma}{\partial\theta}\dot{\theta}
   +\frac{\partial\sigma}{\partial S}\dot{S}
   \right) dV

where :math:`\sigma` is potential density, :math:`t` is time, and
:math:`\dot{\theta}` and :math:`\dot{S}` represent all non-advective sources
(that is, external forcing and mixing) of potential temperature
(:math:`\theta`) and salinity (:math:`S`), respectively. The transformation
rate :math:`\Omega` can be linearly decomposed into many different
contributions from different processes, and the volume integral reduces to a
surface integral for surface fluxes.

Layers configuration and compilation
------------------------------------

.. tabularcolumns:: |\Y{.375}|\Y{.1}|\Y{.55}|

.. csv-table:: CPP preprocessor flags in the :filelink:`layers <pkg/layers>` package.
   :header: "CPP option", "Default", "Description"
   :widths: 30, 10, 60
   :name: tab_phys_pkg_layers_cpp

   :varlink:`LAYERS_UFLUX`, #define, compute isopycnal tranports in the U direction
   :varlink:`LAYERS_VFLUX`, #define, compute isopycnal tranports in the V direction
   :varlink:`LAYERS_THICKNESS`, #define, keep track of layer thicknesses
   :varlink:`LAYERS_THERMODYNAMICS`, #undef, allows using the water mass transformation code
   :varlink:`LAYERS_PRHO_REF`, #define, allow use of potential density as a layering field
   :varlink:`LAYERS_FINEGRID_DIAPYCNAL`, #undef, use refined grid for diapycnal terms
   :varlink:`LAYERS_MNC`, #undef, allows MNC output

Run-time parameters
-------------------

Run-time parameters (see :numref:`tab_phys_pkg_layers_runtimeparms`) are set in
``data.layers`` (read in :filelink:`pkg/layers/layers_readparms.F`).

Enabling the package
~~~~~~~~~~~~~~~~~~~~

:filelink:`layers<pkg/layers>` package is switched on/off at run-time by
setting :varlink:`useLayers` ``= .TRUE.,`` in ``data.pkg``.

General flags and parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:numref:`tab_phys_pkg_layers_runtimeparms` lists most run-time parameters.

.. tabularcolumns:: |\Y{.275}|\Y{.20}|\Y{.525}|

.. table:: Run-time parameters and default values
  :class: longtable
  :name: tab_phys_pkg_layers_runtimeparms

  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  |   Name                             |      Default value           |   Description                                                           |
  +====================================+==============================+=========================================================================+
  | :varlink:`layers_name`             |     ' '                      |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`layers_bounds`           |     UNSET_RL                 |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`layers_krho`             |     1                        |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`layers_bolus`            |   :varlink:`useGMRedi`       |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
