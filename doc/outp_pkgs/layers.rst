.. _pkg_layers:

pkg/layers - Diagnosing Water Mass Transformation
=================================================

Introduction
------------

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
   :varlink:`LAYERS_THERMODYNAMICS`, #undef, allows
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
