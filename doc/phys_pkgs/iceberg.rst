.. _sub_phys_pkg_iceberg:

ICEBERG Package
---------------

Authors: Paul Thomas Summers, Benjamin Joseph Davison

Introduction
~~~~~~~~~~~~

:filelink:`pkg/iceberg` provides a thermodynamic and mechanical model for iceberg melt and drag for icebergs smaller than the grid scale.

CPP options enable or disable different aspects of the package
(:numref:`iceberg_config`). Run-time options, flags, filenames and
field-related dates/times are described in :numref:`iceberg_runtime`. A description of key subroutines is given
in :numref:`iceberg_subroutines`. Available diagnostics output is listed in
:numref:`iceberg_diagnostics`.


.. _iceberg_config:

ICEBERG configuration
~~~~~~~~~~~~~~~~~~~~~
 
As with all MITgcm packages, :filelink:`pkg/iceberg` can be turned on or off at compile
time:

-  using the ``packages.conf`` file by adding ``iceberg`` to it,

-  or using :filelink:`genmake2 <tools/genmake2>` adding ``-enable=iceberg`` or ``disable=iceberg`` switches

:filelink:`pkg/iceberg` does not require any additional packages, but it will only
work with conventional vertical :math:`z`-coordinates (pressure
coordinates are not implemented).
:filelink:`pkg/iceberg` can be used with :filelink:`pkg/ptracers` if enabled to track the trajectory of iceberg meltwater. 

Parts of the :filelink:`pkg/iceberg` code can be enabled or disabled at compile time
via CPP preprocessor flags. These options are set in :filelink:`ICEBERG_OPTIONS.h <pkg/shelfice/ICEBERG_OPTIONS.h>`:

.. tabularcolumns:: |\Y{.32}|\Y{.1}|\Y{.570}|

.. table:: Compile-time parameters
   :name: tab_phys_pkg_iceberg_compileparms

   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | CPP Flag Name                                 | Default | Description                                                                                                          |
   +===============================================+=========+======================================================================================================================+
   | :varlink:`ALLOW_PER_BERG_DIAG`               | #undef  | Enables per iceberg diagnostic files, like older versions of ICEBERG                                                  |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

The maximum number of icebergs per cell is a compile time size that must be set appropriately in :filelink:`/pkg/iceberg/ICEBERG_SIZE.h`.
 The default of 500 is a reasonable starting point, but may need adjustment depending on your grid and average iceberg sizes. 

   .. tabularcolumns:: |\Y{.32}|\Y{.1}|\Y{.570}|

.. table:: Compile-time variables
   :name: tab_phys_pkg_iceberg_compilevars

   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | CPP Flag Name                                 | Default | Description                                                                                                          |
   +===============================================+=========+======================================================================================================================+
   | :varlink:`brg_MaxBergCt`                      | 500     | Maximum number of icebergs that can be placed in any one cell.                                                       |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

.. _iceberg_runtime:

ICEBERG run-time parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~

:filelink:`pkg/iceberg` is switched on/off at run time by setting :varlink:`useICEBERG` to ``.TRUE.`` in file ``data.pkg``.
Run-time parameters are set in file ``data.iceberg`` (read in :filelink:`pkg/iceberg/iceberg_readparms.F`),as listed below.

The data files specifying iceberg dimensions are in meters, all values should be positive.

.. tabularcolumns:: |\Y{.275}|\Y{.28}|\Y{.455}|

.. table:: Run-time parameters and default values; parameters are across namelist groups ``ICEBERG_PARM01, ICEBERG_PARM02``
   :name: tab_phys_pkg_iceberg_runtimeparms
   :class: longtable

   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Default                                    | Description                                                                                             |
   +========================================+============================================+=========================================================================================================+
   | ``ICEBERG_PARM01``                     |                                            |                                                                                                         |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGmaskFile`             | :kbd:`' '`                                 | File containing iceberg mask and flag for orientation                                                   |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGmeltFile`             | :kbd:`' '`                                 | File containing XY mask of where to calculate iceberg melt (1 = melt, 0 no melt)                        |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGmaskNumsFile`         | :kbd:`' '`                                 | File containing arbitrary numbers for each cell containing icebergs. Use in per berg diagnostics        |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGlengthFile`           | :kbd:`' '`                                 | File containing lengths of all icebergs.                                                                |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGwidthsFile`           | :kbd:`' '`                                 | File containing widths of all icebergs.                                                                 |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGdepthsFile`           | :kbd:`' '`                                 | File containing depths of all icebergs.                                                                 |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGnumPerCellFile`       | :kbd:`' '`                                 | File containing number of icebergs per cell.                                                            |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGdriftFile`            | :kbd:`' '`                                 | File containing mask of where effect of iceberg drift on melting will be calculated (1 = drift)         |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGbarrierFile`          | :kbd:`' '`                                 | File containing mask for where icebergs act as physical barrier to water flow (1 = block+drag)          |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGopenFracFile`         | :kbd:`' '`                                 | File containing proportion of cell volume that is open (i.e. not taken up by icebergs)                  |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ICEBERGareaFile`             | :kbd:`' '`                                 | File containing total submerged iceberg surface area in each cell.                                      |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_Rho`                     | 917.0E+00                                  | Density of ice (kg/m\ :sup:`3`)                                                                         |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_iceTemp`                 | 0.0E+00                                    | Mean temperature of ice (:sup:`o`\ C)                                                                   |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_uMin`                    | 6.0E-02                                    | Constant minimum background velocity applied to iceberg faces (m/s)                                     |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_lambda1`                 | -5.73E-02                                  | Freezing point slope  (:sup:`o`\ C /PSU)                                                                |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_lambda2`                 | 8.32E-02                                   | Freezing point offset for 3 EQ melt (:sup:`o`\ C)                                                       |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_lambda3`                 | 7.61E-04                                   | Freezing point depth for 3 EQ melt                                                                      |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_GamT`                    | 2.2E-02                                    | Thermal turbulent transfer coeffcient                                                                   |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_GamS`                    | 6.2E-04                                    | Salt turbulent transfer coefficient                                                                     |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_c_w`                     | 3974.0E+00                                 | Heat capacity of water J /K /kg                                                                         |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_c_i`                     | 2000.0E+00                                 | Heat capacity of icebergs (J /K /kg)                                                                    |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_L`                       | 334000.0E+00                               | Latent heat of fusion (J /kg)                                                                           |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_Cd`                      | 2.5E-3                                     | Quadratic skin drag coefficient for melt calculation                                                    |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_DragForm`                | 2.5E-3                                     | Form drag coefficient across icebergs                                                                   |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_SelectDrag`              | 3                                          | Select how drag is computed from velocity (1:n = 2, 2:n = 2, 3: n = 1 + .75*hFacC, 4: 1 + .75*(hFacC)**3|
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_SelectFill`              | 3                                          | Select how frontal area scales with hFacC (1:linear, 2:quad, 3:quartic)                                 |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | ``ICEBERG_PARM02``                     |                                            |                                                                                                         |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_ptracerMaskFile`         | :kbd:`' '`                                 | File containing mask for ptracer number to be correspond to iceberg freshwater production               |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`brg_useInputPtracers`        | 0                                          | Select if using ptracers input                                                                          |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+

ICEBERG description
~~~~~~~~~~~~~~~~~~~

Ice mélange and icebergs have been shown to impact fjord circulation, heat and freshwater fluxes, and the submarine melting of glacier termini. When icebergs are larger than grid cells, small-scale models can resolve large icebergs using :filelink:`pkg/shelfice`, but for coarser grids and smaller icebergs, is it useful to account the effects of icebergs smaller than grid scale. Here, we have built the :filelink:`/pkg/iceberg` package to implement a novel, scalable parameterization to incorporate the impact of iceberg melt and drag for icebergs below the grid scale.

Geometry Files
^^^^^^^^^^^^^^
The geometry of icebergs is computed outside of MITgcm, and so it is important the user correctly prepare the ICEBERGxxFiles appropriately. Scripts for the generation of these files can be found within Summers et al. (2025) :cite:`summers:25` in python and Davison et al. (2020) :cite:`davison:20` in MATLAB. Both these references also have guidance on how to create iceberg size distributions using a power law distribution, motivated by observations of icebergs in Greenland.


Three-equations thermodynamics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :filelink:`/pkg/iceberg` package solves for ice meltrates using the Three-equations melt parameterization, following Jenkins et
al. (2001) :cite:`jenkins:01` and summarized in detail in :numref:`shelfice_diagnostics`. The thermodynamic component of 
:filelink:`/pkg/iceberg` is outlined in Davison et al. (2020) :cite:`davison:20`

The thermodynamic model loops over every iceberg, in every X,Y grid location where the :varlink:`ICEBERGmaskFile` :math:`\neq 0`, and :varlink:`ICEBERGmeltFile` :math:`= 1`. For each iceberg, meltrates are calculated at each depth level based on ambient ocean conditions using the 3 equation melt parameterization. This meltrate is then multiplied by the surface area of that iceberg at that depth level (including sides and bottom melt of the iceberg) to get total freshwater flux per iceberg. This total freshwater flux is then summed over all icebergs within the X,Y grid location for each depth level. This value of total freshwater production is then divided by iceberg surface area to get the average iceberg meltrate. The total freshwater production is divided by latent heat to solve for heat flux. 

To account for the relative speed of icebergs drifting in a current, cells with :varlink:`ICEBERGdriftFile` :math:`= 0` will calculate the melt velocity
used in the melt parameterization relative to the drifting velocity of each iceberg, determined as the average ocean velocity across 
the depth of the iceberg. 

.. math::
  u_{\rm drift} = \frac{\sum_{i \leq k} (u_{i} d_{i})}{\sum_{i \leq k} (d_{i})}
  :label: driftVelocity

where :math:`i` sums over depth levels from the surface to :math:`k`, the depth level of the iceberg. Though this effect accounts for iceberg drift on iceberg melt rates, the physical dimensions and locations of the icebergs do not change. The icebergs themselves do not experience melt or drifting. In practice, icebergs typically drift relatively close to their ocean currents, and so with this option enabled meltrates often falls into the regime where melt velocities are set by the ambient melt parameterization with melt velocity = :varlink:`brg_uMin`. 

Sub-grid parameterization of drag
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The drag component of :filelink:`/pkg/iceberg` is outlined in Summers et al. (2025) :cite:`summers:25`

To attempt to realistically capture the body drag effect of icebergs we use ocean velocity and ice volume fraction to compute a net blocking and drag effect that impacts the ocean momentum. This only done in cells where the mask defined in :varlink:`ICEBERGbarrierFile` = 1. This drag parameterization relies on ice volume fraction, defined as

.. math::
  \varphi = \frac{1}{\Delta x \Delta y \Delta z} \sum_{i} W_{i} \times L_{i} \times H_{i}
  :label: iceVolumeFraction

where the sum is over all icebergs :math:`i` within a grid cell, with widths :math:`W_{i}` and lengths :math:`L_{i}`, and :math:`H_{i}` is depth the iceberg extends into the cell centered at depth :math:`z`. This is loaded as :math:`1 - \rm openFrac` from :varlink:`ICEBERGopenFracFile`. Ice volume faction :math:`\varphi(z)` is then used to calculate drag using

.. math::
  \tau_{d} = \rho\, C_{bd} u^{\alpha(\varphi)}\Delta z\,\beta(\varphi) 
  :label: icebergDrag

Where :math:`\rho` is water density, :math:`C_{bd}` is the body drag parameter (:varlink:`brg_DragForm`), :math:`u` is the ocean speed, and :math:`\Delta z` is the vertical thickness of the cell. :math:`\alpha,\beta` are user adjustable with :varlink:`brg_SelectDrag` and :varlink:`brg_SelectFill`. 

.. math::
   \alpha(\varphi) = \left\{ 
   \begin{array}{cc}
   1 & \text{Linear} \\
   2 & \text{Quadratic} \\
   1 + .75*(1 - \varphi) & \text{Hybrid (default).} \\
  \end{array}
  \right.

.. math::
   \beta(\varphi) = \left\{ 
   \begin{array}{cc}
   \varphi & \text{Linear} \\
   -(\varphi-1)^2 + 1 & \text{Quadratic} \\
   -(\varphi-1)^4 + 1 & \text{Quartic (default).} \\
   \end{array}
   \right.

Icebergs themselves do not change size or location via influence from ocean currents. This drag calculation assumes icebergs are stationary compared to the model reference frame (U,V = 0). This drag effect will only be calculated for regions where the ICEBERGbarrierFile = 1. The intended use case of this feature is for icebergs 
within a rigid ice mélange, or grounded icebergs. 

Remark
^^^^^^
It is technically possible to enable iceberg drift for melt rates and enable iceberg drag as if the icebergs are locked in place for the same regions of icebergs, but this represents a nonphysical set of conditions. This option is left open for the user to have the most control, but we note that non-physical combinations of input parameters will not throw explicit errors or warnings. 

.. _iceberg_subroutines:

Key subroutines
~~~~~~~~~~~~~~~

The main routine is :filelink:`iceberg_thermodynamics.F <pkg/iceberg/iceberg_thermodynamics.F>`
but note that :filelink:`/pkg/iceberg` routines are also called when solving the momentum equations.

::

    C     !CALLING SEQUENCE:
    C ...
    C |-FORWARD_STEP           :: Step forward a time-step ( AT LAST !!! )
    C ...
    C | |-DO_OCEANIC_PHY       :: Control oceanic physics and parameterization
    C ...
    C | | |-ICEBERG_THERMODYNAMICS :: main routine for thermodynamics
    C                                  with diagnostics
    C ...
    C | |-THERMODYNAMICS       :: theta, salt + tracer equations driver.
    C ...
    C | | |-EXTERNAL_FORCING_T :: Problem specific forcing for temperature.
    C | | |-ICEBERG_FORCING_T :: apply heat fluxes from iceberg model
    C ...
    C | | |-EXTERNAL_FORCING_S :: Problem specific forcing for salinity.
    C | | |-ICEBERG_FORCING_S :: apply fresh water fluxes from iceberg model
    C ...
    C | |-DYNAMICS             :: Momentum equations driver.
    C ...
    C | | |-MOM_FLUXFORM       :: Flux form mom eqn. package 
    C ...
    C | | | |-ICEBERG_U_DRAG  :: apply drag along iceberg to u-equation
    C                             with diagnostics
    C ...
    C | | |-MOM_VECINV         :: Vector invariant form mom eqn. package 
    C ...
    C | | | |-ICEBERG_V_DRAG  :: apply drag along iceberg to v-equation
    C                             with diagnostics
    C ...
    C  o



.. _iceberg_diagnostics:

ICEBERG diagnostics
~~~~~~~~~~~~~~~~~~~

Diagnostics output is available via the diagnostics package (see :numref:`outp_pack`). ``BRGhFacC`` is created to track iceberg volume if it changes over time. Iceberg volume currently does not evolve within MITgcm, but for schemes coupled with MITgcm this may change over time. Thus, this is made available as diagnostic here. Available output fields are summarized as follows:


::

    ---------+----+----+----------------+-----------------
     <-Name->|Levs|grid|<--  Units   -->|<- Tile (max=80c)
    ---------+----+----+----------------+-----------------
     BRGfwFlx| 32 |SM  |m^3/s           |Iceberg freshwater flux
     BRGhtFlx| 32 |SM  |W/m^2           |Iceberg heat flux  (+ve cools ocean)
     BRGmltRt| 32 |SM  |m/d             |Iceberg melt rate
     BRGarea3| 32 |SM  |m^2             |Iceberg surface area
     BRG_TauX| 32 |UU  |N/m^2           |Iceberg drag stress, zonal. comp., >0 ++uVel
     BRG_TauY| 32 |VV  |N/m^2           |Iceberg drag stress, merid. comp., >0 ++vVel
     BRGhFacC| 32 |SM  |none            |Ocean fraction (HfacC) in icebergs


Experiments and tutorials that use iceberg
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See the verification experiment TBD
