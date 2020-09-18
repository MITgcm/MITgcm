SHELFICE Package
----------------

Authors: Martin Losch, Jean-Michel Campin

Introduction
~~~~~~~~~~~~

:filelink:`pkg/shelfice` provides a thermodynamic model for basal melting
underneath floating ice shelves.

CPP options enable or disable different aspects of the package
(:numref:`shelfice_config`). Run-time options, flags, filenames and
field-related dates/times are described in :numref:`shelfice_runtime`. A description of key subroutines is given
in :numref:`shelfice_subroutines`. Available diagnostics output is listed in
:numref:`shelfice_diagnostics`.


.. _shelfice_config:

SHELFICE configuration
~~~~~~~~~~~~~~~~~~~~~~
 
As with all MITgcm packages, :filelink:`pkg/shelfice` can be turned on or off at compile
time:

-  using the ``packages.conf`` file by adding ``shelfice`` to it,

-  or using :filelink:`genmake2 <tools/genmake2>` adding ``-enable=shelfice`` or ``disable=shelfice`` switches

:filelink:`pkg/shelfice` does not require any additional packages, but it will only
work with conventional vertical :math:`z`-coordinates (pressure
coordinates are not implemented). If you use it together with
vertical mixing schemes, be aware that non-local parameterizations
are turned off, e.g., such as :filelink:`pkg/kpp`.

Parts of the :filelink:`pkg/shelfice` code can be enabled or disabled at compile time
via CPP preprocessor flags. These options are set in :filelink:`SHELFICE_OPTIONS.h <pkg/shelfice/SHELFICE_OPTIONS.h>`:

.. tabularcolumns:: |\Y{.475}|\Y{.1}|\Y{.45}|

+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| CPP Flag Name                                 | Default | Description                                                                                                          |
+===============================================+=========+======================================================================================================================+
| :varlink:`ALLOW_SHELFICE_DEBUG`               | #undef  | include code for enhanced diagnostics and debug output                                                               |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`ALLOW_ISOMIP_TD`                    | #define | include code for for simplified ISOMIP thermodynamics                                                                |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`SHI_ALLOW_GAMMAFRICT`               | #define | allow friction velocity-dependent transfer coefficient following Holland and Jenkins (1999) :cite:`holland:99`       |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

.. _shelfice_runtime:

SHELFICE run-time parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:filelink:`pkg/shelfice` is switched on/off at run-time by setting :varlink:`useSHELFICE` to ``.TRUE.`` in file ``data.pkg``.
Run-time parameters are set in file ``data.shelfice`` (read in :filelink:`pkg/shelfice/shelfice_readparms.F`),as listed below.

The data file specifying under-ice topography of ice shelves (:varlink:`SHELFICEtopoFile`) is in meters; upwards is positive,
and as for the bathymetry files, negative values are required for topography below the sea-level.
The data file for the pressure load anomaly at the bottom of the ice shelves :varlink:`SHELFICEloadAnomalyFile` is in pressure
units (Pa). This field is absolutely required to avoid large
excursions of the free surface during initial adjustment processes,
obtained by integrating an approximate density from the surface at
:math:`z=0` down to the bottom of the last fully dry cell within the
ice shelf, see :eq:`surfacepressure`. Note however the file :varlink:`SHELFICEloadAnomalyFile` must
not be :math:`p_{top}`, but
:math:`p_{top}-g\sum_{k'=1}^{n-1}\rho_{0}\Delta{z}_{k'}`, with
:math:`\rho_{0}=` :varlink:`rhoConst`, so that in the absence of a :math:`\rho^{*}`
that is different from :math:`\rho_{0}`, the anomaly is zero.

.. tabularcolumns:: |\Y{.27}|\Y{.21}|\Y{.205}|\Y{.34}|

.. table:: Run-time parameters and default values
   :name: tab_phys_pkg_shelfice_runtimeparms
   :class: longtable

   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group           | Default                                    | Description                                                                                             |
   +========================================+=================+============================================+=========================================================================================================+
   | :varlink:`useISOMIPTD`                 | SHELFICE_PARM01 | FALSE                                      | use simplified ISOMIP thermodynamics on/off flag                                                        |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEconserve`            | SHELFICE_PARM01 | FALSE                                      | use conservative form of temperature boundary conditions on/off flag                                    |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEboundaryLayer`       | SHELFICE_PARM01 | FALSE                                      | use simple boundary layer mixing parameterization on/off flag                                           |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHI_withBL_realFWflux`       | SHELFICE_PARM01 | FALSE                                      | with :varlink:`SHELFICEboundaryLayer`, allow to use real-FW flux                                        |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHI_withBL_uStarTopDz`       | SHELFICE_PARM01 | FALSE                                      | with :varlink:`SHELFICEboundaryLayer`, compute uStar from uVel,vVel averaged over top Dz thickness      |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEloadAnomalyFile`     | SHELFICE_PARM01 | :kbd:`' '`                                 | initial geopotential anomaly                                                                            |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEtopoFile`            | SHELFICE_PARM01 | :kbd:`' '`                                 | filename for under-ice topography of ice shelves                                                        |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEmassFile`            | SHELFICE_PARM01 | :kbd:`' '`                                 | filename for mass of ice shelves                                                                        |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEMassDynTendFile`     | SHELFICE_PARM01 | :kbd:`' '`                                 | filename for mass tendency of ice shelves                                                               |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICETransCoeffTFile`     | SHELFICE_PARM01 | :kbd:`' '`                                 | filename for spatially varying transfer coefficients                                                    |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICElatentHeat`          | SHELFICE_PARM01 | 334.0E+03                                  | latent heat of fusion (J/kg)                                                                            |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEHeatCapacity_Cp`     | SHELFICE_PARM01 | 2000.0E+00                                 | specific heat capacity of ice (J/kg/K)                                                                  |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`rhoShelfIce`                 | SHELFICE_PARM01 | 917.0E+00                                  | (constant) mean density of ice shelf (kg/m\ :sup:`3`)                                                   |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEheatTransCoeff`      | SHELFICE_PARM01 | 1.0E-04                                    | transfer coefficient (exchange velocity) for temperature (m/s)                                          |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEsaltTransCoeff`      | SHELFICE_PARM01 | 5.05E-03 :math:`\times`                    | transfer coefficient (exchange velocity) for salinity (m/s)                                             |
   |                                        |                 | :varlink:`SHELFICEheatTransCoeff`          |                                                                                                         |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEkappa`               | SHELFICE_PARM01 | 1.54E-06                                   | temperature diffusion coefficient of the ice shelf (m\ :sup:`2`\ /s)                                    |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEthetaSurface`        | SHELFICE_PARM01 | -20.0E+00                                  | (constant) surface temperature above the ice shelf (:sup:`o`\ C)                                        |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`no_slip_shelfice`            | SHELFICE_PARM01 | :varlink:`no_slip_bottom`                  | slip along bottom of ice shelf on/off flag                                                              |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEDragLinear`          | SHELFICE_PARM01 | :varlink:`bottomDragLinear`                | linear drag coefficient at bottom ice shelf (m/s)                                                       |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEDragQuadratic`       | SHELFICE_PARM01 | :varlink:`bottomDragQuadratic`             | quadratic drag coefficient at bottom ice shelf (non-dim.)                                               |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEselectDragQuadr`     | SHELFICE_PARM01 | -1                                         | select form of quadratic drag coefficient (non-dim.)                                                    |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEMassStepping`        | SHELFICE_PARM01 | FALSE                                      | recalculate ice shelf mass at every time step                                                           |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEDynMassOnly`         | SHELFICE_PARM01 | FALSE                                      | if :varlink:`SHELFICEmassStepping` = TRUE, exclude freshwater flux contribution                         |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEadvDiffHeatFlux`     | SHELFICE_PARM01 | FALSE                                      | use advective-diffusive heat flux into ice shelf instead of default diffusive heat flux                 |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEuseGammaFrict`       | SHELFICE_PARM01 | FALSE                                      | use velocity dependent exchange coefficients (Holland and Jenkins 1999 :cite:`holland:99`)              |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICE_oldCalcUStar`       | SHELFICE_PARM01 | FALSE                                      | use old uStar averaging expression                                                                      |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEwriteState`          | SHELFICE_PARM01 | FALSE                                      | write ice shelf state to file on/off flag                                                               |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICE_dumpFreq`           | SHELFICE_PARM01 | :varlink:`dumpFreq`                        | dump frequency (s)                                                                                      |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICE_dump_mnc`           | SHELFICE_PARM01 | :varlink:`snapshot_mnc`                    | write snapshot using MNC  on/off flag                                                                   |
   +----------------------------------------+-----------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+

SHELFICE description
~~~~~~~~~~~~~~~~~~~~

In the light of isomorphic equations for pressure and height
coordinates, the ice shelf topography on top of the water column has a
similar role as (and in the language of Marshall et al. (2004) :cite:`marshall:04`,
is isomorphic to) the orography and the pressure boundary conditions at
the bottom of the fluid for atmospheric and oceanic models in pressure
coordinates. The total pressure :math:`p_{tot}` in the ocean can be
divided into the pressure at the top of the water column
:math:`p_{top}`, the hydrostatic pressure and the non-hydrostatic
pressure contribution :math:`p_{NH}`:

.. math::
   p_{tot} = p_{top} + \int_z^{\eta-h} g\,\rho\,dz + p_{NH}
   :label: pressureocean


with the gravitational acceleration :math:`g`, the density
:math:`\rho`, the vertical coordinate :math:`z` (positive upwards), and
the dynamic sea-surface height :math:`\eta`. For the open ocean,
:math:`p_{top}=p_{a}` (atmospheric pressure) and :math:`h=0`. Underneath
an ice-shelf that is assumed to be floating in isostatic equilibrium,
:math:`p_{top}` at the top of the water column is the atmospheric
pressure :math:`p_{a}` plus the weight of the ice-shelf. It is this
weight of the ice-shelf that has to be provided as a boundary condition
at the top of the water column (in run-time parameter :varlink:`SHELFICEloadAnomalyFile`). The weight is
conveniently computed by integrating a density profile :math:`\rho^*`,
that is constant in time and corresponds to the sea-water replaced by
ice, from :math:`z=0` to a “reference” ice-shelf draft at :math:`z=-h` (Beckmann et al. (1999)
:cite:`beckmann:99`), so that

.. math::
   p_{top} = p_{a} + \int_{-h}^{0}g\,\rho^{*}\,dz
   :label: ptop

Underneath the ice shelf, the “sea-surface height” :math:`\eta` is the
deviation from the “reference” ice-shelf draft :math:`h`. During a model
integration, :math:`\eta` adjusts so that the isostatic equilibrium is
maintained for sufficiently slow and large scale motion.

In MITgcm, the total pressure anomaly :math:`p'_{tot}` which is used
for pressure gradient computations is defined by substracting a purely
depth dependent contribution :math:`-g\rho_{0}z` with a constant
reference density :math:`\rho_{0}` from :math:`p_{tot}`.
:eq:`pressureocean` becomes

.. math::
     p_{tot} = p_{top} - g \rho_0 (z+h)  + g \rho_0 \eta + \, \int_z^{\eta-h}{ g (\rho-\rho_0) \, dz} + \, p_{NH}
     :label: pressure

and after rearranging

.. math::
   p'_{tot} = p'_{top} + g \rho_0 \eta + \, \int_z^{\eta-h}{g (\rho-\rho_0) \, dz} + \, p_{NH}

with :math:`p'_{tot} = p_{tot} + g\,\rho_0\,z` and
:math:`p'_{top} = p_{top} -
g\,\rho_0\,h`. The non-hydrostatic pressure contribution :math:`p_{NH}`
is neglected in the following.

In practice, the ice shelf contribution to :math:`p_{top}` is computed
by integrating :eq:`ptop` from :math:`z=0` to the bottom of the
last fully dry cell within the ice shelf:

.. math::
   p_{top} = g\,\sum_{k'=1}^{n-1}\rho_{k'}^{*}\Delta{z_{k'}} + p_{a}
   :label: surfacepressure

where :math:`n` is the vertical index of the first (at least partially)
“wet” cell and :math:`\Delta{z_{k'}}` is the thickness of the
:math:`k'`-th layer (counting downwards). The pressure anomaly for
evaluating the pressure gradient is computed in the center of the “wet”
cell :math:`k` as

.. math::
   p'_{k} = p'_{top} + g\rho_{n}\eta +
   g\,\sum_{k'=n}^{k}\left((\rho_{k'}-\rho_{0})\Delta{z_{k'}}
     \frac{1+H(k'-k)}{2}\right)
   :label: discretizedpressure

where :math:`H(k'-k)=1` for :math:`k'<k` and :math:`0` otherwise.

 .. figure:: figs/gridschematic.*
    :width: 80%
    :align: center
    :alt: schematic of vertical section of grid
    :name: shelfice_grid

    Schematic of a vertical section of the grid at the base of an ice shelf. Grid lines are thin;
    the thick line is the model’s representation of the ice shelf-water interface. Plus signs mark the position
    of pressure points for pressure gradient computations. The letters A, B, and C mark specific grid cells for
    reference. :math:`h_k` is the fractional cell thickness so that :math:`h_k \Delta z_k` is the actual cell thickness.


Setting :varlink:`SHELFICEboundaryLayer` ``=.TRUE.`` introduces a simple boundary layer that reduces the potential
noise problem at the cost of increased vertical mixing. For this purpose
the water temperature at the :math:`k`-th layer abutting ice shelf
topography for use in the heat flux parameterizations is computed as a
mean temperature :math:`\overline{\theta}_{k}` over a boundary layer of
the same thickness as the layer thickness :math:`\Delta{z}_{k}`:

.. math::
   \overline{\theta}_{k} = \theta_{k} h_{k} + \theta_{k+1} (1-h_{k})
   :label: thetabl

where :math:`h_{k}\in[0,1]` is the fractional layer thickness of the
:math:`k`-th layer (see :numref:`shelfice_grid`). The original contributions due to ice shelf-ocean
interaction :math:`g_{\theta}` to the total tendency terms
:math:`G_{\theta}` in the time-stepping equation
:math:`\theta^{n+1} = f(\theta^{n},\Delta{t},G_{\theta}^{n})` are

.. math::
   g_{\theta,k}   = \frac{Q}{\rho_{0} c_{p} h_{k} \Delta{z}_{k}}
   \text{ and } g_{\theta,k+1} = 0
   :label: orgtendency

for layers :math:`k` and :math:`k+1` (:math:`c_{p}` is the heat
capacity). Averaging these terms over a layer thickness
:math:`\Delta{z_{k}}` (e.g., extending from the ice shelf base down to
the dashed line in cell C) and applying the averaged tendency to cell A
(in layer :math:`k`) and to the appropriate fraction of cells C (in
layer :math:`k+1`) yields

.. math::
   g_{\theta,k}^*   = \frac{Q}{\rho_{0} c_{p} \Delta{z}_{k}}
   :label: tendencyk

.. math::
   g_{\theta,k+1}^*
   = \frac{Q}{\rho_{0} c_{p} \Delta{z}_{k}}
   \frac{ \Delta{z}_{k} ( 1- h_{k} )}{\Delta{z}_{k+1}}
   :label: tendencykp1

:eq:`tendencykp1` describes averaging over the part of the grid
cell :math:`k+1` that is part of the boundary layer with tendency
:math:`g_{\theta,k}^*` and the part with no tendency. Salinity is
treated in the same way. The momentum equations are not modified.

Three-equations thermodynamics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Freezing and melting form a boundary layer between ice shelf and ocean.
Phase transitions at the boundary between saline water and ice imply the
following fluxes across the boundary: the freshwater mass flux :math:`q`
(:math:`<0` for melting); the heat flux that consists of the diffusive
flux through the ice, the latent heat flux due to melting and freezing
and the heat that is carried by the mass flux; and the salinity that is
carried by the mass flux, if the ice has a non-zero salinity
:math:`S_I`. Further, the position of the interface between ice and
ocean changes because of :math:`q`, so that, say, in the case of melting
the volume of sea water increases. As a consequence salinity and
temperature are modified.

The turbulent exchange terms for tracers at the ice-ocean interface are
generally expressed as diffusive fluxes. Following Jenkins et al. (2001)
:cite:`jenkins:01`, the boundary conditions for a tracer
take into account that this boundary is not a material surface. The
implied upward freshwater flux :math:`q` (in mass units, negative for
melting) is included in the boundary conditions for the temperature and
salinity equation as an advective flux:

.. math::
   {\rho}K\frac{\partial{X}}{\partial{z}}\biggl|_{b}
   = (\rho\gamma_{X}-q) ( X_{b} - X )
   :label: jenkinsbc

where tracer :math:`X` stands for either temperature :math:`T` or
salinity :math:`S`. :math:`X_b` is the tracer at the interface (taken to
be at freezing), :math:`X` is the tracer at the first interior grid
point, :math:`\rho` is the density of seawater, and :math:`\gamma_X` is
the turbulent exchange coefficient (in units of an exchange velocity).
The left hand side of :eq:`jenkinsbc` is shorthand for the
(downward) flux of tracer :math:`X` across the boundary. :math:`T_b`,
:math:`S_b` and the freshwater flux :math:`q` are obtained from solving
a system of three equations that is derived from the heat and freshwater
balance at the ice ocean interface.

In this so-called three-equation-model (e.g., Hellmer and Olbers (1989)
:cite:`hellmer:89`, Jenkins et al. (2001) :cite:`jenkins:01`)
the heat balance at the ice-ocean interface is expressed as

.. math::
   c_{p} \rho \gamma_T (T - T_{b})
   +\rho_{I} c_{p,I} \kappa \frac{(T_{S} - T_{b})}{h} = -Lq
   :label: hellmerheatbalance

where :math:`\rho` is the density of sea-water,
:math:`c_{p} = 3974 \, \text{J kg}^{-1} \text{K}^{-1}` is the specific heat
capacity of water and :math:`\gamma_T` the turbulent exchange
coefficient of temperature. The value of :math:`\gamma_T` is discussed
in Holland and Jenkins (1999) :cite:`holland:99`. :math:`L =
334000 \, \text{J kg}^{-1}` is the latent heat of fusion.
:math:`\rho_{I} = 920 \, \text{kg m}^{-3}`, :math:`c_{p,I} =
2000 \, \text{J kg}^{-1} \text{K}^{-1}`, and :math:`T_{S}` are the density,
heat capacity and the surface temperature of the ice shelf;
:math:`\kappa=1.54\times10^{-6} \, \text{m}^2 \text{s}^{-1}` is the heat
diffusivity through the ice-shelf and :math:`h` is the ice-shelf draft.
The second term on the right hand side describes the heat flux through
the ice shelf. A constant surface temperature :math:`T_S=-20^{\circ}\text{C}` is
imposed. :math:`T` is the temperature of the model cell adjacent to the
ice-water interface. The temperature at the interface :math:`T_{b}` is
assumed to be the in-situ freezing point temperature of sea-water
:math:`T_{f}` which is computed from a linear equation of state

.. math::
   T_{f} = (0.0901 - 0.0575\ S_{b})^{\circ}
   - 7.61 \times 10^{-4}\frac{\text{K}}{\text{dBar}}\ p_{b}
   :label: hellmerfreeze

with the salinity :math:`S_{b}` and the pressure :math:`p_{b}` (in dBar)
in the cell at the ice-water interface. From the salt budget, the salt
flux across the shelf ice-ocean interface is equal to the salt flux due
to melting and freezing:

.. math::
    \rho \gamma_{S} (S - S_{b}) = - q\,(S_{b}-S_{I})
   :label: hellmersaltbalance

where :math:`\gamma_S = 5.05\times10^{-3}\gamma_T` is the turbulent
salinity exchange coefficient, and :math:`S` and :math:`S_{b}` are
defined in analogy to temperature as the salinity of the model cell
adjacent to the ice-water interface and at the interface, respectively.
Note, that the salinity of the ice shelf is generally neglected
(:math:`S_{I}=0`). :eq:`hellmerheatbalance` to
:eq:`hellmersaltbalance` can be solved for :math:`S_{b}`,
:math:`T_{b}`, and the freshwater flux :math:`q` due to melting. These
values are substituted into expression :eq:`jenkinsbc` to obtain the
boundary conditions for the temperature and salinity equations of the
ocean model. This formulation tends to yield smaller melt rates than the
simpler formulation of the ISOMIP protocol because the freshwater flux
due to melting decreases the salinity which raises the freezing point
temperature and thus leads to less melting at the interface. For a
simpler thermodynamics model where :math:`S_b` is not computed
explicitly, for example as in the ISOMIP protocol,
:eq:`jenkinsbc` cannot be applied directly. In this case
:eq:`hellmersaltbalance` can be used with :eq:`jenkinsbc` to obtain:

.. math:: \rho{K}\frac{\partial{S}}{\partial{z}}\biggl|_{b}  = q\,(S-S_I)

This formulation can be used for all cases for which
:eq:`hellmersaltbalance` is valid. Further, in this
formulation it is obvious that melting (:math:`q<0`) leads to a
reduction of salinity.

The default value of :varlink:`SHELFICEconserve` ``=.FALSE.`` removes the contribution :math:`q\, ( X_{b}-X )` from
:eq:`jenkinsbc`, making the boundary conditions for temperature
non-conservative.

Solving the three-equations system
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There has been some confusion about the three-equations system, so we document the solution in the code here:
We use :eq:`hellmerfreeze` :math:`T_{b} = a_{0} S_{b} + \epsilon_{4}` to eliminate :math:`T_{b}`
from :eq:`hellmerheatbalance` and find an expression for the freshwater flux :math:`q`:

.. math::
   \begin{aligned}
   -Lq &= \epsilon_{1} (T - a_{0} S_{b} - \epsilon_{4})
   + \epsilon_{3} (T_{S} - a_{0} S_{b} - \epsilon_{4}) \\
   \Leftrightarrow Lq &=  a_{0}\,(\epsilon_{1} + \epsilon_{3})\,S_{b}
     + \epsilon_{q}
   \end{aligned}
   :label: solvedmeltrate

to be substituted into :eq:`hellmersaltbalance`:

.. math::
   \begin{aligned}
   \epsilon_{2}\,(S - S_{b}) &= - Lq\,(S_{b}-S_{I})
   = - (a_{0}\,(\epsilon_{1} + \epsilon_{3})\,S_{b}
     + \epsilon_{q})\,(S_{b}-S_{I}) \\
   \Leftrightarrow 0 &= a_{0}\,(\epsilon_{1} + \epsilon_{3})\,S_{b}^{2}
   + \{ \epsilon_{q}  - \epsilon_{2}
     - a_{0}\,(\epsilon_{1} + \epsilon_{3})\,S_{I} \}\,S_{b}
     + \epsilon_{2}\,S - \epsilon_{q}\,S_{I}
   \end{aligned}

where the abbrevations :math:`\epsilon_{1} = c_{p} \rho \gamma_{T}`,
:math:`\epsilon_{2} = \rho L \gamma_{S}`, :math:`\epsilon_{3} =
\frac{\rho_{I} c_{p,I} \kappa}{h}`, :math:`\epsilon_{4}=b_{0}p +
c_{0}`, :math:`\epsilon_{q} = \epsilon_{1}\,(\epsilon_{4} - T) +
\epsilon_{3}\,(\epsilon_{4} - T_{S})` have been introduced. The
quadratic equation in :math:`S_{b}` is solved and the smaller
non-negative root is used. In the MITgcm code, the ice shelf salinity
:math:`S_{I}` is always zero and the quadratic equation simplifies to

.. math::
   \begin{aligned}
   0 &= a_{0}\,(\epsilon_{1} + \epsilon_{3})\,S_{b}^{2}
   + (\epsilon_{q}  - \epsilon_{2}) \,S_{b} + \epsilon_{2}\,S \\
     S_{b} &= \frac{\epsilon_{2} - \epsilon_{q}\mp
     \sqrt{(\epsilon_{q}  - \epsilon_{2})^2
     - 4\, a_{0}\,(\epsilon_{1} + \epsilon_{3})\,\epsilon_{2}}}
     {2\,a_{0}\,(\epsilon_{1} + \epsilon_{3})}
   \end{aligned}

With :math:`S_b`, the boundary layer temperature :math:`T_b` and the
melt rate :math:`q` are known through :eq:`hellmerfreeze` and
:eq:`solvedmeltrate`.

ISOMIP thermodynamics
^^^^^^^^^^^^^^^^^^^^^

A simpler formulation follows the ISOMIP protocol. The
freezing and melting in the boundary layer between ice shelf and ocean
is parameterized following Grosfeld et al. (1997) :cite:`grosfeld:97`. In this
formulation :eq:`hellmerheatbalance` reduces to

.. math::
   c_{p} \rho \gamma_T (T - T_{b})  = -Lq
   :label: isomipheatbalance

and the fresh water flux :math:`q` is computed from

.. math::
   q = - \frac{c_{p} \rho \gamma_T (T - T_{b})}{L}
   :label: isomipfwflx

In order to use this formulation, set run-time parameter :varlink:`useISOMIPTD` ``=.TRUE.`` in
``data.shelfice``.

Exchange coefficients
^^^^^^^^^^^^^^^^^^^^^

The default exchange coefficents :math:`\gamma_{T/S}` are constant and
set by the runtime parameters :varlink:`SHELFICEheatTransCoeff` and
:varlink:`SHELFICEsaltTransCoeff` (see
:numref:`tab_phys_pkg_shelfice_runtimeparms`). If
:varlink:`SHELFICEuseGammaFrict` ``=.TRUE.``, exchange coefficients
are computed from drag laws and friction velocities estimated from
ocean speeds following Holland and Jenkins (1999)
:cite:`holland:99`. This computation can be modified using runtime
parameters and user is referred to S/R
:filelink:`pkg/shelfice/shelfice_readparms.F` for details.

Remark
^^^^^^

The shelfice package and experiments demonstrating its strengths and
weaknesses are also described in Losch (2008) :cite:`losch:08`. Unfortunately however,
the description of the thermodynamics in the
appendix of Losch (2008) is wrong.

.. _shelfice_subroutines:

Key subroutines
~~~~~~~~~~~~~~~

The main routine is :filelink:`shelfice_thermodynamics.F <pkg/shelfice/shelfice_thermodynamics.F>`
but note that :filelink:`/pkg/shelfice` routines are also called when solving the momentum equations.

::

    C     !CALLING SEQUENCE:
    C ...
    C |-FORWARD_STEP           :: Step forward a time-step ( AT LAST !!! )
    C ...
    C | |-DO_OCEANIC_PHY       :: Control oceanic physics and parameterization
    C ...
    C | | |-SHELFICE_THERMODYNAMICS :: main routine for thermodynamics
    C                                  with diagnostics
    C ...
    C | |-THERMODYNAMICS       :: theta, salt + tracer equations driver.
    C ...
    C | | |-EXTERNAL_FORCING_T :: Problem specific forcing for temperature.
    C | | |-SHELFICE_FORCING_T :: apply heat fluxes from ice shelf model
    C ...
    C | | |-EXTERNAL_FORCING_S :: Problem specific forcing for salinity.
    C | | |-SHELFICE_FORCING_S :: apply fresh water fluxes from ice shelf model
    C ...
    C | |-DYNAMICS             :: Momentum equations driver.
    C ...
    C | | |-MOM_FLUXFORM       :: Flux form mom eqn. package ( see
    C ...
    C | | | |-SHELFICE_U_DRAG  :: apply drag along ice shelf to u-equation
    C                             with diagnostics
    C ...
    C | | |-MOM_VECINV         :: Vector invariant form mom eqn. package ( see
    C ...
    C | | | |-SHELFICE_V_DRAG  :: apply drag along ice shelf to v-equation
    C                             with diagnostics
    C ...
    C  o



.. _shelfice_diagnostics:

SHELFICE diagnostics
~~~~~~~~~~~~~~~~~~~~

Diagnostics output is available via the diagnostics package (see
:numref:`outp_pack`). Available output fields are summarized as follows:


::

    ---------+----+----+----------------+-----------------
     <-Name->|Levs|grid|<--  Units   -->|<- Tile (max=80c)
    ---------+----+----+----------------+-----------------
     SHIfwFlx|  1 |SM  |kg/m^2/s        |Ice shelf fresh water flux (positive upward)
     SHIhtFlx|  1 |SM  |W/m^2           |Ice shelf heat flux  (positive upward)
     SHIUDrag| 30 |UU  |m/s^2           |U momentum tendency from ice shelf drag
     SHIVDrag| 30 |VV  |m/s^2           |V momentum tendency from ice shelf drag
     SHIForcT|  1 |SM  |W/m^2           |Ice shelf forcing for theta, >0 increases theta
     SHIForcS|  1 |SM  |g/m^2/s         |Ice shelf forcing for salt, >0 increases salt

Experiments and tutorials that use shelfice
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See the verification experiment :filelink:`isomip <verification/isomip>` for example usage of :filelink:`pkg/shelfice`.
