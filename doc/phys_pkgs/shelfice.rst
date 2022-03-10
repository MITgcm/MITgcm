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

.. tabularcolumns:: |\Y{.32}|\Y{.1}|\Y{.570}|

.. table:: Compile-time parameters
   :name: tab_phys_pkg_shelfice_compileparms

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

:filelink:`pkg/shelfice` is switched on/off at run time by setting :varlink:`useSHELFICE` to ``.TRUE.`` in file ``data.pkg``.
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
:math:`p_{top}-g\sum_{k'=1}^{n-1}\rho_c \Delta{z}_{k'}`, with
:math:`\rho_c =` :varlink:`rhoConst`, so that in the absence of a :math:`\rho^{*}`
that is different from :math:`\rho_c`, the anomaly is zero.

.. tabularcolumns:: |\Y{.275}|\Y{.28}|\Y{.455}|

.. table:: Run-time parameters and default values; all parameters are in namelist group ``SHELFICE_PARM01``
   :name: tab_phys_pkg_shelfice_runtimeparms
   :class: longtable

   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Default                                    | Description                                                                                             |
   +========================================+============================================+=========================================================================================================+
   | :varlink:`useISOMIPTD`                 | FALSE                                      | use simplified ISOMIP thermodynamics on/off flag                                                        |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEconserve`            | FALSE                                      | use conservative form of temperature boundary conditions on/off flag                                    |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEboundaryLayer`       | FALSE                                      | use simple boundary layer mixing parameterization on/off flag                                           |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHI_withBL_realFWflux`       | FALSE                                      | with :varlink:`SHELFICEboundaryLayer`, allow to use real-FW flux                                        |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHI_withBL_uStarTopDz`       | FALSE                                      | with :varlink:`SHELFICEboundaryLayer`, compute uStar from uVel,vVel averaged over top Dz thickness      |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEloadAnomalyFile`     | :kbd:`' '`                                 | initial geopotential anomaly                                                                            |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEtopoFile`            | :kbd:`' '`                                 | filename for under-ice topography of ice shelves                                                        |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEmassFile`            | :kbd:`' '`                                 | filename for mass of ice shelves                                                                        |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEMassDynTendFile`     | :kbd:`' '`                                 | filename for mass tendency of ice shelves                                                               |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICETransCoeffTFile`     | :kbd:`' '`                                 | filename for spatially varying transfer coefficients                                                    |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICElatentHeat`          | 334.0E+03                                  | latent heat of fusion (J/kg)                                                                            |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEHeatCapacity_Cp`     | 2000.0E+00                                 | specific heat capacity of ice (J/kg/K)                                                                  |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`rhoShelfIce`                 | 917.0E+00                                  | (constant) mean density of ice shelf (kg/m\ :sup:`3`)                                                   |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEheatTransCoeff`      | 1.0E-04                                    | transfer coefficient (exchange velocity) for temperature (m/s)                                          |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEsaltTransCoeff`      | :varlink:`SHELFICEsaltToHeatRatio` *       | transfer coefficient (exchange velocity) for salinity (m/s)                                             |
   |                                        | :varlink:`SHELFICEheatTransCoeff`          |                                                                                                         |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEsaltToHeatRatio`     | 5.05E-03                                   | ratio of salinity to temperature transfer coefficients (non-dim.)                                       |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEkappa`               | 1.54E-06                                   | temperature diffusion coefficient of the ice shelf (m\ :sup:`2`\ /s)                                    |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEthetaSurface`        | -20.0E+00                                  | (constant) surface temperature above the ice shelf (:sup:`o`\ C)                                        |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`no_slip_shelfice`            | :varlink:`no_slip_bottom`                  | slip along bottom of ice shelf on/off flag                                                              |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEDragLinear`          | :varlink:`bottomDragLinear`                | linear drag coefficient at bottom ice shelf (m/s)                                                       |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEDragQuadratic`       | :varlink:`bottomDragQuadratic`             | quadratic drag coefficient at bottom ice shelf (non-dim.)                                               |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEselectDragQuadr`     | -1                                         | select form of quadratic drag coefficient (non-dim.)                                                    |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEMassStepping`        | FALSE                                      | recalculate ice shelf mass at every time step                                                           |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEDynMassOnly`         | FALSE                                      | if :varlink:`SHELFICEmassStepping` = TRUE, exclude freshwater flux contribution                         |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEadvDiffHeatFlux`     | FALSE                                      | use advective-diffusive heat flux into ice shelf instead of default diffusive heat flux                 |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEuseGammaFrict`       | FALSE                                      | use velocity dependent exchange coefficients (Holland and Jenkins 1999 :cite:`holland:99`)              |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICE_oldCalcUStar`       | FALSE                                      | use old uStar averaging expression                                                                      |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICEwriteState`          | FALSE                                      | write ice shelf state to file on/off flag                                                               |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICE_dumpFreq`           | :varlink:`dumpFreq`                        | dump frequency (s)                                                                                      |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`SHELFICE_dump_mnc`           | :varlink:`snapshot_mnc`                    | write snapshot using MNC  on/off flag                                                                   |
   +----------------------------------------+--------------------------------------------+---------------------------------------------------------------------------------------------------------+

SHELFICE description
~~~~~~~~~~~~~~~~~~~~

In the light of isomorphic equations for pressure and height
coordinates, the ice shelf topography on top of the water column has a
similar role as (and in the language of Marshall et al. (2004) :cite:`marshall:04`,
is isomorphic to) the orography and the pressure boundary conditions at
the bottom of the fluid for atmospheric and oceanic models in pressure
coordinates. The total pressure :math:`p_{\rm tot}` in the ocean can be
divided into the pressure at the top of the water column
:math:`p_{\rm top}`, the hydrostatic pressure and the non-hydrostatic
pressure contribution :math:`p_{\rm nh}`:

.. math::
   p_{\rm tot} = p_{\rm top} + \int_z^{\eta-h} g\,\rho\,dz + p_{\rm nh}
   :label: pressureocean


with the gravitational acceleration :math:`g`, the density
:math:`\rho`, the vertical coordinate :math:`z` (positive upwards), and
the dynamic sea-surface height :math:`\eta`. For the open ocean,
:math:`p_{\rm top}=p_{a}` (atmospheric pressure) and :math:`h=0`. Underneath
an ice-shelf that is assumed to be floating in isostatic equilibrium,
:math:`p_{\rm top}` at the top of the water column is the atmospheric
pressure :math:`p_{a}` plus the weight of the ice-shelf. It is this
weight of the ice-shelf that has to be provided as a boundary condition
at the top of the water column (in run-time parameter :varlink:`SHELFICEloadAnomalyFile`). The weight is
conveniently computed by integrating a density profile :math:`\rho^*`,
that is constant in time and corresponds to the sea-water replaced by
ice, from :math:`z=0` to a “reference” ice-shelf draft at :math:`z=-h` (Beckmann et al. (1999)
:cite:`beckmann:99`), so that

.. math::
   p_{\rm top} = p_{a} + \int_{-h}^{0}g\,\rho^{*}\,dz
   :label: ptop

Underneath the ice shelf, the “sea-surface height” :math:`\eta` is the
deviation from the “reference” ice-shelf draft :math:`h`. During a model
integration, :math:`\eta` adjusts so that the isostatic equilibrium is
maintained for sufficiently slow and large scale motion.

In MITgcm, the total pressure anomaly :math:`p'_{\rm tot}` which is used
for pressure gradient computations is defined by subtracting a purely
depth dependent contribution :math:`-g\rho_c z` using constant
reference density :math:`\rho_c` from :math:`p_{\rm tot}`.
:eq:`pressureocean` becomes

.. math::
     p_{\rm tot} = p_{\rm top} - g \rho_c (z+h)  + g \rho_c \eta + \, \int_z^{\eta-h}{ g (\rho-\rho_c) \, dz} + \, p_{\rm nh}
     :label: pressure

and after rearranging

.. math::
   p'_{\rm tot} = p'_{\rm top} + g \rho_c \eta + \, \int_z^{\eta-h}{g (\rho-\rho_c) \, dz} + \, p_{\rm nh}

with :math:`p'_{\rm tot} = p_{\rm tot} + g\,\rho_c\,z` and
:math:`p'_{\rm top} = p_{\rm top} - g\,\rho_c\,h`.
The non-hydrostatic pressure contribution :math:`p_{\rm nh}`
is neglected in the following.

In practice, the ice shelf contribution to :math:`p_{\rm top}` is computed
by integrating :eq:`ptop` from :math:`z=0` to the bottom of the
last fully dry cell within the ice shelf:

.. math::
   p_{\rm top} = g\,\sum_{k'=1}^{n-1}\rho_{k'}^{*}\Delta{z_{k'}} + p_{a}
   :label: surfacepressure

where :math:`n` is the vertical index of the first (at least partially)
“wet” cell and :math:`\Delta{z_{k'}}` is the thickness of the
:math:`k'`-th layer (counting downwards). The pressure anomaly for
evaluating the pressure gradient is computed in the center of the “wet”
cell :math:`k` as

.. math::
   p'_{k} = p'_{\rm top} + g\rho_{n}\eta +
   g\,\sum_{k'=n}^{k}\left((\rho_{k'}-\rho_c)\Delta{z_{k'}}
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
   g_{\theta,k}   = \frac{Q}{\rho_c c_{p} h_{k} \Delta{z}_{k}}
   \text{ and } g_{\theta,k+1} = 0
   :label: orgtendency

for layers :math:`k` and :math:`k+1` (:math:`c_{p}` is the heat
capacity). Averaging these terms over a layer thickness
:math:`\Delta{z_{k}}` (e.g., extending from the ice shelf base down to
the dashed line in cell C) and applying the averaged tendency to cell A
(in layer :math:`k`) and to the appropriate fraction of cells C (in
layer :math:`k+1`) yields

.. math::
   g_{\theta,k}^*   = \frac{Q}{\rho_c c_{p} \Delta{z}_{k}}
   :label: tendencyk

.. math::
   g_{\theta,k+1}^*
   = \frac{Q}{\rho_c c_{p} \Delta{z}_{k}}
   \frac{ \Delta{z}_{k} ( 1- h_{k} )}{\Delta{z}_{k+1}}
   :label: tendencykp1

:eq:`tendencykp1` describes averaging over the part of the grid
cell :math:`k+1` that is part of the boundary layer with tendency
:math:`g_{\theta,k}^*` and the part with no tendency. Salinity is
treated in the same way. The momentum equations are not modified.

Three-equations thermodynamics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Freezing and melting form a boundary layer between the ice shelf and the
ocean that is represented in the model by an infinitesimal layer used to
calculate the exchanges between the ocean and the ice. Melting and
freezing at the boundary between saline water and ice imply a freshwater
mass flux :math:`q` (:math:`<0` for melting); the relevant heat fluxes
into and out of the boundary layer therefore include a diffusive flux
through the ice, the latent heat flux due to melting and freezing, and
the advective heat that is carried by the freshwater mass flux. There is
a salt flux carried by the mass flux if the ice has a non-zero salinity
:math:`S_I`. Further, the position of the interface between ice and
ocean changes because of :math:`q`, so that, say, in the case of melting
the volume of sea water increases. As a consequence of these fluxes,
salinity and temperature are affected.

The turbulent tracer exchanges between the infinitesimal boundary layer and
the ocean are expressed as diffusive fluxes. Following Jenkins et
al. (2001) :cite:`jenkins:01`, the boundary conditions for a tracer take
into account that this boundary may not move with the ice ocean interface
(for example, in a linear free surface model). The implied upward freshwater
flux :math:`q` is therefore included in the boundary conditions for the
temperature and salinity equation as an advective flux.

The boundary conditions for tracer :math:`X=S,T` (tracer :math:`X` stands
for either in-situ temperature :math:`T` or salinity :math:`S`, located at
the first interior ocean grid point) in the ocean are expressed as the sum
of advective and diffusive fluxes

.. math::
   F_X = (\rho_c \, \gamma_{X} -q ) ( X_{b} - X )
   :label: jenkinsbc

where the diffusive flux has been parameterized as a turbulent exchange
:math:`\rho_c \, \gamma_{X}( X_{b} - X )` following Holland and
Jenkins (1999) :cite:`holland:99` or Jenkins et al. (2001)
:cite:`jenkins:01`. :math:`X_b` indicates the tracer in the boundary layer,
:math:`\rho_c` the density of seawater (parameter :varlink:`rhoConst`), and
:math:`\gamma_X` is the turbulent exchange (or transfer) coefficient
(parameters :varlink:`SHELFICEheatTransCoeff` and
:varlink:`SHELFICEsaltTransCoeff`), in units of an exchange
velocity. In-situ temperature, computed locally from tracer potential
temperature, is required here to accurately compute the in-situ freezing
point of seawater in order to determine ice melt.

The tracer budget for the infinitesimal boundary layer takes the general
form:

.. math::
   {\rho_I} K_{I,X} \frac{\partial{X_I}}{\partial{z}}\biggl|_{b}
   = \rho_c \, \gamma_{X} ( X_{b} - X ) -  q ( X_{b} - X_{I} )
   :label: jenkinsgenbudget

where the LHS represents diffusive flux from the ice evaluated at the
interface between the infinitesimal boundary layer and the ice, and the RHS
represents the turbulent and advective exchanges between the infinitesimal
layer and the ocean and the advective exchange between the boundary layer
and the ice (:math:`qX_{I}`, this flux will be zero if the ice contains no
tracer: :math:`X_I=0`). The temperature in the boundary layer (:math:`T_b`)
is taken to be at the freezing point, which is a function of pressure and
salinity, :math:`\rho_I` is ice density (:varlink:`rhoShelfIce`), and
:math:`K_{I,X}` the appropriate ice diffusivity.

For any material tracer such as salinity, the LHS in :eq:`jenkinsgenbudget`
vanishes (no material diffusion into the ice), while for temperature, the
term :math:`q\,( T_{b}-T_{I} )` vanishes, because both the boundary layer
and the ice are at the freezing point. Instead, the latent heat of freezing
is included as an additional term to take into account the conversion of
ice to water:

.. math::
   {\rho_I} \, c_{p,I} \, \kappa_{I,T}
   \frac{\partial{T_I}}{\partial{z}}\biggl|_{b}
   = c_{p} \, \rho_c \, \gamma_{T} ( T_{b} - T )+ L q.
   :label: jenkinsheatbudget

where :math:`\kappa_{I,T}` is the thermal ice diffusivity
(:varlink:`SHELFICEkappa`), :math:`L` is the latent heat of fusion
(:varlink:`SHELFICElatentHeat`), :math:`c_{p}` is the specific heat
capacity of water (:varlink:`HeatCapacity_Cp`), and :math:`c_{p,I}` the
heat capacity of the ice shelf (:varlink:`SHELFICEHeatCapacity_Cp`).  A
reasonable choice for :math:`\gamma_T` (:varlink:`SHELFICEheatTransCoeff`),
the turbulent exchange coefficient of temperature, is discussed in Holland
and Jenkins (1999) :cite:`holland:99` (see
:numref:`shelfice_exchange_coeff`).  The temperature at the interface
:math:`T_{b}` is assumed to be the in-situ freezing point temperature of
sea-water :math:`T_{f}`, which is computed from a linear equation of state:

.. math::
   T_{f} = (0.0901 - 0.0575\ S_{b})
   - 7.61 \times 10^{-4} p_{b}.
   :label: hellmerfreeze

where :math:`T_f` is given in :sup:`o`\ C and :math:`p_{b}` is in dBar. In
:eq:`jenkinsheatbudget`, the diffusive heat flux at the ice-ocean interface
can be appproximated by assuming a linear temperature profile in the ice
and approximating the vertical derivative of temperature in the ice as the
difference between the ice surface and ice bottom temperatures divided by
the ice thickness, so that the left-hand-side of :eq:`jenkinsheatbudget`
becomes

.. math::
   {\rho_I} \, c_{p,I} \, \kappa_{I,T}
   \frac{\partial{T_I}}{\partial{z}}\biggl|_{b}
   \approx \rho_{I} \, c_{p,I} \, \kappa_{I,T} \frac{(T_{S} - T_{b})}{h}
   :label: dTdzdiffus

where :math:`T_{S}` the (surface) temperature of the ice shelf
(:varlink:`SHELFICEthetaSurface`) and :math:`h` is the ice-shelf
draft. Alternatively, assuming that the ice is "advected" vertically as
implied by the meltflux :math:`q`, the diffusive flux can be approximated
as :math:`\min(q,0)\,c_{p,I} (T_{S} - T_{b})` (runtime flag
:varlink:`SHELFICEadvDiffHeatFlux`; see Holland and Jenkins, 1999
:cite:`holland:99` for details).

From the salt budget, the salt flux across the shelf ice-ocean interface is
equal to the salt flux due to melting and freezing:

.. math::
   \rho_c \, \gamma_{S} (S - S_{b}) = - q\,(S_{b}-S_{I})
   :label: hellmersaltbalance

where :math:`\gamma_S =` :varlink:`SHELFICEsaltToHeatRatio` :math:`*
\gamma_T` is the turbulent salinity exchange coefficient.  Note, it is
assumed that :math:`\kappa_{I,S} =0`; moreover, the salinity of the ice
shelf is generally neglected (:math:`S_{I}=0`).

The budget equations for temperature :eq:`dTdzdiffus` and salinity
:eq:`hellmersaltbalance`, together with the freezing point temperature of
sea-water :eq:`hellmerfreeze`, form the so-called three-equation-model
(e.g., Hellmer and Olbers (1989) :cite:`hellmer:89`, Jenkins et al. (2001)
:cite:`jenkins:01`). These equations are solved to obtain :math:`S_b, T_b,
q`, which are then substituted into :eq:`jenkinsbc` to obtain the boundary
conditions for the temperature and salinity equations of the ocean
model. Note that with :math:`S_{I}=0` and :eq:`hellmersaltbalance`, the
boundary flux for salinity becomes :math:`F_S = q\,S`, which is the flux
that is necessary to account for the dilution of salinity in the case of
melting.

The three-equation-model tends to yield smaller melt rates than the simpler
formulation of the ISOMIP protocol (:numref:`shelfice_isomip`) because the
freshwater flux (due to melting) decreases the salinity, which in turn
raises the freezing point temperature and thus leads to less melting at the
interface. For a simpler thermodynamics model where :math:`S_b` is not
computed explicitly, for example as in the ISOMIP protocol, :eq:`jenkinsbc`
cannot be applied directly. In this case :eq:`hellmersaltbalance` can be
used with :eq:`jenkinsbc` to obtain:

.. math:: F_X = q\,(S-S_I)

This formulation can be used for all cases for which
:eq:`hellmersaltbalance` is valid. Further, in this formulation it is
obvious that melting (:math:`q<0`) leads to a reduction of salinity.

The default value of :varlink:`SHELFICEconserve` ``=.FALSE.`` removes the
contribution :math:`q\, ( X_{b}-X )` from :eq:`jenkinsbc`, making the
boundary conditions non-conservative.

Solving the three-equations system
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There has been some confusion about the three-equations system, so we
document the solution in the code here: We use :eq:`hellmerfreeze`
:math:`T_{b} = a_{0} S_{b} + \epsilon_{4}` to eliminate :math:`T_{b}`
from :eq:`jenkinsheatbudget` with :eq:`dTdzdiffus` and find an
expression for the freshwater flux :math:`q`:

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

where the abbrevations
:math:`\epsilon_{1} = c_{p} \, \rho_c \, \gamma_{T}`,
:math:`\epsilon_{2} = \rho_c L \, \gamma_{S}`, :math:`\epsilon_{3} =
\frac{\rho_{I} \, c_{p,I} \, \kappa}{h}`,
:math:`\epsilon_{4}=b_{0}p + c_{0}`,
:math:`\epsilon_{q} = \epsilon_{1}\,(\epsilon_{4} - T) +
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

.. _shelfice_isomip:

ISOMIP thermodynamics
^^^^^^^^^^^^^^^^^^^^^

A simpler formulation follows the ISOMIP protocol. The
freezing and melting in the boundary layer between ice shelf and ocean
is parameterized following Grosfeld et al. (1997) :cite:`grosfeld:97`. In this
formulation :eq:`jenkinsheatbudget` reduces to

.. math::
   c_{p} \, \rho_c \, \gamma_T (T - T_{b})  = -L q
   :label: isomipheatbalance

and the fresh water flux :math:`q` is computed from

.. math::
   q = - \frac{c_{p} \, \rho_c \, \gamma_T (T - T_{b})}{L}
   :label: isomipfwflx

In order to use this formulation, set runtime parameter
:varlink:`useISOMIPTD` ``=.TRUE.`` in ``data.shelfice``.

.. _shelfice_exchange_coeff:

Exchange coefficients
^^^^^^^^^^^^^^^^^^^^^

The default exchange coefficents :math:`\gamma_{T/S}` are constant and
set by the run-time parameters :varlink:`SHELFICEheatTransCoeff` and
:varlink:`SHELFICEsaltTransCoeff` (see
:numref:`tab_phys_pkg_shelfice_runtimeparms`). If
:varlink:`SHELFICEuseGammaFrict` ``=.TRUE.``, exchange coefficients
are computed from drag laws and friction velocities estimated from
ocean speeds following Holland and Jenkins (1999)
:cite:`holland:99`. This computation can be modified using runtime
parameters and the user is referred to
:filelink:`pkg/shelfice/shelfice_readparms.F` for details.

Remark
^^^^^^

The shelfice package and experiments demonstrating its strengths and
weaknesses are also described in Losch (2008)
:cite:`losch:08`. Unfortunately, the description of the
thermodynamics in the appendix of Losch (2008) is wrong.

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
