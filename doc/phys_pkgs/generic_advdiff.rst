.. _sec_phys_pkg_gad:

Generic Advection/Diffusion
---------------------------


The generic_advdiff package contains high-level subroutines to solve
the advection-diffusion equation of any tracer, either active (potential
temperature, salinity or water vapor) or passive (see :ref:`pkg/ptracer <sub_phys_pkg_ptracers>`).
(see also :numref:`tracer_eqns` and
:numref:`advection_schemes`).

Introduction
++++++++++++

Package “generic_advdiff” provides a common set of routines for
calculating advective/diffusive fluxes for tracers (cell centered
quantities on a C-grid).

Many different advection schemes are available: the standard centered
second order, centered fourth order and upwind biased third order
schemes are known as linear methods and require some stable
time-stepping method such as Adams-Bashforth. Alternatives such as
flux-limited schemes are stable in the forward sense and are best
combined with the multi-dimensional method provided in :filelink:`gad\_advection <pkg/generic_advdiff/gad_advection.F>`.

Key subroutines, parameters and files
+++++++++++++++++++++++++++++++++++++

There are two high-level routines:

-  :filelink:`GAD\_CALC\_RHS <pkg/generic_advdiff/gad_calc_rhs.F>` calculates all fluxes at time level “n” and is used
   for the standard linear schemes. This must be used in conjuction with
   Adams–Bashforth time stepping. Diffusive and parameterized fluxes are
   always calculated here.

-  :filelink:`GAD\_ADVECTION <pkg/generic_advdiff/gad_advection.F>` calculates just the advective fluxes using the
   non-linear schemes and can not be used in conjuction with
   Adams–Bashforth time stepping.


.. tabularcolumns:: |\Y{.4}|L|L|


+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| CPP Flag Name                                 | Default | Description                                                                                                          |
+===============================================+=========+======================================================================================================================+
| :varlink:`COSINEMETH_III`                     | #define | sets the implementation form of :math:`\cos{\varphi}` scaling of bi-harmonic terms for tracer diffusivity            |
|                                               |         | (note, in :filelink:`pkg/generic_advdiff` routines the definition set here overrides whether this is defined in      |
|                                               |         | :filelink:`model/inc/CPP_OPTIONS.h`, where the setting affects viscous term calculations)                            |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`ISOTROPIC_COS_SCALING`              | #undef  | selects isotropic scaling of harmonic and bi-harmonic terms when using the :math:`\cos{\varphi}` scaling             |
|                                               |         | (note, in :filelink:`pkg/generic_advdiff` routines the definition set here overrides whether this is defined in      |
|                                               |         | :filelink:`model/inc/CPP_OPTIONS.h`, where the setting affects viscous term calculations)                            |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`DISABLE_MULTIDIM_ADVECTION`         | #undef  | disables compilation of multi-dim. advection code                                                                    |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`GAD_MULTIDIM_COMPRESSIBLE`          | #undef  | use compressible flow method for multi-dim advection instead of older, less accurate method; note option has         |
|                                               |         | no effect on SOM advection which always uses compressible flow method                                                |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`GAD_ALLOW_TS_SOM_ADV`               | #undef  | enable the use of 2nd-order moment advection scheme (Prather 1986 :cite:`prather:86`)                                |
|                                               |         | for temp. and salinity                                                                                               |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`GAD_SMOLARKIEWICZ_HACK`             | #undef  | enables hack to get rid of negatives caused by Redi, see Smolarkiewicz (1989) :cite:`smolark:89`                     |
|                                               |         | (for ptracers, except temp and salinity)                                                                             |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+



.. _gad_diagnostics:

GAD Diagnostics
+++++++++++++++

::


    ------------------------------------------------------------------------
    <-Name->|Levs|<-parsing code->|<--  Units   -->|<- Tile (max=80c) 
    ------------------------------------------------------------------------
    ADVr_TH | 15 |WM      LR      |degC.m^3/s      |Vertical   Advective Flux of Pot.Temperature
    ADVx_TH | 15 |UU   087MR      |degC.m^3/s      |Zonal      Advective Flux of Pot.Temperature
    ADVy_TH | 15 |VV   086MR      |degC.m^3/s      |Meridional Advective Flux of Pot.Temperature
    DFrE_TH | 15 |WM      LR      |degC.m^3/s      |Vertical Diffusive Flux of Pot.Temperature (Explicit part)
    DIFx_TH | 15 |UU   090MR      |degC.m^3/s      |Zonal      Diffusive Flux of Pot.Temperature
    DIFy_TH | 15 |VV   089MR      |degC.m^3/s      |Meridional Diffusive Flux of Pot.Temperature
    DFrI_TH | 15 |WM      LR      |degC.m^3/s      |Vertical Diffusive Flux of Pot.Temperature (Implicit part)
    ADVr_SLT| 15 |WM      LR      |(g/kg).m^3/s    |Vertical   Advective Flux of Salinity
    ADVx_SLT| 15 |UU   094MR      |(g/kg).m^3/s    |Zonal      Advective Flux of Salinity
    ADVy_SLT| 15 |VV   093MR      |(g/kg).m^3/s    |Meridional Advective Flux of Salinity
    DFrE_SLT| 15 |WM      LR      |(g/kg).m^3/s    |Vertical Diffusive Flux of Salinity    (Explicit part)
    DIFx_SLT| 15 |UU   097MR      |(g/kg).m^3/s    |Zonal      Diffusive Flux of Salinity
    DIFy_SLT| 15 |VV   096MR      |(g/kg).m^3/s    |Meridional Diffusive Flux of Salinity
    DFrI_SLT| 15 |WM      LR      |(g/kg).m^3/s    |Vertical Diffusive Flux of Salinity    (Implicit part)

Experiments and tutorials that use GAD
++++++++++++++++++++++++++++++++++++++

-  Baroclinic gyre experiment, in :filelink:`tutorial\_baroclinic\_gyre <verification/tutorial_baroclinic_gyre>`
   verification directory.

-  Tracer Sensitivity tutorial, in :filelink:`tutorial\_tracer\_adjsens <verification/tutorial_tracer_adjsens>`
   verification directory.


