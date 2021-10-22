.. _sub_global_oce_biogeo:

Biogeochemistry Simulation
==========================

(in directory: :filelink:`verification/tutorial_global_oce_biogeo/`)

Overview
--------

This model overlays the dissolved inorganic carbon biogeochemistry model
(:filelink:`pkg/dic`) over a 2.8\ :sup:`o` global physical model. The
physical model has 15 levels, and is forced with a climatological annual
cycle of surface wind stresses (Trenberth et al. 1989 :cite:`trenberth:89`,
surface heat and freshwater fluxes (Jiang et al. 1999 :cite:`jiang:99`) with
additional relaxation toward climatological sea surface temperature and
salinity (Levitus and Boyer (1994a,b) :cite:`levitus:94a,levitus:94b`). It uses the Gent and
McWilliams (1990) :cite:`gen-mcw:90` eddy parameterization scheme,
has an implicit free-surface, implicit vertical diffusion and uses the
convective adjustment scheme.

The biogeochemical model considers the coupled cycles of carbon, oxygen,
phosphorus and alkalinity. A simplified parameterization of biological
production is used, limited by the availability of light and phosphate.
A fraction of this productivity enters the dissolved organic pool pool,
which has an e-folding timescale for remineralization of 6 months
(following Yamanaka and Tajika 1997 :cite:`yamanaka:97`). The remaining fraction of this
productivity is instantaneously exported as particulate to depth
(Yamanaka and Tajika 1997 :cite:`yamanaka:97`) where it is remineralized according to the
empirical power law relationship determined by Martin et al. (1987]) :cite:`martin:87`. The
fate of carbon is linked to that of phosphorus by the Redfield ratio.
Carbonate chemistry is explicitly solved (see Follow et al. 2006)
:cite:`follows:06`) and the air-sea exchange of
CO\ :sub:`2` is parameterized with a uniform gas transfer coefficient
following Wanninkhof (1992) :cite:`wannink:92`. Oxygen is also linked to
phosphorus by the Redfield ratio, and oxygen air-sea exchange also
follows Wanninkhof (1992) :cite:`wannink:92`. For more details see
Dutkiewicz et al. (2005) :cite:`dutkiewicz:05`.

The example setup described here shows the physical model after 5900
years of spin-up and the biogeochemistry after 2900 years of spin-up.
The biogeochemistry is at a pre-industrial steady-state (atmospheric
ppmv is kept at 278). Five tracers are resolved: dissolved inorganic
carbon (:math:`DIC`), alkalinity (:math:`ALK`), phosphate (:math:`PO4`),
dissolved organic phosphorus (:math:`DOP`) and dissolved oxygen
(:math:`O2`).

   .. figure:: figs/co2flux.png
       :width: 80%
       :align: center
       :alt: Modeled annual mean air-sea CO2
       :name: tut_biogeochem_co2flux

       Modeled annual mean air-sea CO\ :sub:`2` flux (mol C m\ :sup:`-2` y\ :sup:`-1`) for pre-industrial steady-state. Positive indicates flux of CO\ :sub:`2` from ocean to the atmosphere (out-gassing), contour interval is 1 mol C m\ :sup:`-2` y\ :sup:`-1`.

Equations Solved
----------------

The physical ocean model velocity and diffusivities are used to
redistribute the 5 tracers within the ocean. Additional redistribution
comes from chemical and biological sources and sinks. For any tracer
:math:`A`:

.. math::

   \frac{\partial A}{\partial t}=- \nabla  \cdot (\vec{u^{*}} A)+ \nabla  \cdot
     (\mathbf{K}\nabla A)+S_A \nonumber

where :math:`\vec{u^{*}}` is the transformed Eulerian mean circulation
(which includes Eulerian and eddy-induced advection), :math:`\mathbf{K}`
is the mixing tensor, and :math:`S_A` are the sources and sinks due to
biological and chemical processes.

The sources and sinks are:

.. math::
   \begin{aligned}
   S_{DIC} & =  F_{CO_2} + V_{CO_2} + r_{C:P} S_{PO_4}  + J_{Ca} \\
   S_{ALK} & =  V_{ALK}-r_{N:P} S_{PO_4}  + 2 J_{Ca}  \\
   S_{PO_4}& =  -f_{DOP} J_{prod} - \frac{\partial F_P}{\partial z} + \kappa_{remin} [DOP]\\
   S_{DOP} & =  f_{DOP} J_{prod} -\kappa_{remin} [DOP] \\
   S_{O_2} & = \left\{ \begin{array}{ll}
                  -r_{O:P} S_{PO_4} & \mbox{if $O_2>O_{2crit}$} \\
                   0  & \mbox{if $O_2<O_{2crit}$}
                         \end{array}
                 \right. \end{aligned}

where:

-  :math:`F_{CO_2}` is the flux of CO\ :sup:`2` from the ocean to the
   atmosphere

-  :math:`V_{CO_2}` is “virtual flux” due to changes in :math:`DIC` due
   to the surface freshwater fluxes

-  :math:`r_{C:P}` is Redfield ratio of carbon to phosphorus

-  :math:`J_{Ca}` includes carbon removed from surface due to calcium
   carbonate formation and subsequent cumulation of the downward flux of
   CaCO\ :math:`_3`

-  :math:`V_{ALK}` is “virtual flux” due to changes in alkalinity due to
   the surface freshwater fluxes

-  :math:`r_{N:P}` Redfield ratio is nitrogen to phosphorus

-  :math:`f_{DOP}` is fraction of productivity that remains suspended in
   the water column as dissolved organic phosphorus

-  :math:`J_{prod}` is the net community productivity

-  :math:`\frac{\partial F_P}{\partial z}` is the accumulation of
   remineralized phosphorus with depth

-  :math:`\kappa_{remin}` is rate with which :math:`DOP` remineralizes
   back to :math:`PO_4`

-  :math:`F_{O_2}` is air-sea flux of oxygen

-  :math:`r_{O:P}` is Redfield ratio of oxygen to phosphorus

-  :math:`O_{2crit}` is a critical level below which oxygen consumption
   if halted

These terms (for the first four tracers) are described more in
Dutkiewicz et al. (2005) :cite:`dutkiewicz:05` and by
McKinley et al. (2004) :cite:`mckinley:04` for the terms relating to oxygen.

Code configuration
------------------

The modifications to the code (in
:filelink:`verification/tutorial_global_oce_biogeo/code`) are:

-  :filelink:`code/SIZE.h <verification/tutorial_global_oce_biogeo/code/SIZE.h>`: which dictates the size of the model domain (128x64x15).

-  :filelink:`code/PTRACERS_SIZE.h <verification/tutorial_global_oce_biogeo/code/PTRACERS_SIZE.h>`: which dictates how many tracers to assign how
   many tracers will be used (here, 5).

-  :filelink:`code/DIAGNOSTICS_SIZE.h <verification/tutorial_global_oce_biogeo/code/DIAGNOSTICS_SIZE.h>`: assigns size information for the diagnostics
   package.

-  :filelink:`code/packages.conf <verification/tutorial_global_oce_biogeo/code/packages.conf>`: which dictates which packages will be compiled in
   this version of the model - among the many that are used for the
   physical part of the model, this also includes :filelink:`pkg/ptracers`,  :filelink:`pkg/gchem`,
   and :filelink:`pkg/dic` which allow the biogeochemical part of this setup to
   function.

The input fields needed for this run (in
:filelink:`verification/tutorial_global_oce_biogeo/input`) are:

-  :filelink:`input/data <verification/tutorial_global_oce_biogeo/input/data>`: specifies the main parameters for the experiment. Some
   parameters that may be useful to know: :varlink:`nTimeSteps` number timesteps
   model will run, change to 720 to run for a year :varlink:`taveFreq` frequency
   with which time averages are done, change to 31104000 for annual
   averages.

-  :filelink:`input/data.diagnostics <verification/tutorial_global_oce_biogeo/input/data.diagnostics>`: specifies details of diagnostic pkg output

-  :filelink:`input/data.gchem <verification/tutorial_global_oce_biogeo/input/data.gchem>`: specifies details needed in the
   biogeochemistry model run

-  :filelink:`input/data.gmredi <verification/tutorial_global_oce_biogeo/input/data.gmredi>`: specifies details for the GM parameterization

-  :filelink:`input/data.pkg <verification/tutorial_global_oce_biogeo/input/data.pkg>`: set true or false for various packages to be used

-  :filelink:`input/data.ptracers <verification/tutorial_global_oce_biogeo/input/data.ptracers>`: details of the tracers to be used, including
   makes, diffusivity information and (if needed) initial files. Of
   particular importance is the :varlink:`PTRACERS_numInUse` which states how
   many tracers are used, and :varlink:`PTRACERS_Iter0` which states at which
   timestep the biogeochemistry model tracers were initialized.

-  ``bathy.bin``: bathymetry data file

-  :filelink:`input/eedata <verification/tutorial_global_oce_biogeo/input/eedata>`: This file uses standard default values and does not
   contain customizations for this experiment.

-  ``fice.bin``: ice data file, needed for the biogeochemistry

-  ``lev_monthly_salt.bin``: SSS values which model relaxes toward

-  ``lev_monthly_temp.bin``: SST values which model relaxes toward

-  ``pickup.0005184000.data``: variable and tendency values need to
   restart the physical part of the model

-  ``pickup_cd.0005184000.data``: variable and tendency values need to
   restart the cd pkg

-  ``pickup_ptracers.0005184000.data``: variable and tendency values
   need to restart the the biogeochemistry part of the model

-  ``shi_empmr_year.bin``: freshwater forcing data file

-  ``shi_qnet.bin``: heat flux forcing data file

-  ``sillev1.bin``: silica data file, need for the biogeochemistry

-  ``tren_speed.bin``: wind speed data file, needed for the
   biogeochemistry

-  ``tren_taux.bin``: meridional wind stress data file

-  ``tren_tauy.bin``: zonal wind stress data file

Running the example
-------------------

As the model is set up to run in the verification experiment, it only
runs for 4 timesteps (2 days) and outputs data at the end of this short
run. For a more informative run, you will need to run longer. As set up,
this model starts from a pre-spun up state and initializes physical
fields and the biogeochemical tracers from the pickup files.

Physical data (e.g., S,T, velocities etc) will be output as for any
regular ocean run. The biogeochemical output are:

-  tracer snapshots: look in :filelink:`input/data.ptracers <verification/tutorial_global_oce_biogeo/input/data.ptracers>` to see which
   number matches which type of tracer (e.g., ptracer01 is DIC).

-  tracer time averages

-  specific DIC diagnostics: these are averaged over :varlink:`taveFreq` (set in
   :filelink:`input/data <verification/tutorial_global_oce_biogeo/input/data>`) and are specific to :filelink:`pkg/dic` (currently are only
   available in binary format):

   -  ``DIC_Biotave``: 3-D biological community productivity (mol P
      m\ :sup:`-3` s\ :sup:`-1`)

   -  ``DIC_Cartave``: 3-D tendencies due to calcium carbonate cycle
      (mol C m\ :sup:`-3` s\ :sup:`-1`)

   -  ``DIC_fluxCO2ave``: 2-D air-sea flux of CO\ :sub:`2` (mol C
      m\ :sup:`-2` s\ :sup:`-1`)

   -  ``DIC_pCO2tave``: 2-D partial pressure of CO\ :sub:`2` in
      surface layer

   -  ``DIC_pHtave``: 2-D pH in surface layer

   -  ``DIC_SurOtave``: 2-D tendency due to air-sea flux of
      O\ :sub:`2` (mol O m\ :sup:`-3` s\ :sup:`-1`)

   -  ``DIC_Surtave``: 2-D surface tendency of DIC due to air-sea flux
      and virtual flux (mol C m\ :sup:`-3` s\ :sup:`-1`)
