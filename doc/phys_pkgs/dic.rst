.. _sub_pkg_dic:

DIC Package
-----------

Introduction
~~~~~~~~~~~~

This is one of the biogeochemical packages handled from the pkg gchem.
The main purpose of this package is to consider the cycling of carbon in
the ocean. It also looks at the cycling of phosphorous and potentially
oxygen and iron. There are four standard tracers :math:`DIC`,
:math:`ALK`, :math:`PO4`, :math:`DOP` and also possibly :math:`O2` and
:math:`Fe`. The air-sea exchange of CO\ :math:`_2` and O\ :math:`_2` are
handled as in the OCMIP experiments (reference). The export of
biological matter is computed as a function of available light and
PO\ :math:`_4` (and Fe). This export is remineralized at depth according
to a Martin curve (again, this is the same as in the OCMIP experiments).
There is also a representation of the carbonate flux handled as in the
OCMIP experiments. The air-sea exchange on CO\ :math:`_2` is affected by
temperature, salinity and the pH of the surface waters. The pH is
determined following the method of Follows et al. For more details of
the equations see :numref:`sub_global_oce_biogeo`.

Key subroutines and parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| **INITIALIZATION**
| *DIC\_ABIOTIC.h* contains the common block for the parameters and
  fields needed to calculate the air-sea flux of :math:`CO_2` and
  :math:`O_2`. The fixed parameters are set in *dic\_abiotic\_param*
  which is called from *gchem\_init\_fixed.F*. The parameters needed for
  the biotic part of the calculations are initialized in
  *dic\_biotic\_param* and are stored in *DIC\_BIOTIC.h*. The first
  guess of pH is calculated in *dic\_surfforcing\_init.F*.
|
| **LOADING FIELDS**
| The air-sea exchange of :math:`CO_2` and :math:`O_2` need wind,
  atmospheric pressure (although the current version has this hardwired
  to 1), and sea-ice coverage. The calculation of pH needs silica
  fields. These fields are read in in *dic\_fields\_load.F*. These
  fields are initialized to zero in *dic\_ini\_forcing.F*. The fields
  for interpolating are in common block in *DIC\_LOAD.h*.
|
| **FORCING**
| The tracers are advected-diffused in *ptracers\_integrate.F*. The
  updated tracers are passed to *dic\_biotic\_forcing.F* where the
  effects of the air-sea exchange and biological activity and
  remineralization are calculated and the tracers are updated for a
  second time. Below we discuss the subroutines called from
  *dic\_biotic\_forcing.F*.

| Air-sea exchange of :math:`CO_2` is calculated in *dic\_surfforcing*.
  Air-Sea Exchange of :math:`CO_2` depends on T,S and pH. The
  determination of pH is done in *carbon\_chem.F*. There are three
  subroutines in this file: *carbon\_coeffs* which determines the
  coefficients for the carbon chemistry equations; *calc\_pco2* which
  calculates the pH using a Newton-Raphson method; and
  *calc\_pco2\_approx* which uses the much more efficient method of
  Follows et al. The latter is hard-wired into this package, the former is
  kept here for completeness.

| Biological productivity is determined following Dutkiewicz et al. (2005)
  and is calculated in *bio\_export.F* The light in each latitude band is
  calculate in *insol.F*, unless using one of the flags listed below. The
  formation of hard tissue (carbonate) is linked to the biological
  productivity and has an effect on the alkalinity - the flux of carbonate
  is calculated in *car\_flux.F*, unless using the flag listed below for
  the Friis et al (2006) scheme. The flux of phosphate to depth where it
  instantly remineralized is calculated in *phos\_flux.F*.

| The dilution or concentration of carbon and alkalinity by the addition
  or subtraction of freshwater is important to their surface patterns.
  These “virtual” fluxes can be calculated by the model in several ways.
  The older scheme is done following OCMIP protocols (see more in
  Dutkiewicz et al 2005), in the subroutines *dic\_surfforcing.F* and
  *alk\_surfforcing.F*. To use this you need to set in
  GCHEM\_OPTIONS.h: #define ALLOW\_OLD\_VIRTUALFLUX. But this can also
  be done by the ptracers pkg if this is undefined.
  You will then need to set the concentration of the tracer in rainwater
  and potentially a reference tracer value in data.ptracer
  (PTRACERS_EvPrRn, and PTRACERS_ref respectively).

| Oxygen air-sea exchange is calculated in *o2\_surfforcing.F*.

| Iron chemistry (the amount of free iron) is taken care of in
  *fe\_chem.F*.
|
| **DIAGNOSTICS**
| Averages of air-sea exchanges, biological productivity, carbonate
  activity and pH are calculated. These are initialized to zero in
  *dic\_biotic\_init* and are stored in common block in *DIC\_BIOTIC.h*.
|
| **COMPILE TIME FLAGS**
| These are set in GCHEM\_OPTIONS.h:
| DIC\_BIOTIC: needs to be set for dic to work properly (should be fixed
  sometime).
| ALLOW\_O2: include the tracer oxygen.
| ALLOW\_FE: include the tracer iron. Note you will need an iron dust
  file set in data.gchem in this case.
| MINFE: limit the iron, assuming precpitation of any excess free iron.
| CAR\_DISS: use the calcium carbonate scheme of Friis et al 2006.
| ALLOW\_OLD\_VIRTUALFLUX: use the old OCMIP style virtual flux for
  alklinity adn carbon (rather than doing it through pkg/ptracers).
| READ\_PAR: read the light (photosynthetically available radiation)
  from a file set in data.gchem.
| USE\_QSW: use the numbers from QSW to be the PAR. Note that a file for
  Qsw must be supplied in data, or Qsw must be supplied by an
  atmospheric model.
| If the above two flags are not set, the model calculates PAR in
  insol.F as a function of latitude and year day.
| USE\_QSW\_UNDERICE: if using a sea ice model, or if the Qsw variable
  has the seaice fraction already taken into account, this flag must be
  set.
| AD\_SAFE: will use a tanh function instead of a max function - this is
  better if using the adjoint
| DIC\_NO\_NEG: will include some failsafes in case any of the variables
  become negative. (This is advicable). ALLOW\_DIC\_COST: was used for
  calculating cost function (but hasn’t been updated or maintained, so
  not sure if it works still)
|

Do’s and Don’ts
~~~~~~~~~~~~~~~

This package must be run with both ptracers and gchem enabled. It is set
up for at least 4 tracers, but there is the provision for oxygen and
iron. Note the flags above.

Reference Material
~~~~~~~~~~~~~~~~~~

| Dutkiewicz. S., A. Sokolov, J. Scott and P. Stone, 2005: A
  Three-Dimensional Ocean-Seaice-Carbon Cycle Model and its Coupling to
  a Two-Dimensional Atmospheric Model: Uses in Climate Change Studies,
  Report 122, Joint Program of the Science and Policy of Global Change,
  M.I.T., Cambridge, MA.
| Follows, M., T. Ito and S. Dutkiewicz, 2006: On the solution of the carbonate
  chemistry system in ocean biogeochemistry models, *Ocean Modeling*, 12,
  290-301, doi:10.1016/j.ocemod.2005.05.004
| Friis, K., R. Najjar, M.J. Follows, and S. Dutkiewicz, 2006: Possible
  overestimation of shallow-depth calcium carbonate dissolution in the
  ocean, *Global Biogeochemical Cycles*, 20, GB4019,
  doi:10.1029/2006GB002727.

Experiments and tutorials that use dic
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Global Ocean tutorial, in tutorial\_global\_oce\_biogeo verification
   directory, described in :numref:`sub_global_oce_biogeo`.
