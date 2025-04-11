.. _chap_state_estimation:

Packages III - Ocean State Estimation
=====================================

This chapter describes packages that have been introduced for ocean
state estimation purposes and in relation with automatic differentiation
(see :ref:`chap_autodiff`). Various examples in this chapter rely on two
model configurations that can be setup as explained in :ref:`sec:exp:llc`

.. _sec:pkg:ecco:

ECCO: model-data comparisons using gridded data sets
----------------------------------------------------

Author: Gael Forget

The functionalities implemented in :filelink:`pkg/ecco` are: (1) output
time-averaged model fields to compare with gridded data sets; (2)
compute normalized model-data distances (i.e., cost functions); (3)
compute averages and transports (i.e., integrals). The former is
achieved as the model runs forwards in time whereas the others occur
after time-integration has completed. Following
:cite:`for-eta:15` the total cost function is formulated
generically as

.. math:: \mathcal{J}(\vec{u}) = \sum_i \alpha_i \left(\vec{d}_i^T R_i^{-1}
   \vec{d}_i\right) + \sum_j \beta_j \vec{u}^T\vec{u}
   :label: Jtotal

.. math::
   \vec{d}_i = \mathcal{P}(\vec{m}_i - \vec{o}_i)
   :label: Jposproc

.. math::
   \vec{m}_i = \mathcal{S}\mathcal{D}\mathcal{M}(\vec{v})
   :label: Jpreproc

.. math::
   \vec{v}   = \mathcal{Q}(\vec{u})
   :label: Upreproc

.. math::
   \vec{u}   = \mathcal{R}(\vec{u}')
   :label: Uprecond

using symbols defined in :numref:`gencost_symbols`. Per
Equation :eq:`Jpreproc` model counterparts
(:math:`\vec{m}_i`) to observational data (:math:`\vec{o}_i`) derive
from adjustable model parameters (:math:`\vec{v}`) through model
dynamics integration (:math:`\mathcal{M}`), diagnostic calculations
(:math:`\mathcal{D}`), and averaging in space and time
(:math:`\mathcal{S}`). Alternatively :math:`\mathcal{S}` stands for
subsampling in space and time in the context of
:numref:`sec:pkg:profiles` (:ref:`sec:pkg:profiles`). Plain
model-data misfits (:math:`\vec{m}_i-\vec{o}_i`) can be penalized
directly in Eq. :eq:`Jtotal` but penalized misfits
(:math:`\vec{d}_i`) more generally derive from
:math:`\vec{m}_i-\vec{o}_i` through the generic :math:`\mathcal{P}`
post-processor (Eq. :eq:`Jposproc`). Eqs. :eq:`Upreproc`-:eq:`Uprecond`
pertain to model control parameter adjustment capabilities described in
:numref:`sec:pkg:ctrl` (:ref:`sec:pkg:ctrl`).

.. table:: Symbol used in formulating generic cost functions.
  :name: gencost_symbols

  +-----------------------------------+-----------------------------------+
  | symbol                            | definition                        |
  +===================================+===================================+
  | :math:`\vec{u}`                   | vector of nondimensional control  |
  |                                   | variables                         |
  +-----------------------------------+-----------------------------------+
  | :math:`\vec{v}`                   | vector of dimensional control     |
  |                                   | variables                         |
  +-----------------------------------+-----------------------------------+
  | :math:`\alpha_i, \beta_j`         | misfit and control cost function  |
  |                                   | multipliers (1 by default)        |
  +-----------------------------------+-----------------------------------+
  | :math:`R_i`                       | data error covariance matrix      |
  |                                   | (:math:`R_i^{-1}` are weights)    |
  +-----------------------------------+-----------------------------------+
  | :math:`\vec{d}_i`                 | a set of model-data differences   |
  +-----------------------------------+-----------------------------------+
  | :math:`\vec{o}_i`                 | observational data vector         |
  +-----------------------------------+-----------------------------------+
  | :math:`\vec{m}_i`                 | model counterpart to              |
  |                                   | :math:`\vec{o}_i`                 |
  +-----------------------------------+-----------------------------------+
  | :math:`\mathcal{P}`               | post-processing operator (e.g., a |
  |                                   | smoother)                         |
  +-----------------------------------+-----------------------------------+
  | :math:`\mathcal{M}`               | forward model dynamics operator   |
  +-----------------------------------+-----------------------------------+
  | :math:`\mathcal{D}`               | diagnostic computation operator   |
  +-----------------------------------+-----------------------------------+
  | :math:`\mathcal{S}`               | averaging/subsampling operator    |
  +-----------------------------------+-----------------------------------+
  | :math:`\mathcal{Q}`               | Pre-processing operator           |
  +-----------------------------------+-----------------------------------+
  | :math:`\mathcal{R}`               | Pre-conditioning operator         |
  +-----------------------------------+-----------------------------------+

.. _costgen:

Generic Cost Function
~~~~~~~~~~~~~~~~~~~~~

The parameters available for configuring generic cost function terms in
``data.ecco`` are given in :numref:`gencost_ecco_params` and
examples of possible specifications are available in:

-  MITgcm_contrib/verification_other/global_oce_cs32/input/data.ecco

-  MITgcm_contrib/verification_other/global_oce_cs32/input_ad.sens/data.ecco

-  MITgcm_contrib/gael/verification/global_oce_llc90/input.ecco_v4/data.ecco

The gridded observation file name is specified by :varlink:`gencost_datafile`.
Observational time series may be provided as on big file or split into yearly
files finishing in ‘\_1992’, ‘\_1993’, etc. The corresponding :math:`\vec{m}_i`
physical variable is specified via the :varlink:`gencost_barfile` root (see
:numref:`gencost_ecco_barfile`).  A file named as specified by
:varlink:`gencost_barfile` gets created where averaged fields are written
progressively as the model steps forward in time. After the final time step
this file is re-read by :filelink:`cost_generic.F <pkg/ecco/cost_generic.F>` to
compute the corresponding cost function term. If :varlink:`gencost_outputlevel`
= 1 and :varlink:`gencost_name`\ =‘foo’ then :filelink:`cost_generic.F
<pkg/ecco/cost_generic.F>` outputs model-data misfit fields (i.e.,
:math:`\vec{d}_i`) to a file named ‘misfit_foo.data’ for offline analysis and
visualization.

In the current implementation, model-data error covariance matrices
:math:`R_i` omit non-diagonal terms. Specifying :math:`R_i` thus boils
down to providing uncertainty fields (:math:`\sigma_i` such that
:math:`R_i=\sigma_i^2`) in a file specified via :varlink:`gencost_errfile`. By
default :math:`\sigma_i` is assumed to be time-invariant but a
:math:`\sigma_i` time series of the same length as the :math:`\vec{o}_i`
time series can be provided using the ``variaweight`` option
(:numref:`gencost_ecco_preproc`). By
default cost functions are quadratic but
:math:`\vec{d}_i^T R_i^{-1} \vec{d}_i` can be replaced with
:math:`R_i^{-1/2} \vec{d}_i` using the ``nosumsq`` option
(:numref:`gencost_ecco_preproc`).

In principle, any averaging frequency should be possible, but only
‘day’, ‘month’, ‘step’, and ‘const’ are implemented for
:varlink:`gencost_avgperiod`. If two different averaging frequencies are needed
for a variable used in multiple cost function terms (e.g., daily and
monthly) then an extension starting with ‘\_’ should be added to
:varlink:`gencost_barfile` (such as ‘\_day’ and ‘\_mon’).  [1]_ If two cost
function terms use the same variable and frequency, however, then using
a common :varlink:`gencost_barfile` saves disk space.

Climatologies of :math:`\vec{m}_i` can be formed from the time series of model
averages in order to compare with climatologies of :math:`\vec{o}_i` by
activating the ‘clim’ option via :varlink:`gencost_preproc` and setting the
corresponding :varlink:`gencost_preproc_i` integer parameter to the number of
records (i.e., a # of months, days, or time steps) per climatological
cycle. The generic post-processor (:math:`\mathcal{P}` in Eq. :eq:`Jposproc`)
also allows model-data misfits to be, for example, smoothed in space by setting
:varlink:`gencost_posproc` to ‘smooth’ and specifying the smoother parameters
via :varlink:`gencost_posproc_c` (name of a smoothing scale file) and
:varlink:`gencost_posproc_i` (an integer specifying the smoother number of time
steps, see :numref:`gencost_ecco_preproc`).  The smoothing scale file can be
be based on the large-scale parameter specified in data.smooth or prepared as
a factor of the model resolution dxC and dyC.  As an example, one can read in
offline the model dxC and dyC and create a characteristic length-scale as
sqrt(dxC^2 + dyC^2), then multiply by a factor of 3 if one wants the smoothed
(large scale) field to be of length-scale 3x that of the model grid spacing.
The smoother number of time steps `gencost_posproc_i` can be the same as that
used in data.smooth.  Other options associated with the computation
of Eq. :eq:`Jtotal` are summarized in :numref:`gencost_ecco_preproc` and
further discussed below. Multiple :varlink:`gencost_preproc` /
:varlink:`gencost_posproc` options may be specified per cost term.

In general the specification of :varlink:`gencost_name` is optional, has no
impact on the end-result, and only serves to distinguish between cost function
terms amongst the model output (STDOUT.0000, STDERR.0000, costfunction000,
misfit*.data). Exceptions listed in :numref:`gencost_ecco_name` however
activate alternative cost function codes (in place of :filelink:`cost_generic.F
<pkg/ecco/cost_generic.F>`) described in :numref:`v4custom`. In this section
and in :numref:`gencost_ecco_barfile` (unlike in other parts of the manual)
‘zonal’ / ‘meridional’ are to be taken literally and these components are
centered (i.e., not at the staggered model velocity points). Preparing gridded
velocity data sets for use in cost functions thus boils down to interpolating
them to XC / YC.

The :varlink:`gencost_kLev_select` option allows the user to select the
vertical level of a 3D model field, thereby creating a 2D field out of that
slice which is used for the cost computation. For example, drifter velocities
correspond to the second depth level of the grid used in ECCOv4, so model
velocities are selected from this depth level to compare to the drifter
observations. The user can specify this in ``data.ecco`` with:
:varlink:`gencost_kLev_select` ``( i ) = 2``, where i is replaced with the
index for that cost function term.

.. table:: Run-time parameters used in formulating generic cost functions and
           defined via `ecco_gencost_nml`` namelist in ``data.ecco``.  All
           parameters are vectors of length ``NGENCOST`` (the # of available
           cost terms) except for ``gencost_proc*`` are arrays of size
           ``NGENPPROC``\ :math:`\times`\ ``NGENCOST`` (10 :math:`\times`\ 20
           by default; can be changed in ``ECCO_SIZE.h`` at compile time). In
           addition, the ``gencost_is3d`` internal parameter is reset to true
           on the fly in all 3D cases in :numref:`gencost_ecco_barfile`.
  :name: gencost_ecco_params

  +---------------------------+-------------------+-----------------------------------+
  | parameter                 | type              | function                          |
  +===========================+===================+===================================+
  | ``gencost_name``          | character(\*)     | Name of cost term                 |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_barfile``       | character(\*)     | File to receive model counterpart |
  |                           |                   | :math:`\vec{m}_i` (See            |
  |                           |                   | :numref:`gencost_ecco_barfile`)   |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_datafile``      | character(\*)     | File containing                   |
  |                           |                   | observational data                |
  |                           |                   | :math:`\vec{o}_i`                 |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_avgperiod``     | character(5)      | Averaging period for              |
  |                           |                   | :math:`\vec{o}_i` and             |
  |                           |                   | :math:`\vec{m}_i`                 |
  |                           |                   | (see text)                        |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_outputlevel``   | integer           | Greater than 0 will               |
  |                           |                   | output misfit fields              |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_errfile``       | character(\*)     | Uncertainty field                 |
  |                           |                   | name (not used in                 |
  |                           |                   | :numref:`intgen`)                 |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_mask``          | character(\*)     | Mask file name root               |
  |                           |                   | (used only in                     |
  |                           |                   | :numref:`intgen`)                 |
  +---------------------------+-------------------+-----------------------------------+
  | ``mult_gencost``          | real              | Multiplier                        |
  |                           |                   | :math:`\alpha_i`                  |
  |                           |                   | (default: 1)                      |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_preproc``       | character(\*)     | Preprocessor names                |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_preproc_c``     | character(\*)     | Preprocessor                      |
  |                           |                   | character arguments               |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_preproc_i``     | integer(\*)       | Preprocessor integer              |
  |                           |                   | arguments                         |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_preproc_r``     | real(\*)          | Preprocessor real                 |
  |                           |                   | arguments                         |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_posproc``       | character(\*)     | Post-processor names              |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_posproc_c``     | character(\*)     | Post-processor                    |
  |                           |                   | character arguments               |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_posproc_i``     | integer(\*)       | Post-processor                    |
  |                           |                   | integer arguments                 |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_posproc_r``     | real(\*)          | Post-processor real               |
  |                           |                   | arguments                         |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_spmin``         | real              | Data less than this               |
  |                           |                   | value will be omitted             |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_spmax``         | real              | Data greater than                 |
  |                           |                   | this value will be                |
  |                           |                   | omitted                           |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_spzero``        | real              | Data points equal to              |
  |                           |                   | this value will be                |
  |                           |                   | omitted                           |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_startdate1``    | integer           | Start date of                     |
  |                           |                   | observations                      |
  |                           |                   | (YYYMMDD)                         |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_startdate2``    | integer           | Start date of                     |
  |                           |                   | observations (HHMMSS)             |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_is3d``          | logical           | Needs to be true for              |
  |                           |                   | 3D fields                         |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_enddate1``      | integer           | Not fully implemented             |
  |                           |                   | (used only in                     |
  |                           |                   | :numref:`v4custom`)               |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_enddate2``      | integer           | Not fully implemented             |
  |                           |                   | (used only in                     |
  |                           |                   | :numref:`v4custom`)               |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_kLev_select``   | integer           | Vertical level of a 3D field to   |
  |                           |                   | create a 2D field for cost        |
  |                           |                   | computation                       |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_useDensityMask``| logical           | Needs to be true if density       |
  |                           |                   | following feature is used         |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_sigmaLow``      | real              | Use to define minimum density     |
  |                           |                   | surface chosen                    |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_sigmaHigh``     | real              | Used to define maximum density    |
  |                           |                   | surface chosen                    |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_refPressure``   | real              | Defines reference pressure used   |
  |                           |                   | in density following feature      |
  +---------------------------+-------------------+-----------------------------------+
  | ``gencost_tanhScale``     | real              | Used in defining density levels   |
  |                           |                   | in density following feature      |
  +---------------------------+-------------------+-----------------------------------+

.. table:: Implemented ``gencost_barfile`` options (as of checkpoint 65z) that
           can be used via :filelink:`cost_generic.F
           <pkg/ecco/cost_generic.F>` (:numref:`costgen`). An extension
           starting with ‘\_’ can be appended at the end of the variable name
           to distinguish between separate cost function terms. Note: the
           ‘m_eta’ formula depends on the ``ATMOSPHERIC_LOADING`` and
           ``ALLOW_PSBAR_STERIC`` compile-time options and
           ‘useRealFreshWaterFlux’ run-time parameter.
  :name: gencost_ecco_barfile

  +-----------------------+-----------------------+-----------------------+
  | variable name         | description           | remarks               |
  +=======================+=======================+=======================+
  | ``m_eta``             | sea surface height    | free surface + ice +  |
  |                       |                       | global steric         |
  |                       |                       | correction            |
  +-----------------------+-----------------------+-----------------------+
  | ``m_sst``             | sea surface           | first level potential |
  |                       | temperature           | temperature           |
  +-----------------------+-----------------------+-----------------------+
  | ``m_sss``             | sea surface salinity  | first level salinity  |
  +-----------------------+-----------------------+-----------------------+
  | ``m_bp``              | bottom pressure       | phiHydLow             |
  +-----------------------+-----------------------+-----------------------+
  | ``m_siarea``          | sea-ice area          | from pkg/seaice       |
  +-----------------------+-----------------------+-----------------------+
  | ``m_siheff``          | sea-ice effective     | from pkg/seaice       |
  |                       | thickness             |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``m_sihsnow``         | snow effective        | from pkg/seaice       |
  |                       | thickness             |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``m_theta``           | potential temperature | three-dimensional     |
  +-----------------------+-----------------------+-----------------------+
  | ``m_salt``            | salinity              | three-dimensional     |
  +-----------------------+-----------------------+-----------------------+
  | ``m_UE``              | zonal velocity        | three-dimensional     |
  +-----------------------+-----------------------+-----------------------+
  | ``m_VN``              | meridional velocity   | three-dimensional     |
  +-----------------------+-----------------------+-----------------------+
  | ``m_ustress``         | zonal wind stress     | from pkg/exf          |
  +-----------------------+-----------------------+-----------------------+
  | ``m_vstress``         | meridional wind       | from pkg/exf          |
  |                       | stress                |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``m_uwind``           | zonal wind            | from pkg/exf          |
  +-----------------------+-----------------------+-----------------------+
  | ``m_vwind``           | meridional wind       | from pkg/exf          |
  +-----------------------+-----------------------+-----------------------+
  | ``m_atemp``           | atmospheric           | from pkg/exf          |
  |                       | temperature           |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``m_aqh``             | atmospheric specific  | from pkg/exf          |
  |                       | humidity              |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``m_precip``          | precipitation         | from pkg/exf          |
  +-----------------------+-----------------------+-----------------------+
  | ``m_swdown``          | downward shortwave    | from pkg/exf          |
  +-----------------------+-----------------------+-----------------------+
  | ``m_lwdown``          | downward longwave     | from pkg/exf          |
  +-----------------------+-----------------------+-----------------------+
  | ``m_wspeed``          | wind speed            | from pkg/exf          |
  +-----------------------+-----------------------+-----------------------+
  | ``m_diffkr``          | vertical/diapycnal    | three-dimensional,    |
  |                       | diffusivity           | constant              |
  +-----------------------+-----------------------+-----------------------+
  | ``m_kapgm``           | GM diffusivity        | three-dimensional,    |
  |                       |                       | constant              |
  +-----------------------+-----------------------+-----------------------+
  | ``m_kapredi``         | isopycnal diffusivity | three-dimensional,    |
  |                       |                       | constant              |
  +-----------------------+-----------------------+-----------------------+
  | ``m_geothermalflux``  | geothermal heat flux  | constant              |
  +-----------------------+-----------------------+-----------------------+
  | ``m_bottomdrag``      | bottom drag           | constant              |
  +-----------------------+-----------------------+-----------------------+

.. table:: ``gencost_preproc`` and ``gencost_posproc`` options
           implemented as of checkpoint 65z. Note: the distinction between
           ``gencost_preproc`` and ``gencost_posproc`` seems unclear and may be
           revisited in the future.
  :name: gencost_ecco_preproc

  +-----------------------+-----------------------+-----------------------+
  | name                  | description           | ``gencost_preproc_i`` |
  |                       |                       | , ``_r``, or ``_c``   |
  +=======================+=======================+=======================+
  | ``gencost_preproc``   |                       |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``clim``              | Use climatological    | integer: no. of       |
  |                       | misfits               | records per           |
  |                       |                       | climatological cycle  |
  +-----------------------+-----------------------+-----------------------+
  | ``mean``              | Use time mean of      | —                     |
  |                       | misfits               |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``anom``              | Use anomalies from    | —                     |
  |                       | time mean             |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``variaweight``       | Use time-varying      | —                     |
  |                       | weight :math:`W_i`    |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``nosumsq``           | Use linear misfits    | —                     |
  +-----------------------+-----------------------+-----------------------+
  | ``factor``            | Multiply              | real: the scaling     |
  |                       | :math:`\vec{m}_i` by  | factor                |
  |                       | a scaling factor      |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``offset``            | subtract mean misfit  | —                     |
  +-----------------------+-----------------------+-----------------------+
  | ``mindepth``          | mask (ignore) misfit  | real: minimum water   |
  |                       | above minimum depth   | depth (:math:`< 0`)   |
  +-----------------------+-----------------------+-----------------------+
  | ``gencost_posproc``   |                       |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``smooth``            | Smooth misfits        | character: smoothing  |
  |                       |                       | scale file            |
  +-----------------------+-----------------------+-----------------------+
  |                       |                       | integer: smoother #   |
  |                       |                       | of time steps         |
  +-----------------------+-----------------------+-----------------------+

.. _intgen:

Generic Integral Function
~~~~~~~~~~~~~~~~~~~~~~~~~

The functionality described in this section is operated by
:filelink:`cost_gencost_boxmean.F <pkg/ecco/cost_gencost_boxmean.F>`. It is
primarily aimed at obtaining a mechanistic understanding of a chosen physical
variable via adjoint sensitivity computations (see :ref:`chap_autodiff`) as
done for example in :cite:`maro-eta:99,heim-eta:11,fuku-etal:14`. Thus the
quadratic term in Eq. :eq:`Jtotal` (:math:`\vec{d}_i^T R_i^{-1} \vec{d}_i`) is
by default replaced with a :math:`d_i` scalar [2]_ that derives from model
fields through a generic integral formula (Eq. :eq:`Jpreproc`). The
specification of :varlink:`gencost_barfile` again selects the physical variable
type. Current valid options to use :filelink:`cost_gencost_boxmean.F
<pkg/ecco/cost_gencost_boxmean.F>` are reported in
:numref:`genint_ecco_barfile`. A suffix starting with ``‘_’`` can again be
appended to :varlink:`gencost_barfile`.

The integral formula is defined by masks provided via binary files which
names are specified via :varlink:`gencost_mask`. There are two cases: (1) if
``gencost_mask = ‘foo_mask’`` and :varlink:`gencost_barfile` is of the
‘m_boxmean\*’ type then the model will search for horizontal, vertical,
and temporal mask files named ``foo_maskC``, ``foo_maskK``, and
``foo_maskT``; (2) if instead :varlink:`gencost_barfile` is of the
‘m_horflux\_’ type then the model will search for ``foo_maskW``,
``foo_maskS``, ``foo_maskK``, and ``foo_maskT``.

The ‘C’ mask or the ‘W’ / ‘S’ masks are expected to be two-dimensional
fields. The ‘K’ and ‘T’ masks (both optional; all 1 by default) are
expected to be one-dimensional vectors. The ‘K’ vector length should
match Nr. The ‘T’ vector length should match the # of records that the
specification of :varlink:`gencost_avgperiod` implies but there is no
restriction on its values. In case #1 (‘m_boxmean\*’) the ‘C’ and ‘K’
masks should consists of +1 and 0 values and a volume average will be
computed accordingly. In case #2 (‘m_horflux\*’) the ‘W’, ‘S’, and ‘K’
masks should consists of +1, -1, and 0 values and an integrated
horizontal transport (or overturn) will be computed accordingly.

.. note::

   By default, ``m_boxmean`` cost functions are sums of masked, weighted 
   variables, where the weight of each cell is the current cell volume 
   divided by the total masked *initial* volume (sum of masked 
   ``eccoVol_0``). Note that cell volumes vary in time in the case of 
   a non-linear free surface (see :numref:`nonlinear-freesurface` 
   (:ref:`nonlinear-freesurface`)). To obtain a true weighted mean in 
   the case of a non-linear free surface, please define 
   ``ECCO_VARIABLE_AREAVOLGLOB`` in ``ECCO_OPTIONS.h``, which instead 
   uses the total masked current volume to weight contributions.

In order to define a control volume using both a depth range and a
density range, use a ‘K’ mask and also set
:varlink:`gencost_useDensityMask` ``=.TRUE.``. When the density range
feature is active, the control volume is defined at each timestep by
the bounds set in the ‘K’ mask and also by the density range specified
by the parameters :varlink:`gencost_sigmaLow` (the minimum density to
be included in the control volume) and :varlink:`gencost_sigmaHigh`
(the maximum density to be included in the control volume). As a default
:varlink:`gencost_refPressure` should be set to 0, but other values can
be used (e.g. 1000 dbar, 2000 dbar).

.. table:: Implemented :varlink:`gencost_barfile` options (as of checkpoint
           67x) that can be used via :filelink:`cost_gencost_boxmean.F
           <pkg/ecco/cost_gencost_boxmean.F>` (:numref:`intgen`).
  :name: genint_ecco_barfile

  +---------------------+----------------------------------+------------------+
  | variable name       | description                      | remarks          |
  +=====================+==================================+==================+
  | ``m_boxmean_theta`` | mean of theta over box           | specify box      |
  +---------------------+----------------------------------+------------------+
  | ``m_boxmean_salt``  | mean of salt over box            | specify box      |
  +---------------------+----------------------------------+------------------+
  | ``m_boxmean_eta``   | mean of SSH over box             | specify box      |
  +---------------------+----------------------------------+------------------+
  | ``m_boxmean_shifwf``| total shelfice freshwater flux   | specify box      |
  |                     | over box                         |                  |
  +---------------------+----------------------------------+------------------+
  | ``m_boxmean_shihf`` | total shelfice heat flux over box| specify box      |
  +---------------------+----------------------------------+------------------+
  | ``m_boxmean_vol``   | total volume over box            | specify box      |
  +---------------------+----------------------------------+------------------+
  | ``m_horflux_vol``   | volume transport through section | specify transect |
  +---------------------+----------------------------------+------------------+

.. _v4custom:

Custom Cost Functions
~~~~~~~~~~~~~~~~~~~~~

This section (very much a work in progress...) pertains to the special cases of
:filelink:`cost_gencost_bpv4.F <pkg/ecco/cost_gencost_bpv4.F>`,
:filelink:`cost_gencost_seaicev4.F <pkg/ecco/cost_gencost_seaicev4.F>`,
:filelink:`cost_gencost_sshv4.F <pkg/ecco/cost_gencost_sshv4.F>`,
:filelink:`cost_gencost_sstv4.F <pkg/ecco/cost_gencost_sstv4.F>`,
:filelink:`cost_gencost_transp.F <pkg/ecco/>`, and
:filelink:`cost_gencost_moc.F <pkg/ecco/cost_gencost_moc.>`.  The
:filelink:`cost_gencost_transp.F <pkg/ecco/cost_gencost_transp.F>` function can
be used to compute a transport of volume, heat, or salt through a specified
section (non quadratic cost function). To this end one sets
``gencost_name = ‘transp*’``, where ``*`` is an optional suffix starting
with ``‘_’``, and set :varlink:`gencost_barfile` to one of ``m_trVol``,
``m_trHeat``, and ``m_trSalt``.

The :filelink:`cost_gencost_moc.F <pkg/ecco/cost_gencost_moc.F>` function is
similar to transport function, but is intended to compute the meridional
overturning streamfunction maximum based on the volumetric transport integrated
from the floor to surface, as in Smith and Heimbach (2019) :cite:`smith:19`.
Therefore, this function is intended to work with :varlink:`gencost_barfile`
``= m_trVol``, and note that the first 3 characters of :varlink:`gencost_name`
must be ``moc``, as depicted in :numref:`gencost_ecco_name`.  Users can specify
a latitude band to compute the MOC with appropriately defined West ('W') and
South ('S') masks as described in :numref:`intgen`.  As an example see
parameter group (3) in `this data.ecco file
<https://github.com/MITgcm/verification_other/blob/master/global_oce_cs32/input_ad.sens/data.ecco>`_
.

Note: the functionality in :filelink:`cost_gencost_transp.F
<pkg/ecco/cost_gencost_transp.F>` is not regularly tested.  Users interested in
computing volumetric transports through a section are recommended to use the
``m_horflux_vol`` capabilities described above as it is regularly tested. Users
interested in computing heat and salt transport should note the following about
``m_trHeat`` and ``m_trSalt``:

    1. The associated advection scheme with transports may be inconsistent with
       the model unless ``ENUM_CENTERED_2ND`` is implemented
    2. Bolus velocities are not included
    3. Diffusion components are not included


.. table:: Pre-defined :varlink:`gencost_name` special cases (as of checkpoint
           65z; :numref:`v4custom`).
  :name: gencost_ecco_name

  +-----------------------+-----------------------+-----------------------+
  | name                  | description           | remarks               |
  +=======================+=======================+=======================+
  | ``sshv4-mdt``         | sea surface height    | mean dynamic          |
  |                       |                       | topography (SSH -     |
  |                       |                       | geod)                 |
  +-----------------------+-----------------------+-----------------------+
  | ``sshv4-tp``          | sea surface height    | Along-Track           |
  |                       |                       | Topex/Jason SLA       |
  |                       |                       | (level 3)             |
  +-----------------------+-----------------------+-----------------------+
  | ``sshv4-ers``         | sea surface height    | Along-Track           |
  |                       |                       | ERS/Envisat SLA       |
  |                       |                       | (level 3)             |
  +-----------------------+-----------------------+-----------------------+
  | ``sshv4-gfo``         | sea surface height    | Along-Track GFO class |
  |                       |                       | SLA (level 3)         |
  +-----------------------+-----------------------+-----------------------+
  | ``sshv4-lsc``         | sea surface height    | Large-Scale SLA (from |
  |                       |                       | the above)            |
  +-----------------------+-----------------------+-----------------------+
  | ``sshv4-gmsl``        | sea surface height    | Global-Mean SLA (from |
  |                       |                       | the above)            |
  +-----------------------+-----------------------+-----------------------+
  | ``bpv4-grace``        | bottom pressure       | GRACE maps (level 4)  |
  +-----------------------+-----------------------+-----------------------+
  | ``sstv4-amsre``       | sea surface           | Along-Swath SST       |
  |                       | temperature           | (level 3)             |
  +-----------------------+-----------------------+-----------------------+
  | ``sstv4-amsre-lsc``   | sea surface           | Large-Scale SST (from |
  |                       | temperature           | the above)            |
  +-----------------------+-----------------------+-----------------------+
  | ``si4-cons``          | sea ice concentration | needs sea-ice adjoint |
  |                       |                       | (level 4)             |
  +-----------------------+-----------------------+-----------------------+
  | ``si4-deconc``        | model sea ice         | proxy penalty (from   |
  |                       | deficiency            | the above)            |
  +-----------------------+-----------------------+-----------------------+
  | ``si4-exconc``        | model sea ice excess  | proxy penalty (from   |
  |                       |                       | the above)            |
  +-----------------------+-----------------------+-----------------------+
  | ``transp_trVol``      | volume transport      | specify masks         |
  |                       |                       | (:numref:`intgen`)    |
  +-----------------------+-----------------------+-----------------------+
  | ``transp_trHeat``     | heat transport        | specify masks         |
  |                       |                       | (:numref:`intgen`)    |
  +-----------------------+-----------------------+-----------------------+
  | ``transp_trSalt``     | salt transport        | specify masks         |
  |                       |                       | (:numref:`intgen`)    |
  +-----------------------+-----------------------+-----------------------+
  | ``moc_trVol``         | meridional ovt.       | specify masks         |
  |                       | streamfn. maximum     | (:numref:`intgen`)    |
  +-----------------------+-----------------------+-----------------------+

Key Routines
~~~~~~~~~~~~

TBA ...
:filelink:`ecco_readparms.F <pkg/ecco/ecco_readparms.F>`,
:filelink:`ecco_check.F <pkg/ecco/ecco_check.F>`,
:filelink:`ecco_summary.F <pkg/ecco/ecco_summary.F>`,
:filelink:`cost_generic.F <pkg/ecco/cost_generic.F>`,
:filelink:`cost_gencost_boxmean.F <pkg/ecco/cost_gencost_boxmean.F>`,
:filelink:`ecco_toolbox.F <pkg/ecco/ecco_toolbox.F>`,
:filelink:`ecco_phys.F <pkg/ecco/ecco_phys.F>`,
:filelink:`cost_gencost_customize.F <pkg/ecco/cost_gencost_customize.F>`,
:filelink:`cost_averagesfields.F <pkg/ecco/cost_averagesfields.F>`, ...

Compile Options
~~~~~~~~~~~~~~~

TBA ...
:varlink:`ALLOW_GENCOST_CONTRIBUTION`,
:varlink:`ALLOW_GENCOST3D`,
:varlink:`ALLOW_PSBAR_STERIC`,
:varlink:`ALLOW_SHALLOW_ALTIMETRY`,
:varlink:`ALLOW_HIGHLAT_ALTIMETRY`,
:varlink:`ALLOW_PROFILES_CONTRIBUTION`,
:varlink:`ALLOW_ECCO_OLD_FC_PRINT`,
...

packages required for some functionalities:
:filelink:`smooth <pkg/smooth>`,
:filelink:`profiles <pkg/profiles>`,
:filelink:`ctrl <pkg/ctrl>`

.. _sec:pkg:profiles:

PROFILES: model-data comparisons at observed locations
------------------------------------------------------

Author: Gael Forget

The purpose of :filelink:`pkg/profiles <pkg/profiles>` is to allow sampling of MITgcm runs
according to a chosen pathway (after a ship or a drifter, along
altimeter tracks, etc.), typically leading to easy model-data
comparisons. Given input files that contain positions and dates,
pkg/profiles will interpolate the model trajectory at the observed
location. In particular, pkg/profiles can be used to do model-data
comparison online and formulate a least-squares problem (ECCO
application).

The pkg/profiles namelist is called data.profiles. In the example below,
it includes two input netcdf file names (ARGOifremer_r8.nc
and XBT_v5.nc) that should be linked to the run directory
and *cost function* multipliers that only matter in the
context of automatic differentiation (see :ref:`chap_autodiff`). The
first index is a file number and the second index (in mult\* only) is a
variable number. By convention, the variable number is an integer
ranging 1 to 6: temperature, salinity, zonal velocity, meridional
velocity, sea surface height anomaly, and passive tracer.

.. more updates are needed below

The netcdf input file structure is illustrated in the case of XBT_v5.nc
To create such files, one can use the MITprof matlab toolbox obtained
from https://github.com/gaelforget/MITprof .
At run time, each file is scanned to determine which
variables are included; these will be interpolated. The (final) output
file structure is similar but with interpolated model values in prof_T
etc., and it contains model mask variables (e.g. prof_Tmask). The very
model output consists of one binary (or netcdf) file per processor.
The final netcdf output is to be built from those using
netcdf_ecco_recompose.m (offline).

When the k2 option is used (e.g. for cubed sphere runs), the input file
is to be completed with interpolation grid points and coefficients
computed offline using netcdf_ecco_GenericgridMain.m. Typically, you
would first provide the standard namelist and files. After detecting
that interpolation information is missing, the model will generate
special grid files (profilesXCincl1PointOverlap\* etc.) and then stop.
You then want to run netcdf_ecco_GenericgridMain.m using the special
grid files. *This operation could eventually be inlined.*

``Example: data.profiles``

::

    #
    # \*****************\*
    # PROFILES cost function
    # \*****************\*
    &PROFILES_NML
    #
    profilesfiles(1)= ’ARGOifremer_r8’,
    mult_profiles(1,1) = 1.,
    mult_profiles(1,2) = 1.,
    profilesfiles(2)= ’XBT_v5’,
    mult_profiles(2,1) = 1.,
    #
    /

``Example: XBT_v5.nc``

::

    netcdf XBT_v5 {
    dimensions:
    iPROF = 278026 ;
    iDEPTH = 55 ;
    lTXT = 30 ;
    variables:
    double depth(iDEPTH) ;
    depth:units = "meters" ;
    double prof_YYYYMMDD(iPROF) ;
    prof_YYYYMMDD:missing_value = -9999. ;
    prof_YYYYMMDD:long_name = "year (4 digits), month (2 digits), day (2 digits)" ;
    double prof_HHMMSS(iPROF) ;
    prof_HHMMSS:missing_value = -9999. ;
    prof_HHMMSS:long_name = "hour (2 digits), minute (2 digits), second (2 digits)" ;
    double prof_lon(iPROF) ;
    prof_lon:units = "(degree E)" ;
    prof_lon:missing_value = -9999. ;
    double prof_lat(iPROF) ;
    prof_lat:units = "(degree N)" ;
    prof_lat:missing_value = -9999. ;
    char prof_descr(iPROF, lTXT) ;
    prof_descr:long_name = "profile description" ;
    double prof_T(iPROF, iDEPTH) ;
    prof_T:long_name = "potential temperature" ;
    prof_T:units = "degree Celsius" ;
    prof_T:missing_value = -9999. ;
    double prof_Tweight(iPROF, iDEPTH) ;
    prof_Tweight:long_name = "weights" ;
    prof_Tweight:units = "(degree Celsius)-2" ;
    prof_Tweight:missing_value = -9999. ;
    }

.. _sec:pkg:ObsFit:

OBSFIT: grid-independent model-data comparisons 
------------------------------------------------------

Author: Ariane Verdy

Introduction
~~~~~~~~~~~~

:filelink:`pkg/obsfit <pkg/obsfit>` is a versatile package used for grid-independent model-data comparisons
including cost function calculations.

Given an observational dataset, OBSFIT samples the model during the run at the time and location of observations,
calculates the cost (sum of weighted misfits), and produces a model-equivalent output file that is directly
comparable to an input file containing observational data.
It is designed to accommodate datasets that are sparse, irregular, or non-local.
OBSFIT performs grid-independent model-data comparisons, meaning that observations do not have to be on the same
grid as the model or constrained to a fixed set of depth levels. This increases the efficiency of data
assimilation for many datasets and allows compatibility with multi-grid state estimation. OBSFIT offers the
capability of assimilating tomography data and high-resolution altimetry data (e.g., `SWOT <https://swot.jpl.nasa.gov>`_).

Description
~~~~~~~~~~~

The code is evolved from :filelink:`pkg/profiles <pkg/profiles>` and shares much of its general structure.
In addition to relaxing pkg/profile's constraint on vertical levels, OBSFIT can handle:

-  spatial averages of multiple sample locations;

-  time averages (or a cumulative integral) of multiple sampled points;

-  observations that are combinations of multiple variables.

.. _obsfit_space: 

Observations vs. Samples
^^^^^^^^^^^^^^^^^^^^^^^^
One feature of this package is that it allows measured observations to be averages in both space and/or time
(or alternatively, integrated values in space and/or time via optional parameter choices, see :ref:`below <obsfit_time>`).
Samples, defined as instantaneous model data values
at specific locations (which may or may not coincide with model gridpoints), are aggregated and
interpolated for comparison with observations. Hence, in OBSFIT, sampled points are referred to as *samples*
and the averaged/integrated values as *observations*. 
For example, consider observations of integrated sound speed along the acoustic ray path:
in such case, one specifies multiple locations at which to sample the model, as we require model
data at multiple locations to calculate the model-equivalent of a single observation. 
Samples locations are used during the model run to extract model data (and save it to file). Then, sampled
values are combined at the end of the run to calculate the model-equivalent value. Observational values
are only used at the end of the model run to calculate the cost, i.e., weighted misfits with model-equivalents. 

As noted, each observation can be comprised of a number of samples (NP). Each of those NP samples is assigned a relative
weight in the average/integral; by default all samples are weighed equally. (Note that the our definition of weights is
different from the uncertainty-related weights in :filelink:`pkg/profiles`.)  In many applications however, NP=1
and "samples" and "observations" are effectively the same.

.. _obsfit_type: 

Sample types
^^^^^^^^^^^^

Each OBSFIT sample is assigned a type corresponding to the model variable that will be sampled.
There are currently five types of variables implemented in the code: potential temperature, salinity,
zonal velocity, meridional velocity, and sea surface height. Observations can be made of samples
of different types; for example, one could compute the along-shore current speed (a combination of
zonal and meridional velocities) or the water spiciness (a comnbination of temperature and salinity). 

For sea surface height (SSH) observations, OBSFIT samples the model variable :varlink:`etaN`. Inputs should thus
be the total SSH, not SSH anomalies. Because of arbitrary reference values for the dynamic topography,
the mean offset between modeled and observed SSH is removed when the cost is calculated. 

.. _obsfit_time: 

Observation duration
^^^^^^^^^^^^^^^^^^^^

Each OBSFIT observation is assigned a start time and a duration.
Observations with a specified positive duration are averaged in time, whereas a negative duration
is used to indicate time integration, and instantaneous observations have duration=0
(if no duration is provided, duration=0 is assumed). During each
model time step which falls within the observation specified window, the model is sampled 
at each specified sample location.
In other words, all samples inherit the time and duration from the corresponding observation.
If observation time does not align exactly with model time steps, samples are taken from model data
at the beginning of the time step in which the observation time falls.
Sampled values are saved in tiled files. For non-zero specified duration, accumulated values
are saved in the tiled files and the average is calculated at the end of the model run.


Interpolation
^^^^^^^^^^^^^

Sampling is done by interpolating model values from grid points surrounding the
sample location (up to 8 surrounding grid points are used). For a cartesian or spherical polar grid,
interpolation factors (not to be confused with weights!) are calculated from the input longitude, latitude, and depth.
For a curvilinear grid (LLC, etc), interpolation factors are specified in the input file.

Cost Functions
^^^^^^^^^^^^^^


OBSFIT configuration and compiling
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OBSFIT can be turned on or off at compile time
(see :numref:`building_code`)

- using the ``packages.conf`` file by adding ``obsfit`` to it

- or using :filelink:`genmake2 <tools/genmake2>` adding ``-enable=obsfit`` or
  ``-disable=obsfit`` switches

- *required packages and CPP options*: :filelink:`pkg/cal` must be enabled to use OBSFIT. No other packages or CPP options are required.

If needed, edit :filelink:`OBSFIT_SIZE.h <pkg/obsfit/OBSFIT_SIZE.h>` to change the maximum number of input files,
total number of observations, number of samples per tile, or number of samples per observation. For maximum efficiency,
set those to the smallest values possible for your input datasets. 

Run-time requirements
~~~~~~~~~~~~~~~~~~~~~

Pre-processing: How to make OBSFIT input files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Users must provide at least one OBSFIT input file, in netCDF format, with observed values and sampling locations.
Typically, different datasets will be processed as separate files. In OBSFIT input files, all fields will be
vectors -- with the exception of position and integration factors for the generic grid case.

They must include the following fields:

- obs_val (observed value)

- obs_uncert (uncertainty on the observed value)

- obs_YYYYMMDD (observation start time [year,month,day])

- obs_HHMMSS (observation start time [hour,min,sec])

- sample_type (variable type, [integer; see table below])

- sample_lon (longitude)

- sample_lat (latitude)

- sample_depth (depth)

  
The following fields are optional:

- obs_delt (observation duration [default=0; negative for time integration])

- obs_np (number of samples in the observation [default=1])

- sample_weight (weighting factor [default=1/obs_np])

See make_obsfit_example.m for a matlab example

In the simplest case, the number of samples per observation is 1; then obs_np = 1 (by default), sample_weight = 1 (by default), and sample_{type/lon/lat/depth} give the variable type/longitude/latitude/depth of the observation. If there are {N} observations, each field listed above is a vector of size {1xN}.

If observations are spatial averages or integrals, one must specify the number of samples that make each observation, as well as their relative weight. If there are {N} observations, obs* fields are vectors of size {1xN} and sample* fields are vectors of size :math:`\sum_N` (obs_np). Note that the number of samples can be different for each observation.

The observation start time is given in two separate fields, obs_YYYYMMDD and obs_HHMMSS. They are numeric values with 8 and 6 digits, respectively. The first 4 digits of obs_YYYYMMDD correspond to the year, the next 2 to the month, and the last 2 to the day; a similar notation is used for obs_HHMMSS.


Sample types currently supported:

==============          ======
Variable                Type
==============          ======
:math:`\theta`          1 
:math:`S`               2
:math:`u`               3
:math:`v`               4
SSH                     5
==============          ======


Enabling the package
^^^^^^^^^^^^^^^^^^^^

:filelink:`/pkg/obsfit` package is switched on/off at run-time by
setting :varlink:`useOBSFIT` ``= .TRUE.,`` in ``data.pkg``.

General flags and parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:numref:`tab_phys_pkg_obsfit_runtimeparms` lists run-time parameters.

.. tabularcolumns:: |\Y{.275}|\Y{.20}|\Y{.525}|

.. table:: Run-time parameters and default values
  :class: longtable
  :name: tab_phys_pkg_obsfit_runtimeparms

  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  |   Name                             |      Default value           |   Description                                                           |
  +====================================+==============================+=========================================================================+
  | :varlink:`obsfitDir`               |     ' '                      | subdirectory name containing OBSFIT data files                          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`obsfitFiles`             |     ' '                      | OBSFIT data filenames (``.nc`` automatically appended)                  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`mult_obsfit`             |     1.0                      | multiplier factor for observation in total cost function calculation    |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`obsfit_facmod`           |     1.0                      |                                                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`obsfitDoNcOutput`        |     FALSE                    | boolean to generate tiled output file in netCDF format                  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`obsfitDoGenGrid`         | TRUE (spherical grid, FALSE) | not sure we need this namelist parm                                     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+


File ``data.obsfit`` must be present in the run folder. Here is an example:

::

    # *********************
    # OBSFIT cost function
    # *********************
    &OBSFIT_NML
    obsfitDir      = 'OBSFIT',
    obsfitFiles(1) = 'swot_L3_may2023',
    mult_obsfit(1) = 1.0,
    obsfitFiles(2) = 'moorings_calval_may2023',
    mult_obsfit(2) = 0.0,
    &


In this example there are two input files: swot_L3_may2023.nc and moorings_calval_may2023.nc
(note that the suffix .nc should not be included). They have multiplier factors that will
multiply their respective cost in the total cost calculation. For example, the first dataset
will be counted with a factor=1, and the second dataset will not influence the total cost 
ince its multiplier is 0. Output files will be written in a folder called "OBSFIT" that
will be created if it doesn't already exist. 


Post-processing
^^^^^^^^^^^^^^^

For each input file, two new files are created. One, named <original_filename>.equi.nc,
contains model-equivalent values for direct comparison with observation data.
The other, named <original_filename>.misfits.nc, contains model-observations misfits.
"equi.nc" output files include two variables, mod_val and mod_mask. They are in the same format as the input files,
thus obs_val and mod_val are directly comparable. The mask indicates missing model-equivalent values.

A simple way to plot the observed values and model-equivalent values in matlab could be:

::

    scatter(sample_lon, sample_lat, 30, obs_val);
    scatter(sample_lon, sample_lat, 30, mod_val);

Experiments and tutorials that use OBSFIT
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



.. _sec:pkg:ctrl:

CTRL: Model Parameter Adjustment Capability
-------------------------------------------

Author: Gael Forget, An T. Nguyen, Martin Losch

.. _gen_ctrl:

Generic Control Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Package :filelink:`ctrl <pkg/ctrl>` provides an interface to defining the
control variables for an optimization. After defining CPP-flags
:varlink:`ALLOW_GENTIM2D_CONTROL`, :varlink:`ALLOW_GENARR2D_CONTROL`,
:varlink:`ALLOW_GENARR3D_CONTROL` in :filelink:`CTRL_OPTIONS.h
<pkg/ctrl/CTRL_OPTIONS.h>`, the parameters available for configuring generic
cost terms in ``data.ctrl`` are given in :numref:`gencost_ctrl_params`.  The
control variables are stored as fields on the model grid in files
``$ctrlvar.$iternumber.data/meta``, and corresponding gradients in
``ad$ctrlvar.$iternumber.data/meta``, where ``$ctrl`` is defined in
``data.ctrl`` (see :numref:`gencost_ctrl_files` for possible options) and
``$iternumber`` is the 10-digit iteration number of the optimization. Further,
:filelink:`ctrl <pkg/ctrl>` maps the gradient fields to a vector that can be
handed over to an optimization routine (see :numref:`sectionoptim`) and maps
the resulting new control vector to the model grid unless CPP-flag
:varlink:`EXCLUDE_CTRL_PACK` is defined in :filelink:`CTRL_OPTIONS.h
<pkg/ctrl/CTRL_OPTIONS.h>`.

.. _gen_ctrl_param:

Run-time Parameters
^^^^^^^^^^^^^^^^^^^

.. table:: Parameters in namelist group :varlink:`ctrl_nml_genarr` in ``data.ctrl``.  The
           ``*`` can be replaced by ``arr2d``, ``arr3d``, or ``tim2d`` for
           time-invariant two and three dimensional controls and time-varying
           2D controls, respectively. Parameters for ``genarr2d``,
           ``genarr3d``, and ``gentime2d`` are arrays of length
           :varlink:`maxCtrlArr2D`, :varlink:`maxCtrlArr3D`, and
           :varlink:`maxCtrlTim2D`, respectively, with one entry per term in
           the cost function.
  :name: gencost_ctrl_params

  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  |   Name                             |      Default value           |   Description                                                           |
  +====================================+==============================+=========================================================================+
  | ``xx_gen*_file``                   |    :kbd:`' '`                | control fllename: prefix from :numref:`gencost_ctrl_files` + suffix     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | ``xx_gen*_weight``                 |    :kbd:`' '`                | filename for weights in the form of :math:`\sigma_{\vec{u}_j}^{-2}`     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | ``xx_gen*_bounds``                 |    0.0, 0.0, 0.0, 0.0, 0.0   | apply bounds                                                            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | ``xx_gen*_preproc``                |    :kbd:`' '`                | control preprocessor (see :numref:`gencost_ctrl_preproc`)               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | ``xx_gen*_preproc_c``              |    :kbd:`' '`                | preprocessor character arguments (see :numref:`genarr_preproc_c`)       |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | ``xx_gen*_preproc_i``              |    0                         | preprocessor integer arguments                                          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | ``xx_gen*_preproc_r``              |    0.0                       | preprocessor real arguments                                             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | ``gen*Precond``                    |    1.0                       | preconditioning factor                                                  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | ``mult_gen*``                      |    1.0                       | cost function multiplier :math:`\beta_j`                                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`xx_gentim2d_period`      |    0.0                       | frequency of adjustments (s)                                            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`xx_gentim2d_startdate1`  |  :varlink:`startdate_1`      | adjustment start date 1 yyyymmdd (default from :filelink:`pkg/cal`;     |
  |                                    |                              | see :numref:`sub_phys_pkg_cal`)                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`xx_gentim2d_startdate2`  |  :varlink:`startdate_2`      | adjustment start date 2 hhmmss (default from :filelink:`pkg/cal`;       |
  |                                    |                              | see :numref:`sub_phys_pkg_cal`)                                         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`xx_gentim2d_cumsum`      |   FALSE                      | accumulate control adjustments                                          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`xx_gentim2d_glosum`      |   FALSE                      | global sum of adjustment (note: output is still 2D)                     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+

.. _gen_ctrl_fields:

Generic Control Fields
^^^^^^^^^^^^^^^^^^^^^^

.. table:: Generic control prefixes implemented as of checkpoint 67x.
  :name: gencost_ctrl_files

  +--------------------+-----------------------+--------------------------------+
  |                    | name                  | description                    |
  +====================+=======================+================================+
  | 2D, time-invariant | ``genarr2d``          |                                |
  | controls           |                       |                                |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_etan``           | initial sea surface height     |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_bottomdrag``     | bottom drag                    |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_geothermal``     | geothermal heat flux           |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_shicoefft``      | package :ref:`shelfice         |
  |                    |                       | <sub_phys_pkg_shelfice>`       |
  |                    |                       | thermal transfer coefficient   |
  |                    |                       | (see :numref:`shi_ctrl`)       |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_shicoeffs``      | package :ref:`shelfice         |
  |                    |                       | <sub_phys_pkg_shelfice>`       |
  |                    |                       | salinity transfer              |
  |                    |                       | coefficient                    |
  |                    |                       | (see :numref:`shi_ctrl`)       |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_shicdrag``       | package :ref:`shelfice         |
  |                    |                       | <sub_phys_pkg_shelfice>`       |
  |                    |                       | drag coefficient               |
  |                    |                       | (see :numref:`shi_ctrl`)       |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_depth``          | bottom topography;             |
  |                    |                       | requires #define               |
  |                    |                       | :varlink:`ALLOW_DEPTH_CONTROL` |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_siheff``         | package :ref:`seaice           |
  |                    |                       | <sub_phys_pkg_seaice>`         |
  |                    |                       | initial sea ice thickness      |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_siarea``         | package :ref:`seaice           |
  |                    |                       | <sub_phys_pkg_seaice>`         |
  |                    |                       | initial sea ice area           |
  +--------------------+-----------------------+--------------------------------+
  +--------------------+-----------------------+--------------------------------+
  | 3D, time-invariant | ``genarr3d``          |                                |
  | controls           |                       |                                |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_theta``          | initial potential temperature  |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_salt``           | initial salinity               |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_uvel``           | initial zonal velocity         |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_vvel``           | initial meridional velocity    |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_kapgm``          | package :ref:`gmredi           |
  |                    |                       | <sub_phys_pkg_gmredi>`         |
  |                    |                       | GM thickness diffusivity       |
  |                    |                       | (see :numref:`GM_bolus_desc`)  |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_kapredi``        | package :ref:`gmredi           |
  |                    |                       | <sub_phys_pkg_gmredi>`         |
  |                    |                       | isopycnal ("Redi") diffusivity |
  |                    |                       | (see :numref:`GM_redi_desc`)   |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_diffkr``         | diapycnal diffusivity          |
  +--------------------+-----------------------+--------------------------------+
  +--------------------+-----------------------+--------------------------------+
  | 2D, time-varying   | ``gentim2D``          |                                |
  | controls           |                       |                                |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_atemp``          | atmospheric temperature        |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_aqh``            | atmospheric specific humidity  |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_swdown``         | downward shortwave             |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_lwdown``         | downward longwave              |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_precip``         | precipitation                  |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_runoff``         | river runoff                   |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_uwind``          | zonal wind                     |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_vwind``          | meridional wind                |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_tauu``           | zonal wind stress              |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_tauv``           | meridional wind stres          |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_gen_precip``     | globally averaged              |
  |                    |                       | precipitation                  |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_hflux``          | net heat flux                  |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_sflux``          | net salt (EmPmR) flux          |
  +--------------------+-----------------------+--------------------------------+
  |                    | ``xx_shifwflx``       | shelfice melt rate             |
  +--------------------+-----------------------+--------------------------------+

.. _gen_ctrl_proc:

Generic Control Processing Options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. table:: ``xx_gen????d_preproc`` options implemented as of checkpoint
           67x. Notes: :math:`^a`: If ``noscaling`` is false, the control
           adjustment is scaled by one on the square root of the weight before
           being added to the base control variable; if ``noscaling`` is true,
           the control is multiplied by the weight in the cost function itself.
  :name: gencost_ctrl_preproc

  +-----------------------+-----------------------+-----------------------+
  | name                  | description           | arguments             |
  +=======================+=======================+=======================+
  | ``WC01``              | correlation modeling  | integer: operator     |
  |                       |                       | type (default: 1)     |
  +-----------------------+-----------------------+-----------------------+
  | ``smooth``            | smoothing without     | integer: operator     |
  |                       | normalization         | type (default: 1)     |
  +-----------------------+-----------------------+-----------------------+
  | ``docycle``           | average period        | integer: cycle length |
  |                       | replication           |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``replicate``         | alias for ``docycle`` |(units of              |
  |                       |                       |``xx_gentim2d_period``)|
  +-----------------------+-----------------------+-----------------------+
  | ``rmcycle``           | periodic average      | integer: cycle length |
  |                       | subtraction           |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``variaweight``       | use time-varying      | —                     |
  |                       | weight                |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``noscaling``         | do not scale with     | —                     |
  | :math:`^{a}`          | ``xx_gen*_weight``    |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``documul``           | sets                  | —                     |
  |                       | ``xx_gentim2d_cumsum``|                       |
  |                       |                       |                       |
  +-----------------------+-----------------------+-----------------------+
  | ``doglomean``         | sets                  | —                     |
  |                       | ``xx_gentim2d_glosum``|                       |
  |                       |                       |                       |
  +-----------------------+-----------------------+-----------------------+


.. table:: ``xx_gen????d_preproc_c`` options implemented as of checkpoint
           67x.
  :name: genarr_preproc_c

  +-----------------------+-----------------------+-----------------------+
  | name                  | description           | arguments             |
  +=======================+=======================+=======================+
  |``log10ctrl``          | Control adjustments to| See                   |
  |                       | log10 of              | :numref:`log_ctrl`    |
  |                       | 2D or 3D array        |                       |
  |                       | (not available for    |                       |
  |                       | ``xx_gentim2d``).     |                       |
  +-----------------------+-----------------------+-----------------------+

The control problem is non-dimensional by default, as reflected in the
omission of weights in control penalties [(:math:`\vec{u}_j^T\vec{u}_j`
in :eq:`Jtotal`]. Non-dimensional controls
(:math:`\vec{u}_j`) are scaled to physical units (:math:`\vec{v}_j`)
through multiplication by the respective uncertainty fields
(:math:`\sigma_{\vec{u}_j}`), as part of the generic preprocessor
:math:`\mathcal{Q}` in :eq:`Upreproc`. Besides the
scaling of :math:`\vec{u}_j` to physical units, the preprocessor
:math:`\mathcal{Q}` can include, for example, spatial correlation
modeling (using an implementation of Weaver and Coutier, 2001
:cite:`weaver:01`) by
setting ``xx_gen*_preproc = ’WC01’``. Alternatively, setting
``xx_gen*_preproc = ’smooth’`` activates the smoothing part of ``WC01``,
but omits the normalization. Additionally, bounds for the controls can
be specified by setting ``xx_gen*_bounds``. In forward mode, adjustments
to the :math:`i^\text{th}` control are clipped so that they remain
between ``xx_gen*_bounds(i,1)`` and ``xx_gen*_bounds(i,4)``. The bounds
have no effect in adjoint mode unless ``xx_gen*_bounds(i,j)`` <
``xx_gen*_bounds(i,j+1)`` for :math:`j = 1, 3`. When this is the case,
the bounds will “emulate a local minimum” as follows in
:filelink:`pkg/ctrl/adctrl_bound.F`. On the minimum end,
when ``xx_gen*(i)`` < ``xx_gen*_bounds(i,2)`` and the gradient
``adxx_gen*(i)`` > 0.0, i.e., the derivative suggests that a
further decrease of ``xx_gen*(i)`` will decrease the cost, an adjustment
is enforced to reverse the sign of the gradient ``adxx_gen*(i)`` to be
negative such that any further decrease in ``xx_gen*(i)`` toward its minimum
bound ``xx_gen*_bounds(i,1)`` will be penalized.  The opposite is enforced
at the maximum end when ``xx_gen*(i)`` > ``xx_gen*_bounds(i,3)``
and ``adxx_gen*(i)`` < 0.0 such that the sign of the gradient
``adxx_gen*(i)`` will be reversed to positive to penalize any further
increase in ``xx_gen*(i)`` toward its maximum bound ``xx_gen*_bounds(i,4)``.

For the case of time-varying controls, the frequency is specified by
:varlink:`xx_gentim2d_period`. The generic control package interprets special
values of :varlink:`xx_gentim2d_period` in the same way as the ``exf`` package:
a value of :math:`-12` implies cycling monthly fields while a value of
:math:`0` means that the field is steady. Time varying weights can be
provided by specifying the preprocessor ``variaweight``, in which case
the :varlink:`xx_gentim2d_weight` file must contain as many records as the
control parameter time series itself (approximately the run length
divided by :varlink:`xx_gentim2d_period`).

The parameter ``mult_gen*`` sets the multiplier for the corresponding
cost function penalty [:math:`\beta_j` in :eq:`Jtotal`;
:math:`\beta_j = 1` by default). The preconditioner, :math:`\cal{R}`,
does not directly appear in the estimation problem, but only serves to
push the optimization process in a certain direction in control space;
this operator is specified by ``gen*Precond`` (:math:`=1` by default).

Note that control parameters exist for each individual near surface atmospheric
state variable, as well as the net heat and salt (EmPmR) fluxes.  The user must
be mindful of control parameter combinations that make sense according to their
specific setup, e.g., with the :ref:`EXF package <ssub_phys_pkg_exf_config>`.

.. _gen_ctrl_rec:

Generic Control Record Access
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
For each control variable ``$ctrlvar``, three pairs of ``.data`` files (and their
corresponding ``.meta``) are required or produced per adjoint run:

::

   1a   $ctrlvar.effective.$iternumber.data
   1b ad$ctrlvar.effective.$iternumber.data

   2a   $ctrlvar.tmp.$iternumber.data
   2b ad$ctrlvar.tmp.$iternumber.data

   3a   $ctrlvar.$iternumber.data
   3b ad$ctrlvar.$iternumber.data

Pair 1a,b are the physical fields with physical units. Pair 2a,b are temporary
files storing a repeat cycle for use during calculations when
:varlink:`docycle` and :varlink:`rmcycle` are active. Pair 3a,b have units or
no units depending on the setting of :varlink:`noscaling`, which controls
scaling/unscaling by the corresponding ``xx_gen*_weight`` (see
:numref:`gencost_ctrl_preproc`).

In an adjoint run with the 2-D time-dependent controls (CPP-flag
:varlink:`ALLOW_GENTIM2D_CONTROL` defined), three variables
:varlink:`startrec`, :varlink:`endrec`, and :varlink:`diffrec` =
:varlink:`endrec` - :varlink:`startrec` + 1 will be
initialized as a function of the startdate (:varlink:`startdate_1`,
:varlink:`startdate_2`) in ``data.cal``, the control variables startdates
(:varlink:`xx_gentim2d_startdate1`, :varlink:`xx_gentim2d_startdate2`) in
``data.ctrl``, and the pickup time :varlink:`nIter0` in
:filelink:`packages_init_fixed.F <model/src/packages_init_fixed.F>` (which
calls :filelink:`ctrl_init.F <pkg/ctrl/ctrl_init.F>`,
:filelink:`ctrl_init_rec.F <pkg/ctrl/ctrl_init_rec.F>`). These three variables
are subsequently used to determine the record length of the three pairs (1--3)
of the above files, in the order as follows:

- First the ``ad$ctrlvar.[effective,tmp,].$iternumber`` files (1b,2b,3b) above
  are initialized with zeros in
  :filelink:`packages_init_fixed.F <model/src/packages_init_fixed.F>`-->
  :filelink:`ctrl_init.F <pkg/ctrl/ctrl_init.F>`-->
  :filelink:`ctrl_init_ctrlvar.F <pkg/ctrl/ctrl_init_ctrlvar.F>`
  (with :varlink:`yadprefix` = ``'ad'``); 1b and 2b have size :varlink:`diffrec`
  and 3b has size :varlink:`endrec`.

.. parsed-literal ::

  Flow of :filelink:`pkg/ctrl` when the adjoint is running (below, for $iternumber=0000000001):

  Note: :filelink:`the_model_main.F <model/src/the_model_main.F>` calls :filelink:`the_main_loop.F <model/src/the_main_loop.F>`, but once the code is generated from TAF,
  the preprocessed form the_model_main.f calls either mdthe_main_loop or adthe_main_loop

  :filelink:`the_model_main <model/src/the_model_main.F>`
  \|-:filelink:`initialise_fixed <model/src/initialise_fixed.F>`
    \|-:filelink:`ini_parms <model/src/ini_parms.F>`
    \|-:filelink:`packages_boot <model/src/packages_boot.F>`, :filelink:`packages_readparms <model/src/packages_readparms.F>`
    \|-:filelink:`set_parms <model/src/set_parms.F>`, :filelink:`ini_model_io <model/src/ini_model_io.F>`, :filelink:`ini_grid <model/src/ini_grid.F>`, :filelink:`load_ref_files <model/src/load_ref_files.F>`, :filelink:`ini_eos <model/src/ini_eos.F>`, :filelink:`set_ref_state <model/src/set_ref_state.F>`,
      :filelink:`set_grid_factors <model/src/set_grid_factors.F>`, :filelink:`ini_depths <model/src/ini_depths.F>`, :filelink:`ini_masks_etc <model/src/ini_masks_etc.F>`

    \|-:filelink:`packages_init_fixed <model/src/packages_init_fixed.F>`
      \|-:filelink:`cal_init_fixed <pkg/cal/cal_init_fixed.F>`, :filelink:`diagnostics_init_early <pkg/diagnostics/diagnostics_init_early.F>`, :filelink:`diagnostics_main_init <pkg/diagnostics/diagnostics_main_init.F>`, :filelink:`gad_init_fixed <pkg/generic_advdiff/gad_init_fixed.F>`,
        :filelink:`mom_init_fixed <pkg/mom_common/mom_init_fixed.F>`, :filelink:`obcs_init_fixed <pkg/obcs/obcs_init_fixed.F>`, :filelink:`exf_init_fixed <pkg/exf/exf_init_fixed.F>`, :filelink:`kpp_init_fixed <pkg/kpp/kpp_init_fixed.F>`, :filelink:`gmredi_init_fixed <pkg/gmredi/gmredi_init_fixed.F>`,
        :filelink:`seaice_cost_init_fixed <pkg/seaice/seaice_cost_init_fixed.F>`, :filelink:`smooth_init_fixed <pkg/smooth/smooth_init_fixed.F>`, :filelink:`ecco_cost_init_fixed <pkg/ecco/ecco_cost_init_fixed.F>`,
        :filelink:`profiles_init_fixed <pkg/profiles/profiles_init_fixed.F>`, :filelink:`seaice_init_fixed <pkg/seaice/seaice_init_fixed.F>`, :filelink:`salt_plume_init_fixed <pkg/salt_plume/salt_plume_init_fixed.F>`

      \|-:filelink:`ctrl_init <pkg/ctrl/ctrl_init.F>`
        \|-:filelink:`active_write_xyz <pkg/autodiff/active_file.F>`\ ('wunit')
        \|-:filelink:`ctrl_init_ctrlvar <pkg/ctrl/ctrl_init_ctrlvar.F>`\ (genarr2d, genarr3d)

        \|-:filelink:`ctrl_init_rec <pkg/ctrl/ctrl_init_rec.F>`\ (gentim2d_startdate, diffrec, startrec, endrec)
        \|-:filelink:`ctrl_init_ctrlvar <pkg/ctrl/ctrl_init_ctrlvar.F>`\ (xx_atemp.effective.0000000001, 'c','xy')
          \|-:filelink:`ctrl_set_fname <pkg/ctrl/ctrl_set_fname.F>`\ (xx_fname,fname)
                            **--> fname(1:3)=[,ad,hn]xx_atemp.effective.0000000001**
          \|-:filelink:`ctrl_set_globfld_xy <pkg/ctrl/ctrl_set_globfld_xy.F>`\ (fname(2)) (with yadprefix='ad')
            \|-:filelink:`mds_write_field <pkg/mdsio/mdsio_write_field.F>`\ (adxx_atemp.effective.0000000001)  **<- size diffrec**
        \|-:filelink:`ctrl_init_ctrlvar <pkg/ctrl/ctrl_init_ctrlvar.F>`\ (xx_atemp.tmp.0000000001)
          \|-:filelink:`ctrl_set_fname <pkg/ctrl/ctrl_set_fname.F>`\(xx_fname,fname)
                            **--> fname(1:3)=[,ad,hn]xx_atemp.tmp.0000000001**
          \|-:filelink:`ctrl_set_globfld_xy <pkg/ctrl/ctrl_set_globfld_xy.F>`\ (fname(2)) (with yadprefix='ad')
            \|-:filelink:`mds_write_field <pkg/mdsio/mdsio_write_field.F>`\ (adxx_atemp.tmp.0000000001)        **<- size diffrec**
        \|-:filelink:`ctrl_init_ctrlvar <pkg/ctrl/ctrl_init_ctrlvar.F>`\ (xx_atemp.0000000001)
          \|-:filelink:`ctrl_set_fname <pkg/ctrl/ctrl_set_fname.F>`\(xx_fname,fname)
                            **--> fname(1:3)=[,ad,hn]xx_atemp.0000000001**
          \|-:filelink:`ctrl_set_globfld_xy <pkg/ctrl/ctrl_set_globfld_xy.F>`\ (fname(2)) (with yadprefix='ad')
            \|-:filelink:`mds_write_field <pkg/mdsio/mdsio_write_field.F>`\ (adxx_atemp.0000000001)            **<- size endrec**

- Second, within ``initiase_variamd.f`` (see below), records
  :varlink:`startrec` to :varlink:`endrec` of file 3a
  ``$ctrvar.$iternumber.data`` are read in :filelink:`ctrl_map_ini_gentim2d.F
  <pkg/ctrl/ctrl_map_ini_gentim2d.F>`, processed if scaling or smoothing, etc.,
  need to be applied, and then written to (1a,2a)
  ``$ctrlvar.{effective,tmp}.data`` of size :varlink:`diffrec`.  Note these
  routines contain a ``md`` or ``ad`` suffix and are produced by TAF, e.g.,
  ``s/r ctrl_map_ini_gentim2dmd`` (found in TAF-generated file
  ``ctrl_map_ini_gentim2d_ad.f``) called from ``s/r initialize_variamd`` (found
  in TAF-generated file ``initialize_varia_ad.f``), which in turn is called
  from ``s/r adthe_main_loop`` (found in TAF-generated file
  ``the_main_loop_ad.f``); alternatively, all of these routines are found the
  concatenated file ``ad_taf_output.f``.

.. parsed-literal ::

  \|-adthe_main_loop  **only available in the_main_loop_ad.f, called from the_model_main.f**
    \|-adopen (many tapes, ocean variables, atmos, obcs, etc)  **initialize tapelev grid, etc.**

    \|-initialise_variamd
      \|-packages_init_variablesmd
        \|-:filelink:`diagnostics_init_varia <pkg/diagnostics/diagnostics_init_varia.F>`, :filelink:`kpp_init_varia <pkg/kpp/kpp_init_varia.F>`, :filelink:`exf_init_varia <pkg/exf/exf_init_varia.F>`  **store salt,theta**
        \|-:filelink:`profiles_init_varia <pkg/profiles/profiles_init_varia.F>`, :filelink:`ecco_init_varia <pkg/ecco/ecco_init_varia.F>`, :filelink:`obcs_init_variables <pkg/obcs/obcs_init_variables.F>`  **some done after ctrl**
        \|-ctrl_init_variablesmd
          \|-:filelink:`ctrl_map_ini_genarr <pkg/ctrl/ctrl_map_ini_genarr.F>`
            \|-:filelink:`ctrl_map_genarr2d <pkg/ctrl/ctrl_map_genarr.F>`  **e.g., set etan,siheff ctrl**
            \|-:filelink:`ctrl_map_genarr3d <pkg/ctrl/ctrl_map_genarr.F>`  **e.g., set logdiffkr ctrl**
          \|-ctrl_map_ini_gentim2dmd
            \|-:filelink:`ctrl_init_rec <pkg/ctrl/ctrl_init_rec.F>`\ (xx_atemp)
	             **example here for atemp: [startrec,endrec,diffrec]=[24,37,14]**
            \|-:filelink:`active_read_xy <pkg/autodiff/active_file.F>`\ (fnamegenIn,lrec)
	             **read in xx_atemp.0000000001.data from 24->37**
            \|-:filelink:`active_write_xy <pkg/autodiff/active_file.F>`\ (fnamegenOut,irec)
	             **write out to xx_atemp.effective.0000000001.data from 1->14**
            \|-:filelink:`active_read_xy <pkg/autodiff/active_file.F>`\ (fnamegenOut,irec)
	             **read in xx_atemp.effective.0000000001.data 1->14, do some math**
            \|-:filelink:`active_write_xy <pkg/autodiff/active_file.F>`\ (fnamegenTmp,irec)
	             **write out to xx_atemp.tmp.0000000001.data 1->14**
            do irec=1,diffrec
            \|-:filelink:`active_read_xy <pkg/autodiff/active_file.F>`\ (fnamegenOut,irec)
            \|-:filelink:`mds_read_field <pkg/mdsio/mdsio_read_field.F>`\ (xx_gentim2d_weight,jrec)
	             **if variaweight, jrec=lrec, else jrec=1**
            \|-:filelink:`smooth_correl2d <pkg/smooth/smooth_correl2d.F>`  **or smooth2d**
            \|-xx_gen/sqrt(wgentim2d)  **if doscaling**
            \|-exch_xy_rl
            \|-:filelink:`active_write_xy <pkg/autodiff/active_file.F>`\ (fnamegenOut,irec)
	             **write out to xx_atemp.effective.0000000001.data (smooth/scaled)**
            enddo

The difference in length of records for 3[a,b] compared to 1[a,b] and 2[a,b] is
due to the fact that we need to access records :varlink:`startrec` thru
:varlink:`endrec` in 3a, i.e., file 3a needs a total of at least
:varlink:`endrec` records; file 3b is automatically generated to provide access
to :varlink:`endrec` thru :varlink:`startrec` (i.e., in reverse order). File
3b, in particular, is where adjoint sensitivity will be accumulated backward
and written; note the model would thus crash if its last record were
:varlink:`diffrec` rather than :varlink:`endrec`.  For pairs 1[a,b] and 2[a,b],
because they are generated *after* we have already accessed the correct records
:varlink:`startrec` to :varlink:`endrec` in 3a, we simply create and write out
these records in the shorter file size :varlink:`diffrec`.  After their file
size initializations, the control adjustment field with physical unit from file
1a is passed on to :filelink:`pkg/exf` for surface forcing application.

Note, that :varlink:`xx_gentim2d_startdate` can be used to control how many
records the different :varlink:`xx_gentim2d` files
contain. :numref:`xx_var_sketch` illustrates a few examples.

  .. figure:: figs/ctrl_var_sketch.*
    :width: 100%
    :align: center
    :alt: xx_var_sketch
    :name: xx_var_sketch

    Sketch illustrating which parts of the timeline are covered by which
    :varlink:`xx_gentim2d` files.

.. _shi_ctrl:

Shelfice Control Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The available iceshelf control parameters depend on the form of transfer
coefficient used in the simulation.

The adjustments ``xx_shicoefft`` and ``xx_shicoeffs`` are available when the
velocity **independent** form of transfer coefficients is used, by setting
``#undef`` :varlink:`SHI_ALLOW_GAMMAFRICT`
in :filelink:`SHELFICE_OPTIONS.h <pkg/shelfice/SHELFICE_OPTIONS.h>` at
compile time (see :numref:`tab_phys_pkg_shelfice_compileparms`) and
:varlink:`SHELFICEuseGammaFrict` ``=.FALSE.`` in ``data.shelfice`` (see
:numref:`tab_phys_pkg_shelfice_runtimeparms`).  These parameters provide
adjustments to :math:`\gamma_T` and/or :math:`\gamma_S` directly.  If only one
of either is used, the value of the other is set based on the control
adjustments used together with :varlink:`SHELFICEsaltToHeatRatio`, which can be
set in ``data.shelfice``.  See :ref:`tab_phys_pkg_shelfice_runtimeparms` for
the default.

The adjustment ``xx_shicdrag`` is available in the velocity **dependent** form
of the ice-ocean transfer coefficients, which is specified by ``#define``
:varlink:`SHI_ALLOW_GAMMAFRICT` and :varlink:`SHELFICEuseGammaFrict`
``=.TRUE.`` at compile time and run time respectively.  This parameter provides
adjustments to the drag coefficient at the ice ocean boundary, but by default
only adjusts the drag coefficient used to compute the thermal and freshwater
fluxes, neglecting the momentum contributions.  To allow the contribution
directly to momentum fluxes, specify ``xx_genarr2d_preproc_c(*,iarr) = 'mom'``
in ``data.ctrl``.

.. _log_ctrl:

Logarithmic Control Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As indicated in :numref:`genarr_preproc_c`, the base-10 logarithm of a
control field can be adjusted by specifying the character option
``genarr*d_preproc_c(k2,iarr) = 'log10ctrl'``, with ``k2`` and ``iarr``
as appropriate, and ``*d`` denoting that ``2d`` or ``3d`` are available.
As a concrete example, if the control parameter is updating ``fld2d``,
then the field will be set as follows:

.. code-block:: fortran

	fld2d(i,j,bi,bj) = 10**( log10InitVal + xx_genarr2d(i,j,bi,bj,iarr) )

where ``log10InitVal`` is a scalar with a default value of 0, but can be changed
by setting ``gencost_preproc_r(k2,iarr)``. This is useful in the case where
``doInitXX=.TRUE.``.
Concretely, if we had an initial guess for ``fld2d = 10^-4`` then one could set
the following in ``data.ctrl``:

::

	xx_genarr2d_file(1) = 'xx_fld2d'
	xx_genarr2d_weight(1) = 'nonzero_weights.data'
	xx_genarr2d_preproc_c(1,1) = 'log10ctrl'
	xx_genarr2d_preproc_r(1,1) = -4. ,

Note that the ``log10ctrl`` option can only be used when a weight file
is provided, and finally that this log-option cannot be used with
``xx_gen*_preproc(k2,iarr) = 'noscaling',``.


.. _sec:pkg:smooth:

SMOOTH: Smoothing And Covariance Model
--------------------------------------

Author: Gael Forget

TO BE CONTINUED...

.. _sectionoptim:

The line search optimisation algorithm
--------------------------------------

Author: Patrick Heimbach

General features
~~~~~~~~~~~~~~~~

The line search algorithm is based on a quasi-Newton variable storage
method which was implemented by :cite:`gil-lem:89`.

TO BE CONTINUED...

The online vs. offline version
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  | **Online version**
   | Every call to *simul* refers to an execution of the forward and
     adjoint model. Several iterations of optimization may thus be
     performed within a single run of the main program (lsopt_top). The
     following cases may occur:

   -  cold start only (no optimization)

   -  cold start, followed by one or several iterations of optimization

   -  warm start from previous cold start with one or several iterations

   -  warm start from previous warm start with one or several iterations

-  | **Offline version**
   | Every call to simul refers to a read procedure which reads the
     result of a forward and adjoint run Therefore, only one call to
     simul is allowed, itmax = 0, for cold start itmax = 1, for warm
     start Also, at the end, **x(i+1)** needs to be computed and saved
     to be available for the offline model and adjoint run

In order to achieve minimum difference between the online and offline
code **xdiff(i+1)** is stored to file at the end of an (offline)
iteration, but recomputed identically at the beginning of the next
iteration.

Number of iterations vs. number of simulations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| - itmax: controls the max. number of iterations
| - nfunc: controls the max. number of simulations within one iteration

Summary
^^^^^^^

|  
| From one iteration to the next the descent direction changes. Within
  one iteration more than one forward and adjoint run may be performed.
  The updated control used as input for these simulations uses the same
  descent direction, but different step sizes.

Description
^^^^^^^^^^^

|  
| From one iteration to the next the descent direction dd changes using
  the result for the adjoint vector gg of the previous iteration. In
  lsline the updated control

  .. math::

     \tt
     xdiff(i,1) = xx(i-1) + tact(i-1,1)*dd(i-1)

  serves as input for a forward and adjoint model run yielding a new
  gg(i,1). In general, the new solution passes the 1st and 2nd Wolfe
  tests so xdiff(i,1) represents the solution sought:

  .. math:: {\tt xx(i) = xdiff(i,1)}

  If one of the two tests fails, an inter- or extrapolation is invoked
  to determine a new step size tact(i-1,2). If more than one function
  call is permitted, the new step size is used together with the "old"
  descent direction dd(i-1) (i.e. dd is not updated using the new
  gg(i)), to compute a new

  .. math:: {\tt xdiff(i,2) = xx(i-1) + tact(i-1,2)*dd(i-1)}

  that serves as input in a new forward and adjoint run, yielding
  gg(i,2). If now, both Wolfe tests are successful, the updated solution
  is given by

  .. math::

     \tt
     xx(i) = xdiff(i,2) = xx(i-1) + tact(i-1,2)*dd(i-1)

In order to save memory both the fields dd and xdiff have a double
usage.

-  |  
   | - in *lsopt_top*: used as x(i) - x(i-1) for Hessian update
   | - in *lsline*: intermediate result for control update x = x +
     tact*dd

-  |  
   | - in *lsopt_top, lsline*: descent vector, dd = -gg and hessupd
   | - in *dgscale*: intermediate result to compute new preconditioner

The parameter file lsopt.par
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  **NUPDATE** max. no. of update pairs (gg(i)-gg(i-1), xx(i)-xx(i-1))
   to be stored in OPWARMD to estimate Hessian [pair of current iter. is
   stored in (2*jmax+2, 2*jmax+3) jmax must be > 0 to access these
   entries] Presently NUPDATE must be > 0 (i.e. iteration without
   reference to previous iterations through OPWARMD has not been tested)

-  **EPSX** relative precision on xx bellow which xx should not be
   improved

-  **EPSG** relative precision on gg below which optimization is
   considered successful

-  **IPRINT** controls verbose (>=1) or non-verbose output

-  **NUMITER** max. number of iterations of optimisation; NUMTER = 0:
   cold start only, no optimization

-  **ITER_NUM** index of new restart file to be created (not necessarily
   = NUMITER!)

-  **NFUNC** max. no. of simulations per iteration (must be > 0); is
   used if step size tact is inter-/extrapolated; in this case, if NFUNC
   > 1, a new simulation is performed with same gradient but "improved"
   step size

-  **FMIN** first guess cost function value (only used as long as first
   iteration not completed, i.e. for jmax <= 0)

OPWARMI, OPWARMD files
^^^^^^^^^^^^^^^^^^^^^^

Two files retain values of previous iterations which are used in latest
iteration to update Hessian:

-  **OPWARMI**: contains index settings and scalar variables

   +-------------+-------------------------------------------------------+
   | n = nn      | no. of control variables                              |
   +-------------+-------------------------------------------------------+
   | fc = ff     | cost value of last iteration                          |
   +-------------+-------------------------------------------------------+
   | isize       | no. of bytes per record in OPWARMD                    |
   +-------------+-------------------------------------------------------+
   | m = nupdate | max. no. of updates for Hessian                       |
   +-------------+-------------------------------------------------------+
   | jmin, jmax  | pointer indices for OPWARMD file (cf. below)          |
   +-------------+-------------------------------------------------------+
   | gnorm0      | norm of first (cold start) gradient gg                |
   +-------------+-------------------------------------------------------+
   | iabsiter    | total number of iterations with respect to cold start |
   +-------------+-------------------------------------------------------+

-  **OPWARMD**: contains vectors (control and gradient)

   +-----------------------+-----------------------+-----------------------+
   | entry                 | name                  | description           |
   +=======================+=======================+=======================+
   | 1                     | xx(i)                 | control vector of     |
   |                       |                       | latest iteration      |
   +-----------------------+-----------------------+-----------------------+
   | 2                     | gg(i)                 | gradient of latest    |
   |                       |                       | iteration             |
   +-----------------------+-----------------------+-----------------------+
   | 3                     | xdiff(i),diag         | preconditioning       |
   |                       |                       | vector; (1,...,1) for |
   |                       |                       | cold start            |
   +-----------------------+-----------------------+-----------------------+
   | 2*jmax+2              | gold=g(i)-g(i-1)      | for last update       |
   |                       |                       | (jmax)                |
   +-----------------------+-----------------------+-----------------------+
   | 2*jmax+3              | xdiff=tact*d=xx(i)-xx | for last update       |
   |                       | (i-1)                 | (jmax)                |
   +-----------------------+-----------------------+-----------------------+

::


    Example 1: jmin = 1, jmax = 3, mupd = 5

      1   2   3   |   4   5     6   7     8   9     empty     empty
    |___|___|___| | |___|___| |___|___| |___|___| |___|___| |___|___|
          0       |     1         2         3

    Example 2: jmin = 3, jmax = 7, mupd = 5   ---> jmax = 2

      1   2   3   |
    |___|___|___| | |___|___| |___|___| |___|___| |___|___| |___|___|
                  |     6         7         3         4         5

Error handling
^^^^^^^^^^^^^^

::

      lsopt_top
          |
          |---- check arguments
          |---- CALL INSTORE
          |       |
          |       |---- determine whether OPWARMI available:
          |                * if no:  cold start: create OPWARMI
          |                * if yes: warm start: read from OPWARMI
          |             create or open OPWARMD
          |
          |---- check consistency between OPWARMI and model parameters
          |
          |---- >>> if COLD start: <<<
          |      |  first simulation with f.g. xx_0; output: first ff_0, gg_0
          |      |  set first preconditioner value xdiff_0 to 1
          |      |  store xx(0), gg(0), xdiff(0) to OPWARMD (first 3 entries)
          |      |
          |     >>> else: WARM start: <<<
          |         read xx(i), gg(i) from OPWARMD (first 2 entries)
          |         for first warm start after cold start, i=0
          |
          |
          |
          |---- /// if ITMAX > 0: perform optimization (increment loop index i)
          |      (
          |      )---- save current values of gg(i-1) -> gold(i-1), ff -> fold(i-1)
          |      (---- CALL LSUPDXX
          |      )       |
          |      (       |---- >>> if jmax=0 <<<
          |      )       |      |  first optimization after cold start:
          |      (       |      |  preconditioner estimated via ff_0 - ff_(first guess)
          |      )       |      |  dd(i-1) = -gg(i-1)*preco
          |      (       |      |
          |      )       |     >>> if jmax > 0 <<<
          |      (       |         dd(i-1) = -gg(i-1)
          |      )       |         CALL HESSUPD
          |      (       |           |
          |      )       |           |---- dd(i-1) modified via Hessian approx.
          |      (       |
          |      )       |---- >>> if <dd,gg> >= 0 <<<
          |      (       |         ifail = 4
          |      )       |
          |      (       |---- compute step size: tact(i-1)
          |      )       |---- compute update: xdiff(i) = xx(i-1) + tact(i-1)*dd(i-1)
          |      (
          |      )---- >>> if ifail = 4 <<<
          |      (         goto 1000
          |      )
          |      (---- CALL OPTLINE / LSLINE
          |      )       |
         ...    ...     ...

::

         ...    ...
          |      )
          |      (---- CALL OPTLINE / LSLINE
          |      )       |
          |      (       |---- /// loop over simulations
          |      )              (
          |      (              )---- CALL SIMUL
          |      )              (       |
          |      (              )       |----  input: xdiff(i)
          |      )              (       |---- output: ff(i), gg(i)
          |      (              )       |---- >>> if ONLINE <<<
          |      )              (                 runs model and adjoint
          |      (              )             >>> if OFFLINE <<<
          |      )              (                 reads those values from file
          |      (              )
          |      )              (---- 1st Wolfe test:
          |      (              )     ff(i) <= tact*xpara1*<gg(i-1),dd(i-1)>
          |      )              (
          |      (              )---- 2nd Wolfe test:
          |      )              (     <gg(i),dd(i-1)> >= xpara2*<gg(i-1),dd(i-1)>
          |      (              )
          |      )              (---- >>> if 1st and 2nd Wolfe tests ok <<<
          |      (              )      |  320: update xx: xx(i) = xdiff(i)
          |      )              (      |
          |      (              )     >>> else if 1st Wolfe test not ok <<<
          |      )              (      |  500: INTERpolate new tact:
          |      (              )      |  barr*tact < tact < (1-barr)*tact
          |      )              (      |  CALL CUBIC
          |      (              )      |
          |      )              (     >>> else if 2nd Wolfe test not ok <<<
          |      (              )         350: EXTRApolate new tact:
          |      )              (         (1+barmin)*tact < tact < 10*tact
          |      (              )         CALL CUBIC
          |      )              (
          |      (              )---- >>> if new tact > tmax <<<
          |      )              (      |  ifail = 7
          |      (              )      |
          |      )              (---- >>> if new tact < tmin OR tact*dd < machine precision <<<
          |      (              )      |  ifail = 8
          |      )              (      |
          |      (              )---- >>> else <<<
          |      )              (         update xdiff for new simulation
          |      (              )
          |      )             \\\ if nfunc > 1: use inter-/extrapolated tact and xdiff
          |      (                               for new simulation
          |      )                               N.B.: new xx is thus not based on new gg, but
          |      (                                     rather on new step size tact
          |      )
          |      (---- store new values xx(i), gg(i) to OPWARMD (first 2 entries)
          |      )---- >>> if ifail = 7,8,9 <<<
          |      (         goto 1000
          |      )
         ...    ...

::

         ...    ...
          |      )
          |      (---- store new values xx(i), gg(i) to OPWARMD (first 2 entries)
          |      )---- >>> if ifail = 7,8,9 <<<
          |      (         goto 1000
          |      )
          |      (---- compute new pointers jmin, jmax to include latest values
          |      )     gg(i)-gg(i-1), xx(i)-xx(i-1) to Hessian matrix estimate
          |      (---- store gg(i)-gg(i-1), xx(i)-xx(i-1) to OPWARMD
          |      )     (entries 2*jmax+2, 2*jmax+3)
          |      (
          |      )---- CALL DGSCALE
          |      (       |
          |      )       |---- call dostore
          |      (       |       |
          |      )       |       |---- read preconditioner of previous iteration diag(i-1)
          |      (       |             from OPWARMD (3rd entry)
          |      )       |
          |      (       |---- compute new preconditioner diag(i), based upon diag(i-1),
          |      )       |     gg(i)-gg(i-1), xx(i)-xx(i-1)
          |      (       |
          |      )       |---- call dostore
          |      (               |
          |      )               |---- write new preconditioner diag(i) to OPWARMD (3rd entry)
          |      (
          |---- \\\ end of optimization iteration loop
          |
          |
          |
          |---- CALL OUTSTORE
          |       |
          |       |---- store gnorm0, ff(i), current pointers jmin, jmax, iterabs to OPWARMI
          |
          |---- >>> if OFFLINE version <<<
          |         xx(i+1) needs to be computed as input for offline optimization
          |          |
          |          |---- CALL LSUPDXX
          |          |       |
          |          |       |---- compute dd(i), tact(i) -> xdiff(i+1) = x(i) + tact(i)*dd(i)
          |          |
          |          |---- CALL WRITE_CONTROL
          |          |       |
          |          |       |---- write xdiff(i+1) to special file for offline optim.
          |
          |---- print final information
          |
          O

.. [1]
   ecco_check may be missing a test for conflicting names...

.. [2]
   The quadratic option in fact does not yet exist in
   ``cost_gencost_boxmean.F``...


.. _subsectionoptimm1qn3:

Alternative code to :filelink:`optim` and :filelink:`lsopt`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The non-MITgcm package `optim_m1qn3
<https://github.com/mjlosch/optim_m1qn3>`_ is based on the same
quasi-Newton variable storage method (BFGS) :cite:`gil-lem:89` as the
package in subdirectory ``lsopt``, but it uses a reverse communication
version of the latest (and probably last) release of the subroutine
`m1qn3
<https://who.rocq.inria.fr/Jean-Charles.Gilbert/modulopt/optimization-routines/m1qn3/m1qn3.html>`_.
This avoids having to define a dummy subroutine ``simul`` and also
simplifies the code structure. As a consequence this package is
simple(r) to compile and use, because ``m1qn3.f`` contains all necessary
subroutines and only one extra routine (``ddot``, which was copied
from `BLAS <http://www.netlib.org/blas/>`_) is required.

The principle of reverse communication is outlined in this example::

  external simul_rc
  ...
  reverse = .true.
  do while (.true.)
    call m1qn3 (simul_rc,...,x,f,g,...,reverse,indic,...)
    if (reverse) break
    call simul (indic,n,x,f,g)
  end while

``simul_rc`` is an empty ''model simulator'', and ``simul`` generates a
new state based on the value of ``indic``.

The original ``m1qn3`` has been modified to work "offline", i.e. the
simulator and the driver of ``m1qn3_offline`` are separate programs
that are called alternatingly from a (shell-)script. This requires
that the "state" of ``m1qn3`` is saved before this program
terminates. This state is saved in a single file ``OPWARM.optXXX`` per
simulation, where ``XXX`` is the simulation number. Communication with
the routine, writing and restoring the state of ``m1qn3`` is achieved
via three new common-blocks that are contained in three header
files. ``simul`` is replaced by reading and storing the model state
and gradient vectors. Schematically the driver routine ``optim_sub``
does the following: ::

  external simul_rc
  ...

  call optim_readdata( nn, ctrlname, ...,   xx ) ! read control vector
  call optim_readdata( nn, costname, ..., adxx ) ! read gradient vector
  call optim_store_m1qn3( ..., .false. )         ! read state of m1qn3
  reverse = .true.
  call m1qn3 (simul_rc,...,xx,objf,adxx,...,reverse,indic,...)
  call optim_store_m1qn3( ..., .true. )          ! write state of m1qn3
  call optim_writedata( nn, ctrlname, ..., xx )  ! write control vector

The optimization loop is executed outside of this program within a script.

The code can be obtained at https://github.com/mjlosch/optim_m1qn3. The `README
<https://github.com/mjlosch/optim_m1qn3/blob/master/README.md>`__ contains
short instructions how to build and use the code in combination with the
:filelink:`verification/tutorial_global_oce_optim` experiment. The usage is
very similar to the :filelink:`optim` package.

.. _sec:exp:llc:


Test Cases For Estimation Package Capabilities
----------------------------------------------

First, if you have not done so already, download the model as explained
in :ref:`chap_getting_started` via the `MITgcm git repository <https://github.com/MITgcm/MITgcm>`_: ::

    % git clone https://github.com/MITgcm/MITgcm.git

Then, download the setup from the `MITgcm
verification_other git repository <https://github.com/MITgcm/verification_other>`_: ::

    % cd MITgcm
    % git clone https://github.com/MITgcm/verification_other

and follow the additional directions provided for `global_oce_cs32
<https://github.com/MITgcm/verification_other/tree/master/global_oce_cs32>`_
or for `global_oce_llc90
<https://github.com/MITgcm/verification_other/tree/master/global_oce_llc90>`_.
These model configurations are used for daily regression tests to ensure
continued availability of the tested estimation package features discussed in
:ref:`chap_state_estimation`.  Daily results of these tests, which currently
run on the `glacier` cluster, are reported on https://mitgcm.org/testing-summary.
To this end, one sets a `crontab
job <https://www.computerhope.com/unix/ucrontab.htm>`__ that typically executes
the script reported below.  The various commands can also be used to run these
examples outside of crontab, directly at the command line via the :ref:`testreport
capability <testreport_utility>`.

.. note::

   Users are advised against running `global_oce_llc90
   <https://github.com/MITgcm/verification_other/tree/master/global_oce_llc90>`_
   tests with fewer than 12 cores (96 for adjoint tests) to avoid potential memory
   overloads. `global_oce_llc90
   <https://github.com/MITgcm/verification_other/tree/master/global_oce_llc90>`_
   (595M) uses the same LLC90 grid as the production `ECCO version 4` setup
   :cite:`for-eta:15`. The much coarser resolution `global_oce_cs32
   <https://github.com/MITgcm/verification_other/tree/master/global_oce_cs32>`_
   (614M) uses the CS32 grid and can run on any modern laptop.

::

    % #!/bin/csh -f
    % setenv PATH ~/bin:$PATH
    % setenv MODULESHOME /usr/share/Modules
    % source /usr/share/Modules/init/csh
    % module use /usr/share/Modules
    % module load openmpi-x86_64
    % setenv MPI_INC_DIR $MPI_INCLUDE
    %
    % cd ~/MITgcm
    % #mkdir gitpull.log
    % set D=`date +%Y-%m-%d`
    % git pull -v > gitpull.log/gitpull.$D.log
    %
    % cd verification
    %
    % #ieee case:
    % ./testreport -clean -t 'global_oce_*'
    % ./testreport -of=../tools/build_options/linux_amd64_gfortran -MPI 24 -t 'global_oce_*' -addr username@something.whatever
    % ../tools/do_tst_2+2 -t 'global_oce_*' -mpi -exe 'mpirun -np 24 ./mitgcmuv' -a username@something.whatever
    %
    % #devel case:
    % ./testreport -clean -t 'global_oce_*'
    % ./testreport -of=../tools/build_options/linux_amd64_gfortran -MPI 24 -devel -t 'global_oce_*' -addr username@something.whatever
    % ../tools/do_tst_2+2 -t 'global_oce_*' -mpi -exe 'mpirun -np 24 ./mitgcmuv' -a username@something.whatever
    %
    % #fast case:
    % ./testreport -clean -t 'global_oce_*'
    % ./testreport -of=../tools/build_options/linux_amd64_gfortran -MPI 24 -t 'global_oce_*' -fast -addr username@something.whatever
    % ../tools/do_tst_2+2 -t 'global_oce_*' -mpi -exe 'mpirun -np 24 ./mitgcmuv' -a username@something.whatever
    %
    % #adjoint case:
    % ./testreport -clean -t 'global_oce_*'
    % ./testreport -of=../tools/build_options/linux_amd64_gfortran -MPI 24 -ad -t 'global_oce_*' -addr username@something.whatever
