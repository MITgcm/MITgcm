.. _sub_phys_pkg_exf:

EXF: The external forcing package
---------------------------------

Authors: Patrick Heimbach and Dimitris Menemenlis

.. _ssub_phys_pkg_exf_intro:

Introduction
++++++++++++

The external forcing package, in conjunction with the calendar package
(cal), enables the handling of real-time (or “model-time”) forcing
fields of differing temporal forcing patterns. It comprises
climatological restoring and relaxation. Bulk formulae are implemented
to convert atmospheric fields to surface fluxes. An interpolation
routine provides on-the-fly interpolation of forcing fields an arbitrary
grid onto the model grid.

CPP options enable or disable different aspects of the package
(:numref:`ssub_phys_pkg_exf_config`). Runtime options, flags,
filenames and field-related dates/times are set in ``data.exf``
(:numref:`ssub_phys_pkg_exf_runtime`). A description of key subroutines
is given in :numref:`ssub_phys_pkg_exf_subroutines`. Input fields,
units and sign conventions are summarized in
:numref:`ssub_phys_pkg_exf_inputs_units`, and available
diagnostics output is listed in
:numref:`ssub_phys_pkg_exf_diagnostics`.

.. _ssub_phys_pkg_exf_config:

EXF configuration, compiling & running
++++++++++++++++++++++++++++++++++++++

Compile-time options
####################

As with all MITgcm packages, EXF can be turned on or off at compile time

-  using the ``packages.conf`` file by adding ``exf`` to it,

-  or using ``genmake2`` adding ``-enable=exf`` or ``-disable=exf``
   switches

-  *required packages and CPP options*:
   EXF (only) requires the calendar package ``cal`` to be enabled if
   the julian calendar will be used with the data ; no
   additional CPP options are required.

(see  :numref:`building_code`).

Parts of the EXF code can be enabled or disabled at compile time via CPP
preprocessor flags. These options are set in either ``EXF_OPTIONS.h`` or
in ``ECCO_CPPOPTIONS.h``. :numref:`tab_phys_pkg_exf_cpp_options` summarizes these
options.

.. table:: EXF CPP options
    :name: tab_phys_pkg_exf_cpp_options

    +----------------------------------+-----------------------------------------------------------+
    |        **CPP option**            |  **Description**                                          |
    +==================================+===========================================================+
    | :code:`EXF_VERBOSE`              |   verbose mode (recommended only for testing)             |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_ATM_TEMP`           |  compute heat/freshwater fluxes from atmos. state input   |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_ATM_WIND`           |  compute wind stress from wind speed input                |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_BULKFORMULAE`       |  use bulk formulae following Large and Pond (1981, 1982); |
    |                                  |  requires to define :code:`ALLOW_ATM_TEMP`.               |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_BULK_LARGEYEAGER04` |  use modifications of Large and Pond (1981, 1982), as     |
    |                                  |  described in  Large and Yeager (2004) NCAR/TN-460+STR.   |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_DRAG_LARGEYEAGER09` |  compute drag cofficient following Large and Yeager       |
    |                                  |  (2009), Climate Dynamics, 33, pages 341-364              |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`EXF_READ_EVAP`            |  read evaporation instead of computing it                 |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_RUNOFF`             |  read time-constant river/glacier run-off field           |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_DOWNWARD_RADIATION` |  compute net from downward or downward from net radiation |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`USE_EXF_INTERPOLATION`    |  enable on-the-fly bilinear or bicubic                    |
    |                                  |  interpolation of input fields                            |
    +----------------------------------+-----------------------------------------------------------+
    |  *used in conjunction with relaxation to prescribed (climatological) fields*                 |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_CLIMSST_RELAXATION` |  relaxation to 2-D SST climatology                        |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ALLOW_CLIMSSS_RELAXATION` |  relaxation to 2-D SSS climatology                        |
    +----------------------------------+-----------------------------------------------------------+
    |  *these are set outside of EXF in* :code:`CPP_OPTIONS.h`                                     |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`SHORTWAVE_HEATING`        | enable shortwave radiation                                |
    +----------------------------------+-----------------------------------------------------------+
    | :code:`ATMOSPHERIC_LOADING`      | enable surface pressure forcing                           |
    +----------------------------------+-----------------------------------------------------------+

.. _ssub_phys_pkg_exf_runtime:

Run-time parameters
+++++++++++++++++++

Run-time parameters are set in files ``data.pkg`` and ``data.exf`` which
is read in ``exf_readparms.F``. Run-time parameters may be broken into 3
categories: (i) switching on/off the package at runtime, (ii) general
flags and parameters, and (iii) attributes for each forcing and
climatological field.

Enabling the package
####################

A package is switched on/off at runtime by setting (e.g. for EXF)
``useEXF = .TRUE.`` in ``data.pkg``.

General flags and parameters
############################

.. table:: EXF runtime options
    :name: tab_phys_pkg_exf_runtime_params

    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | **Flag/parameter**      | **default**      |  **Description**                                                              |
    +=========================+==================+===============================================================================+
    | useExfCheckRange        | :code:`.TRUE.`   | check range of input fields and stop if out of range                          |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | useExfYearlyFields      | :code:`.FALSE.`  | append current year postfix of form ``_YYYY`` on filename                     |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | twoDigitYear            | :code:`.FALSE.`  | instead of appending ``_YYYY`` append  ``YY``                                 |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | repeatPeriod            | 0.0              | > 0: cycle through all input fields at the same period (in seconds)           |
    |                         |                  +-------------------------------------------------------------------------------+
    |                         |                  | = 0: use period assigned to each field                                        |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | exf_offset_atemp        | 0.0              | set to 273.16 to convert from deg. Kelvin (assumed input) to Celsius          |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | windstressmax           | 2.0              | max. allowed wind stress N m\ :sup:`--2`                                      |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | exf_albedo              | 0.1              | surface albedo used to compute downward vs. net radiative fluxes              |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | climtempfreeze          | -1.9             | ???                                                                           |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | ocean_emissivity        |                  | longwave ocean-surface emissivity                                             |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | ice_emissivity          |                  | longwave seaice emissivity                                                    |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | snow_emissivity         |                  | longwave  snow  emissivity                                                    |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | exf_iceCd               | 1.63E-3          | drag coefficient over sea-ice                                                 |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | exf_iceCe               | 1.63E-3          | evaporation transfer coeff. over sea-ice                                      |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | exf_iceCh               | 1.63E-3          | sensible heat transfer coeff. over sea-ice                                    |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | exf_scal_BulkCdn        | 1.0              | overall scaling of neutral drag coeff.                                        |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | useStabilityFct_overIce | :code:`.FALSE.`  | compute turbulent transfer coeff. over sea-ice                                |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | readStressOnAgrid       | :code:`.FALSE.`  | read wind-streess located on model-grid, A-grid point                         |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | readStressOnCgrid       | :code:`.FALSE.`  | read wind-streess located on model-grid, C-grid point                         |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | useRelativeWind         | :code:`.FALSE.`  | subtract [U/V]VEL or [U/VICE from U/V]WIND before                             |
    |                         |                  | computing [U/V]STRESS                                                         |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | zref                    | 10.0             | reference height                                                              |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | hu                      | 10.0             | height of mean wind                                                           |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | ht                      | 2.0              | height of mean temperature and rel. humidity                                  |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | umin                    | 0.5              | minimum absolute wind speed for computing Cd                                  |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | atmrho                  | 1.2              | mean atmospheric density [kg/m\ :sup:`3`]                                     |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | atmcp                   | 1005.0           | mean atmospheric specific heat [J/kg/K]                                       |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cdrag_[n]               |                  | n = 1,2,3,8; parameters for drag coeff. function                              |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cdrag_1                 | 0.0027000        | [m/s]                                                                         |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cdrag_2                 | 0.0001420        | [-]                                                                           |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cdrag_3                 | 0.0000764        | [s/m]                                                                         |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cdrag_8                 | -3.14807e-13     | [(s/m)\ :sup:`6`] (only used with Large and Yeager, 2009)                     |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cdragMax                |  0.00234         | maximum drag [-] (only Large and Yeager, 2009) for wind > umax                |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | umax                    | 33.              | threshold above which cdragMax applies [m/s] (only Large and Yeager, 2009)    |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cstanton_[n]            | ???              | n = 1,2; parameters for Stanton number function                               |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cdalton                 | ???              | parameter for Dalton number function                                          |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | flamb                   | 2500000.0        | latent heat of evaporation [J/kg]                                             |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | flami                   | 334000.0         | latent heat of melting of pure ice [J/kg]                                     |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | zolmin                  | -100.0           | minimum stability parameter                                                   |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cvapor_fac              | 640380.0         |                                                                               |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cvapor_exp              | 5107.4           |                                                                               |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cvapor_fac_ice          | 11637800.0       |                                                                               |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | cvapor_fac_ice          | 5897.8           |                                                                               |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | humid_fac               | 0.606            | parameter for virtual temperature calculation                                 |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | gamma_blk               | 0.010            | adiabatic lapse rate                                                          |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | saltsat                 | 0.980            | reduction of saturation vapor pressure over salt-water                        |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | psim_fac                | 5.0              |                                                                               |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | exf_monFreq             | monitorFreq      | output frequency [s]                                                          |
    +-------------------------+------------------+-------------------------------------------------------------------------------+
    | exf_iprec               | 32               | precision of input fields (32-bit or 64-bit)                                  |
    +-------------------------+------------------+-------------------------------------------------------------------------------+

Field attributes
################

All EXF fields are listed in
:numref:`ssub_phys_pkg_exf_inputs_units`. Each field has a number of
attributes which can be customized. They are summarized in
:numref:`tab_phys_pkg_exf_runtime_attributes`. To obtain an attribute for a
specific field, e.g. ``uwind`` prepend the field name to the listed
attribute, e.g. for attribute ``period`` this yields ``uwindperiod``:

.. math::

   \begin{aligned}
     \begin{array}{cccccc}
       ~ & \texttt{field} & \& & \texttt{attribute} & \longrightarrow & \texttt{parameter} \\
       \text{e.g.} & \text{uwind} & \& & \text{period} & \longrightarrow & \text{uwindperiod} \\
     \end{array}\end{aligned}

.. table:: EXF runtime attributes
           Note there is one exception for the default of ``atempconst`` = celsius2K = 273.16
    :name: tab_phys_pkg_exf_runtime_attributes

    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | **attribute**               | **Default**               | **Description**                                                              |
    +=============================+===========================+==============================================================================+
    | *field* ``file``            | ' '                       | filename; if left empty no file will be read; ``const`` will be used instead |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``const``           | 0.0                       | constant that will be used if no file is read                                |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``startdate1``      | 0.0                       | format: ``YYYYMMDD``; start year (YYYY), month (MM), day (YY)                |
    |                             |                           | of field to determine record number                                          |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``startdate2``      | 0.0                       | format: ``HHMMSS``; start hour (HH), minute (MM), second(SS)                 |
    |                             |                           | of field to determine record number                                          |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``period``          | 0.0                       | interval in seconds between two records; the special value -12 means         |
    |                             |                           | 12 repeating (calendar) monthly records; the special value -1 means          |
    |                             |                           | non-repeating (calendar) monthly records (see below)                         |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | ``exf_inscal_``\ *field*    |                           | optional rescaling of input fields to comply with EXF units                  |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | ``exf_outscal_``\ *field*   |                           | optional rescaling of EXF fields when mapped onto MITgcm fields              |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *used in conjunction with* ``EXF_USE_INTERPOLATION``                                                                                   |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``_lon0``           | :code:`xgOrigin+delX/2`   | starting longitude of input                                                  |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``_lon_inc``        | :code:`delX`              | increment in longitude of input                                              |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``_lat0``           | :code:`ygOrigin+delY/2`   | starting latitude of input                                                   |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``_lat_inc``        | :code:`delY`              | increment in latitude of input                                               |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``_nlon``           | :code:`Nx`                | number of grid points in longitude of input                                  |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+
    | *field* ``_nlat``           | :code:`Ny`                | number of grid points in longitude of input                                  |
    +-----------------------------+---------------------------+------------------------------------------------------------------------------+

For *field*\ ``period``\ =-1, the records in the forcing file represent
averages over calendar months.  If ``useExfYearlyFields = .TRUE.``, each yearly
file must have 12 records, starting with January.  For ``useExfYearlyFields =
.FALSE.``, a single file starting with the month given by
*field*\ ``startdate1`` is required.

Example configuration
#####################

The following block is taken from the ``data.exf`` file of the
verification experiment ``global_oce_latlon``. It defines attributes for
the heat flux variable ``hflux``:

::

     hfluxfile       = 'ncep_qnet.bin',
     hfluxstartdate1 = 19920101,
     hfluxstartdate2 = 000000,
     hfluxperiod     = 2592000.0,
     hflux_lon0      = 2
     hflux_lon_inc   = 4
     hflux_lat0      = -78
     hflux_lat_inc   = 39*4
     hflux_nlon      = 90
     hflux_nlat      = 40

EXF will read a file of name ’ncep\_qnet.bin’. Its first record
represents January 1st, 1992 at 00:00 UTC. Next record is 2592000
seconds (or 30 days) later. Note that the first record read and used by
the EXF package corresponds to the value ’startDate1’ set in data.cal.
Therefore if you want to start the EXF forcing from later in the
’ncep\_qnet.bin’ file, it suffices to specify startDate1 in data.cal as
a date later than 19920101 (for example, startDate1 = 19940101, for
starting January 1st, 1994). For this to work, ’ncep\_qnet.bin’ must
have at least 2 years of data because in this configuration EXF will
read 2 years into the file to find the 1994 starting value.
Interpolation on-the-fly is used (in the present case trivially on the
same grid, but included nevertheless for illustration), and input field
grid starting coordinates and increments are supplied as well.

.. _ssub_phys_pkg_exf_bulk_formulae:

EXF bulk formulae
+++++++++++++++++

T.B.D. (cross-ref. to parameter list table)

.. _ssub_phys_pkg_exf_inputs_units:

EXF input fields and units
++++++++++++++++++++++++++

The following list is taken from the header file ``EXF_FIELDS.h``. It
comprises all EXF input fields.

Output fields which EXF provides to the MITgcm are fields **fu**,
**fv**, **Qnet**, **Qsw**, **EmPmR**, and **pload**. They are defined in
``FFIELDS.h``.

::

    c----------------------------------------------------------------------
    c               |
    c     field     :: Description
    c               |
    c----------------------------------------------------------------------
    c     ustress   :: Zonal surface wind stress in N/m^2
    c               |  > 0 for increase in uVel, which is west to
    c               |      east for cartesian and spherical polar grids
    c               |  Typical range: -0.5 < ustress < 0.5
    c               |  Southwest C-grid U point
    c               |  Input field
    c----------------------------------------------------------------------
    c     vstress   :: Meridional surface wind stress in N/m^2
    c               |  > 0 for increase in vVel, which is south to
    c               |      north for cartesian and spherical polar grids
    c               |  Typical range: -0.5 < vstress < 0.5
    c               |  Southwest C-grid V point
    c               |  Input field
    c----------------------------------------------------------------------
    c     hs        :: sensible heat flux into ocean in W/m^2
    c               |  > 0 for increase in theta (ocean warming)
    c----------------------------------------------------------------------
    c     hl        :: latent   heat flux into ocean in W/m^2
    c               |  > 0 for increase in theta (ocean warming)
    c----------------------------------------------------------------------
    c     hflux     :: Net upward surface heat flux in W/m^2
    c               |  (including shortwave)
    c               |  hflux = latent + sensible + lwflux + swflux
    c               |  > 0 for decrease in theta (ocean cooling)
    c               |  Typical range: -250 < hflux < 600
    c               |  Southwest C-grid tracer point
    c               |  Input field
    c----------------------------------------------------------------------
    c     sflux     :: Net upward freshwater flux in m/s
    c               |  sflux = evap - precip - runoff
    c               |  > 0 for increase in salt (ocean salinity)
    c               |  Typical range: -1e-7 < sflux < 1e-7
    c               |  Southwest C-grid tracer point
    c               |  Input field
    c----------------------------------------------------------------------
    c     swflux    :: Net upward shortwave radiation in W/m^2
    c               |  swflux = - ( swdown - ice and snow absorption - reflected )
    c               |  > 0 for decrease in theta (ocean cooling)
    c               |  Typical range: -350 < swflux < 0
    c               |  Southwest C-grid tracer point
    c               |  Input field
    c----------------------------------------------------------------------
    c     uwind     :: Surface (10-m) zonal wind velocity in m/s
    c               |  > 0 for increase in uVel, which is west to
    c               |      east for cartesian and spherical polar grids
    c               |  Typical range: -10 < uwind < 10
    c               |  Southwest C-grid U point
    c               |  Input or input/output field
    c----------------------------------------------------------------------
    c     vwind     :: Surface (10-m) meridional wind velocity in m/s
    c               |  > 0 for increase in vVel, which is south to
    c               |      north for cartesian and spherical polar grids
    c               |  Typical range: -10 < vwind < 10
    c               |  Southwest C-grid V point
    c               |  Input or input/output field
    c----------------------------------------------------------------------
    c     wspeed    :: Surface (10-m) wind speed in m/s
    c               |  >= 0 sqrt(u^2+v^2)
    c               |  Typical range: 0 < wspeed < 10
    c               |  Input or input/output field
    c----------------------------------------------------------------------
    c     atemp     :: Surface (2-m) air temperature in deg K
    c               |  Typical range: 200 < atemp < 300
    c               |  Southwest C-grid tracer point
    c               |  Input or input/output field
    c----------------------------------------------------------------------
    c     aqh       :: Surface (2m) specific humidity in kg/kg
    c               |  Typical range: 0 < aqh < 0.02
    c               |  Southwest C-grid tracer point
    c               |  Input or input/output field
    c----------------------------------------------------------------------
    c     lwflux    :: Net upward longwave radiation in W/m^2
    c               |  lwflux = - ( lwdown - ice and snow absorption - emitted )
    c               |  > 0 for decrease in theta (ocean cooling)
    c               |  Typical range: -20 < lwflux < 170
    c               |  Southwest C-grid tracer point
    c               |  Input field
    c----------------------------------------------------------------------
    c     evap      :: Evaporation in m/s
    c               |  > 0 for increase in salt (ocean salinity)
    c               |  Typical range: 0 < evap < 2.5e-7
    c               |  Southwest C-grid tracer point
    c               |  Input, input/output, or output field
    c----------------------------------------------------------------------
    c     precip    :: Precipitation in m/s
    c               |  > 0 for decrease in salt (ocean salinity)
    c               |  Typical range: 0 < precip < 5e-7
    c               |  Southwest C-grid tracer point
    c               |  Input or input/output field
    c----------------------------------------------------------------------
    c    snowprecip :: snow in m/s
    c               |  > 0 for decrease in salt (ocean salinity)
    c               |  Typical range: 0 < precip < 5e-7
    c               |  Input or input/output field
    c----------------------------------------------------------------------
    c     runoff    :: River and glacier runoff in m/s
    c               |  > 0 for decrease in salt (ocean salinity)
    c               |  Typical range: 0 < runoff < 5e-7
    c               |  Southwest C-grid tracer point
    c               |  Input or input/output field
    c----------------------------------------------------------------------
    c     swdown    :: Downward shortwave radiation in W/m^2
    c               |  > 0 for increase in theta (ocean warming)
    c               |  Typical range: 0 < swdown < 450
    c               |  Southwest C-grid tracer point
    c               |  Input/output field
    c----------------------------------------------------------------------
    c     lwdown    :: Downward longwave radiation in W/m^2
    c               |  > 0 for increase in theta (ocean warming)
    c               |  Typical range: 50 < lwdown < 450
    c               |  Southwest C-grid tracer point
    c               |  Input/output field
    c----------------------------------------------------------------------
    c     apressure :: Atmospheric pressure field in N/m^2
    c               |  Typical range: 88000 < apressure < 108000
    c               |  Southwest C-grid tracer point
    c               |  Input field
    c----------------------------------------------------------------------

.. _ssub_phys_pkg_exf_subroutines:

Key subroutines
+++++++++++++++

Top-level routine: ``exf_getforcing.F``

::

    C     !CALLING SEQUENCE:
    c ...
    c  exf_getforcing (TOP LEVEL ROUTINE)
    c  |
    c  |-- exf_getclim (get climatological fields used e.g. for relax.)
    c  |   |--- exf_set_climsst  (relax. to 2-D SST field)
    c  |   |--- exf_set_climsss  (relax. to 2-D SSS field)
    c  |   o
    c  |
    c  |-- exf_getffields <- this one does almost everything
    c  |   |   1. reads in fields, either flux or atmos. state,
    c  |   |      depending on CPP options (for each variable two fields
    c  |   |      consecutive in time are read in and interpolated onto
    c  |   |      current time step).
    c  |   |   2. If forcing is atmos. state and control is atmos. state,
    c  |   |      then the control variable anomalies are read here via ctrl_get_gen
    c  |   |      (atemp, aqh, precip, swflux, swdown, uwind, vwind).
    c  |   |      If forcing and control are fluxes, then
    c  |   |      controls are added later.
    c  |   o
    c  |
    c  |-- exf_radiation
    c  |   |    Compute net or downwelling radiative fluxes via
    c  |   |    Stefan-Boltzmann law in case only one is known.
    c  |   o
    c  |-- exf_wind
    c  |   |   Computes wind speed and stresses, if required.
    c  |   o
    c  |
    c  |-- exf_bulkformulae
    c  |   |   Compute air-sea buoyancy fluxes from
    c  |   |   atmospheric state following Large and Pond, JPO, 1981/82
    c  |   o
    c  |
    c  |-- < hflux is sum of sensible, latent, longwave rad. >
    c  |-- < sflux is sum of evap. minus precip. minus runoff  >
    c  |
    c  |-- exf_getsurfacefluxes
    c  |   If forcing and control is flux, then the
    c  |   control vector anomalies are read here via ctrl_get_gen
    c  |   (hflux, sflux, ustress, vstress)
    c  |
    c  |-- < update tile edges here >
    c  |
    c  |-- exf_check_range
    c  |   |   Check whether read fields are within assumed range
    c  |   |   (may capture mismatches in units)
    c  |   o
    c  |
    c  |-- < add shortwave to hflux for diagnostics >
    c  |
    c  |-- exf_diagnostics_fill
    c  |   |   Do EXF-related diagnostics output here.
    c  |   o
    c  |
    c  |-- exf_mapfields
    c  |   |   Forcing fields from exf package are mapped onto
    c  |   |   mitgcm forcing arrays.
    c  |   |   Mapping enables a runtime rescaling of fields
    c  |   o
    C  o

Radiation calculation: ``exf_radiation.F``

Wind speed and stress calculation: ``exf_wind.F``

Bulk formula: ``exf_bulkformulae.F``

Generic I/O: ``exf_set_gen.F``

Interpolation: ``exf_interp.F``

Header routines

.. _ssub_phys_pkg_exf_diagnostics:

EXF diagnostics
+++++++++++++++

Diagnostics output is available via the diagnostics package (see
:numref:`sub_outp_pkg_diagnostics`). Available output fields are
summarized below.

::

    ---------+----+----+----------------+-----------------
     <-Name->|Levs|grid|<--  Units   -->|<- Tile (max=80c)
    ---------+----+----+----------------+-----------------
     EXFhs   |  1 | SM | W/m^2          | Sensible heat flux into ocean, >0 increases theta
     EXFhl   |  1 | SM | W/m^2          | Latent heat flux into ocean, >0 increases theta
     EXFlwnet|  1 | SM | W/m^2          | Net upward longwave radiation, >0 decreases theta
     EXFswnet|  1 | SM | W/m^2          | Net upward shortwave radiation, >0 decreases theta
     EXFlwdn |  1 | SM | W/m^2          | Downward longwave radiation, >0 increases theta
     EXFswdn |  1 | SM | W/m^2          | Downward shortwave radiation, >0 increases theta
     EXFqnet |  1 | SM | W/m^2          | Net upward heat flux (turb+rad), >0 decreases theta
     EXFtaux |  1 | SU | N/m^2          | zonal surface wind stress, >0 increases uVel
     EXFtauy |  1 | SV | N/m^2          | meridional surface wind stress, >0 increases vVel
     EXFuwind|  1 | SM | m/s            | zonal 10-m wind speed, >0 increases uVel
     EXFvwind|  1 | SM | m/s            | meridional 10-m wind speed, >0 increases uVel
     EXFwspee|  1 | SM | m/s            | 10-m wind speed modulus ( >= 0 )
     EXFatemp|  1 | SM | degK           | surface (2-m) air temperature
     EXFaqh  |  1 | SM | kg/kg          | surface (2-m) specific humidity
     EXFevap |  1 | SM | m/s            | evaporation, > 0 increases salinity
     EXFpreci|  1 | SM | m/s            | evaporation, > 0 decreases salinity
     EXFsnow |  1 | SM | m/s            | snow precipitation, > 0 decreases salinity
     EXFempmr|  1 | SM | m/s            | net upward freshwater flux, > 0 increases salinity
     EXFpress|  1 | SM | N/m^2          | atmospheric pressure field

References
++++++++++

Experiments and tutorials that use exf
++++++++++++++++++++++++++++++++++++++

-  Global Ocean experiment, in global\_with\_exf verification directory

-  Labrador Sea experiment, in lab\_sea verification directory
