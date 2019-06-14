.. _sub_phys_pkg_gchem:

GCHEM Package
-------------

Introduction
~~~~~~~~~~~~

This package has been developed as interface to the PTRACERS package.
The purpose is to provide a structure where various (any) tracer
experiments can be added to the code. For instance there are
placeholders for routines to read in parameters needed for any tracer
experiments, a routine to read in extra fields required for the tracer
code, routines for either external forcing or internal interactions
between tracers and routines for additional diagnostics relating to the
tracers. Note that the gchem package itself is only a means to call the
subroutines used by specific biogeochemical experiments, and does not
“do” anything on its own.

There are two examples: **cfc** which looks at 2 tracers with a simple
external forcing and **dic** with 4,5 or 6 tracers whose tendency terms
are related to one another. We will discuss these here only as how they
provide examples to use this package.

Key subroutines and parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| **FRAMEWORK**
| The different biogeochemistry frameworks (e.g. cfc of dic) are
  specified in the packages\_conf file. *GCHEM\_OPTIONS.h* includes the
  compiler options to be used in any experiment. An important compiler
  option is #define GCHEM\_SEPARATE\_FORCING which determined how and
  when the tracer forcing is applied (see discussion on Forcing below).
  See section on dic for some additional flags that can be set for that
  experiment.
| There are further runtime parameters set in *data.gchem* and kept in
  common block *GCHEM.h*. These runtime options include:
| :math:`\bullet` Parameters to set the timing for periodic forcing
  files to be loaded are: *gchem\_ForcingPeriod*, *gchem\_ForcingCycle*.
  The former is how often to load, the latter is how often to cycle
  through those fields (eg. period couple be monthly and cycle one
  year). This is used in *dic* and *cfc*, with gchem\_ForcingPeriod=0
  meaning no periodic forcing.
| :math:`\bullet` **nsubtime** is the integer number of extra timesteps
  required by the tracer experiment. This will give a timestep of
  **deltaTtracer**\ :math:`/`\ **nsubtime** for the dependencies between
  tracers. The default is one.
| :math:`\bullet` File names - these are several filenames than can be
  read in for external fields needed in the tracer forcing - for
  instance wind speed is needed in both DIC and CFC packages to
  calculate the air-sea exchange of gases. Not all file names will be
  used for every tracer experiment.
| :math:`\bullet` **gchem\_int\_** are variable names for run-time set
  integer numbers. (Currently 1 through 5).
| :math:`\bullet` **gchem\_rl\_** are variable names for run-time set
  real numbers. (Currently 1 through 5).
| :math:`\bullet` Note that the old **tIter0** has been replaced by
  **PTRACERS\_Iter0** which is set in data.ptracers instead.
|  
| **INITIALIZATION**
| The values set at runtime in data.gchem are read in using
  *gchem\_readparms.F* which is called from packages\_readparms.F. This
  will include any external forcing files that will be needed by the
  tracer experiment.
| There are two routine used to initialize parameters and fields needed by
  the experiment packages. These are *gchem\_init\_fixed.F* which is
  called from *packages\_init\_fixed.F*, and *gchem\_init\_vari.F* called
  from packages\_init\_variable.F. The first should be used to call a
  subroutine specific to the tracer experiment which sets fixed
  parameters, the second should call a subroutine specific to the tracer
  experiment which sets (or initializes) time fields that will vary with
  time.
| 
| **LOADING FIELDS**
| External forcing fields used by the tracer experiment are read in by a
  subroutine (specific to the tracer experiment) called from
  *gchem\_fields\_load.F*. This latter is called from *forward\_step.F*.
| 
| **FORCING**
| Tracer fields are advected-and-diffused by the ptracer package.
  Additional changes (e.g. surface forcing or interactions between
  tracers) to these fields are taken care of by the gchem interface. For
  tracers that are essentially passive (e.g. CFC’s) but may have some
  surface boundary conditions this can easily be done within the regular
  tracer timestep. In this case *gchem\_calc\_tendency.F* is called from
  *forward\_step.F*, where the reactive (as opposed to the advective
  diffusive) tendencies are computed. These tendencies, stored on the 3D
  field **gchemTendency**, are added to the passive tracer tendencies
  **gPtr** in *gchem\_add\_tendency.F*, which is called from
  *ptracers\_forcing.F*. For tracers with more complicated dependencies
  on each other, and especially tracers which require a smaller timestep
  than deltaTtracer, it will be easier to use *gchem\_forcing\_sep.F*
  which is called from forward\_step.F. There is a compiler option set
  in *GCHEM\_OPTIONS.h* that determines which method is used: #define
  GCHEM\_SEPARATE\_FORCING does the latter where tracers are forced
  separately from the advection-diffusion code, and #undef
  GCHEM\_SEPARATE\_FORCING includes the forcing in the regular
  timestepping.
| 
| **DIAGNOSTICS**
| This package also also used the passive tracer routine
  *ptracers\_monitor.F* which prints out tracer statistics as often as
  the model dynamic statistic diagnostics (dynsys) are written (or as
  prescribed by the runtime flag **PTRACERS\_monitorFreq**, set in
  *data.ptracers*). There is also a placeholder for any tracer
  experiment specific diagnostics to be calculated and printed to files.
  This is done in *gchem\_diags.F*. For instance the time average CO2
  air-sea fluxes, and sea surface pH (among others) are written out by
  *dic\_biotic\_diags.F* which is called from *gchem\_diags.F*.
| 

.. _gchem_diagnostics:

GCHEM Diagnostics
~~~~~~~~~~~~~~~~~

These diagnostics are particularly for the **dic** package.

::


    ------------------------------------------------------------------------
    <-Name->|Levs|<-parsing code->|<--  Units   -->|<- Tile (max=80c) 
    ------------------------------------------------------------------------
    DICBIOA | 15 |SM P    MR      |mol/m3/sec      |Biological Productivity (mol/m3/s)
    DICCARB | 15 |SM P    MR      |mol eq/m3/sec   |Carbonate chg-biol prod and remin (mol eq/m3/s)
    DICTFLX |  1 |SM P    L1      |mol/m3/sec      |Tendency of DIC due to air-sea exch (mol/m3/s)
    DICOFLX |  1 |SM P    L1      |mol/m3/sec      |Tendency of O2 due to air-sea exch (mol/m3/s)
    DICCFLX |  1 |SM P    L1      |mol/m2/sec      |Flux of CO2 - air-sea exch (mol/m2/s)
    DICPCO2 |  1 |SM P    M1      |atm             |Partial Pressure of CO2 (atm)
    DICPHAV |  1 |SM P    M1      |dimensionless   |pH (dimensionless)

Do’s and Don’ts
~~~~~~~~~~~~~~~

The pkg ptracer is required with use with this pkg. Also, as usual, the
runtime flag **useGCHEM** must be set to **.TRUE.** in **data.pkg**. By
itself, gchem pkg will read in **data.gchem** and will write out gchem
diagnostics. It requires tracer experiment specific calls to do anything
else (for instance the calls to dic and cfc pkgs).

Reference Material
~~~~~~~~~~~~~~~~~~

Experiments and tutorials that use gchem
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Global Ocean biogeochemical tutorial, in
   tutorial\_global\_oce\_biogeo verification directory, described in
   section [sec:eg-biogeochem\_tutorial] uses gchem and dic

-  Global Ocean cfc tutorial, in tutorial\_cfc\_offline verification
   directory, uses gchem and cfc (and offline) described in
   [sec:eg-offline-cfc]

-  Global Ocean online cfc example in cfc\_example verification
   directory, uses gchem and cfc
