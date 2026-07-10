.. _sub_outp_pkg_diagnostics:

pkg/diagnostics – Output Diagnostics in MITgcm
==============================================

Introduction
------------

This section of the documentation describes the diagnostics package
(:filelink:`pkg/diagnostics`)
available within MITgcm. A large selection of model diagnostics is
available for output. In addition to the diagnostic quantities
pre-defined within MITgcm, there exists the option, in any code setup, to
define a new diagnostic quantity and include it as part of the
diagnostic output with the addition of a single subroutine call in the
routine where the field is computed. As a matter of philosophy, no
diagnostic is enabled as default, thus each user must specify the exact
diagnostic information required for an experiment. This is accomplished
by enabling the specific diagnostics of interest from the list of
:ref:`available diagnostics <diagnostics_list>`. Additional diagnostic quantities,
defined within different MITgcm
packages, are available and are listed in the diagnostic list subsection
of the manual section associated with each relevant package.  Instructions for
enabling diagnostic output and defining new diagnostic quantities are
found in :numref:`usage_notes` of this document.

Once a diagnostic is enabled, MITgcm will continually increment an array
specifically allocated for that diagnostic whenever the appropriate
quantity is computed. A counter is defined which records how many times
each diagnostic quantity has been incremented. Several special
diagnostics are included in the list of :ref:`available diagnostics <diagnostics_list>`.
Quantities referred to as “counter diagnostics” are defined for selected diagnostics which record the
frequency at which a diagnostic is incremented separately for each model
grid location. Quantities referred to as “user diagnostics” are included
to facilitate defining new diagnostics for a particular
experiment.

Equations
---------

Not relevant.

Key Subroutines and Parameters
------------------------------

There are several utilities within MITgcm available to users to enable,
disable, clear, write and retrieve model diagnostics, and may be called
from any routine. The available utilities and the CALL sequences are
listed below.

:filelink:`diagnostics_addtolist.F <pkg/diagnostics/diagnostics_addtolist.F>`:
This routine is the underlying interface
routine for defining a new permanent diagnostic in the main model or in
a package. The calling sequence is:

::

           CALL DIAGNOSTICS_ADDTOLIST (
         O     diagNum,
         I     diagName, diagCode, diagUnits, diagTitle, diagMate,
         I     myThid )

         where:
           diagNum   = diagnostic Id number - Output from routine
           diagName  = name of diagnostic to declare
           diagCode  = parser code for this diagnostic
           diagUnits = field units for this diagnostic
           diagTitle = field description for this diagnostic
           diagMate  = diagnostic mate number
           myThid    = my Thread Id number

:filelink:`diagnostics_fill.F <pkg/diagnostics/diagnostics_fill.F>`:
This is the main user interface routine to the
diagnostics package. This routine will increment the specified
diagnostic quantity with a field sent through the argument list.

::

            CALL DIAGNOSTICS_FILL(
           I             inpFld, diagName,
           I             kLev, nLevs, bibjFlg, bi, bj, myThid )

         where:
            inpFld   = Field to increment diagnostics array
            diagName = diagnostic identificator name (8 characters long)
            kLev     = Integer flag for vertical levels:
                       > 0 (any integer): WHICH single level to increment in qdiag.
                       0,-1 to increment "nLevs" levels in qdiag,
                       0 : fill-in in the same order as the input array
                       -1: fill-in in reverse order.
            nLevs    = indicates Number of levels of the input field array
                       (whether to fill-in all the levels (kLev<1) or just one (kLev>0))
            bibjFlg  = Integer flag to indicate instructions for bi bj loop
                     = 0 indicates that the bi-bj loop must be done here
                     = 1 indicates that the bi-bj loop is done OUTSIDE
                     = 2 indicates that the bi-bj loop is done OUTSIDE
                          AND that we have been sent a local array (with overlap regions)
                          (local array here means that it has no bi-bj dimensions)
                     = 3 indicates that the bi-bj loop is done OUTSIDE
                          AND that we have been sent a local array
                          AND that the array has no overlap region (interior only)
                       NOTE - bibjFlg can be NEGATIVE to indicate not to increment counter
            bi       = X-direction tile number - used for bibjFlg=1-3
            bj       = Y-direction tile number - used for bibjFlg=1-3
            myThid   =  my thread Id number

:filelink:`diagnostics_scale_fill.F <pkg/diagnostics/diagnostics_scale_fill.F>`:
This is a possible alternative routine
to :filelink:`diagnostics_fill.F <pkg/diagnostics/diagnostics_fill.F>`
which performs the same functions and has an
additional option to scale the field before filling or raise the field
to a power before filling.

::

            CALL DIAGNOSTICS_SCALE_FILL(
           I             inpFld, scaleFact, power, diagName,
           I             kLev, nLevs, bibjFlg, bi, bj, myThid )


         where all the arguments are the same as for DIAGNOSTICS_FILL with
         the addition of:
            scaleFact   = Scaling factor to apply to the input field product
            power       = Integer power to which to raise the input field (after scaling)

:filelink:`diagnostics_fract_fill.F <pkg/diagnostics/diagnostics_fract_fill.F>`:
This is a specific alternative routine
to :filelink:`diagnostics_scale_fill.F <pkg/diagnostics/diagnostics_scale_fill.F>`
for the case of a diagnostics which is
associated to a fraction-weight factor (referred to as the diagnostics
“counter-mate”). This fraction-weight field is expected to vary during
the simulation and is provided as argument to :filelink:`diagnostics_fract_fill.F <pkg/diagnostics/diagnostics_frac_fill.F>`
in order to perform fraction-weighted time-average diagnostics. Note
that the fraction-weight field has to correspond to the diagnostics
counter-mate which has to be filled independently with a call to
:filelink:`diagnostics_fill.F <pkg/diagnostics/diagnostics_fill.F>`.

::

            CALL DIAGNOSTICS_FRACT_FILL(
           I             inpFld, fractFld, scaleFact, power, diagName,
           I             kLev, nLevs, bibjFlg, bi, bj, myThid )


         where all the arguments are the same as for DIAGNOSTICS_SCALE_FILL with
         the addition of:
            fractFld    = fraction used for weighted average diagnostics

:filelink:`diagnostics_is_on.F <pkg/diagnostics/diagnostics_is_on.F>`:
Function call to inquire whether a diagnostic
is active and should be incremented. Useful when there is a computation
that must be done locally before a call to :filelink:`diagnostics_fill.F <pkg/diagnostics/diagnostics_fill.F>`.
The call sequence:

::

            flag = DIAGNOSTICS_IS_ON( diagName, myThid )

         where:
            diagName = diagnostic identificator name (8 characters long)
            myThid   = my thread Id number

:filelink:`diagnostics_count.F <pkg/diagnostics/diagnostics_count.F>`:
This subroutine increments the diagnostics
counter only. In general, the diagnostics counter is incremented at the
same time as the diagnostics is filled, by calling :filelink:`diagnostics_fill.F <pkg/diagnostics/diagnostics_fill.F>`.
However, there are few cases where the counter is not incremented during
the filling (e.g., when the filling is done level per level but level 1
is skipped) and needs to be done explicitly with a call to subroutine
:filelink:`diagnostics_count.F <pkg/diagnostics/diagnostics_count.F>`. The call sequence is:

::

            CALL DIAGNOSTICS_COUNT(
           I                        diagName, bi, bj, myThid )

         where:
            diagName  = name of diagnostic to increment the counter
            bi        = X-direction tile number, or 0 if called outside bi,bj loops
            bj        = Y-direction tile number, or 0 if called outside bi,bj loops
            myThid    = my thread Id number


The diagnostics are computed at various times and places within MITgcm.
Because MITgcm may employ a staggered grid, diagnostics may be computed
at grid box centers, corners, or edges, and at the middle or edge in the
vertical. Some diagnostics are scalars, while others are components of
vectors. An internal array is defined which contains information
concerning various grid attributes of each diagnostic. The :varlink:`gdiag` array
(in common block diagnostics in file :filelink:`DIAGNOSTICS.h <pkg/diagnostics/DIAGNOSTICS.h>`) is internally
defined as a character*16 variable, and is equivalenced to a
character*1 “parse” array in output in order to extract the
grid-attribute information. The :varlink:`gdiag` array is described in :numref:`diagnostic_parsing_array`.


.. table:: Diagnostic Parsing Array
   :name: diagnostic_parsing_array

   +-----------+-----------------------+-----------------------------------------------------+
   |  Array    | Value                 | Description                                         |
   +===========+=======================+=====================================================+
   | parse(1)  | :math:`\rightarrow` S | scalar diagnostic                                   |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` U | U-vector component diagnostic                       |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` V | V-vector component diagnostic                       |
   +-----------+-----------------------+-----------------------------------------------------+
   | parse(2)  | :math:`\rightarrow` U | C-grid U-point                                      |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` V | C-grid V-point                                      |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` M | C-grid mass point                                   |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` Z | C-grid vorticity (corner) point                     |
   +-----------+-----------------------+-----------------------------------------------------+
   | parse(3)  | :math:`\rightarrow`   | used for level-integrated output: cumulate levels   |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` r | same but cumulate product by model level thickness  |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` R | same but cumulate product by hFac & level thickness |
   +-----------+-----------------------+-----------------------------------------------------+
   | parse(4)  | :math:`\rightarrow` P | positive definite diagnostic                        |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` A | Adjoint variable diagnostic                         |
   +-----------+-----------------------+-----------------------------------------------------+
   | parse(5)  | :math:`\rightarrow` C | with counter array                                  |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` P | post-processed (not filled up) from other diags     |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` D | disable diagnostic for output                       |
   +-----------+-----------------------+-----------------------------------------------------+
   | parse(6-8)|                       | retired, formerly 3-digit mate number               |
   +-----------+-----------------------+-----------------------------------------------------+
   | parse(9)  | :math:`\rightarrow` U | model level + :math:`\frac{1}{2}`                   |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` M | model level middle                                  |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` L | model level - :math:`\frac{1}{2}`                   |
   +-----------+-----------------------+-----------------------------------------------------+
   | parse(10) | :math:`\rightarrow` 0 | levels = 0                                          |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` 1 | levels = 1                                          |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` R | levels = Nr                                         |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` L | levels = MAX(Nr,NrPhys)                             |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` M | levels = MAX(Nr,NrPhys) - 1                         |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` G | levels = ground_level number                        |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` I | levels = seaice_level number                        |
   +-----------+-----------------------+-----------------------------------------------------+
   |           | :math:`\rightarrow` X | free levels option (need to be set explicitly)      |
   +-----------+-----------------------+-----------------------------------------------------+



As an example, consider a diagnostic whose associated :varlink:`gdiag`
parameter is equal to ``UUR     MR``. From :varlink:`gdiag` we can determine
that this diagnostic is a U-vector component located at the C-grid U-point,
model mid-level (M) with Nr levels (last R).

In this way, each diagnostic in the model has its attributes (i.e., vector
or scalar, C-grid location, etc.) defined internally. The output
routines use this information in order to determine what type of
transformations need to be performed. Any interpolations are done at the
time of output rather than during each model step. In this way the user
has flexibility in determining the type of output gridded data.

.. _usage_notes:

Usage Notes
-----------

Using available diagnostics
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use the diagnostics package, other than enabling it in ``packages.conf``
and turning the :varlink:`useDiagnostics` flag in ``data.pkg`` to ``.TRUE.``, there are two
further steps the user must take to enable the diagnostics package for
output of quantities that are already defined in MITgcm under an
experiment’s configuration of packages. A parameter file
``data.diagnostics`` must be supplied in the run directory, and the file
:filelink:`DIAGNOSTICS_SIZE.h <pkg/diagnostics/DIAGNOSTICS_SIZE.h>`
must be included in the code directory. The steps
for defining a new (permanent or experiment-specific temporary)
diagnostic quantity will be outlined later.

The namelist in parameter file ``data.diagnostics`` will activate a
user-defined list of diagnostics quantities to be computed, specify the
frequency and type of output, the number of levels, and the name of all
the separate output files. A sample ``data.diagnostics`` namelist file:

::

    # Diagnostic Package Choices
    #--------------------
    #  dumpAtLast (logical): always write output at the end of simulation (default=F)
    #  diag_mnc   (logical): write to NetCDF files (default=useMNC)
    #--for each output-stream:
    #  fileName(n) : prefix of the output file name (max 80c long) for outp.stream n
    #  frequency(n):< 0 : write snap-shot output every |frequency| seconds
    #               > 0 : write time-average output every frequency seconds
    #  timePhase(n)     : write at time = timePhase + multiple of |frequency|
    #    averagingFreq  : frequency (in s) for periodic averaging interval
    #    averagingPhase : phase     (in s) for periodic averaging interval
    #    repeatCycle    : number of averaging intervals in 1 cycle
    #  levels(:,n) : list of levels to write to file (Notes: declared as REAL)
    #                when this entry is missing, select all common levels of this list
    #  fields(:,n) : list of selected diagnostics fields (8.c) in outp.stream n
    #                (see "available_diagnostics.log" file for the full list of diags)
    #  missing_value(n) : missing value for real-type fields in output file "n"
    #  fileFlags(n)     : specific code (8c string) for output file "n"
    #--------------------
     &DIAGNOSTICS_LIST
      fields(1:2,1) = 'UVEL    ','VVEL    ',
       levels(1:5,1) = 1.,2.,3.,4.,5.,
       fileName(1) = 'diagout1',
      frequency(1) = 86400.,
      fields(1:2,2) = 'THETA   ','SALT    ',
       fileName(2) = 'diagout2',
      fileFlags(2) = ' P      ',
      levels(1:5,2) = 100000.0, 70000.0, 50000.0, 30000.0, 20000.0,
      frequency(2) = 3600.,
     &

     &DIAG_STATIS_PARMS
     &

In this example, there are two output files that will be generated for
each tile and for each output time. The first set of output files has
the prefix ``diagout1``, does time averaging every 86400. seconds,
(frequency is 86400.), and will write fields which are multiple-level
fields at output levels 1-5. The names of diagnostics quantities are
``UVEL`` and ``VVEL``. The second set of output files has the prefix diagout2,
does time averaging every 3600. seconds and the names of diagnostics quantities
are ``THETA`` and ``SALT``.  It interpolates vertically to the pressure levels
100000 Pa, ..., 20000 Pa.

The :varlink:`fileFlags` parameter is explained in
:numref:`diagnostic_fileFlags`.  Only the first three characters matter.  The
first character determines the precision of the output files.  The default is
to use :varlink:`writeBinaryPrec`.  The second character determines whether the
fields are to be integrated or interpolated vertically or written as is (the
default).  Interpolation is only available in the atmosphere.  The desired
pressure levels need to be specified in ``levels``.  The third character is
used to time average the product of a diagnostic with the appropriate
thickness factor, hFacC, hFacW or hFacS.  This is mostly useful with a
non-linear free surface where the thickness factors vary in time.  This will
have an effect only for certain diagnostics, as determined by the parsing code
(see :numref:`diagnostic_parsing_array` and the file available_diagnostics.log
for a given setup):  parse(3) has to be ``'R'``, parse(5) blank and parse(9:10)
``'MR'``.  Vorticity-point diagnostics cannot be hFac weighted.  Note that the
appropriate hFac factors are automatically included when integrating vertically
(second character ``'I'``), so the 'h' is not needed in this case
but could still improve accuracy of a time-averaged vertical integral when using
non-linear free surface.

.. table:: Diagnostic fileFlags
   :name: diagnostic_fileflags

   +---------------+-------+----------------------------------------------+
   | Character pos | Value | Description                                  |
   +===============+=======+==============================================+
   | 1             | R     | precision: 32 bits                           |
   +---------------+-------+----------------------------------------------+
   |               | D     | precision: 64 bits                           |
   +---------------+-------+----------------------------------------------+
   |               |       | precision: writeBinaryPrec                   |
   +---------------+-------+----------------------------------------------+
   | 2             | I     | integrate vertically                         |
   +---------------+-------+----------------------------------------------+
   |               | P     | interpolate vertically                       |
   +---------------+-------+----------------------------------------------+
   |               |       | do not integrate or interpolate              |
   +---------------+-------+----------------------------------------------+
   | 3             | h     | multiply by hFac (if permitted) when filled  |
   +---------------+-------+----------------------------------------------+


The user must assure that enough computer memory is allocated for the
diagnostics and the output streams selected for a particular experiment.
This is accomplished by modifying the file
:filelink:`DIAGNOSTICS_SIZE.h <pkg/diagnostics/DIAGNOSTICS_SIZE.h>` and
including it in the experiment code directory. The parameters that
should be checked are called :varlink:`numDiags`, :varlink:`numLists`, :varlink:`numperList`, and
:varlink:`diagSt_size`.

:varlink:`numDiags` (and :varlink:`diagSt_size`):
All MITgcm diagnostic quantities are stored in the single diagnostic
array :varlink:`gdiag` which is located in the file and has the form:

::

          _RL  qdiag(1-Olx,sNx+Olx,1-Olx,sNx+Olx,numDiags,nSx,nSy)
          _RL  qSdiag(0:nStats,0:nRegions,diagSt_size,nSx,nSy)
          COMMON / DIAG_STORE_R / qdiag, qSdiag

The first two-dimensions of :varlink:`diagSt_size` correspond to the horizontal dimension
of a given diagnostic, and the third dimension of :varlink:`diagSt_size` is used to
identify diagnostic fields and levels combined. In order to minimize the
memory requirement of the model for diagnostics, the default MITgcm
executable is compiled with room for only one horizontal diagnostic
array, or with :varlink:`numDiags` set to Nr. In order for the user to enable more
than one 3-D diagnostic, the size of the diagnostics common
must be expanded to accommodate the desired diagnostics. This can be
accomplished by manually changing the parameter :varlink:`numDiags` in the file .
:varlink:`numDiags` should be set greater than or equal to the sum of all the
diagnostics activated for output each multiplied by the number of levels
defined for that diagnostic quantity. For the above example, there are four
multiple level fields, which the available diagnostics list (see below) indicates
are defined at the MITgcm vertical resolution, Nr. The value of :varlink:`numDiags` in
:filelink:`DIAGNOSTICS_SIZE.h <pkg/diagnostics/DIAGNOSTICS_SIZE.h>` would therefore be equal to 4*Nr, or, say 40 if
Nr=10.

:varlink:`numLists` and :varlink:`numperList`:
The parameter :varlink:`numLists` must be set greater than or equal to the number
of separate output streams that the user specifies in the namelist
file ``data.diagnostics``. The parameter :varlink:`numperList` corresponds to the
maximum number of diagnostics requested per output streams.


Adjoint variables
~~~~~~~~~~~~~~~~~

The diagnostics package can also be used to print adjoint state variables. Using the diagnostics package
as opposed to using the standard 'adjoint dump' options allows one to take advantage of all the
averaging and post processing routines available to other diagnostics variables.

Currently, the available adjoint state variables are:

::

   109 |ADJetan |  1 |       |SM A    M1|dJ/m            |dJ/dEtaN: Sensitivity to sea surface height anomaly
   110 |ADJuvel | 15 |   111 |UURA    MR|dJ/(m/s)        |dJ/dU: Sensitivity to zonal velocity
   111 |ADJvvel | 15 |   110 |VVRA    MR|dJ/(m/s)        |dJ/dV: Sensitivity to meridional velocity
   112 |ADJwvel | 15 |       |WM A    LR|dJ/(m/s)        |dJ/dW: Sensitivity to vertical velocity
   113 |ADJtheta| 15 |       |SMRA    MR|dJ/degC         |dJ/dTheta: Sensitivity to potential temperature
   114 |ADJsalt | 15 |       |SMRA    MR|dJ/(g/kg)       |dJ/dSalt: Sensitivity to salinity
   115 |ADJtaux |  1 |   116 |UU A    U1|dJ/(N/m^2)      |dJ/dTaux: Senstivity to zonal surface wind stress
   116 |ADJtauy |  1 |   115 |VV A    U1|dJ/(N/m^2)      |dJ/dTauy: Sensitivity to merid. surface wind stress
   117 |ADJempmr|  1 |       |SM A    U1|dJ/(kg/m^2/s)   |dJ/dEmPmR: Sensitivity to net surface freshwater flux
   118 |ADJqnet |  1 |       |SM A    U1|dJ/(W/m^2)      |dJ/dQnet: Sensitivity to net surface heat flux
   119 |ADJqsw  |  1 |       |SM A    U1|dJ/(W/m^2)      |dJ/dQsw: Sensitivitiy to net Short-Wave radiation
   120 |ADJsst  |  1 |       |SM A    M1|dJ/K            |dJ/dSST: Sensitivity to Sea Surface Temperature
   121 |ADJsss  |  1 |       |SM A    M1|dJ/(g/kg)       |dJ/dSSS: Sensitivity to Sea Surface Salinity
   122 |ADJbtdrg|  1 |       |SM A    M1|dJ/d()          |dJ/dCd: Sensitivity to bottom drag coefficient
   123 |ADJdifkr| 15 |       |SMRA    MR|dJ/d(m^2/s))    |dJ/dKr: Sensitivity to vertical diffusivity
   124 |ADJepsix| 15 |   125 |UURA    UR|dJ/(m^2/s)      |dJ/dEddyPsiX: Sensitivity to zonal eddystreamfunction
   125 |ADJepsiy| 15 |   124 |VVRA    UR|dJ/(m^2/s)      |dJ/dEddyPsiY: Sensitivity to meridional eddystreamfunction


Additionally the packages :ref:`gmredi <sub_phys_pkg_gmredi>`,
:ref:`ptracrs <sub_phys_pkg_ptracers>`, :ref:`exf <sub_phys_pkg_exf>`, and
:ref:`seaice <sub_phys_pkg_seaice>` have the following available adjoint diagnostics

::

   225 |ADJkapgm| 15 |       |SMRA    MR|dJ/d[m^2/s]     |dJ/dKgm: Sensitivity to GM Intensity
   226 |ADJkapre| 15 |       |SMRA    MR|dJ/d[m^2/s]     |dJ/dKredi: Sensitivity to Redi Coefficient

::

   227 |TRAC01  | 15 |       |SMR     MR|mol C/m         |Dissolved Inorganic Carbon concentration
   241 |ADJptr01| 15 |       |SMRA    MR|dJ/mol C/m      |sensitivity to Dissolved Inorganic Carbon concentration

::

   221 |ADJustrs|  1 |   222 |UU A    U1|dJ/(N/m^2)      |dJ/dustress: Senstivity to zonal surface wind stress
   222 |ADJvstrs|  1 |   221 |VV A    U1|dJ/(N/m^2)      |dJ/dvstrs: Sensitivity to merid. surface wind stress
   223 |ADJhflux|  1 |       |SM A    U1|dJ/(W/m^2)      |dJ/dhflux: Sensitivity to upward heat flux
   224 |ADJsflux|  1 |       |SM A    U1|dJ/(m/s)        |dJ/dhflux: Sensitivity to upward fresh water flux
   225 |ADJatemp|  1 |       |SM A    U1|dJ/K            |dJ/datemp: Sensitivity to atmos. surface temperature
   226 |ADJpreci|  1 |       |SM A    U1|dJ/(m/s)        |dJ/daqh: Sensitivity to precipitation
   227 |ADJroff |  1 |       |SM A    U1|dJ/(m/s)        |dJ/daqh: Sensitivity to river runoff
   228 |ADJswdn |  1 |       |SM A    U1|dJ/(W/m^2)      |dJ/dswdown: Sensitivity to downward SW radiation
   229 |ADJlwdn |  1 |       |SM A    U1|dJ/(W/m^2)      |dJ/dlwdown: Sensitivity to downward LW radiation
   230 |ADJuwind|  1 |       |UM A    U1|dJ/d(m/s)       |dJ/duwind: Senstivity to zonal 10-m wind speed
   231 |ADJvwind|  1 |       |VM A    U1|dJ/d(m/s)       |dJ/dvwind: Senstivity to meridional 10-m wind speed
   232 |ADJclsst|  1 |       |SM A    U1|dJ/K            |dJ/dclimsst: Sensitivity to restoring SST
   233 |ADJclsss|  1 |       |SM A    U1|dJ/(g/kg)       |dJ/dclimsss: Sensitivity to restoring SSS

::

   332 |ADJarea |  1 |       |SM A    M1|dJ/(m^2/m^2)    |dJ/darea: Sensitivity to seaice fractional ice-cover
   333 |ADJheff |  1 |       |SM A    M1|dJ/dm           |dJ/dheff: Sensitvity to seaice ice thickness
   334 |ADJhsnow|  1 |       |SM A    M1|dJ/dm           |dJ/dhsnow: Sensitivity to seaice snow thickness
   335 |ADJuice |  1 |   336 |UU A    M1|dJ/(m/s)        |dJ/duice: sensitivity to zonal ice velocity
   336 |ADJvice |  1 |   335 |VV A    M1|dJ/(m/s)        |dJ/dvice: sensitivity to meridional ice velocity

Some notes to the user
^^^^^^^^^^^^^^^^^^^^^^

1. This feature is currently untested with OpenAD.

2. This feature does not work with the divided adjoint.

3. The sensitivity to sea surface height `ADJetan` is technically one time step
   ahead of other adjoint diagnostics printed at the same time step number. To be
   concrete, if `ADJetan` is written via the diagnostics package at every
   iteration, `n`, then each field will technically correspond to the written
   iteration number, `n+1`. This is simply due to a techincality about when this
   variable is printed in relation to the adjoint pressure solve.

4. The diagStats options are not available for these variables.

5. Adjoint variables are recognized by checking the 10 character variable `diagCode`.
   To add a new adjoint variable, set the 4th position of `diagCode` to A
   (notice this is the case for the list of available adjoint variables).


Using pkg/diagnostics for adjoint variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. Make sure the following flag is defined in either
   :filelink:`AUTODIFF_OPTIONS.h <pkg/autodiff/AUTODIFF_OPTIONS.h>`
   or `ECCO_CPPOPTIONS.h` if that is being used.

::

    #define ALLOW_AUTODIFF_MONITOR

2. Be sure to increase `numlists` and `numDiags` appropriately in
   :filelink:`DIAGNOSTICS_SIZE.h <pkg/diagnostics/DIAGNOSTICS_SIZE.h>`.
   Safe values are e.g. 10-20 and 500-1000 respectively.

3. Specify desired variables in ``data.diagnostics``
   as any other variable, as in the following example or as in this
   :filelink:`data.diagnostics <verification/global_ocean.cs32x15/input_ad/data.diagnostics>`.
   Note however, adjoint and forward diagnostic variables cannot
   be in the same list. That is, a single `fields(:,:)` list
   cannot contain both adjoint and forward variables.

::

    &DIAGNOSTICS_LIST
    # ---
      fields(1:5,1) = 'ADJtheta','ADJsalt ',
                         'ADJuvel ','ADJvvel ','ADJwvel '
      filename(1) = 'diags/adjState_3d_snaps',
      frequency(1)=-86400.0,
      timePhase(1)=0.0,
    #---
      fields(1:5,2) = 'ADJtheta','ADJsalt ',
                         'ADJuvel ','ADJvvel ','ADJwvel '
      filename(2) = 'diags/adjState_3d_avg',
      frequency(2)= 86400.0,
    #---
    &

Note: the diagnostics package automatically provides a phase shift of :math:`frequency/2`,
so specify `timePhase = 0` to match output from `adjDumpFreq`.


Adding new diagnostics to the code
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to define and include as part of the diagnostic output any
field that is desired for a particular experiment, two steps must be
taken. The first is to enable the “User Diagnostic” in ``data.diagnostics``.
This is accomplished by adding one of the “User Diagnostic” field names
(see :ref:`available diagnostics <diagnostics_list>`):``UDIAG1`` through ``UDIAG10``,
for multi-level fields, or ``SDIAG1`` through
``SDIAG10`` for single level fields) to the ``data.diagnostics`` namelist in one
of the output streams.
The second step is to add a call to
:filelink:`diagnostics_fill.F <pkg/diagnostics/diagnostics_fill.F>` from the
subroutine in which the quantity desired for diagnostic output is
computed.

In order to add a new diagnostic to the permanent set of diagnostics
that the main model or any package contains as part of its diagnostics
menu, the subroutine :filelink:`diagnostics_addtolist.F <pkg/diagnostics/diagnostics_addtolist.F>`
should be called during the
initialization phase of the main model or package. For the main model,
the call should be made from subroutine
:filelink:`diagnostics_main_init.F <pkg/diagnostics/diagnostics_main_init.F>`, and for
a package, the call should probably be made from from inside the
particular package’s init\_fixed routine. A typical code sequence to set
the input arguments to :filelink:`diagnostics_addtolist.F <pkg/diagnostics/diagnostics_addtolist.F>` would look like:

::

          diagName  = 'RHOAnoma'
          diagTitle = 'Density Anomaly (=Rho-rhoConst)'
          diagUnits = 'kg/m^3          '
          diagCode  = 'SMR     MR      '
          CALL DIAGNOSTICS\_ADDTOLIST( diagNum,
         I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

If the new diagnostic quantity is associated with either a vector pair
or a diagnostic counter, the :varlink:`diagMate` argument must be provided with the
proper index corresponding to the “mate”. The output argument from
:filelink:`diagnostics_addtolist.F <pkg/diagnostics/diagnostics_addtolist.F>`
that is called :varlink:`diagNum` here contains a running
total of the number of diagnostics defined in the code up to any point
during the run. The sequence number for the next two diagnostics defined
(the two components of the vector pair, for instance) will be :varlink:`diagNum`\+1
and :varlink:`diagNum`\+2. The definition of the first component of the vector pair
must fill the “mate” segment of the :varlink:`diagCode` as diagnostic number
:varlink:`diagNum`\+2. Since the subroutine increments :varlink:`diagNum`, the definition of
the second component of the vector fills the “mate” part of :varlink:`diagCode`
with :varlink:`diagNum`. A code sequence for this case would look like:

::

          diagName  = 'UVEL    '
          diagTitle = 'Zonal Component of Velocity (m/s)'
          diagUnits = 'm/s             '
          diagCode  = 'UUR     MR      '
          diagMate  = diagNum + 2
          CALL DIAGNOSTICS_ADDTOLIST( diagNum,
         I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

          diagName  = 'VVEL    '
          diagTitle = 'Meridional Component of Velocity (m/s)'
          diagUnits = 'm/s             '
          diagCode  = 'VVR     MR      '
          diagMate  = diagNum
          CALL DIAGNOSTICS_ADDTOLIST( diagNum,
         I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )


.. _diagnostics_list:


MITgcm kernel available diagnostics list:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  ---------------------------------------------------------------
  <-Name->|<- code ->|<--  Units   -->|<- Tile (max=80c)
  ------------------------------------------------------------------------
  SDIAG1  |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #1
  SDIAG2  |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #2
  SDIAG3  |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #3
  SDIAG4  |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #4
  SDIAG5  |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #5
  SDIAG6  |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #6
  SDIAG7  |SU      L1|user-defined    |User-Defined U.pt Surface Diagnostic #7
  SDIAG8  |SV      L1|user-defined    |User-Defined V.pt Surface Diagnostic #8
  SDIAG9  |UU      L1|user-defined    |User-Defined U.vector Surface Diag.  #9
  SDIAG10 |VV      L1|user-defined    |User-Defined V.vector Surface Diag. #10
  UDIAG1  |SM      MR|user-defined    |User-Defined Model-Level Diagnostic  #1
  UDIAG2  |SM      MR|user-defined    |User-Defined Model-Level Diagnostic  #2
  UDIAG3  |SMR     MR|user-defined    |User-Defined Model-Level Diagnostic  #3
  UDIAG4  |SMR     MR|user-defined    |User-Defined Model-Level Diagnostic  #4
  UDIAG5  |SU      MR|user-defined    |User-Defined U.pt Model-Level Diag.  #5
  UDIAG6  |SV      MR|user-defined    |User-Defined V.pt Model-Level Diag.  #6
  UDIAG7  |UUR     MR|user-defined    |User-Defined U.vector Model-Lev Diag.#7
  UDIAG8  |VVR     MR|user-defined    |User-Defined V.vector Model-Lev Diag.#8
  UDIAG9  |SM      ML|user-defined    |User-Defined Phys-Level  Diagnostic  #9
  UDIAG10 |SM      ML|user-defined    |User-Defined Phys-Level  Diagnostic #10
  SDIAGC  |SM  C   L1|user-defined    |User-Defined Counted Surface Diagnostic
  SDIAGCC |SM      L1|count           |User-Defined Surface Diagnostic Counter
  ETAN    |SM      M1|m               |Surface Height Anomaly
  ETANSQ  |SM P    M1|m^2             |Square of Surface Height Anomaly
  DETADT2 |SM      M1|m^2/s^2         |Square of Surface Height Anomaly Tendency
  THETA   |SMR     MR|degC            |Potential Temperature
  SALT    |SMR     MR|g/kg            |Salinity
  RELHUM  |SMR     MR|percent         |Relative Humidity
  SALTanom|SMR     MR|g/kg            |Salt anomaly (=SALT-35; g/kg)
  UVEL    |UUR     MR|m/s             |Zonal Component of Velocity (m/s)
  VVEL    |VVR     MR|m/s             |Meridional Component of Velocity (m/s)
  WVEL    |WM      LR|m/s             |Vertical Component of Velocity (r_units/s)
  THETASQ |SMRP    MR|degC^2          |Square of Potential Temperature
  SALTSQ  |SMRP    MR|(g/kg)^2        |Square of Salinity
  SALTSQan|SMRP    MR|(g/kg)^2        |Square of Salt anomaly (=(SALT-35)^2 (g^2/kg^2)
  UVELSQ  |UURP    MR|m^2/s^2         |Square of Zonal Comp of Velocity (m^2/s^2)
  VVELSQ  |VVRP    MR|m^2/s^2         |Square of Meridional Comp of Velocity (m^2/s^2)
  WVELSQ  |WM P    LR|m^2/s^2         |Square of Vertical Comp of Velocity
  UE_VEL_C|UMR     MR|m/s             |Eastward Velocity (m/s) (cell center)
  VN_VEL_C|VMR     MR|m/s             |Northward Velocity (m/s) (cell center)
  UV_VEL_C|UMR     MR|m^2/s^2         |Product of horizontal Comp of velocity (cell center)
  UV_VEL_Z|UZR     MR|m^2/s^2         |Meridional Transport of Zonal Momentum (m^2/s^2)
  WU_VEL  |WU      LR|m.m/s^2         |Vertical Transport of Zonal Momentum
  WV_VEL  |WV      LR|m.m/s^2         |Vertical Transport of Meridional Momentum
  UVELMASS|UUr     MR|m/s             |Zonal Mass-Weighted Comp of Velocity (m/s)
  VVELMASS|VVr     MR|m/s             |Meridional Mass-Weighted Comp of Velocity (m/s)
  WVELMASS|WM      LR|m/s             |Vertical Mass-Weighted Comp of Velocity
  PhiVEL  |SMR P   MR|m^2/s           |Horizontal Velocity Potential (m^2/s)
  PsiVEL  |SZ  P   MR|m.m^2/s         |Horizontal Velocity Stream-Function
  UTHMASS |UUr     MR|degC.m/s        |Zonal Mass-Weight Transp of Pot Temp
  VTHMASS |VVr     MR|degC.m/s        |Meridional Mass-Weight Transp of Pot Temp
  WTHMASS |WM      LR|degC.m/s        |Vertical Mass-Weight Transp of Pot Temp (K.m/s)
  USLTMASS|UUr     MR|g/kg.m/s        |Zonal Mass-Weight Transp of Salinity
  VSLTMASS|VVr     MR|g/kg.m/s        |Meridional Mass-Weight Transp of Salinity
  WSLTMASS|WM      LR|g/kg.m/s        |Vertical Mass-Weight Transp of Salinity
  UVELTH  |UUR     MR|degC.m/s        |Zonal Transport of Pot Temp
  VVELTH  |VVR     MR|degC.m/s        |Meridional Transport of Pot Temp
  WVELTH  |WM      LR|degC.m/s        |Vertical Transport of Pot Temp
  UVELSLT |UUR     MR|g/kg.m/s        |Zonal Transport of Salinity
  VVELSLT |VVR     MR|g/kg.m/s        |Meridional Transport of Salinity
  WVELSLT |WM      LR|g/kg.m/s        |Vertical Transport of Salinity
  UVELPHI |UUr     MR|m^3/s^3         |Zonal Mass-Weight Transp of Pressure Pot.(p/rho) Anomaly
  VVELPHI |VVr     MR|m^3/s^3         |Merid. Mass-Weight Transp of Pressure Pot.(p/rho) Anomaly
  RHOAnoma|SMR     MR|kg/m^3          |Density Anomaly (=Rho-rhoConst)
  RHOANOSQ|SMRP    MR|kg^2/m^6        |Square of Density Anomaly (=(Rho-rhoConst)^2)
  URHOMASS|UUr     MR|kg/m^2/s        |Zonal Transport of Density
  VRHOMASS|VVr     MR|kg/m^2/s        |Meridional Transport of Density
  WRHOMASS|WM      LR|kg/m^2/s        |Vertical Transport of Density
  WdRHO_P |WM      LR|kg/m^2/s        |Vertical velocity times delta^k(Rho)_at-const-P
  WdRHOdP |WM      LR|kg/m^2/s        |Vertical velocity times delta^k(Rho)_at-const-T,S
  PHIHYD  |SMR     MR|m^2/s^2         |Hydrostatic Pressure Pot.(p/rho) Anomaly
  PHIHYDSQ|SMRP    MR|m^4/s^4         |Square of Hyd. Pressure Pot.(p/rho) Anomaly
  PHIBOT  |SM      M1|m^2/s^2         |Bottom Pressure Pot.(p/rho) Anomaly
  PHIBOTSQ|SM P    M1|m^4/s^4         |Square of Bottom Pressure Pot.(p/rho) Anomaly
  PHI_SURF|SM      M1|m^2/s^2         |Surface Dynamical Pressure Pot.(p/rho)
  PHIHYDcR|SMR     MR|m^2/s^2         |Hydrostatic Pressure Pot.(p/rho) Anomaly @ const r
  PHI_NH  |SMR     MR|m^2/s^2         |Non-Hydrostatic Pressure Pot.(p/rho)
  MXLDEPTH|SM      M1|m               |Mixed-Layer Depth (>0)
  DRHODR  |SM      LR|kg/m^4          |Stratification: d.Sigma/dr (kg/m3/r_unit)
  CONVADJ |SMR     LR|fraction        |Convective Adjustment Index [0-1]
  oceTAUX |UU      U1|N/m^2           |zonal surface wind stress, >0 increases uVel
  oceTAUY |VV      U1|N/m^2           |meridional surf. wind stress, >0 increases vVel
  atmPload|SM      U1|Pa              |Atmospheric pressure loading anomaly (vs surf_pRef)
  sIceLoad|SM      U1|kg/m^2          |sea-ice loading (in Mass of ice+snow / area unit)
  oceFWflx|SM      U1|kg/m^2/s        |net surface Fresh-Water flux into the ocean (+=down), >0 decreases salinity
  oceSflux|SM      U1|g/m^2/s         |net surface Salt flux into the ocean (+=down), >0 increases salinity
  oceQnet |SM      U1|W/m^2           |net surface heat flux into the ocean (+=down), >0 increases theta
  oceQsw  |SM      U1|W/m^2           |net Short-Wave radiation (+=down), >0 increases theta
  oceFreez|SM      U1|W/m^2           |heating from freezing of sea-water (allowFreezing=T)
  TRELAX  |SM      U1|W/m^2           |surface temperature relaxation, >0 increases theta
  SRELAX  |SM      U1|g/m^2/s         |surface salinity relaxation, >0 increases salt
  surForcT|SM      U1|W/m^2           |model surface forcing for Temperature, >0 increases theta
  surForcS|SM      U1|g/m^2/s         |model surface forcing for Salinity, >0 increases salinity
  TFLUX   |SM      U1|W/m^2           |total heat flux (match heat-content variations), >0 increases theta
  SFLUX   |SM      U1|g/m^2/s         |total salt flux (match salt-content variations), >0 increases salt
  RCENTER |SM      MR|m               |Cell-Center Height
  RSURF   |SM      M1|m               |Surface Height
  hFactorC|SMr     MR|1               |Center cell-thickness fraction [-]
  hFactorW|SUr     MR|1               |Western-Edge cell-thickness fraction [-]
  hFactorS|SVr     MR|1               |Southern-Edge cell-thickness fraction [-]
  TOTUTEND|UUR     MR|m/s/day         |Tendency of Zonal Component of Velocity
  TOTVTEND|VVR     MR|m/s/day         |Tendency of Meridional Component of Velocity
  TOTTTEND|SMR     MR|degC/day        |Tendency of Potential Temperature
  TOTSTEND|SMR     MR|g/kg/day        |Tendency of Salinity
  ---------------------------------------------------------------
  <-Name->|<- code ->|<--  Units   -->|<- Tile (max=80c)
  ---------------------------------------------------------------
  MoistCor|SM      MR|W/m^2           |Heating correction due to moist thermodynamics
  HeatDiss|SM      MR|W/m^2           |Heating from frictional dissipation
  gT_Forc |SMR     MR|degC/s          |Potential Temp. forcing tendency
  gS_Forc |SMR     MR|g/kg/s          |Salinity forcing tendency
  AB_gT   |SMR     MR|degC/s          |Potential Temp. tendency from Adams-Bashforth
  AB_gS   |SMR     MR|g/kg/s          |Salinity tendency from Adams-Bashforth
  gTinAB  |SMR     MR|degC/s          |Potential Temp. tendency going in Adams-Bashforth
  gSinAB  |SMR     MR|g/kg/s          |Salinity tendency going in Adams-Bashforth
  AB_gU   |UUR     MR|m/s^2           |U momentum tendency from Adams-Bashforth
  AB_gV   |VVR     MR|m/s^2           |V momentum tendency from Adams-Bashforth
  AB_gW   |WM      LR|m/s^2           |W momentum tendency from Adams-Bashforth
  TAUXEDDY|UU      LR|N/m^2           |Zonal Eddy Stress
  TAUYEDDY|VV      LR|N/m^2           |Meridional Eddy Stress
  U_EulerM|UUR     MR|m/s             |Zonal Eulerian-Mean Velocity (m/s)
  V_EulerM|VVR     MR|m/s             |Meridional Eulerian-Mean Velocity (m/s)
  ADVr_TH |WM      LR|degC.m^3/s      |Vertical   Advective Flux of Pot.Temperature
  ADVx_TH |UU      MR|degC.m^3/s      |Zonal      Advective Flux of Pot.Temperature
  ADVy_TH |VV      MR|degC.m^3/s      |Meridional Advective Flux of Pot.Temperature
  DFrE_TH |WM      LR|degC.m^3/s      |Vertical Diffusive Flux of Pot.Temperature (Explicit part)
  DFxE_TH |UU      MR|degC.m^3/s      |Zonal      Diffusive Flux of Pot.Temperature
  DFyE_TH |VV      MR|degC.m^3/s      |Meridional Diffusive Flux of Pot.Temperature
  DFrI_TH |WM      LR|degC.m^3/s      |Vertical Diffusive Flux of Pot.Temperature (Implicit part)
  SM_x_TH |UM      MR|degC            |Pot.Temp.   1rst Order Moment Sx
  SM_y_TH |VM      MR|degC            |Pot.Temp.   1rst Order Moment Sy
  SM_z_TH |SM      MR|degC            |Pot.Temp.   1rst Order Moment Sz
  SMxx_TH |UM      MR|degC            |Pot.Temp.   2nd Order Moment Sxx
  SMyy_TH |VM      MR|degC            |Pot.Temp.   2nd Order Moment Syy
  SMzz_TH |SM      MR|degC            |Pot.Temp.   2nd Order Moment Szz
  SMxy_TH |SM      MR|degC            |Pot.Temp.   2nd Order Moment Sxy
  SMxz_TH |UM      MR|degC            |Pot.Temp.   2nd Order Moment Sxz
  SMyz_TH |VM      MR|degC            |Pot.Temp.   2nd Order Moment Syz
  SM_v_TH |SM P    MR|(degC)^2        |Pot.Temp.   sub-grid variance
  ADVr_SLT|WM      LR|g/kg.m^3/s      |Vertical   Advective Flux of Salinity
  ADVx_SLT|UU      MR|g/kg.m^3/s      |Zonal      Advective Flux of Salinity
  ADVy_SLT|VV      MR|g/kg.m^3/s      |Meridional Advective Flux of Salinity
  DFrE_SLT|WM      LR|g/kg.m^3/s      |Vertical Diffusive Flux of Salinity    (Explicit part)
  DFxE_SLT|UU      MR|g/kg.m^3/s      |Zonal      Diffusive Flux of Salinity
  DFyE_SLT|VV      MR|g/kg.m^3/s      |Meridional Diffusive Flux of Salinity
  DFrI_SLT|WM      LR|g/kg.m^3/s      |Vertical Diffusive Flux of Salinity    (Implicit part)
  SALTFILL|SM      MR|g/kg.m^3/s      |Filling of Negative Values of Salinity
  SM_x_SLT|UM      MR|g/kg            |Salinity    1rst Order Moment Sx
  SM_y_SLT|VM      MR|g/kg            |Salinity    1rst Order Moment Sy
  SM_z_SLT|SM      MR|g/kg            |Salinity    1rst Order Moment Sz
  SMxx_SLT|UM      MR|g/kg            |Salinity    2nd Order Moment Sxx
  SMyy_SLT|VM      MR|g/kg            |Salinity    2nd Order Moment Syy
  SMzz_SLT|SM      MR|g/kg            |Salinity    2nd Order Moment Szz
  SMxy_SLT|SM      MR|g/kg            |Salinity    2nd Order Moment Sxy
  SMxz_SLT|UM      MR|g/kg            |Salinity    2nd Order Moment Sxz
  SMyz_SLT|VM      MR|g/kg            |Salinity    2nd Order Moment Syz
  SM_v_SLT|SM P    MR|(g/kg)^2        |Salinity    sub-grid variance
  VISCAHZ |SZ      MR|m^2/s           |Harmonic Visc Coefficient (m2/s) (Zeta Pt)
  VISCA4Z |SZ      MR|m^4/s           |Biharmonic Visc Coefficient (m4/s) (Zeta Pt)
  VISCAHD |SM      MR|m^2/s           |Harmonic Viscosity Coefficient (m2/s) (Div Pt)
  VISCA4D |SM      MR|m^4/s           |Biharmonic Viscosity Coefficient (m4/s) (Div Pt)
  VISCAHW |WM      LR|m^2/s           |Harmonic Viscosity Coefficient (m2/s) (W Pt)
  VISCA4W |WM      LR|m^4/s           |Biharmonic Viscosity Coefficient (m4/s) (W Pt)
  VAHZMAX |SZ      MR|m^2/s           |CFL-MAX Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZMAX |SZ      MR|m^4/s           |CFL-MAX Biharm Visc Coefficient (m4/s) (Zeta Pt)
  VAHDMAX |SM      MR|m^2/s           |CFL-MAX Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DMAX |SM      MR|m^4/s           |CFL-MAX Biharm Visc Coefficient (m4/s) (Div Pt)
  VAHZMIN |SZ      MR|m^2/s           |RE-MIN Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZMIN |SZ      MR|m^4/s           |RE-MIN Biharm Visc Coefficient (m4/s) (Zeta Pt)
  VAHDMIN |SM      MR|m^2/s           |RE-MIN Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DMIN |SM      MR|m^4/s           |RE-MIN Biharm Visc Coefficient (m4/s) (Div Pt)
  VAHZLTH |SZ      MR|m^2/s           |Leith Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZLTH |SZ      MR|m^4/s           |Leith Biharm Visc Coefficient (m4/s) (Zeta Pt)
  VAHDLTH |SM      MR|m^2/s           |Leith Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DLTH |SM      MR|m^4/s           |Leith Biharm Visc Coefficient (m4/s) (Div Pt)
  VAHZLTHD|SZ      MR|m^2/s           |LeithD Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZLTHD|SZ      MR|m^4/s           |LeithD Biharm Visc Coefficient (m4/s) (Zeta Pt)
  VAHDLTHD|SM      MR|m^2/s           |LeithD Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DLTHD|SM      MR|m^4/s           |LeithD Biharm Visc Coefficient (m4/s) (Div Pt)
  VAHZLTHQ|SZ      MR|m^2/s           |LeithQG Harm Visc Coefficient (m2/s) (Zeta Pt)
  VAHDLTHQ|SM      MR|m^2/s           |LeithQG Harm Visc Coefficient (m2/s) (Div Pt)
  VAHZSMAG|SZ      MR|m^2/s           |Smagorinsky Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZSMAG|SZ      MR|m^4/s           |Smagorinsky Biharm Visc Coeff. (m4/s) (Zeta Pt)
  VAHDSMAG|SM      MR|m^2/s           |Smagorinsky Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DSMAG|SM      MR|m^4/s           |Smagorinsky Biharm Visc Coeff. (m4/s) (Div Pt)
  momKE   |SMR     MR|m^2/s^2         |Kinetic Energy (in momentum Eq.)
  momHDiv |SMR     MR|s^-1            |Horizontal Divergence (in momentum Eq.)
  momVort3|SZR     MR|s^-1            |3rd component (vertical) of Vorticity
  Strain  |SZR     MR|s^-1            |Horizontal Strain of Horizontal Velocities
  Tension |SMR     MR|s^-1            |Horizontal Tension of Horizontal Velocities
  Stretch |SM      MR|s^-1            |Vortex stretching from QG Leith dynamic viscosity
  USidDrag|UUR     MR|m/s^2           |U momentum tendency from Side Drag
  VSidDrag|VVR     MR|m/s^2           |V momentum tendency from Side Drag
  Um_Diss |UUR     MR|m/s^2           |U momentum tendency from Dissipation (Explicit part)
  Vm_Diss |VVR     MR|m/s^2           |V momentum tendency from Dissipation (Explicit part)
  Um_ImplD|UUR     MR|m/s^2           |U momentum tendency from Dissipation (Implicit part)
  Vm_ImplD|VVR     MR|m/s^2           |V momentum tendency from Dissipation (Implicit part)
  Um_Advec|UUR     MR|m/s^2           |U momentum tendency from Advection terms
  Vm_Advec|VVR     MR|m/s^2           |V momentum tendency from Advection terms
  Um_Cori |UUR     MR|m/s^2           |U momentum tendency from Coriolis term
  Vm_Cori |VVR     MR|m/s^2           |V momentum tendency from Coriolis term
  Um_dPhiX|UUR     MR|m/s^2           |U momentum tendency from Pressure/Potential grad
  Vm_dPhiY|VVR     MR|m/s^2           |V momentum tendency from Pressure/Potential grad
  Um_Ext  |UUR     MR|m/s^2           |U momentum tendency from external forcing
  Vm_Ext  |VVR     MR|m/s^2           |V momentum tendency from external forcing
  Um_AdvZ3|UUR     MR|m/s^2           |U momentum tendency from Vorticity Advection
  Vm_AdvZ3|VVR     MR|m/s^2           |V momentum tendency from Vorticity Advection
  Um_AdvRe|UUR     MR|m/s^2           |U momentum tendency from vertical Advection (Explicit part)
  Vm_AdvRe|VVR     MR|m/s^2           |V momentum tendency from vertical Advection (Explicit part)
  Wm_Diss |WMr     LR|m/s^2           |W momentum tendency from Dissipation
  Wm_Advec|WMr     LR|m/s^2           |W momentum tendency from Advection terms
  WSidDrag|WMr     LR|m/s^2           |Vertical momentum tendency from Side Drag
  botTauX |UU      U1|N/m^2           |zonal bottom stress, >0 increases uVel
  botTauY |VV      U1|N/m^2           |meridional bottom stress, >0 increases vVel
  ADVx_Um |UM      MR|m^4/s^2         |Zonal      Advective Flux of U momentum
  ADVy_Um |VZ      MR|m^4/s^2         |Meridional Advective Flux of U momentum
  ADVrE_Um|WU      LR|m^4/s^2         |Vertical   Advective Flux of U momentum (Explicit part)
  ADVx_Vm |UZ      MR|m^4/s^2         |Zonal      Advective Flux of V momentum
  ADVy_Vm |VM      MR|m^4/s^2         |Meridional Advective Flux of V momentum
  ADVrE_Vm|WV      LR|m^4/s^2         |Vertical   Advective Flux of V momentum (Explicit part)
  VISCx_Um|UM      MR|m^4/s^2         |Zonal      Viscous Flux of U momentum
  VISCy_Um|VZ      MR|m^4/s^2         |Meridional Viscous Flux of U momentum
  VISrE_Um|WU      LR|m^4/s^2         |Vertical   Viscous Flux of U momentum (Explicit part)
  VISrI_Um|WU      LR|m^4/s^2         |Vertical   Viscous Flux of U momentum (Implicit part)
  VISCx_Vm|UZ      MR|m^4/s^2         |Zonal      Viscous Flux of V momentum
  VISCy_Vm|VM      MR|m^4/s^2         |Meridional Viscous Flux of V momentum
  VISrE_Vm|WV      LR|m^4/s^2         |Vertical   Viscous Flux of V momentum (Explicit part)
  VISrI_Vm|WV      LR|m^4/s^2         |Vertical   Viscous Flux of V momentum (Implicit part)

The meaning of the “code” column is explained in
:numref:`diagnostic_parsing_array`. The last character of the code, in particular,
determines the number of vertical levels in the diagnostic (of the commonly used codes,
"1" represents a 2-D diagnostic, "R" and "L" are multi-level diagnostics).

MITgcm packages: available diagnostics lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a list of the diagnostic fields available in the different MITgcm
packages, follow the link to the available diagnostics listing in the manual section
describing the package:

-  :filelink:`pkg/aim_v23`: :ref:`available diagnostics <aim_diagnostics>`

-  :filelink:`pkg/exf`: :ref:`available diagnostics <ssub_phys_pkg_exf_diagnostics>`

-  :filelink:`pkg/gchem`: :ref:`available diagnostics <gchem_diagnostics>`

-  :filelink:`pkg/generic_advdiff`: :ref:`available diagnostics <gad_diagnostics>`

-  :filelink:`pkg/gridalt`: :ref:`available diagnostics <gridalt_diagnostics>`

-  :filelink:`pkg/gmredi`: :ref:`available diagnostics <ssub_phys_pkg_gmredi_diagnostics>`

-  :filelink:`pkg/fizhi`: :ref:`available diagnostics <fizhi_diagnostics>`

-  :filelink:`pkg/kpp`: :ref:`available diagnostics <ssub_phys_pkg_kpp_diagnostics>`

-  :filelink:`pkg/land`: :ref:`available diagnostics <land_diagnostics>`

-  :filelink:`pkg/mom_common`: :ref:`available diagnostics <mom_diagnostics>`

-  :filelink:`pkg/obcs`: :ref:`available diagnostics <ssub_phys_pkg_obcs_diagnostics>`

-  :filelink:`pkg/thsice`: :ref:`available diagnostics <thsice_diagnostics>`

-  :filelink:`pkg/seaice`: :ref:`available diagnostics <ssub_phys_pkg_seaice_diagnostics>`

-  :filelink:`pkg/shap_filt`: :ref:`available diagnostics <shapiro_diagnostics>`

-  :filelink:`pkg/ptracers`: :ref:`available diagnostics <ptracers_diagnostics>`

