
.. _outp_pack:

Packages II - Diagnostics and I/O
*********************************

MITgcm includes several packages related to input and output during a
model integration. The packages described in this chapter are related to
the choice of input/output fields and their on-disk format.

.. _sub_outp_pkg_diagnostics:

pkg/diagnostics – A Flexible Infrastructure
===========================================

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
  


As an example, consider a diagnostic whose associated :varlink:`gdiag` parameter is
equal to “UURMR”. From :varlink:`gdiag` we can determine that this diagnostic is a
U-vector component located at the C-grid U-point, model mid-level (M)
with Nr levels (last R).

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
       filename(1) = 'diagout1',
      frequency(1) = 86400.,
      fields(1:2,2) = 'THETA   ','SALT    ',
       filename(2) = 'diagout2',
      fileflags(2) = ' P1     ',
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
does time averaging every 3600. seconds, includes fields with all
levels, and the names of diagnostics quantities are ``THETA`` and ``SALT``.

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

   110 |ADJetan |  1 |       |SM A    M1|dJ/m            |dJ/dEtaN: Sensitivity to sea surface height anomaly
   111 |ADJuvel | 50 |   112 |UURA    MR|dJ/(m/s)        |dJ/dU: Sensitivity to zonal velocity
   112 |ADJvvel | 50 |   111 |VVRA    MR|dJ/(m/s)        |dJ/dV: Sensitivity to meridional velocity
   113 |ADJwvel | 50 |       |WM A    LR|dJ/(m/s)        |dJ/dW: Sensitivity to vertical velocity
   114 |ADJtheta| 50 |       |SMRA    MR|dJ/degC         |dJ/dTheta: Sensitivity to potential temperature
   115 |ADJsalt | 50 |       |SMRA    MR|dJ/psu          |dJ/dSalt: Sensitivity to salinity

Some notes to the user
^^^^^^^^^^^^^^^^^^^^^^

1. This feature is currently untested with OpenAD.

2. This feature does not work with the divided adjoint.

3. `adEtaN` is broken in :filelink:`addummy_in_stepping.F <pkg/autodiff/addummy_in_stepping.F>`
   so the output through diagnostics is zeros just as with the standard 'adjoint dump' method.

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

  ----------------------------------------------------------------------------
  <-Name->|Levs|  mate |<- code ->|<--  Units   -->|<- Tile (max=80c)         
  -------------------------------------------------------------------------------------
  SDIAG1  |  1 |       |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #1
  SDIAG2  |  1 |       |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #2
  SDIAG3  |  1 |       |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #3
  SDIAG4  |  1 |       |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #4
  SDIAG5  |  1 |       |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #5
  SDIAG6  |  1 |       |SM      L1|user-defined    |User-Defined   Surface   Diagnostic  #6
  SDIAG7  |  1 |       |SU      L1|user-defined    |User-Defined U.pt Surface Diagnostic #7
  SDIAG8  |  1 |       |SV      L1|user-defined    |User-Defined V.pt Surface Diagnostic #8
  SDIAG9  |  1 |    10 |UU      L1|user-defined    |User-Defined U.vector Surface Diag.  #9
  SDIAG10 |  1 |     9 |VV      L1|user-defined    |User-Defined V.vector Surface Diag. #10
  UDIAG1  | 50 |       |SM      MR|user-defined    |User-Defined Model-Level Diagnostic  #1
  UDIAG2  | 50 |       |SM      MR|user-defined    |User-Defined Model-Level Diagnostic  #2
  UDIAG3  | 50 |       |SMR     MR|user-defined    |User-Defined Model-Level Diagnostic  #3
  UDIAG4  | 50 |       |SMR     MR|user-defined    |User-Defined Model-Level Diagnostic  #4
  UDIAG5  | 50 |       |SU      MR|user-defined    |User-Defined U.pt Model-Level Diag.  #5
  UDIAG6  | 50 |       |SV      MR|user-defined    |User-Defined V.pt Model-Level Diag.  #6
  UDIAG7  | 50 |    18 |UUR     MR|user-defined    |User-Defined U.vector Model-Lev Diag.#7
  UDIAG8  | 50 |    17 |VVR     MR|user-defined    |User-Defined V.vector Model-Lev Diag.#8
  UDIAG9  | 50 |       |SM      ML|user-defined    |User-Defined Phys-Level  Diagnostic  #9
  UDIAG10 | 50 |       |SM      ML|user-defined    |User-Defined Phys-Level  Diagnostic #10
  SDIAGC  |  1 |    22 |SM  C   L1|user-defined    |User-Defined Counted Surface Diagnostic
  SDIAGCC |  1 |       |SM      L1|count           |User-Defined Surface Diagnostic Counter
  ETAN    |  1 |       |SM      M1|m               |Surface Height Anomaly
  ETANSQ  |  1 |       |SM P    M1|m^2             |Square of Surface Height Anomaly
  DETADT2 |  1 |       |SM      M1|m^2/s^2         |Square of Surface Height Anomaly Tendency
  THETA   | 50 |       |SMR     MR|degC            |Potential Temperature
  SALT    | 50 |       |SMR     MR|psu             |Salinity
  RELHUM  | 50 |       |SMR     MR|percent         |Relative Humidity
  SALTanom| 50 |       |SMR     MR|psu             |Salt anomaly (=SALT-35; g/kg)
  UVEL    | 50 |    31 |UUR     MR|m/s             |Zonal Component of Velocity (m/s)
  VVEL    | 50 |    30 |VVR     MR|m/s             |Meridional Component of Velocity (m/s)
  WVEL    | 50 |       |WM      LR|m/s             |Vertical Component of Velocity (r_units/s)
  THETASQ | 50 |       |SMRP    MR|degC^2          |Square of Potential Temperature
  SALTSQ  | 50 |       |SMRP    MR|(psu)^2         |Square of Salinity
  SALTSQan| 50 |       |SMRP    MR|(psu)^2         |Square of Salt anomaly (=(SALT-35)^2 (g^2/kg^2)
  UVELSQ  | 50 |    37 |UURP    MR|m^2/s^2         |Square of Zonal Comp of Velocity (m^2/s^2)
  VVELSQ  | 50 |    36 |VVRP    MR|m^2/s^2         |Square of Meridional Comp of Velocity (m^2/s^2)
  WVELSQ  | 50 |       |WM P    LR|m^2/s^2         |Square of Vertical Comp of Velocity
  UE_VEL_C| 50 |    40 |UMR     MR|m/s             |Eastward Velocity (m/s) (cell center)
  VN_VEL_C| 50 |    39 |VMR     MR|m/s             |Northward Velocity (m/s) (cell center)
  UV_VEL_C| 50 |    41 |UMR     MR|m^2/s^2         |Product of horizontal Comp of velocity (cell center)
  UV_VEL_Z| 50 |    42 |UZR     MR|m^2/s^2         |Meridional Transport of Zonal Momentum (m^2/s^2)
  WU_VEL  | 50 |       |WU      LR|m.m/s^2         |Vertical Transport of Zonal Momentum
  WV_VEL  | 50 |       |WV      LR|m.m/s^2         |Vertical Transport of Meridional Momentum
  UVELMASS| 50 |    46 |UUr     MR|m/s             |Zonal Mass-Weighted Comp of Velocity (m/s)
  VVELMASS| 50 |    45 |VVr     MR|m/s             |Meridional Mass-Weighted Comp of Velocity (m/s)
  WVELMASS| 50 |       |WM      LR|m/s             |Vertical Mass-Weighted Comp of Velocity
  PhiVEL  | 50 |    45 |SMR P   MR|m^2/s           |Horizontal Velocity Potential (m^2/s)
  PsiVEL  | 50 |    48 |SZ  P   MR|m.m^2/s         |Horizontal Velocity Stream-Function
  UTHMASS | 50 |    51 |UUr     MR|degC.m/s        |Zonal Mass-Weight Transp of Pot Temp
  VTHMASS | 50 |    50 |VVr     MR|degC.m/s        |Meridional Mass-Weight Transp of Pot Temp
  WTHMASS | 50 |       |WM      LR|degC.m/s        |Vertical Mass-Weight Transp of Pot Temp (K.m/s)
  USLTMASS| 50 |    54 |UUr     MR|psu.m/s         |Zonal Mass-Weight Transp of Salinity
  VSLTMASS| 50 |    53 |VVr     MR|psu.m/s         |Meridional Mass-Weight Transp of Salinity
  WSLTMASS| 50 |       |WM      LR|psu.m/s         |Vertical Mass-Weight Transp of Salinity
  UVELTH  | 50 |    57 |UUR     MR|degC.m/s        |Zonal Transport of Pot Temp
  VVELTH  | 50 |    56 |VVR     MR|degC.m/s        |Meridional Transport of Pot Temp
  WVELTH  | 50 |       |WM      LR|degC.m/s        |Vertical Transport of Pot Temp
  UVELSLT | 50 |    60 |UUR     MR|psu.m/s         |Zonal Transport of Salinity
  VVELSLT | 50 |    59 |VVR     MR|psu.m/s         |Meridional Transport of Salinity
  WVELSLT | 50 |       |WM      LR|psu.m/s         |Vertical Transport of Salinity
  UVELPHI | 50 |    63 |UUr     MR|m^3/s^3         |Zonal Mass-Weight Transp of Pressure Pot.(p/rho) Anomaly
  VVELPHI | 50 |    62 |VVr     MR|m^3/s^3         |Merid. Mass-Weight Transp of Pressure Pot.(p/rho) Anomaly
  RHOAnoma| 50 |       |SMR     MR|kg/m^3          |Density Anomaly (=Rho-rhoConst)
  RHOANOSQ| 50 |       |SMRP    MR|kg^2/m^6        |Square of Density Anomaly (=(Rho-rhoConst)^2)
  URHOMASS| 50 |    67 |UUr     MR|kg/m^2/s        |Zonal Transport of Density
  VRHOMASS| 50 |    66 |VVr     MR|kg/m^2/s        |Meridional Transport of Density
  WRHOMASS| 50 |       |WM      LR|kg/m^2/s        |Vertical Transport of Density
  WdRHO_P | 50 |       |WM      LR|kg/m^2/s        |Vertical velocity times delta^k(Rho)_at-const-P
  WdRHOdP | 50 |       |WM      LR|kg/m^2/s        |Vertical velocity times delta^k(Rho)_at-const-T,S
  PHIHYD  | 50 |       |SMR     MR|m^2/s^2         |Hydrostatic Pressure Pot.(p/rho) Anomaly
  PHIHYDSQ| 50 |       |SMRP    MR|m^4/s^4         |Square of Hyd. Pressure Pot.(p/rho) Anomaly
  PHIBOT  |  1 |       |SM      M1|m^2/s^2         |Bottom Pressure Pot.(p/rho) Anomaly
  PHIBOTSQ|  1 |       |SM P    M1|m^4/s^4         |Square of Bottom Pressure Pot.(p/rho) Anomaly
  PHIHYDcR| 50 |       |SMR     MR|m^2/s^2         |Hydrostatic Pressure Pot.(p/rho) Anomaly @ const r
  MXLDEPTH|  1 |       |SM      M1|m               |Mixed-Layer Depth (>0)
  DRHODR  | 50 |       |SM      LR|kg/m^4          |Stratification: d.Sigma/dr (kg/m3/r_unit)
  CONVADJ | 50 |       |SMR     LR|fraction        |Convective Adjustment Index [0-1]
  oceTAUX |  1 |    80 |UU      U1|N/m^2           |zonal surface wind stress, >0 increases uVel
  oceTAUY |  1 |    79 |VV      U1|N/m^2           |meridional surf. wind stress, >0 increases vVel
  atmPload|  1 |       |SM      U1|Pa              |Atmospheric pressure loading
  sIceLoad|  1 |       |SM      U1|kg/m^2          |sea-ice loading (in Mass of ice+snow / area unit)
  oceFWflx|  1 |       |SM      U1|kg/m^2/s        |net surface Fresh-Water flux into the ocean (+=down), >0 decreases salinity
  oceSflux|  1 |       |SM      U1|g/m^2/s         |net surface Salt flux into the ocean (+=down), >0 increases salinity
  oceQnet |  1 |       |SM      U1|W/m^2           |net surface heat flux into the ocean (+=down), >0 increases theta
  oceQsw  |  1 |       |SM      U1|W/m^2           |net Short-Wave radiation (+=down), >0 increases theta
  oceFreez|  1 |       |SM      U1|W/m^2           |heating from freezing of sea-water (allowFreezing=T)
  TRELAX  |  1 |       |SM      U1|W/m^2           |surface temperature relaxation, >0 increases theta
  SRELAX  |  1 |       |SM      U1|g/m^2/s         |surface salinity relaxation, >0 increases salt
  surForcT|  1 |       |SM      U1|W/m^2           |model surface forcing for Temperature, >0 increases theta
  surForcS|  1 |       |SM      U1|g/m^2/s         |model surface forcing for Salinity, >0 increases salinity
  TFLUX   |  1 |       |SM      U1|W/m^2           |total heat flux (match heat-content variations), >0 increases theta
  SFLUX   |  1 |       |SM      U1|g/m^2/s         |total salt flux (match salt-content variations), >0 increases salt
  RCENTER | 50 |       |SM      MR|m               |Cell-Center Height
  RSURF   |  1 |       |SM      M1|m               |Surface Height
  TOTUTEND| 50 |    97 |UUR     MR|m/s/day         |Tendency of Zonal Component of Velocity
  TOTVTEND| 50 |    96 |VVR     MR|m/s/day         |Tendency of Meridional Component of Velocity
  TOTTTEND| 50 |       |SMR     MR|degC/day        |Tendency of Potential Temperature
  TOTSTEND| 50 |       |SMR     MR|psu/day         |Tendency of Salinity
  ----------------------------------------------------------------------------
  <-Name->|Levs|  mate |<- code ->|<--  Units   -->|<- Tile (max=80c)         
  ----------------------------------------------------------------------------
  MoistCor| 50 |       |SM      MR|W/m^2           |Heating correction due to moist thermodynamics
  gT_Forc | 50 |       |SMR     MR|degC/s          |Potential Temp. forcing tendency
  gS_Forc | 50 |       |SMR     MR|psu/s           |Salinity forcing tendency
  AB_gT   | 50 |       |SMR     MR|degC/s          |Potential Temp. tendency from Adams-Bashforth
  AB_gS   | 50 |       |SMR     MR|psu/s           |Salinity tendency from Adams-Bashforth
  gTinAB  | 50 |       |SMR     MR|degC/s          |Potential Temp. tendency going in Adams-Bashforth
  gSinAB  | 50 |       |SMR     MR|psu/s           |Salinity tendency going in Adams-Bashforth
  AB_gU   | 50 |   108 |UUR     MR|m/s^2           |U momentum tendency from Adams-Bashforth
  AB_gV   | 50 |   107 |VVR     MR|m/s^2           |V momentum tendency from Adams-Bashforth
  ADVr_TH | 50 |       |WM      LR|degC.m^3/s      |Vertical   Advective Flux of Pot.Temperature
  ADVx_TH | 50 |   111 |UU      MR|degC.m^3/s      |Zonal      Advective Flux of Pot.Temperature
  ADVy_TH | 50 |   110 |VV      MR|degC.m^3/s      |Meridional Advective Flux of Pot.Temperature
  DFrE_TH | 50 |       |WM      LR|degC.m^3/s      |Vertical Diffusive Flux of Pot.Temperature (Explicit part)
  DFxE_TH | 50 |   114 |UU      MR|degC.m^3/s      |Zonal      Diffusive Flux of Pot.Temperature
  DFyE_TH | 50 |   113 |VV      MR|degC.m^3/s      |Meridional Diffusive Flux of Pot.Temperature
  DFrI_TH | 50 |       |WM      LR|degC.m^3/s      |Vertical Diffusive Flux of Pot.Temperature (Implicit part)
  SM_x_TH | 50 |   117 |UM      MR|degC            |Pot.Temp.   1rst Order Moment Sx
  SM_y_TH | 50 |   116 |VM      MR|degC            |Pot.Temp.   1rst Order Moment Sy
  SM_z_TH | 50 |       |SM      MR|degC            |Pot.Temp.   1rst Order Moment Sz
  SMxx_TH | 50 |   120 |UM      MR|degC            |Pot.Temp.   2nd Order Moment Sxx
  SMyy_TH | 50 |   119 |VM      MR|degC            |Pot.Temp.   2nd Order Moment Syy
  SMzz_TH | 50 |       |SM      MR|degC            |Pot.Temp.   2nd Order Moment Szz
  SMxy_TH | 50 |       |SM      MR|degC            |Pot.Temp.   2nd Order Moment Sxy
  SMxz_TH | 50 |   124 |UM      MR|degC            |Pot.Temp.   2nd Order Moment Sxz
  SMyz_TH | 50 |   123 |VM      MR|degC            |Pot.Temp.   2nd Order Moment Syz
  SM_v_TH | 50 |       |SM P    MR|(degC)^2        |Pot.Temp.   sub-grid variance
  ADVr_SLT| 50 |       |WM      LR|psu.m^3/s       |Vertical   Advective Flux of Salinity
  ADVx_SLT| 50 |   128 |UU      MR|psu.m^3/s       |Zonal      Advective Flux of Salinity
  ADVy_SLT| 50 |   127 |VV      MR|psu.m^3/s       |Meridional Advective Flux of Salinity
  DFrE_SLT| 50 |       |WM      LR|psu.m^3/s       |Vertical Diffusive Flux of Salinity    (Explicit part)
  DFxE_SLT| 50 |   131 |UU      MR|psu.m^3/s       |Zonal      Diffusive Flux of Salinity
  DFyE_SLT| 50 |   130 |VV      MR|psu.m^3/s       |Meridional Diffusive Flux of Salinity
  DFrI_SLT| 50 |       |WM      LR|psu.m^3/s       |Vertical Diffusive Flux of Salinity    (Implicit part)
  SALTFILL| 50 |       |SM      MR|psu.m^3/s       |Filling of Negative Values of Salinity
  SM_x_SLT| 50 |   135 |UM      MR|psu             |Salinity    1rst Order Moment Sx
  SM_y_SLT| 50 |   134 |VM      MR|psu             |Salinity    1rst Order Moment Sy
  SM_z_SLT| 50 |       |SM      MR|psu             |Salinity    1rst Order Moment Sz
  SMxx_SLT| 50 |   138 |UM      MR|psu             |Salinity    2nd Order Moment Sxx
  SMyy_SLT| 50 |   137 |VM      MR|psu             |Salinity    2nd Order Moment Syy
  SMzz_SLT| 50 |       |SM      MR|psu             |Salinity    2nd Order Moment Szz
  SMxy_SLT| 50 |       |SM      MR|psu             |Salinity    2nd Order Moment Sxy
  SMxz_SLT| 50 |   142 |UM      MR|psu             |Salinity    2nd Order Moment Sxz
  SMyz_SLT| 50 |   141 |VM      MR|psu             |Salinity    2nd Order Moment Syz
  SM_v_SLT| 50 |       |SM P    MR|(psu)^2         |Salinity    sub-grid variance
  VISCAHZ | 50 |       |SZ      MR|m^2/s           |Harmonic Visc Coefficient (m2/s) (Zeta Pt)
  VISCA4Z | 50 |       |SZ      MR|m^4/s           |Biharmonic Visc Coefficient (m4/s) (Zeta Pt)
  VISCAHD | 50 |       |SM      MR|m^2/s           |Harmonic Viscosity Coefficient (m2/s) (Div Pt)
  VISCA4D | 50 |       |SM      MR|m^4/s           |Biharmonic Viscosity Coefficient (m4/s) (Div Pt)
  VISCAHW | 50 |       |WM      LR|m^2/s           |Harmonic Viscosity Coefficient (m2/s) (W Pt)
  VISCA4W | 50 |       |WM      LR|m^4/s           |Biharmonic Viscosity Coefficient (m4/s) (W Pt)
  VAHZMAX | 50 |       |SZ      MR|m^2/s           |CFL-MAX Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZMAX | 50 |       |SZ      MR|m^4/s           |CFL-MAX Biharm Visc Coefficient (m4/s) (Zeta Pt)
  VAHDMAX | 50 |       |SM      MR|m^2/s           |CFL-MAX Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DMAX | 50 |       |SM      MR|m^4/s           |CFL-MAX Biharm Visc Coefficient (m4/s) (Div Pt)
  VAHZMIN | 50 |       |SZ      MR|m^2/s           |RE-MIN Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZMIN | 50 |       |SZ      MR|m^4/s           |RE-MIN Biharm Visc Coefficient (m4/s) (Zeta Pt)
  VAHDMIN | 50 |       |SM      MR|m^2/s           |RE-MIN Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DMIN | 50 |       |SM      MR|m^4/s           |RE-MIN Biharm Visc Coefficient (m4/s) (Div Pt)
  VAHZLTH | 50 |       |SZ      MR|m^2/s           |Leith Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZLTH | 50 |       |SZ      MR|m^4/s           |Leith Biharm Visc Coefficient (m4/s) (Zeta Pt)
  VAHDLTH | 50 |       |SM      MR|m^2/s           |Leith Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DLTH | 50 |       |SM      MR|m^4/s           |Leith Biharm Visc Coefficient (m4/s) (Div Pt)
  VAHZLTHD| 50 |       |SZ      MR|m^2/s           |LeithD Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZLTHD| 50 |       |SZ      MR|m^4/s           |LeithD Biharm Visc Coefficient (m4/s) (Zeta Pt)
  VAHDLTHD| 50 |       |SM      MR|m^2/s           |LeithD Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DLTHD| 50 |       |SM      MR|m^4/s           |LeithD Biharm Visc Coefficient (m4/s) (Div Pt)
  VAHZSMAG| 50 |       |SZ      MR|m^2/s           |Smagorinsky Harm Visc Coefficient (m2/s) (Zeta Pt)
  VA4ZSMAG| 50 |       |SZ      MR|m^4/s           |Smagorinsky Biharm Visc Coeff. (m4/s) (Zeta Pt)
  VAHDSMAG| 50 |       |SM      MR|m^2/s           |Smagorinsky Harm Visc Coefficient (m2/s) (Div Pt)
  VA4DSMAG| 50 |       |SM      MR|m^4/s           |Smagorinsky Biharm Visc Coeff. (m4/s) (Div Pt)
  momKE   | 50 |       |SMR     MR|m^2/s^2         |Kinetic Energy (in momentum Eq.)
  momHDiv | 50 |       |SMR     MR|s^-1            |Horizontal Divergence (in momentum Eq.)
  momVort3| 50 |       |SZR     MR|s^-1            |3rd component (vertical) of Vorticity
  Strain  | 50 |       |SZR     MR|s^-1            |Horizontal Strain of Horizontal Velocities
  Tension | 50 |       |SMR     MR|s^-1            |Horizontal Tension of Horizontal Velocities
  UBotDrag| 50 |   176 |UUR     MR|m/s^2           |U momentum tendency from Bottom Drag
  VBotDrag| 50 |   175 |VVR     MR|m/s^2           |V momentum tendency from Bottom Drag
  USidDrag| 50 |   178 |UUR     MR|m/s^2           |U momentum tendency from Side Drag
  VSidDrag| 50 |   177 |VVR     MR|m/s^2           |V momentum tendency from Side Drag
  Um_Diss | 50 |   180 |UUR     MR|m/s^2           |U momentum tendency from Dissipation
  Vm_Diss | 50 |   179 |VVR     MR|m/s^2           |V momentum tendency from Dissipation
  Um_Advec| 50 |   182 |UUR     MR|m/s^2           |U momentum tendency from Advection terms
  Vm_Advec| 50 |   181 |VVR     MR|m/s^2           |V momentum tendency from Advection terms
  Um_Cori | 50 |   184 |UUR     MR|m/s^2           |U momentum tendency from Coriolis term
  Vm_Cori | 50 |   183 |VVR     MR|m/s^2           |V momentum tendency from Coriolis term
  Um_dPHdx| 50 |   186 |UUR     MR|m/s^2           |U momentum tendency from Hydrostatic Pressure grad
  Vm_dPHdy| 50 |   185 |VVR     MR|m/s^2           |V momentum tendency from Hydrostatic Pressure grad
  Um_Ext  | 50 |   188 |UUR     MR|m/s^2           |U momentum tendency from external forcing
  Vm_Ext  | 50 |   187 |VVR     MR|m/s^2           |V momentum tendency from external forcing
  Um_AdvZ3| 50 |   190 |UUR     MR|m/s^2           |U momentum tendency from Vorticity Advection
  Vm_AdvZ3| 50 |   189 |VVR     MR|m/s^2           |V momentum tendency from Vorticity Advection
  Um_AdvRe| 50 |   192 |UUR     MR|m/s^2           |U momentum tendency from vertical Advection (Explicit part)
  Vm_AdvRe| 50 |   191 |VVR     MR|m/s^2           |V momentum tendency from vertical Advection (Explicit part)
  VISrI_Um| 50 |       |WU      LR|m^4/s^2         |Vertical   Viscous Flux of U momentum (Implicit part)
  VISrI_Vm| 50 |       |WV      LR|m^4/s^2         |Vertical   Viscous Flux of V momentum (Implicit part)


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


.. _pkg_mdsio:

Fortran Native I/O: pkg/mdsio and pkg/rw
========================================

pkg/mdsio
---------

Introduction
~~~~~~~~~~~~

:filelink:`pkg/mdsio` contains a group of Fortran routines intended as a
general interface for reading and writing direct-access (“binary”)
Fortran files. :filelink:`pkg/mdsio` routines are used by :filelink:`pkg/rw`.

Using pkg/mdsio
~~~~~~~~~~~~~~~

:filelink:`pkg/mdsio` is geared toward the reading and writing of
floating point (Fortran ``REAL*4`` or ``REAL*8``) arrays. It assumes
that the in-memory layout of all arrays follows the per-tile MITgcm
convention

::

    C     Example of a "2D" array
          _RL anArray(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

    C     Example of a "3D" array
          _RL anArray(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)

where the first two dimensions are spatial or “horizontal” indicies that
include a “halo” or exchange region (please see :numref:`sarch` and
:numref:`sub_phys_pkg_exch2` which describe domain decomposition), and the remaining
indicies (``Nr``, ``nSx``, and ``nSx``) are often present but may or may not be necessary for a specific variable..

In order to write output, :filelink:`pkg/mdsio` is called with a
function such as:

::

          CALL MDSWRITEFIELD(fn,prec,lgf,typ,Nr,arr,irec,myIter,myThid)

where:

    ``fn``
        is a ``CHARACTER`` string containing a file “base” name which
        will then be used to create file names that contain tile and/or
        model iteration indicies

    ``prec``
        is an integer that contains one of two globally defined values
        (``precFloat64`` or ``precFloat32``)

    ``lgf``
        is a ``LOGICAL`` that typically contains the globally defined
        ``globalFile`` option which specifies the creation of globally
        (spatially) concatenated files

    ``typ``
        is a ``CHARACTER`` string that specifies the type of the
        variable being written (``’RL’`` or ``’RS’``)

    ``Nr``
        is an integer that specifies the number of vertical levels
        within the variable being written

    ``arr``
        is the variable (array) to be written

    ``irec``
        is the starting record within the output file that will contain
        the array

    ``myIter,myThid``
        are integers containing, respectively, the current model
        iteration count and the unique thread ID for the current context
        of execution

As one can see from the above (generic) example, enough information is
made available (through both the argument list and through common
blocks) for :filelink:`pkg/mdsio` to perform the following tasks:

#. open either a per-tile file such as:

   ``uVel.0000302400.003.001.data``

   or a “global” file such as

   ``uVel.0000302400.data``

#. byte-swap (as necessary) the input array and write its contents
   (minus any halo information) to the binary file – or to the correct
   location within the binary file if the ``globalfile`` option is used, and

#. create an ASCII–text metadata file (same name as the binary but with
   a ``.meta`` extension) describing the binary file contents (often,
   for later use with the MATLAB :filelink:`rdmds() <utils/matlab/rdmds.m>` utility).

Reading output with :filelink:`pkg/mdsio` is very similar to writing it. A typical
function call is

::

          CALL MDSREADFIELD(fn,prec,typ,Nr,arr,irec,myThid)

where variables are exactly the same as the ``MDSWRITEFIELD`` example
provided above. It is important to note that the ``lgf`` argument is
missing from the ``MDSREADFIELD`` function. By default, :filelink:`pkg/mdsio` will
first try to read from an appropriately named global file and, failing
that, will try to read from a per-tile file.

Important considerations
~~~~~~~~~~~~~~~~~~~~~~~~

When using :filelink:`pkg/mdsio`, one should be aware of the following package
features and limitations:

-   **Byte-swapping:**
    For the most part, byte-swapping is gracefully handled. All files intended for
    reading/writing by :filelink:`pkg/mdsio` should contain big-endian (sometimes
    called “network byte order”) data. By handling byte-swapping within
    the model, MITgcm output is more easily ported between different
    machines, architectures, compilers, etc. Byteswapping can be turned
    on/off at compile time within :filelink:`pkg/mdsio` using the ``_BYTESWAPIO``
    CPP macro which is usually set within a :filelink:`genmake2 <tools/genmake2>` options file or
    ``optfile`` (see :numref:`genmake2_optfiles`).
    Additionally, some compilers may have byte-swap options that are
    speedier or more convenient to use.

-   **Data types:**
    Data types are currently limited to single– or double–precision floating point
    values. These values can be converted, on-the-fly, from one to the
    other so that any combination of either single– or double–precision
    variables can be read from or written to files containing either
    single– or double–precision data.

-   **Array sizes:**
    Array sizes are limited; :filelink:`pkg/mdsio` is very much geared towards the
    reading/writing of per-tile (that is, domain-decomposed and halo-ed)
    arrays. Data that cannot be made to “fit” within these assumed sizes
    can be challenging to read or write with :filelink:`pkg/mdsio`.

-   **Tiling:**
    Tiling or domain decomposition is automatically handled by :filelink:`pkg/mdsio` for
    logically rectangular grid topologies (e.g., lat-lon grids) and
    “standard” cubed sphere topologies. More complicated topologies will
    probably not be supported. :filelink:`pkg/mdsio` can, without any
    coding changes, read and write to/from files that were run on the
    same global grid but with different tiling (grid decomposition)
    schemes. For example, :filelink:`pkg/mdsio` can use and/or create identical
    input/output files for a “C32” cube when the model is run with
    either 6, 12, or 24 tiles (corresponding to 1, 2 or 4 tiles per
    cubed sphere face). This is one of the primary advantages
    that the :filelink:`pkg/mdsio` package has over :filelink:`pkg/mnc`.

-   **Single-CPU I/O:**
    This option can be specified with the flag
    ``useSingleCpuIO = .TRUE.`` in the ``PARM01`` namelist within the main ``data`` file. Single–CPU
    I/O mode is appropriate for computers (e.g., some SGI systems) where
    it can either speed overall I/O or solve problems where the
    operating system or file systems cannot correctly handle multiple
    threads or MPI processes simultaneously writing to the same file.

-   **Meta-data:**
    Meta-data is written by MITgcm on a per-file basis using a second file with a
    ``.meta`` extension as described above. MITgcm itself does not read
    the ``*.meta`` files, they are there primarly for convenience during
    post-processing. One should be careful not to delete the meta-data
    files when using MATLAB post-processing scripts such as :filelink:`rdmds() <utils/matlab/rdmds.m>`
    since it relies upon them.

-   **Numerous files:**
    If one is not careful (e.g., dumping many variables every time step over a long integration), :filelink:`pkg/mdsio`
    will write copious amounts of files. The creation of both a binary (``*.data``)
    and ASCII text meta-data (``*.meta``) file for each output type step
    exacerbates the issue. Some operating
    systems do not gracefully handle large numbers (e.g., many
    thousands to millions) of files within one directory. So care should be taken to
    split output into smaller groups using subdirectories.

-   **Overwriting output:**
    Overwriting of output is the **default behavior** for :filelink:`pkg/mdsio`. If a model tries to write
    to a file name that already exists, the older file **will be
    deleted**. For this reason, MITgcm users should be careful to move
    output that they wish to keep into, for instance, subdirectories
    before performing subsequent runs that may over–lap in time or
    otherwise produce files with identical names (e.g., Monte-Carlo
    simulations).

-   **No “halo” information:**
    “Halo” information is neither written nor read by :filelink:`pkg/mdsio`. Along the horizontal dimensions,
    all variables are written in an ``sNx``–by–``sNy`` fashion. So,
    although variables (arrays) may be defined at different locations on
    Arakawa grids [U (right/left horizontal edges), V (top/bottom
    horizontal edges), M (mass or cell center), or Z (vorticity or cell
    corner) points], they are all written using only interior (``1:sNx``
    and ``1:sNy``) values. For quantities defined at U, V, and M points,
    writing ``1:sNx`` and ``1:sNy`` for every tile is sufficient to
    ensure that all values are written globally for some grids (e.g.,
    cubed sphere, re-entrant channels, and doubly-periodic rectangular
    regions). For Z points, failing to write values at the ``sNx+1`` and
    ``sNy+1`` locations means that, for some tile topologies, not all
    values are written. For instance, with a cubed sphere topology at
    least two corner values are “lost” (fail to be written for any tile)
    if the ``sNx+1`` and ``sNy+1`` values are ignored. If this is an issue, we recommend
    switching to :filelink:`pkg/mnc`, which writes the ``sNx+1`` and ``sNy+1`` grid
    values for the U, V, and Z locations. Also, :filelink:`pkg/mnc` is
    capable of reading and/or writing entire halo regions and more
    complicated array shapes which can be helpful when
    debugging -- features that do not exist within :filelink:`pkg/mdsio`.

.. tabularcolumns:: |\Y{.4}|L|L|


+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| CPP Flag Name                                 | Default | Description                                                                                                          |
+===============================================+=========+======================================================================================================================+
| :varlink:`SAFE_IO`                            | #undef  | if defined, stops the model from overwriting its own files                                                           |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`ALLOW_WHIO`                         | #undef  | I/O will include tile halos in the files                                                                             |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

pkg/rw basic binary I/O utilities
---------------------------------

:filelink:`pkg/rw` provides a very rudimentary binary I/O capability for
quickly writing *single record* direct-access Fortran binary files. It
is primarily used for writing diagnostic output.

Introduction
~~~~~~~~~~~~

:filelink:`pkg/rw` is an interface to the more general :filelink:`pkg/mdsio` package.
:filelink:`pkg/rw` can be used to write or read direct-access Fortran binary files
for 2-D XY and 3-D XYZ arrays. The arrays are
assumed to have been declared according to the standard MITgcm
2-D or 3-D floating point array type:

::

    C     Example of declaring a standard two dimensional "long"
    C     floating point type array (the _RL macro is usually
    C     mapped to 64-bit floats in most configurations)
          _RL anArray(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

Each call to a :filelink:`pkg/rw` read or write routine will read (or write) to the
first record of a file. To write direct access Fortran files with
multiple records use the higher-level routines in :filelink:`pkg/mdsio` rather than :filelink:`pkg/rw` routines. To
write self-describing files that contain embedded information describing
the variables being written and the spatial and temporal locations of
those variables use the :filelink:`pkg/mnc` instead (see :numref:`pkg_mnc`) which
produces `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ format output.

.. tabularcolumns:: |\Y{.4}|L|L|


+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| CPP Flag Name                                 | Default | Description                                                                                                          |
+===============================================+=========+======================================================================================================================+
| :varlink:`RW_SAFE_MFLDS`                      | #define | use READ_MFLDS in "safe" mode (set/check/unset for each file to read); involves more thread synchronization          |
|                                               |         | which could slow down multi-threaded run                                                                             |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`RW_DISABLE_SMALL_OVERLAP`           | #undef  | disable writing of small-overlap size array (to reduce memory size since those S/R do a local copy to 3-D            |
|                                               |         | full-size overlap array)                                                                                             |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

.. _pkg_mnc:

NetCDF I/O: pkg/mnc
===================

Package :filelink:`pkg/mnc` is a set of convenience routines written to expedite
the process of creating, appending, and reading `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ files.
`NetCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ is
an increasingly popular self-describing file format intended primarily for scientific data sets.
An extensive collection of netCDF `documentation <https://www.unidata.ucar.edu/software/netcdf/docs/index.html>`_,
including a `user’s guide <https://www.unidata.ucar.edu/software/netcdf/docs/user_guide.html>`_,
`tutorial <https://www.unidata.ucar.edu/software/netcdf/docs/tutorial_8dox.html>`_,
`FAQ <https://www.unidata.ucar.edu/software/netcdf/docs/faq.html>`_,
`support archive <https://www.unidata.ucar.edu/support/help/MailArchives/netcdf/maillist.html>`_ and
other information can be obtained from UCAR’s web
site http://www.unidata.ucar.edu/software/netcdf.

Since it is a “wrapper” for `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_,
:filelink:`pkg/mnc` depends upon the Fortran-77
interface included with the standard `NetCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ v3.x library which is often
called ``libnetcdf.a``. Please contact your local systems administrators
or email mitgcm-support@mitgcm.org for help building and installing
`netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ for your particular
platform.

Every effort has been made to allow :filelink:`pkg/mnc` and :filelink:`pkg/mdsio` (see :numref:`pkg_mdsio`) to
peacefully co-exist. In may cases, the model can read one format and
write to the other. This side-by-side functionality can be used to, for
instance, help convert pickup files or other data sets between the two
formats.

Using pkg/mnc
-------------

pkg/mnc configuration:
~~~~~~~~~~~~~~~~~~~~~~

As with all MITgcm packages, :filelink:`pkg/mnc` can be turned on or off at compile time
using the ``packages.conf`` file or the :filelink:`genmake2 <tools/genmake2>` ``-enable=mnc`` or
``-disable=mnc`` switches.

While :filelink:`pkg/mnc` is likely to work “as is”, there are a few compile–time
constants that may need to be increased for simulations that employ
large numbers of tiles within each process. Note that the important
quantity is the maximum number of tiles **per process**. Since MPI
configurations tend to distribute large numbers of tiles over relatively
large numbers of MPI processes, these constants will rarely need to be
increased.

If :filelink:`pkg/mnc` runs out of space within its “lookup” tables during a simulation,
then it will provide an error message along with a recommendation of
which parameter to increase. The parameters are all located within :filelink:`MNC_COMMON.h <pkg/mnc/MNC_COMMON.h>` and
the ones that may need to be increased are:

+----------------+----------+--------------------------------------+
| Name           |Default   | Description                          |
+================+==========+======================================+
| MNC_MAX_ID     | 1000     | IDs for various low-level entities   |
+----------------+----------+--------------------------------------+
| MNC_MAX_INFO   | 400      | IDs (mostly for object sizes)        |
+----------------+----------+--------------------------------------+
| MNC_CW_MAX_I   | 150      | IDs for the “wrapper” layer          |
+----------------+----------+--------------------------------------+

In those rare cases where :filelink:`pkg/mnc` “out-of-memory” error messages are
encountered, it is a good idea to increase the too-small parameter by a
factor of 2–10 in order to avoid wasting time on an iterative
compile–test sequence.

.. _pkg_mnc_inputs:

pkg/mnc Inputs:
~~~~~~~~~~~~~~~

Like most MITgcm packages, all of :filelink:`pkg/mnc` can be turned on/off at runtime
using a single flag in ``data.pkg``:

+---------------------------------+---------+------------+----------------------------------------------+
| Name                            | Type    | Default    | Description                                  |
+=================================+=========+============+==============================================+
| :varlink:`useMNC`               | L       | .FALSE.    | overall MNC ON/OFF switch                    |
+---------------------------------+---------+------------+----------------------------------------------+

One important MNC–related flag is present in the main ``data`` namelist
file in the ``PARM03`` section:

+---------------------------------+---------+------------+----------------------------------------------+
| Name                            | Type    | Default    | Description                                  |
+=================================+=========+============+==============================================+
| :varlink:`outputTypesInclusive` | L       | .FALSE.    | use all available output “types”             |
+---------------------------------+---------+------------+----------------------------------------------+

which specifies that turning on :filelink:`pkg/mnc` for a particular type of output
should not simultaneously turn off the default output method as it
normally does. Usually, this option is only used for debugging purposes
since it is inefficient to write output types using both :filelink:`pkg/mnc` and :filelink:`pkg/mdsio`
or ASCII output. This option can also be helpful when transitioning from
:filelink:`pkg/mdsio` to :filelink:`pkg/mnc` since the output can be readily compared.

For run-time configuration, most of the :filelink:`pkg/mnc`–related model parameters are
contained within a Fortran namelist file called ``data.mnc``. The
available parameters currently include:

+---------------------------------+---------+------------+----------------------------------------------+
| Name                            | Type    | Default    | Description                                  |
+=================================+=========+============+==============================================+
| :varlink:`mnc_use_outdir`       | L       | .FALSE.    | create a directory for output                |
+---------------------------------+---------+------------+----------------------------------------------+
|   :varlink:`mnc_outdir_str`     | S       | ’mnc\_’    | output directory name                        |
+---------------------------------+---------+------------+----------------------------------------------+
|   :varlink:`mnc_outdir_date`    | L       | .FALSE.    | embed date in the outdir name                |
+---------------------------------+---------+------------+----------------------------------------------+
|   :varlink:`mnc_outdir_num`     | L       | .TRUE.     | optional                                     |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`pickup_write_mnc`     | L       | .TRUE.     | use MNC to write pickup files                |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`pickup_read_mnc`      | L       | .TRUE.     | use MNC to read pickup file                  |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`mnc_use_indir`        | L       | .FALSE.    | use a directory (path) for input             |
+---------------------------------+---------+------------+----------------------------------------------+
|   :varlink:`mnc_indir_str`      | S       | ‘ ’        | input directory (or path) name               |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`snapshot_mnc`         | L       | .TRUE.     | write snapshot output w/MNC                  |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`monitor_mnc`          | L       | .TRUE.     | write :filelink:`pkg/monitor` output w/MNC   |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`timeave_mnc`          | L       | .TRUE.     | write :filelink:`pkg/timeave` output w/MNC   |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`autodiff_mnc`         | L       | .TRUE.     | write :filelink:`pkg/autodiff` output w/MNC  |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`mnc_max_fsize`        | R       | 2.1e+09    | max allowable file size (<2GB)               |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`mnc_filefreq`         | R       | -1         | frequency of new file creation (seconds)     |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`readgrid_mnc`         | L       | .FALSE.    | read grid quantities using MNC               |
+---------------------------------+---------+------------+----------------------------------------------+
| :varlink:`mnc_echo_gvtypes`     | L       | .FALSE.    | list pre-defined “types” (debug)             |
+---------------------------------+---------+------------+----------------------------------------------+

Unlike the older :filelink:`pkg/mdsio` method, :filelink:`pkg/mnc` has the ability to create or use
existing output directories. If either :varlink:`mnc_outdir_date` or
:varlink:`mnc_outdir_num` is ``.TRUE.``, then :filelink:`pkg/mnc` will try to create directories on a
*per process* basis for its output. This means that a single directory
will be created for a non-MPI run and multiple directories (one per MPI
process) will be created for an MPI run. This approach was chosen since
it works safely on both shared global file systems (such as NFS and AFS)
and on local (per-compute-node) file systems. And if both
:varlink:`mnc_outdir_date` and :varlink:`mnc_outdir_num` are ``.FALSE.``, then the :filelink:`pkg/mnc`
package will assume that the directory specified in :varlink:`mnc_outdir_str`
already exists and will use it. This allows the user to create and
specify directories outside of the model.

For input, :filelink:`pkg/mnc` can use a single global input directory. This is a just
convenience that allows :filelink:`pkg/mnc` to gather all of its input files from a path
other than the current working directory. As with :filelink:`pkg/mdsio`, the default is
to use the current working directory.

The flags :varlink:`snapshot_mnc`, :varlink:`monitor_mnc`, :varlink:`timeave_mnc`, and
:varlink:`autodiff_mnc` allow the user to turn on :filelink:`pkg/mnc` for particular “types” of
output. If a type is selected, then :filelink:`pkg/mnc` will be used for all output that
matches that type. This applies to output from the main model and from
all of the optional MITgcm packages. Mostly, the names used here
correspond to the names used for the output frequencies in the main
``data`` namelist file.

The :varlink:`mnc_max_fsize` parameter is a convenience added to help users
work around common file size limitations. On many computer systems,
either the operating system, the file system(s), and/or the `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_
libraries are unable to handle files greater than two or four gigabytes
in size. :filelink:`pkg/mnc` is able to work within this limitation by
creating new files which grow along the `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ “unlimited” (usually,
time) dimension. The default value for this parameter is just slightly
less than 2GB which is safe on virtually all operating systems.
Essentially, this feature is a way to intelligently and automatically
split files output along the unlimited dimension. On systems that
support large file sizes, these splits can be readily concatenated (that
is, un-done) using tools such as the NetCDF
Operators (with `ncrcat <http://nco.sourceforge.net/nco.html#ncrcat-netCDF-Record-Concatenator>`_)
which is available at http://nco.sourceforge.net.

Another way users can force the splitting of :filelink:`pkg/mnc` files along the time
dimension is the :varlink:`mnc_filefreq` option. With it, files that contain
variables with a temporal dimension can be split at regular intervals
based solely upon the model time (specified in seconds). For some
problems, this can be much more convenient than splitting based upon
file size.

Additional :filelink:`pkg/mnc`–related parameters may be contained within each package.
Please see the individual packages for descriptions of their use of :filelink:`pkg/mnc`.

pkg/mnc output:
~~~~~~~~~~~~~~~

Depending upon the flags used, :filelink:`pkg/mnc` will produce zero or more directories
containing one or more `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ files as output. These files are either
mostly or entirely compliant with the `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ “CF” convention (v1.0) and
any conformance issues will be fixed over time. The patterns used for file names are:

- «BASENAME».«tileNum».nc
- «BASENAME».«nIter».«faceNum».nc
- «BASENAME».«nIter».«tileNum».nc

and examples are:


- grid.t001.nc, grid.t002.nc
- input.0000072000.f001.nc
- state.0000000000.t001.nc, surfDiag.0000036000.t001.nc


where «BASENAME» is the name selected to represent a set of variables
written together, «nIter» is the current iteration number as specified
in the main ``data`` namelist input file and written in a zero-filled
10-digit format, «tileNum» is a three-or-more-digit zero-filled and
``t``–prefixed tile number, «faceNum» is a three-or-more-digit
zero-filled and ``f``–prefixed face number, and ``.nc`` is the file
suffix specified by the current `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ “CF” conventions.

Some example «BASENAME» values are:

**grid**
    contains the variables that describe the various grid constants
    related to locations, lengths, areas, etc.

**state**
    contains the variables output at the :varlink:`dumpFreq` time frequency

**pickup.ckptA, pickup.ckptB**
    are the “rolling” checkpoint files

**tave**
    contains the time-averaged quantities from the main model

All :filelink:`pkg/mnc` output is currently done in a “file-per-tile” fashion since most
`NetCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ v3.x implementations cannot write safely within MPI or
multi-threaded environments. This tiling is done in a global fashion and
the tile numbers are appended to the base names as described above. Some
scripts to manipulate :filelink:`pkg/mnc` output are available at
:filelink:`utils/matlab` which includes a spatial “assembly” script
:filelink:`mnc_assembly.m <utils/matlab/mnc_assembly.m>`.

More general manipulations can be performed on `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_  files with
the NetCDF Operators (“NCO”) at http://nco.sourceforge.net
or with the Climate Data Operators (“CDO”) at https://code.mpimet.mpg.de/projects/cdo.

Unlike the older :filelink:`pkg/mdsio` routines, :filelink:`pkg/mnc` reads and writes variables on
different “grids” depending upon their location in the
Arakawa C–grid. The following table provides examples:

+---------------+-----------------------+--------------+--------------+
| Name          | C–grid location       |  # in X      | # in Y       |
+===============+=======================+==============+==============+
| Temperature   | mass                  | sNx          | sNy          |
+---------------+-----------------------+--------------+--------------+
| Salinity      | mass                  | sNx          | sNy          |
+---------------+-----------------------+--------------+--------------+
| U velocity    | U                     | sNx+1        | sNy          |
+---------------+-----------------------+--------------+--------------+
| V velocity    | V                     | sNx          | sNy+1        |
+---------------+-----------------------+--------------+--------------+
| Vorticity     | vorticity             | sNx+1        | sNy+1        |
+---------------+-----------------------+--------------+--------------+

and the intent is two–fold:

#. For some grid topologies it is impossible to output all quantities
   using only ``sNx,sNy`` arrays for every tile. Two examples of this
   failure are the missing corners problem for vorticity values on the
   cubed sphere and the velocity edge values for some open–boundary
   domains.

#. Writing quantities located on velocity or vorticity points with the
   above scheme introduces a very small data redundancy. However, any
   slight inconvenience is easily offset by the ease with which one can,
   on every individual tile, interpolate these values to mass points
   without having to perform an “exchange” (or “halo-filling”) operation
   to collect the values from neighboring tiles. This makes the most
   common post–processing operations much easier to implement.

pkg/mnc Troubleshooting
-----------------------

Build troubleshooting:
~~~~~~~~~~~~~~~~~~~~~~

In order to build MITgcm with :filelink:`pkg/mnc` enabled,
the `NetCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ v3.x Fortran-77
(not Fortran-90) library must be available. This library is composed of
a single header file (called ``netcdf.inc``) and a single library file
(usually called ``libnetcdf.a``) and it must be built with the same
compiler with compatible compiler
options as the one used to build MITgcm (in other words,
while one does not have to build ``libnetcdf.a``
with the same exact set of compiler options as MITgcm,
one must avoid using some specific different compiler options which are incompatible,
i.e., causing a compile-time or run-time error).

For more details concerning the netCDF build and install process, please
visit the `Getting and Building NetCDF guide <https://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html>`_
which includes an extensive list of known–good netCDF configurations for various platforms.

Runtime troubleshooting:
~~~~~~~~~~~~~~~~~~~~~~~~

Please be aware of the following:

-  As a safety feature, the :filelink:`pkg/mnc` does not, by default, allow
   pre-existing files to be appended to or overwritten. This is in
   contrast to the older :filelink:`pkg/mdsio` which will, without any warning,
   overwrite existing files. If MITgcm aborts with an error message
   about the inability to open or write to a netCDF file, please check
   **first** whether you are attempting to overwrite files from a
   previous run.

-  The constraints placed upon the “unlimited” (or “record”) dimension
   inherent with `NetCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ v3.x
   make it very inefficient to put variables
   written at potentially different intervals within the same file. For
   this reason, :filelink:`pkg/mnc` output is split into groups of files which attempt
   to reflect the nature of their content.

-  On many systems, `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_
   has practical file size limits on the order
   of 2–4GB (the maximium memory addressable with 32bit pointers or
   pointer differences) due to a lack of operating system, compiler,
   and/or library support. The latest revisions of 
   `NetCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ v3.x have
   large file support and, on some operating systems, file sizes are
   only limited by available disk space.

-  There is an 80 character limit to the total length of all file names.
   This limit includes the directory (or path) since paths and file
   names are internally appended. Generally, file names will not exceed
   the limit and paths can usually be shortened using, for example, soft
   links.

-  :filelink:`pkg/mnc` does not (yet) provide a mechanism for reading information from a
   single “global” file as can be done with :filelink:`pkg/mdsio`. This is
   in progress.

pkg/mnc Internals
-----------------

:filelink:`pkg/mnc` is a two-level convenience library (or “wrapper”)
for most of the `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ Fortran API. Its purpose is to streamline the
user interface to `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ by maintaining internal relations (look-up
tables) keyed with strings (or names) and entities such as `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ files,
variables, and attributes.

The two levels of :filelink:`pkg/mnc` are:

Upper level
     

    The upper level contains information about two kinds of
    associations:

    grid type
        is lookup table indexed with a grid type name. Each grid type
        name is associated with a number of dimensions, the dimension
        sizes (one of which may be unlimited), and starting and ending
        index arrays. The intent is to store all the necessary size and
        shape information for the Fortran arrays containing MITgcm–style
        “tile” variables (i.e., a central region surrounded by a
        variably-sized “halo” or exchange region as shown in :numref:`comm-prim`
        and :numref:`tiling_detail`).

    variable type
        is a lookup table indexed by a variable type name. For each
        name, the table contains a reference to a grid type for the
        variable and the names and values of various attributes.

    Within the upper level, these associations are not permanently tied
    to any particular `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ file. This allows the information to be
    re-used over multiple file reads and writes.

Lower level
     

    In the lower (or internal) level, associations are stored for `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_
    files and many of the entities that they contain including
    dimensions, variables, and global attributes. All associations are
    on a per-file basis. Thus, each entity is tied to a unique `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_
    file and will be created or destroyed when files are, respectively,
    opened or closed.

pkg/mnc grid–tTypes and variable–types:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As a convenience for users, :filelink:`pkg/mnc` includes numerous routines
to aid in the writing of data to `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ format. Probably the biggest
convenience is the use of pre-defined “grid types” and “variable types”.
These “types” are simply look-up tables that store dimensions, indicies,
attributes, and other information that can all be retrieved using a
single character string.

The “grid types” are a way of mapping variables within MITgcm to `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_
arrays. Within MITgcm, most spatial variables are defined using 2–D or
3–D arrays with “overlap” regions (see :numref:`comm-prim`, a possible vertical index,
and :numref:`tiling_detail`) and tile indicies such as the following “U”
velocity:

::

          _RL  uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

as defined in :filelink:`DYNVARS.h <model/inc/DYNVARS.h>`.

The grid type is a character string that encodes the presence and types
associated with the four possible dimensions. The character string
follows the format: 

::

          «H0»_«H1»_«H2»__«V»__«T» 

(note the double underscore
between «H2» and «V», and «V» and  «T») where the terms
«H0», «H1», «H2», «V», «T» can be almost any combination
of the following:

+------------------------------------------------+---------------+------------+
|                     Horizontal                 | Vertical      | Time       |
+----------------+------------------+------------+---------------+------------+
| H0: location   | H1: dimensions   | H2: halo   | V: location   | T: level   |
+================+==================+============+===============+============+
|    	–        | xy               | Hn         |  	–        |   	–     |
+----------------+------------------+------------+---------------+------------+
| U              | x                | Hy         | i             | t          |
+----------------+------------------+------------+---------------+------------+
| V              | y                |            | c             |            |
+----------------+------------------+------------+---------------+------------+
| Cen            |                  |            |               |            |
+----------------+------------------+------------+---------------+------------+
| Cor            |                  |            |               |            |
+----------------+------------------+------------+---------------+------------+

A example list of all pre-defined combinations is contained in the file :filelink:`pkg/mnc/pre-defined_grids.txt`.

The variable type is an association between a variable type name and the
following items:

+------------------------+-------------------------------------+
|   Item                 |   Purpose                           |
+========================+=====================================+
| grid type              | defines the in-memory arrangement   |
+------------------------+-------------------------------------+
| bi,bj dimensions       | tiling indices, if present          |
+------------------------+-------------------------------------+

and is used by the ``mnc_cw__[R|W]`` subroutines for reading and writing
variables.

Using pkg/mnc: examples
~~~~~~~~~~~~~~~~~~~~~~~

Writing variables to `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ files can be accomplished in as few as two
function calls. The first function call defines a variable type,
associates it with a name (character string), and provides additional
information about the indicies for the tile (bi,bj)
dimensions. The second function call will write the data at, if
necessary, the current time level within the model.

Examples of the initialization calls can be found in the file
:filelink:`model/src/ini_model_io.F` where these function calls:

::

    C     Create MNC definitions for DYNVARS.h variables
          CALL MNC_CW_ADD_VNAME('iter', '-_-_--__-__t', 0,0, myThid)
          CALL MNC_CW_ADD_VATTR_TEXT('iter',1,
         &     'long_name','iteration_count', myThid)

          CALL MNC_CW_ADD_VNAME('model_time', '-_-_--__-__t', 0,0, myThid)
          CALL MNC_CW_ADD_VATTR_TEXT('model_time',1,
         &     'long_name','Model Time', myThid)
          CALL MNC_CW_ADD_VATTR_TEXT('model_time',1,'units','s', myThid)

          CALL MNC_CW_ADD_VNAME('U', 'U_xy_Hn__C__t', 4,5, myThid)
          CALL MNC_CW_ADD_VATTR_TEXT('U',1,'units','m/s', myThid)
          CALL MNC_CW_ADD_VATTR_TEXT('U',1,
         &     'coordinates','XU YU RC iter', myThid)

          CALL MNC_CW_ADD_VNAME('T', 'Cen_xy_Hn__C__t', 4,5, myThid)
          CALL MNC_CW_ADD_VATTR_TEXT('T',1,'units','degC', myThid)
          CALL MNC_CW_ADD_VATTR_TEXT('T',1,'long_name',
         &     'potential_temperature', myThid)
          CALL MNC_CW_ADD_VATTR_TEXT('T',1,
         &     'coordinates','XC YC RC iter', myThid)

initialize four ``VNAME``\ s and add one or more
`netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ attributes to each.

The four variables defined above are subsequently written at specific
time steps within :filelink:`model/src/write_state.F` using the function calls:

::

    C       Write dynvars using the MNC package
            CALL MNC_CW_SET_UDIM('state', -1, myThid)
            CALL MNC_CW_I_W('I','state',0,0,'iter', myIter, myThid)
            CALL MNC_CW_SET_UDIM('state', 0, myThid)
            CALL MNC_CW_RL_W('D','state',0,0,'model_time',myTime, myThid)
            CALL MNC_CW_RL_W('D','state',0,0,'U', uVel, myThid)
            CALL MNC_CW_RL_W('D','state',0,0,'T', theta, myThid)

While it is easiest to write variables within typical 2-D and 3-D fields
where all data is known at a given time, it is also possible to write
fields where only a portion (e.g., a “slab” or “slice”) is known at a
given instant. An example is provided within :filelink:`pkg/mom_vecinv/mom_vecinv.F`
where an offset vector is used:

::

           IF (useMNC .AND. snapshot_mnc) THEN
             CALL MNC_CW_RL_W_OFFSET('D','mom_vi',bi,bj, 'fV', uCf,
       &          offsets, myThid)
             CALL MNC_CW_RL_W_OFFSET('D','mom_vi',bi,bj, 'fU', vCf,
       &          offsets, myThid)
           ENDIF

to write a 3-D field one depth slice at a time.

Each element in the offset vector corresponds (in order) to the
dimensions of the “full” (or virtual) array and specifies which are
known at the time of the call. A zero within the offset array means that
all values along that dimension are available while a positive integer
means that only values along that index of the dimension are available.
In all cases, the matrix passed is assumed to start (that is, have an
in-memory structure) coinciding with the start of the specified slice.
Thus, using this offset array mechanism, a slice can be written along
any single dimension or combinations of dimensions.

.. _pkg_monitor:

Monitor: Simulation State Monitoring Toolkit
============================================

Introduction
------------

:filelink:`pkg/monitor` is primarily intended as a convenient method for
calculating and writing the following statistics:

- minimum
- maximum
- mean
- standard deviation

for spatially distributed fields. By default, :filelink:`pkg/monitor` output is sent
to the “standard output” channel where it appears as ASCII text
containing a ``%MON`` string such as this example:

::

         (PID.TID 0000.0001) %MON time_tsnumber      =                     3
         (PID.TID 0000.0001) %MON time_secondsf      =   3.6000000000000E+03
         (PID.TID 0000.0001) %MON dynstat_eta_max    =   1.0025466645951E-03
         (PID.TID 0000.0001) %MON dynstat_eta_min    =  -1.0008899950901E-03
         (PID.TID 0000.0001) %MON dynstat_eta_mean   =   2.1037438449350E-14
         (PID.TID 0000.0001) %MON dynstat_eta_sd     =   5.0985228723396E-04
         (PID.TID 0000.0001) %MON dynstat_eta_del2   =   3.5216706549525E-07
         (PID.TID 0000.0001) %MON dynstat_uvel_max   =   3.7594045977254E-05
         (PID.TID 0000.0001) %MON dynstat_uvel_min   =  -2.8264287531564E-05
         (PID.TID 0000.0001) %MON dynstat_uvel_mean  =   9.1369201945671E-06
         (PID.TID 0000.0001) %MON dynstat_uvel_sd    =   1.6868439193567E-05
         (PID.TID 0000.0001) %MON dynstat_uvel_del2  =   8.4315445301916E-08

:filelink:`pkg/monitor` text can be readily parsed by the ``testreport`` script
to determine, somewhat crudely but quickly, how similar the output from
two experiments are when run on different platforms or before/after code
changes.

:filelink:`pkg/monitor` output can also be useful for quickly diagnosing
practical problems such as CFL limitations, model progress (through
iteration counts), and behavior within some packages that use it.

Using pkg/monitor
-----------------

As with most packages, :filelink:`pkg/monitor` can be turned on or off at compile
and/or run times using the ``packages.conf`` and ``data.pkg`` files.

The monitor output can be sent to the standard output channel, to an
:filelink:`pkg/mnc`–generated file, or to both simultaneously. For :filelink:`pkg/mnc` output,
the flag  ``monitor_mnc=.TRUE.`` should be set within the ``data.mnc`` file. For output to both ASCII and
:filelink:`pkg/mnc`, the flag ``outputTypesInclusive=.TRUE.`` should be set
within the ``PARM03`` section of the main ``data`` file.
It should be noted that the ``outputTypesInclusive`` flag will make
**ALL** kinds of output (that is, everything written by :filelink:`pkg/mdsio`,
:filelink:`pkg/mnc`, and :filelink:`pkg/monitor`) simultaneously active so it should be used
only with caution -– and perhaps only for debugging purposes.


.. tabularcolumns:: |\Y{.4}|L|L|


+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| CPP Flag Name                                 | Default | Description                                                                                                          |
+===============================================+=========+======================================================================================================================+
| :varlink:`MONITOR_TEST_HFACZ`                 | #undef  | disable use of hFacZ                                                                                                 |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+


Grid Generation
===============

The horizontal discretizations within MITgcm have been written to work
with many different grid types including:

-  cartesian coordinates

-  spherical polar (“latitude-longitude”) coordinates

-  general curvilinear orthogonal coordinates

The last of these, especially when combined with the domain
decomposition capabilities of MITgcm, allows a great degree of grid
flexibility. To date, general curvilinear orthogonal coordinates have
been used extensively in conjunction with
so-called “cubed sphere” grids. However, it is important to observe that
cubed sphere arrangements are only one example of what is possible with
domain-decomposed logically rectangular regions each containing
curvilinear orthogonal coordinate systems. Much more sophisticated
domains can be imagined and constructed.

In order to explore the possibilities of domain-decomposed curvilinear
orthogonal coordinate systems, a suite of grid generation software
called “SPGrid” (for SPherical Gridding) has been developed. SPGrid is a
relatively new facility and papers detailing its algorithms are in
preparation. Although SPGrid is new and rapidly developing, it has
already demonstrated the ability to generate some useful and interesting
grids.

This section provides a very brief introduction to SPGrid and shows some
early results. For further information, please contact the MITgcm
support list MITgcm-support@mitgcm.org.

Using SPGrid
------------

The SPGrid software is not a single program. Rather, it is a collection
of C++ code and `MATLAB <https://www.mathworks.com/>`_ scripts that can be used as a framework or
library for grid generation and manipulation. Currently, grid creation
is accomplished by either directly running `MATLAB <https://www.mathworks.com/>`_ scripts or by writing
a C++ “driver” program. The `MATLAB <https://www.mathworks.com/>`_ scripts are suitable for grids
composed of a single “face” (that is, a single logically rectangular
region on the surface of a sphere). The C++ driver programs are
appropriate for grids composed of multiple connected logically
rectangular patches. Each driver program is written to specify the
shape and connectivity of tiles and the preferred grid density (that is,
the number of grid cells in each logical direction) and edge locations
of the cells where they meet the edges of each face. The driver programs
pass this information to the SPGrid library, which generates the actual
grid and produces the output files that describe it.

Currently, driver programs are available for a few examples including
cubes, “lat-lon caps” (cube topologies that have conformal caps at the
poles and are exactly lat-lon channels for the remainder of the domain),
and some simple “embedded” regions that are meant to be used within
typical cubes or traditional lat-lon grids.

To create new grids, one may start with an existing driver program and
modify it to describe a domain that has a different arrangement. The
number, location, size, and connectivity of grid “faces” (the name used
for the logically rectangular regions) can be readily changed. Further,
the number of grid cells within faces and the location of the grid cells
at the face edges can also be specified.

SPGrid requirements
~~~~~~~~~~~~~~~~~~~

The following programs and libraries are required to build and/or run
the SPGrid suite:

-  `MATLAB <https://www.mathworks.com/>`_ is a run-time requirement since many of the generation
   algorithms have been written as `MATLAB <https://www.mathworks.com/>`_ scripts.

-  The `Geometric Tools Engine <https://geometrictools.com>`_  (a C++ library) is needed for the
   main “driver” code.

-  The `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ library is needed for file I/O.

-  The `Boost serialization library <http://www.boost.org/doc/libs/1_66_0/libs/serialization/doc/index.html>`_ is also used for I/O:

-  a typical Linux/Unix build environment including the make utility
   (preferably GNU Make) and a C++ compiler (SPGrid was developed with
   g++ v4.x).

Obtaining SPGrid
~~~~~~~~~~~~~~~~

The latest version can be obtained from:


Building SPGrid
~~~~~~~~~~~~~~~

The procedure for building is similar to many open source projects:

::

         tar -xf spgrid-0.9.4.tar.gz
         cd spgrid-0.9.4
         export CPPFLAGS="-I/usr/include/netcdf-3"
         export LDFLAGS="-L/usr/lib/netcdf-3"
         ./configure
         make

where the ``CPPFLAGS`` and ``LDFLAGS`` environment variables can be
edited to reflect the locations of all the necessary dependencies.
SPGrid is known to work on Fedora Core Linux (versions 4 and 5) and is
likely to work on most any Linux distribution that provides the needed
dependencies.

Running SPGrid
~~~~~~~~~~~~~~

Within the ``src`` sub-directory, various example driver programs exist.
These examples describe small, simple domains and can generate the input
files (formatted as either binary ``*.mitgrid`` or netCDF) used by
MITgcm.

One such example is called ``SpF_test_cube_cap`` and it can be run with
the following sequence of commands:

::

         cd spgrid-0.9.4/src
         make SpF_test_cube_cap
         mkdir SpF_test_cube_cap.d
         ( cd SpF_test_cube_cap.d && ln -s ../../scripts/*.m . )
         ./SpF_test_cube_cap

which should create a series of output files:

::

         SpF_test_cube_cap.d/grid_*.mitgrid
         SpF_test_cube_cap.d/grid_*.nc
         SpF_test_cube_cap.d/std_topology.nc

where the ``grid_.mitgrid`` and ``grid_.nc`` files contain the grid
information in binary and netCDF formats and the ``std_topology.nc``
file contains the information describing the connectivity (both
edge–edge and corner–corner contacts) between all the faces.

Example Grids
-------------

The following grids are various examples created with SPGrid.

Pre– and Post–Processing Scripts and Utilities
==============================================

There are numerous tools for pre-processing data, converting model
output and analysis written in `MATLAB <https://www.mathworks.com/>`_, Fortran (f77 and f90) and perl.
As yet they remain undocumented although many are self-documenting
(`MATLAB <https://www.mathworks.com/>`_ routines have “help” written into them).

Here we’ll summarize what is available but this is an ever growing
resource so this may not cover everything that is out there:

Utilities Supplied With the Model
---------------------------------

We supply some basic scripts with the model to facilitate conversion or
reading of data into analysis software.

utils/scripts
~~~~~~~~~~~~~

In the directory :filelink:`utils/scripts`,  :filelink:`joinds <utils/scripts/joinds>`
and :filelink:`joinmds <utils/scripts/joinmds>`
are perl scripts used to joining multi-part files created by
MITgcm. Use :filelink:`joinmds <utils/scripts/joinmds>`.
You will only need :filelink:`joinds <utils/scripts/joinds>` if you are
working with output older than two years (prior to c23).

utils/matlab
~~~~~~~~~~~~

In the directory :filelink:`utils/matlab` you will find
several `MATLAB <https://www.mathworks.com/>`_  scripts (``.m``
files). The principle script is :filelink:`rdmds.m <utils/matlab/rdmds.m>`, used for reading
the multi-part model output files into `MATLAB <https://www.mathworks.com/>`_ . Place the scripts in
your `MATLAB <https://www.mathworks.com/>`_  path or change the path appropriately,
then at the `MATLAB <https://www.mathworks.com/>`_ 
prompt type:

::

      >> help rdmds

to get help on how to use :filelink:`rdmds <utils/matlab/rdmds.m>`.

Another useful script scans the terminal output file for :filelink:`pkg/monitor`
information.

Most other scripts are for working in the curvilinear coordinate systems,
and as yet are unpublished and undocumented.

pkg/mnc utils
~~~~~~~~~~~~~

The following scripts and
utilities have been written to help manipulate `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ files:

Tile Assembly:
    A `MATLAB <https://www.mathworks.com/>`_ script
    :filelink:`mnc_assembly.m <utils/matlab/mnc_assembly.m>` is available for
    spatially “assembling” :filelink:`pkg/mnc` output. A convenience wrapper script
    called :filelink:`gluemnc.m <utils/matlab/gluemnc.m>` is also provided. Please use the
    `MATLAB <https://www.mathworks.com/>`_ help facility for more information.

gmt:
    As MITgcm evolves to handle more complicated domains and topologies,
    a suite of matlab tools is being written to more gracefully handle
    the model files. This suite is called “gmt” which refers to
    “generalized model topology” pre-/post-processing. Currently, this
    directory contains a `MATLAB <https://www.mathworks.com/>`_ script 
    :filelink:`gmt/rdnctiles.m <utils/matlab/gmt/rdnctiles.m>` that
    is able to read `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ files for any domain.
    Additional scripts are being created that will work with these
    fields on a per-tile basis.

Pre-Processing Software
-----------------------

There is a suite of pre-processing software for interpolating bathymetry
and forcing data, written by Adcroft and Biastoch. At some point, these
will be made available for download. If you are in need of such
software, contact one of them.

Potential Vorticity Matlab Toolbox
==================================

Author: Guillaume Maze

Introduction
------------

This section of the documentation describes a `MATLAB <https://www.mathworks.com/>`_  package that aims
to provide useful routines to compute vorticity fields (relative,
potential and planetary) and its related components. This is an offline
computation. It was developed to be used in mode water studies, so that
it comes with other related routines, in particular ones computing
surface vertical potential vorticity fluxes.

Equations
---------

Potential vorticity
~~~~~~~~~~~~~~~~~~~

The package computes the three components of the relative vorticity
defined by:

.. math::
   \begin{aligned}
     \omega &= \nabla \times {\bf U} = \left( \begin{array}{c} 
         \omega_x\\
         \omega_y\\
         \zeta
     \end{array}\right)
        \simeq &\left( \begin{array}{c} 
         -\frac{\partial v}{\partial z}\\
         -\frac{\partial u}{\partial z}\\
         \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y}
     \end{array}\right)\end{aligned}
   :label: pv_eq1

where we omitted the vertical velocity component (as done throughout the package).

The package then computes the potential vorticity as:

.. math::
   \begin{aligned}
   Q &= -\frac{1}{\rho} \omega\cdot\nabla\sigma_\theta\\
    &= -\frac{1}{\rho}\left(\omega_x \frac{\partial \sigma_\theta}{\partial x} + 
   \omega_y \frac{\partial \sigma_\theta}{\partial y} + 
   \left(f+\zeta\right) \frac{\partial \sigma_\theta}{\partial z}\right)\end{aligned}
   :label: pv_eq2

where :math:`\rho` is the density, :math:`\sigma_\theta` is the
potential density (both eventually computed by the package) and
:math:`f` is the Coriolis parameter.

The package is also able to compute the simpler planetary vorticity as:

.. math::
   \begin{aligned}
   Q_{spl} &=& -\frac{f}{\rho}\frac{\sigma_\theta}{\partial z}\end{aligned} 
   :label: pv_eq3

Surface vertical potential vorticity fluxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These quantities are useful in mode water studies because of the
impermeability theorem which states that for a given potential density
layer (embedding a mode water), the integrated PV only changes through
surface input/output.

Vertical PV fluxes due to frictional and diabatic processes are given
by:

.. math::
   J^B_z = -\frac{f}{h}\left( \frac{\alpha Q_{net}}{C_w}-\rho_0 \beta S_{net}\right)
   :label: pv_eq14a

.. math::
   J^F_z = \frac{1}{\rho\delta_e} \vec{k}\times\tau\cdot\nabla\sigma_m
  :label: pv_eq15a

These components can be computed with the package. Details on the
variables definition and the way these fluxes are derived can be found
in :numref:`notes_flux_form`.

We now give some simple explanations about these fluxes and how they can
reduce the PV value of an oceanic potential density layer.

Diabatic process
^^^^^^^^^^^^^^^^

Let’s take the PV flux due to surface buoyancy forcing from
:eq:`pv_eq14a` and simplify it as:

.. math::

   \begin{aligned}
     J^B_z &\simeq& -\frac{\alpha f}{hC_w} Q_{net}\end{aligned}

When the net surface heat flux :math:`Q_{net}` is upward, i.e., negative
and cooling the ocean (buoyancy loss), surface density will increase,
triggering mixing which reduces the stratification and then the PV.

.. math::
   \begin{aligned}
     Q_{net} &< 0 \phantom{WWW}\text{(upward, cooling)} \\
     J^B_z   &> 0 \phantom{WWW}\text{(upward)} \\
     -\rho^{-1}\nabla\cdot J^B_z &< 0 \phantom{WWW}\text{(PV flux divergence)} \\
     PV &\searrow \phantom{WWWi}\text{where } Q_{net}<0 \end{aligned}


Frictional process: “Down-front” wind-stress
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now let’s take the PV flux due to the “wind-driven buoyancy flux” from
:eq:`pv_eq15a` and simplify it as:

.. math::
   \begin{aligned}
     J^F_z &= \frac{1}{\rho\delta_e} \left( \tau_x\frac{\partial \sigma}{\partial y} - \tau_y\frac{\partial \sigma}{\partial x} \right) \\
     &\simeq \frac{1}{\rho\delta_e} \tau_x\frac{\partial \sigma}{\partial y} \end{aligned}

When the wind is blowing from the east above the Gulf Stream (a region
of high meridional density gradient), it induces an advection of dense
water from the northern side of the GS to the southern side through
Ekman currents. Then, it induces a “wind-driven” buoyancy lost and
mixing which reduces the stratification and the PV.

.. math::
   \begin{aligned}
    \vec{k}\times\tau\cdot\nabla\sigma &> 0 \phantom{WWW}\text{("Down-front" wind)} \\
    J^F_z &> 0 \phantom{WWW}\text{(upward)} \\
     -\rho^{-1}\nabla\cdot J^F_z &< 0 \phantom{WWW}\text{(PV flux divergence)} \\
     PV &\searrow \phantom{WWW}\text{where } \vec{k}\times\tau\cdot\nabla\sigma>0 \end{aligned}


Diabatic versus frictional processes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A recent debate in the community arose about the relative role of these
processes. Taking the ratio of :eq:`pv_eq14a` and
:eq:`pv_eq15a` leads to:

.. math::

   \begin{aligned}
     \frac{J^F_z}{J^B_Z} &=& \frac{ \frac{1}{\rho\delta_e} \vec{k}\times\tau\cdot\nabla\sigma }
     {-\frac{f}{h}\left( \frac{\alpha Q_{net}}{C_w}-\rho_0 \beta S_{net}\right)} \\
     &\simeq& \frac{Q_{Ek}/\delta_e}{Q_{net}/h} \nonumber\end{aligned}

where appears the lateral heat flux induced by Ekman currents:

.. math::

   \begin{aligned}
     Q_{Ek} &=& -\frac{C_w}{\alpha\rho f}\vec{k}\times\tau\cdot\nabla\sigma 
     \nonumber \\
     &=& \frac{C_w}{\alpha}\delta_e\vec{u_{Ek}}\cdot\nabla\sigma\end{aligned}

which can be computed with the package. In the aim of comparing both
processes, it will be useful to plot surface net and lateral
Ekman-induced heat fluxes together with PV fluxes.

Key routines
------------

-  **A_compute_potential_density.m**: Compute the potential density
   field. Requires the potential temperature and salinity (either total
   or anomalous) and produces one output file with the potential density
   field (file prefix is ``SIGMATHETA``). The routine uses :filelink:`utils/matlab/densjmd95.m`,
   a Matlab counterpart of the MITgcm built-in function to compute the
   density.

-  **B_compute_relative_vorticity.m**: Compute the three components
   of the relative vorticity defined in :eq:`pv_eq1`.
   Requires the two horizontal velocity components and produces three
   output files with the three components (files prefix are ``OMEGAX``,
   ``OMEGAY`` and ``ZETA``).

-  **C_compute_potential_vorticity.m**: Compute the potential
   vorticity without the negative ratio by the density. Two options are
   possible in order to compute either the full component (term into
   parenthesis in :eq:`pv_eq2` or the planetary component
   (:math:`f\partial_z\sigma_\theta` in :eq:`pv_eq3`). Requires
   the relative vorticity components and the potential density, and
   produces one output file with the potential vorticity (file prefix is
   ``PV`` for the full term and ``splPV`` for the planetary component).

-  **D_compute_potential_vorticity.m**: Load the field computed with
   and divide it by :math:`-\rho` to obtain the correct potential
   vorticity. Require the density field and after loading, overwrite the
   file with prefix ``PV`` or ``splPV``.

-  **compute_density.m**: Compute the density :math:`\rho` from the
   potential temperature and the salinity fields.

-  **compute_JFz.m**: Compute the surface vertical PV flux due to
   frictional processes. Requires the wind stress components, density,
   potential density and Ekman layer depth (all of them, except the wind
   stress, may be computed with the package), and produces one output
   file with the PV flux :math:`J^F_z` (see :eq:`pv_eq15a` and
   with ``JFz`` as a prefix.

-  **compute_JBz.m**: Compute the surface vertical PV flux due to
   diabatic processes as:

   .. math::
      \begin{aligned}
        J^B_z &=& -\frac{f}{h}\frac{\alpha Q_{net}}{C_w} \end{aligned}

   which is a simplified version of the full expression given in
   :eq:`pv_eq14a`. Requires the net surface heat flux and the
   mixed layer depth (of which an estimation can be computed with the
   package), and produces one output file with the PV flux :math:`J^B_z`
   and with JBz as a prefix.

-  **compute\_QEk.m**: Compute the horizontal heat flux due to Ekman
   currents from the PV flux induced by frictional forces as:

   .. math::
      \begin{aligned}
       Q_{Ek} &=& - \frac{C_w \delta_e}{\alpha f}J^F_z\end{aligned}

   Requires the PV flux due to frictional forces and the Ekman layer
   depth, and produces one output with the heat flux and with QEk as a
   prefix.

-  **eg\_main\_getPV**: A complete example of how to set up a master
   routine able to compute everything from the package.

Technical details
-----------------

File name
~~~~~~~~~

A file name is formed by three parameters which need to be set up as
global variables in `MATLAB <https://www.mathworks.com/>`_ before running any routines. They are:

-  the prefix, i.e., the variable name (``netcdf_UVEL`` for example). This
   parameter is specified in the help section of all diagnostic
   routines.

-  ``netcdf_domain``: the geographical domain.

-  ``netcdf_suff``: the netcdf extension (nc or cdf for example).

Then, for example, if the calling `MATLAB <https://www.mathworks.com/>`_ routine had set up:

::

    global netcdf_THETA netcdf_SALTanom netcdf_domain netcdf_suff
    netcdf_THETA    = 'THETA';
    netcdf_SALTanom = 'SALT';
    netcdf_domain   = 'north_atlantic';
    netcdf_suff     = 'nc';

the routine A_compute_potential_density.m to compute the potential
density field, will look for the files:

::

    THETA.north_atlantic.nc
    SALT.north_atlantic.nc

and the output file will automatically be:
``SIGMATHETA.north_atlantic.nc``.

Otherwise indicated, output file prefix cannot be changed.

Path to file
~~~~~~~~~~~~

All diagnostic routines look for input files in a subdirectory (relative
to the `MATLAB <https://www.mathworks.com/>`_ routine directory)
called ``./netcdf-files``, which in turn is
supposed to contain subdirectories for each set of fields. For example,
computing the potential density for the timestep 12H00 02/03/2005 will
require a subdirectory with the potential temperature and salinity files
like:

::

    ./netcdf-files/200501031200/THETA.north_atlantic.nc
    ./netcdf-files/200501031200/SALT.north_atlantic.nc

The output file ``SIGMATHETA.north\_atlantic.nc`` will be created in
``./netcdf-files/200501031200/``. All diagnostic routines take as argument
the name of the timestep subdirectory into ``./netcdf-files``.

Grids
~~~~~

With MITgcm numerical outputs, velocity and tracer fields may not be
defined on the same grid. Usually, ``UVEL`` and ``VVEL`` are defined on a C-grid
but when interpolated from a cube-sphere simulation they are defined on
a A-grid. When it is needed, routines allow to set up a global variable
which define the grid to use.

.. _notes_flux_form:

Notes on the flux form of the PV equation and vertical PV fluxes
----------------------------------------------------------------

Flux form of the PV equation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The conservative flux form of the potential vorticity equation is:

.. math::
   \begin{aligned}
   \frac{\partial \rho Q}{\partial t} + \nabla \cdot \vec{J} &=& 0 \end{aligned}
   :label: pv_eq4

where the potential vorticity :math:`Q` is given by :eq:`pv_eq2`.

The generalized flux vector of potential vorticity is:

.. math::
   \begin{aligned}
    \vec{J} &=& \rho Q \vec{u} + \vec{N_Q}\end{aligned}

which allows to rewrite :eq:`pv_eq4` as:

.. math::
   \begin{aligned}
   \frac{DQ}{dt} &=& -\frac{1}{\rho}\nabla\cdot\vec{N_Q}\end{aligned}
   :label: pv_eq5

where the non-advective PV flux :math:`\vec{N_Q}` is given by:

.. math::
   \begin{aligned}
   \vec{N_Q} &=& -\frac{\rho_0}{g}B\vec{\omega_a} + \vec{F}\times\nabla\sigma_\theta \end{aligned}
   :label: pv_eq6

Its first component is linked to the buoyancy forcing:

.. math::
   \begin{aligned}
    B &=& -\frac{g}{\rho_o}\frac{D \sigma_\theta}{dt} \end{aligned}

and the second one to the non-conservative body forces per unit mass:

.. math::
   \begin{aligned}
    \vec{F} &=& \frac{D \vec{u}}{dt} + 2\Omega\times\vec{u} + \nabla p \end{aligned}

Note that introducing :math:`B` into :eq:`pv_eq6` yields:

   .. math::
      \begin{aligned}
        \vec{N_Q} &=& \omega_a \frac{D \sigma_\theta}{dt} + \vec{F}\times\nabla\sigma_\theta\end{aligned}


Determining the PV flux at the ocean’s surface
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the context of mode water study, we are particularly interested in how
the PV may be reduced by surface PV fluxes because a mode water is
characterized by a low PV value. Considering the volume limited by two
:math:`iso-\sigma_\theta`, PV flux is limited to surface processes and
then vertical component of :math:`\vec{N_Q}`. It is supposed that
:math:`B` and :math:`\vec{F}` will only be non-zero in the mixed layer
(of depth :math:`h` and variable density :math:`\sigma_m`) exposed to
mechanical forcing by the wind and buoyancy fluxes through the ocean’s
surface.

Given the assumption of a mechanical forcing confined to a thin surface
Ekman layer (of depth :math:`\delta_e`, eventually computed by the
package) and of hydrostatic and geostrophic balances, we can write:

.. math::
   \begin{aligned}
     \vec{u_g} &=& \frac{1}{\rho f} \vec{k}\times\nabla p \\
     \frac{\partial p_m}{\partial z} &=& -\sigma_m g \\ 
     \frac{\partial \sigma_m}{\partial t} + \vec{u}_m\cdot\nabla\sigma_m &=& -\frac{\rho_0}{g}B \end{aligned}
   :label: pv_eq7

where:

.. math::
   \begin{aligned}
     \vec{u}_m &=& \vec{u}_g + \vec{u}_{Ek} + o(R_o) \end{aligned}
   :label: pv_eq8

is the full velocity field composed of the geostrophic current
:math:`\vec{u}_g` and the Ekman drift:

.. math::
  \begin{aligned}
     \vec{u}_{Ek} &=& -\frac{1}{\rho f}\vec{k}\times\frac{\partial \tau}{\partial z}\end{aligned}
  :label: pv_eq9

(where :math:`\tau` is the wind stress) and last by other ageostrophic
components of :math:`o(R_o)` which are neglected.

Partitioning the buoyancy forcing as:

.. math::
   \begin{aligned}
     B &=& B_g + B_{Ek}\end{aligned}
   :label: pv_eq10

and using :eq:`pv_eq8` and :eq:`pv_eq9`, :eq:`pv_eq7` becomes:

.. math::
   \begin{aligned}
    \frac{\partial \sigma_m}{\partial t} + \vec{u}_g\cdot\nabla\sigma_m &=& -\frac{\rho_0}{g} B_g\end{aligned}

revealing the “wind-driven buoyancy forcing”:

.. math::
   \begin{aligned}
     B_{Ek} &=& \frac{g}{\rho_0}\frac{1}{\rho f}\left(\vec{k}\times\frac{\partial \tau}{\partial z}\right)\cdot\nabla\sigma_m\end{aligned}

Note that since:

.. math::
   \begin{aligned}
     \frac{\partial B_g}{\partial z} &=& \frac{\partial}{\partial z}\left(-\frac{g}{\rho_0}\vec{u_g}\cdot\nabla\sigma_m\right)
     = -\frac{g}{\rho_0}\frac{\partial \vec{u_g}}{\partial z}\cdot\nabla\sigma_m 
     = 0\end{aligned}

:math:`B_g` must be uniform throughout the depth of the mixed layer and
then being related to the surface buoyancy flux by integrating
:eq:`pv_eq10` through the mixed layer:

.. math::
   \begin{aligned}
     \int_{-h}^0B\,dz &=\, hB_g + \int_{-h}^0B_{Ek}\,dz  \,=& \mathcal{B}_{in}\end{aligned}
   :label: pv_eq11

where :math:`\mathcal{B}_{in}` is the vertically integrated surface buoyancy (in)flux:

.. math::
   \begin{aligned}
     \mathcal{B}_{in} &=& \frac{g}{\rho_o}\left( \frac{\alpha Q_{net}}{C_w} - \rho_0\beta S_{net}\right)\end{aligned}
   :label: pv_eq12

with :math:`\alpha\simeq 2.5\times10^{-4}\, \text{K}^{-1}` the thermal
expansion coefficient (computed by the package otherwise),
:math:`C_w=4187 \text{ J kg}^{-1}\text{K}^{-1}` the specific heat of seawater,
:math:`Q_{net}\text{ [W/m}^{-2}]` the net heat surface flux (positive downward,
warming the ocean), :math:`\beta\text{ [psu}^{-1}]` the saline contraction
coefficient, and :math:`S_{net}=S*(E-P)\text{ [psu m s}^{-1}]` the net
freshwater surface flux with :math:`S\text{ [psu}]` the surface salinity and
:math:`(E-P)\text{ [m/s]}` the fresh water flux.

Introducing the body force in the Ekman layer:

.. math::
   \begin{aligned}
     F_z &=& \frac{1}{\rho}\frac{\partial \tau}{\partial z}\end{aligned}

the vertical component of :eq:`pv_eq6` is:

.. math::
   \begin{aligned}
     \vec{N_Q}_z &= -\frac{\rho_0}{g}(B_g+B_{Ek})\omega_z 
     + \frac{1}{\rho}
     \left( \frac{\partial \tau}{\partial z}\times\nabla\sigma_\theta \right)\cdot\vec{k} \\
     &= -\frac{\rho_0}{g}B_g\omega_z
     -\frac{\rho_0}{g}
     \left(\frac{g}{\rho_0}\frac{1}{\rho f}\vec{k}\times\frac{\partial \tau}{\partial z}
       \cdot\nabla\sigma_m\right)\omega_z
     + \frac{1}{\rho}
     \left( \frac{\partial \tau}{\partial z}\times\nabla\sigma_\theta \right)\cdot\vec{k}\\
     &= -\frac{\rho_0}{g}B_g\omega_z
     + \left(1-\frac{\omega_z}{f}\right)\left(\frac{1}{\rho}\frac{\partial \tau}{\partial z}
                   \times\nabla\sigma_\theta \right)\cdot\vec{k}\end{aligned}

and given the assumption that :math:`\omega_z\simeq f`, the second term
vanishes and we obtain:

.. math::
   \begin{aligned}
     \vec{N_Q}_z &=& -\frac{\rho_0}{g}f B_g\end{aligned}
   :label: pv_eq13

Note that the wind-stress forcing does not appear explicitly here but
is implicit in :math:`B_g` through :eq:`pv_eq11`: the buoyancy
forcing :math:`B_g` is determined by the difference between the
integrated surface buoyancy flux :math:`\mathcal{B}_{in}` and the
integrated “wind-driven buoyancy forcing”:

.. math::

   \begin{aligned}
     B_g &= \frac{1}{h}\left( \mathcal{B}_{in} - \int_{-h}^0B_{Ek}dz \right)  \\
     &= \frac{1}{h}\frac{g}{\rho_0}\left( \frac{\alpha Q_{net}}{C_w} - \rho_0 \beta S_{net}\right)
     - \frac{1}{h}\int_{-h}^0 
     \frac{g}{\rho_0}\frac{1}{\rho f}\vec{k}\times \frac{\partial \tau}{\partial z} \cdot\nabla\sigma_m dz \\
     &= \frac{1}{h}\frac{g}{\rho_0}\left( \frac{\alpha Q_{net}}{C_w} - \rho_0 \beta S_{net}\right)  
     - \frac{g}{\rho_0}\frac{1}{\rho f \delta_e}\vec{k}\times\tau\cdot\nabla\sigma_m\end{aligned}

Finally, from :eq:`pv_eq6`, the vertical surface flux of PV may
be written as:

.. math::
   \begin{aligned}
     \vec{N_Q}_z &= J^B_z + J^F_z  \\
     J^B_z &= -\frac{f}{h}\left( \frac{\alpha Q_{net}}{C_w}-\rho_0 \beta S_{net}\right) \\
     J^F_z &= \frac{1}{\rho\delta_e} \vec{k}\times\tau\cdot\nabla\sigma_m \end{aligned}

.. _sub_outp_pkg_flt:

pkg/flt – Simulation of float / parcel displacements
====================================================

.. include:: flt.rst

