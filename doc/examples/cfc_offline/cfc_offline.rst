.. _sec_eg_offline_cfc:

Offline Experiments
===================

  (in directory: :filelink:`verification/tutorial_cfc_offline/`)

This document describes two experiments using the offline form of
the MITgcm.

Overview
--------

The first experiment demonstrates use of the offline form of the MITgcm to
study advection of a passive tracer. Time-averaged flow-fields and
mixing coefficients, deriving from a prior online run, are re-used
leaving only the tracer equation to be integrated. This first experiment's
run configuration is specified in directory :filelink:`verification/tutorial_cfc_offline/input_tutorial`.

Figure *— missing figure —* shows a movie of tracer being advected using
the offline package of the MITgcm. In the top panel the frames of the
movie show the monthly surface evolution of an initially local source of
passive tracer. In the lower panel, the frames of the movie show the
changing monthly surface evolution where the initial tracer field had a
global distribution.

The second experiment, a more complicated example exploring contamination of the global ocean
through surface exposure to CFCs during the last century, is described after this more simple first example.
The run configuration for this second experiment is specified in directory :filelink:`verification/tutorial_cfc_offline/input`.

Time-stepping of tracers
------------------------

See :numref:`tracer_eqns` and :numref:`advection_schemes` for details of available tracer
time-stepping schemes and their characteristics.

Code Configuration
------------------

The experiment files

-  :filelink:`verification/tutorial_cfc_offline/input_tutorial/data`

-  :filelink:`verification/tutorial_cfc_offline/input_tutorial/data.off`

-  :filelink:`verification/tutorial_cfc_offline/input_tutorial/data.pkg`

-  :filelink:`verification/tutorial_cfc_offline/input_tutorial/data.ptracers`

-  :filelink:`verification/tutorial_cfc_offline/input_tutorial/eedata`

-  :filelink:`verification/tutorial_cfc_offline/code/packages.conf`

-  :filelink:`verification/tutorial_cfc_offline/code/PTRACERS_SIZE.h`

-  :filelink:`verification/tutorial_cfc_offline/code/GMREDI_OPTIONS.h`

-  :filelink:`verification/tutorial_cfc_offline/code/SIZE.h`

contain the code customizations and parameter settings required to run
the example. In addition the following binary data files are required:

-  ``input/depth_g77.bin``

- ``pickup_ptracers.0004269600``, ``pickup_ptracers.0004269600.meta``

-  binary files in :filelink:`verification/tutorial_cfc_offline/input/input_off`

File :filelink:`input_tutorial/data <verification/tutorial_cfc_offline/input_tutorial/data>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input_tutorial/data
    :linenos:
    :caption: verification/tutorial_cfc_offline/input_tutorial/data

This file specifies the main parameters
for the experiment.

-  Lines 18-19,

   ::

        nIter0  = 4269600,
        nTimeSteps = 4,

:varlink:`nIter0` and :varlink:`nTimesteps` control the start time and the length of the run
(in timesteps). Given at :varlink:`nIter0` is non-zero, the model requires appropriate
pickup files to be present in the run directory. For testing purposes, the model has been
prescribed to run for 4 timesteps; for a longer run, increase :varlink:`nTimesteps`.

-  Line 20,

   ::

        deltaTtracer= 43200.0,

:varlink:`deltaTtracer` is the tracer timestep in seconds, in this case, 12 hours
(43200 seconds = 12 hours). Note that :varlink:`deltatTracer` must be specified in
:filelink:`input_tutorial/data <verification/tutorial_cfc_offline/input_tutorial/data>` as well as
specified in  :varlink:`deltaToffline` in :filelink:`input_tutorial/data.off <verification/tutorial_cfc_offline/input_tutorial/data.off>`.

-  Line 21,

   ::

        deltaTClock= 43200.0,

When using the MITgcm in offline mode, :varlink:`deltaTClock` (an internal model
counter) should be made equal to the value assigned to :varlink:`deltatTtracer`.

-  Line 27,

   ::

        periodicExternalForcing=.TRUE.,

:varlink:`periodicExternalForcing` is a flag telling the model whether to
cyclically re-use forcing data where there is external forcing
(see :numref:`tut_offline_example2` below). Where there is no external forcing, as
here, but where there is to be cyclic re-use of the offline flow and
mixing fields, :varlink:`periodicExternalForcing` must be assigned the value ``.TRUE.``.

-  Line 28,

   ::

        externForcingPeriod=2592000.,

:varlink:`externForcingPeriod` specifies the period of the external forcing data in
seconds. In the absence of external forcing, as in this example, it must
be made equal to the value of :varlink:`externForcingPeriod` in
:filelink:`input_tutorial/data.off <verification/tutorial_cfc_offline/input_tutorial/data.off>`,
in this case, monthly (2592000 seconds = 1 month).

-  Line 29,

   ::

        externForcingCycle=31104000.,

:varlink:`externForcingCycle` specifies the duration of the external forcing data
cycle in seconds. In the absence of external forcing, as in this
example, it must be made equal to the value of :varlink:`externForcingCycle` in
:filelink:`input_tutorial/data.off <verification/tutorial_cfc_offline/input_tutorial/data.off>`,
in this case, the cycle is one year (31104000 seconds = 1 year).

-  Line34,

   ::

        usingSphericalPolarGrid=.TRUE.,

This line requests that the simulation be performed in a spherical polar
coordinate system. It affects the interpretation of grid input
parameters and causes the grid generation routines to initialize an
internal grid based on spherical polar geometry.

-  Lines 35-37,

   ::

        delR=  50.,  70., 100., 140., 190.,
              240., 290., 340., 390., 440.,
              490., 540., 590., 640., 690.,

This line sets the vertical grid spacing between each :math:`z`-coordinate line
in the discrete grid. Here the total model depth is 5200 m.

-  Line 38,

   ::

        ygOrigin=-90.,

This line sets the southern boundary of the modeled domain to
-90\ :sup:`o` latitude N (= 90\ :sup:`o`\  S). This value
affects both the generation of the locally orthogonal grid that the
model uses internally and affects the initialization of the Coriolis
force. Note: it is not required to set a longitude boundary, since the
absolute longitude does not alter the kernel equation discretization.

-  Line 39,

   ::

        dxSpacing=2.8125,

This line sets the horizontal grid spacing between each :math:`y`-coordinate
line in the discrete grid to 2.8125\ :sup:`o` in longitude.

-  Line 40,

   ::

        dySpacing=2.8125,

This line sets the vertical grid spacing between each :math:`x`-coordinate line
in the discrete grid to 2.8125\ :sup:`o` in latitude.

-  Line 45,

   ::

       bathyFile='depth_g77.bin',

This line specifies the name of the file
from which the domain bathymetry is read. This file contains a
2-D (:math:`x,y`) map of (assumed 64-bit) binary numbers
giving the depth of the model at each grid cell, ordered with the :math:`x`
coordinate varying fastest. The points are ordered from low coordinate
to high coordinate for both axes. The units and orientation of the
depths in this file are the same as used in the MITgcm code. In this
experiment, a depth of 0 m indicates land.

File :filelink:`input_tutorial/data.off <verification/tutorial_cfc_offline/input_tutorial/data.off>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input_tutorial/data.off
    :linenos:
    :caption: verification/tutorial_cfc_offline/input_tutorial/data.off

:filelink:`input_tutorial/data.off <verification/tutorial_cfc_offline/input_tutorial/data.off>`
provides the MITgcm offline package with package
specific parameters. Specifically, it contains the location (relative
to the run directory) and prefix of files describing the flow field
(:varlink:`UvelFile`, :varlink:`VvelFile`, :varlink:`WvelFile`) and the corresponding convective mixing
coefficients (:varlink:`ConvFile`) which together prescribe the 3-D,
time varying dynamic system within which the offline model will advect
the tracer.

-  Lines 2-4,8

   ::

         UvelFile= '../input/input_off/uVeltave',
         VvelFile= '../input/input_off/vVeltave',
         WvelFile= '../input/input_off/wVeltave',
         ConvFile= '../input/input_off/Convtave',

In the example the offline data is located in the sub-directory
:filelink:`verification/tutorial_cfc_offline/input/input_off`.
In this directory are fields describing the velocity
and convective mixing histories of a prior forward integration of the
MITgcm, required for the offline package. Based on the values of :varlink:`deltaToffline`,
:varlink:`offlineForcingPeriod` and :varlink:`offlineForcingCycle` specified in
:filelink:`verification/tutorial_cfc_offline/input/input_off`,
since :varlink:`offlineForcingCycle` corresponds to twelve forcing
periods :varlink:`offlineForcingPeriod` and since :varlink:`offlineIter0` is zero, there
needs to be twelve :varlink:`uVeltave`, twelve :varlink:`vVeltave`, twelve :varlink:`wVeltave` and twelve ``Convtave`` files
each having a 10 digit sequence identifier between 0000000001 to
0000000012, that is, a total of 48 files.

-  Line 12,

   ::

         offlineIter0=4248000,

:varlink:`offlineIter0`, here specified to be 4248000 timesteps, corresponds to the
timestep at which the tracer model is initialized. Note that
:varlink:`offlineIter0` and :varlink:`nIter0` (set in
:filelink:`input_tutorial/data <verification/tutorial_cfc_offline/input_tutorial/data>`)
need not be the same.

-  Line 13,

   ::

         deltaToffline=43200.,

:varlink:`deltatToffline` sets the timestep associated with the offline model data
in seconds, here 12 hours (43200 seconds = 12 hours).

-  Line 14,

   ::

         offlineForcingPeriod=43200.,

:varlink:`offlineForcingPeriod` sets the forcing period associated with the offline
model data in seconds.

-  Line 15,

   ::

         offlineForcingCycle=518400.,

:varlink:`offlineForcingCycle` sets the forcing cycle length associated with the
offline model data in seconds. In this example the offline forcing cycle
is 6 days, or twelve offline forcing periods. Together :varlink:`deltatToffline`,
:varlink:`offlineForcingPeriod` and :varlink:`offlineForcingCycle` determine the value of the
ten digit sequencing tag the model expects files in
:filelink:`input_tutorial/data.off <verification/tutorial_cfc_offline/input_tutorial/data.off>`
to have.

File :filelink:`input_tutorial/data.pkg <verification/tutorial_cfc_offline/input_tutorial/data.pkg>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input_tutorial/data.pkg
    :linenos:
    :caption: verification/tutorial_cfc_offline/input_tutorial/data.pkg

This file specifies which
MITgcm packages are to be used.

-  Line 4,

   ::

        usePTRACERS=.TRUE.,

:varlink:`usePTRACERS` is a flag invoking :filelink:`pkg/ptracers` which is responsible
for the advection of the tracer within the model.

File :filelink:`input_tutorial/data.ptracers <verification/tutorial_cfc_offline/input_tutorial/data.ptracers>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input_tutorial/data.ptracers
    :linenos:
    :caption: verification/tutorial_cfc_offline/input_tutorial/data.ptracers

This file provides the
MITgcm ptracers package with package specific parameters, prescribing
the nature of the the tracer/tracers as well as the variables associated
with their advection.

-  Line 2,

   ::

        PTRACERS_numInUse=2,

:varlink:`PTRACERS_numInUse` tells the model how many separate tracers are to be
advected, in this case 2. Note: The value of :varlink:`PTRACERS_numInUse` must
agree with the value specified in :filelink:`code/PTRACERS_SIZE.h <verification/tutorial_cfc_offline/code/PTRACERS_SIZE.h>`
(see :ref:`below <tut_offline_ptracers_size>`).

-  Line 3,

   ::

        PTRACERS_Iter0= 4248000,

:varlink:`PTRACERS_Iter0` specifies the iteration at which the tracer is to be
introduced.

-  Lines 6 and 13,

   ::

        PTRACERS_advScheme(1)=77,

:varlink:`PTRACERS_advScheme`\ (n) identifies which advection scheme will be used
for tracer n, where n is the number of the tracer up to
:varlink:`PTRACERS_numInUse`. See :numref:`advection_schemes`
to identify the numerical codes used to specify different advection
schemes (e.g. centered 2nd order, 3rd order upwind) as well as details
of each.

-  Lines 7 and 14,

   ::

        PTRACERS_diffKh(1)=1.E3,

:varlink:`PTRACERS_diffKh`\ (n) is the horizontal diffusion coefficient for tracer
n, where n is the number of the tracer up to :varlink:`PTRACERS_numInUse`.

-  Lines 8 and 15,

   ::

        PTRACERS_diffKr(1)=5.E-5,

:varlink:`PTRACERS_diffKr`\ (n) is the vertical diffusion coefficient for tracer n,
where n is the number of the tracer up to :varlink:`PTRACERS_numInUse`.

-  Lines 11 and 18,

   ::

        PTRACERS_initialFile(1)=' ',

:varlink:`PTRACERS_initialFile`\ (n) identifies the initial tracer field to be
associated with tracer n, where n is the number of the tracer up to
PTRACERS\_numInUse. Note that no initial file is specified here.

Note :filelink:`input_tutorial/data.ptracers <verification/tutorial_cfc_offline/input_tutorial/data.ptracers>`
requires a set of entries for each tracer.

File :filelink:`input_tutorial/eedata <verification/tutorial_cfc_offline/input_tutorial/eedata>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File :filelink:`code/packages.conf <verification/tutorial_cfc_offline/code/packages.conf>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/code/packages.conf
    :linenos:
    :caption: verification/tutorial_cfc_offline/code/packages.conf

This file is used to invoke the model components required for a
particular implementation of the MITgcm.

.. _tut_offline_ptracers_size:

File :filelink:`code/PTRACERS_SIZE.h <verification/tutorial_cfc_offline/code/PTRACERS_SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/code/PTRACERS_SIZE.h
    :linenos:
    :caption: verification/tutorial_cfc_offline/code/PTRACERS_SIZE.h

-  Line 16,

   ::

            PARAMETER(PTRACERS_num = 2 )

This line sets the parameters :varlink:`PTRACERS_num` (the number of tracers to be
integrated) to 2 (in agreement with :filelink:`input_tutorial/data.ptracers <verification/tutorial_cfc_offline/input_tutorial/data.ptracers>`).

File :filelink:`code/SIZE.h <verification/tutorial_cfc_offline/code/SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_cfc_offline/code/SIZE.h

Several lines are customized in this file for the current experiment:

-  Line 45,

   ::

        sNx=64,

   this line sets the lateral domain extent in grid points for the axis
   aligned with the :math:`x`-coordinate.

-  Line 46,

   ::

        sNy=64,

   this line sets the lateral domain extent in grid points for the axis
   aligned with the :math:`y`-coordinate.

-  Line 55,

   ::

        Nr=15,

   this line sets the vertical domain extent in grid points.

Running the Experiment
----------------------

In your run directory, as per usual, a copy of all files from the input directory (here, :filelink:`input_tutorial/ <verification/tutorial_cfc_offline/input_tutorial/>`)
are required. In addition, you will also need to copy ``.data`` and ``.meta`` files from directory  :filelink:`input/input_off <verification/tutorial_cfc_offline/input/input_off>`.

.. _tut_offline_example2:

A more complicated example
--------------------------

The previous example demonstrated simple advection of a passive tracer using
the offline form of the MITgcm. Now we present a more complicated
example in which the model is used to explore contamination of the
global ocean through surface exposure to CFCs during the last century.
In invoking packages :filelink:`pkg/gchem`, :filelink:`pkg/gmredi` and :filelink:`pkg/cfc` it provides a starting point
and template for more complicated offline modeling, involving as it does
surface forcing through wind and ice fields, more sophisticated mixing,
and a time-varying forcing function.

The run configuration for this experiment resides under the directory
:filelink:`verification/tutorial_cfc_offline/input/` (the code configuration is the same as in the first example,
so the same model executable can be used, i.e., no need to re-compile). The  files

-  :filelink:`verification/tutorial_cfc_offline/input/data`

-  :filelink:`verification/tutorial_cfc_offline/input/data.off`

-  :filelink:`verification/tutorial_cfc_offline/input/data.pkg`

-  :filelink:`verification/tutorial_cfc_offline/input/data.ptracers`

-  :filelink:`verification/tutorial_cfc_offline/input/data.gmredi`

-  :filelink:`verification/tutorial_cfc_offline/input/data.gchem`

-  :filelink:`verification/tutorial_cfc_offline/input/data.cfc`

-  :filelink:`verification/tutorial_cfc_offline/input/eedata`

contain all the parameter settings required.

File :filelink:`input/data <verification/tutorial_cfc_offline/input/data>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input/data
    :linenos:
    :caption: verification/tutorial_cfc_offline/input/data

A single line must be added (under ``PARM01``, line 21) from the previous example

::

     &PARM01
     implicitDiffusion=.TRUE.,
     &

When :filelink:`pkg/gmredi` is used, the flag :varlink:`implicitDiffusion` must be assigned
the value ``.TRUE.``

In this example the starting timestep nIter0 is set to 4269600 requiring
model access to pickup files with the suffix 0004269600. The model will
run for 4 timesteps (:varlink:`nTimeSteps` = 4). In this case the frequencies with
which permanent and rolling checkpoints (:varlink:`pChkptFreq` and :varlink:`chkptFreq`) have
been set is sufficiently long to ensure that only one from the last
timestep will be written. This is also true of the values that have been
assigned to the frequency with which dumps are written (:varlink:`dumpFreq`) and
time averaging (:varlink:`taveFreq`) is performed. However, since the model always
dumps the state of the model when it stops without error, a dump will be
written with suffix 0004269604 upon completion.

File :filelink:`input/data.off <verification/tutorial_cfc_offline/input/data.off>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input/data.off
    :linenos:
    :caption: verification/tutorial_cfc_offline/input/data.off

This file specifies the prefixes
and locations of additional input files required to run the offline
model. Note that directory :filelink:`input/input_off <verification/tutorial_cfc_offline/input/input_off>`
contains only as many offline files
as are required to successfully run for 4 timesteps. Where the GMREDI
scheme was used in the forward run, as here, package GMREDI must again
be invoked when running offline. In this example, tracer is specified as
having been introduced with a non-zero starttime, at timestep 4248000.

File :filelink:`input/data.pkg <verification/tutorial_cfc_offline/input/data.pkg>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input/data.pkg
    :linenos:
    :caption: verification/tutorial_cfc_offline/input/data.pkg

This file specifies which
MITgcm packages are to be used. It now invokes
additional packages :filelink:`pkg/gmredi` and :filelink:`pkg/gchem`.

File :filelink:`input/data.ptracers <verification/tutorial_cfc_offline/input/data.ptracers>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input/data.ptracers
    :linenos:
    :caption: verification/tutorial_cfc_offline/input/data.ptracers

This file specifies
parameters associated with the CFC11 and CFC12 tracer fields advected in
this example.

-  Line 3,

   ::

         PTRACERS_Iter0= 4248000,

In this example the tracers were introduced at iteration 4248000.

-  Lines 12 and 21,

   ::

         PTRACERS_diffKh(n)=0.E3,

Since package GMREDI is being used, regular horizontal diffusion is set
to zero.

-  Lines 14-15 and 23-24,

   ::

         PTRACERS_useGMRedi(n)=.TRUE. ,
         PTRACERS_useKPP(n)=.FALSE. ,

Setting flag :varlink:`PTRACERS_useGMRedi`\ (n) to ``.TRUE.`` identifies that :filelink:`/pkg/gmredi`
is to be used. Setting flag :varlink:`PTRACERS_useKPP`\ (n) to ``.FALSE.``
explicitly turns off KPP mixing.

-  Lines 16 and 25,

   ::

        PTRACERS_initialFile(n)=' ',

Since this is a ‘pickup’ run the initial tracer files
:varlink:`PTRACERS_initialFile` are not needed.
The model will obtain the tracer state from
``pickup_ptracers.0004269600.data``

File :filelink:`input/data.gchem <verification/tutorial_cfc_offline/input/data.gchem>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input/data.gchem
    :linenos:
    :caption: verification/tutorial_cfc_offline/input/data.gchem

This file specifies the parameters
used in :filelink:`/pkg/gchem`.

File :filelink:`input/data.gmredi <verification/tutorial_cfc_offline/input/data.gmredi>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_cfc_offline/input/data.gmredi
    :linenos:
    :caption: verification/tutorial_cfc_offline/input/data.gmredi

This file specifies parameters required for :filelink:`/pkg/gmredi`.

File :filelink:`input/cfc1112.atm <verification/tutorial_cfc_offline/input/cfc1112.atm>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a ASCII data file containing the CFC source
functions over the northern and southern hemispheres annually from 1931
through 1998.

Running the Experiment
~~~~~~~~~~~~~~~~~~~~~~

The model is run as before.
