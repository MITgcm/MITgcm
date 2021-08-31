.. _sub_phys_pkg_rbcs:

RBCS Package
------------

.. _ssub_phys_pkg_rbcs_intro:

Introduction
++++++++++++

:filelink:`pkg/rbcs` provides the flexibility to relax fields (temperature,
salinity, ptracers, horizontal velocities) in any 3-D location, thus can be
used to implement a sponge layer, or a “source” anywhere in the domain, among
other uses.

For a field (:math:`T`) at every grid point the tendency is modified so that:

.. math:: \frac{dT}{dt}=\frac{dT}{dt} - \frac{M_{rbc}}{\tau_T} (T-T_{rbc})

where :math:`M_{rbc}` is a 3-D mask (no time dependence) between 0
and 1. Values are relaxed to a field given by :math:`T_{rbc}`, which can be set
to vary in time.
Where :math:`M_{rbc}(x,y,z) = 1.0`, the relaxing timescale is :math:`\tau_T`;
where :math:`M_{rbc}(x,y,z) = 0.0`, there is no relaxing.  If
:math:`M_{rbc}(x,y,z) = 0.5` then relaxation occurs but at a timescale of
:math:`2\,\tau_T`.

A separate mask can be used for :math:`\theta, S` and ptracers and each of
these can be relaxed or not and can have its own timescale
:math:`\tau_T`. These are set in ``data.rbcs`` (see below).

Key subroutines and parameters
++++++++++++++++++++++++++++++

The only compile-time parameter you are likely to have to change is in
:filelink:`RBCS_SIZE.h <pkg/rbcs/RBCS_SIZE.h>`, the number of masks,
PARAMETER( :varlink:`maskLEN` = 3 ) by default, see below.

:numref:`tab_phys_pkg_rbcs_runtime_flags` summarizes the runtime flags that are
set in ``data.rbcs``, and their default values.

.. tabularcolumns:: |\Y{.285}|\Y{.09}|\Y{.105}|\Y{.545}|

.. table:: RBCS runtime parameters
   :name: tab_phys_pkg_rbcs_runtime_flags

   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   |           Flag/Parameter           | Group  |        Default         |                                                                                   Description                                                                |
   +====================================+========+========================+==============================================================================================================================================================+
   | :varlink:`rbcsForcingPeriod`       | PARM01 | 0.0                    | time interval between forcing fields (in seconds), zero means constant-in-time forcing                                                                       |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`rbcsForcingCycle`        | PARM01 | 0.0                    | repeat cycle of forcing fields (in seconds), zero means non-cyclic forcing                                                                                   |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`rbcsForcingOffset`       | PARM01 | 0.0                    | time offset of forcing fields (in seconds), relative to time averages starting at :math:`t=0`,                                                               |
   |                                    |        |                        | i.e., the first forcing record/file is placed at (:varlink:`rbcsForcingOffset` + :varlink:`rbcsForcingPeriod`)/2; see below for examples                     |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`rbcsSingleTimeFiles`     | PARM01 | FALSE                  | if ``.TRUE.``, forcing fields are given one file per :varlink:`rbcsForcingPeriod`                                                                            |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`deltaTrbcs`              | PARM01 | :varlink:`deltaTclock` | time step used to compute the iteration numbers for :varlink:`rbcsSingleTimeFiles` = ``.TRUE.``                                                              |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`rbcsVanishingTime`       | PARM01 | 0.0                    | if :varlink:`rbcsVanishingTime` > 0, the relaxation strength reduces linearly to vanish at :varlink:`myTime` == :varlink:`rbcsVanishingTime`                 |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`rbcsIter0`               | PARM01 | 0                      | shift in iteration numbers used to label files if :varlink:`rbcsSingleTimeFiles` = ``.TRUE.`` (see below for examples)                                       |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`useRBCtemp`,             | PARM01 | FALSE                  | whether to use RBCS for T/S/U/V                                                                                                                              |
   | :varlink:`useRBCsalt`,             |        |                        |                                                                                                                                                              |
   | :varlink:`useRBCuVel`,             |        |                        |                                                                                                                                                              |
   | :varlink:`useRBCvVel`              |        |                        |                                                                                                                                                              |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`tauRelaxT`,              | PARM01 | 0.0                    | timescales (in seconds) for relaxing T/S/U/V (:math:`\tau_T` in equation above); required if the corresponding ``useRBCxxx`` is ``.TRUE.``                   |
   | :varlink:`tauRelaxS`,              |        |                        |                                                                                                                                                              |
   | :varlink:`tauRelaxU`,              |        |                        |                                                                                                                                                              |
   | :varlink:`tauRelaxV`               |        |                        |                                                                                                                                                              |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`relaxMaskFile` (irbc)    | PARM01 | :kbd:`' '`             | filename of 3-D file with mask (:math:`M_{rbc}` above), requires a file for each irbc (1=temperature, 2=salinity, 3=ptracer1, 4=ptracer2, etc);              |
   |                                    |        |                        | if :varlink:`maskLEN` is less than the number of tracers, then :code:`relaxMaskFile(maskLEN)` is used for all remaining tracers                              |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`relaxMaskUFile`,         | PARM01 | :kbd:`' '`             | filename of 3-D file with mask for U/V.                                                                                                                      |
   | :varlink:`relaxMaskVFile`          |        |                        |                                                                                                                                                              |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`relaxTFile`,             | PARM01 | :kbd:`' '`             | name of file with 3-D relaxation field values (:math:`T_{rbc}` in equation above);                                                                           |
   | :varlink:`relaxSFile`,             |        |                        | if :varlink:`rbcsSingleTimeFiles` = ``.FALSE.``, it must have one record for each forcing period,                                                            |
   | :varlink:`relaxUFile`,             |        |                        | otherwise there must be a separate file for each period with a 10-digit iteration number appended                                                            |
   | :varlink:`relaxVFile`              |        |                        | to the file name (see :numref:`tab_phys_pkg_rbcs_timing` and examples below)                                                                                 |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`useRBCpTrNum` (iTrc)     | PARM02 | FALSE                  | if ``.TRUE.``,  use RBCS for the corresponding passive tracer                                                                                                |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`tauRelaxPTR` (iTrc)      | PARM02 | 0.0                    | relaxing timescale (seconds) for the corresponding ptracer                                                                                                   |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+
   | :varlink:`relaxPtracerFile` (iTrc) | PARM02 | :kbd:`' '`             | file with relaxation field for the corresponding ptracer                                                                                                     |
   +------------------------------------+--------+------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------+

Timing of relaxation forcing fields
+++++++++++++++++++++++++++++++++++

For constant-in-time relaxation, set :varlink:`rbcsForcingPeriod` =0. For
time-varying relaxation, :numref:`tab_phys_pkg_rbcs_timing` illustrates the
relation between model time and forcing fields (either records in one big file
or, for :varlink:`rbcsSingleTimeFiles` = ``.TRUE.`` , individual files labeled
with an iteration number). With :varlink:`rbcsSingleTimeFiles` = ``.TRUE.`` ,
this is the same as in the offline package, except that the forcing offset is
in seconds.

.. tabularcolumns:: |l|l|l|c|

.. _tab_phys_pkg_rbcs_timing:

.. table:: Timing of RBCS relaxation fields

  +-------------------+-------------------------------------------------------------------------------------+-------------------+
  |                   |                               rbcsSingleTimeFiles = T                               |        F          |
  +-------------------+------------------------------------------+------------------------------------------+-------------------+
  |                   |    :math:`c=0`                           |    :math:`c\ne0`                         |  :math:`c\ne0`    |
  +===================+==========================================+==========================================+===================+
  | **model time**    | **file number**                          | **file number**                          | **record**        |
  +-------------------+------------------------------------------+------------------------------------------+-------------------+
  | :math:`t_0 - p/2` | :math:`i_0`                              | :math:`i_0 + c/{\Delta t_{\text{rbcs}}}` | :math:`c/p`       |
  +-------------------+------------------------------------------+------------------------------------------+-------------------+
  | :math:`t_0 + p/2` | :math:`i_0 + p/{\Delta t_{\text{rbcs}}}` | :math:`i_0 + p/{\Delta t_{\text{rbcs}}}` | :math:`1`         |
  +-------------------+------------------------------------------+------------------------------------------+-------------------+
  | :math:`t_0+p+p/2` | :math:`i_0 + 2p/{\Delta t_{\text{rbcs}}}`| :math:`i_0 + 2p/{\Delta t_{\text{rbcs}}}`| :math:`2`         |
  +-------------------+------------------------------------------+------------------------------------------+-------------------+
  | ...               |               ...                        |      ...                                 |    ...            |
  +-------------------+------------------------------------------+------------------------------------------+-------------------+
  | :math:`t_0+c-p/2` | ...                                      | :math:`i_0 + c/{\Delta t_{\text{rbcs}}}` | :math:`c/p`       |
  +-------------------+------------------------------------------+------------------------------------------+-------------------+
  | ...               |               ...                        |      ...                                 |    ...            |
  +-------------------+------------------------------------------+------------------------------------------+-------------------+

where

:math:`p` = :varlink:`rbcsForcingPeriod`

:math:`c` = :varlink:`rbcsForcingCycle`

:math:`t_0` = :varlink:`rbcsForcingOffset`

:math:`i_0` = :varlink:`rbcsIter0`

:math:`{\Delta t_{\text{rbcs}}}` = :varlink:`deltaTrbcs`




Example 1: forcing with time averages starting at :math:`t=0`
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Cyclic data in a single file
#############################

Set :varlink:`rbcsSingleTimeFiles` = :code:`.FALSE.` and
:varlink:`rbcsForcingOffset` = 0, and the model will start by interpolating the
last and first records of rbcs data, placed at :math:`-p/2` and :math:`p/2`,
respectively, as appropriate for fields averaged over the time intervals
:math:`[-p, 0]` and :math:`[0, p]`.

Non-cyclic data, multiple files
###############################

Set :varlink:`rbcsForcingCycle` = 0 and :varlink:`rbcsSingleTimeFiles` =
:code:`.TRUE.` . With :varlink:`rbcsForcingOffset` = 0, :varlink:`rbcsIter0` =
0 and :varlink:`deltaTrbcs` = :varlink:`rbcsForcingPeriod`, the model would
then start by interpolating data from files ``relax\*File.0000000000.data`` and
``relax\*File.0000000001.data``, ... , again placed at :math:`-p/2` and
:math:`p/2`.

Example 2: forcing with snapshots starting at :math:`t=0`
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Cyclic data in a single file
############################

Set :varlink:`rbcsSingleTimeFiles` = :code:`.FALSE.` and
:varlink:`rbcsForcingOffset` =\ :math:`-p/2`, and the model will start forcing
with the first record at :math:`t=0`.

Non-cyclic data, multiple files
###############################

Set :varlink:`rbcsForcingCycle` = 0 and :varlink:`rbcsSingleTimeFiles` =
:code:`.TRUE.`. In this case, it is more natural to set
:varlink:`rbcsForcingOffset` =\ :math:`+p/2`. With :varlink:`rbcsIter0` = 0 and
:varlink:`deltaTrbcs` = :varlink:`rbcsForcingPeriod`, the model would then
start with data from files ``relax\*File.0000000000.data`` at :math:`t=0`. It
would then proceed to interpolate between this file and files
``relax\*File.0000000001.data`` at :math:`t={}`\ :varlink:`rbcsForcingPeriod`.

Do’s and Don’ts
+++++++++++++++

Reference Material
++++++++++++++++++

Experiments and tutorials that use rbcs
+++++++++++++++++++++++++++++++++++++++

In the directory, the following experiments use :filelink:`pkg/rbcs`:

- tutorial :ref:`Southern Ocean Reentrant Channel <sec_eg_reentrant_channel>`
  makes use of :filelink:`pkg/rbcs` at its northern boundary as a sponge layer;

- :filelink:`exp4 <verification/exp4>` simulating flow over a Gaussian bump
  based on :cite:`adcroft:97`, demonstrating relaxation of a ptracer field.
