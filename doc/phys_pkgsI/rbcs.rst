.. _sub_phys_pkg_rbcs:

RBCS Package
------------


.. _ssub_phys_pkg_rbcs_intro:

Introduction
++++++++++++

A package which provides the flexibility to relax fields (temperature,
salinity, ptracers) in any 3-D location: so could be used as a sponge
layer, or as a “source” anywhere in the domain.

For a tracer (:math:`T`) at every grid point the tendency is modified so
that:

.. math:: \frac{dT}{dt}=\frac{dT}{dt} - \frac{M_{rbc}}{\tau_T} (T-T_{rbc})

where :math:`M_{rbc}` is a 3-D mask (no time dependence) with values
between 0 and 1. Where :math:`M_{rbc}` is 1, relaxing timescale is
:math:`1/\tau_T`. Where it is 0 there is no relaxing. The value relaxed
to is a 3-D (potentially varying in time) field given by
:math:`T_{rbc}`.

A seperate mask can be used for T,S and ptracers and each of these can
be relaxed or not and can have its own timescale :math:`\tau_T`. These
are set in data.rbcs (see below).

Key subroutines and parameters
++++++++++++++++++++++++++++++

The only compile-time parameter you are likely to have to change is in
:code:`RBCS.h`, the number of masks, PARAMETER(maskLEN = 3 ), see below.

The runtime parameters are set in :code:`data.rbcs`:

Set in RBCS\_PARM01:
- **rbcsForcingPeriod**: time interval between forcing fields (in seconds), zero means constant-in-time forcing.
- **rbcsForcingCycle**: repeat cycle of forcing fields (in seconds), zero means non-cyclic forcing.
- **rbcsForcingOffset**: time offset of forcing fields (in seconds, default 0); this is relative to time averages starting at :math:`t=0`, i.e., the first forcing record/file is placed at :math:`{\rm rbcsForcingOffset+rbcsForcingPeriod}/2`; see below for examples.
- **rbcsSingleTimeFiles**: true or false (default false), if true, forcing fields are given 1 file per rbcsForcingPeriod.
- **deltaTrbcs**: time step used to compute the iteration numbers for rbcsSingleTimeFiles=T.
- **rbcsIter0**: shift in iteration numbers used to label files if rbcsSingleTimeFiles=T (default 0, see below for examples).
- **useRBCtemp**: true or false (default false)
- **useRBCsalt**: true or false (default false)
- **useRBCptracers**: true or false (default false), must be using ptracers to set true
- **tauRelaxT**: timescale in seconds of relaxing in temperature (:math:`\tau_T` in equation above). Where mask is 1, relax rate will be 1/tauRelaxT. Default is 1.
- **tauRelaxS**: same for salinity.
- **relaxMaskFile(irbc)**: filename of 3-D file with mask (:math:`M_{rbc}` in equation above. Need a file for each irbc. 1=temperature, 2=salinity, 3=ptracer01, 4=ptracer02 etc. If the mask numbers end (see maskLEN) are less than the number tracers, then relaxMaskFile(maskLEN) is used for all remaining ptracers.
- **relaxTFile**: name of file where temperatures that need to be relaxed to (:math:`T_{rbc}` in equation above) are stored. The file must contain 3-D records to match the model domain. If rbcsSingleTimeFiles=F, it must have one record for each forcing period. If T, there must be a separate file for each period and a 10-digit iteration number is appended to the file name (see Table [tab:pkg:rbcs:timing] and examples below).
- **relaxSFile**: same for salinity.

Set in RBCS\_PARM02 for each of the ptracers (iTrc):
- **useRBCptrnum(iTrc)**: true or false (default is false).
- **tauRelaxPTR(iTrc)**: relax timescale.
- **relaxPtracerFile(iTrc)**: file with relax fields.


Timing of relaxation forcing fields
+++++++++++++++++++++++++++++++++++

For constant-in-time relaxation, set rbcsForcingPeriod=0. For
time-varying relaxation, Table [tab:pkg:rbcs:timing] illustrates the
relation between model time and forcing fields (either records in one
big file or, for rbcsSingleTimeFiles=T, individual files labeled with an
iteration number). With rbcsSingleTimeFiles=T, this is the same as in
the offline package, except that the forcing offset is in seconds.

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

:math:`p` = rbcsForcingPeriod

:math:`c` = rbcsForcingCycle

:math:`t_0` = rbcsForcingOffset

:math:`i_0` = rbcsIter0

:math:`{\Delta t_{\text{rbcs}}}` = deltaTrbcs




Example 1: forcing with time averages starting at :math:`t=0`
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Cyclic data in a single file
#############################

Set rbcsSingleTimeFiles=F and rbcsForcingOffset=0, and the model will
start by interpolating the last and first records of rbcs data, placed
at :math:`-p/2` and :math:`p/2`, resp., as appropriate for fields
averaged over the time intervals :math:`[-p, 0]` and :math:`[0, p]`.

Non-cyclic data, multiple files
###############################

Set rbcsForcingCycle=0 and rbcsSingleTimeFiles=T. With
rbcsForcingOffset=0, rbcsIter0=0 and deltaTrbcs=rbcsForcingPeriod, the
model would then start by interpolating data from files
relax\*File.0000000000.data and relax\*File.0000000001.data, ... , again
placed at :math:`-p/2` and :math:`p/2`.

Example 2: forcing with snapshots starting at :math:`t=0`
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Cyclic data in a single file
############################

Set rbcsSingleTimeFiles=F and rbcsForcingOffset=\ :math:`-p/2`, and the
model will start forcing with the first record at :math:`t=0`.

Non-cyclic data, multiple files
###############################

Set rbcsForcingCycle=0 and rbcsSingleTimeFiles=T. In this case, it is
more natural to set rbcsForcingOffset=\ :math:`+p/2`. With rbcsIter0=0
and deltaTrbcs=rbcsForcingPeriod, the model would then start with data
from files relax\*File.0000000000.data at :math:`t=0`. It would then
proceed to interpolate between this file and files
relax\*File.0000000001.data at :math:`t={}`\ rbcsForcingPeriod.

Do’s and Don’ts
+++++++++++++++

Reference Material
++++++++++++++++++

Experiments and tutorials that use rbcs
+++++++++++++++++++++++++++++++++++++++

In the directory , the following experiments use :code:`rbcs`:

-  :code:`exp4` : box with 4 open boundaries, simulating flow over a Gaussian bump
   based on :cite:`adcroft:97`


