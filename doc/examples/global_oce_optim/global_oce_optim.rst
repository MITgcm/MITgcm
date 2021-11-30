.. _sec_global_oce_optim:

Global Ocean State Estimation
=============================

(in directory: :filelink:`verification/tutorial_global_oce_optim/`)

Overview
--------

This experiment illustrates the optimization capacity of the MITgcm:
here, a high level description.

In this tutorial, a very simple case is used to illustrate the
optimization capacity of the MITgcm. Using an ocean configuration with
realistic geography and bathymetry on a :math:`4\times4^\circ` spherical
polar grid, we estimate a time-independent surface heat flux adjustment
:math:`Q_\mathrm{netm}` that attempts to bring the model climatology
into consistency with observations (Levitus and Boyer (1994a,b)
:cite:`levitus:94a,levitus:94b`).

This adjustment :math:`Q_\mathrm{netm}` (a 2-D field only function of
longitude and latitude) is the control variable of an optimization
problem. It is inferred by an iterative procedure using an ‘adjoint
technique’ and a least-squares method (see, for example,
Stammer et al. (2002) :cite:`stammer:02` and Ferriera et a. (2005) :cite:`ferriera:05`.

The ocean model is run forward in time and the quality of the solution
is determined by a cost function, :math:`J_1`, a measure of the
departure of the model climatology from observations:

.. math::
   J_1=\frac{1}{N}\sum_{i=1}^N \left[ \frac{\overline{T}_i-\overline{T}_i^{lev}}{\sigma_i^T}\right]^2
   :label: cost-temp

where :math:`\overline{T}_i` and :math:`\overline{T}_i^{lev}` are,
respectively, the model and observed potential temperature at each grid
point :math:`i`. The differences are weighted by an *a priori*
uncertainty :math:`\sigma_i^T` on observations (as provided by
Levitus and Boyer (1994a)
:cite:`levitus:94a`). The error :math:`\sigma_i^T` is only a
function of depth and varies from 0.5 K at the surface to 0.05 K at the
bottom of the ocean, mainly reflecting the decreasing temperature
variance with depth (see :numref:`tut_global_optim_errors`\ a). A value of :math:`J_1` of order 1
means that the model is, on average, within observational uncertainties.

   .. figure:: figs/Error.png
       :width: 80%
       :align: center
       :alt: A priori errors on potential temperature and surface hf
       :name: tut_global_optim_errors

       *A priori* errors on potential temperature (left, in :sup:`o`\ C) and surface heat flux
       (right, in W m\ :sup:`-2`) used to compute the cost terms :math:`J_1` and :math:`J_2`, respectively.

The cost function also places constraints on the adjustment to insure it
is “reasonable”, i.e., of order of the uncertainties on the observed
surface heat flux:

.. math:: J_2 = \frac{1}{N} \sum_{i=1}^N \left[\frac{Q_\mathrm{netm}}{\sigma^Q_i} \right]^2

where :math:`\sigma^Q_i` are the *a priori* errors on the observed heat
flux as estimated by Stammer et al. (2002) :cite:`stammer:02` from 30% of local
root-mean-square variability of the NCEP forcing field (see :numref:`tut_global_optim_errors`\ b).

The total cost function is defined as
:math:`J=\lambda_1 J_1+ \lambda_2 J_2` where :math:`\lambda_1` and
:math:`\lambda_2` are weights controlling the relative contribution of
the two components. The adjoint model then yields the sensitivities
:math:`\partial J/\partial Q_\mathrm{netm}` of :math:`J` relative to the
2-D fields :math:`Q_\mathrm{netm}`. Using a line-searching algorithm
(Gilbert and Lemaréchal 1989 :cite:`gil-lem:89`), :math:`Q_\mathrm{netm}` is adjusted
then in the sense to reduce :math:`J` — the procedure is repeated until
convergence.

:numref:`tut_global_optim_tutfig` shows the results of such an optimization. The model is
started from rest and from January-mean temperature and salinity initial
conditions taken from the Levitus dataset. The experiment is run a year
and the averaged temperature over the whole run (i.e., annual mean) is
used in the cost function :eq:`cost-temp` to evaluate the model [1]_.
Only the top 2 levels are used. The first guess :math:`Q_\mathrm{netm}`
is chosen to be zero. The weights :math:`\lambda_1` and
:math:`\lambda_2` are set to 1 and 2, respectively. The total cost
function converges after 15 iterations, decreasing from 6.1 to 2.7 (the
temperature contribution decreases from 6.1 to 1.8 while the heat flux
one increases from 0 to 0.42). The right panels of :numref:`tut_global_optim_tutfig`
illustrate the evolution of the temperature error at the surface from
iteration 0 to iteration 15. Unsurprisingly, the largest errors at
iteration 0 (up to 6 :sup:`o`\ C, top left panels) are found in
the Western boundary currents. After optimization, the departure of the
model temperature from observations is reduced to 1 :sup:`o`\ C
or less almost everywhere except in the Pacific equatorial cold tongue.
Comparison of the initial temperature error (top, right) and heat flux
adjustment (bottom, left) shows that the system basically increased the
heat flux out of the ocean where temperatures were too warm and
vice-versa. Obviously, heat flux uncertainties are not solely
responsible for temperature errors, and the heat flux adjustment partly
compensates the poor representation of narrow currents (Western boundary
currents, equatorial currents) at :math:`4\times4^\circ` resolution.
This is allowed by the large *a priori* error on the heat flux :numref:`tut_global_optim_errors`.
The Pacific cold tongue is a counter example: there, heat
fluxes uncertainties are fairly small (about 20 W m\ :sup:`-2`), and a
large temperature errors remains after optimization.

  .. figure:: figs/Tutorial_fig.png
       :width: 100%
       :align: center
       :alt: Surface HF and Temp Iter 0 v 15
       :name: tut_global_optim_tutfig

       Initial annual mean surface heat flux (top right in W m\ :sup:`-2`) and adjustment obtained at iteration 15 (bottom right).
       Averaged difference between model and observed potential temperatures at the surface (in :math:`^\circ`\ C)
       before optimization (iteration 0, top right) and after optimization (iteration 15, bottom right).
       Contour intervals for heat flux and temperature are 25 W m\ :sup:`-2` and 1 :sup:`o`\ C, respectively. A positive flux is out of the ocean.

Implementation of the control variable and the cost function
------------------------------------------------------------

One of the goals of this tutorial is to illustrate how to implement a new
control variable. Most of this is fairly generic and is done in :filelink:`pkg/ctrl`
and :filelink:`pkg/cost`. The modifications can be
tracked by the CPP option :varlink:`ALLOW_HFLUXM_CONTROL` or the comment
``cHFLUXM_CONTROL``. The more specific modifications required for the
experiment are found in
:filelink:`verification/tutorial_global_oce_optim/code_ad`. Here follows a brief
description of the implementation.

The control variable
~~~~~~~~~~~~~~~~~~~~

The adjustment :math:`Q_\mathrm{netm}` is activated by setting ``#define``
:varlink:`ALLOW_HFLUXM_CONTROL` in :filelink:`code_ad/CTRL_OPTIONS.h <verification/tutorial_global_oce_optim/code_ad//CTRL_OPTIONS.h>`.

It is first implemented as a “normal” forcing variable. It is defined in
:filelink:`model/inc/FFIELDS.h`, initialized to zero in :filelink:`model/src/ini_forcing.F`, and then used in
:filelink:`model/src/external_forcing_surf.F`. :math:`Q_\mathrm{netm}` is made a control
variable in :filelink:`pkg/ctrl` by modifying the following subroutines:

-  :filelink:`pkg/ctrl/ctrl_init.F` where :math:`Q_\mathrm{netm}` is defined as the control
   variable number 24,

-  :filelink:`pkg/ctrl/ctrl_pack.F` which writes, at the end of each iteration, the
   sensitivity of the cost function
   :math:`\partial J/\partial Q_\mathrm{netm}` in to a file to be used
   by the line-search algorithm,

-  :filelink:`pkg/ctrl/ctrl_unpack.F` which reads, at the start of each iteration, the
   updated adjustment as provided by the line-search algorithm,

-  :filelink:`pkg/ctrl/ctrl_map_forcing.F` in which the updated adjustment is added to the
   first guess :math:`Q_\mathrm{netm}`.

Note also some minor changes in :filelink:`pkg/ctrl/ctrl.h`, :filelink:`pkg/ctrl/ctrl_readparms.F`, and
:filelink:`pkg/ctrl/ctrl_dummy.h` (:varlink:`xx_hfluxm_file`, :varlink:`fname_hfluxm`, :varlink:`xx_hfluxm_dummy`).

Cost functions
~~~~~~~~~~~~~~

The cost functions are implemented using :filelink:`pkg/cost`.

-  The temperature cost function :math:`J_1` which measures the drift of
   the mean model temperature from the Levitus climatology is
   implemented in :filelink:`/verification/tutorial_global_oce_optim/code_ad/cost_temp.F`.
   It is activated by ``#define`` :varlink:`ALLOW_COST_TEMP` in
   :filelink:`code_ad/COST_OPTIONS.h <verification/tutorial_global_oce_optim/code_ad//COST_OPTIONS.h>`.
   It requires the mean temperature of the model which
   is obtained by accumulating the temperature in :filelink:`pkg/cost/cost_tile.F` (called
   at each time step). The value of the cost function is stored in
   :varlink:`objf_temp` and its weight :math:`\lambda_1` in :varlink:`mult_temp`.

-  The heat flux cost function, penalizing the departure of the surface
   heat flux from observations is implemented in :filelink:`/verification/tutorial_global_oce_optim/code_ad/cost_hflux.F`, and
   activated by ``#define``  :varlink:`ALLOW_COST_HFLUXM` in
   :filelink:`code_ad/COST_OPTIONS.h <verification/tutorial_global_oce_optim/code_ad//COST_OPTIONS.h>`. The
   value of the cost function is stored in :varlink:`objf_hfluxm` and its weight
   :math:`\lambda_2` in :varlink:`mult_hflux`.

-  The subroutine :filelink:`pkg/cost/cost_final.F` calls the cost function subroutines
   and makes the (weighted) sum of the various contributions.

-  The various weights used in the cost functions are read in
   :filelink:`/verification/tutorial_global_oce_optim/code_ad/cost_weights.F`. The weight of the cost functions are read in
   :filelink:`pkg/cost/cost_readparms.F` from the input file :filelink:`verification/tutorial_global_oce_optim/input_ad/data.cost`.

Code Configuration
------------------

The experiment files in :filelink:`verification/tutorial_global_oce_optim/code_ad/`
and :filelink:`verification/tutorial_global_oce_optim/input_ad/` contain the code customizations and parameter
settings. Most of them are identical to those used in the Global Ocean (
experiment :filelink:`verification/tutorial_global_oce_latlon/`). Below, we describe some of
the customizations required for this experiment.

Compilation-time customizations in :filelink:`code_ad <verification/tutorial_global_oce_optim/code_ad/>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In :filelink:`code_ad/CTRL_OPTIONS.h <verification/tutorial_global_oce_optim/code_ad//CTRL_OPTIONS.h>`:

-  ``#define`` :varlink:`ALLOW_ECCO_OPTIMIZATION`

.. _tut_global_oce_runsect:

Running-time customizations in :filelink:`input_ad <verification/tutorial_global_oce_optim/input_ad/>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  :filelink:`input_ad/data <verification/tutorial_global_oce_optim/input_ad/data>`: note the smaller :varlink:`cg2dTargetResidual` than in the
   forward-only experiment,

-  :filelink:`input_ad/data.optim <verification/tutorial_global_oce_optim/input_ad/data.optim>` specifies the iteration number,

-  :filelink:`input_ad/data.ctrl <verification/tutorial_global_oce_optim/input_ad/data.ctrl>` is used, in particular, to specify the name of the
   sensitivity and adjustment files associated to a control variable,

-  :filelink:`input_ad/data.cost <verification/tutorial_global_oce_optim/input_ad/data.cost>`: parameters of the cost functions, in particular
   :varlink:`lastinterval` specifies the length of time-averaging for the model
   temperature to be used in the cost function :eq:`cost-temp`,

-  :filelink:`input_ad/data.pkg <verification/tutorial_global_oce_optim/input_ad/data.pkg>`: note that the Gradient Check package is turned on by
   default (:varlink:`useGrdchk` ``=.TRUE.``),

-  ``Err_hflux.bin`` and ``Err_levitus_15layer.bin`` are the files
   containing the heat flux and potential temperature uncertainties,
   respectively.

Compiling
---------

The optimization experiment requires two executables: 1) the MITgcm and
its adjoint (``mitgcmuv_ad``) and 2) the line-search algorithm
(``optim.x``).

Compilation of MITgcm and its adjoint: ``mitcgmuv_ad``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before compiling, first note that in the directory :filelink:`code_ad <verification/tutorial_global_oce_optim/code_ad/>`, two files
must be updated:

-  :filelink:`code_ad/code_ad_diff.list <verification/tutorial_global_oce_optim/code_ad/code_ad_diff.list>` which lists new subroutines to be compiled by the
   TAF software (:filelink:`code_ad/cost_temp.F <verification/tutorial_global_oce_optim/code_ad/cost_temp.F>`
   and :filelink:`code_ad/cost_hflux.F <verification/tutorial_global_oce_optim/code_ad/cost_hflux.F>`),

-  the file :filelink:`code_ad/ad_optfile.local <verification/tutorial_global_oce_optim/code_ad/ad_optfile.local>` provides a list of the control
   variables and the name of cost function to the TAF software.

Then, in the directory :filelink:`build <verification/tutorial_global_oce_optim/build/>`, type:

::

    % ../../../tools/genmake2 -mods=../code_ad -adof=../code_ad/ad_optfile.local
    % make depend
    % make adall

to generate the MITgcm executable ``mitgcmuv_ad``.

Compilation of the line-search algorithm: ``optim.x``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is done from the directories :filelink:`lsopt/` and :filelink:`optim/` (found in the top MITgcm directory). In
:filelink:`lsopt/`, unzip the ``blash1`` library adapted to your platform (see :filelink:`lsopt/README`), and change
the ``Makefile`` accordingly. Compile with:

::

    % make all

(more details in :filelink:`lsopt/lsopt_doc.txt`)

In :filelink:`optim/`, the path of the directory where ``mitgcm_ad`` was compiled
must be specified in the ``Makefile`` in the variable :varlink:`INCLUDEDIRS`. The file
name of the control variable (here, :varlink:`xx_hfluxm_file`) must be added to
the namelist read by :filelink:`optim/optim_numbmod.F`. Then use

::

    % make depend

and

::

    % make

to generate the line-search executable ``optim.x``.

Running the estimation
----------------------

Make a new subdirectory ``input_ad/OPTIM``.
Copy the ``mitgcmuv_ad`` executable to :filelink:`input_ad <verification/tutorial_global_oce_optim/input_ad/>`
and ``optim.x`` to this subdirectory.
``cd`` into :filelink:`input_ad/<verification/tutorial_global_oce_optim/input_ad/>`. The first iteration
is somewhat particular and is best done “by hand” while the following
iterations can be run automatically (see below). Check that the
iteration number is set to 0 in :filelink:`input_ad/data.optim <verification/tutorial_global_oce_optim/input_ad/data.optim>` and run MITgcm:

::

    % ./mitgcmuv_ad

The output files ``adxx_hfluxm.0000000000.*`` and ``xx_hfluxm.0000000000.*``
contain the sensitivity of the cost function to :math:`Q_\mathrm{netm}`
and the adjustment to :math:`Q_\mathrm{netm}` (zero at the first
iteration), respectively. Two other files called
``costhflux_tut_MITgcm.opt0000`` and ``ctrlhflux_tut_MITgcm.opt0000`` are
also generated. They essentially contain the same information as the
``adxx_.hfluxm*`` and ``xx_hfluxm*`` files, but in a compressed format.
These two files are the only ones involved in the communication between
the adjoint model ``mitgcmuv_ad`` and the line-search algorithm
``optim.x``. Only at the first iteration, are they both generated by
``mitgcmuv_ad``. Subsequently, ``costhflux_tut_MITgcm.opt`` :math:`n` is
an output of the adjoint model at iteration :math:`n` and an input of
the line-search. The latter returns an updated adjustment in
``ctrlhflux_tut_MITgcm.opt`` :math:`n+1` to be used as an input of the
adjoint model at iteration :math:`n+1`.

At the first iteration, move ``costhflux_tut_MITgcm.opt0000`` and
``ctrlhflux_tut_MITgcm.opt0000`` to ``input_ad/OPTIM``,
move into this directory and link :filelink:`input_ad/data.optim <verification/tutorial_global_oce_optim/input_ad/data.optim>`
and :filelink:`input_ad/data.ctrl <verification/tutorial_global_oce_optim/input_ad/data.ctrl>` locally:

::

    % cd OPTIM/
    % ln -s ../data.optim .
    % ln -s ../data.ctrl .

The target cost function :varlink:`fmin` needs to be specified
in :filelink:`input_ad/data.optim <verification/tutorial_global_oce_optim/input_ad/data.optim>`: as a rule of
thumb, it should be about 0.95-0.90 times the value of the cost function
at the first iteration. This value is only used at the first iteration
and does not need to be updated afterward. However, it implicitly
specifies the “pace” at which the cost function is going down (if you
are lucky and it does indeed diminish!).

Once this is done, run the line-search algorithm:

::

    % ./optim.x

which computes the updated adjustment for iteration 1,
``ctrlhflux_tut_MITgcm.opt0001``.

The following iterations can be executed automatically using the shell
script :filelink:`input_ad/cycsh <verification/tutorial_global_oce_optim/input_ad/cycsh>`. This script will take care of
changing the iteration numbers in :filelink:`input_ad/data.optim <verification/tutorial_global_oce_optim/input_ad/data.optim>`, launch the adjoint
model, clean and store the outputs, move the ``costhflux*`` and ``ctrlhflux*``
files, and run the line-search algorithm. Edit :filelink:`input_ad/cycsh <verification/tutorial_global_oce_optim/input_ad/cycsh>` to specify the
prefix of the directories used to store the outputs and the maximum
number of iteration.

.. [1]
   Because of the daily automatic testing, the experiment as found in
   the repository is set-up with a very small number of time-steps. To
   reproduce the results shown here, one needs to set :varlink:`nTimeSteps` = 360
   and :varlink:`lastinterval` =31104000 (both corresponding to a year, see :numref:`tut_global_oce_runsect` for further details).
