.. _sec_tracer_adj_sens:

Adjoint Sensitivity Analysis for Tracer Injection
=================================================

(in directory: :filelink:`verification/tutorial_tracer_adjsens/`)

MITgcm has been adapted to enable AD using TAMC or TAF. The present
description is specific to the use of TAMC or TAF as AD
tool. The following sections describe the steps which are necessary to
generate a tangent linear or adjoint model of MITgcm. We take as an
example the sensitivity of carbon sequestration in the ocean. The
AD-relevant hooks in the code are outlined in :numref:`sec_ad_tlm_and_adm`
and :numref:`sec_ad_finalize_contribtuions`.

Overview of the experiment
--------------------------

We describe an adjoint sensitivity analysis of out-gassing from the
ocean into the atmosphere of a carbon-like tracer injected into the
ocean interior (see Hill et al. 2004 :cite:`hill:04`).

Passive tracer equation
~~~~~~~~~~~~~~~~~~~~~~~

For this work, MITgcm was augmented with a thermodynamically inactive
tracer, :math:`C`. Tracer residing in the ocean model surface layer is
out-gassed according to a relaxation time scale, :math:`\mu`. Within the
ocean interior, the tracer is passively advected by the ocean model
currents. The full equation for the time evolution

.. math::
   \frac{\partial C}{\partial t} \, = \,
   -\mathbf{U} \cdot  \nabla  C \, - \, \mu C \, + \, \Gamma(C) \,+ \, S
   :label: carbon_ddt

also includes a source term :math:`S`. This term represents interior
sources of :math:`C` such as would arise due to direct injection. The
velocity term, :math:`\mathbf{U}`, is the sum of the model Eulerian circulation
and an eddy-induced velocity, the latter parameterized according to
Gent/McWilliams (Gent and McWilliams 1990 :cite:`gen-mcw:90`; Gent et al. (1995) :cite:`gen-eta:95`). The
convection function, :math:`\Gamma`, mixes :math:`C` vertically wherever
the fluid is locally statically unstable.

The out-gassing time scale, :math:`\mu`, in :eq:`carbon_ddt` is set
so that :math:`1/\mu \sim 1` year for the surface ocean and
:math:`\mu=0` elsewhere. With this value, :eq:`carbon_ddt` is valid
as a prognostic equation for small perturbations in oceanic carbon
concentrations. This configuration provides a powerful tool for
examining the impact of large-scale ocean circulation on CO\ :sub:`2`
out-gassing due to interior injections. As source we choose a constant
in time injection of :math:`S = 1`  mol s\ :sup:`-1`.

Model configuration
~~~~~~~~~~~~~~~~~~~

The model configuration employed has a constant
:math:`4^\circ \times 4^\circ` resolution horizontal grid and realistic
geography and bathymetry. Twenty vertical layers are used with vertical
spacing ranging from 50 m near the surface to 815 m at depth. Driven to
steady-state by climatological wind-stress, heat and fresh-water forcing,
the model reproduces well known large-scale features of the ocean
general circulation.

Out-gassing cost function
~~~~~~~~~~~~~~~~~~~~~~~~~

To quantify and understand out-gassing due to injections of :math:`C` in
:eq:`carbon_ddt`, we define a cost function :math:`{\cal J}` that
measures the total amount of tracer out-gassed at each timestep:

.. math::
   {\cal J}(t=T)=\int_{t=0}^{t=T}\int_{A} \mu C \, dA \, dt
   :label: cost_tracer

:eq:`cost_tracer` integrates the out-gassing term,
:math:`\mu C`, from :eq:`carbon_ddt` over the entire ocean surface area,
:math:`A`, and accumulates it up to time :math:`T`. Physically,
:math:`{\cal J}` can be thought of as representing the amount of
CO\ :sub:`2` that our model predicts would be out-gassed following an
injection at rate :math:`S`. The sensitivity of :math:`{\cal J}` to the
spatial location of :math:`S`,
:math:`\frac{\partial {\cal J}}{\partial S}`, can be used to identify
regions from which circulation would cause CO\ :sub:`2` to rapidly
out-gas following injection and regions in which CO\ :sub:`2` injections
would remain effectively sequestered within the ocean.

Code configuration
------------------

The code customization routines are in
:filelink:`verification/tutorial_tracer_adjsens/code_ad`:

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/COST_OPTIONS.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/CTRL_OPTIONS.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/CPP_OPTIONS.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/AUTODIFF_OPTIONS.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/CTRL_SIZE.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/GAD_OPTIONS.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/GMREDI_OPTIONS.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/SIZE.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/tamc.h`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/ctrl_map_ini_genarr.F`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/ptracers_forcing_surf.F`

-  :filelink:`verification/tutorial_tracer_adjsens/code_ad/packages.conf`

The runtime flag and parameters settings are contained in :filelink:`verification/tutorial_tracer_adjsens/input/`
and :filelink:`verification/tutorial_tracer_adjsens/input_ad/`, together with the forcing fields and and
restart files:

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/data`

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/data.cost`

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/data.ctrl`

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/data.gmredi`

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/data.grdchk`

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/data.optim`

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/data.pkg`

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/data.ptracers`

-  :filelink:`verification/tutorial_tracer_adjsens/input_ad/eedata`

-  ``verification/tutorial_tracer_adjsens/input/topog.bin``

-  ``verification/tutorial_tracer_adjsens/input/windx.bin``, ``verification/tutorial_tracer_adjsens/inputwindy.bin``

-  ``verification/tutorial_tracer_adjsens/input/salt.bin``, ``verification/tutorial_tracer_adjsens/input/theta.bin``

-  ``verification/tutorial_tracer_adjsens/input/SSS.bin``, ``verification/tutorial_tracer_adjsens/input/SST.bin``

Below we describe the customizations of this files which are specific to
this experiment.

File :filelink:`code_ad/COST_OPTIONS.h /<verification/tutorial_tracer_adjsens/code_ad/COST_OPTIONS.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file contains package-specific CPP-options (see :numref:`pkg_cost_description`).

File :filelink:`code_ad/CTRL_OPTIONS.h /<verification/tutorial_tracer_adjsens/code_ad/CTRL_OPTIONS.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file contains package-specific CPP-options (see :numref:`sec:pkg:ctrl`).

File :filelink:`code_ad/CPP_OPTIONS.h /<verification/tutorial_tracer_adjsens/code_ad/CPP_OPTIONS.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file contains model-specific CPP options (see :numref:`customize_compilation`).
Most options are related to the forward model
setup. They are identical to the global steady circulation setup of
:filelink:`verification/global_ocean.90x40x15/`. The three options specific to
this experiment are as follows.
``#define ALLOW_PASSIVE_TRACER`` enables the code to carry through the advection/diffusion of
a passive tracer along the model integration.
``#define ALLOW_MIT_ADJOINT_RUN`` enables the inclusion of some AD-related fields concerning
initialization, link between control variables and forward model
variables, and the call to the top-level forward/adjoint subroutine
``adthe_main_loop.F`` instead of :filelink:`model/src/the_main_loop.F`.
``#define ALLOW_GRADIENT_CHECK`` enables the gradient check package. After computing the
unperturbed cost function and its gradient, a series of computations
are performed for which:

  - an element of the control vector is perturbed
  - the cost function w.r.t. the perturbed element is computed
  - the difference between the perturbed and unperturbed
    cost function is computed to compute the finite difference gradient
  - the finite difference gradient is compared with the
    adjoint-generated gradient.

The gradient check package is further
described in :numref:`ad_gradient_check`.

File ``ECCO_OPTIONS.h``
~~~~~~~~~~~~~~~~~~~~~~~

The CPP options of several AD-related packages are grouped in this file:

-  | Overall ECCO-related execution modus:
   | These determine whether a pure forward run, a sensitivity run or an
     iteration of optimization is performed. These options are not
     needed in the present context.

-  | Adjoint support package: :filelink:`pkg/autodiff/`
   | This package contains hand-written adjoint code such as active file
     handling, flow directives for files which must not be
     differentiated, and TAMC-specific header files. ``#define`` :varlink:`ALLOW_AUTODIFF_TAMC`
     defines TAMC-related features in the code. ``#define`` :varlink:`ALLOW_TAMC_CHECKPOINTING`
     enables the checkpointing feature of TAMC (see :numref:`sec_autodiff_storage_v_recompute`).
     In the present example a 3-level checkpointing
     is implemented. The code contains the relevant store directives,
     common block and tape initializations, storing key computation, and
     loop index handling. The checkpointing length at each level is
     defined in file :filelink:`code_ad/tamc.h <verification/tutorial_tracer_adjsens/code_ad/tamc.h>`,
     see :ref:`below <tut_tracer_adjsens_tamc>`. The out and intermediate loop
     directives are contained in the files
     :filelink:`pkg/autodiff/checkpoint_lev3_directives.h`, :filelink:`pkg/autodiff/checkpoint_lev2_directives.h`.
     ``#define`` :varlink:`ALLOW_AUTODIFF_MONITOR` enables the monitoring of intermediate adjoint variables (see :numref:`sec_autodiff_output_adj_vars`).
     ``#define`` :varlink:`ALLOW_DIVIDED_ADJOINT` enables adjoint dump and restart (see :numref:`sec_autodiff_diva`).

-  | Cost function package: :filelink:`pkg/cost/`
   | This package contains all relevant routines for initializing,
     accumulating and finalizing the cost function (see :numref:`pkg_cost_description`).
     ``#define`` :varlink:`ALLOW_COST` enables all general aspects of the cost function handling, in
     particular the hooks in the forward code for initializing,
     accumulating and finalizing the cost function.
     ``#define`` :varlink:`ALLOW_COST_TRACER` includes the call to the cost function for this particular
     experiment, eqn. :eq:`cost_tracer`.

-  | Control variable package: :filelink:`pkg/ctrl/`
   | This package contains all relevant routines for the handling of the
     control vector. Each control variable can be enabled/disabled with
     its own flag:

   +-----------------------------------------------+----------------------------------------+
   | ``#define`` :varlink:`ALLOW_THETA0_CONTROL`   | initial temperature                    |
   +-----------------------------------------------+----------------------------------------+
   | ``#define`` :varlink:`ALLOW_SALT0_CONTROL`    | initial salinity                       |
   +-----------------------------------------------+----------------------------------------+
   | ``#define`` :varlink:`ALLOW_TR10_CONTROL`     | initial passive tracer concentration   |
   +-----------------------------------------------+----------------------------------------+
   | ``#define`` :varlink:`ALLOW_TAUU0_CONTROL`    | zonal wind stress                      |
   +-----------------------------------------------+----------------------------------------+
   | ``#define`` :varlink:`ALLOW_TAUV0_CONTROL`    | meridional wind stress                 |
   +-----------------------------------------------+----------------------------------------+
   | ``#define`` :varlink:`ALLOW_SFLUX0_CONTROL`   | freshwater flux                        |
   +-----------------------------------------------+----------------------------------------+
   | ``#define`` :varlink:`ALLOW_HFLUX0_CONTROL`   | heat flux                              |
   +-----------------------------------------------+----------------------------------------+
   | ``#define`` :varlink:`ALLOW_DIFFKR_CONTROL`   | diapycnal diffusivity                  |
   +-----------------------------------------------+----------------------------------------+
   | ``#undef`` :varlink:`ALLOW_KAPGM_CONTROL`     | isopycnal diffusivity                  |
   +-----------------------------------------------+----------------------------------------+

File :filelink:`SIZE.h <verification/tutorial_tracer_adjsens/code_ad/SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_tracer_adjsens/code_ad/SIZE.h
    :linenos:
    :caption: verification/tutorial_global_oce_latlon/code/SIZE.h

The file contains the grid point dimensions of the forward model. It
is identical to the :filelink:`verification/exp2/`.

File :filelink:`/pkg/autodiff/adcommon.h`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file contains common blocks of some adjoint variables that are
generated by TAMC. The common blocks are used by the adjoint support
routine :filelink:`/pkg/autodiff/addummy_in_stepping.F` which needs to access those variables:

+-------------------------------------------+-------------------------------------------------+
| common /\ :varlink:`addynvars_r`\ /       | is related to :filelink:`model/inc/DYNVARS.h`   |
+-------------------------------------------+-------------------------------------------------+
| common /\ :varlink:`addynvars_cd`\ /      | is related to :filelink:`model/inc/DYNVARS.h`   |
+-------------------------------------------+-------------------------------------------------+
| common /\ :varlink:`addynvars_diffkr`\ /  | is related to :filelink:`model/inc/DYNVARS.h`   |
+-------------------------------------------+-------------------------------------------------+
| common /\ :varlink:`addynvars_kapgm`\ /   | is related to :filelink:`model/inc/DYNVARS.h`   |
+-------------------------------------------+-------------------------------------------------+
| common /\ :varlink:`adtr1_r`\ /           | is related to ``TR1.h``                         |
+-------------------------------------------+-------------------------------------------------+
| common /\ :varlink:`adffields`\ /         | is related to  :filelink:`model/inc/FFIELDS.h`  |
+-------------------------------------------+-------------------------------------------------+

Note that if the structure of the common block changes in the above
header files of the forward code, the structure of the adjoint common
blocks will change accordingly. Thus, one must make sure that the
structure of the adjoint common block in the hand-written file
:filelink:`/pkg/autodiff/adcommon.h` complies with the automatically generated adjoint common
blocks in ``adjoint_model.F``. The header file is enabled via the
CPP-option :varlink:`ALLOW_AUTODIFF_MONITOR`.

.. _tut_tracer_adjsens_tamc:

File :filelink:`code_ad/tamc.h <verification/tutorial_tracer_adjsens/code_ad/tamc.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This routine contains the dimensions for TAMC checkpointing and some
indices relevant for storing ky computations.

-  | ``#ifdef`` :varlink:`ALLOW_TAMC_CHECKPOINTING`
   | 3-level checkpointing is enabled, i.e., the timestepping is divided
     into three different levels (see :numref:`sec_autodiff_storage_v_recompute`). The
     model state of the outermost (:varlink:`nchklev_3`) and the intermediate
     (:varlink:`nchklev_2`) timestepping loop are stored to file (handled in
     :filelink:`model/src/the_main_loop.F`). The innermost loop (:varlink:`nchklev_1`) avoids I/O by
     storing all required variables to common blocks. This storing may
     also be necessary if no checkpointing is chosen (nonlinear
     functions, if-statements, iterative loops, ...). In the present
     example the dimensions are chosen as follows:

     ::

        nchklev_1 = 36
        nchklev_2 = 30
        nchklev_3 = 60

   | To guarantee that the checkpointing intervals span the entire
     integration period the following relation must be satisfied:

   | :varlink:`nchklev_1` * :varlink:`nchklev_2` * :varlink:`nchklev_3` :math:`\ge` :varlink:`nTimeSteps`

   | where :varlink:`nTimeSteps` is either specified in :filelink:`input_ad/data <verification/tutorial_tracer_adjsens/input_ad/data>`
     or computed via:

   | :varlink:`nTimeSteps` = (:varlink:`endTime` - :varlink:`startTime` )/ :varlink:`deltaTClock`.

-  |  ``#undef`` :varlink:`ALLOW_TAMC_CHECKPOINTING`
   |  No checkpointing is enabled. In this case the relevant counter is
     :varlink:`nchklev_0`. Similar to above, the following relation has to be
     satisfied:

   | :varlink:`nchklev_0` :math:`\ge` :varlink:`nTimeSteps`

The following parameters may be worth describing: :varlink:`isbyte`, :varlink:`maxpass`.

File ``makefile``
~~~~~~~~~~~~~~~~~

This file contains all relevant parameter flags and lists to run TAMC or
TAF. It is assumed that TAMC is available to you, either locally, being
installed on your network, or remotely through the ’TAMC Utility’. TAMC
is called with the command ``tamc`` followed by a number of options. They
are described in detail in the TAMC manual (Giering 1999 :cite:`giering:99`).
Here we briefly discuss the main flags used in the ``makefile``. The
standard output for TAF is written to file ``taf.log``.

TAMC:

  ::

      -input «variable names» -output «variable name» -i4 -r4 ...
      -toplevel «S/R name» -reverse «file names»

TAF:

  ::

     -input «variable names» -output «variable name» -i4 -r4 ...
     -toplevel «S/R name» -reverse «file names»
     -flow taf_flow.log -nonew_arg

-  ``-toplevel «S/R name»``

   Name of the toplevel routine, with respect to which the control
   flow analysis is performed.

-  ``input «variable names»``

   List of independent variables :math:`u` with respect to which the
   dependent variable :math:`J` is differentiated.

-  ``-output «variable name»``

   Dependent variable :math:`J` which is to be differentiated.

-  ``-reverse «file names»``

   Adjoint code is generated to compute the sensitivity of an
   independent variable w.r.t. many dependent variables. In the
   discussion of :numref:`chap_autodiff` the generated adjoint top-level routine
   computes the product of the transposed Jacobian matrix
   :math:`M^T` times the gradient vector :math:`\nabla_v J`.
   «file names» refers to the list of files ``.f`` which are to be analyzed by TAMC.
   This list is generally smaller than the full list of code to be
   compiled. The files not contained are either above the top-level
   routine (some initializations), or are deliberately hidden from
   TAMC, either because hand-written adjoint routines exist, or the
   routines must not (or don’t have to) be differentiated. For each
   routine which is part of the flow tree of the top-level routine,
   but deliberately hidden from TAMC (or for each package which
   contains such routines), a corresponding file ``.flow`` exists
   containing flow directives for TAMC.

-  ``-i4 -r4``
 

-  ``-flow taf_flow.log``

   Will cause TAF to produce a flow listing file named ``taf_flow.log``
   in which the set of active and passive variables are identified for
   each subroutine.

- ``-nonew_arg``

  The default in the order of the parameter list of adjoint routines
  has changed. Before TAF 1.3 the default was compatible with the
  TAMC-generated list. As of TAF 1.3 the order of adjoint routine
  parameter lists is no longer compatible with TAMC. To restore
  compatibility when using TAF 1.3 and higher, this argument is
  needed. It is currently crucial to use since all hand-written
  adjoint routines refer to the TAMC default.

File ``input/topog.bin``
^^^^^^^^^^^^^^^^^^^^^^^^
 
Contains 2-D bathymetry information.

Files ``input/windx.bin``, ``input/windy.bin``, ``input/salt.bin``, ``input/theta.bin``, ``input/SSS.bin``, ``input/SST.bin``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These contain the initial values of salnity and potential temperature (``salt.bin``, ``theta.bin``), surface boundary values (surface wind stresses
``windx.bin``, ``windy.bin``), and surface restoring fields (``SSS.bin``, ``SST.bin``).

Compiling the model and its adjoint
-----------------------------------

The build process of the adjoint model is slightly more complex than
that of compiling the forward code. The main reason is that the adjoint
code generation requires a specific list of routines that are to be
differentiated (as opposed to the automatic generation of a list of
files to be compiled by :filelink:`genmake2 <tools/genmake2>`). This list excludes routines that don’t
have to be or must not be differentiated. For some of the latter
routines flow directives may be necessary, a list of which has to be
given as well. For this reason, a separate ``makefile`` is currently
maintained in the directory adjoint/. This makefile is responsible for
the adjoint code generation.

In the following we describe the build process step by step, assuming
you are in the directory bin/. A summary of steps to follow is given at
the end.

Adjoint code generation and compilation – step by step
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. | ``ln -s ../verification/???/code/.genmakerc .``
   | ``ln -s ../verification/???/code/*.[Fh] .``
   | Link your customized genmake options, header files, and modified
     code to the compile directory.

2. | ``../tools/genmake -makefile``
   | Generate your Makefile (see :numref:`genmake2_desc`).

3. | ``make depend``
   | Dependency analysis for the CPP pre-compiler (see :numref:`building_quickstart`).

4. | ``cd ../adjoint``
   | ``make adtaf`` or ``make adtamc``
   | Depending on whether you have TAF or TAMC at your disposal, you’ll
     choose adtaf or adtamc as your make target for the ``makefile`` in
     the directory adjoint/. Several things happen at this stage.

  -  | ``make adrestore``
     | ``make ftlrestore``
     | The initial template files ``adjoint_model.F`` and
       ``tangentlinear_model.F`` in :filelink:`pkg/autodiff` which are part of the
       compiling list created by :filelink:`genmake2 <tools/genmake2>` are restored.

  -  | ``make depend, make small_f``
     | The bin/ directory is brought up to date, i.e., for recent
       changes in header or source code ``.[Fh]``, corresponding ``.f``
       routines are generated or re-generated. Note that here, only CPP
       pre-compiling is performed; no object code ``.o`` is generated as
       yet. Pre-compiling is necessary for TAMC to see the full code.

  -  | ``make allcode``
     | All Fortran routines .f in bin/ are concatenated into a single
       file called ``tamc_code.f``.

  -  | ``make admodeltaf/admodeltamc``
     | Adjoint code is generated by TAMC or TAF. The adjoint code is
       written to the file ``tamc_code_ad.f``. It contains all adjoint
       routines of the forward routines concatenated in ``tamc_code.f``.
       For a given forward routine subroutine routinename the adjoint
       routine is named adsubroutine routinename by default (that
       default can be changed via the flag ``-admark «markname»``).
       Furthermore, it may contain modified code which incorporates the
       translation of adjoint store directives into specific Fortran
       code. For a given forward routines subroutine routinename the
       modified routine is named mdsubroutine routinename. TAMC or TAF
       info is written to file ``tamc_code.prot`` or ``taf.log``,
       respectively.

  -  | ``make adchange``
     | The multi-threading capability of MITgcm requires a slight
       change in the parameter list of some routines that are related
       to to active file handling. This post-processing invokes the sed
       script :filelink:`tools/adjoint_sed` to insert the threading counter
       :varlink:`myThId` into the parameter list of those subroutines. The
       resulting code is written to file ``tamc_code_sed_ad.f`` and
       appended to the file ``adjoint_model.F``. This concludes the
       adjoint code generation.

5. | ``cd ../bin``
   | ``make``
   | The file ``adjoint_model.F`` cnow contains the full adjoint code. All
     routines are now compiled.

N.B.: The targets ``make adtaf/adtamc`` now comprise a series of targets
that in previous versions had to be invoked separately. This was
probably preferable at a more experimental stage, but has now been
dropped in favor of a more straightforward build process.

Adjoint code generation and compilation – summary
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

      cd bin
      ln -s ../verification/my_experiment/code/.genmakerc .
      ln -s ../verification/my_experiment/code/*.[Fh] .
      ../tools/genmake -makefile
      make depend
      cd ../adjoint
      make adtaf <OR: make adtamc>
           contains the targets:
           adrestore small_f allcode admodeltaf/admodeltamc adchange
      cd ../bin
      make
