.. _chap_getting_started:

Getting Started with MITgcm
***************************

This chapter is divided into two main parts. The first part, which is
covered in sections :numref:`whereToFindInfo` through
:numref:`run_the_model`, contains information about how to download,
build and run MITgcm.
We believe the best way to familiarize yourself with the
model is to run one of the tutorial examples provided in the MITgcm repository
(see :numref:`chap_modelExamples`), so would suggest newer MITgcm users
jump there following a read-through of the first part of this chapter.
Information is also provided
in this chapter on how to customize the code
when you are ready to try implementing
the configuration you have in mind, in the second
part (:numref:`customize_model`).
The code and algorithm are described more fully in
:numref:`discret_algorithm` and
:numref:`sarch` and chapters thereafter.

In this chapter and others (e.g., chapter :ref:`chap_contributing`),
for arguments where the user is expected to replace the text
with a user-chosen name, userid, etc., our convention
is to show these as upper-case
text surrounded by ``« »``, such as ``«USER_MUST_REPLACE_TEXT_HERE»``.
The ``«`` and ``»`` characters are **NOT** typed when  the text is replaced.

.. _whereToFindInfo:

Where to find information
=========================

There is a web-archived support mailing list for the model that you can
email at MITgcm-support@mitgcm.org once you have subscribed.

To sign up (subscribe) for the mailing list (highly recommended),
click `here <http://mailman.mitgcm.org/mailman/listinfo/mitgcm-support/>`__

To browse through the support archive,
click `here <http://mailman.mitgcm.org/pipermail/mitgcm-support/>`__

Obtaining the code
==================

The MITgcm code and documentation are under
continuous development and we generally
recommend that one downloads the latest version of the code.
You will need to decide if you want to work in a “git-aware”
environment (`Method 1`_) or with a one-time “stagnant” download (`Method 2`_).
We generally recommend method 1, as it is more
flexible and allows your version of the code to be regularly updated as MITgcm
developers check in bug fixes and new features. However, this typically
requires at minimum a rudimentary understanding of git in
order to make it worth one’s while.

Periodically we release an official checkpoint
(or “tag”). We recommend one download
the latest code, unless there are reasons for obtaining a specific checkpoint
(e.g. duplicating older results, collaborating with
someone using an older release, etc.)

.. _git-aware_download:

Method 1
--------

This section describes how to download git-aware copies of the repository.
In a terminal window, cd to the directory where you want your code to reside.
Type:

::

    % git clone https://github.com/MITgcm/MITgcm.git

This will download the latest available code. If you now want to revert this
code to a specific checkpoint release, first ``cd`` into the MITgcm directory
you just downloaded, then type ``git checkout checkpoint«XXX»``
where ``«XXX»`` is the checkpoint version.

Alternatively, if you prefer to use ssh keys (say for example, you have
a firewall which won’t allow a https download), type:

::

    % git clone git@github.com:MITgcm/MITgcm.git

You will need a GitHub account for this, and will have to generate a ssh
key though your GitHub account user settings.

The fully git-aware download is over several hundred MB, which is considerable
if one has limited internet download speed. In comparison, the one-time
download zip file (`Method 2`_, below) is order 100 MB. However, one can
obtain a truncated, yet still git-aware copy of the current code by adding
the option ``--depth=1`` to the git clone command above; all files will be
present, but it will not include the full git history. However, the repository
can be updated going forward.

Method 2
--------

This section describes how to do a one-time download of MITgcm, NOT git-aware.
In a terminal window, ``cd`` to the directory where
you want your code to reside. To obtain the current code, type:

::

    % wget https://github.com/MITgcm/MITgcm/archive/master.zip

For specific checkpoint release ``XXX``, instead type:

::

    % wget https://github.com/MITgcm/MITgcm/archive/checkpoint«XXX».zip

Updating the code
=================

There are several different approaches one can use to obtain updates to MITgcm;
which is best for you depends a bit on how you intend to use MITgcm and your
knowledge of git (and/or willingness to learn).
Below we outline three suggested update pathways:

1. **Fresh Download of MITgcm**

This approach is the most simple, and virtually foolproof. Whether you
downloaded the code from a static zip file (`Method 2`_) or used the git
clone command (`Method 1`_), create a new directory and repeat
this procedure to download a current copy of MITgcm. Say for example you
are starting a new research project, this would be a great time to grab the
most recent code repository and keep this new work entirely separate
from any past simulations. This approach requires no understanding of git,
and you are free to make changes to any files in the MIT repo tree
(although we generally recommend that you avoid doing so, instead working
in new subdirectories or on separate scratch disks as described
:ref:`here <build_elsewhere>`, for example).

2. **Using** ``git pull`` **to update the (unmodified) MITgcm repo tree**

If you have downloaded the code through a git clone command
(`Method 1`_ above), you can incorporate any changes to the source code
(including any changes to any files in the MITgcm repository, new packages
or analysis routines, etc.) that may have occurred since your
original download. There is a simple command to bring all code in the
repository to a ‘current release’ state. From the MITgcm top directory
or any of its subdirectories, type:

::

    % git pull

and all files will be updated to match the current state of the code
repository, as it exists at `GitHub <https://github.com/MITgcm/MITgcm.git>`_.
(*Note:* if you plan to contribute to MITgcm and followed the steps to
download the code as described in :numref:`chap_contributing`,
you will need to type ``git pull upstream`` instead.)

This update pathway is ideal if you are in the midst of a project and
you want to incorporate new MITgcm features into your executable(s),
or take advantage of recently added analysis utilties, etc.
After the git pull, any changes in model source code and include files
will be updated, so you can repeat the build procedure
(:numref:`building_code`) and you will include all these new features
in your new executable.

Be forewarned, this will only work if you have not modified ANY of the
files in the MITgcm repository (adding new files is ok; also, all
verification run subdirectories ``build`` and ``run`` are also ignored by git).
If you have modified files and the ``git pull`` fails with errors,
there is no easy fix other than to learn something
about git (continue reading...)

3. **Fully embracing the power of git!**

Git offers many tools to help organize and track changes in your work.
For example, one might keep separate projects on different branches,
and update the code separately (using ``git pull``) on these separate branches.
You can even make changes to code in the MIT repo tree; when git then
tries to update code from upstream (see :numref:`git_setup`), it will notify
you about possible conflicts and even merge the code changes
together if it can. You can also use ``git commit`` to help you track what
you are modifying in your simulations over time. If you're planning to
submit a pull request to include your changes, you should read the
contributing guide in :numref:`chap_contributing`, and we suggest you do
this model development in a separate, fresh copy of the code. See
:numref:`using_git_and_github` for more information and how
to use git effectively to manage your workflow.

Model and directory structure
=============================

The “numerical” model is contained within a execution environment
support wrapper. This wrapper is designed to provide a general framework
for grid-point models; MITgcm is a specific numerical model that makes use of
this framework (see :numref:`wrapper` for additional detail).
Under this structure, the model is split into execution
environment support code and conventional numerical model code. The
execution environment support code is held under the :filelink:`eesupp`
directory. The grid point model code is held under the :filelink:`model`
directory. Code execution actually starts in the :filelink:`eesupp` routines
and not in the :filelink:`model` routines. For this reason the top-level
:filelink:`main.F <eesupp/src/main.F>` is in the :filelink:`eesupp/src`
directory. In general, end-users should not need to worry about the
wrapper support code. The top-level routine for the numerical
part of the code is in :filelink:`model/src/the_model_main.F`. Here is a brief
description of the directory structure of the model under the root tree.

-  :filelink:`model`: this directory contains the main source code. Also
   subdivided into two subdirectories: :filelink:`model/inc` (includes files)
   and :filelink:`model/src` (source code).

-  :filelink:`eesupp`: contains the execution environment source code.
   Also subdivided into two subdirectories: :filelink:`eesupp/inc`
   and :filelink:`eesupp/src`.

-  :filelink:`pkg`: contains the source code for the packages. Each package
   corresponds to a subdirectory. For example, :filelink:`pkg/gmredi`
   contains the code related to the Gent-McWilliams/Redi scheme,
   :filelink:`pkg/seaice` the code for a dynamic seaice model which can be
   coupled to the ocean model. The packages are described in detail in
   :numref:`packagesI` and :numref:`outp_pack`].

-  :filelink:`doc`: contains MITgcm documentation in reStructured
   Text (rst) format.

-  :filelink:`tools`: this directory contains various useful tools.
   For example, :filelink:`genmake2 <tools/genmake2>` is a script written
   in bash that should be used to generate your makefile.
   The subdirectory :filelink:`tools/build_options` contains
   ‘optfiles’ with the compiler options for many different
   compilers and machines that can run MITgcm
   (see :numref:`genmake2_optfiles`). This directory also contains
   subdirectories :filelink:`tools/adjoint_options` and
   :filelink:`tools/OAD_support` that are used to generate the tangent linear
   and adjoint model (see details in :numref:`chap_autodiff`).

-  :filelink:`utils`: this directory contains various utilities.
   The :filelink:`utils/matlab` subdirectory
   contains matlab scripts for reading model output directly into
   matlab. The subdirectory :filelink:`utils/python` contains similar
   routines for python. :filelink:`utils/scripts` contains C-shell
   post-processing scripts for joining processor-based and
   tiled-based model output.

-  :filelink:`verification`: this directory contains the model examples. See
   :numref:`chap_modelExamples`.

-  :filelink:`jobs`: contains sample job scripts for running MITgcm.

-  :filelink:`lsopt`: Line search code used for optimization.

-  :filelink:`optim`: Interface between MITgcm and line search code.

.. _building_code:

Building the model
==================

.. _building_quickstart:

Quickstart Guide
----------------

To compile the code, we use the ``make`` program. This uses a file
(``Makefile``) that allows us to pre-process source files, specify
compiler and optimization options and also figures out any file
dependencies. We supply a script (:filelink:`genmake2 <tools/genmake2>`),
described in section :numref:`genmake2_desc`, that automatically generates
the ``Makefile`` for you. You then need to build the dependencies and
compile the code (:numref:`make_target_commands`).

As an example, assume that you want to build and run experiment
:filelink:`verification/exp2`.
Let’s build the code in :filelink:`verification/exp2/build`:

::

    % cd verification/exp2/build

First, generate the ``Makefile``:

::

    % ../../../tools/genmake2 -mods ../code -optfile «/PATH/TO/OPTFILE»

The ``-mods`` command line option tells :filelink:`genmake2 <tools/genmake2>`
to override model source code with any files in the subdirectory ``../code``
(here, you need to configure the size of the model domain by overriding
MITgcm’s default :filelink:`SIZE.h <model/inc/SIZE.h>`
with an edited copy :filelink:`../code/SIZE.h <verification/exp2/code/SIZE.h>`
containing the specific domain size for :filelink:`exp2 <verification/exp2>`).

The ``-optfile`` command line option tells :filelink:`genmake2 <tools/genmake2>`
to run the specified
:ref:`optfile <genmake2_optfiles>`, a  `bash <https://en.wikipedia.org/wiki/Bash_(Unix_shell)>`_
shell script, during :filelink:`genmake2 <tools/genmake2>`’s execution.
An :ref:`optfile <genmake2_optfiles>` typically contains
definitions of
`environment variables <https://en.wikipedia.org/wiki/Environment_variable>`_,
paths, compiler options, and anything else that needs to be set in order
to compile on your local computer system or cluster with your specific
Fortan compiler. As an example, we might replace ``«/PATH/TO/OPTFILE»`` with
:filelink:`../../../tools/build_options/linux_amd64_ifort11 <tools/build_options/linux_amd64_ifort11>`
for use with the
`Intel Fortran <https://software.intel.com/en-us/fortran-compilers>`_ compiler
(version 11 and above) on a linux x86_64 platform.
This and many other :ref:`optfiles <genmake2_optfiles>` for common systems
and Fortran compilers are located in :filelink:`tools/build_options`.

``-mods``, ``-optfile``, and many additional
:filelink:`genmake2 <tools/genmake2>` command line options are described
more fully in :numref:`command_line_options`. Detailed instructions on
building with
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
are given in :numref:`build_mpi`.

Once a ``Makefile`` has been generated, we create the dependencies with
the command:

::

    % make depend

It is important to note that the ``make depend`` stage will occasionally
produce warnings or errors if the dependency parsing tool is unable
to find all of the necessary header files (e.g., ``netcdf.inc``, or worse,
say it cannot find a Fortran compiler in your path). In some cases you
may need to obtain help from your system administrator to locate these files.

Next, one can compile the code using:

::

    % make

Assuming no errors occurred, the ``make`` command creates
an executable called ``mitgcmuv``.

Now you are ready to run the model. General instructions for doing so
are given in section :numref:`run_the_model`.

.. _genmake2_desc:

Generating a ``Makefile`` using genmake2
----------------------------------------

A shell script called ``genmake2`` for generating a ``Makefile`` is
included as part of MITgcm.
Typically ``genmake2`` is used in a sequence of steps as shown below:

::

  % ../../../tools/genmake2 -mods ../code -optfile «/PATH/TO/OPTFILE»
  % make depend
  % make

The first step above creates a unix-style ``Makefile``. The ``Makefile``
is used by ``make`` to specify how to compile the MITgcm source files
(for more detailed descriptions of what the ``make`` tools
are, and how they are used, see
`here <https://www.gnu.org/software/make/make.html>`__).

This section describes details and capabilities of
:filelink:`genmake2 <tools/genmake2>`, located in the
:filelink:`tools` directory. :filelink:`genmake2 <tools/genmake2>` is a shell
script written to work in
`bash <https://en.wikipedia.org/wiki/Bash_(Unix_shell)>`_ (and with all
“sh”–compatible shells including
`Bourne <https://en.wikipedia.org/wiki/Bourne_shell>`_ shells).
Like many unix tools, there is a help option that is invoked
thru ``genmake2 -h``. :filelink:`genmake2 <tools/genmake2>` parses
information from the following sources, in this order:

#.    Command-line options (see :numref:`command_line_options`)

#.    A ``genmake_local`` file if one is found in the current directory.
      This is a `bash <https://en.wikipedia.org/wiki/Bash_(Unix_shell)>`_ shell
      script that is executed prior to the :ref:`optfile <genmake2_optfiles>`
      (see step #3), used in some special model configurations and/or to
      set some options that can affect which lines of the
      :ref:`optfile <genmake2_optfiles>` are executed.
      For example, this
      :filelink:`genmake_local <verification/cpl_aim+ocn/build_cpl/genmake_local>`
      file is required for a special setup, building a ‘MITgcm coupler’
      executable; in a more typical setup, one will not require a
      ``genmake_local`` file.

#.    An “options file” a.k.a. :ref:`optfile <genmake2_optfiles>`
      (a `bash <https://en.wikipedia.org/wiki/Bash_(Unix_shell)>`_ shell
      script) specified by the command-line option
      ``–optfile «/PATH/TO/OPTFILE»``, as mentioned briefly
      in :numref:`building_quickstart`
      and described in detail in :numref:`genmake2_optfiles`.

#.    A ``packages.conf`` file (if one is found) with the specific list of
      packages to compile (see :numref:`using_packages`). The search path for
      file ``packages.conf`` is first the current directory, and then each
      of the ``-mods`` directories in the given order (as described
      :ref:`here <mods_option>`).

When you run the :filelink:`genmake2 <tools/genmake2>` script,
typical output might be as follows:

::

  % ../../../tools/genmake2 -mods ../code -optfile ../../../tools/build_options/linux_amd64_gfortran

  GENMAKE :

  A program for GENerating MAKEfiles for the MITgcm project.
     For a quick list of options, use "genmake2 -h"
  or for more detail see the documentation, section "Building the model"
     (under "Getting Started") at:  https://mitgcm.readthedocs.io/

  ===  Processing options files and arguments  ===
    getting local config information:  none found
  Warning: MITgcm root directory was not specified ; try using a local copy of MITgcm found at "../../.."
    getting OPTFILE information:
      using OPTFILE="../../../tools/build_options/linux_amd64_gfortran"
    getting AD_OPTFILE information:
      using AD_OPTFILE="../../../tools/adjoint_options/adjoint_default"
    check Fortran Compiler...  pass  (set FC_CHECK=5/5)
    check makedepend (local: 0, system: 1, 1)

  ===  Checking system libraries  ===
    Do we have the system() command using gfortran...  yes
    Do we have the fdate() command using gfortran...  yes
    Do we have the etime() command using gfortran... c,r: yes (SbR)
    Can we call simple C routines (here, "cloc()") using gfortran...  yes
    Can we unlimit the stack size using gfortran...  yes
    Can we register a signal handler using gfortran...  yes
    Can we use stat() through C calls...  yes
    Can we create NetCDF-enabled binaries...  yes
      skip check for LAPACK Libs
    Can we call FLUSH intrinsic subroutine...  yes

  ===  Setting defaults  ===
    Adding MODS directories: ../code
    Making source files in eesupp from templates
    Making source files in pkg/exch2 from templates
    Making source files in pkg/regrid from templates

  ===  Determining package settings  ===
    getting package dependency info from  ../../../pkg/pkg_depend
    getting package groups info from      ../../../pkg/pkg_groups
    checking list of packages to compile:
      using PKG_LIST="../code/packages.conf"
      before group expansion packages are: oceanic -kpp -gmredi cd_code
      replacing "oceanic" with:  gfd gmredi kpp
      replacing "gfd" with:  mom_common mom_fluxform mom_vecinv generic_advdiff debug mdsio rw monitor
      after group expansion packages are:  mom_common mom_fluxform mom_vecinv generic_advdiff debug mdsio rw monitor gmredi kpp -kpp -gmredi cd_code
    applying DISABLE settings
    applying ENABLE settings
      packages are:  cd_code debug generic_advdiff mdsio mom_common mom_fluxform mom_vecinv monitor rw
    applying package dependency rules
      packages are:  cd_code debug generic_advdiff mdsio mom_common mom_fluxform mom_vecinv monitor rw
    Adding STANDARDDIRS='eesupp model'
    Searching for *OPTIONS.h files in order to warn about the presence
      of "#define "-type statements that are no longer allowed:
      found CPP_EEOPTIONS="../../../eesupp/inc/CPP_EEOPTIONS.h"
      found CPP_OPTIONS="../../../model/inc/CPP_OPTIONS.h"
    Creating the list of files for the adjoint compiler.

  ===  Creating the Makefile  ===
    setting INCLUDES
    Determining the list of source and include files
    Writing makefile: Makefile
    Add the source list for AD code generation
    Making list of "exceptions" that need ".p" files
    Making list of NOOPTFILES
    Add rules for links
    Adding makedepend marker

  ===  Done  ===
    original 'Makefile' generated successfully
  => next steps:
    > make depend
    > make       (<-- to generate executable)

In the above, notice:

- we did not specify MITgcm root directory,
  i.e., a path to your MITgcm repository,
  but here we are building code from within the repository (specifically,
  in one of the verification subdirectory experiments). As such,
  :filelink:`genmake2 <tools/genmake2>` was smart enough to
  locate all necessary files on its own. To specify a remote MITgcm root directory,
  see :ref:`here <build_elsewhere>`.
- we specified the :ref:`optfile <genmake2_optfiles>`
  :filelink:`linux_amd64_gfortran <tools/build_options/linux_amd64_gfortran>`
  based on the computer system and Fortran compiler we used
  (here, a linux 64-bit machine with gfortran installed).
- :filelink:`genmake2 <tools/genmake2>` did
  some simple checking on availability
  of certain system libraries; all were found (except
  `LAPACK <https://en.wikipedia.org/wiki/LAPACK>`_,
  which was not checked since it is not needed here).
  `NetCDF <http://www.unidata.ucar.edu/software/netcdf>`_ only requires a ‘yes’
  if you want to write `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
  output; more specifically, a ‘no’ response to “Can we create NetCDF-enabled
  binaries” will disable including  :filelink:`pkg/mnc` and switch to output
  plain binary files. While the makefile can still be built with other
  ‘no’ responses, sometimes this will foretell errors during the
  ``make depend`` or ``make`` commands.
- any ``.F`` or ``.h`` files in the ``-mods`` directory ``../code`` will
  also be compiled, overriding any MITgcm repository versions of files,
  if they exist.
- a handful of packages are being used in this build; see
  :numref:`using_packages` for more detail about how to enable and disable
  packages.
- :filelink:`genmake2 <tools/genmake2>` terminated without error
  (note output at end after ``===  Done  ===``), generating ``Makefile`` and
  a log file ``genmake.log``. As mentioned, this does not guarantee that
  your setup will compile properly, but if there are errors during
  ``make depend`` or ``make``, these error messages and/or the
  standard output from :filelink:`genmake2 <tools/genmake2>` or
  ``genmake.log`` may provide clues as to the problem.
  If instead :filelink:`genmake2 <tools/genmake2>` finishes with
  a warning message  ``Warning: FORTRAN compiler test failed`` , this means
  that :filelink:`genmake2 <tools/genmake2>` is unable to locate the Fortran
  compiler or pass a trivial “hello world” Fortran compilation test.
  In this case, you should see ``genmake.log`` for errors and/or seek
  assistance from your system administrator;
  these tests need to pass in order to proceed to the ``make`` steps.

.. _command_line_options:

Command-line options:
~~~~~~~~~~~~~~~~~~~~~

:filelink:`genmake2 <tools/genmake2>` supports a number of helpful
command-line options. A complete list of these options can be obtained by:

::

    % genmake2 -h

The most important command-line options are:

``–optfile «/PATH/TO/OPTFILE»``
    (or shorter: ``-of`` ) specifies the :ref:`optfile <genmake2_optfiles>`
    that should be used for a particular build.

    If no :ref:`optfile <genmake2_optfiles>` is specified through the command
    line, :filelink:`genmake2 <tools/genmake2>` will try to make a
    reasonable guess from the list provided in :filelink:`tools/build_options`.
    The method used for making this guess is to first determine the
    combination of operating system and hardware and
    then find a working Fortran compiler within the user’s path. When
    these three items have been identified,
    :filelink:`genmake2 <tools/genmake2>` will try to find an
    :ref:`optfile <genmake2_optfiles>` that has a matching name.
    See :numref:`genmake2_optfiles`.

.. _mods_option:

``–mods '«DIR1 DIR2 DIR3 ...»'``
    specifies a list of directories containing “modifications”. These
    directories contain files with names that may (or may not) exist in
    the main MITgcm source tree but will be overridden by any
    identically-named sources within the ``-mods`` directories.
    Note the quotes around the list of directories,
    necessary given multiple arguments.

    The order of precedence for versions of files with identical names:

    -  “mods” directories in the order given (e.g., will use copy of file
       located in DIR1 instead of DIR2)

    -  Packages either explicitly specified or included by default

    -  Packages included due to package dependencies

    -  The “standard dirs” (which may have been specified by the
       ``-standarddirs`` option below)

.. _build_elsewhere:

``-rootdir «/PATH/TO/MITGCMDIR»``
    specify the location of the MITgcm repository top directory (MITgcm root directory).
    By default, :filelink:`genmake2 <tools/genmake2>` will try to find this
    location by looking in parent directories from where
    :filelink:`genmake2 <tools/genmake2>` is executed
    (up to 5 directory levels above the current directory).

    In the quickstart example above (:numref:`building_quickstart`) we built
    the executable in the ``build`` directory of the experiment.
    Below, we show how to configure and compile the code on a scratch disk,
    without having to copy the entire source
    tree. The only requirement is that you have
    :filelink:`genmake2 <tools/genmake2>`
    in your `$PATH <https://en.wikipedia.org/wiki/PATH_(variable)>`_, or
    you know the absolute path to :filelink:`genmake2 <tools/genmake2>`.
    In general, one can compile the code in any given directory by following
    this procedure. Assuming the model source is in ``~/MITgcm``, then the
    following commands will build the model in ``/scratch/exp2-run1``:

    ::

       % cd /scratch/exp2-run1
       % ~/MITgcm/tools/genmake2 -rootdir ~/MITgcm -mods ~/MITgcm/verification/exp2/code
       % make depend
       % make

    As an alternative to specifying the MITgcm repository location through
    the ``-rootdir`` command-line option, :filelink:`genmake2 <tools/genmake2>`
    recognizes the
    `environment variable <https://en.wikipedia.org/wiki/Environment_variable>`_
    ``$MITGCM_ROOTDIR``.

``-standarddirs «/PATH/TO/STANDARDDIR»``
    specify a path to the standard MITgcm directories for source and includes
    files. By default, :filelink:`model` and :filelink:`eesupp`
    directories (``src`` and ``inc``)  are the “standard dirs”.
    This command can be used
    to reset these default standard directories,
    or instead NOT include either :filelink:`model` or :filelink:`eesupp`
    as done in some specialized configurations.

``-oad``
    generates a makefile for an OpenAD build (see :numref:`ad_openad`)

``–adoptfile «/PATH/TO/FILE»``
    (or shorter: ``-adof`` ) specifies the “adjoint” or automatic
    differentiation options file to be used. The file is analogous to the
    optfile defined above but it specifies information for the AD
    build process. See :numref:`adoptfile`.

    The default file is located in
    :filelink:`tools/adjoint_options/adjoint_default` and it defines the “TAF”
    and “TAMC” compiler options.

``–mpi``
    enables certain
    `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
    features (using CPP ``#define``) within the code and is necessary for
    `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ builds
    (see :numref:`build_mpi`).

``–omp``
    enables OpenMP code and compiler flag ``OMPFLAG``
    (see :numref:`build_openmp`).

``–ieee``
    use IEEE numerics (requires support in optfile).
    This option is typically a good choice if one wants to compare output
    from different machines running the same code.
    Note using IEEE disables all compiler optimizations.

``-devel``
    use IEEE numerics (requires support in optfile) and add additional
    compiler options to check array bounds and add other additional warning
    and debugging flags.

``–make «/PATH/TO/GMAKE»``
    due to the poor handling of soft-links and other bugs common with
    the ``make`` versions provided by commercial unix vendors, GNU
    ``make`` (sometimes called ``gmake``) may be preferred. This
    option provides a means for specifying the make executable to be
    used.

While it is possible to use :filelink:`genmake2 <tools/genmake2>` command-line
options to set the Fortran or C compiler name (``-fc`` and ``-cc``
respectively), we generally recommend setting these through an
:ref:`optfile <genmake2_optfiles>`, as discussed in :numref:`genmake2_optfiles`.
Other :filelink:`genmake2 <tools/genmake2>` options  are available to
enable performance/timing analyses, etc.; see ``genmake2 -h`` for more info.

.. _genmake2_optfiles:

Optfiles in tools/build_options directory:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The purpose of the optfiles is to provide all the compilation options
for particular “platforms” (where “platform” roughly means the
combination of the hardware and the compiler) and code configurations.
Given the combinations of possible compilers and library dependencies
(e.g., `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ or
`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_) there may be
numerous optfiles available for a
single machine. The naming scheme for the majority of the optfiles
shipped with the code is **OS_HARDWARE_COMPILER** where

**OS**
    is the name of the operating system (generally the lower-case output
    of a linux terminal ``uname`` command)

**HARDWARE**
    is a string that describes the CPU type and corresponds to output
    from a ``uname -m`` command. Some common CPU types:

    ``amd64``
        use this code for x86_64 systems (most common, including AMD and
        Intel 64-bit CPUs)

    ``ia64``
        is for Intel IA64 systems (eg. Itanium, Itanium2)

    ``ppc``
        is for (old) Mac PowerPC systems

**COMPILER**
    is the compiler name (generally, the name of the Fortran compiler
    executable). MITgcm is primarily written in
    `FORTRAN 77 <https://en.wikipedia.org/wiki/Fortran#FORTRAN_77>`_.
    Compiling the code  requires a
    `FORTRAN 77 <https://en.wikipedia.org/wiki/Fortran#FORTRAN_77>`_ compiler.
    Any more recent compiler which is backwards compatible with
    `FORTRAN 77 <https://en.wikipedia.org/wiki/Fortran#FORTRAN_77>`_
    can also be used; for example, the model will build successfully
    with a `Fortran 90 <https://en.wikipedia.org/wiki/Fortran#Fortran_90>`_
    or  `Fortran 95 <https://en.wikipedia.org/wiki/Fortran#Fortran_95>`_
    compiler. A `C99 <https://en.wikipedia.org/wiki/C99>`_ compatible compiler
    is also need, together with a
    `C preprocessor <https://en.wikipedia.org/wiki/C_preprocessor>`_ .
    Some optional packages make use of
    `Fortran 90 <https://en.wikipedia.org/wiki/Fortran#Fortran_90>`_ constructs
    (either
    `free-form formatting <https://en.wikipedia.org/wiki/Free-form_language>`_,
    or
    `dynamic memory allocation <https://en.wikipedia.org/wiki/Memory_management#DYNAMIC>`_);
    as such, setups which use these packages require a
    `Fortran 90 <https://en.wikipedia.org/wiki/Fortran#Fortran_90>`_
    or later compiler build.

There are existing optfiles that work with many common hardware/compiler
configurations; we first suggest you peruse the list in
:filelink:`tools/build_options` and try to find your platform/compiler
configuration. These are the most common:

- :filelink:`linux_amd64_gfortran <tools/build_options/linux_amd64_gfortran>`
- :filelink:`linux_amd64_ifort11 <tools/build_options/linux_amd64_ifort11>`
- :filelink:`linux_amd64_ifort+impi <tools/build_options/linux_amd64_ifort+impi>`
- :filelink:`linux_amd64_pgf77 <tools/build_options/linux_amd64_pgf77>`

The above optfiles are all for linux x86_64 (64-bit) systems, utilized in many
large high-performance computing centers. All of the above will work with
single-threaded,
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_,
or shared memory (`OpenMP <https://en.wikipedia.org/wiki/OpenMP>`_) code
configurations. gfortran is `GNU Fortran <https://gcc.gnu.org/fortran>`_,
ifort is `Intel Fortran <https://software.intel.com/en-us/fortran-compilers>`_,
pgf77 is `PGI Fortran <https://www.pgroup.com/>`_ (formerly known as
“The Portland Group”). Note in the above list there are two ``ifort`` optfiles:
:filelink:`linux_amd64_ifort+impi <tools/build_options/linux_amd64_ifort+impi>`
is for a specific case of using ``ifort`` with the
`Intel MPI library <https://software.intel.com/en-us/intel-mpi-library>`_
(a.k.a. ``impi``), which requires special define statements in the optfile
(in contrast with `Open MPI <https://www.open-mpi.org/>`_ or
`MVAPICH2 <http:mvapich.cse.ohio-state.edu/>`_ libraries;
see :numref:`build_mpi`). Note that both ifort optfiles require ifort
version 11 or higher. Many clusters nowadays use
`environment modules <http:modules.sourceforge.net>`_,
which allows one to easily choose which compiler to use through
``module load «MODULENAME»``, automatically configuring your environment
for a specific compiler choice (type ``echo $PATH`` to see where
:filelink:`genmake2 <tools/genmake2>` will look for compilers
and system software).

In most cases, your platform configuration will be included in the available
optfiles :filelink:`list <tools/build_options/>` and will result in a
usable ``Makefile`` being generated. If you are unsure which optfile is
correct for your configuration, you can try not specifying an optfile;
on some systems the :filelink:`genmake2 <tools/genmake2>` program will be able
to automatically recognize the hardware, find a compiler and other tools
within the user’s path, and then make a best guess as to an appropriate optfile
from the list in the :filelink:`tools/build_options` directory.
However, for some platforms and code configurations, new
optfiles must be written. To create a new optfile, it is generally
best to start with one of the defaults and modify it to suit your needs.
Like
:filelink:`genmake2 <tools/genmake2>`, the optfiles are all written in `bash <https://en.wikipedia.org/wiki/Bash_(Unix_shell)>`_
(or using a simple
`sh–compatible <https://en.wikipedia.org/wiki/Bourne_shell>`_ syntax).
While nearly all
`environment variables <https://en.wikipedia.org/wiki/Environment_variable>`_
used within :filelink:`genmake2 <tools/genmake2>` may be specified in the
optfiles, the critical ones that should be defined are:

.. _list_of_optfile_env_vars:

``FC``
    the Fortran compiler (executable) to use on ``.F`` files, e.g., ``ifort``
    or ``gfortran``, or if using MPI, the mpi-wrapper equivalent,
    e.g., ``mpif77``

``F90C``
    the Fortran compiler to use on ``.F90`` files (only necessary if your setup
    includes a package which contains ``.F90`` source code)

``CC``
    similarly, the C compiler to use, e.g., ``icc`` or ``gcc``, or if using MPI,
    the mpi-wrapper equivalent, e.g., ``mpicc``

``DEFINES``
    command-line options passed to the compiler

``CPP``
    the C preprocessor to use, and any necessary command-line options,
    e.g. ``cpp -traditional -P``

``CFLAGS``, ``FFLAGS``
    command-line compiler flags required for your C and Fortran compilers,
    respectively, to compile and execute properly. See your C and Fortran
    compiler documentation for specific options and syntax.

``FOPTIM``
    command-line optimization Fortran compiler settings. See your Fortran
    compiler documentation for specific options and syntax.

``NOOPTFLAGS``
    command-line settings for special files that should not be optimized
    using the ``FOPTIM`` flags

``NOOPTFILES``
    list of source code files that should be compiled using ``NOOPTFLAGS``
    settings

``INCLUDES``
    path for additional files (e.g., ``netcdf.inc``, ``mpif.h``) to include
    in the compilation using the command-line ``-I`` option

``INCLUDEDIRS``
   path for additional files to be included in the compilation

``LIBS``
   path for additional library files that need to be linked to generate the
   final executable, e.g., ``libnetcdf.a``

For example, an excerpt from an optfile which specifies several of these
variables (here, for the linux-amd64 architecture using the PGI Fortran
compiler) is as follows:

.. literalinclude:: ../../tools/build_options/linux_amd64_pgf77
    :start-at: if test "x$MPI" = xtrue ; then
    :end-at: F90OPTIM=$FOPTIM

The
:ref:`above <list_of_optfile_env_vars>` list of `environment variables <https://en.wikipedia.org/wiki/Environment_variable>`_
typically specified in an optfile is by no means complete;
additional variables may be required for your specific setup and/or your
specific Fortran (or C) compiler.

If you write an optfile for an unrepresented machine or compiler, you
are strongly encouraged to submit the optfile to the MITgcm project for
inclusion. MITgcm developers are willing to
provide help writing or modifing optfiles.  Please submit the file through
the `GitHub issue tracker <https://github.com/MITgcm/MITgcm/issues>`_
or email the MITgcm-support@mitgcm.org list.

Instructions on how to use optfiles to build
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_\ -enabled
executables is presented in :numref:`build_mpi`.

.. _make_target_commands:

``make`` commands
-----------------

Following a successful build of ``Makefile``, type ``make depend``. This
command modifies the ``Makefile`` by attaching a (usually, long) list of
files upon which other files depend. The purpose of this is to reduce
re-compilation if and when you start to modify the code. The ``make depend``
command also creates local links for all source files from the source
directories (see "-mods" description in :numref:`command_line_options`),
so that all source files to be used are visible from the local build directory,
either as hardcopy or as symbolic link.

**IMPORTANT NOTE:** Editing the source code files in the build directory
will not edit a local copy (since these are just links) but will
edit the original files in :filelink:`model/src` (or :filelink:`model/inc`)
or in the specified ``-mods`` directory. While the latter might
be what you intend, editing the master copy in :filelink:`model/src`
is usually **NOT** what is intended and may cause grief somewhere down
the road. Rather, if you need to add
to the list of modified source code files, place a copy of
the file(s) to edit in the ``-mods`` directory, make the edits to
these ``-mods`` directory files, go back to the build directory and
type ``make Clean``, and then re-build the makefile (these latter steps
critical or the makefile will not link to this newly edited file).

The final ``make`` invokes the
`C preprocessor <https://en.wikipedia.org/wiki/C_preprocessor>`_
to produce the “little f” files (``*.f`` and ``*.f90``) and then compiles them
to object code using the specified Fortran compiler and options.
The C preprocessor step converts a number of CPP macros and ``#ifdef``
statements to actual Fortran and expands C-style ``#include`` statements to
incorporate header files into the “little f" files. CPP style macros and
``#ifdef`` statements are used to support generating
different compile code for different model configurations.
The result of the build process is an executable with the name ``mitgcmuv``.

Additional make “targets” are defined within the makefile to aid in the
production of adjoint (:numref:`building_adcode_using_taf`) and other
versions of MITgcm.

On computers with multiple processor cores, the build process can often be sped
up appreciably using the command:

::

    % make -j 2

where the “2” can be replaced with a number that corresponds to the
number of cores (or discrete CPUs) available.

In addition, there are several housekeeping ``make clean`` options that might
be useful:

- ``make clean`` removes files that ``make`` generates
  (e.g., \*.o and \*.f files)
- ``make Clean`` removes files and links generated by ``make`` and
  ``make depend``; strongly recommended for “un-clean” directories which
  may contain the (perhaps partial) results of previous builds
- ``make CLEAN`` removes pretty much everything, including any executables
  and output from :filelink:`genmake2 <tools/genmake2>`

.. _build_mpi:

Building with MPI
-----------------

Building MITgcm to use
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ libraries can
be complicated due to the variety of different
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
implementations available, their dependencies
or interactions with different compilers, and their often ad-hoc
locations within file systems. For these reasons, its generally a good
idea to start by finding and reading the documentation for your
machine(s) and, if necessary, seeking help from your local systems
administrator.

The steps for building MITgcm with
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ support are:

#. Make sure you have
   `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ libraries
   installed on your computer system or cluster. Different Fortran compilers
   (and different versions of a specific compiler) will generally require a
   custom version (of a
   `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ library)
   built specifically for it.
   On `environment module <http:modules.sourceforge.net>`_-enabled
   clusters, one typically must first load a
   Fortran compiler, then specific
   `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
   libraries for that compiler will become available to load.
   If libraries are not installed, MPI implementations and related tools are
   available including:

   -  `Open MPI <https://www.open-mpi.org/>`_

   -  `MVAPICH2 <http:mvapich.cse.ohio-state.edu/>`_

   -  `MPICH <https://www.mpich.org/>`_

   -  `Intel MPI <https://software.intel.com/en-us/intel-mpi-library/>`_

   Ask you systems administrator for assistance in installing these libraries.

#. Determine the location of your
   `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
   library “wrapper” Fortran compiler, e.g., ``mpif77`` or ``mpifort`` etc.
   which will be used instead of the name of the fortran compiler
   (``gfortran``, ``ifort``, ``pgi77`` etc.) to compile your code.
   Often the directory in which these wrappers are located will be
   automatically added to your
   `$PATH <https://en.wikipedia.org/wiki/PATH_(variable)>`_
   `environment variable <https://en.wikipedia.org/wiki/Environment_variable>`_
   when you perform a ``module load «SOME_MPI_MODULE»``; thus, you will not
   need to do anything beyond the module load itself.
   If you are on a cluster that does not support
   `environment modules <http:modules.sourceforge.net>`_,
   you may have to manually add this directory to your path,
   e.g., type ``PATH=$PATH:«ADD_ADDITIONAL_PATH_TO_MPI_WRAPPER_HERE»``
   in a bash shell.

#. Determine the location of the includes file ``mpif.h`` and any other
   `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_-related
   includes files. Often these files will be located in a subdirectory off
   the main `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
   library ``include/``. In all optfiles in :filelink:`tools/build_options`,
   it is assumed
   `environment variable <https://en.wikipedia.org/wiki/Environment_variable>`_
   ``$MPI_INC_DIR`` specifies this location; ``$MPI_INC_DIR``
   should be set in your terminal session prior to generating a ``Makefile``.

#. Determine how many processors (i.e., CPU cores) you will be using in your
   run, and modify your configuration’s :filelink:`SIZE.h <model/inc/SIZE.h>`
   (located in a “modified code” directory, as specified in your
   :filelink:`genmake2 <tools/genmake2>`
   :ref:`command-line <command_line_options>`).
   In :filelink:`SIZE.h <model/inc/SIZE.h>`,
   you will need to set variables :varlink:`nPx`\*\ :varlink:`nPy` to
   match the number of processors you will specify in
   your run script’s MITgcm execution statement (i.e., typically ``mpirun``
   or some similar command, see :numref:`running_mpi`).
   Note that MITgcm does not use
   `dynamic memory allocation <https://en.wikipedia.org/wiki/Memory_management#DYNAMIC>`_
   (a feature of
   `Fortran 90 <https://en.wikipedia.org/wiki/Fortran#Fortran_90>`_,
   not `FORTRAN 77 <https://en.wikipedia.org/wiki/Fortran#FORTRAN_77>`_), so
   all array sizes, and hence the number of processors
   to be used in your
   `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ run,
   must be specified at compile-time in addition to run-time. More information
   about the MITgcm WRAPPER, domain decomposition, and how to configure
   :filelink:`SIZE.h <model/inc/SIZE.h>`
   can be found in :numref:`using_wrapper`.

#. Build the code with the :filelink:`genmake2 <tools/genmake2>` ``-mpi``
   option using commands such as:

   ::

         %  ../../../tools/genmake2 -mods=../code -mpi -of=«/PATH/TO/OPTFILE»
         %  make depend
         %  make

.. _build_openmp:

Building  with OpenMP
---------------------

Unlike MPI, which requires installation of additional software support
libraries, using shared memory
(`OpenMP <https://en.wikipedia.org/wiki/OpenMP>`_) for multi-threaded
executable builds can be accomplished simply through the
:filelink:`genmake2 <tools/genmake2>` command-line option ``-omp``:

   ::

         %  ../../../tools/genmake2 -mods=../code -omp -of=«/PATH/TO/OPTFILE»
         %  make depend
         %  make

While the most common optfiles specified in :numref:`genmake2_optfiles` include
support for the ``-omp`` option, some optfiles in
:filelink:`tools/build_options` do not include support for multi-threaded
executable builds. Before using one of the less common optfiles,
check whether ``OMPFLAG`` is defined.

Note that one does not need to specify the number of threads until runtime
(see :numref:`running_openmp`). However, the default maximum number of threads
in MITgcm is set to a (low) value of 4, so if you plan on more you will need
to change this value in :filelink:`eesupp/inc/EEPARAMS.h` in your modified
code directory.

.. _run_the_model:

Running the model
=================

If compilation finished successfully (:numref:`building_code`) then an
executable called ``mitgcmuv`` will now exist in the
local (``build``) directory.

To run the model as a single process (i.e., not in parallel) simply
type (assuming you are still in the ``build`` directory):

::

    % cd ../run
    % ln -s ../input/* .
    % cp ../build/mitgcmuv .
    % ./mitgcmuv

Here, we are making a link to all the support data files (in ``../input/``)
needed by the MITgcm for this experiment, and then copying the executable from
the the build directory. The ``./`` in the last step is a safe-guard to make
sure you use the local executable in case you have others that might exist in
your ``$PATH``. The above command will spew out many lines of text output to
your screen. This output contains details such as parameter values as well as
diagnostics such as mean kinetic energy, largest CFL number, etc. It is
worth keeping this text output with the binary output so we normally
re-direct the ``stdout`` stream as follows:

::

    % ./mitgcmuv > output.txt

In the event that the model encounters an error and stops, it is very
helpful to include the last few line of this ``output.txt`` file along
with the (``stderr``) error message within any bug reports.

For the example experiment in :filelink:`verification/exp2`, an example of the
output is kept in :filelink:`verification/exp2/results/output.txt` for
comparison. You can compare your ``output.txt`` with the corresponding one for
that experiment to check that your set-up indeed works. Congratulations!

.. _running_mpi:

Running with MPI
----------------

Run the code with the appropriate
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ “run” or
“exec” program provided with your particular implementation of
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_.
Typical `MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_
packages such as `Open MPI <https://www.open-mpi.org/>`_ will
use something like:

   ::

         %  mpirun -np 4 ./mitgcmuv

Sightly more complicated scripts may be needed for many machines
since execution of the code may be controlled by both the
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ library
and a job scheduling and queueing system such as
`Slurm <https://slurm.schedmd.com/>`_,
`PBS/TORQUE <http://www.adaptivecomputing.com/products/open-source/torque>`_,
`LoadLeveler <https://www-03.ibm.com/systems/power/software/loadleveler/>`_,
or any of a number of similar tools. See your local cluster documentation
or system administrator for the specific syntax required to run on your
computing facility.

.. _running_openmp:

Running with OpenMP
-------------------

Assuming the executable ``mitgcmuv`` was built with OpenMP
(see :numref:`build_openmp`), the syntax to run a multi-threaded simulation is
the same as running single-threaded (see :numref:`run_the_model`), except that
the following additional steps are required beforehand:

#. `Environment variables <https://en.wikipedia.org/wiki/Environment_variable>`_
   for the number of threads and the stacksize need to be set prior to
   executing the model. The exact names of these
   `environment variables <https://en.wikipedia.org/wiki/Environment_variable>`_
   differ by Fortran compiler, but are typically some variant of
   ``OMP_NUM_THREADS`` and ``OMP_STACKSIZE``, respectively.
   For the latter, in your run script we recommend adding the line
   ``export OMP_STACKSIZE=400M``  (or for a
   `C shell <https://en.wikipedia.org/wiki/C_shell>`_-variant,
   ``setenv OMP_STACKSIZE 400M``). If this stacksize setting is insufficient,
   MITgcm will crash, in which case a larger number can be used. Similarly,
   ``OMP_NUM_THREADS`` should be set to the exact number of threads you require.

#. In file ``eedata`` you will need to change namelist parameters :varlink:`nTx`
   and :varlink:`nTy` to reflect the number of threads in x and y, respectively
   (for a single-threaded run, :varlink:`nTx` \=\ :varlink:`nTy`\ =1).
   The value of :varlink:`nTx` \*\ :varlink:`nTy` must equal the value of
   `environment variable <https://en.wikipedia.org/wiki/Environment_variable>`_
   ``OMP_NUM_THREADS`` (or its name-equivalent for your Fortan compiler) or
   MITgcm will terminate during its initialization with an error message.

MITgcm will take the number of tiles used in the model (as specified in
:filelink:`SIZE.h <model/inc/SIZE.h>`) and the number of threads
(:varlink:`nTx` and :varlink:`nTy` from file ``eedata``),
and in running will spread the tiles out evenly across the threads.
This is done independently for x and y. As such,
the number of tiles in x (variable :varlink:`nSx` as defined in
:filelink:`SIZE.h <model/inc/SIZE.h>`) must divide evenly by
the number of threads in x (namelist parameter :varlink:`nTx`),
and similarly for :varlink:`nSy` and :varlink:`nTy`, else MITgcm will
terminate on initialization. More information about the MITgcm
WRAPPER, domain decomposition, and how to configure
:filelink:`SIZE.h <model/inc/SIZE.h>` can be found in :numref:`using_wrapper`.

Output files
------------

The model produces various output files and, when using :filelink:`pkg/mnc`
(i.e., `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_),
sometimes even directories. Depending upon the I/O package(s) selected
at compile time (either :filelink:`pkg/mdsio`, :filelink:`pkg/mnc`, or both as
determined by ``packages.conf``) and the run-time flags set (in
``data.pkg``), the following output may appear. More complete information
describing output files and model diagnostics is described
in :numref:`outp_pack`.

Raw binary output files
~~~~~~~~~~~~~~~~~~~~~~~

The “traditional” output files are generated by the :filelink:`pkg/mdsio`
(see :numref:`pkg_mdsio`).The :filelink:`pkg/mdsio` model data are written
according to a “meta/data” file format. Each variable is associated with two
files with suffix names ``.data`` and ``.meta``. The ``.data`` file contains
the data written in binary form (big endian by default). The ``.meta`` file
is a “header” file that contains information about the size and the
structure of the ``.data`` file. This way of organizing the output is
particularly useful when running multi-processors calculations.

At a minimum, the instantaneous “state” of the model is written out,
which is made of the following files:

-  ``U.00000nIter`` - zonal component of velocity field (m/s and
   positive eastward).

-  ``V.00000nIter`` - meridional component of velocity field (m/s and
   positive northward).

-  ``W.00000nIter`` - vertical component of velocity field (ocean: m/s
   and positive upward, atmosphere: Pa/s and positive towards increasing
   pressure i.e., downward).

-  ``T.00000nIter`` - potential temperature (ocean:
   :math:`^{\circ}\mathrm{C}`, atmosphere: :math:`\mathrm{K}`).

-  ``S.00000nIter`` - ocean: salinity (g/kg), atmosphere: water vapor
   (g/kg).

-  ``Eta.00000nIter`` - ocean: surface elevation (m), atmosphere:
   surface pressure anomaly (Pa).

The chain ``00000nIter`` consists of ten figures that specify the
iteration number at which the output is written out. For example,
``U.0000000300`` is the zonal velocity at iteration 300.

In addition, a “pickup” or “checkpoint” file called:

-  ``pickup.00000nIter``

is written out. This file represents the state of the model in a
condensed form and is used for restarting the integration (at the specific
iteration number). Some additional parameterizations and packages also produce
separate pickup files, e.g.,

-  ``pickup_cd.00000nIter`` if the C-D scheme is used (see
   :ref:`C_D Scheme <C-D_scheme>`)

-  ``pickup_seaice.00000nIter`` if the seaice package is turned on (see
   :ref:`sub_phys_pkg_seaice`)

-  ``pickup_ptracers.00000nIter`` if passive tracers are included in the
   simulation (see :ref:`sub_phys_pkg_ptracers`)

Rolling checkpoint files are
the same as the pickup files but are named differently. Their name
contain the chain ``ckptA`` or ``ckptB`` instead of ``00000nIter``. They
can be used to restart the model but are overwritten every other time
they are output to save disk space during long integrations.

NetCDF output files
~~~~~~~~~~~~~~~~~~~

:filelink:`pkg/mnc` is a set of routines written to read, write, and
append `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ files.
Unlike the :filelink:`pkg/mdsio` output, the :filelink:`pkg/mnc`–generated
output is usually placed within a subdirectory with a name such as
``mnc_output_`` (by default,
`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ tries to append,
rather than overwrite, existing files,
so a unique output directory is helpful for each separate run).

The :filelink:`pkg/mnc` output files are all in the “self-describing”
`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ format and
can thus be browsed and/or plotted using tools such as:

-  `ncdump <https://www.unidata.ucar.edu/software/netcdf/netcdf-4/newdocs/netcdf/ncdump.html>`_
   is a utility which is typically included with every
   `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
   install, and converts the
   `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ binaries
   into formatted ASCII text files.

-  `ncview <http://meteora.ucsd.edu/~pierce/ncview_home_page.html>`_
   is a very convenient and quick way to plot
   `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
   data and it runs on most platforms.
   `Panoply <https://www.giss.nasa.gov/tools/panoply/>`_ is a similar
   alternative.

-  `MATLAB <https://www.mathworks.com/products/matlab.html>`_,
   `GrADS <http://cola.gmu.edu/grads/>`_,
   `IDL <http://www.harrisgeospatial.com/SoftwareTechnology/IDL.aspx>`_ and
   other common post-processing environments provide
   built-in `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ interfaces.

Looking at the output
---------------------

MATLAB
~~~~~~

Raw binary output
^^^^^^^^^^^^^^^^^

The repository includes a few
`MATLAB <https://www.mathworks.com/products/matlab.html>`_ utilities to read
binary output files written in the :filelink:`/pkg/mdsio` format. The
`MATLAB <https://www.mathworks.com/products/matlab.html>`_ scripts are located
in the directory :filelink:`utils/matlab` under the root tree. The script
:filelink:`utils/matlab/rdmds.m` reads the data. Look at the comments inside
the script to see how to use it.

Some examples of reading and visualizing some output in
`MATLAB <https://www.mathworks.com/products/matlab.html>`_:

::

    % matlab
    >> H=rdmds('Depth');
    >> contourf(H');colorbar;
    >> title('Depth of fluid as used by model');

    >> eta=rdmds('Eta',10);
    >> imagesc(eta');axis ij;colorbar;
    >> title('Surface height at iter=10');

    >> [eta,iters,M]=rdmds('Eta',NaN); % this will read all dumped iterations
    >> % iter numbers put in variable 'iters'; 'M' is a character string w/metadata
    >> for n=1:length(iters); imagesc(eta(:,:,n)');axis ij;colorbar;pause(.5);end

Typing ``help rdmds`` in
`MATLAB <https://www.mathworks.com/products/matlab.html>`_ will pull up further
information on how to use the :filelink:`rdmds <utils/matlab/rdmds.m>` utility.

NetCDF output
^^^^^^^^^^^^^

Similar scripts for `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
output (e.g., :filelink:`utils/matlab/rdmnc.m`) are available and they
are described in :numref:`pkg_mnc`.

.. _sec_python:

Python
~~~~~~

Install the MITgcmutils python package following the instructions in
:numref:`MITgcmutils`.

Raw binary output
^^^^^^^^^^^^^^^^^

The following example shows how to load in some data:

::

    # python
    from MITgcmutils import mds

    Eta = mds.rdmds('Eta', itrs=10)

For more information about this function and its options,
see the API docs, :meth:`MITgcmutils.mds.rdmds`.

NetCDF output
^^^^^^^^^^^^^

The `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ output
is currently produced with one file per processor. This means the individual
tiles need to be stitched together to create a single
`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ file that spans the
model domain. The script :filelink:`utils/python/MITgcmutils/scripts/gluemncbig`
can do this efficiently from the command line.  If you have installed the
MITgcmutils package, a copy of gluemncbig should be on your path.  For usage
information, see :numref:`gluemncbig`.

The following example shows how to use the
`xarray python package <http://xarray.pydata.org/>`_ to read
the resulting `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ file
into `Python <https://www.python.org/>`_:

::

  # python
  import xarray as xr

  Eta = xr.open_dataset('Eta.nc')

Bash scripts
~~~~~~~~~~~~

The repository includes utilities for handling model input and output. You can 
add these command line scripts to the system's search path by modifying the
unix `PATH <https://www.digitalocean.com/community/tutorials/how-to-view-and-update-the-linux-path-environment-variable>`_
variable. To permanently access MITgcm bash utilities, put this line in 
your shell configuration file e.g. ``.bashrc`` or ``.zshrc``:

::

    export PATH=$PATH:/path/to/your/MITgcm/utils/scripts

NetCDF output
^^^^^^^^^^^^^

`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ output is produced 
with one file per processor. This means unique tiles need to be stitched 
together to create a single 
`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ file that spans the
model domain. The script :filelink:`utils/scripts/gluemnc` can do this from the 
command line. For usage information and dependencies, see :numref:`gluemnc`.

.. _customize_compilation:

Customizing the Model Configuration - Code Parameters and Compilation Options
=============================================================================

Model Array Dimensions
----------------------     

MITgcm’s array dimensions need to be configured for each unique model domain.
The size of each tile (in dimensions :math:`x`, :math:`y`, and vertical
coordinate :math:`r`) the “overlap” region of each tile (in :math:`x` and
:math:`y`), the number of tiles in the :math:`x` and :math:`y` dimensions,
and the number of processes (using
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_)
in the :math:`x` and :math:`y` dimensions all need to be specified in
:filelink:`SIZE.h <model/inc/SIZE.h>`. From these parameters, global
domain-size variables :varlink:`Nx`, :varlink:`Ny` are computed by the model.
See a more technical discussion of :filelink:`SIZE.h <model/inc/SIZE.h>`
parameters in in :numref:`specify_decomp`, and a detailed explanation of an
example :filelink:`SIZE.h <model/inc/SIZE.h>` setup in tutorial
:ref:`Baroclinic Ocean Gyre <baroc_code_size>`.

+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Default :filelink:`SIZE.h <model/inc/SIZE.h>`    | Description                                                                                             |
+========================================+==================================================+=========================================================================================================+
| :varlink:`sNx`                         | 30                                               | number of points in :math:`x` dimension in a single tile                                                |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`sNy`                         | 15                                               | number of points in :math:`y` dimension in a single tile                                                |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`Nr`                          | 4                                                | number of points in :math:`r` dimension                                                                 |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`OLx`                         | 2                                                | number of “overlap” points in :math:`x` dimension for a tile                                            |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`OLy`                         | 2                                                | number of “overlap” points in :math:`y` dimension for a tile                                            |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nSx`                         | 2                                                | number of tile per process in :math:`x` dimension                                                       |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nSy`                         | 4                                                | number of tile per process in :math:`y` dimension                                                       |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nPx`                         | 1                                                | number of processes in :math:`x` dimension                                                              |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nPy`                         | 1                                                | number of processes in :math:`y` dimension                                                              |
+----------------------------------------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Note the repository version of :filelink:`SIZE.h <model/inc/SIZE.h>` includes
several lines of text at the top that will halt compilation with errors. Thus,
to use MITgcm you will need to copy :filelink:`SIZE.h <model/inc/SIZE.h>` to a
code modification directory and make edits, including deleting or commenting
out the offending lines of text.

C Preprocessor Options
----------------------

The CPP flags relative to the “numerical model” part of the code are
defined and set in the file :filelink:`CPP_OPTIONS.h <model/inc/CPP_OPTIONS.h>`
in the directory :filelink:`model/inc/`. In the parameter tables in
:numref:`customize_model` we have noted CPP options **that need to be changed
from the default** to enable specific runtime parameter to be used properly.
Also note many of the options below are for less-common situations or are
somewhat obscure, so newer users of the MITgcm are encouraged to jump to
:numref:`customize_model` where more basic runtime parameters are discussed.

.. tabularcolumns:: |\Y{.475}|\Y{.1}|\Y{.45}|

.. table::
   :class: longtable

   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | CPP Flag Name                                 | Default | Description                                                                                                          |
   +===============================================+=========+======================================================================================================================+
   | :varlink:`SHORTWAVE_HEATING`                  | #undef  | provide separate shortwave heating file, allowing shortwave to penetrate below surface layer                         |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_GEOTHERMAL_FLUX`              | #undef  | include code for applying geothermal heat flux at the bottom of the ocean                                            |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_FRICTION_HEATING`             | #undef  | include code to allow heating due to friction (and momentum dissipation)                                             |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_ADDFLUID`                     | #undef  | allow mass source or sink of fluid in the interior (3D generalization of oceanic real-fresh water flux)              |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ATMOSPHERIC_LOADING`                | #define | include code for atmospheric pressure-loading (and seaice-loading) on ocean surface                                  |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_BALANCE_FLUXES`               | #undef  | include balancing surface forcing fluxes code                                                                        |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_BALANCE_RELAX`                | #undef  | include balancing surface forcing relaxation code                                                                    |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`CHECK_SALINITY_FOR_NEGATIVE_VALUES` | #undef  | include code checking for negative salinity                                                                          |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`EXCLUDE_FFIELDS_LOAD`               | #undef  | exclude external forcing-fields load; code allows reading and simple linear time interpolation of oceanic            |
   |                                               |         | forcing fields, if no specific pkg (e.g., :filelink:`pkg/exf`) is used to compute them                               |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`INCLUDE_PHIHYD_CALCULATION_CODE`    | #define | include code to calculate :math:`\phi_{\rm hyd}`                                                                     |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`INCLUDE_CONVECT_CALL`               | #define | include code for convective adjustment mixing algorithm                                                              |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`INCLUDE_CALC_DIFFUSIVITY_CALL`      | #define | include codes that calculates (tracer) diffusivities and viscosities                                                 |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_3D_DIFFKR`                    | #undef  | allow full 3D specification of vertical diffusivity                                                                  |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_BL79_LAT_VARY`                | #undef  | allow latitudinally varying Bryan and Lewis 1979 :cite:`bryan:79` vertical diffusivity                               |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`EXCLUDE_PCELL_MIX_CODE`             | #undef  | exclude code for partial-cell effect (physical or enhanced) in vertical mixing; this allows accounting               |
   |                                               |         | for partial-cell in vertical viscosity and diffusion, either from grid-spacing reduction effect or as                |
   |                                               |         | artificially enhanced mixing near surface & bottom for too thin grid-cell                                            |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_SMAG_3D_DIFFUSIVITY`          | #undef  | include code for isotropic 3-D Smagorinsky diffusivity for tracers (viscosity scaled by constant Prandtl number)     |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_SOLVE4_PS_AND_DRAG`           | #undef  | include code for combined surface pressure and drag implicit solver                                                  |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`INCLUDE_IMPLVERTADV_CODE`           | #define | include code for implicit vertical advection                                                                         |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_ADAMSBASHFORTH_3`             | #undef  | include code for Adams-Bashforth 3rd-order                                                                           |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_QHYD_STAGGER_TS`              | #undef  | include code for quasi-hydrostatic stagger time-step Adams-Bashforth code                                            |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`EXACT_CONSERV`                      | #define | include code for  “exact conservation" of fluid in free-surface formulation                                          |
   |                                               |         | (recompute divergence after pressure solver)                                                                         |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`NONLIN_FRSURF`                      | #undef  | allow the use of non-linear free-surface formulation; implies that grid-cell thickness (hFactors) varies with time   |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_NONHYDROSTATIC`               | #undef  | include non-hydrostatic and 3D pressure solver codes                                                                 |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_EDDYPSI`                      | #undef  | include GM-like eddy stress in momentum code (untested, not recommended)                                             |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_CG2D_NSA`                     | #undef  | use non-self-adjoint (NSA) conjugate-gradient solver                                                                 |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`ALLOW_SRCG`                         | #define | include code for single reduction conjugate gradient solver                                                          |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`SOLVE_DIAGONAL_LOWMEMORY`           | #undef  | low memory footprint (not suitable for AD) choice for implicit solver routines solve_*diagonal.F                     |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`SOLVE_DIAGONAL_KINNER`              | #undef  | choice for implicit solver routines solve_*diagonal.F suitable for AD                                                |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

.. _default_pkg_list:

By default, MITgcm includes several core packages, i.e., these packages are
enabled during :filelink:`genmake2 <tools/genmake2>` execution if a file
``packages.conf`` is not found. See :numref:`using_packages` for more
information about ``packages.conf``, and see :filelink:`pkg/pkg_groups` for
more information about default packages and package groups.
These default packages are as follows:

- :filelink:`pkg/mom_common`
- :filelink:`pkg/mom_fluxform`
- :filelink:`pkg/mom_vecinv`
- :filelink:`pkg/generic_advdiff`
- :filelink:`pkg/debug`
- :filelink:`pkg/mdsio`
- :filelink:`pkg/rw`
- :filelink:`pkg/monitor`

Additional CPP options that affect the model core code are set in files
``${PKG}_OPTIONS.h`` located in these packages' directories. Similarly,
optional (non-default) packages also include package-specific CPP options that
must be set in files ``${PKG}_OPTIONS.h``.

.. _cpp_eeoptions_doc:

Preprocessor Execution Environment Options
------------------------------------------

**Most MITgcm users can skip this section**; many of these flags were
intended for very specific platform environments, and not meant to be changed
for more general environments (an exception being if you are using a coupled
setup, see below).

The file :filelink:`CPP_EEOPTIONS.h <eesupp/inc/CPP_EEOPTIONS.h>` in the
directory :filelink:`eesupp/inc/` contains a number of CPP flags related to
the execution environment where the model will run. Below we describe the
subset of user-editable CPP flags:

.. tabularcolumns:: |\Y{.475}|\Y{.1}|\Y{.45}|

.. table::
   :class: longtable
   :name: cpp_eeopt_flags

   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | CPP Flag Name                                 | Default | Description                                                                                                          |
   +===============================================+=========+======================================================================================================================+
   | :varlink:`GLOBAL_SUM_ORDER_TILES`             | #define | always cumulate tile local-sum in the same order by applying MPI allreduce to array of tiles                         |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`CG2D_SINGLECPU_SUM`                 | #undef  | alternative way of doing global sum on a single CPU  to eliminate tiling-dependent roundoff errors                   |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`SINGLE_DISK_IO`                     | #undef  | to write STDOUT, STDERR and scratch files from process 0 only                                                        |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`USE_FORTRAN_SCRATCH_FILES`          | #undef  | flag to turn on old default of opening scratch files with the STATUS='SCRATCH' option                                |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`COMPONENT_MODULE`                   | #undef  | control use of communication with other components, i.e., sets component to work with a coupler interface            |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`DISCONNECTED_TILES`                 | #undef  | use disconnected tiles (no exchange between tiles, just fill-in edges assuming locally periodic subdomain)           |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
   | :varlink:`REAL4_IS_SLOW`                      | #define | if undefined, force ``_RS`` variables to be declared as real*4                                                       |
   +-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+

The default setting of ``#define`` :varlink:`GLOBAL_SUM_ORDER_TILES` in
:filelink:`CPP_EEOPTIONS.h <eesupp/inc/CPP_EEOPTIONS.h>` provides a way to
achieve numerically reproducible global sums for a given tile domain
decomposition. As implemented however, this
approach will increase the volume of network traffic in a way that scales
with the total number of tiles.
Profiling has shown that letting the code fall through to a baseline
approach that simply uses
`MPI_Allreduce() <https://www.open-mpi.org/doc/v3.0/man3/MPI_Allreduce.3.php>`_
can provide significantly improved performance for certain simulations [#]_.
The fall-though approach is activated by ``#undef``
:varlink:`GLOBAL_SUM_ORDER_TILES`.

In order to get bit-wise reproducible results between different tile domain
decompositions (e.g., single tile on single processor versus multiple tiles
either on single or multiple processors), one can choose to ``#define``
option :varlink:`CG2D_SINGLECPU_SUM` to use the **MUCH** slower
:filelink:`global_sum_singlecpu.F <eesupp/src/global_sum_singlecpu.F>`
for the key part of MITgcm algorithm :filelink:`CG2D <model/src/cg2d.F>`
that relies on global sum.
This option is not only much slower but also requires a large volume of
communications so it is practically unusable for a large set-up;
furthermore, it does not address reproducibility when global sum is used
outside :filelink:`CG2D <model/src/cg2d.F>`, e.g., in non-hydrostatic simulations.

In a default multi-processor configuration, each process opens and reads its
own set of namelist files and open and writes its own standard output. This can
be slow or even problematic for very large processor counts. Defining the
CPP-flag :varlink:`SINGLE_DISK_IO` suppresses this behavior and lets only the
master process (process 0) read namelist files and write a standard output
stream. This may seem advantageous, because it reduces the amount of seemingly
redundant output, but use this option with caution and only when absolutely
confident that the setup is working since any message (error/warning/print)
from any processor :math:`\ne 0` will be lost.

The way the namelist files are read requires temporary (scratch) files in the
initialization phase. By default, the MITgcm does not use intrinsic Fortran
scratch files (``STATUS='scratch'``) because they can lead to conflicts in
multi-processor simulations on some HPC-platforms, when the processors do not
open scratch files with reserved names. However, the implemented default scheme
for the scratch files can be slow for large processor counts. If this is a
problem in a given configuration, defining the CPP-flag
:varlink:`USE_FORTRAN_SCRATCH_FILES` may help by making the code use the
intrinsic Fortran scratch files.

The CPP-flag :varlink:`COMPONENT_MODULE` needs to be set to ``#define`` only for
builds in which the MITgcm executable (for either an oceanic or atmospheric
simulation) is configured to communicate with a coupler.
This coupler can be a specially configured build of MITgcm itself;
see, for example, verification experiment `cpl_aim+ocn
<https://github.com/MITgcm/MITgcm/tree/master/verification/cpl_aim+ocn>`_.

The CPP-flag :varlink:`DISCONNECTED_TILES` should not be ``#define``
unless one wants to run simultaneously several small, single-tile ensemble
members from a single process, as each tile will be disconnected from the others
and considered locally as a doubly periodic patch.

..
    should reference the to-be-written section about _RS, _RL within chapter 6

MITgcm ``_RS`` variables are forced to be declared as
``real*4`` if CPP-flag :varlink:`REAL4_IS_SLOW` to is set to ``#undef``
in :filelink:`CPP_EEOPTIONS.h <eesupp/inc/CPP_EEOPTIONS.h>`
(``_RS`` is a macro used in declaring real variables that, in principle,
do not require double precision). However, this option is not recommended
except for computational benchmarking or for testing the trade-off between memory
footprint and model precision.  And even for these specialized tests, there is no need
to edit :filelink:`CPP_EEOPTIONS.h <eesupp/inc/CPP_EEOPTIONS.h>`
since this feature can be activated using the :filelink:`genmake2 <tools/genmake2>`
command line option ``-use_r4``,  as done in some regression tests
(see testing `results <https://mitgcm.org/testing-summary>`_
page tests with optfile suffix ``.use_r4``).

.. [#] One example is the llc_540 case located at
   https://github.com/MITgcm-contrib/llc_hires/tree/master/llc_540. This case
   was run on the Pleiades computer for 20 simulated days using 767 and 2919
   MPI ranks.  At 767 ranks, the fall-through approach provided a throughput of
   to 799.0 simulated days per calendar day (dd/d) while the default approach
   gave 781.0.  The profiler showed the speedup was directly attributable to
   spending less time in MPI_Allreduce. The volume of memory traffic associated
   with MPI_Allreduce dropped by 3 orders (22.456T -> 32.596G).  At 2819 MPI
   ranks the fall-through approach gave a throughput of 1300 dd/d while the
   default approach gave 800.0 dd/d. Put another way, this case did not scale
   at all from 767p to 2819p unless the fall-though approach was utilized. The
   profiler showed the speedup was directly attributable to spending less time
   in MPI_Allreduce. The volume of memory traffic associated with MPI_Allreduce
   dropped by 3 orders (303.70T ->121.08G ).

.. _customize_model:

Customizing the Model Configuration - Runtime Parameters
========================================================

When you are ready to run the model in the configuration you want, the
most straightforward approach is to use and adapt the setup of a tutorial or
verification experiment (described in :numref:`chap_modelExamples`) that is the
closest to your configuration. Then, the amount of setup will be minimized.
In this section, we document the complete list of MITgcm model namelist runtime
parameters set in file ``data``, which needs to be located in the
directory where you will run the model.
Model parameters are defined and
declared in the file :filelink:`PARAMS.h <model/inc/PARAMS.h>` and their
default values are generally set in the routine
:filelink:`set_defaults.F <model/src/set_defaults.F>`, otherwise
when initialized in the routine :filelink:`ini_parms.F <model/src/ini_parms.F>`.
:numref:`eedata_parms` documents the “execution environment” namelist parameters
in file ``eedata``, which must also reside in the current run directory.
Note that runtime parameters used by (non-default) MITgcm packages are not
documented here but rather in :numref:`packagesI`
and :numref:`outp_pack`, and prescribed in package-specific ``data.${pkg}``
namelist files which are read in via package-specific
``${pkg}_readparms.F`` where ``${pkg}`` is the package name
(see :numref:`using_packages`).

In what follows, model parameters are grouped into categories related to
configuration/computational domain, algorithmic parameters, equations solved
in the model, parameters related to model forcing, and simulation controls.
The tables below specify the namelist parameter name, the namelist parameter
group in ``data`` (and ``eedata`` in :numref:`eedata_parms`), the default
value, and a short description of its function. Runtime parameters that
require **non-default** CPP options to be set prior to compilation
(see :numref:`customize_compilation`) for proper use are noted.

Parameters: Configuration, Computational Domain, Geometry, and Time-Discretization
----------------------------------------------------------------------------------

.. _model_config_parms:

Model Configuration
~~~~~~~~~~~~~~~~~~~

:varlink:`buoyancyRelation` is set to ``OCEANIC`` by default, which employes a
:math:`z`-coordinate vertical axis. To simulate an ocean using pressure
coordinates in the vertical, set it to ``OCEANICP``. For atmospheric
simulations, :varlink:`buoyancyRelation` needs to be set to ``ATMOSPHERIC``,
which also uses pressure as the vertical coordinate.
The default model configuration is hydrostatic; to run a non-hydrostatic
simulation, set the logical variable :varlink:`nonHydrostatic` to ``.TRUE.``.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`buoyancyRelation`            | PARM01    | OCEANIC                                          | buoyancy relation (``OCEANIC``, ``OCEANICP``, or ``ATMOSPHERIC``)                                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`quasiHydrostatic`            | PARM01    | FALSE                                            | quasi-hydrostatic formulation on/off flag                                                               |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`rhoRefFile`                  | PARM01    | :kbd:`' '`                                       | filename for reference density profile (kg/m\ :sup:`3`); activates anelastic form of model              |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nonHydrostatic`              | PARM01    | FALSE                                            | non-hydrostatic formulation on/off flag; requires #define :varlink:`ALLOW_NONHYDROSTATIC`               |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Grid
~~~~     

Four different grids are available: Cartesian, spherical polar, cylindrical, and
curvilinear (which includes the cubed sphere). The grid is set
through the logical variables :varlink:`usingCartesianGrid`,
:varlink:`usingSphericalPolarGrid`, :varlink:`usingCylindricalGrid`,
and :varlink:`usingCurvilinearGrid`. Note that the cylindrical grid is designed
for modeling a rotating tank, so that :math:`x` is the azimuthual direction,
:math:`y` is the radial direction, and :math:`r` is vertical coordinate
(see tutorial :ref:`rotating tank <sec_eg_tank>`).

The variable :varlink:`xgOrigin` sets the position of the western
most gridcell face in the :math:`x` dimension (Cartesian, meters; spherical and
cyclindrical, degrees). For a Cartesian or spherical grid, the southern boundary
is defined through the variable :varlink:`ygOrigin` which corresponds to the
latitude of the southern most gridcell face (Cartesian, meters; spherical,
degrees). For a cyclindrical grid, a positive :varlink:`ygOrigin` (m) adds an
inner cylindrical boundary at the center of the tank. The resolution
along the :math:`x` and :math:`y` directions is controlled by the 1-D arrays
:varlink:`delX` (meters for a Cartesian grid, degrees otherwise)
and :varlink:`delY` (meters for Cartesian and cyclindrical grids, degrees
spherical). On a spherical polar grid, you might decide to set the variable
:varlink:`cosPower` which is set to 0 by default and which represents :math:`n`
in :math:`(\cos\varphi)^n`, the power of cosine of latitude to
multiply horizontal viscosity and tracer diffusivity.
The vertical grid spacing is set through the 1-D array
:varlink:`delR` (:math:`z`-coordinates: in meters; :math:`p`-coordinates,
in Pa). Using a curvilinear grid requires complete specification of all
horizontal MITgcm grid variables, either through a default filename (link to
new doc section) or as specified by :varlink:`horizGridFile`.

The variable :varlink:`seaLev_Z` represents the standard
position of sea level, in meters. This is typically set to 0 m
for the ocean (default value). If instead pressure is used as the vertical
coordinate, the pressure at the top (of the atmosphere or ocean) is set through
:varlink:`top_Pres`, typically 0 Pa. As such, these variables are analogous to
:varlink:`xgOrigin` and :varlink:`ygOrigin` to define the vertical grid axis.
But they also are used for a second purpose: in a :math:`z`-coordinate setup,
:varlink:`top_Pres` sets a reference top pressure (required in a non-linear
equation of state computation, for example); note that 1 bar
(i.e., typical Earth atmospheric sea-level pressure) is added already, so the
default is 0 Pa. Similarly, for a :math:`p`-coordinate setup,
:varlink:`seaLev_Z` is used to set a reference geopotential (after gravity
scaling) at the top of the ocean or bottom of the atmosphere.

.. tabularcolumns:: |\Y{.275}|\Y{.1}|\Y{.125}|\Y{.525}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`usingCartesianGrid`          | PARM04    | TRUE                                             | use Cartesian grid/coordinates on/off flag                                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`usingSphericalPolarGrid`     | PARM04    | FALSE                                            | use spherical grid/coordinates on/off flag                                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`usingCylindricalGrid`        | PARM04    | FALSE                                            | use cylindrical grid/coordinates on/off flag                                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`usingCurvilinearGrid`        | PARM04    | FALSE                                            | use curvilinear grid/coordinates on/off flag                                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`xgOrigin`                    | PARM04    | 0.0                                              | west edge :math:`x`-axis origin (Cartesian: m; spherical and cyclindrical: degrees longitude)           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ygOrigin`                    | PARM04    | 0.0                                              | South edge :math:`y`-axis origin (Cartesian and cyclindrical: m; spherical: degrees latitude)           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`dxSpacing`                   | PARM04    | unset                                            | :math:`x`-axis uniform grid spacing, separation between cell faces                                      |
   |                                        |           |                                                  | (Cartesian: m; spherical and cyclindrical: degrees)                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`delX`                        | PARM04    | :varlink:`dxSpacing`                             | 1D array of :math:`x`-axis grid spacing, separation between cell faces                                  |
   |                                        |           |                                                  | (Cartesian: m; spherical and cyclindrical: degrees)                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`delXFile`                    | PARM04    | :kbd:`' '`                                       | filename containing 1D array of :math:`x`-axis grid spacing                                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`dySpacing`                   | PARM04    | unset                                            | :math:`y`-axis uniform grid spacing, separation between cell faces                                      |
   |                                        |           |                                                  | (Cartesian and cyclindrical: m; spherical: degrees)                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`delY`                        | PARM04    | :varlink:`dySpacing`                             | 1D array of :math:`x`-axis grid spacing, separation between cell faces                                  |
   |                                        |           |                                                  | (Cartesian and cyclindrical: m; spherical: degrees)                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`delYFile`                    | PARM04    | :kbd:`' '`                                       | filename containing 1D array of :math:`y`-axis grid spacing                                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`cosPower`                    | PARM01    | 0.0                                              | power law :math:`n` in :math:`(\cos\varphi)^n` factor for horizontal (harmonic or biharmonic)           |
   |                                        |           |                                                  | viscosity and tracer diffusivity (spherical polar)                                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`delR`                        | PARM04    | computed using delRc                             | vertical grid spacing 1D array ([:math:`r`] unit)                                                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`delRc`                       | PARM04    | computed using delR                              | vertical cell center spacing 1D array ([:math:`r`] unit)                                                |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`delRFile`                    | PARM04    | :kbd:`' '`                                       | filename for vertical grid spacing 1D array ([:math:`r`] unit)                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`delRcFile`                   | PARM04    | :kbd:`' '`                                       | filename for vertical cell center spacing 1D array ([:math:`r`] unit)                                   |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`rSphere`                     | PARM04    | 6.37E+06                                         | radius of sphere for spherical polar or curvilinear grid (m)                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`seaLev_Z`                    | PARM04    | 0.0                                              | reference height of sea level (m)                                                                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`top_Pres`                    | PARM04    | 0.0                                              | top pressure (:math:`p`-coordinates) or top reference pressure (:math:`z`-coordinates) (Pa)             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`selectFindRoSurf`            | PARM01    | 0                                                | select method to determine surface reference pressure from orography (atmos.-only)                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`horizGridFile`               | PARM04    | :kbd:`' '`                                       | filename containing full set of horizontal grid variables (curvilinear)                                 |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`radius_fromHorizGrid`        | PARM04    | :varlink:`rSphere`                               | radius of sphere used in input curvilinear horizontal grid file (m)                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`phiEuler`                    | PARM04    | 0.0                                              | Euler angle, rotation about original :math:`z`-axis (spherical polar)  (degrees)                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`thetaEuler`                  | PARM04    | 0.0                                              | Euler angle, rotation about new :math:`x`-axis (spherical polar)  (degrees)                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`psiEuler`                    | PARM04    | 0.0                                              | Euler angle, rotation about new :math:`z`-axis (spherical polar)  (degrees)                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _parms_topo:

Topography - Full and Partial Cells
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the ocean, the topography is read from a file that contains a
2-D(:math:`x,y`) map of bathymetry, in meters for :math:`z`-coordinates, in
pascals for :math:`p`-coordinates. The bathymetry is specified by entering the
vertical position of the ocean floor relative to the surface, so by convention
in :math:`z`-coordinates bathymetry is specified as negative numbers (“depth”
is defined as positive-definite) whereas in :math:`p`-coordinates
bathymetry data is positive. The file name is represented by the variable
:varlink:`bathyFile`. See our introductory tutorial setup :numref:`sec_eg_baro`
for additional details on the file format. Note no changes are required in the
model source code to represent enclosed, periodic, or double periodic
domains: periodicity is assumed by default and is suppressed by
setting the depths to 0 m for the cells at the limits of the
computational domain.

To use the partial cell capability, the variable :varlink:`hFacMin` needs
to be set to a value between 0.0 and 1.0 (it is set to 1.0 by default)
corresponding to the minimum fractional size of a gridcell. For
example, if a gridcell is 500 m thick and :varlink:`hFacMin` is set to
0.1, the minimum thickness for a “thin-cell” for this specific gridcell is 50 m.
Thus, if the specified bathymetry depth were to fall exactly
in the middle of this 500m thick gridcell, the initial model variable
:varlink:`hFacC`\ (:math:`x,y,r`) would be set to 0.5.
If the specified bathymetry depth fell within the top 50m of this gridcell
(i.e., less than :varlink:`hFacMin`), the model bathymetry would snap to the
nearest legal value (i.e., initial :varlink:`hFacC`\ (:math:`x,y,r`) would be
equal to 0.0 or 0.1 depending if the depth was within 0-25 m or 25-50 m,
respectively). Also note while specified bathymetry bottom depths (or
pressures) need not coincide with the model's levels as deduced from
:varlink:`delR`, any depth falling below the model's defined vertical axis is
truncated.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`bathyFile`                   | PARM05    | :kbd:`' '`                                       | filename for 2D bathymetry (ocean) (:math:`z`-coor.: m, negative; :math:`p`-coor.: Pa, positive)        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`topoFile`                    | PARM05    | :kbd:`' '`                                       | filename for 2D surface topography (atmosphere) (m)                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`addWwallFile`                | PARM05    | :kbd:`' '`                                       | filename for 2D western cell-edge “thin-wall”                                                           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`addSwallFile`                | PARM05    | :kbd:`' '`                                       | filename for 2D southern cell-edge “thin-wall”                                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`hFacMin`                     | PARM01    | 1.0E+00                                          | minimum fraction size of a cell                                                                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`hFacMinDr`                   | PARM01    | 1.0E+00                                          | minimum dimensional size of a cell ([:math:`r`] unit)                                                   |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`hFacInf`                     | PARM01    | 2.0E-01                                          | lower threshold fraction for surface cell;                                                              |
   |                                        |           |                                                  | for non-linear free surface only, see parameter :ref:`nonlinFreeSurf <free_surface_parms>`              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`hFacSup`                     | PARM01    | 2.0E+00                                          | upper threshold fraction for surface cell;                                                              |
   |                                        |           |                                                  | for non-linear free surface, only see parameter :ref:`nonlinFreeSurf <free_surface_parms>`              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useMin4hFacEdges`            | PARM04    | FALSE                                            | set :varlink:`hFacW`, :varlink:`hFacS` as minimum of adjacent :varlink:`hFacC` on/off flag              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`pCellMix_select`             | PARM04    | 0                                                | option/factor to enhance mixing at the surface or bottom (0- 99)                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`pCellMix_maxFac`             | PARM04    | 1.0E+04                                          | maximum enhanced mixing factor for too thin partial-cell (non-dim.)                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`pCellMix_delR`               | PARM04    | 0.0                                              | thickness criteria for too thin partial-cell ([:math:`r`] unit)                                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Physical Constants
~~~~~~~~~~~~~~~~~~

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`rhoConst`                    | PARM01    | :varlink:`rhoNil`                                | vertically constant reference density (Boussinesq) (kg/m\ :sup:`3`)                                     |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`gravity`                     | PARM01    | 9.81E+00                                         | gravitational acceleration (m/s\ :sup:`2`)                                                              |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`gravityFile`                 | PARM01    | :kbd:`' '`                                       | filename for 1D gravity vertical profile (m/s\ :sup:`2`)                                                |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`gBaro`                       | PARM01    | :varlink:`gravity`                               | gravity constant in barotropic equation (m/s\ :sup:`2`)                                                 |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Rotation
~~~~~~~~

.. tabularcolumns:: |\Y{.2}|\Y{.1}|\Y{.2}|\Y{.525}|

For a Cartesian or cylindical grid, the Coriolis parameter :math:`f` is set
through the variables :varlink:`f0` (in s\ :sup:`--1`) and :varlink:`beta`
(:math:`\frac{\partial f}{ \partial y}`; in m\ :sup:`--1`\ s\ :sup:`--1`),
which corresponds to a Coriolis parameter  :math:`f = f_o + \beta y`
(the so-called :math:`\beta`\ -plane).

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`rotationPeriod`              | PARM01    | 8.6164E+04                                       | rotation period (s)                                                                                     |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`omega`                       | PARM01    | :math:`2\pi/`\ :varlink:`rotationPeriod`         | angular velocity (rad/s)                                                                                |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`selectCoriMap`               | PARM01    | depends on grid                                  | Coriolis map options                                                                                    |
|                                        |           | (Cartesian and cylindrical=1,                    |                                                                                                         |
|                                        |           | spherical and curvilinear=2)                     | - 0: f-plane                                                                                            |
|                                        |           |                                                  | - 1: beta-plane                                                                                         |
|                                        |           |                                                  | - 2: spherical Coriolis (:math:`=2\Omega\sin{\varphi}`)                                                 |
|                                        |           |                                                  | - 3: read 2D field from file                                                                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`f0`                          | PARM01    | 1.0E-04                                          | reference Coriolis parameter (Cartesian or cylindrical grid) (1/s)                                      |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`beta`                        | PARM01    | 1.0E-11                                          | :math:`\beta` (Cartesian or cylindrical grid) (m\ :sup:`--1`\ s\ :sup:`--1`)                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`fPrime`                      | PARM01    | 0.0                                              | :math:`2 \Omega \cos{\phi}` parameter (Cartesian or cylindical grid) (1/s); i.e., for                   |
|                                        |           |                                                  | :math:`\cos{\varphi}` Coriolis terms from horizontal component of rotation vector                       |
|                                        |           |                                                  | (also sometimes referred to as reciprocal Coriolis parm.)                                               |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _free_surface_parms:

Free Surface
~~~~~~~~~~~~

The logical variables :varlink:`rigidLid` and :varlink:`implicitFreeSurface`
specify your choice for ocean upper boundary (or lower boundary if using
:math:`p`-coordinates); set one to ``.TRUE.`` and the other to ``.FALSE.``.
These settings affect the calculations of surface pressure (for the ocean) or
surface geopotential (for the atmosphere); see :numref:`parms-main_algorithm`.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.175}|\Y{.525}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`implicitFreeSurface`         | PARM01    | TRUE                                             | implicit free surface on/off flag                                                                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`rigidLid`                    | PARM01    | FALSE                                            | rigid lid on/off flag                                                                                   |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useRealFreshWaterFlux`       | PARM01    | FALSE                                            | use true E-P-R freshwater flux (changes free surface/sea level) on/off flag                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`implicSurfPress`             | PARM01    | 1.0E+00                                          | implicit fraction of the surface pressure gradient (0-1)                                                |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`implicDiv2Dflow`             | PARM01    | 1.0E+00                                          | implicit fraction of the barotropic flow divergence (0-1)                                               |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`implicitNHPress`             | PARM01    | :varlink:`implicSurfPress`                       | implicit fraction of the non-hydrostatic pressure gradient (0-1);                                       |
   |                                        |           |                                                  | for non-hydrostatic only, see parameter :ref:`nonHydrostatic <model_config_parms>`                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`nonlinFreeSurf`              | PARM01    | 0                                                | non-linear free surface options (-1,0,1,2,3; see :numref:`nonlinFreeSurf-flags`);                       |
   |                                        |           |                                                  | requires #define :varlink:`NONLIN_FRSURF`                                                               |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`select_rStar`                | PARM01    | 0                                                | vertical coordinate option                                                                              |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 0: use r                                                                                              |
   |                                        |           |                                                  | - >0: use :math:`r^*`                                                                                   |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | see :numref:`nonlinFreeSurf-flags`; requires #define :varlink:`NONLIN_FRSURF`                           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`selectNHfreeSurf`            | PARM01    | 0                                                | non-hydrostatic free surface formulation option                                                         |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 0: don’t use                                                                                          |
   |                                        |           |                                                  | - >0: use                                                                                               |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | requires non-hydrostatic formulation, see parameter :ref:`nonHydrostatic <model_config_parms>`          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`exactConserv`                | PARM01    | FALSE                                            | exact total volume conservation (recompute divergence after pressure solver) on/off flag                |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Time-Discretization
~~~~~~~~~~~~~~~~~~~

The time steps are set through the real variables :varlink:`deltaTMom` and
:varlink:`deltaTtracer` (in seconds) which represent the time step for the
momentum and tracer equations, respectively (or you can prescribe a single
time step value for all parameters using :varlink:`deltaT`). The model “clock”
is defined by the variable :varlink:`deltaTClock` (in seconds)
which determines the I/O frequencies and is used in tagging output.
Time in the model is thus computed as:

|  model time = :varlink:`baseTime` + iteration number  * :varlink:`deltaTClock`

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
|  :varlink:`deltaT`                     | PARM03    | 0.0                                              | default value used for model time step parameters (s)                                                   |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`deltaTClock`                | PARM03    | :varlink:`deltaT`                                | timestep used for model clock (s): used for I/O frequency and tagging output and checkpoints            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`deltaTmom`                  | PARM03    | :varlink:`deltaT`                                | momentum equation timestep (s)                                                                          |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`deltaTtracer`               | PARM03    | :varlink:`deltaT`                                | tracer equation timestep (s)                                                                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`dTtracerLev`                | PARM03    | :varlink:`deltaTtracer`                          | tracer equation timestep specified at each vertical level (s)                                           |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`deltaTfreesurf`             | PARM03    | :varlink:`deltaTmom`                             | free-surface equation timestep (s)                                                                      |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`baseTime`                    | PARM03    | 0.0                                              | model base time corresponding to iteration 0 (s)                                                        |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _parms-main_algorithm:

Parameters: Main Algorithmic Parameters
---------------------------------------

Pressure Solver
~~~~~~~~~~~~~~~

By default, a hydrostatic
simulation is assumed and a 2-D elliptic equation is used to invert the
pressure field. If using a non-hydrostatic configuration, the pressure field is
inverted through a 3-D elliptic equation (note this capability is not yet
available for the atmosphere). The parameters controlling the behavior of the
elliptic solvers are the variables :varlink:`cg2dMaxIters` and
:varlink:`cg2dTargetResidual` for the 2-D case and :varlink:`cg3dMaxIters` and
:varlink:`cg3dTargetResidual` for the 3-D case.

.. tabularcolumns:: |\Y{.2}|\Y{.1}|\Y{.2}|\Y{.525}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`cg2dMaxIters`                | PARM02    | 150                                              | upper limit on 2D conjugate gradient solver iterations                                                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`cg2dTargetResidual`          | PARM02    | 1.0E-07                                          | 2D conjugate gradient target residual (non-dim. due to RHS normalization )                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`cg2dTargetResWunit`          | PARM02    | -1.0E+00                                         | 2D conjugate gradient target residual (:math:`\dot{r}` units);                                          |
   |                                        |           |                                                  | <0: use RHS normalization, i.e., :varlink:`cg2dTargetResidual` instead                                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`cg2dPreCondFreq`             | PARM02    | 1                                                | frequency (in number of iterations) for updating cg2d pre-conditioner;                                  |
   |                                        |           |                                                  | for non-linear free surface only, see parameter :ref:`nonlinFreeSurf <free_surface_parms>`              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`cg2dUseMinResSol`            | PARM02    | 0 unless flat-bottom, Cartesian                  | - 0: use last-iteration/converged cg2d solution                                                         |
   |                                        |           |                                                  | - 1: use solver minimum-residual solution                                                               |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`cg3dMaxIters`                | PARM02    | 150                                              | upper limit on 3D conjugate gradient solver iterations; requires #define :varlink:`ALLOW_NONHYDROSTATIC`|
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`cg3dTargetResidual`          | PARM02    | 1.0E-07                                          | 3D conjugate gradient target residual (non-dim. due to RHS normalization );                             |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_NONHYDROSTATIC`                                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`cg3dTargetResWunit`          | PARM02    | -1.0E+00                                         | 3D conjugate gradient target residual (:math:`\dot{r}` units);                                          |
   |                                        |           |                                                  | <0: use RHS normalization, i.e., :varlink:`cg3dTargetResidual` instead                                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useSRCGSolver`               | PARM02    | FALSE                                            | use conjugate gradient solver with single reduction (single call of mpi_allreduce)                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`printResidualFreq`           | PARM02    | 1 unless :varlink:`debugLevel` >4                | frequency (in number of iterations) of printing conjugate gradient residual                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`integr_GeoPot`               | PARM01    | 2                                                | select method to integrate geopotential                                                                 |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 1: finite volume                                                                                      |
   |                                        |           |                                                  | - :math:`\neq`\ 1: finite difference                                                                    |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`uniformLin_PhiSurf`          | PARM01    | TRUE                                             | use uniform :math:`b_s` relation for :math:`\phi_s` on/off flag                                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`deepAtmosphere`              | PARM04    | FALSE                                            | don’t make the thin shell/shallow water approximation                                                   |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`nh_Am2`                      | PARM01    | 1.0E+00                                          | non-hydrostatic terms scaling factor; requires #define :varlink:`ALLOW_NONHYDROSTATIC`                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Time-Stepping Algorithm
~~~~~~~~~~~~~~~~~~~~~~~

The Adams-Bashforth stabilizing parameter is set through the
variable :varlink:`abEps` (dimensionless). The stagger baroclinic time
stepping algorithm can be activated by setting the logical variable
:varlink:`staggerTimeStep` to ``.TRUE.``.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`abEps`                       | PARM03    | 1.0E-02                                          | Adams-Bashforth-2 stabilizing weight (non-dim.)                                                         |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`alph_AB`                     | PARM03    | 0.5E+00                                          | Adams-Bashforth-3 primary factor (non-dim.); requires #define :varlink:`ALLOW_ADAMSBASHFORTH_3`         |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`beta_AB`                     | PARM03    | 5/12                                             | Adams-Bashforth-3 secondary factor (non-dim.); requires #define :varlink:`ALLOW_ADAMSBASHFORTH_3`       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`staggerTimeStep`             | PARM01    | FALSE                                            | use staggered time stepping (thermodynamic vs. flow variables) on/off flag                              |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`multiDimAdvection`           | PARM01    | TRUE                                             | use multi-dim. advection algorithm in schemes where non multi-dim. is possible on/off flag              |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`implicitIntGravWave`         | PARM01    | FALSE                                            | treat internal gravity waves implicitly on/off flag; requires #define :varlink:`ALLOW_NONHYDROSTATIC`   |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _parms-eos:

Parameters: Equation of State
-----------------------------

The form of the equation of state is controlled by the model configuration
and :varlink:`eosType`.

For the atmosphere, :varlink:`eosType` must be set to ``IDEALGAS``.

For the ocean, several forms of the equation of state are
available:

- For a linear approximation, set :varlink:`eosType` to ``LINEAR``),
  and you will need to specify the thermal
  and haline expansion coefficients, represented by the variables
  :varlink:`tAlpha` (in K\ :sup:`--1`) and :varlink:`sBeta`
  (in (g/kg)\ :sup:`--1`).
  Because the model equations are written in terms of
  perturbations, a reference thermodynamic state needs to be specified.
  This is done through the 1-D arrays :varlink:`tRef` and :varlink:`sRef`.
  :varlink:`tRef` specifies the reference potential temperature profile (in
  :sup:`o`\ C for the ocean and K for the atmosphere) starting
  from the level k=1. Similarly, :varlink:`sRef` specifies the reference
  salinity profile (in g/kg) for the ocean or the reference specific
  humidity profile (in g/kg) for the atmosphere.

- MITgcm offers several approximations to the full (oceanic) non-linear equation
  of state that can be selected as :varlink:`eosType`:

   ``'POLYNOMIAL'``:
    This approximation is based on the Knudsen formula (see Bryan and Cox 1972
    :cite:`bryan:72`). For this option you need to generate a file of polynomial
    coefficients called ``POLY3.COEFFS``. To do this, use the program
    :filelink:`utils/knudsen2/knudsen2.f` under the model tree (a ``Makefile``
    is available in the same directory; you will need to edit the number and
    the values of the vertical levels in
    :filelink:`knudsen2.f <utils/knudsen2/knudsen2.f>`
    so that they match those of your configuration).

   ``’UNESCO’``:
    The UNESCO equation of state formula (IES80) of Fofonoff and Millard (1983)
    :cite:`fofonoff:83`. This equation of state assumes
    in-situ temperature, which is not a model variable; **its use is
    therefore discouraged**.

   ``’JMD95Z’``:
    A modified UNESCO formula by Jackett and McDougall (1995)
    :cite:`jackett:95`, which uses the model variable
    potential temperature as input. The ’Z’ indicates that this
    equation of state uses a horizontally and temporally constant
    pressure :math:`p_{0}=-g\rho_{0}z`.

   ``’JMD95P’``:
    A modified UNESCO formula by Jackett and McDougall (1995)
    :cite:`jackett:95`, which uses the model variable
    potential temperature as input. The ’P’ indicates that this
    equation of state uses the actual hydrostatic pressure of the last
    time step. Lagging the pressure in this way requires an additional
    pickup file for restarts.

   ``’MDJWF’``:
    A more accurate and less expensive equation of state than UNESCO by
    McDougall et al. (2003) :cite:`mcdougall:03`, also using the model variable
    potential temperature as input. It also requires
    lagging the pressure and therefore an additional pickup file for
    restarts.

   ``’TEOS10’``:
    TEOS-10 is based on a Gibbs function formulation from which all
    thermodynamic properties of seawater (density, enthalpy, entropy sound
    speed, etc.) can be derived in a thermodynamically consistent manner;
    see http://www.teos-10.org. See IOC et al. (2010) :cite:`ioc:10`, McDougall
    and Parker (2011) :cite:`mcdougall:11`, and Roquet et al. (2015)
    :cite:`roquet:15` for implementation details.
    It also requires lagging the pressure and therefore an additional pickup
    file for restarts. Note at this time a full implementation of TEOS10 (i.e.,
    ocean variables of conservative temperature and practical salinity,
    including consideration of surface forcings) has not been implemented;
    also note the original 48-term polynomial term is used, not the newer,
    preferred 75-term polynomial.

   For these non-linear approximations, neither a reference profile of
   temperature or salinity is required, except for a setup where
   :varlink:`implicitIntGravWave` is set to ``.TRUE.`` or
   :varlink:`selectP_inEOS_Zc`\ =1.

Note that for simplicity, salinity is expressed as a ratio in g/kg (thus
effectively unitless) regardless of the choice of equation of state,
despite "Practical Salinity" not precisely equal to salinity expressed as a
dissolved mass fraction. If TEOS-10 is selected, the model variable
:varlink:`salt` can be interpreted as "Absolute Salinity". See Millero (2010)
:cite:`millero:10` and Pawlowicz (2013) :cite:`pawlowicz:13` for detailed
discussion of salinity measurements, and why being expressed
as g/kg is preferred, in the context of the ocean equation of state.

.. tabularcolumns:: |\Y{.2}|\Y{.1}|\Y{.2}|\Y{.525}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`eosType`                     | PARM01    | LINEAR                                           | equation of state form                                                                                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tRef`                        | PARM01    | 20.0 :sup:`o`\ C (ocn) or 300.0 K (atm)          | 1D vertical reference temperature profile (:sup:`o`\ C or K)                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tRefFile`                    | PARM01    | :kbd:`' '`                                       | filename for reference temperature profile (:sup:`o`\ C or K)                                           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`thetaConst`                  | PARM01    | :varlink:`tRef`\ (k=1)                           | vertically constant reference temp. for atmosphere :math:`p^*` coordinates (:sup:`o`\ K);               |
   |                                        |           |                                                  | for ocean, specify instead of :varlink:`tRef` or :varlink:`tRefFile`                                    |
   |                                        |           |                                                  | for vertically constant reference temp. (:sup:`o`\ C )                                                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`sRef`                        | PARM01    | 30.0 (g/kg) (ocn) or 0.0 (atm)                   | 1D vertical reference salinity profile (g/kg)                                                           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`sRefFile`                    | PARM01    | :kbd:`' '`                                       | filename for reference salinity profile (g/kg)                                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`selectP_inEOS_Zc`            | PARM01    | depends on :varlink:`eosType`                    | select which pressure to use in EOS for :math:`z`-coor.                                                 |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | -  0: use :math:`-g \rho_c z`                                                                           |
   |                                        |           |                                                  | -  1: use :math:`p_{ref} = -\int{-g\rho(T_{ref},S_{ref},p_{ref})dz}`                                    |
   |                                        |           |                                                  | -  2: hydrostatic dynamical pressure                                                                    |
   |                                        |           |                                                  | -  3: use full hyd.+non-hyd. pressure                                                                   |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | for ``JMD95P``, ``UNESCO``, ``MDJWF``, ``TEOS10`` default=2,  otherwise default =0                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`rhonil`                      | PARM01    | 9.998E+02                                        | reference density for linear EOS (kg/m\ :sup:`3`)                                                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tAlpha`                      | PARM01    | 2.0E-04                                          | linear EOS thermal expansion coefficient (1/\ :sup:`o`\ C)                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`sBeta`                       | PARM01    | 7.4E-04                                          | linear EOS haline contraction coefficient ((g/kg)\ :sup:`-1`)                                           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Thermodynamic Constants
~~~~~~~~~~~~~~~~~~~~~~~

.. tabularcolumns:: |\Y{.2}|\Y{.1}|\Y{.175}|\Y{.55}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`HeatCapacity_Cp`             | PARM01    | 3.994E+03                                        | specific heat capacity C\ :sub:`p` (ocean) (J/kg/K)                                                     |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`celsius2K`                   | PARM01    | 2.7315E+02                                       | conversion constant :sup:`o`\ C to Kelvin                                                               |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`atm_Cp`                      | PARM01    | 1.004E+03                                        | specific heat capacity C\ :sub:`p` dry air at const. press. (J/kg/K)                                    |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`atm_Rd`                      | PARM01    | :varlink:`atm_Cp`\*(2/7)                         | gas constant for dry air (J/kg/K)                                                                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`atm_Rq`                      | PARM01    | 0.0                                              | water vapor specific volume anomaly relative to dry air (g/kg)                                          |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`atm_Po`                      | PARM01    | 1.0E+05                                          | atmosphere standard reference pressure (for potential temp. defn.)  (Pa)                                |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _parms_mom:

Parameters: Momentum Equations
------------------------------

Configuration
~~~~~~~~~~~~~

There are a few logical variables that allow you to turn on/off various
terms in the momentum equation. These variables are called
:varlink:`momViscosity`, :varlink:`momAdvection`,  :varlink:`useCoriolis`,
:varlink:`momStepping`, :varlink:`metricTerms`, and
:varlink:`momPressureForcing` and by default are set to ``.TRUE.``.
Vertical diffusive fluxes of momentum can be computed implicitly
by setting the logical variable :varlink:`implicitViscosity` to
``.TRUE.``. The details relevant to both the momentum flux-form and the
vector-invariant form of the equations and the various (momentum) advection
schemes are covered in :numref:`discret_algorithm`.

.. tabularcolumns:: |\Y{.275}|\Y{.1}|\Y{.125}|\Y{.525}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`momStepping`                 | PARM01    | TRUE                                             | momentum equation time-stepping on/off flag                                                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`momViscosity`                | PARM01    | TRUE                                             | momentum friction terms on/off flag                                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`momAdvection`                | PARM01    | TRUE                                             | advection of momentum on/off flag                                                                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`momPressureForcing`          | PARM01    | TRUE                                             | pressure term in momentum equation on/off flag                                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`selectmetricTerms`           | PARM01    | 1                                                | spherical-polar, cyclindrical grid momentum flux-form metric terms options                              |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 0: don't include terms                                                                                |
   |                                        |           |                                                  | - 1 (and above): include terms (1=original discretization)                                              |
   |                                        |           |                                                  | - 2: alternate discretization, see :eq:`gu_metric`, :eq:`gv_metric` but averaging centered              |
   |                                        |           |                                                  |   at gridcell corner                                                                                    |
   |                                        |           |                                                  | - 3: as 2 but skip gU spherical terms by advecting :varlink:`uVel` * :varlink:`dxC`                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useNHMTerms`                 | PARM01    | FALSE                                            | use "non-hydrostatic form" of metric terms on/off flag; (see :numref:`non_hyd_metric_terms`;            |
   |                                        |           |                                                  | note these terms are non-zero in many model configurations beside non-hydrostatic)                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`momImplVertAdv`              | PARM01    | FALSE                                            | momentum implicit vertical advection on/off flag; requires #define :varlink:`INCLUDE_IMPLVERTADV_CODE`  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`implicitViscosity`           | PARM01    | FALSE                                            | implicit vertical viscosity on/off flag                                                                 |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`interViscAr_pCell`           | PARM04    | FALSE                                            | account for partial-cell in interior vertical viscosity on/off flag                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`momDissip_In_AB`             | PARM03    | TRUE                                             | use Adams-Bashforth time stepping for dissipation tendency                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useCoriolis`                 | PARM01    | TRUE                                             | include Coriolis terms on/off flag                                                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`selectCoriScheme`            | PARM01    | 0                                                | Coriolis scheme selector                                                                                |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 0: original scheme                                                                                    |
   |                                        |           |                                                  | - 1: wet-point averaging method                                                                         |
   |                                        |           |                                                  | - 2: Flux-Form: energy conserving; Vector-Inv: hFac weighted average                                    |
   |                                        |           |                                                  | - 3: Flux-Form: energy conserving using wet-point method; Vector-Inv: energy conserving with hFac weight|
   |                                        |           |                                                  | - 4: Flux-Form: hFac weighted average (angular momentum conserving)                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`select3dCoriScheme`          | PARM01    | 1                                                | :math:`\cos{\varphi}` Coriolis terms options                                                            |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 0: don't include terms                                                                                |
   |                                        |           |                                                  | - 1: (and above): include terms (1=original discretization)                                             |
   |                                        |           |                                                  | - 2: alternative discretization using averaged transport                                                |
   |                                        |           |                                                  | - 3: same as 2 with hFac in :math:`G_w^{\rm Cor}` term                                                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`vectorInvariantMomentum`     | PARM01    | FALSE                                            | use vector-invariant form of momentum equations flag                                                    |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useJamartMomAdv`             | PARM01    | FALSE                                            | use Jamart wetpoints method for relative vorticity advection (vector invariant form) on/off flag        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`selectVortScheme`            | PARM01    | 1                                                | vorticity scheme (vector invariant form) options                                                        |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 0,1: enstrophy conserving forms                                                                       |
   |                                        |           |                                                  | - 2: energy conserving form                                                                             |
   |                                        |           |                                                  | - 3: energy and enstrophy conserving form                                                               |
   |                                        |           |                                                  | - 4: shift 1/hFac from vorticity equation to final gU, gV tendency (angular momentum conserving)        |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | see Sadourny 1975 :cite:`sadourny:75` and Burridge & Haseler 1977 :cite:`burridge:77`                   |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   |  :varlink:`upwindVorticity`            | PARM01    | FALSE                                            | bias interpolation of vorticity in the Coriolis term (vector invariant form) on/off flag                |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   |  :varlink:`useAbsVorticity`            | PARM01    | FALSE                                            | use :math:`f + \zeta` in Coriolis terms (vector invariant form) on/off flag                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   |  :varlink:`highOrderVorticity`         | PARM01    | FALSE                                            | use 3rd/4th order interpolation of vorticity (vector invariant form) on/off flag                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   |  :varlink:`upwindShear`                | PARM01    | FALSE                                            | use 1st order upwind for vertical advection (vector invariant form) on/off flag                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   |  :varlink:`selectKEscheme`             | PARM01    | 0                                                | kinetic energy computation in Bernoulli function (vector invariant form) options                        |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 0: standard form                                                                                      |
   |                                        |           |                                                  | - 1: area-weighted standard form                                                                        |
   |                                        |           |                                                  | - 2: as 0 but account for partial cells                                                                 |
   |                                        |           |                                                  | - 3: as 1 w/partial cells                                                                               |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | see :filelink:`mom_calc_ke.F <pkg/mom_common/mom_calc_ke.F>`                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Initialization
~~~~~~~~~~~~~~    

The initial horizontal velocity components can be specified from
binary files :varlink:`uVelInitFile` and :varlink:`vVelInitFile`. These files
should contain 3-D data ordered in an (:math:`x,y,r`) fashion with k=1 as the
first vertical level (surface level). If no file names are provided,
the velocity is initialized to zero. The initial vertical velocity
is always derived from the horizontal velocity using the continuity
equation. In the case of a restart (from the end of a previous simulation),
the velocity field is read from a pickup file
(see :numref:`simulation_controls`) and the initial velocity files are ignored.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`uVelInitFile`                | PARM05    | :kbd:`' '`                                       | filename for 3D specification of initial zonal velocity field (m/s)                                     |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`vVelInitFile`                | PARM05    | :kbd:`' '`                                       | filename for 3D specification of initial meridional velocity field (m/s)                                |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`pSurfInitFile`               | PARM05    | :kbd:`' '`                                       | filename for 2D specification of initial free surface position ([:math:`r`] unit)                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _mom_dissip:

General Dissipation Scheme
~~~~~~~~~~~~~~~~~~~~~~~~~~
    
The lateral eddy viscosity coefficient is specified through the
variable :varlink:`viscAh` (in m\ :sup:`2`\ s\ :sup:`--1`). The
vertical eddy viscosity coefficient is specified through the
variable :varlink:`viscAr` (in [:math:`r`]\ :sup:`2`\ s\ :sup:`--1`,
where [:math:`r`] is the dimension of the vertical coordinate).
In addition, biharmonic mixing can be added as well
through the variable :varlink:`viscA4` (in
m\ :sup:`4`\ s\ :sup:`--1`).

.. tabularcolumns:: |\Y{.215}|\Y{.1}|\Y{.115}|\Y{.595}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`viscAh`                      | PARM01    | 0.0                                              | lateral eddy viscosity (m\ :sup:`2`\ /s)                                                                |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhD`                     | PARM01    | :varlink:`viscAh`                                | lateral eddy viscosity acts on divergence part (m\ :sup:`2`\ /s)                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhZ`                     | PARM01    | :varlink:`viscAh`                                | lateral eddy viscosity acts on vorticity part (:math:`\zeta` points) (m\ :sup:`2`\ /s)                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhW`                     | PARM01    | :varlink:`viscAhD`                               | lateral eddy viscosity for mixing vertical momentum (non-hydrostatic form) (m\ :sup:`2`\ /s);           |
   |                                        |           |                                                  | for non-hydrostatic only, see parameter :ref:`nonHydrostatic <model_config_parms>`                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhDfile`                 | PARM05    | :kbd:`' '`                                       | filename for 3D specification of lateral eddy viscosity (divergence part) (m\ :sup:`2`\ /s);            |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_3D_VISCAH` in :filelink:`pkg/mom_common/MOM_COMMON_OPTIONS.h`          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhZfile`                 | PARM05    | :kbd:`' '`                                       | filename for 3D specification of lateral eddy viscosity (vorticity part, :math:`\zeta` points);         |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_3D_VISCAH` in :filelink:`pkg/mom_common/MOM_COMMON_OPTIONS.h`          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhGrid`                  | PARM01    | 0.0                                              | grid-dependent lateral eddy viscosity (non-dim.)                                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhMax`                   | PARM01    | 1.0E+21                                          | maximum lateral eddy viscosity (m\ :sup:`2`\ /s)                                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhGridMax`               | PARM01    | 1.0E+21                                          | maximum lateral eddy (grid-dependent) viscosity (non-dim.)                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhGridMin`               | PARM01    | 0.0                                              | minimum lateral eddy (grid-dependent) viscosity (non-dim.)                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAhReMax`                 | PARM01    | 0.0                                              | minimum lateral eddy viscosity based on Reynolds number (non-dim.)                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscC2leith`                 | PARM01    | 0.0                                              | Leith harmonic viscosity factor (vorticity part, :math:`\zeta` points) (non-dim.)                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscC2leithD`                | PARM01    | 0.0                                              | Leith harmonic viscosity factor (divergence part) (non-dim.)                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscC2LeithQG`               | PARM01    | 0.0                                              | Quasi-geostrophic Leith  viscosity factor (non-dim.)                                                    |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscC2smag`                  | PARM01    | 0.0                                              | Smagorinsky harmonic viscosity factor (non-dim.)                                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4`                      | PARM01    | 0.0                                              | lateral biharmonic viscosity (m\ :sup:`4`\ /s)                                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4D`                     | PARM01    | :varlink:`viscA4`                                | lateral biharmonic viscosity (divergence part) (m\ :sup:`4`\ /s)                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4Z`                     | PARM01    | :varlink:`viscA4`                                | lateral biharmonic viscosity (vorticity part, :math:`\zeta` points) (m\ :sup:`4`\ /s)                   |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4W`                     | PARM01    | :varlink:`viscA4D`                               | lateral biharmonic viscosity for mixing vertical momentum (non-hydrostatic form) (m\ :sup:`4`\ /s);     |
   |                                        |           |                                                  | for non-hydrostatic only, see parameter :ref:`nonHydrostatic <model_config_parms>`                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4Dfile`                 | PARM05    | :kbd:`' '`                                       | filename for 3D specification of lateral biharmonic viscosity (divergence part)  (m\ :sup:`4`\ /s);     |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_3D_VISCA4` in :filelink:`pkg/mom_common/MOM_COMMON_OPTIONS.h`          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4Zfile`                 | PARM05    | :kbd:`' '`                                       | filename for 3D specification of lateral biharmonic viscosity (vorticity part, :math:`\zeta` points);   |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_3D_VISCA4` in :filelink:`pkg/mom_common/MOM_COMMON_OPTIONS.h`          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4Grid`                  | PARM01    | 0.0                                              | grid dependent biharmonic viscosity (non-dim.)                                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4Max`                   | PARM01    | 1.0E+21                                          | maximum biharmonic viscosity (m\ :sup:`4`\ /s)                                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4GridMax`               | PARM01    | 1.0E+21                                          | maximum biharmonic (grid-dependent) viscosity (non-dim.)                                                |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4GridMin`               | PARM01    | 0.0                                              | minimum biharmonic (grid-dependent) viscosity (mon-dim.)                                                |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscA4ReMax`                 | PARM01    | 0.0                                              | minimum biharmonic viscosity based on Reynolds number (non-dim.)                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscC4leith`                 | PARM01    | 0.0                                              | Leith biharmonic viscosity factor (vorticity part, :math:`\zeta` points) (non-dim.)                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscC4leithD`                | PARM01    | 0.0                                              | Leith biharmonic viscosity factor (divergence part) (non-dim.)                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscC4smag`                  | PARM01    | 0.0                                              | Smagorinsky biharmonic viscosity factor (non-dim.)                                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useFullLeith`                | PARM01    | FALSE                                            | use full form of Leith viscosities on/off flag                                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useSmag3D`                   | PARM01    | FALSE                                            | use isotropic 3D Smagorinsky harmonic viscosities flag; requires #define :varlink:`ALLOW_SMAG_3D`       |
   |                                        |           |                                                  | in :filelink:`pkg/mom_common/MOM_COMMON_OPTIONS.h`                                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`smag3D_coeff`                | PARM01    | 1.0E-02                                          | isotropic 3D Smagorinsky coefficient (non-dim.); requires #define :varlink:`ALLOW_SMAG_3D`              |
   |                                        |           |                                                  | in :filelink:`pkg/mom_common/MOM_COMMON_OPTIONS.h`                                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useStrainTensionVisc`        | PARM01    | FALSE                                            | flag to use strain-tension form of viscous operator                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`useAreaViscLength`           | PARM01    | FALSE                                            | flag to use area for viscous :math:`L^2` instead of harmonic mean of :math:`{L_x}^2, {L_y}^2`           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscAr`                      | PARM01    | 0.0                                              | vertical eddy viscosity ([:math:`r`]\ :sup:`2`\ /s)                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`viscArNr`                    | PARM01    | 0.0                                              | vertical profile of vertical eddy viscosity ([:math:`r`]\ :sup:`2`\ /s)                                 |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`pCellMix_viscAr`             | PARM04    | :varlink:`viscArNr`                              | vertical viscosity for too thin partial-cell ([:math:`r`]\ :sup:`2`\ /s)                                |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Sidewall/Bottom Dissipation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Slip or no-slip conditions at lateral and bottom
boundaries are specified through the logical variables
:varlink:`no_slip_sides` and :varlink:`no_slip_bottom`. If set to
``.FALSE.``, free-slip boundary conditions are applied. If no-slip
boundary conditions are applied at the bottom, a bottom drag can be
applied as well. Two forms are available: linear (set the variable
:varlink:`bottomDragLinear` in [:math:`r`]/s, )
and quadratic (set the variable
:varlink:`bottomDragQuadratic`, [:math:`r`]/m).

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`no_slip_sides`               | PARM01    | TRUE                                             | viscous BCs: no-slip sides on/off flag                                                                  |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`sideDragFactor`              | PARM01    | 2.0E+00                                          | side-drag scaling factor (2.0: full drag) (non-dim.)                                                    |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`no_slip_bottom`              | PARM01    | TRUE                                             | viscous BCs: no-slip bottom on/off flag                                                                 |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`bottomDragLinear`            | PARM01    | 0.0                                              | linear bottom-drag coefficient ([:math:`r`]/s)                                                          |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`bottomDragQuadratic`         | PARM01    | 0.0                                              | quadratic bottom-drag coefficient ([:math:`r`]/m)                                                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`zRoughBot`                   | PARM01    | 0.0                                              | roughness length for quadratic bottom friction coefficient (m)                                          |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`selectBotDragQuadr`          | PARM01    | -1                                               | select quadratic bottom drag discretization option                                                      |
|                                        |           |                                                  |                                                                                                         |
|                                        |           |                                                  | - -1: not used                                                                                          |
|                                        |           |                                                  | - 0: average KE from grid center to :math:`u,v` location                                                |
|                                        |           |                                                  | - 1: use local velocity norm @ :math:`u,v` location                                                     |
|                                        |           |                                                  | - 2: as 1 with wet-point averaging of other velocity component                                          |
|                                        |           |                                                  |                                                                                                         |
|                                        |           |                                                  | if :varlink:`bottomDragQuadratic` :math:`\neq 0.` then default is 0                                     |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`selectImplicitDrag`          | PARM01    | 0                                                | top/bottom drag implicit treatment options                                                              |
|                                        |           |                                                  |                                                                                                         |
|                                        |           |                                                  | - 0: fully explicit                                                                                     |
|                                        |           |                                                  | - 1: implicit on provisional velocity, i.e., before :math:`\nabla \eta` increment                       |
|                                        |           |                                                  | - 2: fully implicit                                                                                     |
|                                        |           |                                                  |                                                                                                         |
|                                        |           |                                                  | if =2, requires #define :varlink:`ALLOW_SOLVE4_PS_AND_DRAG`                                             |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`bottomVisc_pCell`            | PARM01    | FALSE                                            | account for partial-cell in bottom viscosity (using :varlink:`no_slip_bottom` = ``.TRUE.``) on/off flag |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Parameters: Tracer Equations
----------------------------

This section covers the tracer equations, i.e., the potential temperature
equation and the salinity (for the ocean) or specific humidity (for the
atmosphere) equation.

Configuration
~~~~~~~~~~~~~

The logical variables :varlink:`tempAdvection`,  and
:varlink:`tempStepping` allow you to turn on/off terms in the temperature
equation (similarly for salinity or specific humidity with variables
:varlink:`saltAdvection` etc.). These variables all
default to a value of ``.TRUE.``.  The vertical diffusive
fluxes can be computed implicitly by setting the logical variable
:varlink:`implicitDiffusion` to ``.TRUE.``.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.175}|\Y{.525}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`tempStepping`                | PARM01    | TRUE                                             | temperature equation time-stepping on/off flag                                                          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tempAdvection`               | PARM01    | TRUE                                             | advection of temperature on/off flag                                                                    |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tempAdvScheme`               | PARM01    | 2                                                | temperature horizontal advection scheme selector (see :numref:`adv_scheme_summary`)                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tempVertAdvScheme`           | PARM01    | :varlink:`tempAdvScheme`                         | temperature vertical advection scheme selector (see :numref:`adv_scheme_summary`)                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tempImplVertAdv`             | PARM01    | FALSE                                            | temperature implicit vertical advection on/off flag                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`addFrictionHeating`          | PARM01    | FALSE                                            | include frictional heating in temperature equation on/off flag;                                         |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_FRICTION_HEATING`                                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`temp_stayPositive`           | PARM01    | FALSE                                            | use Smolarkiewicz hack to ensure temperature stays positive on/off flag;                                |
   |                                        |           |                                                  | requires #define :varlink:`GAD_SMOLARKIEWICZ_HACK` in :filelink:`pkg/generic_advdiff/GAD_OPTIONS.h`     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`saltStepping`                | PARM01    | TRUE                                             | salinity equation time-stepping on/off flag                                                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`saltAdvection`               | PARM01    | TRUE                                             | advection of salinity on/off flag                                                                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`saltAdvScheme`               | PARM01    | 2                                                | salinity horizontal advection scheme selector (see :numref:`adv_scheme_summary`)                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`saltVertAdvScheme`           | PARM01    | :varlink:`saltAdvScheme`                         | salinity vertical  advection scheme selector (see :numref:`adv_scheme_summary`)                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`saltImplVertAdv`             | PARM01    | FALSE                                            | salinity implicit vertical advection on/off flag                                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`salt_stayPositive`           | PARM01    | FALSE                                            | use Smolarkiewicz hack to ensure salinity stays positive on/off flag;                                   |
   |                                        |           |                                                  | requires #define :varlink:`GAD_SMOLARKIEWICZ_HACK` in :filelink:`pkg/generic_advdiff/GAD_OPTIONS.h`     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`implicitDiffusion`           | PARM01    | FALSE                                            | implicit vertical diffusion on/off flag                                                                 |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`interDiffKr_pCell`           | PARM04    | FALSE                                            | account for partial-cell in interior vertical diffusion on/off flag                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`linFSConserveTr`             | PARM01    | FALSE                                            | correct source/sink of tracer due to use of linear free surface on/off flag                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`doAB_onGtGs`                 | PARM03    | TRUE                                             | apply Adams-Bashforth on tendencies (rather than on T,S) on/off flag                                    |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Initialization
~~~~~~~~~~~~~~

The initial tracer data can be contained in the binary files
:varlink:`hydrogThetaFile` and :varlink:`hydrogSaltFile`. These files should
contain 3-D data ordered in an (:math:`x,y,r`) fashion with k=1 as the first
vertical level. If no file names are provided, the tracers are then
initialized with the values of :varlink:`tRef` and :varlink:`sRef` discussed in
:numref:`parms-eos`. In this case, the initial tracer data are uniform in
:math:`x` and :math:`y` for each depth level.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`hydrogThetaFile`             | PARM05    | :kbd:`' '`                                       | filename for 3D specification of initial potential temperature (:sup:`o`\ C)                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`hydrogSaltFile`              | PARM05    | :kbd:`' '`                                       | filename for 3D specification of initial salinity (g/kg)                                                |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`maskIniTemp`                 | PARM05    | TRUE                                             | apply (center-point) mask to initial hydrographic theta data on/off flag                                |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`maskIniSalt`                 | PARM05    | TRUE                                             | apply (center-point) mask to initial hydrographic salinity on/off flag                                  |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`checkIniTemp`                | PARM05    | TRUE                                             | check if initial theta (at wet-point) identically zero on/off flag                                      |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`checkIniSalt`                | PARM05    | TRUE                                             | check if initial salinity (at wet-point) identically zero on/off flag                                   |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Tracer Diffusivities
~~~~~~~~~~~~~~~~~~~~     

Lateral eddy diffusivities for temperature and salinity/specific
humidity are specified through the variables :varlink:`diffKhT` and
:varlink:`diffKhS` (in m\ :sup:`2`\ /s). Vertical eddy diffusivities are
specified through the variables :varlink:`diffKrT` and :varlink:`diffKrS`.
In addition, biharmonic diffusivities can be specified as well through the
coefficients :varlink:`diffK4T` and :varlink:`diffK4S` (in m\ :sup:`4`\ /s).
The Gent and McWilliams parameterization for advection and mixing of oceanic
tracers is described in :numref:`sub_phys_pkg_gmredi`.

.. tabularcolumns:: |\Y{.2}|\Y{.1}|\Y{.15}|\Y{.575}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`diffKhT`                     | PARM01    | 0.0                                              | Laplacian diffusivity of heat laterally (m\ :sup:`2`\ /s)                                               |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffK4T`                     | PARM01    | 0.0                                              | biharmonic diffusivity of heat laterally (m\ :sup:`4`\ /s)                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrT`                     | PARM01    | 0.0                                              | Laplacian diffusivity of heat vertically (m\ :sup:`2`\ /s)                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKr4T`                    | PARM01    | 0.0                                              | biharmonic diffusivity of heat vertically (m\ :sup:`2`\ /s)                                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrNrT`                   | PARM01    | 0.0 at k=top                                     | vertical profile of vertical diffusivity of temperature (m\ :sup:`2`\ /s)                               |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`pCellMix_diffKr`             | PARM04    | :varlink:`diffKrNr`                              | vertical diffusivity for too thin partial-cell ([r]\ :sup:`2`\ /s)                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKhS`                     | PARM01    | 0.0                                              | Laplacian diffusivity of salt laterally (m\ :sup:`2`\ /s)                                               |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffK4S`                     | PARM01    | 0.0                                              | biharmonic diffusivity of salt laterally (m\ :sup:`4`\ /s)                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrS`                     | PARM01    | 0.0                                              | Laplacian diffusivity of salt vertically (m\ :sup:`2`\ /s)                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKr4S`                    | PARM01    | 0.0                                              | biharmonic diffusivity of salt vertically (m\ :sup:`2`\ /s)                                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrNrS`                   | PARM01    | 0.0 at k=top                                     | vertical profile of vertical diffusivity of salt (m\ :sup:`2`\ /s)                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrFile`                  | PARM05    | :kbd:`' '`                                       | filename for 3D specification of vertical diffusivity (m\ :sup:`2`\ /s);                                |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_3D_DIFFKR`                                                             |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrBL79surf`              | PARM01    | 0.0                                              | surface diffusivity for Bryan & Lewis 1979 :cite:`bryan:79` (m\ :sup:`2`\ /s)                           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrBL79deep`              | PARM01    | 0.0                                              | deep diffusivity for Bryan & Lewis 1979 :cite:`bryan:79` (m\ :sup:`2`\ /s)                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrBL79scl`               | PARM01    | 2.0E+02                                          | depth scale for Bryan & Lewis 1979 :cite:`bryan:79` (m)                                                 |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrBL79Ho`                | PARM01    | -2.0E+03                                         | turning depth for Bryan & Lewis 1979 :cite:`bryan:79` (m)                                               |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrBLEQsurf`              | PARM01    | 0.0                                              | same as :varlink:`diffKrBL79surf` but at equator; requires #define :varlink:`ALLOW_BL79_LAT_VARY`       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrBLEQdeep`              | PARM01    | 0.0                                              | same as :varlink:`diffKrBL79deep` but at equator; requires #define :varlink:`ALLOW_BL79_LAT_VARY`       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrBLEQscl`               | PARM01    | 2.0E+02                                          | same as :varlink:`diffKrBL79scl` but at equator; requires #define :varlink:`ALLOW_BL79_LAT_VARY`        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`diffKrBLEQHo`                | PARM01    | -2.0E+03                                         | same as :varlink:`diffKrBL79Ho` but at equator; requires #define :varlink:`ALLOW_BL79_LAT_VARY`         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`BL79LatVary`                 | PARM01    | 3.0E+01                                          | transition from diffKrBLEQ to diffKrBL79 parms at this latitude;                                        |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_BL79_LAT_VARY`                                                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _ocean_convection_parms:

Ocean Convection
~~~~~~~~~~~~~~~~     

In addition to specific packages that parameterize ocean convection, two main
model options are available. To use the first option, a convective adjustment
scheme, you need to set the variable :varlink:`cadjFreq`, the frequency
(in seconds) with which the adjustment algorithm is called, to a non-zero value
(note, if :varlink:`cadjFreq` set to a negative value by the user, the model
will set it to the model clock time step). The second option is to parameterize
convection with implicit vertical diffusion. To do this, set the
logical variable :varlink:`implicitDiffusion` to ``.TRUE.`` and the real
variable :varlink:`ivdc_kappa` (in m\ :sup:`2`\ /s) to
an appropriate tracer vertical diffusivity value for mixing
due to static instabilities (typically, several orders of magnitude above the
background vertical diffusivity). Note that :varlink:`cadjFreq` and
:varlink:`ivdc_kappa` cannot both have non-zero value.

.. tabularcolumns:: |\Y{.2}|\Y{.1}|\Y{.125}|\Y{.6}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`ivdc_kappa`                  | PARM01    | 0.0                                              | implicit vertical diffusivity for convection (m\ :sup:`2`\ /s)                                          |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`cAdjFreq`                    | PARM03    | 0                                                | frequency of convective adj. scheme; <0: sets value to :varlink:`deltaTclock` (s)                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`hMixCriteria`                | PARM01    | -0.8E+00                                         | - <0: specifies :math:`\Delta T` (:sup:`o`\ C) to define ML depth where                                 |
|                                        |           |                                                  |   :math:`\Delta\rho = \Delta T*d\rho/dT` occurs                                                         |
|                                        |           |                                                  | - >1: define ML depth where local strat. exceeds mean strat. by this factor (non-dim.)                  |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`hMixSmooth`                  | PARM01    | 0.0                                              | use this fraction of neighboring points (for smoothing) in ML calculation (0-1; 0: no smoothing)        |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Parameters: Model Forcing
-------------------------

The forcing options that can be prescribed through runtime parameters in
``data`` are easy to use but somewhat limited in scope. More complex forcing
setups are possible with optional packages such as :filelink:`pkg/exf` or
:filelink:`pkg/rbcs`, in which case most or all of the parameters in this
section can simply be left at their default value.

Momentum Forcing
~~~~~~~~~~~~~~~~
    
This section only applies to the ocean. You need to generate
wind-stress data into two files :varlink:`zonalWindFile` and
:varlink:`meridWindFile` corresponding to the zonal and meridional
components of the wind stress, respectively (if you want the stress
to be along the direction of only one of the model horizontal axes,
you only need to generate one file). The format of the files is
similar to the bathymetry file. The zonal (meridional) stress data
are assumed to be in pascals and located at U-points (V-points). See the MATLAB
program ``gendata.m`` in the ``input`` directories of
``verification`` for several tutorial example (e.g.
:filelink:`gendata.m <verification/tutorial_barotropic_gyre/input/gendata.m>`
in the :ref:`barotropic gyre tutorial <sec_eg_baro>`)
to see how simple analytical wind forcing data are generated for the
case study experiments.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`momForcing`                  | PARM01    | TRUE                                             | included external forcing of momentum on/off flag                                                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`zonalWindFile`               | PARM05    | :kbd:`' '`                                       | filename for 2D specification of zonal component of wind forcing (N/m\ :sup:`2`)                        |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`meridWindFile`               | PARM05    | :kbd:`' '`                                       | filename for 2D specification of meridional component of wind forcing (N/m\ :sup:`2`)                   |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`momForcingOutAB`             | PARM03    | 0                                                | 1: take momentum forcing out of Adams-Bashforth time stepping                                           |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`momTidalForcing`             | PARM01    | TRUE                                             | tidal forcing of momentum equation on/off flag (requires tidal forcing files)                           |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`ploadFile`                   | PARM05    | :kbd:`' '`                                       | filename for 2D specification of atmospheric pressure loading (ocean :math:`z`-coor. only) (Pa)         |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Tracer Forcing
~~~~~~~~~~~~~~     

A combination of flux data and relaxation terms can be used for
driving the tracer equations. For potential temperature, heat flux
data (in W/m\ :sup:`2`) can be stored in the 2-D binary file
:varlink:`surfQnetfile`. Alternatively or in addition, the forcing can be
specified through a relaxation term. The SST data to which the model
surface temperatures are restored are stored in
the 2-D binary file :varlink:`thetaClimFile`. The corresponding relaxation
time scale coefficient is set through the variable
:varlink:`tauThetaClimRelax` (in seconds). The same procedure applies for
salinity with the variable names :varlink:`EmPmRfile`, :varlink:`saltClimFile`,
and :varlink:`tauSaltClimRelax` for freshwater flux (in m/s) and surface
salinity (in g/kg) data files and relaxation timescale coefficient
(in seconds), respectively.

.. tabularcolumns:: |\Y{.24}|\Y{.1}|\Y{.15}|\Y{.535}|

.. table::
   :class: longtable

   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | Parameter                              | Group     | Default                                          | Description                                                                                             |
   +========================================+===========+==================================================+=========================================================================================================+
   | :varlink:`tempForcing`                 | PARM01    | TRUE                                             | external forcing of temperature forcing on/off flag                                                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`surfQnetFile`                | PARM05    | :kbd:`' '`                                       | filename for 2D specification of net total heat flux (W/m\ :sup:`2`)                                    |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`surfQswFile`                 | PARM05    | :kbd:`' '`                                       | filename for 2D specification of net shortwave flux (W/m\ :sup:`2`);                                    |
   |                                        |           |                                                  | requires #define :varlink:`SHORTWAVE_HEATING`                                                           |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tauThetaClimRelax`           | PARM03    | 0.0                                              | temperature (surface) relaxation time scale (s)                                                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`lambdaThetaFile`             | PARM05    | :kbd:`' '`                                       | filename for 2D specification of inverse temperature (surface) relaxation time scale (1/s)              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`ThetaClimFile`               | PARM05    | :kbd:`' '`                                       | filename for specification of (surface) temperature relaxation values (:sup:`o`\ C)                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`balanceThetaClimRelax`       | PARM01    | FALSE                                            | subtract global mean heat flux due to temp. relaxation flux every time step on/off flag;                |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_BALANCE_RELAX`                                                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`balanceQnet`                 | PARM01    | FALSE                                            | subtract global mean Qnet every time step on/off flag; requires #define :varlink:`ALLOW_BALANCE_FLUXES` |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`geothermalFile`              | PARM05    | :kbd:`' '`                                       | filename for 2D specification of geothermal heating flux through bottom (W/m\ :sup:`2`);                |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_GEOTHERMAL_FLUX`                                                       |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`temp_EvPrRn`                 | PARM01    | UNSET                                            | temperature of rain and evaporated water (unset, use local temp.) (:sup:`o`\ C)                         |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`allowFreezing`               | PARM01    | FALSE                                            | limit (ocean) temperature at surface to >= -1.9\ :sup:`o`\ C                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`saltForcing`                 | PARM01    | TRUE                                             | external forcing of salinity forcing on/off flag                                                        |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`convertFW2Salt`              | PARM01    | 3.5E+01                                          | salinity used to convert freshwater flux to salt flux (-1: use local S) (g/kg)                          |
   |                                        |           |                                                  | (note default is -1 if :varlink:`useRealFreshWaterFlux`\ = ``.TRUE.``)                                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`rhoConstFresh`               | PARM01    | :varlink:`rhoConst`                              | constant reference density for fresh water (rain) (kg/m\ :sup:`3`)                                      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`EmPmRFile`                   | PARM05    | :kbd:`' '`                                       | filename for 2D specification of net freshwater flux (m/s)                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`saltFluxFile`                | PARM05    | :kbd:`' '`                                       | filename for 2D specification of salt flux (from seaice) ((g/kg).kg/m\ :sup:`2`\/s)                     |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tauSaltClimRelax`            | PARM03    | 0.0                                              | salinity (surface) relaxation time scale (s)                                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`lambdaSaltFile`              | PARM05    | :kbd:`' '`                                       | filename for 2D specification of inverse salinity (surface) relaxation time scale (1/s)                 |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`saltClimFile`                | PARM05    | :kbd:`' '`                                       | filename for specification of (surface) salinity relaxation values (g/kg)                               |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`balanceSaltClimRelax`        | PARM01    | FALSE                                            | subtract global mean flux due to salt relaxation every time step on/off flag                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`selectBalanceEmPmR`          | PARM01    | 0                                                | option to balance net surface freshwater flux every time step                                           |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | - 0: off                                                                                                |
   |                                        |           |                                                  | - 1: uniform surface correction                                                                         |
   |                                        |           |                                                  | - 2: non-uniform surface correction, scaled using :varlink:`wghtBalancedFile` for local weighting       |
   |                                        |           |                                                  |                                                                                                         |
   |                                        |           |                                                  | if =1 or 2, requires #define :varlink:`ALLOW_BALANCE_FLUXES`                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`wghtBalanceFile`             | PARM05    | :kbd:`' '`                                       | filename for 2D specification of weights used in :varlink:`selectBalanceEmPmR` =2 correction            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`salt_EvPrRn`                 | PARM01    | 0.0                                              | salinity of rain and evaporated water (g/kg)                                                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`selectAddFluid`              | PARM01    | 0                                                | add fluid to ocean interior options (-1, 0: off, or 1); requires #define :varlink:`ALLOW_ADDFLUID`      |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`temp_addMass`                | PARM01    | :varlink:`temp_EvPrRn`                           | temp. of added or removed (interior) water (:sup:`o`\ C); requires #define :varlink:`ALLOW_ADDFLUID`    |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`salt_addMass`                | PARM01    | :varlink:`salt_EvPrRn`                           | salinity of added or removed (interior) water (:sup:`o`\ C); requires #define :varlink:`ALLOW_ADDFLUID` |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`addMassFile`                 | PARM05    | :kbd:`' '`                                       | filename for 3D specification of mass source/sink (+=source, kg/s);                                     |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_ADDFLUID`                                                              |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`balancePrintMean`            | PARM01    | FALSE                                            | print subtracted balancing means to STDOUT on/off flag;                                                 |
   |                                        |           |                                                  | requires #define :varlink:`ALLOW_BALANCE_FLUXES` and/or #define :varlink:`ALLOW_BALANCE_RELAX`          |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`latBandClimRelax`            | PARM03    | whole domain                                     | relaxation to (T,S) climatology equatorward of this latitude band is applied                            |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
   | :varlink:`tracForcingOutAB`            | PARM03    | 0                                                | 1: take T, S, and pTracer forcing out of Adams-Bashforth time stepping                                  |
   +----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _periodic_forcing_expl:

Periodic Forcing
~~~~~~~~~~~~~~~~

To prescribe time-dependent periodic
forcing, concatenate successive time records into a
single file ordered in a (:math:`x,y`,time) fashion
and set the following variables: :varlink:`periodicExternalForcing` to
``.TRUE.``, :varlink:`externForcingPeriod` to the period (in seconds between
two records in input files) with which
the forcing varies (e.g., 1 month), and :varlink:`externForcingCycle`
to the repeat time (in seconds) of the forcing (e.g., 1 year; note
:varlink:`externForcingCycle` must be a multiple of
:varlink:`externForcingPeriod`). With these variables specified, the model
will interpolate the forcing linearly at each iteration.

.. tabularcolumns:: |\Y{.25}|\Y{.1}|\Y{.125}|\Y{.55}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`periodicExternalForcing`     | PARM03    | FALSE                                            | allow time-dependent periodic forcing on/off flag                                                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`externForcingPeriod`         | PARM03    | 0.0                                              | period over which forcing varies (e.g. monthly) (s)                                                     |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`externForcingCycle`          | PARM03    | 0.0                                              | period over which the forcing cycle repeats (e.g. one year) (s)                                         |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _simulation_controls:

Parameters: Simulation Controls
-------------------------------

Run Start and Duration
~~~~~~~~~~~~~~~~~~~~~~     

The beginning of a simulation is set by specifying a start time (in seconds)
through the real variable :varlink:`startTime` or by specifying an
initial iteration number through the integer variable :varlink:`nIter0`. If
these variables are set to non-zero values, the model will look for a
”pickup” file (by default, ``pickup.0000nIter0``) to restart the integration.
The end of a simulation is set through the real variable :varlink:`endTime`
(in seconds). Alternatively, one can instead specify the number of time steps
to execute through the integer variable :varlink:`nTimeSteps`.
Iterations are referenced to :varlink:`deltaTClock`, i.e., each iteration is
:varlink:`deltaTClock` seconds of model time.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`nIter0`                      | PARM03    | 0                                                | starting timestep iteration number for this integration                                                 |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nTimeSteps`                  | PARM03    | 0                                                | number of (model clock) timesteps to execute                                                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nEndIter`                    | PARM03    | 0                                                | run ending timestep iteration number (alternate way to prescribe :varlink:`nTimeSteps`)                 |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`startTime`                   | PARM03    | :varlink:`baseTime`                              | run start time for this integration (s) (alternate way to prescribe :varlink:`nIter0`)                  |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`endTime`                     | PARM03    | 0.0                                              | run ending time (s) (with :varlink:`startTime`, alternate way to prescribe :varlink:`nTimeSteps`)       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Input/Output Files
~~~~~~~~~~~~~~~~~~

The precision with which to read binary data is
controlled by the integer variable :varlink:`readBinaryPrec`,  which can take
the value 32 (single precision) or 64 (double precision). Similarly, the
precision with which to write binary data is controlled by the integer variable
:varlink:`writeBinaryPrec`. By default, MITgcm writes output (snapshots,
diagnostics, and pickups) separately for individual tiles, leaving it to the
user to reassemble these into global files, if needed (scripts are available in
:filelink:`utils/`). There are two options however to have the model do this
for you. Setting :varlink:`globalFiles` to ``.TRUE.`` should always work in a
single process setup (including multi-threaded processes), but for
`MPI <https://en.wikipedia.org/wiki/Message_Passing_Interface>`_ runs this will
depend on the platform -- it requires simultaneous write access to a common
file (permissible in typical
`Lustre <https://en.wikipedia.org/wiki/Lustre_(file_system)>`_ setups, but not
on all file systems). Alternatively, one can set :varlink:`useSingleCpuIO`
to ``.TRUE.`` to generate global files, which should always work, but requires
additional mpi-passing of data and may result in slower execution.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
|  :varlink:`globalFiles`                | PARM01    | FALSE                                            | write output “global” (i.e. not per tile) files on/off flag                                             |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`useSingleCpuIO`             | PARM01    | FALSE                                            | only master MPI process does I/O (producing global output files)                                        |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`the_run_name`               | PARM05    | :kbd:`' '`                                       | string identifying the name of the model "run" for meta files                                           |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`readBinaryPrec`             | PARM01    | 32                                               | precision used for reading binary files (32 or 64)                                                      |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`writeBinaryPrec`            | PARM01    | 32                                               | precision used for writing binary files (32 or 64)                                                      |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`outputTypesInclusive`       | PARM03    | FALSE                                            | allows writing of output files in multiple formats (i.e. :filelink:`pkg/mdsio` and  :filelink:`pkg/mnc`)|
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`rwSuffixType`               | PARM03    | 0                                                | controls the format of the :filelink:`pkg/mdsio` binary file “suffix”                                   |
|                                        |           |                                                  |                                                                                                         |
|                                        |           |                                                  | - 0: use iteration number (myIter, I10.10)                                                              |
|                                        |           |                                                  | - 1: 100*myTime                                                                                         |
|                                        |           |                                                  | - 2: myTime                                                                                             |
|                                        |           |                                                  | - 3: myTime/360                                                                                         |
|                                        |           |                                                  | - 4: myTime/3600                                                                                        |
|                                        |           |                                                  |                                                                                                         |
|                                        |           |                                                  | where :varlink:`myTime` is model time in seconds                                                        |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`mdsioLocalDir`              | PARM05    | :kbd:`' '`                                       | if not blank, read-write output tiled files from/to this directory name                                 |
|                                        |           |                                                  | (+four-digit processor-rank code)                                                                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _freq_of_output:

Frequency/Amount of Output
~~~~~~~~~~~~~~~~~~~~~~~~~~

The frequency (in seconds) with which output
is written to disk needs to be specified. :varlink:`dumpFreq` controls the
frequency with which the instantaneous state of the model is written.
:varlink:`monitorFreq` controls the frequency with which monitor output is
dumped to the standard output file(s). The frequency of output is referenced
to :varlink:`deltaTClock`.

.. tabularcolumns:: |\Y{.18}|\Y{.1}|\Y{.2}|\Y{.545}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`dumpFreq`                    | PARM03    | 0.0                                              | interval to write model state/snapshot data (s)                                                         |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`dumpInitAndLast`             | PARM03    | TRUE                                             | write out initial and last iteration model state on/off flag                                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`diagFreq`                    | PARM03    | 0.0                                              | interval to write additional intermediate (debugging cg2d/3d) output (s)                                |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`monitorFreq`                 | PARM03    | lowest of other output \*Freq parms              | interval to write monitor output (s)                                                                    |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`monitorSelect`               | PARM03    | 2 (3 if fluid is water)                          | select group of monitor variables to output                                                             |
|                                        |           |                                                  |                                                                                                         |
|                                        |           |                                                  | - 1: dynamic variables only                                                                             |
|                                        |           |                                                  | - 2: add vorticity variables                                                                            |
|                                        |           |                                                  | - 3: add surface variables                                                                              |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`debugLevel`                 | PARM01    | depends on :varlink:`debugMode`                  | level of printing of MITgcm activity messages/statistics (1-5, higher -> more activity messages)        |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`plotLevel`                  | PARM01    | :varlink:`debugLevel`                            | controls printing of field maps (1-5, higher -> more fields)                                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Restart/Pickup Files
~~~~~~~~~~~~~~~~~~~~

:varlink:`chkPtFreq` and :varlink:`pchkPtFreq` control the output frequency of
rolling and permanent pickup (a.k.a. checkpoint) files, respectively. These
frequencies are referenced to :varlink:`deltaTClock`.

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`pChkPtFreq`                  | PARM03    | 0.0                                              | permanent restart/pickup checkpoint file write interval ( s )                                           |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`chkPtFreq`                   | PARM03    | 0.0                                              | rolling restart/pickup checkpoint file write interval ( s )                                             |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`pickupSuff`                  | PARM03    | :kbd:`' '`                                       | force run to use pickups (even if :varlink:`nIter0` =0) and read files with this suffix (10 char. max)  |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`pickupStrictlyMatch`         | PARM03    | TRUE                                             | force pickup (meta) file formats to exactly match (or terminate with error) on/off flag                 |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`writePickupAtEnd`            | PARM03    | FALSE                                            | write a (rolling) pickup file at run completion on/off flag                                             |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`usePickupBeforeC54`         | PARM01    | FALSE                                            | initialize run using old pickup format from code prior to checkpoint54a                                 |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`startFromPickupAB2`         | PARM03    | FALSE                                            | using Adams-Bashforth-3, start using Adams-Bashforth-2 pickup format;                                   |
|                                        |           |                                                  | requires #define :varlink:`ALLOW_ADAMSBASHFORTH_3`                                                      |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Parameters Used In Optional Packages
------------------------------------

Some optional packages were not written with package-specific namelist
parameters in a ``data.${pkg}`` file; or for historical and/or other reasons,
several package-specific namelist parameters remain in ``data``.

.. _c-d_scheme:

C-D Scheme
~~~~~~~~~~     

(package :filelink:`pkg/cd_code`)

If you run at a sufficiently coarse resolution, you might choose to enable the
C-D scheme for the computation of the Coriolis terms. The
variable :varlink:`tauCD`, which represents the C-D scheme coupling
timescale (in seconds) needs to be set.

.. tabularcolumns:: |\Y{.175}|\Y{.1}|\Y{.225}|\Y{.525}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`useCDscheme`                 | PARM01    | FALSE                                            | use C-D scheme for Coriolis terms on/off flag                                                           |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`tauCD`                       | PARM03    | :varlink:`deltaTMom`                             | C-D scheme coupling timescale (s)                                                                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`rCD`                         | PARM03    | 1 - :varlink:`deltaTMom`/:varlink:`tauCD`        | C-D scheme normalized coupling parameter (non-dim.)                                                     |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`epsAB_CD`                    | PARM03    | :varlink:`abEps`                                 | Adams-Bashforth-2 stabilizing weight used in C-D scheme                                                 |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

Automatic Differentiation
~~~~~~~~~~~~~~~~~~~~~~~~~

(package :filelink:`pkg/autodiff`; see :numref:`chap_autodiff`)

.. tabularcolumns:: |\Y{.225}|\Y{.1}|\Y{.125}|\Y{.575}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`nTimeSteps_l2`               | PARM03    | 4                                                | number of inner timesteps to execute per timestep                                                       |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`adjdumpFreq`                 | PARM03    | 0.0                                              | interval to write model state/snapshot data adjoint run (s)                                             |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`adjMonitorFreq`              | PARM03    | 0.0                                              | interval to write monitor output adjoint run (s)                                                        |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
|  :varlink:`adTapeDir`                  | PARM05    | :kbd:`' '`                                       | if not blank, read-write checkpointing files from/to this directory name                                |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _eedata_parms:

Execution Environment Parameters
--------------------------------

If running multi-threaded (i.e., using shared
memory/`OpenMP <https://en.wikipedia.org/wiki/OpenMP>`_), you will need to set
:varlink:`nTx` and/or :varlink:`nTy` so that :varlink:`nTx`\ \*\ :varlink:`nTy`
is the total number of threads (per process).

The parameter :varlink:`useCubedSphereExchange` needs to be changed to
``.TRUE.`` if you are using any type of grid composed of interconnected
individual faces, including the cubed sphere topology or a lat-lon cap grid.
See (needs section to be written).

Note that setting flag :varlink:`debugMode` to ``.TRUE.`` activates a separate
set of debugging print statements than parameter :varlink:`debugLevel`
(see :numref:`freq_of_output`). The latter controls print statements that
monitor model activity (such as opening files, etc.), whereas the former
produces a more coding-oriented set of print statements (e.g., entering and
exiting subroutines, etc.)

.. tabularcolumns:: |\Y{.25}|\Y{.125}|\Y{.125}|\Y{.525}|

+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| Parameter                              | Group     | Default                                          | Description                                                                                             |
+========================================+===========+==================================================+=========================================================================================================+
| :varlink:`useCubedSphereExchange`      | EEPARMS   | FALSE                                            | use cubed-sphere topology domain on/off flag                                                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nTx`                         | EEPARMS   | 1                                                | number of threads in the :math:`x` direction                                                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`nTy`                         | EEPARMS   | 1                                                | number of threads in the :math:`y` direction                                                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`useCoupler`                  | EEPARMS   | FALSE                                            | communicate with other model components through a coupler on/off flag                                   |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`useSETRLSTK`                 | EEPARMS   | FALSE                                            | call C routine to set environment stacksize to ‘unlimited’                                              |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`useSIGREG`                   | EEPARMS   | FALSE                                            | enable signal handler to receive signal to terminate run cleanly on/off flag                            |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`debugMode`                   | EEPARMS   | FALSE                                            | print additional debugging messages; also “flush” STDOUT file unit after each print                     |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`printMapIncludesZeros`       | EEPARMS   | FALSE                                            | text map plots of fields should ignore exact zero values on/off flag                                    |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+
| :varlink:`maxLengthPrt1D`              | EEPARMS   | 65                                               | maximum number of 1D array elements to print to standard output                                         |
+----------------------------------------+-----------+--------------------------------------------------+---------------------------------------------------------------------------------------------------------+

.. _sec_mitgcm_inp_file_format:

MITgcm Input Data File Format
=============================

MITgcm input files for grid-related data (e.g., :varlink:`delXFile`),
forcing fields (e.g., :varlink:`tauThetaClimRelax`),
parameter fields (e.g., :varlink:`viscAhZfile`), etc. are assumed to
be in "flat" or "unblocked" `binary format <https://en.wikipedia.org/wiki/Binary_file>`_.

Data is expected to be in
`Fortran/column-major order <https://en.wikipedia.org/wiki/Row-_and_column-major_order>`_,
in the order (:math:`x`, :math:`y`, :math:`z`, :math:`t`).
`MATLAB <https://www.mathworks.com/products/matlab.html>`_ typically
uses F-order, while Python's `NumPy <https://numpy.org>`_ uses C-order (row-major order).

For historical reasons, many large MITgcm projects use big-endian
`byte ordering <https://en.wikipedia.org/wiki/Endianness>`_,
**NOT** little-endian which is the more common default for today's computers.
Thus, some care is required to create MITgcm-readable input files.  However, if
you prepare your own input files, it is perfectly fine to use little-endian so
long as you also compile your executable to be little-endian compatible.

- Using `MATLAB <https://www.mathworks.com/products/matlab.html>`_:
  When writing binary files, MATLAB's `fopen <https://www.mathworks.com/help/matlab/ref/fopen.html>`_
  command includes a MACHINEFORMAT option ``'b'`` which instructs MATLAB
  to read or write using big-endian byte ordering. 2-D arrays should be
  index-ordered in MATLAB as (:math:`x`, :math:`y`), 3-D arrays as
  (:math:`x`, :math:`y`, :math:`z`), and 4-D arrays as
  (:math:`x`, :math:`y`, :math:`z`, :math:`t`); data is ordered from low to high in
  each index, with :math:`x` varying most rapidly.

  An example to create a bathymetry file of single-precision, floating
  point values (from tutorial :ref:`sec_eg_baro`, a simple enclosed,
  flat-bottom domain) is as follows:

  ::

     ieee = 'b';           % big-endian format
     accuracy = 'float32'; % this is single-precision (='real*4')

     Ho=5000;  % ocean depth in meters
     nx=62;    % number of gridpoints in x-direction
     ny=62;    % number of gridpoints in y-direction

     % Flat bottom at z = -Ho
     h = -Ho * ones(nx, ny);

     % Walls (surrounding domain)
     h([1 end], :) = 0;   % set ocean depth to zero at east and west walls
     h(:, [1 end]) = 0;   % set ocean depth to zero at south and north walls

     % save as single-precision (float32) with big-endian byte ordering
     fid = fopen('bathy.bin', 'w', ieee);
     fwrite(fid, h, accuracy);
     fclose(fid);

  To read this bathymetry file back into MATLAB, reshaped back to (nx, ny):

  ::

     fid = fopen('bathy.bin', 'r', ieee);
     h = reshape(fread(fid, Inf, accuracy), nx, ny);
     fclose(fid);

- Using Python's `NumPy <https://numpy.org>`_:

  The `tofile <https://numpy.org/doc/stable/reference/generated/numpy.ndarray.tofile.html>`_
  method on a NumPy array writes the data in
  `row-major or C-order <https://en.wikipedia.org/wiki/Row-_and_column-major_order>`_,
  so arrays should be shaped to take this into account for the MITgcm:
  (:math:`y`, :math:`x`) for 2-D,  (:math:`z`, :math:`y`, :math:`x`) for 3-D, and
  (:math:`t`, :math:`z`, :math:`y`, :math:`x`) for 4-D.

  A python version of the above script can use NumPy to create a bathymetry file is as
  follows:

  ::

    import numpy as np

    Ho = 5000  # ocean depth in meters
    nx = 62    # number of gridpoints in x-direction
    ny = 62    # number of gridpoints in y-direction

    # Flat bottom at z = -Ho
    h = -Ho * np.ones((ny, nx))

    # Walls (surrounding domain)
    h[:, [0,-1]] = 0   # set ocean depth to zero at east and west walls
    h[[0,-1], :] = 0   # set ocean depth to zero at south and north walls

    # save as single-precision (NumPy type float32) with big-endian byte ordering
    h.astype('>f4').tofile('bathy.bin')

  The dtype specification ``'>f4'`` above instructs NumPy to write the file with
  big-endian byte ordering (specifically, due to the '>') as single-precision real
  numbers (due to the 'f4' which is NumPy ``float32`` or equivalently,
  Fortran ``real*4`` format).

  To read this bathymetry file back into NumPy, reshaped back to (ny, nx):

  ::

    h = np.fromfile('bathy.bin', '>f4').reshape(ny, nx)

  where again the dtype spec instructs NumPy to read a big-endian
  file of single-precision, floating point values.

  A more complicated example of using Python to generate input date is provided in
  :filelink:`verification/tutorial_baroclinic_gyre/input/gendata.py`.

- Using `Fortran <https://en.wikipedia.org/wiki/Fortran>`_:
  To create flat binary files in Fortran, open with
  syntax ``OPEN(..., ACCESS='DIRECT', ...)`` (i.e., **NOT** ``ACCESS='SEQUENTIAL'``
  which includes additional metadata). By default Fortran will use the
  local computer system's native byte ordering for reading and writing binary files,
  which for most systems will be little-endian. One therefore has two options:
  after creating a binary file in Fortran, use MATLAB or Python (or some
  other utility) to read in and swap the bytes in the process of writing a new file;
  or, determine if your local Fortran has
  a compiler flag to control byte-ordering of binary files.
  Similar to MATLAB, 2-D and 3-D arrays in Fortran should be index-ordered
  as (:math:`x`, :math:`y`) and (:math:`x`, :math:`y`, :math:`z`), respectively.

Using `NetCDF <http://www.unidata.ucar.edu/software/netcdf>`_ format for input files is only
partially implemented at present in MITgcm, and use is thus discouraged.

Input files are by default single-precision real numbers (32-bit, ``real*4``),
but can be switched to double precision by setting
namelist parameter :varlink:`readBinaryPrec` (``PARM01`` in file ``data``)
to a value of 64.
