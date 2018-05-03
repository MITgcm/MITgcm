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
in this chapter on how to customize the code when you are ready to try implementing 
the configuration you have in mind, in the second part (:numref:`customize_model`). 
The code and algorithm are described more fully in :numref:`discret_algorithm` and 
:numref:`sarch` and chapters thereafter. 

In this chapter and others (e.g., chapter :ref:`chap_contributing`),
for arguments where the user is expected to replace the text
with a user-chosen name, userid, etc., our convention is to show these as upper-case
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

The MITgcm code and documentation are under continuous development and we generally
recommend that one downloads the latest version of the code. You will need to decide
if you want to work in a “git-aware” environment (`Method 1`_) or with a one-time
“stagnant” download (`Method 2`_). We generally recommend method 1, as it is more
flexible and allows your version of the code to be regularly updated as MITgcm
developers check in bug fixes and new features. However, this typically requires
at minimum a rudimentary understanding of git in order to make it worth one’s while.

Periodically we release an official checkpoint (or “tag”). We recommend one download
the latest code, unless there are reasons for obtaining a specific checkpoint
(e.g. duplicating older results, collaborating with someone using an older release, etc.) 

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
you just downloaded, then type ``git checkout checkpoint«XXX»`` where ``«XXX»``
is the checkpoint version.

Alternatively, if you prefer to use ssh keys (say for example, you have
a firewall which won’t allow a https download), type:

::

    % git clone git@github.com:MITgcm/MITgcm.git

You will need a GitHub account for this, and will have to generate a ssh
key though your GitHub account user settings. 

The fully git-aware download is over several hundred MB, which is considerable
if one has limited internet download speed. In comparison, the one-time
download zip file (`Method 2`_, below) is order 100MB. However, one can
obtain a truncated, yet still git-aware copy of the current code by adding
the option ``--depth=1`` to the git clone command above; all files will be
present, but it will not include the full git history. However, the repository
can be updated going forward. 

Method 2
--------

This section describes how to do a one-time download of MITgcm, NOT git-aware.
In a terminal window, ``cd`` to the directory where you want your code to reside. 
To obtain the current code, type:

::

    % wget https://github.com/MITgcm/MITgcm/archive/master.zip

For specific checkpoint release ``XXX``, instead type: 

::

    % wget https://github.com/MITgcm/MITgcm/archive/checkpoint«XXX».zip

Updating the code
=================

There are several different approaches one can use to obtain updates to MITgcm; which is best for
you depends a bit on how you intend to use MITgcm and your knowledge of git (and/or willingness
to learn). Below we outline three suggested update pathways:

1. **Fresh Download of MITgcm**

This approach is the most simple, and virtually foolproof. Whether you downloaded the code from a static
zip file (`Method 2`_) or used the git clone command (`Method 1`_), create a new directory and repeat
this procedure to download a current copy of MITgcm. Say for example you are starting a new
research project, this would be a great time to grab the most recent code repository and keep this
new work entirely separate from any past simulations. This approach requires no understanding of git,
and you are free to make changes to any files in the MIT repo tree (although we generally recommend
that you avoid doing so, instead working in new subdirectories or on separate scratch disks as described
in :numref:`build_elsewhere`, for example). 

2. **Using** ``git pull`` **to update the (unmodified) MITgcm repo tree**

If you have downloaded the code through a git clone command (`Method 1`_ above), you can incorporate
any changes to the source code (including any changes to any files in the MITgcm repository, new packages
or analysis routines, etc.) that may have occurred since your original download. There is a simple
command to bring all code in the repository to a ‘current release’ state. From the MITgcm top directory
or any of its subdirectories, type:

::

    % git pull

and all files will be updated to match the current state of the code repository, as it exists
at `GitHub <https://github.com/MITgcm/MITgcm.git>`_. (*Note:* if you plan to contribute to
MITgcm and followed the steps to download the code as described in 
:numref:`chap_contributing`, you will need to type ``git pull upstream`` instead.)

This update pathway is ideal if you are in the midst of a project and you want to incorporate new
MITgcm features into your executable(s), or take advantage of recently added analysis utilties, etc.
After the git pull, any changes in model source code and include files will be updated, so you can
repeat the build procedure (:numref:`building_code`) and you will include all these new features
in your new executable.

Be forewarned, this will only work if you have not modified ANY of the files in the MITgcm repository
(adding new files is ok; also, all verification run subdirectories ``build`` and ``run`` are also ignored by git).
If you have modified files and the ``git pull`` fails with errors, there is no easy fix other than
to learn something about git (continue reading...)

3. **Fully embracing the power of git!**

Git offers many tools to help organize and track changes in your work.  For example, one might keep separate
projects on different branches, and update the code separately (using ``git pull``) on these separate branches.
You can even make changes to code in the MIT repo tree; when git then tries to update code from upstream
(see :numref:`git_setup`), it will notify you about possible conflicts and even merge the code changes
together if it can. You can also use ``git commit`` to help you track what you are modifying in your
simulations over time. If you're planning to submit a pull request to include your changes, you should
read the contributing guide in :numref:`chap_contributing`, and we suggest you do this model development
in a separate, fresh copy of the code. See :numref:`using_git_and_github` for more information and how
to use git effectively to manage your workflow.


Model and directory structure
=============================

The “numerical” model is contained within a execution environment
support wrapper. This wrapper is designed to provide a general framework
for grid-point models; MITgcm is a specific numerical model that makes use of
this framework (see :numref:`wrapper` for additional detail). Under this structure,
the model is split into execution
environment support code and conventional numerical model code. The
execution environment support code is held under the :filelink:`eesupp`
directory. The grid point model code is held under the :filelink:`model`
directory. Code execution actually starts in the :filelink:`eesupp` routines and
not in the :filelink:`model` routines. For this reason the top-level :filelink:`main.F <eesupp/src/main.F>`
is in the :filelink:`eesupp/src` directory. In general, end-users should not
need to worry about the wrapper support code. The top-level routine for the numerical
part of the code is in :filelink:`model/src/the_model_main.F`. Here is a brief
description of the directory structure of the model under the root tree.

-  :filelink:`model`: this directory contains the main source code. Also
   subdivided into two subdirectories: :filelink:`model/inc` (includes files) and :filelink:`model/src` (source code).

-  :filelink:`eesupp`: contains the execution environment source code. Also
   subdivided into two subdirectories: :filelink:`eesupp/inc` and :filelink:`eesupp/src`.

-  :filelink:`pkg`: contains the source code for the packages. Each package
   corresponds to a subdirectory. For example, :filelink:`pkg/gmredi` contains the
   code related to the Gent-McWilliams/Redi scheme, :filelink:`pkg/seaice` the code
   for a dynamic seaice model which can be coupled to the ocean model. The packages are
   described in detail in :numref:`packagesI` and :numref:`outp_pack`].

-  :filelink:`doc`: contains MITgcm documentation in reStructured Text (rst) format.

-  :filelink:`tools`: this directory contains various useful tools. For example,
   :filelink:`genmake2 <tools/genmake2>` is a script written in bash that should be used
   to generate your makefile. The subdirectory :filelink:`tools/build_options` contains
   ‘optfiles’ with the compiler options for many different compilers and machines
   that can run MITgcm (see :numref:`genmake2_optfiles`).
   This directory also contains subdirectories :filelink:`tools/adjoint_options` and :filelink:`tools/OAD_support`
   that are used to generate the tangent linear and adjoint model (see details
   in :numref:`chap_autodiff`).

-  :filelink:`utils`: this directory contains various utilities. The :filelink:`utils/matlab` subdirectory
   contains matlab scripts for reading model output directly into
   matlab. The subdirectory :filelink:`utils/python` contains similar routines for python.
   :filelink:`utils/scripts` contains C-shell post-processing scripts for
   joining processor-based and tiled-based model output. 

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
dependencies. We supply a script (:filelink:`genmake2 <tools/genmake2>`), described in section
:numref:`genmake2_desc`, that automatically creates the ``Makefile`` for you. You
then need to build the dependencies and compile the code.

As an example, assume that you want to build and run experiment
:filelink:`verification/exp2`. Let’s build the code in :filelink:`verification/exp2/build`:

::

    % cd verification/exp2/build

First, build the ``Makefile``:

::

    % ../../../tools/genmake2 -mods ../code -optfile «/PATH/TO/OPTFILE»

The ``-mods`` command line option tells :filelink:`genmake2 <tools/genmake2>` to override model source code
with any files in the directory ``../code/`` (for example, you will need to configure the size
of your model domain by overriding MITgcm’s default :filelink:`SIZE.h <model/inc/SIZE.h>`).

The ``-optfile`` command line option (short form,``-of``) tells :filelink:`genmake2 <tools/genmake2>`
to run the bash shell script ``«/PATH/TO/OPTFILE»`` during :filelink:`genmake2 <tools/genmake2>`’s execution. This
file contains typically definitions of environment variables,
paths, compiler options, and anything else that needs to be set in order to compile on your
local computer system or cluster with your specific Fortan compiler.
There are existing ‘optfiles’ that work with many common hardware/compiler configurations;
we first suggest you peruse the list in :filelink:`tools/build_options`
and try to find your setup. Failing that, on many systems,
the :filelink:`genmake2 <tools/genmake2>` program will be able to automatically
recognize the hardware, find compilers and other tools within the user’s
path (``echo $PATH``), and then choose an appropriate set of options
from the files (optfiles) contained in the :filelink:`tools/build_options`
directory. Under some circumstances, a user may have to create a new
optfile in order to specify the exact combination of compiler,
compiler flags, libraries, and other options necessary to build a
particular configuration of MITgcm. In such cases, it is generally
helpful to peruse the existing optfiles and mimic their syntax.
See :numref:`genmake2_optfiles`.

``-mods``, ``-optfile``, and many additional :filelink:`genmake2 <tools/genmake2>` command line options are described
more fully in :numref:`genmake_commandline`.

Once a ``Makefile`` has been generated, we create the dependencies with
the command:

::

    % make depend


It is important to note that the make depend stage will occasionally
produce warnings or errors if the dependency parsing tool is unable
to find all of the necessary header files (e.g., ``netcdf.inc``, or worse, 
say it cannot find a fortran compiler in your path). In some cases you
may need to obtain help from your system administrator to locate these files.

Next, one can compile the code using:

::

    % make

Assuming no errors occurred, the ``make`` command creates an executable called ``mitgcmuv``. 

Now you are ready to run the model. General instructions for doing so
are given in section :numref:`run_the_model`. 

.. _genmake2_desc:

Using genmake2
--------------

Many open source projects use the `GNU Autotools <https://www.gnu.org/software/automake/faq/autotools-faq.html>`_
to help streamline the build process for various unix and unix-like architectures.
For a user, the result is the common "configure" (that is,``./configure && make && make install``) commands.
For MITgcm, the process is similar. Typical commands are:

::

  % ../../../tools/genmake2 -mods../code -optfile «/PATH/TO/OPTFILE»
  % make depend
  % make


The first step in any MITgcm build is to create a unix-style ``Makefile`` which will be parsed by ``make``
to specify how to compile the MITgcm source files (for more detailed descriptions of what the make tools 
are, and how they are used, see https://www.gnu.org/software/make/make.html).
This section describes details and capabilities of :filelink:`genmake2 <tools/genmake2>` (located in the
``tools`` directory), the MITgcm tool used to generate a Makefile. :filelink:`genmake2 <tools/genmake2>` is a shell
script written to work with all “sh”–compatible shells including bash
v1, bash v2, and Bourne. Like many unix tools, there is a help option that is invoked thru ``genmake2 -h``.
:filelink:`genmake2 <tools/genmake2>` parses information from the following sources, in this order:

#.    Command-line options (see :numref:`genmake_commandline`)

#.    A ``genmake_local`` file if one is found in the current directory.
      This is a bash shell script that is executed prior to the optfile (see step #3),
      used in some special model configurations and/or to set some options that can
      affect which lines of the optfile are executed.
      An example is :filelink:`here <verification/cpl_aim+ocn/build_cpl/genmake_local>`,
      where ``genmake_local`` is required for a special setup, building a ‘MITgcm coupler’
      executable (in a more typical setup, one will not require a ``genmake_local`` file).

#.    An “options file” a.k.a. optfile (a bash shell script) specified by the command-line option
      ``–of «/PATH/TO/OPTFILE»``, as mentioned briefly in :numref:`building_quickstart`
      and in more detail in :numref:`genmake2_optfiles`.

#.    A ``packages.conf`` file (if one is found) with the specific list of
      packages to compile (see :numref:`using_packages`). The search path for file ``packages.conf`` is
      first the current directory, and then each of the ``-mods`` directories
      in the given order (see :ref:`here <mods_option>`).


When you run the :filelink:`genmake2 <tools/genmake2>` script, typical output might be as follows:

::

| % ../../../tools/genmake2 -mods ../code
| 
| GENMAKE :
| 
| A program for GENerating MAKEfiles for the MITgcm project.
|    For a quick list of options, use "genmake2 -h"
| 
| ===  Processing options files and arguments  ===
|   getting local config information:  none found
| Warning: ROOTDIR was not specified ; try using a local copy of MITgcm found at "../../.."
|   getting OPTFILE information:
| Warning: no OPTFILE specified so we'll look for possible settings
| 
| ===  Searching for possible settings for OPTFILE  ===
|   The platform appears to be:  linux_amd64
|   The possible FORTRAN compilers found in your path are:  gfortran f95
|   Setting OPTFILE to: ../../../tools/build_options/linux_amd64_gfortran
|     using OPTFILE="../../../tools/build_options/linux_amd64_gfortran"
|   getting AD_OPTFILE information:
|     using AD_OPTFILE="../../../tools/adjoint_options/adjoint_default"
|   check makedepend (local: 0, system: 1, 1)
| 
| ===  Checking system libraries  ===
|   Do we have the system() command using gfortran...  yes
|   Do we have the fdate() command using gfortran...  yes
|   Do we have the etime() command using gfortran... c,r: yes (SbR)
|   Can we call simple C routines (here, "cloc()") using gfortran...  yes
|   Can we unlimit the stack size using gfortran...  yes
|   Can we register a signal handler using gfortran...  yes
|   Can we use stat() through C calls...  yes
|   Can we create NetCDF-enabled binaries...  yes
|   Can we create LAPACK-enabled binaries...  no
|   Can we call FLUSH intrinsic subroutine...  yes
| 
| ===  Setting defaults  ===
|   Adding MODS directories: ../code 
|   Making source files in eesupp from templates
|   Making source files in pkg/exch2 from templates
|   Making source files in pkg/regrid from templates
| 
| ===  Determining package settings  ===
|   getting package dependency info from  ../../../pkg/pkg_depend
|   getting package groups info from      ../../../pkg/pkg_groups
|   checking list of packages to compile:
|     using PKG_LIST="../code/packages.conf"
|     before group expansion packages are: oceanic -kpp -gmredi cd_code
|     replacing "oceanic" with:  gfd gmredi kpp
|     replacing "gfd" with:  mom_common mom_fluxform mom_vecinv generic_advdiff debug mdsio rw monitor
|     after group expansion packages are:  mom_common mom_fluxform mom_vecinv generic_advdiff debug mdsio rw monitor gmredi kpp -kpp -gmredi cd_code
|   applying DISABLE settings
|   applying ENABLE settings
|     packages are:  cd_code debug generic_advdiff mdsio mom_common mom_fluxform mom_vecinv monitor rw
|   applying package dependency rules
|     packages are:  cd_code debug generic_advdiff mdsio mom_common mom_fluxform mom_vecinv monitor rw
|   Adding STANDARDDIRS='eesupp model'
|   Searching for *OPTIONS.h files in order to warn about the presence
|     of "#define "-type statements that are no longer allowed:
|     found CPP_OPTIONS="./CPP_OPTIONS.h"
|     found CPP_EEOPTIONS="./CPP_EEOPTIONS.h"
|   Creating the list of files for the adjoint compiler.
| 
| ===  Creating the Makefile  ===
|   setting INCLUDES
|   Determining the list of source and include files
|   Writing makefile: Makefile
|   Add the source list for AD code generation
|   Making list of "exceptions" that need ".p" files
|   Making list of NOOPTFILES
|   Add rules for links
|   Adding makedepend marker
| 
| ===  Done  ===
| 

In the above, notice:

- we did not specify ``ROOTDIR`` (see :numref:`genmake_commandline`),
  i.e., a path to your MITgcm repository, 
  but here we are building code from within the repository (specifically,
  in one of the verification subdirectory experiments); as such, 
  :filelink:`genmake2 <tools/genmake2>` was smart enough to
  locate all necessary files on its own.
- we did not specify an ``optfile`` but :filelink:`genmake2 <tools/genmake2>`
  picked one it determined was correct for our computer system
  (here, a linux 64-bit machine with gfortran installed).
- :filelink:`genmake2 <tools/genmake2>` did some simple checking on availability
  of certain system libraries; all were found except LAPACK, 
  which is only required for a few specialized packages (in other words,
  in most configurations, this ‘no’ will not block successful compilation).
  `NetCDF <http://www.unidata.ucar.edu/software/netcdf>`_ only requires a ‘yes’
  if `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ output is chosen (see :numref:`pkg_mnc`); in fact, a ‘no’ response 
  to “Can we create NetCDF-enabled binaries” will disable including  :filelink:`pkg/mnc`.
  While the makefile can still be built with other ‘no’ responses,
  sometimes this will foretell errors during the
  ``make depend`` or ``make`` commands.
- any ``.F`` or ``.h`` files in the MODS directory ``../code`` will also be compiled (overriding any MITgcm repository versions of files, if they exist)
- a handful of packages are being used in this build; see :numref:`using_packages` for more detail about how the enable/disable packages.
- :filelink:`genmake2 <tools/genmake2>` terminated without error, generating ``Makefile`` and a log file ``genmake.log``. As mentioned, this does not guarantee that
  your setup will compile properly, but if there are errors during ``make depend`` or ``make``, 
  these error messages and/or the standard output from :filelink:`genmake2 <tools/genmake2>` or
  ``genmake.log`` may provide clues as to the problem.

.. _genmake2_optfiles:

Optfiles in tools/build_options directory:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The purpose of the optfiles is to provide all the compilation options
for particular “platforms” (where “platform” roughly means the
combination of the hardware and the compiler) and code configurations.
Given the combinations of possible compilers and library dependencies
(e.g., MPI and `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_) there may be numerous optfiles available for a
single machine. The naming scheme for the majority of the optfiles
shipped with the code is **OS_HARDWARE_COMPILER** where

**OS**
    is the name of the operating system (generally the lower-case output
    of a linux terminal ``uname`` command)

**HARDWARE**
    is a string that describes the CPU type and corresponds to output
    from a ``uname -m`` command. Some common CPU types:

    ``amd64``
        use this code for x86\_64 systems (most common, including AMD and Intel 64-bit CPUs)

    ``ia64``
        is for Intel IA64 systems (eg. Itanium, Itanium2)

    ``ppc``
        is for (old) Mac PowerPC systems

**COMPILER**
    is the compiler name (generally, the name of the FORTRAN executable, e.g., ``ifort``)

In many cases, the default optfiles are sufficient and will result in
usable Makefiles. However, for some machines or code configurations, new
optfiles must be written. To create a new optfile, it is generally
best to start with one of the defaults and modify it to suit your needs.
Like :filelink:`genmake2 <tools/genmake2>`, the optfiles are all written using a simple
sh–compatible syntax. While nearly all variables used within
:filelink:`genmake2 <tools/genmake2>` may be specified in the optfiles, the critical ones that
should be defined are:

``FC``
    the Fortran compiler (executable) to use

``DEFINES``
    command-line options passed to the compiler

``CPP``
    the C pre-processor to use

``NOOPTFLAGS``
    options flags for special files that should not be optimized

For example, the optfile for a typical Red Hat Linux machine (amd64
architecture) using the GCC (g77) compiler is

::

    FC=g77
    DEFINES='-D_BYTESWAPIO -DWORDLENGTH=4'
    CPP='cpp  -traditional -P'
    NOOPTFLAGS='-O0'
    #  For IEEE, use the "-ffloat-store" option
    if test "x$IEEE" = x ; then
        FFLAGS='-Wimplicit -Wunused -Wuninitialized'
        FOPTIM='-O3 -malign-double -funroll-loops'
    else
        FFLAGS='-Wimplicit -Wunused -ffloat-store'
        FOPTIM='-O0 -malign-double'
    fi

If you write an optfile for an unrepresented machine or compiler, you
are strongly encouraged to submit the optfile to the MITgcm project for
inclusion; MITgcm developers are willing to
provide help writing or modifing optfiles.  Please submit the file through
the `GitHub issue tracker <https://github.com/MITgcm/MITgcm/issues>`_
or email the MITgcm-support@mitgcm.org list.

.. _genmake_commandline:

Command-line options:
~~~~~~~~~~~~~~~~~~~~~

In addition to the optfiles, :filelink:`genmake2 <tools/genmake2>` supports a number of helpful
command-line options. A complete list of these options can be obtained by:

::

    % genmake2 -h

The most important command-line options are:

``–optfile «/PATH/TO/FILE»``
    (= ``-of``) specifies the optfile that should be used for a particular build.

    If no optfile is specified (either through the command line or the
    ``MITGCM_OPTFILE`` environment variable), :filelink:`genmake2 <tools/genmake2>` will try to make a
    reasonable guess from the list provided in :filelink:`tools/build_options`.
    The method used for making this guess is to first determine the
    combination of operating system and hardware and
    then find a working Fortran compiler within the user’s path
    (e.g., :filelink:`tools/build_options/linux_amd64_ifort`). When
    these three items have been identified, :filelink:`genmake2 <tools/genmake2>` will try to find an
    optfile that has a matching name. See :numref:`genmake2_optfiles`.

.. _mods_option:

``–mods ’«DIR1 DIR2 DIR3 ...»’``
    specifies a list of directories containing “modifications”. These
    directories contain files with names that may (or may not) exist in
    the main MITgcm source tree but will be overridden by any
    identically-named sources within the ``-mods`` directories.
    Note the quotes around the list of directories, necessary given multiple arguments.

    The order of precedence for this “name-hiding” is as follows:

    -  “mods” directories in the order given (e.g., will use copy of file located in DIR1 instead of DIR2)

    -  Packages either explicitly specified or provided by default (in
       the order given)

    -  Packages included due to package dependencies (in the order that
       that package dependencies are parsed)

    -  The “standard dirs” (which may have been specified by the
       ``-standarddirs`` option)

``-rootdir «/PATH/TO/MITGCMDIR»``
    specify the location of the MITgcm repository top directory (``ROOTDIR``).
    By default, :filelink:`genmake2 <tools/genmake2>` will try to find this location by
    looking in parent directories from where :filelink:`genmake2 <tools/genmake2>` is executed
    (up to the 5 directory levels “higher”).


``-standarddirs «/PATH/TO/STANDARDDIR»``
    specify a path to the standard MITgcm directories for source and includes files. By default, :filelink:`model` and :filelink:`eesupp`
    directories (``src`` and ``inc``)  are the “standard dirs”. This command can be used to reset these default standard directories,
    or instead NOT include either :filelink:`model` or :filelink:`eesupp`
    as done in some specialized configurations.

``-oad``
    generates a makefile for a OpenAD build (see :numref:`ad_openad`)

``–adoptfile «/PATH/TO/FILE»``
    (= ``-adof``) specifies the “adjoint” or automatic differentiation options file to
    be used . The file is analogous to the optfile defined above but it
    specifies information for the AD build process. See :numref:`adoptfile`.

    The default file is located in
    :filelink:`tools/adjoint_options/adjoint_default` and it defines the “TAF”
    and “TAMC” compilers. As with any compiler, it is helpful to have their
    directories listed in your ``$PATH`` environment variable.

``–mpi``
    enables certain MPI features (using CPP ``#define``)
    within the code and is necessary for MPI builds (see :numref:`build_mpi`).

``–omp``
    enables OPENMP code and compiler flag ``OMPFLAG`` 

``–ieee``
    use IEEE numerics (requires support in optfile). This option is typically a good choice if one wants to compare output among different machines
    running the same code. Note using IEEE disables all compiler optimizations.

``-devel``
    use IEEE numerics and add additional compiler options to check array bounds and add other additional warning and debugging flags
    (requires support in optfile).

``–make «/PATH/TO/GMAKE»``
    due to the poor handling of soft-links and other bugs common with
    the ``make`` versions provided by commercial unix vendors, GNU
    ``make`` (sometimes called ``gmake``) may be preferred. This
    option provides a means for specifying the make executable to be
    used.

While it is possible to use :filelink:`genmake2 <tools/genmake2>` command line options to set the fortran or C compiler name (``-fc`` and ``-cc`` respectively),
we generally recommend setting these through an optfile, as discussed in :numref:`genmake2_optfiles`.
Other :filelink:`genmake2 <tools/genmake2>` options  are available to
enable performance/timing analyses, etc.; see ``genmake2 -h`` for more info.


``make`` commands
~~~~~~~~~~~~~~~~~

Following a successful build of ``Makefile``, type ``make depend``. This command
modifies the ``Makefile`` by attaching a (usually, long) list of
files upon which other files depend. The purpose of this is to reduce
re-compilation if and when you start to modify the code. The ``make depend``
command also creates links from the model source to this directory, except for links to those files 
in the specified ``-mods`` directory. The  links
that exist at this stage are mostly “large F” files (``*.F`` and ``*.F90``) that need to be processed by a C preprocessor (``CPP``).
**IMPORTANT NOTE:** Editing the source code files in the build directory
will not edit a local copy (since these are just links) but will
edit the original files in :filelink:`model/src` (or :filelink:`model/inc`)
or in the specified ``-mods`` directory. While the latter might
be what you intend, editing the master copy in :filelink:`model/src`
is usually **NOT** what was intended and may cause grief somewhere down the road.
Rather, if you need to add 
to the list of modified source code files, place a copy of
the file(s) to edit in the ``-mods`` directory, make the edits to
these ``-mods`` directory files, go back to the build directory and type ``make Clean``,
and then re-build the makefile (these latter steps critical or the makefile will not 
link to to this newly edited file).

The final ``make`` invokes the C preprocessor to produce the “little f” files (``*.f`` and ``*.f90``) and then compiles them
to object code using the specified Fortran compiler and options. An intermediate step occurs during this
stage to further process custom definitions (i.e., make simple substitutions) such as variable types within the source
files. This additional stage is necessary in order to overcome some of the inconsistencies in the sizes of objects (bytes)
between different compilers. The result of the build process is an executable with the name
``mitgcmuv``.

Additional make “targets” are defined within the makefile to aid in the production
of adjoint (:numref:`building_adcode_using_taf`) and other versions of MITgcm. On computers with multiple processor cores
or shared multi-processor (a.k.a. SMP) systems, the build process can often be sped
up appreciably using the command:

::

    % make -j 2

where the “2” can be replaced with a number that corresponds to the
number of cores (or discrete CPUs) available.


In addition, there are several housekeeping ``make clean`` options that might be useful:

- ``make clean`` removes files that ``make`` generates (e.g., \*.o and \*.f files)
- ``make Clean`` removes files and links generated by ``make`` and ``make depend``; strongly recommended for “un-clean” directories which may contain
  the (perhaps partial) results of previous builds
- ``make CLEAN`` removes pretty much everything, including any executables and output from :filelink:`genmake2 <tools/genmake2>`



.. _build_elsewhere:

Building/compiling the code elsewhere
-------------------------------------

In the quickstart example above (:numref:`building_quickstart`) we built the
executable in the ``build`` directory of the experiment.
Model object files and output data can use up large amounts of disk
space so it is often preferable to operate on a large
scratch disk. Here, we show how to configure and compile the code on a scratch disk,
without having to copy the entire source
tree. The only requirement to do so is you have :filelink:`genmake2 <tools/genmake2>` in your path, or
you know the absolute path to :filelink:`genmake2 <tools/genmake2>`.

Assuming the model source is in ``~/MITgcm``, then the
following commands will build the model in ``/scratch/exp2-run1``:

::

    % cd /scratch/exp2-run1
    % ~/MITgcm/tools/genmake2 -rootdir ~/MITgcm -mods ~/MITgcm/verification/exp2/code
    % make depend
    % make

Note the use of the command line option ``-rootdir`` to tell :filelink:`genmake2 <tools/genmake2>` where to find the MITgcm directory tree.
In general, one can compile the code in any given directory by following this procedure.

.. _build_mpi:

Building  with MPI
------------------

Building MITgcm to use MPI libraries can be complicated due to the
variety of different MPI implementations available, their dependencies
or interactions with different compilers, and their often ad-hoc
locations within file systems. For these reasons, its generally a good
idea to start by finding and reading the documentation for your
machine(s) and, if necessary, seeking help from your local systems
administrator.

The steps for building MITgcm with MPI support are:

#. Determine the locations of your MPI-enabled compiler and/or MPI
   libraries and put them into an options file as described in :numref:`genmake2_optfiles`. 
   One can start with one of the examples in
   :filelink:`tools/build_options`
   such as :filelink:`tools/build_options/linux_amd64_gfortran`
   or :filelink:`tools/build_options/linux_amd64_ifort+impi` and
   then edit it to suit the machine at hand. You may need help from your
   user guide or local systems administrator to determine the exact
   location of the MPI libraries. If libraries are not installed, MPI
   implementations and related tools are available including:

   -  `Open MPI <https://www.open-mpi.org/>`_ 

   -  `MVAPICH2 <http:mvapich.cse.ohio-state.edu/>`_

   -  `MPICH <https://www.mpich.org/>`_

   -  `Intel MPI <https://software.intel.com/en-us/intel-mpi-library/>`_

  
#. Build the code with the :filelink:`genmake2 <tools/genmake2>` ``-mpi`` option (see :numref:`genmake_commandline`)
   using commands such as:

   ::

         %  ../../../tools/genmake2 -mods=../code -mpi -of=«YOUR_OPTFILE»
         %  make depend
         %  make


.. _run_the_model:

Running the model 
=================

If compilation finished successfully (:numref:`building_code`) then an
executable called ``mitgcmuv`` will now exist in the local (``build``) directory.

To run the model as a single process (i.e., not in parallel) simply
type (assuming you are still in the ``build`` directory):

::

    % cd ../run
    % ln -s ../input/* .
    % cp ../build/mitgcmuv .
    % ./mitgcmuv

Here, we are making a link to all the support data files (in ``../input/``) needed by the MITgcm
for this experiment, and then copying the executable from the the build directory.
The ``./`` in the last step is a safe-guard to make sure you use the local executable in
case you have others that might exist in your ``$PATH``.
The above command will spew out many lines of text output to your
screen. This output contains details such as parameter values as well as
diagnostics such as mean kinetic energy, largest CFL number, etc. It is
worth keeping this text output with the binary output so we normally
re-direct the ``stdout`` stream as follows:

::

    % ./mitgcmuv > output.txt

In the event that the model encounters an error and stops, it is very
helpful to include the last few line of this ``output.txt`` file along
with the (``stderr``) error message within any bug reports.

For the example experiment in :filelink:`verification/exp2`, an example of the
output is kept in :filelink:`verification/exp2/results/output.txt` for comparison. You can compare
your ``output.txt`` with the corresponding one for that experiment to
check that your set-up indeed works. Congratulations!


.. _running_mpi:

Running with MPI
----------------

Run the code with the appropriate MPI “run” or “exec” program
provided with your particular implementation of MPI. Typical MPI
packages such as `Open MPI <https://www.open-mpi.org/>`_ will use something like:

   ::

         %  mpirun -np 4 ./mitgcmuv

Sightly more complicated scripts may be needed for many machines
since execution of the code may be controlled by both the MPI library
and a job scheduling and queueing system such as `Slurm <https://slurm.schedmd.com/>`_,
`PBS/TORQUE <http://www.adaptivecomputing.com/products/open-source/torque>`_,
`LoadLeveler <https://www-03.ibm.com/systems/power/software/loadleveler/>`_,
or any of a number of similar tools. See your local cluster documentation 
or system administrator for the specific syntax required to run on your computing facility.


Output files
------------

The model produces various output files and, when using :filelink:`pkg/mnc` (i.e., netCDF),
sometimes even directories. Depending upon the I/O package(s) selected
at compile time (either :filelink:`pkg/mdsio`, :filelink:`pkg/mnc`, or both as determined by
``packages.conf``) and the run-time flags set (in
``data.pkg``), the following output may appear. More complete information describing output files
and model diagnostics is described in :numref:`outp_pack`.

Raw binary output files
~~~~~~~~~~~~~~~~~~~~~~~

The “traditional” output files are generated by the :filelink:`pkg/mdsio` 
(see :numref:`pkg_mdsio`).The :filelink:`pkg/mdsio` model data are written according to a
“meta/data” file format. Each variable is associated with two files with
suffix names ``.data`` and ``.meta``. The ``.data`` file contains the
data written in binary form (big endian by default). The ``.meta`` file
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
   :math:`^{\circ}\mathrm{C}`, atmosphere: :math:`^{\circ}\mathrm{K}`).

-  ``S.00000nIter`` - ocean: salinity (psu), atmosphere: water vapor
   (g/kg).

-  ``Eta.00000nIter`` - ocean: surface elevation (m), atmosphere:
   surface pressure anomaly (Pa).

The chain ``00000nIter`` consists of ten figures that specify the
iteration number at which the output is written out. For example,
``U.0000000300`` is the zonal velocity at iteration 300.

In addition, a “pickup” or “checkpoint” file called:

-  ``pickup.00000nIter``

is written out. This file represents the state of the model in a
condensed form and is used for restarting the integration (at the specific iteration number).
Some additional parameterizations and packages also produce separate pickup files, e.g.,

-  ``pickup_cd.00000nIter`` if the C-D scheme is used (see :ref:`C_D Scheme <C-D_scheme>`)

-  ``pickup_seaice.00000nIter`` if the seaice package is turned on (see :ref:`sub_phys_pkg_seaice`)

-  ``pickup_ptracers.00000nIter`` if passive tracers are included in the simulation (see :ref:`sub_phys_pkg_ptracers`)


Rolling checkpoint files are
the same as the pickup files but are named differently. Their name
contain the chain ``ckptA`` or ``ckptB`` instead of ``00000nIter``. They
can be used to restart the model but are overwritten every other time
they are output to save disk space during long integrations.

NetCDF output files
~~~~~~~~~~~~~~~~~~~

:filelink:`pkg/mnc` is a set of routines written to read, write, and
append `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ files.
Unlike the :filelink:`pkg/mdsio` output, the :filelink:`pkg/mnc`–generated output is usually
placed within a subdirectory with a name such as ``mnc_output_`` (by default,
`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ tries to append, rather than overwrite, existing files,
so a unique output directory is helpful for each separate run).


The :filelink:`pkg/mnc` output files are all in the “self-describing” `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ format and
can thus be browsed and/or plotted using tools such as:

-  `ncdump <https://www.unidata.ucar.edu/software/netcdf/netcdf-4/newdocs/netcdf/ncdump.html>`_
   is a utility which is typically included with every `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
   install, and converts the `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ binaries into formatted ASCII text files.

-  `ncview <http://meteora.ucsd.edu/~pierce/ncview_home_page.html>`_
   is a very convenient and quick way to plot `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_
   data and it runs on most platforms. `Panoply <https://www.giss.nasa.gov/tools/panoply/>`_ is a similar alternative.

-  `MATLAB <https://www.mathworks.com/>`_, `GrADS <http://cola.gmu.edu/grads/>`_, 
   `IDL <http://www.harrisgeospatial.com/SoftwareTechnology/IDL.aspx>`_ and other common post-processing environments provide
   built-in `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ interfaces.



Looking at the output
---------------------

MATLAB
~~~~~~

Raw binary output
^^^^^^^^^^^^^^^^^

The repository includes a few `MATLAB <https://www.mathworks.com/>`_ utilities to read binary output
files written in the :filelink:`/pkg/mdsio` format. The `MATLAB <https://www.mathworks.com/>`_ scripts are located in the
directory :filelink:`utils/matlab` under the root tree. The script :filelink:`utils/matlab/rdmds.m`
reads the data. Look at the comments inside the script to see how to use it.

Some examples of reading and visualizing some output in `Matlab <https://www.mathworks.com/>`_:

::

    % matlab
    >> H=rdmds('Depth');
    >> contourf(H');colorbar;
    >> title('Depth of fluid as used by model');

    >> eta=rdmds('Eta',10);
    >> imagesc(eta');axis ij;colorbar;
    >> title('Surface height at iter=10');

    >> eta=rdmds('Eta',[0:10:100]);
    >> for n=1:11; imagesc(eta(:,:,n)');axis ij;colorbar;pause(.5);end


NetCDF output 
^^^^^^^^^^^^^

Similar scripts for `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ output (e.g., :filelink:`utils/matlab/rdmnc.m`) are available and they
are described in :numref:`pkg_mnc`.

Python
~~~~~~

Raw binary output
^^^^^^^^^^^^^^^^^

The repository includes `Python <https://www.python.org/>`_ scripts
for reading binary :filelink:`/pkg/mdsio` format under :filelink:`utils/python`.
The following example shows how to load in some data:

::
  
    # python
    import mds

    Eta = mds.rdmds('Eta', itrs=10)

The docstring for ``mds.rdmds`` (see file :filelink:`utils/python/MITgcmutils/MITgcmutils/mds.py`)
contains much more detail about using this function and the options that it takes.

NetCDF output 
^^^^^^^^^^^^^

The `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ output
is currently produced with one file per processor. This means the individual tiles
need to be stitched together to create a single
`netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ file that spans the model domain. The script
:filelink:`utils/python/MITgcmutils/scripts/gluemncbig` can do
this efficiently from the command line. 

The following example shows how to use the `xarray python package <http://xarray.pydata.org/>`_ to read
the resulting `netCDF <http://www.unidata.ucar.edu/software/netcdf>`_ file into `Python <https://www.python.org/>`_:


::
  
  # python
  import xarray as xr

  Eta = xr.open_dataset('Eta.nc')

.. _customize_model:

Customizing the model configuration
===================================

When you are ready to run the model in the configuration you want, the
easiest thing is to use and adapt the setup of the case studies
experiment (described in :numref:`chap_modelExamples`) that is the closest to your
configuration. Then, the amount of setup will be minimized. In this
section, we focus on the setup relative to the “numerical model” part of
the code (the setup relative to the “execution environment” part is
covered in the software architecture/wrapper section) and on the variables and
parameters that you are likely to change.


In what follows, the parameters are grouped into categories related to
the computational domain, the equations solved in the model, and the
simulation controls.


Parameters: Computational Domain, Geometry and Time-Discretization
------------------------------------------------------------------

Dimensions
     

    The number of points in the x, y, and r directions are represented
    by the variables :varlink:`sNx`, :varlink:`sNy` and :varlink:`Nr` respectively which are
    declared and set in the file :filelink:`SIZE.h <model/inc/SIZE.h>`. (Again, this
    assumes a mono-processor calculation. For multiprocessor
    calculations see the section on parallel implementation.)

Grid
     

    Three different grids are available: cartesian, spherical polar, and
    curvilinear (which includes the cubed sphere). The grid is set
    through the logical variables :varlink:`usingCartesianGrid`,
    :varlink:`usingSphericalPolarGrid`, and :varlink:`usingCurvilinearGrid`. In the
    case of spherical and curvilinear grids, the southern boundary is
    defined through the variable :varlink:`ygOrigin` which corresponds to the
    latitude of the southern most cell face (in degrees). The resolution
    along the x and y directions is controlled by the 1D arrays :varlink:`delx`
    and :varlink:`dely` (in meters in the case of a cartesian grid, in degrees
    otherwise). The vertical grid spacing is set through the 1D array
    :varlink:`delz` for the ocean (in meters) or :varlink:`delp` for the atmosphere
    (in Pa). The variable :varlink:`Ro_SeaLevel` represents the standard
    position of sea level in “r” coordinate. This is typically set to 0 m
    for the ocean (default value) and 10\ :sup:`5` Pa for the
    atmosphere. For the atmosphere, also set the logical variable
    :varlink:`groundAtK1` to ``.TRUE.`` which puts the first level (k=1) at
    the lower boundary (ground).

    For the cartesian grid case, the Coriolis parameter :math:`f` is set
    through the variables :varlink:`f0` and :varlink:`beta` which correspond to the
    reference Coriolis parameter (in s\ :sup:`--1`) and
    :math:`\frac{\partial f}{ \partial y}`\ (in
    m\ :sup:`--1`\ s\ :sup:`--1`) respectively. If :varlink:`beta` is set
    to a nonzero value, :varlink:`f0` is the value of :math:`f` at the southern
    edge of the domain.

Topography - Full and Partial Cells
     

    The domain bathymetry is read from a file that contains a 2D (x,y)
    map of depths (in m) for the ocean or pressures (in Pa) for the
    atmosphere. The file name is represented by the variable
    :varlink:`bathyFile`. The file is assumed to contain binary numbers giving
    the depth (pressure) of the model at each grid cell, ordered with
    the x coordinate varying fastest. The points are ordered from low
    coordinate to high coordinate for both axes. The model code applies
    without modification to enclosed, periodic, and double periodic
    domains. Periodicity is assumed by default and is suppressed by
    setting the depths to 0 m for the cells at the limits of the
    computational domain (note: not sure this is the case for the
    atmosphere). The precision with which to read the binary data is
    controlled by the integer variable :varlink:`readBinaryPrec` which can take
    the value 32 (single precision) or 64 (double precision).
    See the matlab program ``gendata.m`` in the ``input`` directories of
    ``verification`` for several tutorial examples (e.g., :filelink:`gendata.m <verification/tutorial_barotropic_gyre/input/gendata.m>`
    in the :ref:`barotropic gyre tutorial <sec_eg_baro>`)
    to see how the bathymetry files are generated for the
    case study experiments.

    To use the partial cell capability, the variable :varlink:`hFacMin` needs
    to be set to a value between 0 and 1 (it is set to 1 by default)
    corresponding to the minimum fractional size of the cell. For
    example if the bottom cell is 500 m thick and :varlink:`hFacMin` is set to
    0.1, the actual thickness of the cell (i.e. used in the code) can
    cover a range of discrete values 50 m apart from 50 m to 500 m
    depending on the value of the bottom depth (in :varlink:`bathyFile`) at
    this point.

    Note that the bottom depths (or pressures) need not coincide with
    the models levels as deduced from :varlink:`delz` or :varlink:`delp`. The model
    will interpolate the numbers in :varlink:`bathyFile` so that they match the
    levels obtained from :varlink:`delz` or :varlink:`delp` and :varlink:`hFacMin`.

    (Note: the atmospheric case is a bit more complicated than what is
    written here. To come soon...)

Time-Discretization
     

    The time steps are set through the real variables :varlink:`deltaTMom` and
    :varlink:`deltaTtracer` (in s) which represent the time step for the
    momentum and tracer equations, respectively. For synchronous
    integrations, simply set the two variables to the same value (or you
    can prescribe one time step only through the variable :varlink:`deltaT`).
    The Adams-Bashforth stabilizing parameter is set through the
    variable :varlink:`abEps` (dimensionless). The stagger baroclinic time
    stepping can be activated by setting the logical variable
    :varlink:`staggerTimeStep` to ``.TRUE.``.

.. _parms-eos:

Parameters: Equation of State
-----------------------------

First, because the model equations are written in terms of
perturbations, a reference thermodynamic state needs to be specified.
This is done through the 1D arrays :varlink:`tRef` and :varlink:`sRef`. :varlink:`tRef`
specifies the reference potential temperature profile (in
:sup:`o`\ C for the ocean and K for the atmosphere)
starting from the level k=1. Similarly, :varlink:`sRef` specifies the reference
salinity profile (in ppt) for the ocean or the reference specific
humidity profile (in g/kg) for the atmosphere.

The form of the equation of state is controlled by the character
variables :varlink:`buoyancyRelation` and :varlink:`eosType`. :varlink:`buoyancyRelation` is
set to ``OCEANIC`` by default and needs to be set to ``ATMOSPHERIC``
for atmosphere simulations. In this case, :varlink:`eosType` must be set to
``IDEALGAS``. For the ocean, two forms of the equation of state are
available: linear (set :varlink:`eosType` to ``LINEAR``) and a polynomial
approximation to the full nonlinear equation ( set :varlink:`eosType` to
``POLYNOMIAL``). In the linear case, you need to specify the thermal
and haline expansion coefficients represented by the variables
:varlink:`tAlpha` (in K\ :sup:`--1`) and :varlink:`sBeta` (in ppt\ :sup:`--1`).
For the nonlinear case, you need to generate a file of polynomial
coefficients called ``POLY3.COEFFS``. To do this, use the program
:filelink:`utils/knudsen2/knudsen2.f` under the model tree (a Makefile is
available in the same directory and you will need to edit the number and
the values of the vertical levels in :filelink:`knudsen2.f <utils/knudsen2/knudsen2.f>` so that they match
those of your configuration).

There there are also higher polynomials for the equation of state:

``’UNESCO’``:
    The UNESCO equation of state formula of Fofonoff and Millard (1983)
    :cite:`fofonoff:83`. This equation of state assumes
    in-situ temperature, which is not a model variable; *its use is
    therefore discouraged, and it is only listed for completeness*.

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
    The new, more accurate and less expensive equation of state by
    McDougall et al. (1983) :cite:`mcdougall:03`. It also requires
    lagging the pressure and therefore an additional pickup file for
    restarts.

For none of these options an reference profile of temperature or
salinity is required.


Parameters: Momentum Equations
------------------------------

In this section, we only focus for now on the parameters that you are
likely to change, i.e. the ones relative to forcing and dissipation for
example. The details relevant to the vector-invariant form of the
equations and the various advection schemes are not covered for the
moment. We assume that you use the standard form of the momentum
equations (i.e. the flux-form) with the default advection scheme. Also,
there are a few logical variables that allow you to turn on/off various
terms in the momentum equation. These variables are called
:varlink:`momViscosity`, :varlink:`momAdvection`, :varlink:`momForcing`, :varlink:`useCoriolis`,
:varlink:`momPressureForcing`, :varlink:`momStepping` and :varlink:`metricTerms` and are assumed to
be set to ``.TRUE.`` here. Look at the file :filelink:`PARAMS.h <model/inc/PARAMS.h>` for a
precise definition of these variables.

Initialization
     

    The initial horizontal velocity components can be specified from
    binary files :varlink:`uVelInitFile` and :varlink:`vVelInitFile`. These files
    should contain 3D data ordered in an (x,y,r) fashion with k=1 as the
    first vertical level (surface level). If no file names are provided,
    the velocity is initialized to zero. The initial vertical velocity
    is always derived from the horizontal velocity using the continuity
    equation, even in the case of non-hydrostatic simulation (see, e.g.,
    :filelink:`verification/tutorial_deep_convection/input/`).

    In the case of a restart (from the end of a previous simulation),
    the velocity field is read from a pickup file (see section on
    simulation control parameters) and the initial velocity files are
    ignored.

Forcing

    
    This section only applies to the ocean. You need to generate
    wind-stress data into two files :varlink:`zonalWindFile` and
    :varlink:`meridWindFile` corresponding to the zonal and meridional
    components of the wind stress, respectively (if you want the stress
    to be along the direction of only one of the model horizontal axes,
    you only need to generate one file). The format of the files is
    similar to the bathymetry file. The zonal (meridional) stress data
    are assumed to be in Pa and located at U-points (V-points). As for
    the bathymetry, the precision with which to read the binary data is
    controlled by the variable :varlink:`readBinaryPrec`. See the matlab
    program ``gendata.m`` in the ``input`` directories of
    ``verification`` for several tutorial example
    (e.g. :filelink:`gendata.m <verification/tutorial_barotropic_gyre/input/gendata.m>`
    in the :ref:`barotropic gyre tutorial <sec_eg_baro>`)
    to see how simple analytical wind forcing data are generated for the
    case study experiments.

.. _periodic_forcing_expl:

    There is also the possibility of prescribing time-dependent periodic
    forcing. To do this, concatenate the successive time records into a
    single file (for each stress component) ordered in a (x,y,t) fashion
    and set the following variables: :varlink:`periodicExternalForcing` to
    ``.TRUE.``, :varlink:`externForcingPeriod` to the period (in s) of which
    the forcing varies (typically 1 month), and :varlink:`externForcingCycle`
    to the repeat time (in s) of the forcing (typically 1 year; note
    :varlink:`externForcingCycle` must be a multiple of
    :varlink:`externForcingPeriod`). With these variables set up, the model
    will interpolate the forcing linearly at each iteration.

.. _mom_dissip:

Dissipation

    
    The lateral eddy viscosity coefficient is specified through the
    variable :varlink:`viscAh` (in m\ :sup:`2`\ s\ :sup:`--1`). The
    vertical eddy viscosity coefficient is specified through the
    variable :varlink:`viscAz` (in m\ :sup:`2`\ s\ :sup:`--1`) for the
    ocean and :varlink:`viscAp` (in Pa\ :sup:`2`\ s\ :sup:`--1`) for the
    atmosphere. The vertical diffusive fluxes can be computed implicitly
    by setting the logical variable :varlink:`implicitViscosity` to
    ``.TRUE.``. In addition, biharmonic mixing can be added as well
    through the variable :varlink:`viscA4` (in
    m\ :sup:`4`\ s\ :sup:`--1`). On a spherical polar grid, you
    might also need to set the variable :varlink:`cosPower` which is set to 0
    by default and which represents the power of cosine of latitude to
    multiply viscosity. Slip or no-slip conditions at lateral and bottom
    boundaries are specified through the logical variables
    :varlink:`no_slip_sides` and :varlink:`no_slip_bottom`. If set to
    ``.FALSE.``, free-slip boundary conditions are applied. If no-slip
    boundary conditions are applied at the bottom, a bottom drag can be
    applied as well. Two forms are available: linear (set the variable
    :varlink:`bottomDragLinear` in m/s) and quadratic (set the variable
    :varlink:`bottomDragQuadratic`, dimensionless).

    The Fourier and Shapiro filters are described elsewhere.

.. _C-D_scheme:

C-D Scheme
     

    If you run at a sufficiently coarse resolution, you will need the
    C-D scheme for the computation of the Coriolis terms. The
    variable :varlink:`tauCD`, which represents the C-D scheme coupling
    timescale (in s) needs to be set.

Calculation of Pressure/Geopotential
     

    First, to run a non-hydrostatic ocean simulation, set the logical
    variable :varlink:`nonHydrostatic` to ``.TRUE.``. The pressure field is
    then inverted through a 3D elliptic equation. (Note: this capability
    is not available for the atmosphere yet.) By default, a hydrostatic
    simulation is assumed and a 2D elliptic equation is used to invert
    the pressure field. The parameters controlling the behavior of the
    elliptic solvers are the variables :varlink:`cg2dMaxIters` and
    :varlink:`cg2dTargetResidual` for the 2D case and :varlink:`cg3dMaxIters` and
    :varlink:`cg3dTargetResidual` for the 3D case. You probably won’t need to
    alter the default values (are we sure of this?).

    For the calculation of the surface pressure (for the ocean) or
    surface geopotential (for the atmosphere) you need to set the
    logical variables :varlink:`rigidLid` and :varlink:`implicitFreeSurface` (set one
    to ``.TRUE.`` and the other to ``.FALSE.`` depending on how you
    want to deal with the ocean upper or atmosphere lower boundary).

Parameters: Tracer Equations
----------------------------

This section covers the tracer equations i.e. the potential temperature
equation and the salinity (for the ocean) or specific humidity (for the
atmosphere) equation. As for the momentum equations, we only describe
for now the parameters that you are likely to change. The logical
variables :varlink:`tempDiffusion`, :varlink:`tempAdvection`, :varlink:`tempForcing`, and
:varlink:`tempStepping` allow you to turn on/off terms in the temperature
equation (same thing for salinity or specific humidity with variables
:varlink:`saltDiffusion`, :varlink:`saltAdvection` etc.). These variables are all
assumed here to be set to ``.TRUE.``. Look at file
:filelink:`PARAMS.h <model/inc/PARAMS.h>` for a precise definition.

Initialization
     

    The initial tracer data can be contained in the binary files
    :varlink:`hydrogThetaFile` and :varlink:`hydrogSaltFile`. These files should
    contain 3D data ordered in an (x,y,r) fashion with k=1 as the first
    vertical level. If no file names are provided, the tracers are then
    initialized with the values of :varlink:`tRef` and :varlink:`sRef` mentioned :ref:`above <parms-eos>`.
    In this case, the initial tracer
    data are uniform in x and y for each depth level.

Forcing
     

    This part is more relevant for the ocean, the procedure for the
    atmosphere not being completely stabilized at the moment.

    A combination of fluxes data and relaxation terms can be used for
    driving the tracer equations. For potential temperature, heat flux
    data (in W/m\ :sup:`2`) can be stored in the 2D binary file
    :varlink:`surfQfile`. Alternatively or in addition, the forcing can be
    specified through a relaxation term. The SST data to which the model
    surface temperatures are restored to are supposed to be stored in
    the 2D binary file :varlink:`thetaClimFile`. The corresponding relaxation
    time scale coefficient is set through the variable
    :varlink:`tauThetaClimRelax` (in s). The same procedure applies for
    salinity with the variable names :varlink:`EmPmRfile`, :varlink:`saltClimFile`,
    and :varlink:`tauSaltClimRelax` for freshwater flux (in m/s) and surface
    salinity (in ppt) data files and relaxation time scale coefficient
    (in s), respectively. Also for salinity, if the CPP key
    ``USE_NATURAL_BCS`` is turned on, natural boundary conditions are
    applied, i.e., when computing the surface salinity tendency, the
    freshwater flux is multiplied by the model surface salinity instead
    of a constant salinity value.

    As for the other input files, the precision with which to read the
    data is controlled by the variable :varlink:`readBinaryPrec`.
    Time-dependent, periodic forcing can be applied as well following
    the same procedure used for the wind forcing data (see :ref:`above <periodic_forcing_expl>`).

Dissipation
     

    Lateral eddy diffusivities for temperature and salinity/specific
    humidity are specified through the variables :varlink:`diffKhT` and
    :varlink:`diffKhS` (in m\ :sup:`2`\ /s). Vertical eddy diffusivities are
    specified through the variables :varlink:`diffKzT` and :varlink:`diffKzS` (in
    m\ :sup:`2`\ /s) for the ocean and :varlink:`diffKpT` and :varlink:`diffKpS` (in
    Pa\ :sup:`2`\ /s) for the atmosphere. The vertical diffusive
    fluxes can be computed implicitly by setting the logical variable
    :varlink:`implicitDiffusion` to ``.TRUE.``. In addition, biharmonic
    diffusivities can be specified as well through the coefficients
    :varlink:`diffK4T` and :varlink:`diffK4S` (in m\ :sup:`4`\ /s). Note that the
    cosine power scaling (specified through :varlink:`cosPower`; see :ref:`above <mom_dissip>`)
    is applied to the tracer diffusivities
    (Laplacian and biharmonic) as well. The Gent and McWilliams
    parameterization for oceanic tracers is described in the package
    section. Finally, note that tracers can be also subject to Fourier
    and Shapiro filtering (see the corresponding section on these
    filters).

Ocean convection
     

    Two options are available to parameterize ocean convection.
    To use the first option, a convective adjustment scheme, you need to
    set the variable :varlink:`cadjFreq`, which represents the frequency (in s)
    with which the adjustment algorithm is called, to a non-zero value
    (note, if :varlink:`cadjFreq` set to a negative value by the user, the model will set it to
    the tracer time step). The second option is to parameterize
    convection with implicit vertical diffusion. To do this, set the
    logical variable :varlink:`implicitDiffusion` to ``.TRUE.`` and the real
    variable :varlink:`ivdc_kappa` to a value (in m\ :sup:`2`\ /s) you wish
    the tracer vertical diffusivities to have when mixing tracers
    vertically due to static instabilities. Note that :varlink:`cadjFreq` and
    :varlink:`ivdc_kappa` cannot both have non-zero value.

Parameters: Simulation Controls
-------------------------------

The model ”clock” is defined by the variable :varlink:`deltaTClock` (in s)
which determines the I/O frequencies and is used in tagging output.
Typically, you will set it to the tracer time step for accelerated runs
(otherwise it is simply set to the default time step :varlink:`deltaT`).
Frequency of checkpointing and dumping of the model state are referenced
to this clock (see :ref:`below <freq_of_output>`).

Run Duration
     

    The beginning of a simulation is set by specifying a start time (in s)
    through the real variable :varlink:`startTime` or by specifying an
    initial iteration number through the integer variable :varlink:`nIter0`. If
    these variables are set to nonzero values, the model will look for a
    ”pickup” file ``pickup.0000nIter0`` to restart the integration. The
    end of a simulation is set through the real variable :varlink:`endTime` (in s).
    Alternatively, you can specify instead the number of time steps
    to execute through the integer variable :varlink:`nTimeSteps`.

.. _freq_of_output:

Frequency of Output

    Real variables defining frequencies (in s) with which output files
    are written on disk need to be set up. :varlink:`dumpFreq` controls the
    frequency with which the instantaneous state of the model is saved.
    :varlink:`chkPtFreq` and :varlink:`pchkPtFreq` control the output frequency of
    rolling and permanent checkpoint files, respectively. In addition, time-averaged fields can be written out by
    setting the variable :varlink:`taveFreq` (in s). The precision with which
    to write the binary data is controlled by the integer variable
    :varlink:`writeBinaryPrec` (set it to 32 or 64).


Parameters: Default Values
--------------------------

The CPP keys relative to the “numerical model” part of the code are all
defined and set in the file :filelink:`CPP_OPTIONS.h <model/inc/CPP_OPTIONS.h>` in the directory
:filelink:`model/inc/` or in one of the ``code`` directories of the case study
experiments under :filelink:`verification/`. The model parameters are defined and
declared in the file :filelink:`PARAMS.h <model/inc/PARAMS.h>` and their default values are
set in the routine :filelink:`set_defaults.F <model/src/set_defaults.F>`. The default values can
be modified in the namelist file ``data`` which needs to be located in the
directory where you will run the model. The parameters are initialized
in the routine :filelink:`ini_parms.F <model/src/ini_parms.F>`. Look at this routine to see in
what part of the namelist the parameters are located. Here is a complete
list of the model parameters related to the main model (namelist
parameters for the packages are located in the package descriptions),
their meaning, and their default values:

+--------------------------------+---------------------+--------------------------------------------------------------------+
| **Name**                       | **Value**           | **Description**                                                    |
+--------------------------------+---------------------+--------------------------------------------------------------------+
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`buoyancyRelation`    | OCEANIC             | buoyancy relation                                                  |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`fluidIsAir`          | F                   | fluid major constituent is air                                     |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`fluidIsWater`        | T                   | fluid major constituent is water                                   |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`usingPCoords`        | F                   | use pressure coordinates                                           |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`usingZCoords`        | T                   | use z-coordinates                                                  |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`tRef`                | 2.0E+01 at k=top    | reference temperature profile ( :sup:`o`\ C or K )                 |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`sRef`                | 3.0E+01 at k=top    | reference salinity profile ( psu )                                 |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscAh`              | 0.0E+00             | lateral eddy viscosity ( m\ :sup:`2`\ /s )                         |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscAhMax`           | 1.0E+21             | maximum lateral eddy viscosity ( m\ :sup:`2`\ /s )                 |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscAhGrid`          | 0.0E+00             | grid dependent lateral eddy viscosity ( non-dim. )                 |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`useFullLeith`        | F                   | use full form of Leith viscosity on/off flag                       |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`useStrainTensionVisc`| F                   | use StrainTension form of viscous operator on/off flag             |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`useAreaViscLength`   | F                   | use area for visc length instead of geom. mean                     |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscC2leith`         | 0.0E+00             | Leith harmonic visc. factor (on grad(vort),non-dim.)               |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscC2leithD`        | 0.0E+00             | Leith harmonic viscosity factor (on grad(div),non-dim.)            |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscC2smag`          | 0.0E+00             | Smagorinsky harmonic viscosity factor (non-dim.)                   |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscA4`              | 0.0E+00             | lateral biharmonic viscosity ( m\ :sup:`4`\ /s )                   |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscA4Max`           | 1.0E+21             | maximum biharmonic viscosity ( m\ :sup:`4`\ /s )                   |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscA4Grid`          | 0.0E+00             | grid dependent biharmonic viscosity ( non-dim. )                   |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscC4leith`         | 0.0E+00             | Leith biharmonic viscosity factor (on grad(vort), non-dim.)        |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscC4leithD`        | 0.0E+00             | Leith biharmonic viscosity factor (on grad(div), non-dim.)         |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscC4Smag`          | 0.0E+00             | Smagorinsky biharmonic viscosity factor (non-dim)                  |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`no_slip_sides`       | T                   | viscous BCs: no-slip sides                                         |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`sideDragFactor`      | 2.0E+00             | side-drag scaling factor (non-dim)                                 |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`viscAr`              | 0.0E+00             | vertical eddy viscosity ( units of r\ :sup:`2`\ /s )               |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`no_slip_bottom`      | T                   | viscous BCs: no-slip bottom                                        |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`bottomDragLinear`    | 0.0E+00             | linear bottom-drag coefficient ( m/s )                             |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`bottomDragQuadratic` | 0.0E+00             | quadratic bottom-drag coeff. ( 1 )                                 |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffKhT`             | 0.0E+00             | Laplacian diffusion of heat laterally ( m\ :sup:`2`\ /s )          |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffK4T`             | 0.0E+00             | biharmonic diffusion of heat laterally ( m\ :sup:`4`\ /s )         |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffKhS`             | 0.0E+00             | Laplacian diffusion of salt laterally ( m\ :sup:`2`\ /s )          |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffK4S`             | 0.0E+00             | biharmonic diffusion of salt laterally ( m\ :sup:`4`\ /s  )        |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffKrNrT`           | 0.0E+00 at k=top    | vertical profile of vertical diffusion of temp ( m\ :sup:`2`\ /s ) |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffKrNrS`           | 0.0E+00 at k=top    | vertical profile of vertical diffusion of salt ( m\ :sup:`2`\ /s ) |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffKrBL79surf`      | 0.0E+00             | surface diffusion for Bryan and Lewis 1979 ( m\ :sup:`2`\ /s )     |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffKrBL79deep`      | 0.0E+00             | deep diffusion for Bryan and Lewis 1979 ( m\ :sup:`2`\ /s )        |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffKrBL79scl`       | 2.0E+02             | depth scale for Bryan and Lewis 1979 ( m )                         |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`diffKrBL79Ho`        | -2.0E+03            | turning depth for Bryan and Lewis 1979 ( m )                       |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`eosType`             | LINEAR              | equation of state                                                  |
+--------------------------------+---------------------+--------------------------------------------------------------------+
| :varlink:`tAlpha`              | 2.0E-04             | linear EOS thermal expansion coefficient ( 1/\ :sup:`o`\ C )       |
+--------------------------------+---------------------+--------------------------------------------------------------------+

+-----------------------------------+-------------------------------+---------------------------------------------------+
| **Name**                          | **Value**                     | **Description**                                   |
+-----------------------------------+-------------------------------+---------------------------------------------------+
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`sBeta`                  | 7.4E-04                       | linear EOS haline contraction coef ( 1/psu )      |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`rhonil`                 | 9.998E+02                     | reference density ( kg/m\ :sup:`3` )              |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`rhoConst`               | 9.998E+02                     | reference density ( kg/m\ :sup:`3` )              |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`rhoConstFresh`          | 9.998E+02                     | reference density ( kg/m\ :sup:`3` )              |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`gravity`                | 9.81E+00                      | gravitational acceleration ( m/s\ :sup:`2` )      |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`gBaro`                  | 9.81E+00                      | barotropic gravity ( m/s\ :sup:`2` )              |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`rotationPeriod`         | 8.6164E+04                    | rotation period ( s )                             |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`omega`                  | :math:`2\pi/`\ rotationPeriod | angular velocity ( rad/s )                        |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`f0`                     | 1.0E-04                       | reference coriolis parameter ( 1/s )              |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`beta`                   | 1.0E-11                       | beta ( m\ :sup:`--1`\ s\ :sup:`--1` )             |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`freeSurfFac`            | 1.0E+00                       | implicit free surface factor                      |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`implicitFreeSurface`    | T                             | implicit free surface on/off flag                 |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`rigidLid`               | F                             | rigid lid on/off flag                             |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`implicSurfPress`        | 1.0E+00                       | surface pressure implicit factor (0-1)            |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`implicDiv2Dflow`        | 1.0E+00                       | barotropic flow div. implicit factor (0-1)        |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`exactConserv`           | F                             | exact volume conservation on/off flag             |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`uniformLin_PhiSurf`     | T                             | use uniform Bo_surf on/off flag                   |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`nonlinFreeSurf`         | 0                             | non-linear free surf. options (-1,0,1,2,3)        |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`hFacInf`                | 2.0E-01                       | lower threshold for hFac (nonlinFreeSurf only)    |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`hFacSup`                | 2.0E+00                       | upper threshold for hFac (nonlinFreeSurf only)    |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`select_rStar`           | 0                             | r                                                 |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`useRealFreshWaterFlux`  | F                             | real freshwater flux on/off flag                  |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`convertFW2Salt`         | 3.5E+01                       | convert FW flux to salt flux (-1=use local S)     |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`use3Dsolver`            | F                             | use 3-D pressure solver on/off flag               |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`nonHydrostatic`         | F                             | non-hydrostatic on/off flag                       |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`nh_Am2`                 | 1.0E+00                       | non-hydrostatic terms scaling factor              | 
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`quasiHydrostatic`       | F                             | quasi-hydrostatic on/off flag                     |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`momStepping`            | T                             | momentum equation on/off flag                     |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`vectorInvariantMomentum`| F                             | vector-invariant momentum on/off                  |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`momAdvection`           | T                             | momentum advection on/off flag                    |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`momViscosity`           | T                             | momentum viscosity on/off flag                    |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`momImplVertAdv`         | F                             | momentum implicit vert. advection on/off          |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`implicitViscosity`      | F                             | implicit viscosity on/off flag                    |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`metricTerms`            | F                             | metric terms on/off flag                          |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`useNHMTerms`            | F                             | non-hydrostatic metric terms on/off               |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`useCoriolis`            | T                             | Coriolis on/off flag                              |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`useCDscheme`            | F                             | CD scheme on/off flag                             |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`useJamartWetPoints`     | F                             | Coriolis wetpoints method flag                    |
+-----------------------------------+-------------------------------+---------------------------------------------------+
| :varlink:`useJamartMomAdv`        | F                             | VI non-linear terms Jamart flag                   |
+-----------------------------------+-------------------------------+---------------------------------------------------+

+-----------------------------------+---------------------+-------------------------------------------+
| **Name**                          | **Value**           | **Description**                           |
+-----------------------------------+---------------------+-------------------------------------------+
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`SadournyCoriolis`      | F                   | Sadourny Coriolis discretization flag     |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`upwindVorticity`       | F                   | upwind bias vorticity flag                |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`useAbsVorticity`       | F                   | work with f                               |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`highOrderVorticity`    | F                   | high order interp. of vort. flag          |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`upwindShear`           | F                   | upwind vertical shear advection flag      |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`selectKEscheme`        | 0                   | kinetic energy scheme selector            |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`momForcing`            | T                   | momentum forcing on/off flag              |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`momPressureForcing`    | T                   | momentum pressure term on/off flag        |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`implicitIntGravWave`   | F                   | implicit internal gravity wave flag       |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`staggerTimeStep`       | F                   | stagger time stepping on/off flag         |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`multiDimAdvection`     | T                   | enable/disable multi-dim advection        |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`useMultiDimAdvec`      | F                   | multi-dim advection is/is-not used        |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`implicitDiffusion`     | F                   | implicit diffusion on/off flag            |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`tempStepping`          | T                   | temperature equation on/off flag          |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`tempAdvection`         | T                   | temperature advection on/off flag         |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`tempImplVertAdv`       | F                   | temp. implicit vert. advection on/off     |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`tempForcing`           | T                   | temperature forcing on/off flag           |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`saltStepping`          | T                   | salinity equation on/off flag             |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`saltAdvection`         | T                   | salinity advection on/off flag            |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`saltImplVertAdv`       | F                   | salinity implicit vert. advection on/off  |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`saltForcing`           | T                   | salinity forcing on/off flag              |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`readBinaryPrec`        | 32                  | precision used for reading binary files   |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`writeBinaryPrec`       | 32                  | precision used for writing binary files   |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`globalFiles`           | F                   | write “global” (=not per tile) files      |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`useSingleCpuIO`        | F                   | only master MPI process does I/O          |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`debugMode`             | F                   | debug Mode on/off flag                    |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`debLevA`               | 1                   | 1st level of debugging                    |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`debLevB`               | 2                   | 2nd level of debugging                    |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`debugLevel`            | 1                   | select debugging level                    |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`cg2dMaxIters`          | 150                 | upper limit on 2d con. grad iterations    |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`cg2dChkResFreq`        | 1                   | 2d con. grad convergence test frequency   |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`cg2dTargetResidual`    | 1.0E-07             | 2d con. grad target residual              |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`cg2dTargetResWunit`    | -1.0E+00            | cg2d target residual [W units]            |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`cg2dPreCondFreq`       | 1                   | freq. for updating cg2d pre-conditioner   |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`nIter0`                | 0                   | run starting timestep number              |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`nTimeSteps`            | 0                   | number of timesteps                       |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`deltatTmom`            | 6.0E+01             | momentum equation timestep ( s )          |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`deltaTfreesurf`        | 6.0E+01             | freeSurface equation timestep ( s )       |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`dTtracerLev`           | 6.0E+01 at k=top    | tracer equation timestep ( s )            |
+-----------------------------------+---------------------+-------------------------------------------+
|  :varlink:`deltaTClock`           | 6.0E+01             | model clock timestep ( s )                |
+-----------------------------------+---------------------+-------------------------------------------+

+-------------------------------------+---------------------------+---------------------------------------------------------------+
| **Name**                            | **Value**                 | **Description**                                               |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`cAdjFreq`                 | 0.0E+00                   | convective adjustment interval ( s )                          |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`momForcingOutAB`          | 0                         | =1: take momentum forcing out of Adams-Bashforth              |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`tracForcingOutAB`         | 0                         | =1: take T,S,pTr forcing out of Adams-Bashforth               |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`momDissip_In_AB`          | T                         | put dissipation tendency in Adams-Bashforth                   |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`doAB_onGtGs`              | T                         | apply AB on tendencies (rather than on T,S)                   |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`abEps`                    | 1.0E-02                   | Adams-Bashforth-2 stabilizing weight                          |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`baseTime`                 | 0.0E+00                   | model base time ( s )                                         |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`startTime`                | 0.0E+00                   | run start time ( s )                                          |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`endTime`                  | 0.0E+00                   | integration ending time ( s )                                 |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`pChkPtFreq`               | 0.0E+00                   | permanent restart/checkpoint file interval ( s )              |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`chkPtFreq`                | 0.0E+00                   | rolling restart/checkpoint file interval ( s )                |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`pickup_write_mdsio`       | T                         | model I/O flag                                                |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`pickup_read_mdsio`        | T                         | model I/O flag                                                |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`pickup_write_immed`       | F                         | model I/O flag                                                |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`dumpFreq`                 | 0.0E+00                   | model state write out interval ( s )                          |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`dumpInitAndLast`          | T                         | write out initial and last iteration model state              |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`snapshot_mdsio`          | T                         | model I/O flag.                                                |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`monitorFreq`              | 6.0E+01                   | monitor output interval ( s )                                 |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`monitor_stdio`           | T                         | model I/O flag.                                                |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`externForcingPeriod`      | 0.0E+00                   | forcing period (s)                                            |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`externForcingCycle`       | 0.0E+00                   | period of the cycle (s)                                       |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`tauThetaClimRelax`        | 0.0E+00                   | relaxation time scale (s)                                     |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`tauSaltClimRelax`         | 0.0E+00                   | relaxation time scale (s)                                     |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`latBandClimRelax`         | 3.703701E+05              | maximum latitude where relaxation applied                     |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`usingCartesianGrid`       | T                         | Cartesian coordinates flag ( true / false )                   |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`usingSphericalPolarGrid`  | F                         | spherical coordinates flag ( true / false )                   |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`usingCylindricalGrid`     | F                         | spherical coordinates flag ( true / false )                   |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`Ro_SeaLevel`              | 0.0E+00                   | r(1) ( units of r )                                           |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`rkSign`                   | -1.0E+00                  | index orientation relative to vertical coordinate             |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`horiVertRatio`            | 1.0E+00                   | ratio on units : horizontal - vertical                        |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`drC`                      | 5.0E+03 at k=1            | center cell separation along Z axis ( units of r )            |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`drF`                      | 1.0E+04 at k=top          | cell face separation along Z axis ( units of r )              |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`delX`                     | 1.234567E+05 at i=east    | U-point spacing ( m - cartesian, degrees - spherical )        |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`delY`                     | 1.234567E+05 at j=1       | V-point spacing ( m - cartesian, degrees - spherical )        |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`ygOrigin`                 | 0.0E+00                   | South edge Y-axis origin (cartesian: m, spherical: deg.)      |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`xgOrigin`                 | 0.0E+00                   | West edge X-axis origin (cartesian: m, spherical: deg.)       |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`rSphere`                  | 6.37E+06                  | Radius ( ignored - cartesian, m - spherical )                 |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`xcoord`                   | 6.172835E+04 at i=1       | P-point X coord ( m - cartesian, degrees - spherical )        |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`ycoord`                   | 6.172835E+04 at j=1       | P-point Y coord ( m - cartesian, degrees - spherical )        |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`rcoord`                   | -5.0E+03 at k=1           | P-point r coordinate ( units of r )                           |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`rF`                       | 0.0E+00 at k=1            | W-interface r coordinate ( units of r )                       |
+-------------------------------------+---------------------------+---------------------------------------------------------------+
| :varlink:`dBdrRef`                  | 0.0E+00 at k=top          | vertical gradient of reference buoyancy [ (m/s/r)\ :sup:`2` ] |
+-------------------------------------+---------------------------+---------------------------------------------------------------+

+-------------------+--------------------------------+-------------------------------------------------------+
| **Name**          | **Value**                      | **Description**                                       |
+-------------------+--------------------------------+-------------------------------------------------------+
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`dxF`    | 1.234567E+05 at k=top          | dxF(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`dyF`    | 1.234567E+05 at i=east         | dyF(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`dxG`    | 1.234567E+05 at i=east         | dxG(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`dyG`    | 1.234567E+05 at i=east         | dyG(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`dxC`    | 1.234567E+05 at i=east         | dxC(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`dyC`    | 1.234567E+05 at i=east         | dyC(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`dxV`    | 1.234567E+05 at i=east         | dxV(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`dyU`    | 1.234567E+05 at i=east         | dyU(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`rA`     | 1.524155E+10 at i=east         | rA(:,1,:,1) ( m - cartesian, degrees - spherical )    |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`rAw`    | 1.524155E+10 at k=top          | rAw(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+
| :varlink:`rAs`    | 1.524155E+10 at k=top          | rAs(:,1,:,1) ( m - cartesian, degrees - spherical )   |
+-------------------+--------------------------------+-------------------------------------------------------+

+--------------------------------+-------------------+----------------------------------------------+
| **Name**                       | **Value**         | **Description**                              |
+--------------------------------+-------------------+----------------------------------------------+
+--------------------------------+-------------------+----------------------------------------------+
| :varlink:`tempAdvScheme`       | 2                 | temp. horiz. advection scheme selector       |
+--------------------------------+-------------------+----------------------------------------------+
| :varlink:`tempVertAdvScheme`   | 2                 | temp. vert. advection scheme selector        |
+--------------------------------+-------------------+----------------------------------------------+
| :varlink:`tempMultiDimAdvec`   | F                 | use multi-dim advection method for temp      |
+--------------------------------+-------------------+----------------------------------------------+
| :varlink:`tempAdamsBashforth`  | T                 | use Adams-Bashforth time-stepping for temp   |
+--------------------------------+-------------------+----------------------------------------------+
| :varlink:`saltAdvScheme`       | 2                 | salinity horiz. advection scheme selector    |
+--------------------------------+-------------------+----------------------------------------------+
| :varlink:`saltVertAdvScheme`   | 2                 | salinity vert.  advection scheme selector    |
+--------------------------------+-------------------+----------------------------------------------+
| :varlink:`saltMultiDimAdvec`   | F                 | use multi-dim advection method for salt      |
+--------------------------------+-------------------+----------------------------------------------+
| :varlink:`saltAdamsBashforth`  | T                 | use Adams-Bashforth time-stepping for salt   |
+--------------------------------+-------------------+----------------------------------------------+




