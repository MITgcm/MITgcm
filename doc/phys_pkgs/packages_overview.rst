Using MITgcm Packages
=====================


The set of packages that will be used within a partiucular model can be
configured using a combination of both “compile–time” and “run–time”
options. Compile–time options are those used to select which packages
will be “compiled in” or implemented within the program. Packages
excluded at compile time are completely absent from the executable
program(s) and thus cannot be later activated by any set of subsequent
run–time options.

Package Inclusion/Exclusion
---------------------------

There are numerous ways that one can specify compile–time package
inclusion or exclusion and they are all implemented by the ``genmake2``
program which was previously described in Section [sec:buildingCode].
The options are as follows:

#. Setting the ``genamake2`` options ``–enable PKG`` and/or
   ``–disable PKG`` specifies inclusion or exclusion. This method is
   intended as a convenient way to perform a single (perhaps for a quick
   test) compilation.

#. By creating a text file with the name ``packages.conf`` in either the
   local build directory or the ``-mods=DIR`` directory, one can specify
   a list of packages (one package per line, with ’\ ``#``\ ’ as the
   comment character) to be included. Since the ``packages.conf`` file
   can be saved, this is the preferred method for setting and recording
   (for future reference) the package configuration.

#. For convenience, a list of “standard” package groups is contained in
   the ``pkg/pkg_groups`` file. By selecting one of the package group
   names in the ``packages.conf`` file, one automatically obtains all
   packages in that group.

#. By default (that is, if a ``packages.conf`` file is not found), the
   ``genmake2`` program will use the package group default
   “``default_pkg_list``” as defined in ``pkg/pkg_groups`` file.

#. To help prevent users from creating unusable package groups, the
   ``genmake2`` program will parse the contents of the
   ``pkg/pkg_depend`` file to determine:

   -  whether any two requested packages cannot be simultaneously
      included (*eg.* *seaice* and *thsice* are mutually exclusive),

   -  whether additional packages must be included in order to satisfy
      package dependencies (*eg.* *rw* depends upon functionality within
      the *mdsio* package), and

   -  whether the set of all requested packages is compatible with the
      dependencies (and producing an error if they aren’t).

   Thus, as a result of the dependencies, additional packages may be
   added to those originally requested.

Package Activation
------------------

For run–time package control, MITgcm uses flags set through a
``data.pkg`` file. While some packages (*eg.* ``debug``, ``mnc``,
``exch2``) may have their own usage conventions, most follow a simple
flag naming convention of the form:

::

      usePackageName=.TRUE.

where the ``usePackageName`` variable can activate or disable the
package at runtime. As mentioned previously, packages must be included
in order to be activated. Generally, such mistakes will be detected and
reported as errors by the code. However, users should still be aware of
the dependency.

Package Coding Standards
------------------------

The following sections describe how to modify and/or create new MITgcm
packages.

Packages are Not Libraries
++++++++++++++++++++++++++

To a beginner, the MITgcm packages may resemble libraries as used in
myriad software projects. While future versions are likely to implement
packages as libraries (perhaps using FORTRAN90/95 syntax) the current
packages (FORTRAN77) are **not** based upon any concept of libraries.

File Inclusion Rules
++++++++++++++++++++

Instead, packages should be viewed only as directories containing “sets
of source files” that are built using some simple mechanisms provided by
``genmake2``. Conceptually, the build process adds files as they are
found and proceeds according to the following rules:

#. ``genmake2`` locates a “core” or main set of source files (the
   ``-standarddirs`` option sets these locations and the default value
   contains the directories ``eesupp`` and ``model``).

#. ``genmake2`` then finds additional source files by inspecting the
   contents of each of the package directories:

   #. As the new files are found, they are added to a list of source
      files.

   #. If there is a file name “collision” (that is, if one of the files
      in a package has the same name as one of the files previously
      encountered) then the file within the newer (more recently
      visited) package will superseed (or “hide”) any previous file(s)
      with the same name.

   #. Packages are visited (and thus files discovered) *in the order
      that the packages are enabled* within ``genmake2``. Thus, the
      files in ``PackB`` may superseed the files in ``PackA`` if
      ``PackA`` is enabled before ``PackB``. Thus, package ordering can
      be significant! For this reason, ``genmake2`` honors the order in
      which packages are specified.

These rules were adopted since they provide a relatively simple means
for rapidly including (or “hiding”) existing files with modified
versions.

Conditional Compilation and ``PACKAGES_CONFIG.h``
+++++++++++++++++++++++++++++++++++++++++++++++++

Given that packages are simply groups of files that may be added or
removed to form a whole, one may wonder how linking (that is, FORTRAN
symbol resolution) is handled. This is the second way that ``genmake2``
supports the concept of packages. Basically, ``genmake2`` creates a
``Makefile`` that, in turn, is able to create a file called
``PACKAGES_CONFIG.h`` that contains a set of C pre-processor (or “CPP”)
directives such as:

::

       #undef  ALLOW_KPP
       #undef  ALLOW_LAND
       ...
       #define ALLOW_GENERIC_ADVDIFF
       #define ALLOW_MDSIO
       ...

These CPP symbols are then used throughout the code to conditionally
isolate variable definitions, function calls, or any other code that
depends upon the presence or absence of any particular package.

An example illustrating the use of these defines is:

::

       #ifdef ALLOW_GMREDI
             IF (useGMRedi) CALL GMREDI_CALC_DIFF(
            I        bi,bj,iMin,iMax,jMin,jMax,K,
            I        maskUp,
            O        KappaRT,KappaRS,
            I        myThid)
       #endif

which is included from the file and shows how both the compile–time
``ALLOW_GMREDI`` flag and the run–time ``useGMRedi`` are nested.

There are some benefits to using the technique described here. The first
is that code snippets or subroutines associated with packages can be
placed or called from almost anywhere else within the code. The second
benefit is related to memory footprint and performance. Since unused
code can be removed, there is no performance penalty due to unnecessary
memory allocation, unused function calls, or extra run-time ``IF (...)``
conditions. The major problems with this approach are the potentially
difficult-to-read and difficult-to-debug code caused by an overuse of
CPP statements. So while it can be done, developers should exerecise
some discipline and avoid unnecesarily “smearing” their package
implementation details across numerous files.

Package Startup or Boot Sequence
++++++++++++++++++++++++++++++++

Calls to package routines within the core code timestepping loop can
vary. However, all packages should follow a required “boot” sequence
outlined here:

::

        1. S/R PACKAGES_BOOT()
                :
            CALL OPEN_COPY_DATA_FILE( 'data.pkg', 'PACKAGES_BOOT', ... )
     

        2. S/R PACKAGES_READPARMS()
                :
            #ifdef ALLOW_${PKG}
              if ( use${Pkg} )
         &       CALL ${PKG}_READPARMS( retCode )
            #endif

        3. S/R PACKAGES_INIT_FIXED()
                :
            #ifdef ALLOW_${PKG}
              if ( use${Pkg} )
         &       CALL ${PKG}_INIT_FIXED( retCode )
            #endif

        4. S/R PACKAGES_CHECK()
                :
            #ifdef ALLOW_${PKG}
              if ( use${Pkg} )
         &       CALL ${PKG}_CHECK( retCode )
            #else
              if ( use${Pkg} )
         &       CALL PACKAGES_CHECK_ERROR('${PKG}')
            #endif

        5. S/R PACKAGES_INIT_VARIABLES()
                :
            #ifdef ALLOW_${PKG}
              if ( use${Pkg} )
         &       CALL ${PKG}_INIT_VARIA( )
            #endif

         6. S/R DO_THE_MODEL_IO

            #ifdef ALLOW_${PKG}
              if ( use${Pkg} )
         &       CALL ${PKG}_OUTPUT( )
            #endif

         7. S/R PACKAGES_WRITE_PICKUP()

            #ifdef ALLOW_${PKG}
              if ( use${Pkg} )
         &       CALL ${PKG}_WRITE_PICKUP( )
            #endif

Adding a package to PARAMS.h and packages\_boot()
+++++++++++++++++++++++++++++++++++++++++++++++++

An MITgcm package directory contains all the code needed for that
package apart from one variable for each package. This variable is the
*use${Pkg} * flag. This flag, which is of type logical, **must** be
declared in the shared header file *PARAMS.h* in the *PARM\_PACKAGES*
block. This convention is used to support a single runtime control file
*data.pkg* which is read by the startup routine *packages\_boot()* and
that sets a flag controlling the runtime use of a package. This routine
needs to be able to read the flags for packages that were not built at
compile time. Therefore when adding a new package, in addition to
creating the per-package directory in the *pkg/* subdirectory a
developer should add a *use${Pkg} * flag to *PARAMS.h* and a *use${Pkg}
* entry to the *packages\_boot()* *PACKAGES* namelist. The only other
package specific code that should appear outside the individual package
directory are calls to the specific package API.
