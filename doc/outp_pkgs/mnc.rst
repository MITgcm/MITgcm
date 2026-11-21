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
See :ref:`gluemnc <gluemnc>` for post-processing NetCDF output via command line. 

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
