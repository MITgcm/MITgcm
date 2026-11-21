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
