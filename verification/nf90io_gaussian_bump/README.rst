Example to test pkg/nf90io
==========================

This example tests ``pkg/nf90io`` and
``pkg/diagnostics/diagnostics_nf90io_out.F``.

Historically, netcdf output for the ``MITgcm`` has been via a
non-parallel implimentation, and netcdf files were saved as
one-per-tile, and pasted together afterwards with ``matlab`` or
``python`` routines. These can fail for very large models since it
requires large data sets to be loaded into memory.

``pkg/nf90io`` impliments parallel saving, so just one ``netcdf`` file
is saved for all the tiles.

The output is controlled by the ``diagnostics`` package only. Setting
``diag_nf90io=.TRUE.`` in ``data.diagnostics`` allows the single-file
netcdf files to be saved. Note that we allow ``diag_mnc=.TRUE.`` to also
be set if the user wants both (though the information should be
duplicated). ``diag_nf90io=.TRUE.`` is set if ``NF90IO`` is found by
``genmake2`` and ``useNF90io=.TRUE.`` in ``data.pkg``.

One difference with other i/o in the MITgcm is that the file writes will
*not* overwrite old files, but will append data to the end. This is done
on purpose to allow files to be contiguously saved after a model restart
using pickup files. If you want the file to be clobbered, you need to
delete it manually. Note that if the model configuration changes in a
way that incompatible with the old files ``NF90`` itself will throw an
error.

This example is a test of that functionality. It should make the files
``statevars.nc``, ``statevars2d.nc`` and ``levelvars.nc`` in the
\`./run/' directory.

Compiling
---------

This test relies on having a parallel enabled version ``netcdf4``.
Usually enabled when ``hdf5`` has been installed as parallel. Of course
it also requires ``mpi``.

``genmake2`` checks for ``nf90io`` by compiling a small test program. If
this test fails then either your ``netcdf`` was not compiled with
parallel libraries or you have not set your ``INCLUDES`` and ``LIBS``
variables correctly in your build options file.

My build\_options look like:

::

    CPP='/usr/local/bin/cpp-7 -traditional -P'
    FC=mpif90
    CC=mpicc
    LINK=$FC
    NOOPTFLAGS='-O0'
    MAKEDEPEND='makedepend'
    DEFINES='-DWORDLENGTH=4 -DALWAYS_USE_MPI -DALLOW_USE_MPI'
    INCLUDES = '-I. /usr/local/include'
    FFLAGS='-Wunused -Wuninitialized'
    FOPTIM='-O3 -funroll-loops -ftree-vectorize -ffpe-trap=invalid'

To build, make a ``build/`` directory. Run ``genmake2``. Run
``make depend`` and then run ``make``.

Setting up and running
----------------------

The file ``input/gendata.py`` should set up a directory ``run/`` where
the model should be run. The initialization files will be put there. In
order to run, something like:

::

    % mpirun -n 8 ./mitgcmuv

The result should be three netcdf files ``statevars.nc``,
``statevars2d.nc``, and ``levelvars.nc``, as specified in
``input/data.diagnostics``.
