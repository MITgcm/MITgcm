NetCDF90 Parallel Output: NF90IO
********************************

The ``nf90io`` package implements parallel oputput for the diagnostics package.  It writes one NetCDF file per ``filename`` specified in ``data.diagnostics``, unlike the older ``mnc`` package, which writes one file per tile of the model domain, and requires post-processing to merge into one file:

As example, consider the following ``data.diagnostic``:

::

  &diagnostics_list
  # 3D variables.
  filename(1)   = 'statevars'
  frequency(1)  = -1860,
  timePhase(1)      =  0,
  fields(1:2,1) = 'UVEL    ',
                  'THETA   ',
  #
  filename(2)   = 'statevars2d'
  frequency(2)  = -1860,
  timePhase(2)  = 0,
  fields(1:2,2) = 'ETAN    ',
                  'PHIBOT  ',
  /

If NF90IO is turned on, then the model will produce two NetCDF files ``statevars.nc`` and ``statevars2d.nc`` containing the diagnostics.  Files can, of course, have many more diagnostics.  The NetCDF files also contain all the grid information for the model.

The resulting file is structured like this example:

::

  Dimensions:    (i: 80, i_g: 80, j: 1, j_g: 1, k: 25, k_l: 25, k_p1: 26, k_u: 25, record: 53)
  Coordinates:
    * record     (record) int32 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 ...
    * i          (i) int32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...
    * i_g        (i_g) int32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...
    * j          (j) int32 0
    * j_g        (j_g) int32 0
    * k          (k) int32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...
    * k_l        (k_l) int32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...
    * k_u        (k_u) int32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ...
    * k_p1       (k_p1) int32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 ...
      XC         (j, i) float64 1.237e+04 3.629e+04 5.859e+04 7.925e+04 ...
      XG         (j_g, i_g) float64 7.276e-12 2.474e+04 4.785e+04 6.933e+04 ...
      YC         (j, i) float64 2.5e+03 2.5e+03 2.5e+03 2.5e+03 2.5e+03 ...
      YG         (j_g, i_g) float64 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 ...
  Data variables:
      iter       (record) int32 0 150 300 450 600 750 900 1050 1200 1350 1500 ...
      time       (record) timedelta64[ns] 00:00:00 00:31:00 01:02:00 01:33:00 ...
      Depth      (j, i) float64 2e+03 2e+03 2e+03 2e+03 2e+03 2e+03 2e+03 ...
      Z          (k) float64 -40.0 -120.0 -200.0 -280.0 -360.0 -440.0 -520.0 ...
      Zu         (k_u) float64 -80.0 -160.0 -240.0 -320.0 -400.0 -480.0 -560.0 ...
      Zl         (k_l) float64 0.0 -80.0 -160.0 -240.0 -320.0 -400.0 -480.0 ...
      Zp1        (k_p1) float64 0.0 -80.0 -160.0 -240.0 -320.0 -400.0 -480.0 ...
      rA         (j, i) float64 1.237e+08 1.155e+08 1.074e+08 9.925e+07 ...
      rAw        (j, i_g) float64 1.237e+08 1.196e+08 1.115e+08 1.033e+08 ...
      rAs        (j_g, i) float64 1.237e+08 1.155e+08 1.074e+08 9.925e+07 ...
      rAz        (j_g, i_g) float64 1.237e+08 1.196e+08 1.115e+08 1.033e+08 ...
      dxG        (j_g, i) float64 2.474e+04 2.311e+04 2.148e+04 1.985e+04 ...
      dyG        (j, i_g) float64 2.474e+04 2.311e+04 2.148e+04 1.985e+04 ...
      dxC        (j, i_g) float64 2.474e+04 2.392e+04 2.229e+04 2.066e+04 ...
      dyC        (j_g, i) float64 2.474e+04 2.392e+04 2.229e+04 2.066e+04 ...
      drC        (k_p1) float64 40.0 80.0 80.0 80.0 80.0 80.0 80.0 80.0 80.0 ...
      drF        (k) float64 80.0 80.0 80.0 80.0 80.0 80.0 80.0 80.0 80.0 80.0 ...
      hFacC      (k, j, i) float64 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 ...
      hFacW      (k, j, i_g) float64 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 ...
      hFacS      (k, j_g, i) float64 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 ...
      PHrefC     (k) float64 392.4 1.177e+03 1.962e+03 2.747e+03 3.532e+03 ...
      PHrefF     (k_p1) float64 0.0 784.8 1.57e+03 2.354e+03 3.139e+03 ...
      timestart  (record) float64 0.0 1.86e+03 3.72e+03 5.58e+03 7.44e+03 ...
      timeend    (record) float64 2.176e-314 2.176e-314 2.176e-314 2.176e-314 ...
      UVEL       (record, k, j, i_g) float64 0.338 0.338 0.0 0.0 0.0 0.0 0.0 ...
      THETA      (record, k, j, i) float64 26.9 26.9 26.9 26.9 26.9 26.9 26.9 ...
  Attributes:
      the_run_name:
      build_user:    jklymak
      build_host:    JMKMBP.local
      build_date:    Fri Jul 28 18:37:18 PDT 2017
      MITgcm_URL:    http://mitgcm.org
      history:       Saved from nf90io

The co-ordinates are kept deliberately abstract ``i``, ``j`` etc because various non-Cartesian grids are possible in the model.  The variables ``time`` are in seconds, and the variables ``timestart`` and ``timeend`` indicate the start and end of an averaging period for averaged diagnostics.  Note that variables are assigned dimensions based on their location on the Arakawa C-Grid, so in this example ``UVEL`` has a third spatial dimension ``i_g`` instead of ``i`` to indicate that it is on the west side of the cell faces.  ``VVEL`` would be on ``j_g`` instead of ``j`` to indicate it is located on the south side of cell faces.  Note how the first element of ``XG`` is smaller than the first element of ``XC``.

Using NF90IO
-------------

NF90IO can be turned on or off at compile time with ``code/packages.conf`` or using ``genmake2 -enable=nf90io``.  There is a test to make sure NF90IO can be compiled in ``genmake2`` (see below).

It can be turned on or off at run time in ``input/data.pkg`` by setting ``useNF90IO=.TRUE.``.

Finally it can be turned on or off explicitly inside ``input/data.diagnostics`` by setting ``diag_nf90io=.TRUE.`` or ``.FLASE.``.  By default it is set to ``useNF90IO``, so it is just as easy to control from ``input/data.pkg``

Currently there is no ``input/data.nf90io`` because there are no package-specific options to turn on or off.  This may change in the future.

Note that NF90IO output can be mixed with MDSIO and MNC outputs.  If ``input/data.pkg`` has ``useMNC=.TRUE.`` and ``useNF90IO=.TRUE`` then both styles of files will be written unless explicitly turned off in ``input/data.diagnostics``.  Users probably don't want to output both types of files, so if ``useMNC=.TRUE.`` and ``useNF90IO=.TRUE`` are set in ``input/data.pkg`` then in ``input/data.diagnostics`` users probably want to set ``diag_nf90io=.FALSE.`` *or* ``diag_mnc=.FALSE.``.

To suppress MDS output (binary files), the ``input/data`` file should be edited to have ``dumpFreq=0`` and ``dumpInitAndLast=.FALSE.`` in the parameter list ``&PARM03``.

NF90IO Appends to Existing Files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

NF90IO *appends* to existing files.  In the example above, if ``statevars.nc`` exists, and the model is re-run, the file will not be erased and started over again, but will instead have new records appended to it.  This will lead to duplicated information if the model is restarted from scratch.

Conversely, it allows the model to be restarted from a pickup and the original file to be appended to, keeping all model output in one place.

If you don't want the old file to be appended to, then rename or delete the old file before running the model or set a new ``filename`` in ``input/data.diagnostics``.

Compiling NF90IO
----------------------

NF90IO is based on `NetCDF F90 <https://www.unidata.ucar.edu/software/netcdf/netcdf-4/newdocs/netcdf-f90.html>`_ routines.  It requires ``netcdf`` to be built for Fortran, linked to a ``hdf5`` library that has been built with parallel support.  You should know where the NetCDF library and include files are for these libraries are so you can specify them in your ``build_options/yourmachine`` file.  i.e. if these files are in ``$NETCDF_DIR`` then you should have something like

::

  INCLUDE="-I$NETCDF_DIR/include -I$MPI_DIR/include"
  LIBS="-L$NETCDF_DIR/libs"

in that file.

The script ``genmake2`` will check to see if NF90IO can be compiled using your ``build_options``.  If that check fails, then NF90IO will be made unavailable.  During the ``genmake2`` process you should get a series of messages like:

::

  Can we create NetCDF-enabled binaries...  yes
  Can we create NF90-enabled binaries...  yes
  Can we create LAPACK-enabled binaries...  no

If the ``NF90-enabled binaries`` message returns ``yes`` then the variable ``HAVE_NF90IO`` is set to true and NF90IO will be able to be compiled.

NF90IO vs MNC and MDSIO
-----------------------

Which output format should you choose for the MITgcm?  MNC is the older NetCDF interface which makes NetCDF 3.x files.  MDSIO writes binary files.  Which output you choose depends on your complier environment, your model output, and your processing environment.

MDSIO is simple, and has a good set of tools built around it.  On the other hand, there are few tools available to handle very large MDSIO binary files.  This can be an issue when model output becomes larger than memory space in the analysis computer.  Many supercomputing systems do not allow users to access swap space memory, so memory is confined to the available memory on the machine, and even one snapshot from a modern MITgcm data set can exceed 50 Gb if all variables are loaded.  MDSIO also has the plus/minus of storing each time step as a unique file.  This is sometimes useful if you just want a few snapshots near the end of a run, but has obvious archiving drawbacks and is less convenient than accessing records in a single NetCDF file.

MNC writes NetCDF files, and has all the advantages of a standard well-used file standard, including many large tool suites.  Modern versions of these tool suites allow "chunked" data operations whereby data is only loaded into machine memory on an as-needed basis.  (i.e. in python the ``xarray`` plus ``dask`` libraries do this.  Matlab has the ability to read chunked arrays as well). MNC is also somewhat more flexible than NF90IO, with options controlling file sizes and file directories (NF90IO could add such options in the future).

Conversely, MNC cannot do parallel output operations, so each MPI processor has to write to a separate file.  So instead of ``statevars.nc`` in the example above, a series of files are written ``statevars.0000000000.t0001``, ``statevars.0000000000.t0002``, etc, one file for each tile in the simulation.   So, a 1024-core job  produces 1024 NetCDF files, one for each core.  In order to make a single NetCDF file, these files need to be ``glued`` together (by python and/or matlab scripts) in post-processing.  For large files on memory-constrained machines these scripts can fail.

The memory drawback of both the MDSIO and MNC approaches is what inspired NF90IO.  NF90IO creates a *single* file for each diagnostic, and these files can be arbitrarily large.

When would you *not* use NF90IO?  If you don't have a parallel NetCDF library is one case.  If your file system cannot support very large files would be another case.  The other limitation of NF90IO is that it only (currently) supports ``pkg/diagnostics`` controlled output.  Using ``pkg/diagnostics`` is *highly* recommended, but there may be good reasons to not use it in certain cases.

Dealing with Large NetCDF Files
-------------------------------

The NF90IO style output can lead to very large NetCDF files depending on the values of ``frequency`` in  ``input/data.diagnostics`` and the size of the simulation. Transfering such large files can be difficult, and often users are just interested in part of the output.  There are two strategies here:

  1. run the model to the time of interest with large values of ``frequency`` in ``input/data.diagnostics``, and then restart from a pickup file specifying new values of ``frequency`` and presumably specfying new filenames for the diagnostics (or moving/deleting the old file).

  For instance to spinup for 100 days, and then start saving output in ``input/data`` one would specify ``endTime=8640000,`` and ``pickupFreq=8640000``, and in ``input/diagnostics`` the values for the frequency would be set to something large i.e. ``frequency(1)=864000,``.  Then if the user wanted hourly output they would edit ``input/data`` to have ``startTime=8640000``, ``endTime=8812800`` and in ``input/diagnostics`` we would set ``frequency(1)=3600``.  We may also want to move the NetCDF file that was cerated in the spinup stage to ``spinupstatevars.nc`` so the high temporal resolution data could be saved in ``statevars.nc``.

  2. Use a tool like ``ncea`` in the `NCO toolbox <http://nco.sourceforge.net>`_ to subset the ``time`` or ``record`` variables

  ::

    ncea -F -d record,first,last statevars.nc statevarssubset.nc

  ``ncea`` can also be used to make the files smaller in space (see the online documentation for this utility).

  A similar effect can be carried out in Python using the `xarray <http://xarray.pydata.org/en/stable/>`_ package.  For instance

  ::

    import xarray as xr

    ds = xr.open_dataset('statevars.nc')
    ds = ds.isel(i=range(40,50), i_g=range(40,50), record=range(50,54))
    ds.to_netcdf('small.nc')

  subsets the files on records 50 to 53, and the x co-ordinates between index 40 and 49, making a much smaller file.  Slices in various dimensions can be selected in a similar manner.  Because `xarray <http://xarray.pydata.org/en/stable/>`_ uses `dask <https://dask.pydata.org/en/latest/>`_ as a backend, the files need not exist in memory at any point.

Of course some combination of both these approaches will be useful in many cases.
