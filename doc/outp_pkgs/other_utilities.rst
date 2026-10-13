Pre– and Post–Processing Scripts and Utilities
==============================================

There are numerous tools for pre-processing data, converting model
output and analysis written in `MATLAB <https://www.mathworks.com/>`_, Fortran (f77 and f90) and perl.
As yet they remain undocumented although many are self-documenting
(`MATLAB <https://www.mathworks.com/>`_ routines have “help” written into them).

Here we’ll summarize what is available but this is an ever growing
resource so this may not cover everything that is out there:

Utilities Supplied With the Model
---------------------------------

We supply some basic scripts with the model to facilitate conversion or
reading of data into analysis software.

utils/scripts
~~~~~~~~~~~~~

In the directory :filelink:`utils/scripts`,  :filelink:`joinds <utils/scripts/joinds>`
and :filelink:`joinmds <utils/scripts/joinmds>`
are perl scripts used to joining multi-part files created by
MITgcm. Use :filelink:`joinmds <utils/scripts/joinmds>`.
You will only need :filelink:`joinds <utils/scripts/joinds>` if you are
working with output older than two years (prior to c23).

utils/matlab
~~~~~~~~~~~~

In the directory :filelink:`utils/matlab` you will find
several `MATLAB <https://www.mathworks.com/>`_  scripts (``.m``
files). The principle script is :filelink:`rdmds.m <utils/matlab/rdmds.m>`, used for reading
the multi-part model output files into `MATLAB <https://www.mathworks.com/>`_ . Place the scripts in
your `MATLAB <https://www.mathworks.com/>`_  path or change the path appropriately,
then at the `MATLAB <https://www.mathworks.com/>`_
prompt type:

::

      >> help rdmds

to get help on how to use :filelink:`rdmds <utils/matlab/rdmds.m>`.

Another useful script scans the terminal output file for :filelink:`pkg/monitor`
information.

Most other scripts are for working in the curvilinear coordinate systems,
and as yet are unpublished and undocumented.

pkg/mnc utils
~~~~~~~~~~~~~

The following scripts and utilities have been written to help manipulate 
`netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ files:

Tile Assembly:
    A `MATLAB <https://www.mathworks.com/>`_ script
    :filelink:`mnc_assembly.m <utils/matlab/mnc_assembly.m>` is available for
    spatially “assembling” :filelink:`pkg/mnc` output. A convenience wrapper script
    called :filelink:`gluemnc.m <utils/matlab/gluemnc.m>` is also provided. Please use the
    `MATLAB <https://www.mathworks.com/>`_ help facility for more information.

    A bash script :filelink:`gluemnc <utils/scripts/gluemnc>` is available for 
    spatially “assembling” :filelink:`pkg/mnc` NetCDF output. Please see 
    :numref:`gluemnc` for details. 

gmt:
    As MITgcm evolves to handle more complicated domains and topologies,
    a suite of matlab tools is being written to more gracefully handle
    the model files. This suite is called “gmt” which refers to
    “generalized model topology” pre-/post-processing. Currently, this
    directory contains a `MATLAB <https://www.mathworks.com/>`_ script
    :filelink:`gmt/rdnctiles.m <utils/matlab/gmt/rdnctiles.m>` that
    is able to read `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ files for any domain.
    Additional scripts are being created that will work with these
    fields on a per-tile basis.

Pre-Processing Software
-----------------------

There is a suite of pre-processing software for interpolating bathymetry
and forcing data, written by Adcroft and Biastoch. At some point, these
will be made available for download. If you are in need of such
software, contact one of them.
