
Grid Generation
===============

The horizontal discretizations within MITgcm have been written to work
with many different grid types including:

-  cartesian coordinates

-  spherical polar (“latitude-longitude”) coordinates

-  general curvilinear orthogonal coordinates

The last of these, especially when combined with the domain
decomposition capabilities of MITgcm, allows a great degree of grid
flexibility. To date, general curvilinear orthogonal coordinates have
been used extensively in conjunction with
so-called “cubed sphere” grids. However, it is important to observe that
cubed sphere arrangements are only one example of what is possible with
domain-decomposed logically rectangular regions each containing
curvilinear orthogonal coordinate systems. Much more sophisticated
domains can be imagined and constructed.

In order to explore the possibilities of domain-decomposed curvilinear
orthogonal coordinate systems, a suite of grid generation software
called “SPGrid” (for SPherical Gridding) has been developed. SPGrid is a
relatively new facility and papers detailing its algorithms are in
preparation. Although SPGrid is new and rapidly developing, it has
already demonstrated the ability to generate some useful and interesting
grids.

This section provides a very brief introduction to SPGrid and shows some
early results. For further information, please contact the MITgcm
support list MITgcm-support@mitgcm.org.

Using SPGrid
------------

The SPGrid software is not a single program. Rather, it is a collection
of C++ code and `MATLAB <https://www.mathworks.com/>`_ scripts that can be used as a framework or
library for grid generation and manipulation. Currently, grid creation
is accomplished by either directly running `MATLAB <https://www.mathworks.com/>`_ scripts or by writing
a C++ “driver” program. The `MATLAB <https://www.mathworks.com/>`_ scripts are suitable for grids
composed of a single “face” (that is, a single logically rectangular
region on the surface of a sphere). The C++ driver programs are
appropriate for grids composed of multiple connected logically
rectangular patches. Each driver program is written to specify the
shape and connectivity of tiles and the preferred grid density (that is,
the number of grid cells in each logical direction) and edge locations
of the cells where they meet the edges of each face. The driver programs
pass this information to the SPGrid library, which generates the actual
grid and produces the output files that describe it.

Currently, driver programs are available for a few examples including
cubes, “lat-lon caps” (cube topologies that have conformal caps at the
poles and are exactly lat-lon channels for the remainder of the domain),
and some simple “embedded” regions that are meant to be used within
typical cubes or traditional lat-lon grids.

To create new grids, one may start with an existing driver program and
modify it to describe a domain that has a different arrangement. The
number, location, size, and connectivity of grid “faces” (the name used
for the logically rectangular regions) can be readily changed. Further,
the number of grid cells within faces and the location of the grid cells
at the face edges can also be specified.

SPGrid requirements
~~~~~~~~~~~~~~~~~~~

The following programs and libraries are required to build and/or run
the SPGrid suite:

-  `MATLAB <https://www.mathworks.com/>`_ is a run-time requirement since many of the generation
   algorithms have been written as `MATLAB <https://www.mathworks.com/>`_ scripts.

-  The `Geometric Tools Engine <https://geometrictools.com>`_  (a C++ library) is needed for the
   main “driver” code.

-  The `netCDF <http://www.unidata.ucar.edu/software/netcdf/>`_ library is needed for file I/O.

-  The `Boost serialization library <http://www.boost.org/doc/libs/1_66_0/libs/serialization/doc/index.html>`_ is also used for I/O:

-  a typical Linux/Unix build environment including the make utility
   (preferably GNU Make) and a C++ compiler (SPGrid was developed with
   g++ v4.x).

Obtaining SPGrid
~~~~~~~~~~~~~~~~

The latest version can be obtained from:


Building SPGrid
~~~~~~~~~~~~~~~

The procedure for building is similar to many open source projects:

::

         tar -xf spgrid-0.9.4.tar.gz
         cd spgrid-0.9.4
         export CPPFLAGS="-I/usr/include/netcdf-3"
         export LDFLAGS="-L/usr/lib/netcdf-3"
         ./configure
         make

where the ``CPPFLAGS`` and ``LDFLAGS`` environment variables can be
edited to reflect the locations of all the necessary dependencies.
SPGrid is known to work on Fedora Core Linux (versions 4 and 5) and is
likely to work on most any Linux distribution that provides the needed
dependencies.

Running SPGrid
~~~~~~~~~~~~~~

Within the ``src`` sub-directory, various example driver programs exist.
These examples describe small, simple domains and can generate the input
files (formatted as either binary ``*.mitgrid`` or netCDF) used by
MITgcm.

One such example is called ``SpF_test_cube_cap`` and it can be run with
the following sequence of commands:

::

         cd spgrid-0.9.4/src
         make SpF_test_cube_cap
         mkdir SpF_test_cube_cap.d
         ( cd SpF_test_cube_cap.d && ln -s ../../scripts/*.m . )
         ./SpF_test_cube_cap

which should create a series of output files:

::

         SpF_test_cube_cap.d/grid_*.mitgrid
         SpF_test_cube_cap.d/grid_*.nc
         SpF_test_cube_cap.d/std_topology.nc

where the ``grid_.mitgrid`` and ``grid_.nc`` files contain the grid
information in binary and netCDF formats and the ``std_topology.nc``
file contains the information describing the connectivity (both
edge–edge and corner–corner contacts) between all the faces.

Example Grids
-------------

The following grids are various examples created with SPGrid.

