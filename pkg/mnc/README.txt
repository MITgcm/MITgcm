
API Discussions:
================

As discussed in our group meeting of 2003-12-17 (AJA, CNH, JMC, AM,
PH, EH3), the NetCDF interface should resemble the following FORTRAN
subroutines:

  1) "stubs" of the form:  MNC_WV_[G|L]2D_R[S|L] ()

  2) MNC_INIT_VGRID( 'V_GRID_TYPE', nx, ny, nz, zc, zg )
				    1   1

  3) MNC_INIT_HGRID( 'H_GRID_TYPE', nx, ny, xc, yc, xg, yg )

  4) MNC_INIT_VAR( 'file', 'Vname', 'Vunits', 'H_GTYPE', 'V_GTYPE', PREC, FillVal )

  5) MNC_WRITE_VAR( 'file', 'Vname', var, bi, bj, myThid )

This is a reasonable start but its inflexible since it isn't easily
generalized to grids with dimensions other than [2,3,4] or grids with
non-horizontal orientations (eg. vertical slices).


Generalizing what we would like to write as "variables defined on 1-D
to n-D grids", one can imagine a small number of objects containing
all the relevant information:

  a dimension:   [ name, size, units ]
  a grid:        [ name, 1+ dim-ref ]
  a variable:    [ name, units, *1* grid-ref, data ]
  an attribute:  [ name, units, data ]
  a NetCDF file: [ name, 0+ attr, 0+ grid-ref, 0+ var-ref ]

which can then be manipulated (created, associated, destroyed, etc.)
using a simple interface such as:

  MNC_INIT(		 myThid )

  MNC_FILE_CREATE(	 myThid, fname )
  MNC_FILE_OPEN(	 myThid, fname, itype )
  MNC_FILE_ADD_ATTR_STR( myThid, fname, atname, sval )
  MNC_FILE_ADD_ATTR_INT( myThid, fname, atname, ival )
  MNC_FILE_ADD_ATTR_DBL( myThid, fname, atname, dval )
  ...
  MNC_FILE_READ_HEADER(  myThid, fname )

  MNC_DIM_INIT(		 myThid, dname, dunits, dlen )
  MNC_DIM_REMOVE(	 myThid, dname )

  MNC_GRID_INIT(	 myThid, fname, gname, ndim, dnames )
  MNC_GRID_SET_LL(	 myThid, fname, gname, type, lats, lons )

  MNC_VAR_INIT_DBL(	 myThid, fname, gname, vname, units, fillval )
  MNC_VAR_INIT_REAL(	 myThid, fname, gname, vname, units, fillval )
  MNC_VAR_INIT_INT(	 myThid, fname, gname, vname, units, fillval )
  MNC_VAR_ADD_ATTR_STR(	 myThid, fname, vname, name, atname, sval )
  MNC_VAR_ADD_ATTR_INT(	 myThid, fname, vname, name, atname, ival )
  MNC_VAR_ADD_ATTR_DBL(	 myThid, fname, vname, name, atname, dval )
  MNC_VAR_WRITE(	 myThid, fname, vname, var )
  ...
  MNC_VAR_READ(          myThid, vname, var )

  MNC_FILE_CLOSE(	 myThid, fname )

The above interface is powerful yet easy to use (easier than the
entire NetCDF interface) since it helps the user keep track of the
associations between files, "grids", variables, and dimensions.


To-Do:
======

 1) CNH pointed out that grid interpolation needs to be handled
    "on-the-fly" since pre-processing would result in overly large
    input files.  We need an interpolation API...
