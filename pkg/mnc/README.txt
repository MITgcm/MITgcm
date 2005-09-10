
API Discussions:
================

As discussed in our group meeting of 2003-12-17 (AJA, CNH, JMC, AM,
PH, EH3), the NetCDF interface should resemble the following FORTRAN
subroutines:

  1) "stubs" of the form:  MNC_WV_[G|L]2D_R[S|L] ()

  2) MNC_INIT_VGRID('V_GRID_TYPE', nx, ny, nz, zc, zg)

  3) MNC_INIT_HGRID('H_GRID_TYPE', nx, ny, xc, yc, xg, yg)

  4) MNC_INIT_VAR('file', 'Vname', 'Vunits', 'H_GTYPE', 'V_GTYPE', PREC, FillVal)

  5) MNC_WRITE_VAR('file', 'Vname', var, bi, bj, myThid)

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

  MNC_INIT(              myThid )

  MNC_FILE_CREATE(       fname, myThid )
  MNC_FILE_OPEN(         fname, itype, myThid )
  MNC_FILE_ADD_ATTR_STR( fname, atname, sval, myThid )
  MNC_FILE_ADD_ATTR_DBL( fname, atname, len, dval, myThid )
  MNC_FILE_ADD_ATTR_REAL(fname, atname, len, rval, myThid )
  MNC_FILE_ADD_ATTR_INT( fname, atname, len, ival, myThid )
  MNC_FILE_ADD_ATTR_ANY( fname, atname, atype, cs,len,dv,rv,iv, myThid )
  ...
  MNC_FILE_READ_HEADER(  fname, myThid )

  MNC_DIM_INIT(          fname, dname, dlen, myThid )

  MNC_GRID_INIT(         fname, gname, ndim, dnames, myThid )

  MNC_VAR_INIT_DBL(      fname, gname, vname, units, myThid )
  MNC_VAR_INIT_REAL(     fname, gname, vname, units, myThid )
  MNC_VAR_INIT_INT(      fname, gname, vname, units, myThid )
  MNC_VAR_INIT_ANY(      fname, gname, vname, units, type, myThid )
  MNC_VAR_ADD_ATTR_STR(  fname, vname, atname, sval, myThid )
  MNC_VAR_ADD_ATTR_DBL(  fname, vname, atname, nv, dval, myThid )
  MNC_VAR_ADD_ATTR_REAL( fname, vname, atname, nv, rval, myThid )
  MNC_VAR_ADD_ATTR_INT(  fname, vname, atname, nv, ival, myThid )
  MNC_VAR_ADD_ATTR_ANY(  fname, vname, atname, atype, cs,len,dv,rv,iv, myThid )
  MNC_VAR_WRITE_DBL(     fname, vname, var, myThid )
  MNC_VAR_WRITE_REAL(    fname, vname, var, myThid )
  MNC_VAR_WRITE_INT(     fname, vname, var, myThid )
  MNC_VAR_WRITE_ANY(     fname, vname, vtype, dv, rv, iv, myThid )
  ...
  MNC_VAR_READ(          fname, vname, var, myThid )

  MNC_FILE_SYNC(         fname, myThid )
  MNC_FILE_CLOSE(        fname, myThid )


Heres a further "convenience wrapper" written on top of the above UI:

  MNC_CW_INIT(  Gtype, Htype, Hsub, Vtype, Ttype, wHalo, myThid )

    with pre-defined           -      xy    -      -      n
    combinations:              U      x     c      t      y
    'Cen_xy_c_t_Hn'            V      y     i
    'U_xy_i_t_Hn',             Cen
    'Cor_x_-_-_Hy'             Cor

  MNC_CW_SET_UDIM( fbname, udim, myThid )

  MNC_CW_RX_W( 'R|D|I', fbname,bi,bj, vtype, indu, var, myThid )
  MNC_CW_RX_R( 'R|D|I', fbname,bi,bj, vtype, indu, var, myThid )
          |
	  +--  RX = 'RL', 'RS', or 'I'

  MNC_COMP_VTYPE_VAR( ind_vt, ind_fv_ids, ires, myThid )



To-Do:
======

 1) NAMING -- We should (as much as possible) try to name variables so
    that they are in agreement with the CF naming conventions.

 2) UNITS -- as with the names we need to follow conventions

 3) CNH pointed out that grid interpolation needs to be handled
    "on-the-fly" since pre-processing would result in overly large
    input files.  We need an interpolation API...


