
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

  MNC_FILE_CREATE(       myThid, fname )
  MNC_FILE_OPEN(         myThid, fname, itype )
  MNC_FILE_ADD_ATTR_STR( myThid, fname, atname, sval )
  MNC_FILE_ADD_ATTR_DBL( myThid, fname, atname, len, dval )
  MNC_FILE_ADD_ATTR_REAL(myThid, fname, atname, len, rval )
  MNC_FILE_ADD_ATTR_INT( myThid, fname, atname, len, ival )
  MNC_FILE_ADD_ATTR_ANY( myThid, fname, atname, atype, cs,len,dv,rv,iv )
  ...
  MNC_FILE_READ_HEADER(  myThid, fname )

  MNC_DIM_INIT(          myThid, fname, dname, dlen )

  MNC_GRID_INIT(         myThid, fname, gname, ndim, dnames )

  MNC_VAR_INIT_DBL(      myThid, fname, gname, vname, units )
  MNC_VAR_INIT_REAL(     myThid, fname, gname, vname, units )
  MNC_VAR_INIT_INT(      myThid, fname, gname, vname, units )
  MNC_VAR_INIT_ANY(      myThid, fname, gname, vname, units, type )
  MNC_VAR_ADD_ATTR_STR(  myThid, fname, vname, atname, sval )
  MNC_VAR_ADD_ATTR_DBL(  myThid, fname, vname, atname, nv, dval )
  MNC_VAR_ADD_ATTR_REAL( myThid, fname, vname, atname, nv, rval )
  MNC_VAR_ADD_ATTR_INT(  myThid, fname, vname, atname, nv, ival )
  MNC_VAR_ADD_ATTR_ANY(  myThid, fname, vname, atname, atype, cs,len,dv,rv,iv )
  MNC_VAR_WRITE_DBL(     myThid, fname, vname, var )
  MNC_VAR_WRITE_REAL(    myThid, fname, vname, var )
  MNC_VAR_WRITE_INT(     myThid, fname, vname, var )
  MNC_VAR_WRITE_ANY(     myThid, fname, vname, vtype, dv, rv, iv )
  ...
  MNC_VAR_READ(          myThid, fname, vname, var )

  MNC_FILE_SYNC(         myThid, fname )
  MNC_FILE_CLOSE(        myThid, fname )


Heres a further "convenience wrapper" written on top of the above UI:

  MNC_CW_INIT(  myThid, Gtype, Htype, Hsub, Vtype, Ttype, wHalo )

    with pre-defined           -      xy    -      -      n
    combinations:              U      x     c      t      y
    'Cen_xy_c_t_Hn'            V      y     i
    'U_xy_i_t_Hn',             Cen
    'Cor_x_-_-_Hy'             Cor

  MNC_CW_WRITE( myThid,myIter, filebn,bi,bj, Gtype, RX, vname, var )
  MNC_CW_READ(  myThid,myIter, filebn,bi,bj, Gtype, RX, vname, var )



To-Do:
======

 1) NAMING -- We should (as much as possible) try to name variables so
    that they are in agreement with the CF naming conventions.

 2) UNITS -- as with the names we need to follow conventions

 3) AM described her "diags" (or "myDiags" or "mDiags") interface
    which should use MNC for output.  The data storage idea is similar
    to the MNC tables-of-indicies approach but also includes one huge
    double-precision "accumulator" to hold all the temporary values
    (eg. partial sums for averages, current max/mins):

      vname ( ni ) 
      vlen  ( ni ) 
      vind  ( ni ) ------+
                         |

      vij_diag ( i, j, [...] )  w/ lat/lon indicies
      vgl_diag ( [...] )        wo/ lat/lon indicies (global)

 4) CNH pointed out that grid interpolation needs to be handled
    "on-the-fly" since pre-processing would result in overly large
    input files.  We need an interpolation API...

 5) From the group meeting on 2004/01/21, we need to define
    "sub-grids" corresponding to:

         var_name       HGRID       VGRID       TIME
         ===========================================
         u              U_xy        r_c         t
         eta            T_xy        -           t
         rac            T_xy        -           -

    And write a convenience wrapper so that users can write variables
    using just two function calls.  JMC and I worked out the
    following:


