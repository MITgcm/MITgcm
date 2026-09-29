Global ocean on Lat-Lon grid using forcing from pkg/exf
=========================================================
This experiment has been moved (PR #830, #944):
1. inside `global_oce_latlon` (Forward primary and secondary set-up)
   and is now run there.
2. inside `global_oce_latlon` (Adjoint set-ups) and is now run there
   as a secondary test ("global_oce_latlon.w_exf")
   using `input_ad.w_exfs/` and `input_tap.w_exf/`.

Original Description:
=====================

 heimbach@mit.edu 07-Nov-2002.
This verification experiment is almost identical to the
`tutorial_global_oce_latlon` experiment, except that it uses
the `exf` (and `cal`) package instead of the
external_fields_load routine.
To obtain identical results between the two experiments,
the following 2 modifications are necessary:
1. in `external_fields_load.F`:
   replace the line
      `Imytm=int(myTime*rdt+0.5)`
   by
      `Imytm=int((myTime+1296000.)*rdt+0.5)`
2. in `exf_set_climsst.F`:
   comment the 'quality' check, i.e. avoid the setting
   to freezing temperature if less than `climtempfreeze`.

================================

menemenlis@jpl.nasa.gov 05-Aug-2003

Input-field spatial interpolation has been added to `pkg/exf`.
It is enabled by defining CPP option `USE_EXF_INTERPOLATION`
in `EXF_CPPOPTIONS.h` or in `ECCO_CPPOPTIONS.h`
Both bi-linear and bi-cubic interpolation schemes are supported.

This package is a placeholder until a more general coupler
is made available by the ESMF project.  The output grid can be
arbitrary (cubed-sphere should be OK), but it is assumed that
the input grid is Cartesian with arbitrary latitude increments
in the y-direction and with equidistant longitude increments in
the x-direction.  The input grid must encompass the complete
output grid in the y-direction (i.e., extrapolation is not
supported).  It is also assumed that the grid is periodic in
the x-direction; to use the interpolation routine with a
non-periodic domain, make sure there is sufficient padding
at the edges, i.e., two points for bicubic and one for bilinear
interpolation.

The `verification/global_with_exf` experiment has been modified to use
the `USE_EXF_INTERPOLATION` option, the input grids being defined by variables
`*_lon0`, `*_lon_inc`, `*_lat0`, `*_lat_inc`, `*_nlon` and `*_nlat`
in `input/dat.exf` and `input/data.exf_clim`

```
   *_lon0, *_lat0    :: lon and lat of sw corner of global input grid
   *_lon_inc         :: scalar x-grid increment
   *_lat_inc         :: vector y-grid increments
   *_nlon and *_nlat :: input x-grid and y-grid size
```
In this particular example the input and output grids are
the same, so the results of the verification experiment remain
unchanged from before.
