The following utilities are available in this directory:

  cm_landwater.m            :: creates colormap for plotting bathymetry/orography
  contourfv.m               :: customized contourf version (for finite volume topo)
  densjmd95.m               :: calculate in-situ density (Jackett & McDougall 1995)
  densmdjwf.m               :: calculate in-situ density (McDougall et.al. 2003)
  dens_poly3.m              :: calculate in-situ density as MITgcm POLY3
  gluemnc.m                 :: glue together different tiles into a global file
  grepread.m                :: a new (from Aug 2001) useful script
  griddata_fast.m           :: script to help use cube and netcdf
  griddata_preprocess.m     :: script to help use cube and netcdf
  ini_poly3.m               :: calculate in-situ density as MITgcm POLY3
  inpaint_nans.m            :: used by interpickups.m (for topography mask)
  interpickups.m            :: interpolate mnc pickup files
  ioLb2num.m                :: convert from ptracer io-label to ptracer number
  load_grid.m               :: load MITgcm output grid-files into one structure array
  longitude.m               :: new (from Aug 2001) script for plotting cube stuff.
  mnc_assembly.m            :: assemble MNC tiles into single NetCDF files
  nc_add.m                  :: script to help use cube and netcdf
  num2ioLb.m                :: convert from ptracer number to ptracer io-label
  pcolorfv.m                :: customized pcolor version (for finite volume topo)
  rdmds.m                   :: read MDSIO output (data & meta) files
  rdmeta.m                  :: read MDSIO output meta file
  rdmnc.m                   :: read tiled MNC output files
  stats.m                   :: calculate and print basic statistics of 1 array
  test_nc_add.m             :: script to help use cube and netcdf


Subdirectories:

  Graphix                   :: cubed-sphere data manipulation and plotting package
  cs_grid                   :: cubed-sphere and lat-lon-cap routines
  gmt                       :: general MITgcm tiles/topologies
  ocean_basin               :: basin mask generation routines


And more complete directions for each script are:

===  mnc_assembly.m  ===

  This utility is available for assembling the data from one or more
  variables contained in multiple MNC tiles (each having their own
  file) into a single NetCDF file.  It understands both "old-style"
  MITgcm exch1 topologies and newer exch2 topologies.  A complete set
  of examples demonstrating the use of this tool on both exch1 and
  exch2 data sets is available from the MITgcm.org CVS server at:

    $ export CVSROOT=':pserver:cvsanon@mitgcm.org:/u/gcmpack'
    $ cvs login
    $ cvs co development/edhill/mnc_assembly
