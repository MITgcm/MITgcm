
The following utilities are available in this directory:

  contourfv.m               ::
  cube2latlon.m             ::
  cube2latlon_fast.m        ::
  cube2latlon_preprocess.m  ::
  dens_poly3.m              ::
  displaytiles.m            ::
  drawedges.m               ::
  grepread.m                ::
  griddata_fast.m           ::
  griddata_preprocess.m     ::
  ini_poly3.m               ::
  longitude.m               ::
  merccube.m                ::
  mnc_assembly.m            :: assemble MNC tiles into single NetCDF files
  nc_add.m                  ::
  pcolorfv.m                ::
  permutetiles.m            ::
  plotcube.m                ::
  rdmds.m                   ::
  rdmeta.m                  ::
  rdmnc.m                   ::
  stats.m                   ::
  test_nc_add.m             ::
  tiles.m                   ::
  uvcube2latlon.m           ::
  uvcube2latlon_fast.m      ::



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
