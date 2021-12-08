This directory contains code to compile the optimization routine
`optim.x`.

Note, that there is an alternative optimization package [optim_m1qn3](
https://github.com/mjlosch/optim_m1qn3) that is based on the latest
(and last) version of
[m1qn3](https://who.rocq.inria.fr/Jean-Charles.Gilbert/modulopt/optimization-routines/m1qn3/m1qn3.html).

`optim.x` requires the library `lsopt_ecco` and the BLAS library
`blas`. The build procedure is a two step process (first build
`lsopt_ecco` and then `optim.x` ) and by no means foolproof. It
requires that you modify the sample Makefile in this directory and in
`../lsopt`:

1. both here and in `../lsopt` adjust the compiler and compiler flags
   in the Makefiles. Using the same compiler and flags as for building
   the "mitgcmuv_ad" executable is probably the best guess. The
   default works for a standard Ubuntu system, but not e.g. for a Mac.

2. add the path to the build directory where you have built your
   mitgcmuv_ad. This is necessary for the Makefile to pull the size of
   the control vector and other things.

3. compile like this:
```
cd ../lsopt
make
cd ../optim
make
```

The default setting in `lsopt/Makefile` and `optim/Makefile` can be
used on an Ubuntu system (last tested on Ubuntu 20.04.3 LTS with gcc
9.3.0) to compile `lsopt` and `optim` after running the
`tutorial_global_oce_optim` like this:

```
cd MITgcm/verification
./testreport -t tutorial_global_oce_optim -adm -ncad -devel
cd ../lsopt
make
cd ../optim
make
```
because `INCLUDEDIRS` points to `../verification/tutorial_global_oce_optim/build/`.

This is the content of the old README. It describes some sort of
interface, i.e. the header of the control and gradient vectors written
and read by the `mitgcmuv_ad` (and "optim.x"), see `optim_readdata.F`
and `optim_writedata.F`. More details can be found in the [online
manual](https://mitgcm.org/documentation) (Chapter 10).
```
c     expid           - experiment name
c     optimcycle      - optimization no.
c     missing value   - missing value identifier (usually -9999.)
c     ig              - global start index x (zonal)
c     jg              - global start index y (merid.)
c     nsx             - no. of x-subgrids
c     nsy             - no. of y-subgrids

 >>> MISSING: <<<
c     nr              - no. of z-points vertical
c     snx
c     sny
c     nvartype
c     nvarlength
 >>> <<<

c     maxcvars        - Number of control variables
c                       (currently 6; 2 init. + 4 bound.)
      integer     maxcvars
      parameter ( maxcvars = 20 )

c     ncvarindex      - "arbitrary" index to define variable:
c                       * 101: initial temp.
c                       * 102: initial sali.
c                       * 103: heat flux
c                       * 104: freshwater flux
c                       * 105: u stress (zonal)
c                       * 106: v stress (merid.)
      integer ncvarindex    ( maxcvars )

c     ncvarrecs       - no. of records in control vector
c                       * = 1                      for init. temp./sali.
c                       * = endrec - startrec + 1  for fluxes
      integer ncvarrecs     ( maxcvars )

c     ncvarrecstart   - first record:
c                       * NOT DEFINED  for init. temp./sali.
c                       * = startrec   for fluxes
      integer ncvarrecstart ( maxcvars )

c     ncvarrecsend    - last record:
c                       * NOT DEFINED  for init. temp./sali.
c                       * = endrec     for fluxes
      integer ncvarrecsend  ( maxcvars )

c     ncvarxmax       - no. of x-points in subgrid (zonal)
c                       = snx
      integer ncvarxmax     ( maxcvars )

c     ncvarymax       - no. of y-points in subgrid (merid.)
c                       = sny
      integer ncvarymax     ( maxcvars )

c     ncvarnrmax      - no. of z-points (vert.)
c                       * = nr  for init. temp./sali.
c                       * = 1   for fluxes
      integer ncvarnrmax    ( maxcvars )

c     nwet[c/s/w]tile - Number of wet points in a tile for center (c),
c                       south (s), and western (w) mask, resp.
      integer nwetctile     ( nsx, nsy, nr )
      integer nwetstile     ( nsx, nsy, nr )
      integer nwetwtile     ( nsx, nsy, nr )

c     ncvargrd        - position in grid
c                       * = 'c' : (center,center)
c                       * = 's' : (center,south)
c                       * = 'w' : (west,center)
      character*(1) ncvargrd(maxcvars)

```
