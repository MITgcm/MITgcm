# Description and building instructions for optim.x

This directory contains code to compile the optimization routine `optim.x`.

Note, that there is an alternative optimization package [optim_m1qn3](
https://github.com/mjlosch/optim_m1qn3) that is based on the latest (and last)
version of
[m1qn3](https://who.rocq.inria.fr/Jean-Charles.Gilbert/modulopt/optimization-routines/m1qn3/m1qn3.html).

`optim.x` requires the library `lsopt_ecco` and the BLAS library `blas`. The
build procedure is a two step process (first build `lsopt_ecco` and `blas`, and
then `optim.x` ) and by no means foolproof. It requires that you generate a
makefile in this directory and modify the sample `Makefile` in `../lsopt`. For
illustration purposes, let us assume that we want to generate an `optim.x` for
`myExp=tutorial_global_oce_optim` in `MITgcm/verification`.

## Building library `lsopt_ecco` and `blas`

In `MITgcm/lsopt` adjust the compiler and compiler flags in `Makefile`. Using
the same compiler and flags as for building the `mitgcmuv_ad` executable in
`$myExp` is probably the best guess, see also below how the `MITgcm/optim`
`Makefile` is generated. The default works for a standard Ubuntu system, but
not e.g. for a Mac. There's a makefile `MITgcm/lsopt/Makefile_macos` that has
worked for MacOS, but may require adjustment. After adjusting the makefile,
compile the libraries like this:

```
cd ../lsopt
make
```
The resulting libraries `lsopt_ecco` and `blas` will be used in the second
step.

## Building `optim.x`

To generate the makefile based on the setting in `$myExp` and to build
`optim.x`, change into `optim` and run

```
cd ../optim
./prep_make ../../MITgcm/verification/${myExp}/build
make clean
make depend
make
```

`prep_make` generates a local `Makefile` from `makefile_templ` and from
`$myExp` `build/Makefile`. It fills some `makefile_templ` placeholder
`_GET_keyWord` with the corresponding option/parameter value "keyWord" found in
the build/Makefile (the current list of keyWords is: `BLD_DIR`, `EXTRA_OPT`,
`CPPCMD`, `SFX`, `FC`, `FFLAGS` and `FOPTIM`).  A simple usage description is
returned when typing `./prep_make` alone.

In some cases you may have to adjust `makefile_templ`(e.g. for the path to a
non-standard `makedepend`) before running `prep_make`.
Note that the `Makefile` generated using `prep_make` option `-fake` is only for
testing purpose, to get a fake `optim.x` that just reads gradient vector files
without any lsopt pieces.

It may make sense to first generate the makefile in `MITgcm/optim` with
`prep_make`, and then use the parameters in the generated `Makefile` to adjust
the sample `Makefile` in `MITgcm/lsopt`, build the libraries, and then
return to `MITgcm/optim` to build `optim.x`.

## Backward compatibility

With PR #[796](https://github.com/MITgcm/MITgcm/pull/796), the header format of
the control and gradient vector files and the length of the stored fields has
changed. To read old gradient vector files (typically called
`ecco_cost_MIT_CE_000.opt0000` for the first optimisation call) with `optim.x`,
define CPP flag `READ_OLD_CTRL_PACK_FILE` in `CTRL_OPTIONS.h` before compiling
`optim.x`. The resulting `optim.x` will read the old format gradient vector and
write the new control vector (typically called `ecco_ctrl_MIT_CE_000.opt0001`)
in the new format. After the new control vector has been written,
`READ_OLD_CTRL_PACK_FILE` should be reset to `undef` again (and `optim.x`
recompiled) for the next optimisation.

## Old README

The following is the content of the old README. It describes some sort of
interface, i.e. the header of the control and gradient vectors written and read
by the `mitgcmuv_ad` (and `optim.x`), see `optim_readdata.F` and
`optim_writedata.F`. More details can be found in the [online
manual](https://mitgcm.readthedocs.io/en/latest/ocean_state_est/ocean_state_est.html#the-line-search-optimisation-algorithm)
(Chapter 10).

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
