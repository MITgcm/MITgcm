Labrador Sea Region with Sea-Ice
=========================================

### Primary test Overview:
This example sets up a small (20x16x23) Labrador Sea experiment
coupled to a dynamic thermodynamic sea-ice model (MITgcm Documentation 8.6.2).

The domain of integration spans $`[280, 320]^\circ`$E and $`[46, 78]^\circ`$N.
Horizontal grid spacing is 2 degrees.
The 23 vertical levels and the bathymetry file

```
  bathyFile      = 'bathy.labsea1979'
```
are obtained from the the 2$`^\circ`$ ECCO configuration.

Integration is initialized from annual-mean Levitus climatology

```
 hydrogThetaFile = 'LevCli_temp.labsea1979'
 hydrogSaltFile  = 'LevCli_salt.labsea1979'
```

Surface salinity relaxation is to the monthly mean Levitus climatology

```
 saltClimFile    = 'SSS.labsea1979'
```

Forcing files are a 1979-1999 monthly climatology computed from the
NCEP reanalysis (see [`SEAICE_PARAMS.h`](https://github.com/MITgcm/MITgcm/blob/master/pkg/seaice/SEAICE_PARAMS.h) for units and signs)

```
  uwindFile      = 'u10m.labsea1979'  # 10-m zonal wind
  vwindFile      = 'v10m.labsea1979'  # 10-m meridional wind
  atempFile      = 'tair.labsea1979'  # 2-m air temperature
  aqhFile        = 'qa.labsea1979'    # 2-m specific humidity
  lwdownFile     = 'flo.labsea1979'   # downward longwave radiation
  swdownFile     = 'fsh.labsea1979'   # downward shortwave radiation
  precipFile     = 'prate.labsea1979' # precipitation
```

The experiment uses `pkg/gmredi`, `pkg/kpp`, `pkg/seaice`, and `pkg/exf`.
The test is a 1-cpu, 10-hour integration. Both the atmospheric
state and the open-water surface fluxes are provided by `pkg/exf`.

More `pkg/seaice` test experiments, configured for low and
high-resolution global cube-sphere domains are described
in `MITgcm_contrib/high_res_cube/README_ice`.

### Lab Sea adjoint
The `code_ad` directory provides files required to compile the adjoint
version of this verification experiment.  This verification
experiment uses the 'divided adjoint'.

To compile the adjoint, one must enable the divided adjoint with the
compile-time flag `USE_DIVA`, the location of which is specified in
the file `build/genmake_local`.
To wit,

```
  USE_DIVA=1
```

To compile the adjoint without the divided adjoint, the compile-time
flag `ALLOW_DIVIDED_ADJOINT` in `code_ad/AUTODIFF_OPTIONS.h` should
be changed from

```
  #define ALLOW_DIVIDED_ADJOINT
```
to

```
  #undef ALLOW_DIVIDED_ADJOINT
```

Note: `testreport` builds in the `lab_sea/build` directory which contains
this `genmake_local` file and so it knows to use the divided adjoint.

## Instructions
Navigate to experiment directory

```
  cd MITgcm/verification/lab_sea
```

### 1-CPU forward experiment
Configure and compile the code:
```
  cd build
  ../../../tools/genmake2 -mods ../code [-of my_platform_optionFile]
 [make Clean]
  make depend
  make
  cd ..
```

To run:
```
  cd run
  ln -s ../input/* .
  ln -s ../build/mitgcmuv .
  ./mitgcmuv > output.txt
  cd ..
```

There is comparison output in the directory:
```
  results/output.txt
```

Use matlab script `lookat_ice.m` to compare the output
 with that from `checkpoint51f` sea-ice code:
```
  cd ../../../verification/lab_sea/matlab
  matlab
  lookat_ice
```

### 2-CPU forward experiment
Configure and compile the code:
```
  cd build
  ../../../tools/genmake2 -mpi -mods ../code [-of my_platform_optionFile]
  ln -s ../code/SIZE.h_mpi SIZE.h
 [make Clean]
  make depend
  make
  cd ..
```

To run:
```
  cd run
  ln -s ../input/* .
  mpirun -np 2 ../build/mitgcmuv
  cd ..
```

### 1-CPU adjoint experiment
Configure and compile the code:
```
  cd build
  ../../../tools/genmake2 -mods ../code_ad [-of my_platform_optionFile]
  make adall
  cd ..
```

To run:
```
  cd run
  ln -s ../input_ad/* .
  ../input_ad/prepare_run
  ln -s ../build/mitgcmuv_ad .
  ./do_run.sh
  cd ..
```

**Note:** `prepare_run` shell script is also used when running `testreport` (see below)
and could be replaced by these 2 commands:
```
  ln -s ../input/* .
  ln -s ../../isomip/input_ad/ones_64b.bin .
```
And the overly simple shell script "do_run.sh" just executes four times
(as specified in file "run_ADM_DIVA", `add_DIVA_runs = 4`) `mitgcmuv_ad`, saving
output in intermediate files "output_adm.txt.diva_0,1,2,3", plus a final time:
```
  mitgcmuv_ad > output_adm.txt
```
where output file `output_adm.txt` can be compared with reference output:
```
  results/output_adm.txt
```

## Secondary tests
In addition to the primary tests described above, 5 secondary forward tests and
and 2 secondary adjoint tests can be run using the same executable as the corresponding
primary tests but with specific input parameter files (in `input.$st\` and `input_ad.$st\`).
The secondary forward tests include alternative seaice model formulations:
free-drift in `input.fd/`; using EVP and `useHB87stressCoupling` in `input.hb87/` ;
with `pkg/salt_plume` in `input.salt_plume/`;
and two other ice-free "North-Altlantic box" set-up (formerly in `verification/natl_box/`)
in `input.natl_box\` and with `pkg/longstep` in `input.longstep/`.
The secondary adjoint tests are simpler version of the primary adjoint test,
without seaice in `input_ad.noseaice/` and without seaice dynamics in `input_ad.noseaicedyn/`.

### Instruction to run secondary tests
Run the testscript _forward_ experiments:

```
  cd MITgcm/verification
  ./testreport -t lab_sea [-of my_platform_optionFile]
```

Standard testreport output, with all secondary tests:
```
default 10  ----T-----  ----S-----  ----U-----  ----V-----  --PTR 01--  --PTR 02--  --PTR 03--  --PTR 04--  --PTR 05--
G D M    c        m  s        m  s        m  s        m  s        m  s        m  s        m  s        m  s        m  s
e p a R  g  m  m  e  .  m  m  e  .  m  m  e  .  m  m  e  .  m  m  e  .  m  m  e  .  m  m  e  .  m  m  e  .  m  m  e  .
n n k u  2  i  a  a  d  i  a  a  d  i  a  a  d  i  a  a  d  i  a  a  d  i  a  a  d  i  a  a  d  i  a  a  d  i  a  a  d
2 d e n  d  n  x  n  .  n  x  n  .  n  x  n  .  n  x  n  .  n  x  n  .  n  x  n  .  n  x  n  .  n  x  n  .  n  x  n  .

Y Y Y Y>11<13 16 16 16 16 16 16 14 13 13 13 16 16 16 12 14 22 16 16 16 22 16 16 16 pass  lab_sea
Y Y Y Y>12<16 16 16 16 16 16 16 16 16 13 13 16 14 16 13 16 22 16 16 16 22 16 16 16 pass  lab_sea.fd
Y Y Y Y> 4< 9 10  9  9 16 13 11  8  6  8  4  5  7  8  4  6 22  7  7  7 22  6  7  7 FAIL  lab_sea.hb87
Y Y Y Y 11 16 16 16 14 16 16 16 16 16 13 12 14 16 13 12 14 16 16 16>16<pass  lab_sea.longstep
Y Y Y Y>11<16 16 16 16 16 16 16 16 13 12 12 14 14 13 12 14 pass  lab_sea.natl_box
Y Y Y Y>13<16 16 16 16 16 16 16 14 16 14 13 14 16 14 13 16 22 16 16 16 22 16 16 16  pass  lab_sea.salt_plume
```

**Note:** Some differences in accuracy occur across different platforms as seen
here for secondary test "lab_sea.hb87".

Run the testscript _adjoint_ experiments:

```
  cd MITgcm/verification
  ./testreport -t lab_sea -ad [-of my_platform_optionFile]
```

Standard adjoint testreport output, with all secondary tests:
```
Adjoint generated by TAF Version 6.5.1

default    10     ----T-----  ----S-----  ----U-----  ----V-----
G D M    C  A  F        m  s        m  s        m  s        m  s
e p a R  o  d  D  m  m  e  .  m  m  e  .  m  m  e  .  m  m  e  .
n n k u  s  G  G  i  a  a  d  i  a  a  d  i  a  a  d  i  a  a  d
2 d e n  t  r  r  n  x  n  .  n  x  n  .  n  x  n  .  n  x  n  .

Y Y Y Y 16>16<16 16 14 16 16 16 16 16 16 16 16 16 14 14 16 16 16 pass  lab_sea  (e=0, w=0, lfd=1, dop=1, sm=1)
Y Y Y Y 14>15< 6 16 16 16 13 16 13 13 11 13 13 12 13 11 11 12 12 pass  lab_sea.noseaice
Y Y Y Y 16>13<16 16 16 14 16 16 13 13 16 16 16 16 14 14 16 14 16 pass  lab_sea.noseaicedyn
```
