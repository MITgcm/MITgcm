# Global Ocean Simulation at 4 degree Resolution, Adjoint Set-Up
First configuration to use OpenAD, started on 2005-08-19
 by heimbach@mit.edu, utke@mcs.anl.gov, cnh@mit.edu
***Note*** this experiment was previously named "OpenAD".

### Overview:
This experiment is derived from `tutorial_global_oce_latlon` (see also `global_ocean.90x40x15`
 and `global_ocean_ebm`) but with different combinations of pkgs or options from
 `global_ocean.90x40x15` adjoint test exp.
It provides adjoint settings for 3 AD compilers, OpenAD, TAF and Tapenade, with primary
test input files in `input_oad/`, `input_ad/` and `input_tap/` respectively, but also
several secondary test setting with each of the AD compilers.

## Part 1, using OpenAD Adjoint Compiler:

The built process needed to be modified, and some routines
needed changes. Most changes were commited to default routines,
the remaining changes are kept in `code_oad/` for now.

### Instructions:
Configure and compile the code:
```
  cd build
  ../../../tools/genmake2 -mods ../code_oad [-of my_platform_optionFile]
 [make Clean]
  make adAll
  cd ..
```

***Note:*** might want to split the full single step above (make adAll) in
several intermediate steps such as:
```
  make cb2m
  make makefile
  make small_f
  make allmods
  make adAll
```
where the first step (cb2m) invoke script to convert COMMON block headers
(e.g.. FILE.h) to MODULE headers (FILE_mod.h) and create new module file
(FILE_mod.F90); the second step (makefile) re-generate makefile which takes
into account newly created files FILE_mod.h, FILE_mod.F90 ; the third step
(small_f) generate `.f` and `.f90` files. The fourth step (allmods)
compiles all module files .f90 ; and the fifth and last step complile all f90
src files.

To run primary test:
```
  cd run
  ln -s ../input_oad/* .
  ../input_oad/prepare_run
  ../build/mitgcmuv_ad > output_oadm.txt
  cd ..
```

There is comparison output in the directory:
  `results/output_oadm.txt`

To run any of secondary `$st` test (`$st` in: `ggl90`, `kpp`):
```
  cd run
  rm *
  ln -s ../input_oad.$st/* .
  ln -s ../input_oad/* .
  ./prepare_run
  ../build/mitgcmuv_ad > output_oadm.txt
```

There is comparison output in the directory:
  `results/output_oadm.$st.txt`

## Part 2, using TAF Adjoint Compiler:
similar to above but using set-up specific code from `code_ad/` and input files from `input_ad/`

### Instructions:
Configure and compile the code:
```
  cd build
  ../../../tools/genmake2 -mods ../code_ad [-of my_platform_optionFile]
 [make Clean]
  make depend
  make adall
  cd ..
```

To run primary test:
```
  cd run
  ln -s ../input_ad/* .
  ../input_ad/prepare_run
  ../build/mitgcmuv_ad > output_adm.txt
  cd ..
```

There is comparison output in the directory:
  `results/output_adm.txt`

To run any of secondary `$st` test (`$st` in: `ggl90`, `w_exf`):
```
  cd run
  rm *
  ln -s ../input_ad.$st/* .
  ln -s ../input_ad/* .
  ./prepare_run
  ../build/mitgcmuv_ad > output_adm.txt
```

There is comparison output in the directory:
  `results/output_adm.$st.txt`

***Notes:*** `input_ad.w_exf` set-up has been moved (in PR #830)
from `verification/global_with_exf/input_ad`

## Part 3, using Tapenade Adjoint Compiler:
similar to above but using set-up specific code from `code_tap/` and input files from `input_tap/`

