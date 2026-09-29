# Global Ocean Simulation at 4 degree Resolution, Alternative Forcing
Quasi-global ocean set-up, $`80^\circ`$S -- $`80^\circ`$N, at $`4^\circ`$
horizontal resolution with 15 vertical levels.

This experiment has been moved (PR #944) inside `global_oce_latlon`,
and is now run there as a secondary test (`global_oce_latlon.ebm`) using
`input.ebm/` and `input_ad.ebm/` for the forward and adjoint test respectively.

Original Description:
=====================

### Overview:
This experiment is derived from `tutorial_global_oce_latlon` (see also
`global_ocean.90x40x15`) and relies on Energy-Balance Model
package (`pkg/ebm`) to compute oceanic surface forcing.<br>

The adjoint test, with customized code in `code_ad` and input files in `input_ad/` dir, provides
an adjoint set-up of the forward primary tests with some adjoint specific simplifications
(less compiled pkgs, simpler GM-Redi code, no time-dependent control ...).

### Instructions for Forward tests:
Configure and compile the code:
```
  cd build
  ../../../tools/genmake2 -mods ../code [-of my_platform_optionFile]
 [make Clean]
  make depend
  make
  cd ..
```

To run primary test:
```
  cd run
  ln -s ../input/* .
  ../build/mitgcmuv > output.txt
  cd ..
```

There is comparison output in the directory:
  `results/output.txt`

### Instructions for Adjoint tests:
Note: This requires access to a TAF license.<br>
Configure and compile the code:
```
  cd build
  ../../../tools/genmake2 -mods ../code_ad [-of my_platform_optionFile]
 [make Clean]
  make depend
  make adall
  cd ..
```

To run:
```
  cd run
  ln -s ../input_ad/* .
  ../input_ad/prepare_run
  ../build/mitgcmuv_ad > output_adm.txt
  cd ..
```

There is comparison output in the directory:
  `results/output_adm.txt`
