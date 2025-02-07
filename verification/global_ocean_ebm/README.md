# Global Ocean Simulation at 4 degree Resolution, Alternative Forcing
Quasi-global ocean set-up, $`80^\circ`$S -- $`80^\circ`$N, at $`4^\circ`$
horizontal resolution with 15 vertical levels.

### Overview:
This experiment is derived from `tutorial_global_oce_latlon` (see also
`global_ocean.90x40x15`) with surface forcing provided by specific pkgs, either
`pkg/ebm` or `pkg/exf`, instead of relying on the main model surface forcing capability.
It contains 3 forward set-up, all using the same executable built from `code`
config but with specific input files from `input/` (primary test) and,
as secondary tests, from `input.w_exf\` and `input.yearly/`.
An adjoint set-up configuration for TAF AD compiler, is also provided as an
example with simple control (initial Temperature, `xx_theta`).

The **primary** forward test, using input files from `input/` dir, relies on Energy-Balance Model
package (`pkg/ebm`) to compute oceanic surface forcing.<br>
The adjoint test, with customized code in `code_ad` and input files in `input_ad/` dir, provides
an adjoint set-up of the forward primary tests with some adjoint specific simplifications
(less compiled pkgs, simpler GM-Redi code, no time-dependent control ...).

The two **secondary** tests, using input files from `input.w_exf\` and `input.yearly/`,
use prescribed monthly-mean air-sea surface fluxes from `pkg/exf`.<br>
**Note:**
1.  these 2 set-up have been moved (in PR #830) from `verification/global_with_exf/`
    where a "README" still provides some details related to `pkg/exf` specific
    features used here.
2.  the ability, using `pkg/exf`, to compute surface fluxes from near surface
    atmospheric state and downward radiation as shown, e.g., in experiment
    `global_ocean.cs32x15` (secondary test `input.seaice` or `input.icedyn` or `input.in_p`)
    is not used here (`#undef ALLOW_BULKFORMULAE`).

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

To run any of secondary `$st` test (`$st` in: `w_exf`, `yearly`):
```
  cd run
  rm *
  ln -s ../input.$st/* .
  ln -s ../input/* .
  ./prepare_run
  ../build/mitgcmuv > output.txt
```

There is comparison output in the directory:
  `results/output.$st.txt`

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
