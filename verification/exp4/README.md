Example: "Flow over a bump with Open Boundaries and passive tracers"
====================================================================

This experiment is a 400 x 210 x 4.5 km channel (80 x 42 x 8 grid-points)
on an f-plane with a tall seamount in the middle.
It is intended to illustrate the interaction of a zonal flow over and around
a steep bump as well as the use of Open Boundary (`pkg/obcs`) and Floats
packages (`pkg/flt`).

### Overview:
This experiment contains 4 set-ups (with corresponding `input[.*]/` dir) that
can be run with the same executable (built from `build/` dir using customized
code from `code/`);
binary input files (all `real*8`) have been generated using matlab script
`gendata.m` from `input` dir.
All four set-ups use a simple EOS ( $\rho' = -\rho_0 ~ \alpha_T ~ \theta'$ )
and treat salt as a passive tracer ;

The **primary** test, using input files from `input/` dir, use four open
boundaries with simple specifications (`useOBCSprescribe`) from open boundary
parameter file `data.obcs`.
This is a non-hydrostatic set-up using the flux-form momentum equations.

Different kinds of open boundary values are used:
zonal (x-)velocity U is prescribed at all open boundaries with values that are
read from data files (specified in data.obcs);
meridional (y-)velocity V is set to zero on all boundaries, and temperature to
`tRef(z)`, both `in obcs_calc.F`, this is the default behavior;
at the western boundary, salinity values are used for salinity and one passive
tracer in the same way.
Salinity is set to sLev at all other boundaries, while a (nearly) homogeneous
Neumann condition is applied to the passive tracer (the latter is the default
in `obcs_calc.F`), with a relaxation (using pkg rbcs) in the Eastern part of
the channel.

The **secondary** test, using `input.nlfs/` dir, is similar to the primary test except
it is hydrostatic with the vector-invariant momentum formulation and using the
`z*` coordinate. A time-varying small imbalance between the Western boundary
inflow and Eastern boundary outflow generates sea-level fluctuations.

The **secondary** test, using `input.stevens/` dir, is very similar to the primary test
except it is hydrostatic with some bottom friction and use Stevens Open-Boundary
Condition formulation at the Eastern and Western edges.

The **secondary** test, using `input.with_flt/` dir, is intended to illustrate the use
of `pkg/flt` in a simple configuration (hydrostatic, no Open-Boundary but force
with a zonal wind-stress instead).
***Note:*** this set-up has been moved (in PR #830) from `verification/flt_example/`

### Instructions:
default paths are (as used in `testreport`):

```
  set build_dir = ${cwd}/build
  set run_dir   = ${cwd}/run
  set MITGCM    = ${cwd}/../../
```

Configure and compile the code:

```
  cd ${build_dir}
  ${MITGCM}/tools/genmake2 -mods ../code [-of my_platform_optionFile]
 [make Clean]
  make depend
  make
  cd ..
```

To run primary test:

```
  cd ${run_dir}
  ln -s ../input/* .
  ${build_dir}/mitgcmuv > output.txt
  cd ..
```

There is comparison output in the directory:
 results      ( absolute path: `${run_dir}/../results` )

To run any of secondary `$st` test (`$st` in: `nlfs`, `stevens`, `with_flt`):

```
  cd ${run_dir}
  rm *
  ln -s ../input.$st/* .
  ln -s ../input/* .
  ${build_dir}/mitgcmuv > output.txt
  cd ..
```

There is comparison output in the directory:

  `results/output.$st.txt`

### Notes:
