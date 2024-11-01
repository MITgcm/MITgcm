Gray atmosphere physics example on Cubed-Sphere grid
============================================================

Use gray atmospheric physics (O'Gorman and Schneider, JCl, 2008)
from package `atm_phys` inside  MITgcm dynamical core, in a global
cubed-sphere grid set-up (6 faces 32x32, 26 levels, non uniform deltaP).

### Overview:
This experiment contains 2 aqua-planet like set-ups (with corresponding `input[.*]/` dir)
that can be run with the same executable (built from `build/` dir using customized
code from `code/`); binary input files have been generated using matlab script
`gendata.m` from the `input` dir.

The **primary** test, using input files from `input/` dir,
has an interactive SST with a 10m mixed layer depth and a prescribed,
time-invariant Q-flux.  It also includes a weak damping of stratospheric winds.
This test experiment starts from a spin-up state using pickup files (@ t= 1.yr).

The **secondary** test `input.ape/` dir is the same as the primary test but
use prescribed idealized SST from Neale and Hoskins, 2001, Aqua-Planet Experiment (APE) project
without stratospheric wind damping.

### Instructions:
Configure and compile the code:

```
  cd build
  ../../../tools/genmake2 -mods ../code [-of my_platform_optionFile]
  make depend
  make
  cd ..
```

To run primary test:

```
  cd run
  ln -s ../input/* .
  ./prepare_run
  ../build/mitgcmuv > output.txt
```

There is comparison output in the directory:

  `results/output.txt`

To run secondary "ape" test:

```
  cd run
  rm *
  ln -s ../input.ape/* .
  ln -s ../input/* .
  ./prepare_run
  ../build/mitgcmuv > output.txt
```
There is comparison output in the directory:

  `results/output.ape.txt`

### Notes:
There special compiler options inside `build/genmake_local` to compile some
of the files from `pkg/atm_phy` that requires to explicitly provide an optfile
to `genmake2` command.
