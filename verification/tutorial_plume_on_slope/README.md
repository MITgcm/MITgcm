Tutorial Example: "Gravity plume on a continental slope"
========================================================
(formerly "plume_on_slope" verification ;
 also "nonhydrostatic_plume_on_slope" in release.1 branch)

### Overview:
This is a 2D set-up with (variable) high-resolution and non-hydrostatic dynamics, where dense water is produced on a shelf that then flows as a gravity current down the slope. 

The **primary** test uses a no-slip bottom boundary condition (`no_slip_bottom=.TRUE.`) and no explicit drag. 

The **secondary** test `rough.Bot` uses the logarithmic law of the wall to compute the drag coefficient for quadratic bottom drag as a function of distance from the bottom (i.e. cell thickness) and a prescribed roughness length `zRoughBot = 0.01` (in meters). For this configuration (i.e. vertical grid spacing) this value of `zRoughBot` corresponds to approximately `bottomDragQuadratic=5.E-2`. For consistency, the bottom boundary conditions is set to free slip (`no_slip_bottom=.FALSE.`).

## Instructions
Configure and compile the code:

```
  cd build
  ../../../tools/genmake2 -mods ../code [-of my_platform_optionFile]
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

To run the **secondary** test `roughBot`:

```
  cd run
  rm *
  ln -s ../input.roughBot/* .
  ln -s ../input/* .
  ../build/mitgcmuv > output.txt
```

There is comparison output in the directory:

```
  results/output.txt
  results/output.roughBot.txt
```

## Comments
  The input data is `real*8` and generated using the MATLAB script
  gendata.m.

