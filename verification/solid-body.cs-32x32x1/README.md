Simple solid-body rotation test on cubed-sphere grid
========================================================

### Overview:
This is a single level, steady-state example on cubed-sphere (cs-32) grid with initial zonal wind field $U(\phi)$ and surface pressure anomaly $\eta(\phi)$ , both dependent on latitude $\phi$ only,  that corresponds to an additional relative rotation ($\omega\'$) on top of the solid-planet rotation ($\Omega$) and around the same axis:

$$ U(\phi) = U_{eq} ~ \cos( \phi ) ~~~ \mathrm{with:} ~~~ U_{eq} = \omega' \times R $$

$$ \eta(\phi) = \rho_{const} ~ U_{eq} ~ ( \Omega R + U_{eq} / 2 ) ~~ ( \cos^{2}(\phi) - 2/3 ) $$

The parameters used here are slightly different from Earth (an oportunity to test this capability) with a smaller planet radius (`rSphere`) $R = 5500 km$ , a slower rotation ( 30 h period, `rotationPeriod=108000.`) and an equatorial zonal wind $U_{eq} = 80 m/s$ which corresponds to a 5 day revolution time.

The set-up uses linear free-surface with uniform density $\rho_{const} = 1$ , no viscosity and no bottom friction so that the solution is expected to remain unchanged over time.
A bell-shape patch of passive tracer (here salinity) centered at mid-latide ($\phi_{0} = 45^{o}$)
is advected with the simulated wind field.

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

There is comparison output in the directory:

```
  results/output.txt
```

## Setup details
In the current setup, the initial wind field and initial surface pressure anomaly are computed
from customized versions of
[ini_vel.F](https://github.com/MITgcm/MITgcm/blob/master/verification/solid-body.cs-32x32x1/code/ini_vel.F)
 and [ini_psurf.F](https://github.com/MITgcm/MITgcm/blob/master/verification/solid-body.cs-32x32x1/code/ini_psurf.F), respectively.
Note that $(1/3 - \sin^2)$ is used instead of $(\cos^2 - 2/3)$ in `ini_psurf.F` for $\eta(\phi)$ expression;
and the two horizontal components of the wind `uVel,vVel` along the local grid direction are computed from a stream-function $\Psi$ which is naturally defined at the grid-cell corner of the C-grid mesh (`XG,YG`) so that the initial flow is divergence free:

$$ \Psi( \phi ) = R \times U_{eq} ~ \sin( \phi ) $$

$$ \mathrm{uVel} = + \delta^j \Psi / \mathrm{dyG} ~~ ; ~~ \mathrm{vVel} = - \delta^i \Psi / \mathrm{dxG} $$

Alternatively, one could generate initial condition binary files by running matlab script `gendata.m`
(after changing `kwr=1` to `kwr=2` in gendata.m, line 75) and use these binary files instead of
customized source code (just by un-commenting lines 67-69 in main parameter file `data`).

## Comments
  This set-up uses the "compact format" for all I/O (i.e., one facet after the other, stacked
  along the second dimension, as opposed to the default old format with all 6 facets stacked along the first dimension) by setting `W2_mapIO = 1` in `data.exch2`.
  The initial passive tracer (here "salinity") input file is `real*8` and generated using the MATLAB script gendata.m.
  Note that `gendata.m` uses scripts from `MITgcm/utils/matlab/` and `MITgcm/utils/matlab/cd_grid/`.
