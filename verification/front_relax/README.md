# Relaxation of a front in a channel : simplest example that uses GM-Redi parameterization


A 2-D, y-z set-up is used to mimic a zonally symmetric, reentrant channel with
a baroclinicly unstable initial density front.<br>
As meso-scale eddies are not resolved in this 2-D set-up, the GM-Redi
parameterization is used to represent their effects.

### Overview:
This experiment contains 5 set-ups (with corresponding `input[.*]/` dir) that
can be run with the same executable (built from `build/` dir using customized
code from `code/`); binary input files have been generated using matlab script
`gendata.m` from the corresponding `input` dir. All five set-ups use a
simple EOS ( $\rho' = -\rho_0 ~ \alpha_T ~ \theta'$ ) and treat salt as a
passive tracer ; without any surface forcing, the density front is expected to
flatten (GM effect) while salinity spread along isopycnal (Redi diffusion).

The **primary** test, using input files from `input/` dir, is the simplest
one, with flat bottom, non-uniform resolution in both direction (15 levels
from 50 m to 400 m thick near the bottom and, in Y-direction, 32 grid-points
with about 10 km spacing) and stratified every-where (background
$N = 2\times 10^{-3} ~s^{-1}$, see matlab script `input/gendata.m`),
avoiding the need for tapering or clipping.<br>
It uses the skew-flux formulation of GM with same Redi and GM diffusivity (
`GM_background_K` = 1000 $m^2/s$, see: `input/data.gmredi`). Note that 10 dead
levels were added (below the bottom) to allow to use the same executable
(compiled with `Nr = 25`) for all 5 set-ups.

The **secondary** test `input.in_p/` dir is the same as the primary test but
converted to use P-coordinates instead of height coordinates. For the purpose
of comparing P and Z coordinates, gravity and reference density `rhoNil` are
set to round number (respectively 10 and 1000) to facilitate conversions. It
uses the advective form of GM with same Redi and GM diffusivity (see:
`input.in_p/data.gmredi`).

The **next two secondary** set-ups, `input.mxl/` and `input.bvp/` are very
similar, sharing the same binary input files from `input.mxl/` dir ; they use
the full 25 level model to represent a 10 level, 200 m thick mixed layer on
top of a stratified warm bowl of water.  The `input.mxl/` illustrates the use
of the transition-layer tapering scheme 'fm07' with the skew-flux formulation
of GM with same Redi and GM diffusivity ( `GM_background_K` = 1000 $m^2/s$,
see: `input.mxl/data.gmredi`) and a flat bottom while the secondary test
`input.bvp/` has a sloping bottom and uses the Boundary-Value Problem
(`GM_useBVP=T,` with 5 modes: `GM_BVP_modeNumber=5,`) of the GM advective form
with same Redi and GM diffusivity (see: `input.bvp/data.gmredi`). In addition,
in this later test, the sub-meso parameterization is activated
(`GM_useSubMeso=T`).

The **last secondary** test `input.top/` shares some similarity with the previous
one (similar warm bowl, use BVP with GM advective form) except that the model
top is depressed by 50 m near the center, as it would under, e.g., a floating
ice-shelf. Also the mixed layer is thinner (60 m only) and very weakly
stratified ( $N = 10^{-6} ~s^{-1}$ ) and vertical resolution is slightly
different, reaching a maximum depth of 2.5 km (vs only 2 km in the previous 2
set-ups).

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
  ../build/mitgcmuv > output.txt
```

There is comparison output in the directory:

  `results/output.txt`

To run any of secondary `$sc` test (`$sc` in: `in_p`, `mxl`, `bvp`, `top`):

```
  cd run
  rm *
  ln -s ../input.$sc/* .
  ln -s ../input/* .
  ./prepare_run          #- only for "bvp" test
  ../build/mitgcmuv > output.txt
```
Note that the "prepare_run" step is only needed for `bvp` test.

There is comparison output in the directory:

  `results/output.$sc.txt`

### Notes:
1. **Comparison between P and Z coordinates**:

    The results from the secondary set-up `input.in_p/`, using Pressure
    coordinate, can be compared to the Z-coordinate, primary set-up `input/`,
    providing one uses the same `pkg/gmredi` parameters (same 'data.gmredi'),
    e.g.:

    - Using GM advective form, with 'data.gmredi' from `input.in_p/` for both
      runs.

    - Using the GM skew-flux form, with 'data.gmredi' from `input/` for both
      runs.

    Note that, when comparing output, the vertical index `k` of 3-D variables
    needs to be flipped ; the SSH (in m) comparison is between output variable
    'Eta' (in-Z) and 'PHL' (in-P) divided by 10 (=gravity); and the bottom
    pressure (in Pa) is between 'PHL' (in-Z) multiplied by $1000 = \rho_0$ and
    'Eta' (in-P).<br>
    Most of the differences (e.g., after 20 time-steps, T,S max-diff are:
    $1.4\times 10^{-4}$, $2.3 \times 10^{-5}$ and RMS: $6.6 \times 10^{-5}$,
    $9.6 \times 10^{-6}$ ) come from the dynamics and not from GM since without
    dynamics (un-commenting line 34: `momStepping=.FALSE.,` in both set-up
    `data` files) the differences are down to machine precision (RMS of T,S
    diff: $1.5 \times 10^{-14}, ~ 1. \times 10^{-15}$ ).

2. **Testing GM or Redi diffusion alone** in either of the 2 set-ups above:

    To test GM alone, without isopycnal diffusion, just un-comment:
    `GM_isopycK = 0.,` in `data.gmredi`.<br>
    And to test Redi diffusion alone, without GM, just set: `GM_isopycK =
    1000.,` and comment out `GM_background_K` setting in `data.gmredi`. Also
    changing the initial salinity to a centered patch (using file
    `Sini_Patch.bin` instead of `Sini_Ydir.bin`) better illustrates the Redi
    effects on a passive tracer.

3. **Switching from a Y-Z to X-Z set-up**

    Since none of the 5 set-up use any feature specific to Y-direction, all
    using 'f-plane' (`f0 = 1.E-4, beta=0`), the same set of input files could
    be used by the X-Z model instead of the current Y-Z model (simply
    switching `sNx,nSx,nPx` <-> `sNy,nSy,nPy` in `SIZE.h`) to produce similar
    results (flipping U & V with 1 minus sign). The only minor adjustment to
    input files is related to the setting of delX, delY in the parameter file
    `data`, which also need to be switched, e.g., for the primary set-up:

    ```
     delXfile='dy.bin',
     delY=1*10.E3,
    ```
    instead of:

    ```
     delX=1*10.E3,
     delYfile='dy.bin',
    ```
    This may be used to verify implementation of X-fluxes versus Y-fluxes.
