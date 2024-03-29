Test set-up for seaice pkg with Open-Boundary Conditions
========================================================

This verification experiment is used to test `pkg/seaice` with `pkg/obcs`

This set-up is carved out from `../lab_sea/input.salt_plume/`

The **primary** test, using input files from `input/` dir, which have been generated using
the Matlab script `input/mk_input.m` together with the set of output files from running
test experiment `../lab_sea/input.salt_plume`.

The **secondary** test `input.seaiceSponge/` use OBCS sponge-layer for seaice fields
(`useSeaiceSponge=.TRUE.`) in addition to prescribed OBCS from the primary test.

The **secondary** test  `input.tides/` add 4 tidal components to the barotropic velocity
at the open-boundaries (`useOBCStides=.TRUE.`) in addition to prescribed OBCS from the
primary test. The additional tidal component binary input files have been generated using
the matlab script `mk_tides.m` (see comments inside).

Note: specification of tidal components have been updated in PR #752
 (see `input.tides/update_TideFileName.sed` on how to update `data.obcs`)
