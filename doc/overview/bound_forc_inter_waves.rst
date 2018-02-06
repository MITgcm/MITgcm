
Boundary forced internal waves
------------------------------


The unique ability of MITgcm to treat non-hydrostatic dynamics in the
presence of complex geometry makes it an ideal tool to study internal wave
dynamics and mixing in oceanic canyons and ridges driven by large amplitude
barotropic tidal currents imposed through open boundary conditions.

:numref:`slope_TU` shows the influence of cross-slope topographic variations on internal wave breaking - the cross-slope velocity is in color, the density contoured. The internal waves are excited by application of open boundary conditions on the left. They propagate to the sloping boundary (represented using MITgcm's finite volume spatial discretization) where they break under non-hydrostatic dynamics. 


  .. figure:: figs/TUt8000slope.png
    :width: 80%
    :align: center
    :alt: slope_TU
    :name: slope_TU

    Simulation of internal waves forced at an open boundary (on the left) impacting a sloping shelf. The along slope velocity is shown colored, contour lines show density surfaces. The slope is represented with high-fidelity using lopped cells.