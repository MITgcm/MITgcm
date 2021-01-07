Global state estimation of the ocean
------------------------------------

An important application of MITgcm is in state estimation of the global
ocean circulation. An appropriately defined ‘cost function', which measures
the departure of the model from observations (both remotely sensed and
in-situ) over an interval of time, is minimized by adjusting ‘control
parameters' such as air-sea fluxes, the wind field, the initial conditions
etc. :numref:`assim_figure` and :numref:`assim_figure2` show the large scale planetary
circulation and a Hopf-Muller plot of Equatorial sea-surface height.
Both are obtained from assimilation bringing the model in to
consistency with altimetric and in-situ observations over the period
1992-1997.

  .. figure:: figs/globes.png
    :width: 60%
    :align: center
    :alt: assim_figure
    :name: assim_figure

    Circulation patterns from a multi-year, global circulation simulation constrained by Topex altimeter data and WOCE cruise observations. This output is from a higher resolution, shorter duration experiment with equatorially enhanced grid spacing.

  .. figure:: figs/ssh_sim_assim_obs.png
    :width: 90%
    :align: center
    :alt: assim_figure2
    :name: assim_figure2

    Equatorial sea-surface height in unconstrained (left), constrained (middle) simulations and in observations (right).
