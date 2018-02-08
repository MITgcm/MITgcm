Global atmosphere: ‘Held-Suarez’ benchmark
------------------------------------------


A novel feature of MITgcm is its ability to simulate, using one basic algorithm, 
both atmospheric and oceanographic flows at both small and large scales.

:numref:`cubic_eddies_figure` shows an instantaneous plot of the 500 mb
temperature field obtained using the atmospheric isomorph of MITgcm run at
2.8° resolution on the cubed sphere. We see cold air over the pole
(blue) and warm air along an equatorial band (red). Fully developed
baroclinic eddies spawned in the northern hemisphere storm track are
evident. There are no mountains or land-sea contrast in this calculation,
but you can easily put them in. The model is driven by relaxation to a
radiative-convective equilibrium profile, following the description set out
in Held and Suarez (1994) :cite:`held-suar:94` designed to test atmospheric hydrodynamical cores -
there are no mountains or land-sea contrast.


  .. figure:: figs/eddy_on_cubic_globe.*
    :width: 60%
    :align: center
    :alt: cubic eddies figure
    :name: cubic_eddies_figure

    Instantaneous plot of the temperature field at 500 mb obtained using the atmospheric isomorph of MITgcm


As described in Adcroft et al. (2004) :cite:`adcroft:04b`, a ‘cubed sphere’ is used to discretize the
globe permitting a uniform griding and obviated the need to Fourier filter.
The ‘vector-invariant’ form of MITgcm supports any orthogonal curvilinear
grid, of which the cubed sphere is just one of many choices.

:numref:`hs_zave_u_figure` shows the 5-year mean, zonally averaged zonal
wind from a 20-level configuration of
the model. It compares favorable with more conventional spatial
discretization approaches. The two plots show the field calculated using the
cube-sphere grid and the flow calculated using a regular, spherical polar
latitude-longitude grid. Both grids are supported within the model.

 .. figure:: figs/u_cube_latlon_comb.*
    :width: 80%
    :align: center
    :alt: hs_zave_u_figure
    :name: hs_zave_u_figure

    Five year mean, zonally averaged zonal flow for cube-sphere simulation (top) and latitude-longitude simulation (bottom) and using Held-Suarez forcing. Note the difference in the solutions over the pole — the cubed sphere is superior.

