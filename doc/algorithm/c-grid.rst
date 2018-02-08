C grid staggering of variables
------------------------------

The basic algorithm employed for stepping forward the momentum equations
is based on retaining non-divergence of the flow at all times. This is
most naturally done if the components of flow are staggered in space in
the form of an Arakawa C grid (Arakawa and Lamb, 1977 :cite:`arakawa:77`).

:numref:`cgrid3d` shows the components of flow
(:math:`u`,\ :math:`v`,\ :math:`w`) staggered in space such that the
zonal component falls on the interface between continuity cells in the
zonal direction. Similarly for the meridional and vertical directions.
The continuity cell is synonymous with tracer cells (they are one and
the same).

  .. figure:: figs/cgrid3d.*
    :width: 40%
    :align: center
    :alt: cgrid3d
    :name: cgrid3d

    Three dimensional staggering of velocity components. This facilitates the natural discretization of the continuity and tracer equations.

Grid initialization and data
----------------------------

Initialization of grid data is controlled by subroutine :filelink:`INI_GRID <model/src/ini_grid.F>`
which in calls :filelink:`INI_VERTICAL_GRID <model/src/ini_vertical_grid.F>` to initialize the vertical grid,
and then either of :filelink:`INI_CARTESIAN_GRID <model/src/ini_cartesian_grid.F>`,
:filelink:`INI_SPHERICAL_POLAR_GRID <model/src/ini_spherical_polar_grid.F>`
or :filelink:`INI_CURVILINEAR_GRID <model/src/ini_curvilinear_grid.F>` to initialize the horizontal grid for
cartesian, spherical-polar or curvilinear coordinates respectively.

The reciprocals of all grid quantities are pre-calculated and this is
done in subroutine :filelink:`INI_MASKS_ETC <model/src/ini_masks_etc.F>` which is called later by subroutine
:filelink:`INITIALISE_FIXED <model/src/initialise_fixed.F>`.

All grid descriptors are global arrays and stored in common blocks in
:filelink:`GRID.h <model/inc/GRID.h>` and a generally declared as ``_RS``.

