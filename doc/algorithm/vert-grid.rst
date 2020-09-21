
Vertical grid
-------------

  .. figure:: figs/vgrid-accur-center.*
    :width: 60%
    :align: center
    :alt: vgrid-accur-center
    :name: vgrid-accur-center

    Two versions of the vertical grid. a) The cell centered approach where the interface depths are specified
    and the tracer points centered in between the interfaces. b) The interface centered approach where tracer
    levels are specified and the w-interfaces are centered in between.

As for the horizontal grid, we use the suffixes “c” and “f” to indicates
faces and centers. :numref:`vgrid-accur-center` (a) shows the default vertical grid
used by the model. :math:`\Delta r_f` is the difference in :math:`r`
(vertical coordinate) between the faces (i.e. :math:`\Delta r_f \equiv -
\delta_k r` where the minus sign appears due to the convention that the
surface layer has index :math:`k=1`.).

The vertical grid is calculated in subroutine :filelink:`INI_VERTICAL_GRID <model/src/ini_vertical_grid.F>` and
specified via the vector :varlink:`delR` in namelist ``PARM04``. The units of “r”
are either meters or Pascals depending on the isomorphism being used
which in turn is dependent only on the choice of equation of state.

There are alternative namelist vectors :varlink:`delZ` and :varlink:`delP` which
dictate whether z- or p- coordinates are to be used but we intend to
phase this out since they are redundant.

The reciprocals :math:`\Delta r_f^{-1}` and :math:`\Delta r_c^{-1}` are
pre-calculated (also in subroutine :filelink:`INI_VERTICAL_GRID <model/src/ini_vertical_grid.F>`). All vertical
grid descriptors are stored in common blocks in :filelink:`GRID.h <model/inc/GRID.h>`.

The above grid :numref:`vgrid-accur-center` (a) is known as the cell centered
approach because the tracer points are at cell centers; the cell centers
are mid-way between the cell interfaces. This discretization is selected
when the thickness of the levels are provided (:varlink:`delR`, parameter file
``data``, namelist ``PARM04``) An alternative, the vertex or interface
centered approach, is shown in :numref:`vgrid-accur-center` (b). Here, the interior
interfaces are positioned mid-way between the tracer nodes (no longer
cell centers). This approach is formally more accurate for evaluation of
hydrostatic pressure and vertical advection but historically the cell
centered approach has been used. An alternative form of subroutine
:filelink:`INI_VERTICAL_GRID <model/src/ini_vertical_grid.F>` is used to select the interface centered approach
This form requires to specify :math:`Nr+1` vertical distances :varlink:`delRc`
(parameter file ``data``, namelist ``PARM04``, e.g.
:filelink:`ideal\_2D\_oce/input/data <verification/ideal_2D_oce/input/data>`) corresponding to surface to
center, :math:`Nr-1` center to center, and center to bottom distances.

.. admonition:: S/R :filelink:`INI_VERTICAL_GRID <model/src/ini_vertical_grid.F>`
  :class: note

    | :math:`\Delta r_f , \Delta r_c` : :varlink:`drF`, :varlink:`drC` ( :filelink:`GRID.h <model/inc/GRID.h>` )
    | :math:`\Delta r_f^{-1} , \Delta r_c^{-1}` : :varlink:`recip_drF`, :varlink:`recip_drC` ( :filelink:`GRID.h <model/inc/GRID.h>` )

.. _sec_topo_partial_cells:

Topography: partially filled cells
----------------------------------

Adcroft et al. (1997) :cite:`adcroft:97` presented two alternatives to the
step-wise finite difference representation of topography. The method is
known to the engineering community as *intersecting boundary method*. It
involves allowing the boundary to intersect a grid of cells thereby
modifying the shape of those cells intersected. We suggested allowing
the topography to take on a piece-wise linear representation (shaved
cells) or a simpler piecewise constant representation (partial step).
Both show dramatic improvements in solution compared to the traditional
full step representation, the piece-wise linear being the best. However,
the storage requirements are excessive so the simpler piece-wise
constant or partial-step method is all that is currently supported.

  .. figure:: figs/vgrid-xz.*
    :width: 60%
    :align: center
    :alt: vgrid-xz
    :name: vgrid-xz

    A schematic of the x-r plane showing the location of the non-dimensional fractions :math:`h_c` and :math:`h_w` . The physical thickness of a tracer cell is given by :math:`h_c(i,j,k) \Delta r_f(k)` and the physical thickness of the open side is given by  :math:`h_w(i,j,k) \Delta r_f(k)` .

:numref:`vgrid-xz` shows a schematic of the x-r plane indicating how the
thickness of a level is determined at tracer and u points. The physical
thickness of a tracer cell is given by :math:`h_c(i,j,k) \Delta
r_f(k)` and the physical thickness of the open side is given by
:math:`h_w(i,j,k) \Delta r_f(k)`. Three 3-D descriptors :math:`h_c`,
:math:`h_w` and :math:`h_s` are used to describe the geometry:
:varlink:`hFacC`, :varlink:`hFacW` and :varlink:`hFacS` respectively. These are calculated in
subroutine :filelink:`INI_MASKS_ETC <model/src/ini_masks_etc.F>` along with there reciprocals
:varlink:`recip_hFacC`, :varlink:`recip_hFacW` and :varlink:`recip_hFacS`.

The non-dimensional fractions (or h-facs as we call them) are calculated
from the model depth array and then processed to avoid tiny volumes. The
rule is that if a fraction is less than :varlink:`hFacMin` then it is rounded
to the nearer of :math:`0` or :varlink:`hFacMin` or if the physical thickness
is less than :varlink:`hFacMinDr` then it is similarly rounded. The larger of
the two methods is used when there is a conflict. By setting
:varlink:`hFacMinDr` equal to or larger than the thinnest nominal layers,
:math:`\min{(\Delta z_f)}`, but setting :varlink:`hFacMin` to some small
fraction then the model will only lop thick layers but retain stability
based on the thinnest unlopped thickness;
:math:`\min{(\Delta z_f,hFacMinDr)}`.

.. admonition:: S/R :filelink::filelink:`INI_MASKS_ETC <model/src/ini_masks_etc.F>`
  :class: note

    | :math:`h_c , h_w , h_s` : :varlink:`hFacC`, :varlink:`hFacW`, :varlink:`hFacS` ( :filelink:`GRID.h <model/inc/GRID.h>` )
    | :math:`h_c^{-1} , h_w^{-1} , h_s^{-1}` : :varlink:`recip_hFacC`, :varlink:`recip_hFacW`, :varlink:`recip_hFacS` ( :filelink:`GRID.h <model/inc/GRID.h>` )


