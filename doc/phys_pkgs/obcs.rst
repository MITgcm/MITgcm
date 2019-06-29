.. _sub_phys_pkg_obcs:

OBCS: Open boundary conditions for regional modeling
----------------------------------------------------

Authors: 
Alistair Adcroft, Patrick Heimbach, Samar Katiwala, Martin Losch


.. _ssub_pkg_obcs_intro:

Introduction
++++++++++++

The OBCS-package is fundamental to regional ocean modeling with the
MITgcm, but there are so many details to be considered in
regional ocean modeling that this package cannot accommodate all
imaginable and possible options. Therefore, for a regional simulation
with very particular details it is recommended to familiarize oneself
not only with the compile- and runtime-options of this package, but
also with the code itself. In many cases it will be necessary to adapt
the obcs-code (in particular S/R OBCS\_CALC) to the application
in question; in these cases the obcs-package (together with the
rbcs-package :numref:`sub_phys_pkg_rbcs`) is a very
useful infrastructure for implementing special regional models.

.. _ssub_pkg_obcs_config_compiling:

OBCS configuration and compiling
++++++++++++++++++++++++++++++++

As with all MITgcm packages OBCS can be turned on or off 
at compile time

 - using the :code:`packages.conf` file by adding :code:`obcs` to it
 - or using :code:`genmake2` adding :code:`-enable=obcs` or :code:`-disable=obcs` switches
 - *Required packages and CPP options:*

   - Two alternatives are available for prescribing open boundary values, which differ in the way how OB's are treated in time:

     - Simple time-management (e.g., constant in time, or cyclic with fixed frequency) is provided through S/R :code:`obcs_external_fields_load`.
     - More sophisticated 'real-time' (i.e. calendar time) management is available through :code:`obcs_prescribe_read`.
   - The latter case requires packages :code:`cal` and :code:`exf` to be enabled.




(see also :numref:`building_code`).

Parts of the OBCS code can be enabled or disabled at compile time
via CPP preprocessor flags. These options are set in
:filelink:`OBCS_OPTIONS.h <pkg/obcs/OBCS_OPTIONS.h>`. :numref:`pkg_obcs_cpp_opts` summarizes these options.


.. tabularcolumns:: |\Y{.475}|\Y{.1}|\Y{.45}|

.. _pkg_obcs_cpp_opts:

+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| CPP option               | Default | Description                                                                                              |
+==========================+=========+==========================================================================================================+
| ALLOW_OBCS_NORTH         | #define | enable Northern OB                                                                                       |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_SOUTH         | #define | enable Southern OB                                                                                       |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_EAST          | #define | enable Eastern OB                                                                                        |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_WEST          | #define | enable Western OB                                                                                        |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_PRESCRIBE     | #define | enable code for prescribing OB's                                                                         |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_SPONGE        | #undef  | enable sponge layer code                                                                                 |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_BALANCE       | #define | enable code for balancing transports through OB's                                                        |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_ORLANSKI           | #define | enable Orlanski radiation conditions at OB's                                                             |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_STEVENS       | #undef  | enable Stevens (1990) boundary conditions at OB's  (currently only implemented for eastern and western   |
|                          |         | boundaries and NOT for ptracers)                                                                         |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_SEAICE_SPONGE | #undef  | Include hooks to sponge layer treatment of pkg/seaice variables                                          |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+
| ALLOW_OBCS_TIDES         | #undef  | Add tidal contributions to normal OB flow (At the moment tidal forcing is applied only to "normal" flow) |
+--------------------------+---------+----------------------------------------------------------------------------------------------------------+


.. _pkg_obcs_runtime:

Run-time parameters
+++++++++++++++++++


Run-time parameters are set in files :code:`data.pkg` :code:`data.obcs` and 
:code:`data.exf`  if 'real-time' prescription is requested 
(i.e., package :ref:`exf <sub_phys_pkg_exf>` enabled). These parameter files are read in S/R 
:filelink:`packages_readparms.F <model/src/packages_readparms.F>` 
:filelink:`obcs_readparms.F <pkg/obcs/obcs_readparms.F>` and
:filelink:`exf_readparms.F <pkg/exf/exf_readparms.F>` respectively.
Run-time parameters may be broken into 3 categories:
 
 #. switching on/off the package at runtime
 #. OBCS package flags and parameters
 #. additional timing flags in :code:`data.exf` if selected.


Enabling the package
####################

The OBCS package is switched on at runtime by setting
:varlink:`useOBCS` = :code:`.TRUE.` in :code:`data.pkg`.

Package flags and parameters
############################

:numref:`pkg_obcs_runtime_flags` summarizes the
runtime flags that are set in :code:`data.obcs` and
their default values.


.. tabularcolumns:: |\X{1}{3}|c|\X{1}{2}|

.. _pkg_obcs_runtime_flags:

+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
|         Flag/parameter         |    default    |                                             Description                                             |
+================================+===============+=====================================================================================================+
| :varlink:`OB_Jnorth`           | 0             | Nx-vector of J-indices (w.r.t. Ny) of Northern OB at each I-position (w.r.t. Nx)                    |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`OB_Jsouth`           | 0             | Nx-vector of J-indices (w.r.t. Ny) of Southern OB at each I-position (w.r.t. Nx)                    |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`OB_Ieast`            | 0             | Ny-vector of I-indices (w.r.t. Nx) of Eastern OB at each J-position (w.r.t. Ny)                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`OB_Iwest`            | 0             | Ny-vector of I-indices (w.r.t. Nx) of Western OB at each J-position (w.r.t. Ny)                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useOBCSprescribe`    | FALSE         |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useOBCSsponge`       | FALSE         |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useOBCSbalance`      | FALSE         |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`OBCS_balanceFacN`,   | 1             | Factor(s) determining the details of the balancing code                                             |
| :varlink:`OBCS_balanceFacS`,   |               |                                                                                                     |
| :varlink:`OBCS_balanceFacE`,   |               |                                                                                                     |
| :varlink:`OBCS_balanceFacW`    |               |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useOrlanskiNorth`,   | FALSE         | Turn on Orlanski boundary conditions for individual boundary.                                       |
| :varlink:`useOrlanskiSouth`,   |               |                                                                                                     |
| :varlink:`useOrlanskiEast`,    |               |                                                                                                     |
| :varlink:`useOrlanskiWest`     |               |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useStevensNorth`,    | FALSE         | Turn on Stevens boundary conditions for individual boundary                                         |
| :varlink:`useStevensSouth`,    |               |                                                                                                     |
| :varlink:`useStevensEast`,     |               |                                                                                                     |
| :varlink:`useStevensWest`      |               |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| OB\ **Xy**\ File               | :kbd:`' '`    | File name of OB field:                                                                              |
|                                |               |                                                                                                     |
|                                |               | **X**: **N**\ (orth), **S**\ (outh), **E**\ (ast), **W**\(est)                                      |
|                                |               |                                                                                                     |
|                                |               | **y**: **t**\(emperature), **s**\ (salinity), **eta** (sea surface height),                         |
|                                |               | **u**\ (-velocity),  **v**\(-velocity), **w**\ (-velocity),                                         |
|                                |               | **a** (seaice area), **h** (sea ice thickness), **sn** (snow thickness), **sl** (sea ice salinity ) |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| **Orlanski Parameters**        | *OBCS_PARM02* |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`cvelTimeScale`       | 2000.0        | Averaging period for phase speed (seconds)                                                          |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`CMAX`                | 0.45          | Maximum allowable phase speed-CFL for AB-II (m/s)                                                   |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`CFIX`                | 0.8           | Fixed boundary phase speed (m/s)                                                                    |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useFixedCEast`       | FALSE         |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useFixedCWest`       | FALSE         |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| **Sponge layer parameters**    | *OBCS_PARM03* |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`spongeThickness`     | 0             | sponge layer thickness (in grid points)                                                             |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`Urelaxobcsinner`     | 0.0           | relaxation time scale at the innermost sponge layer point of a meridional OB (s)                    |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`Vrelaxobcsinner`     | 0.0           | relaxation time scale at the innermost sponge layer point of a zonal OB (s)                         |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`Urelaxobcsbound`     | 0.0           | relaxation time scale at the outermost sponge layer point of a meridional OB (s)                    |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`Vrelaxobcsbound`     | 0.0           | relaxation time scale at the outermost sponge layer point of a zonal OB (s)                         |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| **Stevens parameters**         | *OBCS_PARM04* |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`TrelaxStevens`       | 0             | Relaxation time scale for temperature/salinity (s)                                                  |
| :varlink:`SrelaxStevens`       |               |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useStevensPhaseVel`  | TRUE          |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+
| :varlink:`useStevensAdvection` | TRUE          |                                                                                                     |
+--------------------------------+---------------+-----------------------------------------------------------------------------------------------------+


.. _ssub_phys_pkg_obcs_defining_open_boundaries:

Defining open boundary positions
++++++++++++++++++++++++++++++++

There are four open boundaries (OBs): Northern, Southern, Eastern, and
Western. All OB locations are specified by their absolute meridional
(Northern/Southern) or zonal (Eastern/Western) indices. Thus, for each
zonal position :math:`i=1\ldots N_x` a meridional index :math:`j`
specifies the Northern/Southern OB position, and for each meridional
position :math:`j=1\ldots N_y` a zonal index :math:`i` specifies the
Eastern/Western OB position. For Northern/Southern OB this defines an
:math:`N_x`-dimensional “row” array :math:`\tt OB\_Jnorth(Nx)` /
:math:`\tt OB\_Jsouth(Nx)` and an :math:`N_y`-dimenisonal “column”
array :math:`\tt OB\_Ieast(Ny)` / :math:`\tt OB\_Iwest(Ny)`. Positions
determined in this way allows Northern/Southern OBs to be at variable
:math:`j` (or :math:`y`) positions and Eastern/Western OBs at variable
:math:`i` (or :math:`x`) positions. Here indices refer to tracer points
on the C-grid. A zero (0) element in :math:`\tt OB\_I\ldots`
:math:`\tt OB\_J\ldots` means there is no corresponding OB in that
column/row. For a Northern/Southern OB, the OB V point is to the
South/North. For an Eastern/Western OB, the OB U point is to the
West/East. For example


:code:`OB_Jnorth(3)=34`  means that:
  - :code:`T(3,34)`  is a an OB point  
  - :code:`U(3,34)`  is a an OB point
  - :code:`V(3,34)`  is a an OB point
:code:`OB_Jsouth(3)=1`  means that: 
  - :code:`T(3,1)`  is a an OB point 
  - :code:`U(3,1)`  is a an OB point  
  - :code:`V(3,2)`  is a an OB point 
:code:`OB_Ieast(10)=69`   means that:    
  - :code:`T(69,10)`  is a an OB point 
  - :code:`U(69,10)`  is a an OB point 
  - :code:`V(69,10)`  is a an OB point 
:code:`OB_Iwest(10)=1`   means that:    
  - :code:`T(1,10)`  is a an OB point 
  - :code:`U(2,10)`  is a an OB point 
  - :code:`V(1,10)`  is a an OB point


For convenience, negative values for :code:`Jnorth`/:code:`Ieast` refer to
points relative to the Northern/Eastern edges of the model
eg. :math:`\tt OB\_Jnorth(3)=-1`
means that the point :math:`\tt (3,Ny)` is a northern OB.


**Simple examples:** For a model grid with :math:`N_x \times N_y = 120 \times 144`
horizontal grid points with four open boundaries
along the four edges of the domain the simplest way of specifying the
boundary points in is:

::

      OB_Ieast = 144*-1,
    # or OB_Ieast = 144*120,
      OB_Iwest = 144*1,
      OB_Jnorth = 120*-1,
    # or OB_Jnorth = 120*144,
      OB_Jsouth = 120*1,

If only the first 50 grid points of the southern boundary are
boundary points:

::

      OB_Jsouth(1:50) = 50*1,


.. _ssub_phys_pkg_obcs_equations:

Equations and key routines
++++++++++++++++++++++++++

:filelink:`OBCS\_READPARMS <pkg/obcs/obcs_readparms.F>`:
########################################################

Set OB positions through arrays OB\_Jnorth(Nx), OB\_Jsouth(Nx),
OB\_Ieast(Ny), OB\_Iwest(Ny) and runtime flags (see Table
:numref:`pkg_obcs_runtime_flags`).

:filelink:`OBCS\_CALC <pkg/obcs/obcs_calc.F>`:
##############################################

Top-level routine for filling values to be applied at OB for
:math:`T,S,U,V,\eta` into corresponding “slice” arrays :math:`(x,z)`
:math:`(y,z)` for each OB: :math:`\tt OB[N/S/E/W][t/s/u/v]`; e.g. for
salinity array at Southern OB, array name is :math:`\tt OBSt`. Values
filled are either

-  constant vertical :math:`T,S` profiles as specified in file data
   (tRef(Nr), sRef(Nr)) with zero velocities :math:`U,V`

-  :math:`T,S,U,V` values determined via Orlanski radiation conditions
   (see below)

-  prescribed time-constant or time-varying fields (see below).

-  use prescribed boundary fields to compute Stevens boundary
   conditions.


:filelink:`ORLANSKI <pkg/obcs/ORLANSKI.h>`:
###########################################

Orlanski radiation conditions :cite:`orl:76` examples can be found in example configurations
:filelink:`dome <verification/dome>` (http://www.rsmas.miami.edu/personal/tamay/DOME/dome.html)
and :filelink:`plume\_on\_slope <verification/tutorial\_plume\_on\_slope>`.


:filelink:`OBCS\_PRESCRIBE\_READ <pkg/obcs/obcs\_prescibe\_read.F>`:
####################################################################


When :varlink:`useOBCSprescribe` = :code:`.TRUE.` the model tries to read
temperature, salinity, u- and v-velocities from files specified in the
runtime parameters :code:`OB[N/S/E/W][t/s/u/v]File`. These files are
the usual IEEE, big-endian files with dimensions of a section along an
open boundary:

-  For North/South boundary files the dimensions are
   :math:`(N_x\times N_r\times\mbox{time levels})`, for East/West
   boundary files the dimensions are
   :math:`(N_y\times N_r\times\mbox{time levels})`.

-  If a non-linear free surface is used
   (:numref:`nonlinear-freesurface`), additional files
   :code:`OB[N/S/E/W]etaFile` for the sea surface height $\eta$ with
   dimension :math:`(N_{x/y}\times\mbox{time levels})` may be specified.

- If non-hydrostatic dynamics are used
  (:numref:`non-hydrostatic`), additional files
  :code:`OB[N/S/E/W]wFile` for the vertical velocity $w$ with
  dimensions :math:`(N_{x/y}\times N_r\times\mbox{time levels})` can be
  specified.

- If :code:`useSEAICE=.TRUE.` then additional files
  :code:`OB[N/S/E/W][a,h,sl,sn,uice,vice]` for sea ice area, thickness
  (:code:`HEFF`), seaice salinity, snow and ice velocities
  :math:`(N_{x/y}\times\mbox{time levels})` can be specified.

As in :filelink:`S/R external_fields_load <model/src/external\_fields\_load.F>` or the :code:`exf`-package, the
code reads two time levels for each variable, e.g., :varlink:`OBNu0` and
:varlink:`OBNu1`, and interpolates linearly between these time levels to
obtain the value :code:`OBNu` at the current model time (step). When the
:code:`exf`-package is used, the time levels are controlled for each
boundary separately in the same way as the :code:`exf`-fields in
:code:`data.exf`, namelist :code:`EXF_NML_OBCS`. The runtime flags
follow the above naming conventions, e.g., for the western boundary the
corresponding flags are :code:`OBCWstartdate1/2` and
:code:`OBCWperiod`. Sea-ice boundary values are controlled separately
with :code:`siobWstartdate1/2` and :code:`siobWperiod`.  When the
:code:`exf`-package is not used the time levels are controlled by the
runtime flags :code:`externForcingPeriod` and :code:`externForcingCycle`
in :code:`data`; see :code:`verification/exp4` for an example.


:filelink:`OBCS\_CALC\_STEVENS <pkg/obcs/obcs_calc_stevens.F>`:
###############################################################

The boundary conditions following :cite:`stevens:90` require the
vertically averaged normal velocity (originally specified as a stream
function along the open boundary) :math:`\bar{u}_{ob}` and the tracer fields
:math:`\chi_{ob}` (note: passive tracers are currently not implemented and
the code stops when package :ref:`ptracers <sub_phys_pkg_ptracers>` is used together with this
option). Currently the code vertically averages the normal velocity
as specified in :code:`OB[E,W]u` or :code:`OB[N,S]v`. From these
prescribed values the code computes the boundary values for the next
timestep :math:`n+1` as follows (as an example, we use the notation for an
eastern or western boundary):


-  :math:`u^{n+1}(y,z) = \bar{u}_{ob}(y) + (u')^{n}(y,z)` where
   :math:`(u')^{n}` is the deviation from the vertically averaged
   velocity at timestep :math:`n` on the boundary. :math:`(u')^{n}` is
   computed in the previous time step :math:`n` from the intermediate
   velocity :math:`u^*` prior to the correction step (see
   :numref:`time_stepping` eq. :eq:`ustar-backward-free-surface`). (This velocity is not
   available at the beginning of the next time step :math:`n+1`, when
   S/R :code:`OBCS_CALC/OBCS_CALC_STEVENS` are called, therefore it needs to
   be saved in S/R :code:`DYNAMICS`  by calling S/R :code:`OBCS\_SAVE\_UV\_N` and also
   stored in a separate restart files
   ``pickup_stevens[N/S/E/W].${iteration}.data``)

-  If :math:`u^{n+1}` is directed into the model domain, the boudary
   value for tracer :math:`\chi` is restored to the prescribed values:

   .. math::

      \chi^{n+1} =   \chi^{n} + \frac{\Delta{t}}{\tau_\chi} (\chi_{ob} -
        \chi^{n})

   where :math:`\tau_\chi` is the relaxation time scale
   ``T/SrelaxStevens``. The new :math:`\chi^{n+1}` is then subject to
   the advection by :math:`u^{n+1}`.

-  If :math:`u^{n+1}` is directed out of the model domain, the tracer
   :math:`\chi^{n+1}` on the boundary at timestep :math:`n+1` is
   estimated from advection out of the domain with :math:`u^{n+1}+c`,
   where :math:`c` is a phase velocity estimated as
   :math:`\frac{1}{2}\frac{\partial\chi}{\partial{t}}/\frac{\partial\chi}{\partial{x}}`.
   The numerical scheme is (as an example for an eastern boundary):

   .. math::

      \chi_{i_{b},j,k}^{n+1} =   \chi_{i_{b},j,k}^{n} + \Delta{t} 
        (u^{n+1}+c)_{i_{b},j,k}\frac{\chi_{i_{b},j,k}^{n}
          - \chi_{i_{b}-1,j,k}^{n}}{\Delta{x}_{i_{b}j}^{C}}\mbox{ if }u_{i_{b}jk}^{n+1}>0

   where :math:`i_{b}` is the boundary index.
   For test purposes, the phase velocity contribution or the entire
   advection can be turned off by setting the corresponding parameters
   ``useStevensPhaseVel`` and ``useStevensAdvection`` to ``.FALSE.``.

See :cite:`stevens:90` for details. With this boundary condition
specifying the exact net transport across the open boundary is simple,
so that balancing the flow with (S/R :code:`OBCS_BALANCE_FLOW` see next
paragraph) is usually not necessary.

Special cases where the current implementation is not complete:

- When you use the non-linear free surface option (parameter ``nonlinFreeSurf`` > 1), the current implementation just assumes that the gradient normal to the open boundary is zero (:math:`\frac{\partial\eta}{\partial{n}} = 0`). Although this is inconsistent with geostrophic dynamics and the possibility to specify a non-zero tangent velocity together with Stevens BCs for normal velocities, it seems to work. Recommendation: Always specify zero tangential velocities with Stevens BCs.

- There is no code for passive tracers, just a commented template in S/R ``obcs_calc_stevens``. This means that passive tracers can be specified independently and are fluxed with the velocities that the Stevens BCs compute, but without the restoring term.

- There are no specific Stevens BCs for sea ice, e.g., :ref:`pkg/seaice <sub_phys_pkg_seaice>`. The model uses the default boundary conditions for the sea ice packages.

:filelink:`OBCS\_BALANCE\_FLOW <pkg/obcs/obcs_balance_flow.F>`:
###############################################################

When turned on (:code:`ALLOW_OBCS_BALANCE`
defined in :code:`OBCS_OPTIONS.h` and :code:`useOBCSbalance=.true.` in
:code:`data.obcs/OBCS_PARM01`), this routine balances the net flow
across the open boundaries. By default the net flow across the
boundaries is computed and all normal velocities on boundaries are
adjusted to obtain zero net inflow.

This behavior can be controlled with the runtime flags
:code:`OBCS_balanceFacN/S/E/W`. The values of these flags determine
how the net inflow is redistributed as small correction velocities
between the individual sections. A value ``-1`` balances an
individual boundary, values :math:`>0` determine the relative size of the
correction. For example, the values

:code:`OBCS_balanceFacE = 1.,`
:code:`OBCS_balanceFacW = -1.,`
:code:`OBCS_balanceFacN = 2.,`
:code:`OBCS_balanceFacS = 0.,`


make the model

-  correct Western :code:`OBWu` by substracting a uniform velocity to ensure zero net
   transport through the Western open boundary;

-  correct Eastern and Northern normal flow, with the Northern velocity
   correction two times larger than the Eastern correction, but *not*
   the Southern normal flow, to ensure that the total inflow through
   East, Northern, and Southern open boundary is balanced.


The old method of balancing the net flow for all sections individually
can be recovered by setting all flags to -1. Then the normal velocities
across each of the four boundaries are modified separately, so that the
net volume transport across *each* boundary is zero. For example, for
the western boundary at :math:`i=i_{b}`, the modified velocity is:

.. math::

   u(y,z) - \int_{\mbox{western boundary}}u dy dz \approx OBNu(j k) - \sum_{j k}
   OBNu(j k) h_{w}(i_{b} j k)\Delta{y_G(i_{b} j)}\Delta{z(k)}.

This also ensures a net total inflow of zero through all boundaries, but
this combination of flags is *not* useful if you want to simulate, for example,
a sector of the Southern Ocean with a strong ACC entering through the
western and leaving through the eastern boundary, because the value of
''-1'' for these flags will make sure that the strong inflow is removed.
Clearly, global balancing with :code:`OBCS_balanceFacE/W/N/S` :math:`\ge 0` is the preferred method.


OBCS\_APPLY\_*:
###############


:filelink:`OBCS\_SPONGE <pkg/obcs/obcs_sponge.F>`:
##################################################

The sponge layer code (turned on with :code:`ALLOW_OBCS_SPONGE` and
:code:`useOBCSsponge`) adds a relaxation term to the right-hand-side of
the momentum and tracer equations. The variables are relaxed towards
the boundary values with a relaxation time scale that increases
linearly with distance from the boundary

.. math::

   G_{\chi}^{\mbox{(sponge)}} = 
   - \frac{\chi - [( L - \delta{L} ) \chi_{BC} + \delta{L}\chi]/L}
   {[(L-\delta{L})\tau_{b}+\delta{L}\tau_{i}]/L} 
   = - \frac{\chi - [( 1 - l ) \chi_{BC} + l\chi]}
   {[(1-l)\tau_{b}+l\tau_{i}]}

where :math:`\chi` is the model variable (U/V/T/S) in the interior,
:math:`\chi_{BC}` the boundary value, :math:`L` the thickness of the
sponge layer (runtime parameter :code:`spongeThickness` in number of grid points),
:math:`\delta{L}\in[0,L]` (:math:`\frac{\delta{L}}{L}=l\in[0,1]`) the
distance from the boundary (also in grid points), and :math:`\tau_{b}`
(runtime parameters :code:`Urelaxobcsbound` and :code:`Vrelaxobcsbound`) and :math:`\tau_{i}` (runtime parameters :code:`Urelaxobcsinner` and :code:`Vrelaxobcsinner`)
the relaxation time scales on the boundary and at the interior
termination of the sponge layer. The parameters :code:`Urelaxobcsbound/inner` set the relaxation time
scales for the Eastern and Western boundaries, :code:`Vrelaxobcsbound/inner` for the Northern and
Southern boundaries.


OB's with nonlinear free surface
################################


OB's with sea ice
#################


.. _ssub_phys_pkg_obcs_flowchart:

Flow chart
++++++++++


::


    C     !CALLING SEQUENCE:
    c ...


.. _ssub_phys_pkg_obcs_diagnostics:

OBCS diagnostics
++++++++++++++++

Diagnostics output is available via the diagnostics package (see :numref:`outp_pack`). Available output fields are summarized below:

::

    ------------------------------------------------------
     <-Name->|Levs|grid|<--  Units   -->|<- Tile (max=80c)
    ------------------------------------------------------


.. _ssub_phys_pkg_obcs_experiments:

Experiments and tutorials that use obcs
+++++++++++++++++++++++++++++++++++++++

In the directory :filelink:`verification` the following experiments use
:code:`obcs`: 


-  :filelink:`exp4 <verification/exp4>`: box with 4 open boundaries, simulating flow over a Gaussian bump
   based on  also tests Stevens-boundary conditions;

-  :filelink:`dome <verification/dome>`: based on the project “Dynamics of Overflow Mixing and Entrainment”
   (http://www.rsmas.miami.edu/personal/tamay/DOME/dome.html) uses
   Orlanski-BCs;

-  :filelink:`internal_wave <verification/internal_wave>`: uses a heavily modified S/R ``OBCS_CALC``

-  :filelink:`seaice_obcs <verification/seaice_obcs>`: simple example who to use the sea-ice related code based on :filelink:`lab_sea <verification/lab_sea>`;

-  :filelink:`tutorial_plume_on_slope <verification/tutorial_plume_on_slope>`: uses Orlanski-BCs.
