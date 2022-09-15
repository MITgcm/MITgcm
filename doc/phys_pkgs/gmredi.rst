.. _sub_phys_pkg_gmredi:

GMREDI: Gent-McWilliams/Redi Eddy Parameterization
**************************************************

Introduction
============

Package :filelink:`gmredi <pkg/gmredi>` parameterizes
the effects of unresolved mesoscale eddies on tracer distribution,
i.e., temperature, salinity and other tracers.

There are two parts to the Redi/GM subgrid-scale parameterization of geostrophic
eddies. The first, the Redi scheme (Redi 1982 :cite:`redi1982`), aims to mix tracer properties along
isentropes (neutral surfaces) by means of a diffusion operator oriented
along the local isentropic surface. The second part, GM 
(Gent and McWiliams 1990 :cite:`gen-mcw:90`, Gent et al. 1995 :cite:`gen-eta:95`), adiabatically
rearranges tracers through an advective flux where the advecting flow
is a function of slope of the isentropic surfaces.

A description of key package equations used is
given in :numref:`ssub_phys_pkg_gmredi_descr`.
CPP options enable or disable different aspects of the package
(:numref:`ssub_phys_pkg_gmredi_config`). Run-time options, flags, and filenames
are set in ``data.gmredi``
(:numref:`ssub_phys_pkg_gmredi_runtime`). Available diagnostics
output is listed in :numref:`ssub_phys_pkg_gmredi_diagnostics`.

.. _ssub_phys_pkg_gmredi_descr:

Description
===========

The first GCM implementation of the Redi scheme was by Cox (1987) :cite:`cox87` in the GFDL ocean
circulation model. The original approach failed to distinguish between
isopycnals and surfaces of locally referenced potential density (now
called neutral surfaces), which are proper isentropes for the ocean. As
will be discussed later, it also appears that the Cox implementation is
susceptible to a computational mode. Due to this mode, the Cox scheme
requires a background lateral diffusion to be present to conserve the
integrity of the model fields.

The GM parameterization was then added to the GFDL code in the form of a
non-divergent bolus velocity. The method defines two streamfunctions
expressed in terms of the isoneutral slopes subject to the boundary
condition of zero value on upper and lower boundaries. The horizontal
bolus velocities are then the vertical derivative of these functions.
Here in lies a problem highlighted by Griffies et al. (1998) :cite:`gretal:98`: the
bolus velocities involve multiple derivatives on the potential density field,
which can consequently give rise to noise. Griffies et al. point out that the GM
bolus fluxes can be identically written as a skew flux which involves
fewer differential operators. Further, combining the skew flux
formulation and Redi scheme, substantial cancellations take place to the
point that the horizontal fluxes are unmodified from the lateral
diffusion parameterization.

Redi scheme: Isopycnal diffusion
--------------------------------

The Redi scheme diffuses tracers along isopycnals and introduces a term
in the tendency (rhs) of such a tracer (here :math:`\tau`) of the form:

.. math:: \nabla \cdot ( \kappa_\rho {\bf K}_{\rm Redi} \nabla \tau )

where :math:`\kappa_\rho` is the along isopycnal diffusivity and
:math:`{\bf K}_{\rm Redi}` is a rank 2 tensor that projects the gradient of
:math:`\tau` onto the isopycnal surface. The unapproximated projection
tensor is:

.. math::

   {\bf K}_{\rm Redi} = \frac{1}{1 + |{\bf S}|^2} 
   \begin{pmatrix}
   1 + S_y^2& -S_x S_y & S_x \\
   -S_x S_y  & 1 + S_x^2 & S_y \\
   S_x & S_y & |{\bf S}|^2 \\
   \end{pmatrix}

Here, :math:`S_x = -\partial_x \sigma / \partial_z \sigma`,
:math:`S_y =
-\partial_y \sigma / \partial_z \sigma` are the components of the
isoneutral slope, and :math:`|{\bf S}|^2 = S_x^2 + S_y^2`.

The first point to note is that a typical slope in the ocean interior is
small, say of the order :math:`10^{-4}`. A maximum slope might be of
order :math:`10^{-2}` and only exceeds such in unstratified regions
where the slope is ill-defined. It is, therefore, justifiable, and
customary, to make the small-slope approximation, i.e., :math:`|{\bf S}| \ll 1`. Then
Redi projection tensor then simplifies to:

.. math::
   {\bf K}_{\rm Redi} =
   \begin{pmatrix}
   1 & 0 & S_x \\
   0 & 1 & S_y \\
   S_x & S_y & |{\bf S}|^2 \\
   \end{pmatrix}

.. _GM_bolus_desc:  

GM parameterization
-------------------

The GM parameterization aims to represent the advective or “transport”
effect of geostrophic eddies by means of a “bolus” velocity,
:math:`{\bf u}^\star`. The divergence of this advective flux is added to
the tracer tendency equation (on the rhs):

.. math:: - \nabla \cdot ( \tau {\bf u}^\star )

The bolus velocity :math:`{\bf u}^\star` is defined as the rotational part
of a streamfunction
:math:`{\bf F}^\star = (F_x^\star, F_y^\star, 0)`:

.. math::

   {\bf u}^\star = \nabla \times {\bf F}^\star =
   \begin{pmatrix}
   - \partial_z  F_y^\star \\
   + \partial_z  F_x^\star \\
   \partial_x F_y^\star - \partial_y F_x^\star
   \end{pmatrix}

and thus is automatically non-divergent. In the GM parameterization, the
streamfunction is specified in terms of the isoneutral slopes
:math:`S_x` and :math:`S_y`:

.. math::

   \begin{aligned}
   F_x^\star & = -\kappa_{\rm GM} S_y\\
   F_y^\star & =  \kappa_{\rm GM} S_x
   \end{aligned}

with boundary conditions :math:`F_x^\star=F_y^\star=0` on upper and
lower boundaries. :math:`\kappa_{\rm GM}` is colloquially called the isopycnal "thickness diffusivity"
or the "GM diffusivity". The bolus transport in the GM
parameterization is thus given by:

.. math::

   {\bf u}^\star =
   \begin{pmatrix}
   u^\star \\
   v^\star \\
   w^\star
   \end{pmatrix} =
   \begin{pmatrix}
   - \partial_z (\kappa_{\rm GM} S_x) \\
   - \partial_z (\kappa_{\rm GM} S_y) \\
   \partial_x (\kappa_{\rm GM} S_x) + \partial_y (\kappa_{\rm GM} S_y)
   \end{pmatrix}

This is the "advective form" of the GM parameterization as applied by Danabasoglu and McWilliams (1995) :cite:`danabasoglu:95`,
employed in the GFDL Modular Ocean Model (MOM) versions 1 and 2. To use the advective form in MITgcm, set
:varlink:`GM_AdvForm` ``=.TRUE.`` in ``data.gmredi``
(also requires ``#define`` :varlink:`GM_BOLUS_ADVEC` and :varlink:`GM_EXTRA_DIAGONAL`).
As implemented in the MITgcm code, :math:`{\bf u}^\star` is simply added to Eulerian :math:`\vec{\bf u}`
(i.e., MITgcm variables :varlink:`uVel`, :varlink:`vVel`, :varlink:`wVel`)
and passed to tracer advection subroutines (:numref:`advection_schemes`)
unless :varlink:`GM_AdvSeparate` ``=.TRUE.`` in ``data.gmredi``, in which case the bolus transport is computed separately.

Note that in MITgcm, the variables for the GM bolus
streamfunction :varlink:`GM_PsiX` and :varlink:`GM_PsiY` are defined:

.. math::

   \begin{pmatrix}
   \sf{GM\_PsiX} \\
   \sf{GM\_PsiY}
   \end{pmatrix} =
   \begin{pmatrix}
   \kappa_{\rm GM} S_x \\
   \kappa_{\rm GM} S_y
   \end{pmatrix} =
   \begin{pmatrix}
   F_y^\star \\
   -F_x^\star
   \end{pmatrix}

.. _sub_gmredi_skewflux:

Griffies Skew Flux
------------------

Griffies (1998) :cite:`gr:98` notes that the discretization of bolus velocities involves multiple
layers of differencing and interpolation that potentially lead to noisy
fields and computational modes. He pointed out that the bolus flux can
be re-written in terms of a non-divergent flux and a skew-flux:

.. math::

   \begin{aligned}
   {\bf u}^\star \tau
   & = 
   \begin{pmatrix}
   - \partial_z ( \kappa_{\rm GM} S_x ) \tau \\
   - \partial_z ( \kappa_{\rm GM} S_y ) \tau \\
   \Big[ \partial_x (\kappa_{\rm GM} S_x) + \partial_y (\kappa_{\rm GM} S_y) \Big] \tau
   \end{pmatrix}
   \\
   & = 
   \begin{pmatrix}
   - \partial_z ( \kappa_{\rm GM} S_x \tau) \\
   - \partial_z ( \kappa_{\rm GM} S_y \tau) \\
   \partial_x ( \kappa_{\rm GM} S_x \tau) + \partial_y ( \kappa_{\rm GM} S_y \tau)
   \end{pmatrix}
   + \kappa_{\rm GM} \begin{pmatrix}
     S_x \partial_z \tau \\
     S_y \partial_z \tau \\
   -  S_x \partial_x \tau - S_y \partial_y \tau
   \end{pmatrix}
   \end{aligned}

The first vector is non-divergent and thus has no effect on the tracer
field and can be dropped. The remaining flux can be written:

.. math:: \bf{u}^\star \tau = - \kappa_{\rm GM} \bf{K}_{\rm GM} \bf{\nabla} \tau

where

.. math::

   {\bf K}_{\rm GM} =
   \begin{pmatrix}
    0  &  0  & -S_x \\
    0  &  0  & -S_y \\
   S_x & S_y &   0
   \end{pmatrix}

is an anti-symmetric tensor.

This formulation of the GM parameterization involves fewer derivatives
than the original and also involves only terms that already appear in
the Redi mixing scheme. Indeed, a somewhat fortunate cancellation
becomes apparent when we use the GM parameterization in conjunction with
the Redi isoneutral mixing scheme:

.. math::

   \kappa_\rho {\bf K}_{\rm Redi} \nabla \tau
   - {\bf u}^\star \tau = 
   ( \kappa_\rho {\bf K}_{\rm Redi} + \kappa_{\rm GM} {\bf K}_{\rm GM} ) \nabla \tau

If the Redi and GM diffusivities are equal, :math:`\kappa_{\rm GM} = \kappa_{\rho}`, then

.. math::
   \kappa_\rho {\bf K}_{\rm Redi} + \kappa_{\rm GM} {\bf K}_{\rm GM} =
   \kappa_\rho
   \begin{pmatrix}
   1 & 0 & 0 \\
   0 & 1 & 0 \\
   2 S_x & 2 S_y & |{\bf S}|^2 
   \end{pmatrix}

which only differs from the variable Laplacian diffusion tensor by the two
non-zero elements in the :math:`z`-row.

.. admonition:: Subroutine
  :class: note

  S/R GMREDI_CALC_TENSOR (:filelink:`pkg/gmredi/gmredi_calc_tensor.F`)

  :math:`\sigma_x`: **SlopeX** (argument on entry)

  :math:`\sigma_y`: **SlopeY** (argument on entry)

  :math:`\sigma_z`: **SlopeY** (argument)

  :math:`S_x`: **SlopeX** (argument on exit)

  :math:`S_y`: **SlopeY** (argument on exit)

Visbeck et al. 1997 GM diffusivity :math:`\kappa_{GM}(x,y)`
-----------------------------------------------------------

Visbeck et al. (1997) :cite:`visbeck:97` suggest making the eddy coefficient,
:math:`\kappa_{\rm GM}`, a function of
the Eady growth rate, :math:`|f|/\sqrt{\rm Ri}`. The formula involves a
non-dimensional constant, :math:`\alpha`, and a length-scale :math:`L`:

.. math:: \kappa_{\rm GM} = \alpha L^2 \overline{ \frac{|f|}{\sqrt{\rm Ri}} }^z

where the Eady growth rate has been depth averaged (indicated by the
over-line). A local Richardson number is defined
:math:`{\rm Ri} = N^2 / (\partial_z u)^2` which, when combined with thermal wind gives:

.. math::

   \frac{1}{\rm Ri} = \frac{(\partial u/\partial z)^2}{N^2} =
   \frac{ \left ( \dfrac{g}{f \rho_0} | \nabla \sigma | \right )^2 }{N^2} =
   \frac{ M^4 }{ |f|^2 N^2 }

where :math:`M^2 = g | \nabla \sigma| / \rho_0`. Substituting into
the formula for :math:`\kappa_{\rm GM}` gives:

.. math::

   \kappa_{\rm GM} = \alpha L^2 \overline{ \frac{M^2}{N} }^z =
   \alpha L^2 \overline{ \frac{M^2}{N^2} N }^z =
   \alpha L^2 \overline{ |{\bf S}| N }^z

.. _sub_gmredi_tapering_stability:

Tapering and stability
----------------------

Experience with the GFDL model showed that the GM scheme has to be
matched to the convective parameterization. This was originally
expressed in connection with the introduction of the KPP boundary layer
scheme (Large et al. 1994 :cite:`lar-eta:94`) but in fact, as subsequent experience with the MIT model has
found, is necessary for any convective parameterization.

Slope clipping
++++++++++++++

Deep convection sites and the mixed layer are indicated by homogenized,
unstable or nearly unstable stratification. The slopes in such regions
can be either infinite, very large with a sign reversal or simply very
large. From a numerical point of view, large slopes lead to large
variations in the tensor elements (implying large bolus flow) and can be
numerically unstable. This was first recognized by Cox (1987) :cite:`cox87` who implemented
“slope clipping” in the isopycnal mixing tensor. Here, the slope
magnitude is simply restricted by an upper limit:

.. math::

   \begin{aligned}
   |\nabla_h \sigma| & = \sqrt{ \sigma_x^2 + \sigma_y^2 }\\
   S_{\rm lim} & = - \frac{|\nabla_h \sigma|}{ S_{\max} }, 
   \quad \mbox{where $S_{\max}>0$ is a parameter} \\
   \sigma_z^\star & = \min( \sigma_z, S_{\rm lim} ) \\
   {[s_x, s_y]} & = - \frac{ [\sigma_x, \sigma_y] }{\sigma_z^\star}
   \end{aligned}

Notice that this algorithm assumes stable stratification through the
“min” function. In the case where the fluid is well stratified
(:math:`\sigma_z < S_{\rm lim}`) then the slopes evaluate to:

.. math:: {[s_x, s_y]} = - \frac{ [\sigma_x, \sigma_y] }{\sigma_z}

while in the limited regions (:math:`\sigma_z > S_{\rm lim}`) the slopes
become:

.. math:: {[s_x, s_y]} = \frac{ [\sigma_x, \sigma_y] }{|\nabla_h \sigma| / S_{\max}}

so that the slope magnitude is limited :math:`\sqrt{s_x^2 + s_y^2} =
S_{\max}`.

The slope clipping scheme is activated in the model by setting
:varlink:`GM_taper_scheme` ``= ’clipping’`` in ``data.gmredi``.

Even using slope clipping, it is normally the case that the vertical
diffusion term (with coefficient :math:`\kappa_\rho{\bf K}_{33} =
\kappa_\rho S_{\max}^2`) is large and must be time-stepped using an
implicit procedure (see :numref:`implicit-backward-stepping`). Fig.
[fig-mixedlayer] shows the mixed layer depth resulting from a) using the
GM scheme with clipping and b) no GM scheme (horizontal diffusion). The
classic result of dramatically reduced mixed layers is evident. Indeed,
the deep convection sites to just one or two points each and are much
shallower than we might prefer. This, it turns out, is due to the over
zealous re-stratification due to the bolus transport parameterization.
Limiting the slopes also breaks the adiabatic nature of the GM/Redi
parameterization, re-introducing diabatic fluxes in regions where the
limiting is in effect.

.. admonition:: Subroutine
  :class: note

  S/R GMREDI_SLOPE_LIMIT (:filelink:`pkg/gmredi/gmredi_slope_limit.F`)

  :math:`\sigma_x, s_x`: **SlopeX** (argument)

  :math:`\sigma_y, s_y`: **SlopeY** (argument)

  :math:`\sigma_z`: **dSigmadRReal** (argument)

  :math:`z_\sigma^{*}`: **dRdSigmaLtd** (argument)

Tapering: Gerdes, Koberle and Willebrand, 1991 (GKW91)
++++++++++++++++++++++++++++++++++++++++++++++++++++++

The tapering scheme used in Gerdes et al. (1991) :cite:`gkw:91` (GKW91)
addressed two issues with the clipping
method: the introduction of large vertical fluxes in addition to
convective adjustment fluxes is avoided by tapering the GM/Redi slopes
back to zero in low-stratification regions; the adjustment of slopes is
replaced by a tapering of the entire GM/Redi tensor. This means the
direction of fluxes is unaffected as the amplitude is scaled.

The scheme inserts a tapering function, :math:`f_1(S)`, in front of the
GM/Redi tensor:

.. math:: f_1(S) = \min \left[ 1, \left( \frac{S_{\max}}{|{\bf S}|}\right)^2 \right]

where :math:`S_{\max}` is the maximum slope you want allowed. Where the
slopes, :math:`|{\bf S}|<S_{\max}` then :math:`f_1(S) = 1` and the tensor is
un-tapered but where :math:`|{\bf S}| \ge S_{\max}` then :math:`f_1(S)` scales
down the tensor so that the effective vertical diffusivity term
:math:`\kappa f_1(S) |{\bf S}|^2 = \kappa S_{\max}^2`.

The GKW91 tapering scheme is activated in the model by setting
:varlink:`GM_taper_scheme` ``= ’gkw91’`` in ``data.gmredi``.

.. figure:: figs/tapers.*
    :width: 70%
    :align: center
    :alt: Tapering for GM scheme
    :name: tapers

    Taper functions used in GKW91 and DM95. 


.. figure:: figs/effective_slopes.*
    :width: 70%
    :align: center
    :alt: Tapering for GM scheme
    :name: effective_slopes

    Effective slope as a function of 'true' slope using Cox slope clipping, GKW91 limiting and DM95 limiting.

Tapering: Danabasoglu and McWilliams, 1995 (DM95)
+++++++++++++++++++++++++++++++++++++++++++++++++

The tapering scheme used by Danabasoglu and McWilliams (1995) :cite:`danabasoglu:95` (DM95)
followed a similar procedure but used a different tapering function, :math:`f_1(S)`:

.. math:: f_1(S) = \frac{1}{2} \left[ 1+\tanh \left( \frac{S_c - |{\bf S}|}{S_d} \right) \right]

where :math:`S_c = 0.004` is a cut-off slope and :math:`S_d=0.001` is a
scale over which the slopes are smoothly tapered. Functionally, the
operates in the same way as the GKW91 scheme but has a substantially
lower cut-off, turning off the GM/Redi parameterization for weaker
slopes.

The DM95 tapering scheme is activated in the model by setting
:varlink:`GM_taper_scheme` ``= ’dm95’`` in ``data.gmredi``.


Tapering: Large, Danabasoglu and Doney, 1997 (LDD97)
++++++++++++++++++++++++++++++++++++++++++++++++++++

The tapering used in Large et al. (1997) :cite:`lar-eta:97` (LDD97)
is based on the DM95 tapering scheme, but also
tapers the scheme with an additional function of height, :math:`f_2(z)`,
so that the GM/Redi subgrid-scale fluxes are reduced near the surface:

.. math:: f_2(z) = \frac{1}{2} \left[ 1 + \sin \left(\pi \frac{z}{D} - \frac{\pi}{2} \right) \right]

where :math:`D = (c / f) |{\bf S}|` is a depth scale, with :math:`f` the
Coriolis parameter and :math:`c=2` m/s (corresponding to the first baroclinic wave speed, so that :math:`c/f` is the Rossby radius).
This tapering that varies with depth
was introduced to fix some spurious interaction with the mixed-layer KPP
parameterization.

The LDD97 tapering scheme is activated in the model by setting
:varlink:`GM_taper_scheme` ``= ’ldd97’`` in ``data.gmredi``.

.. _ssub_phys_pkg_gmredi_config:

GMREDI configuration and compiling
==================================

Compile-time options
--------------------

As with all MITgcm packages, GMREDI can be turned on or off at compile time
(see :numref:`building_code`)

- using the ``packages.conf`` file by adding ``gmredi`` to it

- or using :filelink:`genmake2 <tools/genmake2>` adding ``-enable=gmredi`` or
  ``-disable=gmredi`` switches

- **required packages and CPP options**:
  :filelink:`gmredi <pkg/gmredi>` requires

Parts of the :filelink:`gmredi <pkg/gmredi>` code can be enabled or disabled at
compile time via CPP preprocessor flags. These options are set in
:filelink:`GMREDI_OPTIONS.h <pkg/gmredi/GMREDI_OPTIONS.h>`.
:numref:`tab_phys_pkg_gmredi_cpp` summarizes the most important ones. For additional
options see :filelink:`GMREDI_OPTIONS.h <pkg/gmredi/GMREDI_OPTIONS.h>`.

.. tabularcolumns:: |\Y{.375}|\Y{.1}|\Y{.55}|

.. csv-table:: Some of the most relevant CPP preprocessor flags in the :filelink:`gmredi <pkg/gmredi>` package.
   :header: "CPP option", "Default", "Description"
   :widths: 30, 10, 60
   :name: tab_phys_pkg_gmredi_cpp

   :varlink:`GM_NON_UNITY_DIAGONAL`, #define, allows the leading diagonal (top two rows) to be non-unity
   :varlink:`GM_EXTRA_DIAGONAL`, #define, allows different values of :math:`\kappa_{\rm GM}` and :math:`\kappa_{\rho}`; also required for advective form
   :varlink:`GM_BOLUS_ADVEC`, #define, allows use of the advective form (bolus velocity)
   :varlink:`GM_BOLUS_BVP`, #define, allows use of Boundary-Value-Problem method to evaluate bolus transport
   :varlink:`ALLOW_GM_LEITH_QG`, #undef, allow QG Leith variable viscosity to be added to GMRedi coefficient
   :varlink:`GM_VISBECK_VARIABLE_K`, #undef, allows Visbeck et al. formulation to compute :math:`\kappa_{\rm GM}`

.. _ssub_phys_pkg_gmredi_runtime:

Run-time parameters
===================

Run-time parameters (see :numref:`tab_phys_pkg_gmredi_runtimeparms`) are set in
``data.gmredi`` (read in :filelink:`pkg/gmredi/gmredi_readparms.F`).

Enabling the package
--------------------

:filelink:`gmredi <pkg/gmredi>` package is switched on/off at run-time by
setting :varlink:`useGMREDI` ``= .TRUE.,`` in ``data.pkg``.

General flags and parameters
----------------------------

:numref:`tab_phys_pkg_gmredi_runtimeparms` lists most run-time parameters.

.. tabularcolumns:: |\Y{.275}|\Y{.20}|\Y{.525}|

.. table:: Run-time parameters and default values
  :class: longtable
  :name: tab_phys_pkg_gmredi_runtimeparms

  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  |   Name                             |      Default value           |   Description                                                           |
  +====================================+==============================+=========================================================================+
  | :varlink:`GM_AdvForm`              |     FALSE                    | use advective form (bolus velocity); FALSE uses skewflux form           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_AdvSeparate`          |     FALSE                    | do advection by Eulerian and bolus velocity separately                  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_background_K`         |     0.0                      | thickness diffusivity :math:`\kappa_{\rm GM}` (m\ :sup:`2`\ /s)         |
  |                                    |                              | (GM bolus transport)                                                    |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_isopycK`              |   :varlink:`GM_background_K` | isopycnal diffusivity :math:`\kappa_{\rho}` (m\ :sup:`2`\ /s)           |
  |                                    |                              | (Redi tensor)                                                           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_maxSlope`             |     1.0E-02                  | maximum slope (tapering/clipping)                                       |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Kmin_horiz`           |     0.0                      | minimum horizontal diffusivity (m\ :sup:`2`\ /s)                        |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Small_Number`         |     1.0E-20                  | :math:`\epsilon` used in computing the slope                            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_slopeSqCutoff`        |     1.0E+48                  | :math:`|{\bf S}|^2` cut-off value for zero taper function               |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_taper_scheme`         |     ' '                      | taper scheme option ('orig', 'clipping', 'fm07', 'stableGmAdjTap',      |
  |                                    |                              | 'linear', 'ac02', 'gkw91', 'dm95', 'ldd97')                             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_maxTransLay`          |     500.0                    | maximum transition layer thickness (m)                                  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_facTrL2ML`            |     5.0                      | maximum trans. layer thick. as a factor of local mixed-layer depth      |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_facTrL2dz`            |     1.0                      | minimum trans. layer thick. as a factor of local dr                     |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Scrit`                |     0.004                    | :math:`S_c` parameter for 'dm95' and 'ldd97 ' tapering function         |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Sd`                   |     0.001                    | :math:`S_d` parameter for 'dm95' and 'ldd97' tapering function          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_UseBVP`               |     FALSE                    | use Boundary-Value-Problem method for bolus transport                   |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_BVP_ModeNumber`       |     1                        | vertical mode number used for speed :math:`c` in BVP transport          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_BVP_cMin`             |     1.0E-01                  | minimum value for wave speed parameter :math:`c` in BVP (m/s)           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_UseSubMeso`           |     FALSE                    | use sub-mesoscale eddy parameterization for bolus transport             |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`subMeso_Ceff`            |     7.0E-02                  | efficiency coefficient of mixed-layer eddies                            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`subMeso_invTau`          |     2.0E-06                  | inverse of mixing timescale in sub-meso parameterization (s\ :sup:`-1`) |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`subMeso_LfMin`           |     1.0E+03                  | minimum value for length-scale :math:`L_f` (m)                          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`subMeso_Lmax`            |     110.0E+03                | maximum horizontal grid-scale length (m)                                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Visbeck_alpha`        |     0.0                      | :math:`\alpha` parameter for Visbeck et al. scheme (non-dim.)           |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Visbeck_length`       |     200.0E+03                | :math:`L` length scale parameter for Visbeck et al. scheme (m)          |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Visbeck_depth`        |     1000.0                   | depth (m) over which to average in computing Visbeck                    |
  |                                    |                              | :math:`\kappa_{\rm GM}`                                                 |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Visbeck_maxSlope`     |     :varlink:`GM_maxSlope`   | maximum slope used in computing Visbeck et al. :math:`\kappa_{\rm GM}`  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Visbeck_minVal_K`     |     0.0                      | minimum :math:`\kappa_{\rm GM}` (m\ :sup:`2`\ /s) using Visbeck et al.  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_Visbeck_maxVal_K`     |     2500.0                   | maximum :math:`\kappa_{\rm GM}` (m\ :sup:`2`\ /s) using Visbeck et al.  |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_useLeithQG`           |     FALSE                    | add Leith QG viscosity to GMRedi tensor                                 |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_iso2dFile`            |     ' '                      | input file for 2D (:math:`x,y`) scaling of isopycnal diffusivity        |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_iso1dFile`            |     ' '                      | input file for 1D vert. scaling of isopycnal diffusivity                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_bol2dFile`            |     ' '                      | input file for 2D (:math:`x,y`) scaling of thickness diffusivity        |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_bol1dFile`            |     ' '                      | input file for 1D vert. scaling of thickness diffusivity                |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_background_K3dFile`   |     ' '                      | input file for 3D (:math:`x,y,r`) :varlink:`GM_background_K`            |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_isopycK3dFile`        |     ' '                      | input file for 3D (:math:`x,y,r`) :varlink:`GM_isopycK`                 |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+
  | :varlink:`GM_MNC`                  |     :varlink:`useMNC`        | write GMREDI snapshot output using :filelink:`/pkg/mnc`                 |
  +------------------------------------+------------------------------+-------------------------------------------------------------------------+

.. _ssub_phys_pkg_gmredi_diagnostics:

GMREDI Diagnostics
==================

::

   ----------------------------------------------------------------------
   <-Name->|Levs|<- code ->|<--  Units   -->|<- Description
   ----------------------------------------------------------------------
   GM_VisbK|  1 |SM P    M1|m^2/s           |Mixing coefficient from Visbeck etal parameterization
   GM_hTrsL|  1 |SM P    M1|m               |Base depth (>0) of the Transition Layer
   GM_baseS|  1 |SM P    M1|1               |Slope at the base of the Transition Layer
   GM_rLamb|  1 |SM P    M1|1/m             |Slope vertical gradient at Trans. Layer Base (=recip.Lambda)
   SubMesLf|  1 |SM P    M1|m               |Sub-Meso horiz. Length Scale (Lf)
   SubMpsiX|  1 |UU      M1|m^2/s           |Sub-Meso transp.stream-funct. magnitude (Psi0): U component
   SubMpsiY|  1 |VV      M1|m^2/s           |Sub-Meso transp.stream-funct. magnitude (Psi0): V component
   GM_Kux  | 18 |UU P    MR|m^2/s           |K_11 element (U.point, X.dir) of GM-Redi tensor
   GM_Kvy  | 18 |VV P    MR|m^2/s           |K_22 element (V.point, Y.dir) of GM-Redi tensor
   GM_Kuz  | 18 |UU      MR|m^2/s           |K_13 element (U.point, Z.dir) of GM-Redi tensor
   GM_Kvz  | 18 |VV      MR|m^2/s           |K_23 element (V.point, Z.dir) of GM-Redi tensor
   GM_Kwx  | 18 |UM      LR|m^2/s           |K_31 element (W.point, X.dir) of GM-Redi tensor
   GM_Kwy  | 18 |VM      LR|m^2/s           |K_32 element (W.point, Y.dir) of GM-Redi tensor
   GM_Kwz  | 18 |WM P    LR|m^2/s           |K_33 element (W.point, Z.dir) of GM-Redi tensor
   GM_PsiX | 18 |UU      LR|m^2/s           |GM Bolus transport stream-function : U component
   GM_PsiY | 18 |VV      LR|m^2/s           |GM Bolus transport stream-function : V component
   GM_KuzTz| 18 |UU      MR|degC.m^3/s      |Redi Off-diagonal Temperature flux: X component
   GM_KvzTz| 18 |VV      MR|degC.m^3/s      |Redi Off-diagonal Temperature flux: Y component
   GM_KwzTz| 18 |WM      LR|degC.m^3/s      |Redi main-diagonal vertical Temperature flux
   GM_ubT  | 18 |UUr     MR|degC.m^3/s      |Zonal Mass-Weight Bolus Transp of Pot Temp
   GM_vbT  | 18 |VVr     MR|degC.m^3/s      |Meridional Mass-Weight Bolus Transp of Pot Temp
   GM_BVPcW|  1 |SU P    M1|m/s             |WKB wave speed (at Western edge location)
   GM_BVPcS|  1 |SV P    M1|m/s             |WKB wave speed (at Southern edge location)


Experiments and tutorials that use GMREDI
=========================================

-  Southern Ocean Reentrant Channel Example, in :filelink:`verification/tutorial_reentrant_channel`,
   described in :numref:`sec_eg_reentrant_channel`

-  Global Ocean Simulation, in :filelink:`verification/tutorial_global_oce_latlon`,
   described in :numref:`sec_global_oce_latlon`

-  Front Relax experiment, in :filelink:`verification/front_relax`

-  Ideal 2D Ocean experiment, in :filelink:`verification/ideal_2D_oce`.
