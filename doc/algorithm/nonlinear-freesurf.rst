.. _nonlinear-freesurface:

Non-linear free-surface
-----------------------

Options have been added to the model that concern the free surface formulation.

Pressure/geo-potential and free surface
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the atmosphere, since :math:`\phi = \phi_{\rm topo} - \int^p_{p_s} \alpha dp`, subtracting the
reference state defined in section :numref:`hpe-p-geo-potential-split` :


.. math::
     \phi_o = \phi_{\rm topo} - \int^p_{p_o} \alpha_o dp
     \hspace{5mm}\mathrm{with}\hspace{3mm} \phi_o(p_o)=\phi_{\rm topo}

we get:

.. math:: \phi' = \phi - \phi_o = \int^{p_s}_p \alpha dp - \int^{p_o}_p \alpha_o dp

For the ocean, the reference state is simpler since :math:`\rho_c`
does not dependent on :math:`z` (:math:`b_o=g`) and the surface
reference position is uniformly :math:`z=0` (:math:`R_o=0`), and the
same subtraction leads to a similar relation. For both fluids, using
the isomorphic notations, we can write:

.. math:: \phi' = \int^{r_{\rm surf}}_r b~ dr - \int^{R_o}_r b_o dr

and re-write as:

.. math::
     \phi' = \int^{r_{\rm surf}}_{R_o} b~ dr + \int^{R_o}_r (b - b_o) dr
     :label: split-phi-Ro

or:

.. math::
    \phi' = \int^{r_{surf}}_{R_o} b_o dr + \int^{r_{\rm surf}}_r (b - b_o) dr
    :label: split-phi-bo

In section :numref:`finding_the_pressure_field`, following
eq. :eq:`split-phi-Ro`, the pressure/geo-potential :math:`\phi'` has been
separated into surface (:math:`\phi_s`), and hydrostatic anomaly
(:math:`\phi'_{\rm hyd}`). In this section, the split between :math:`\phi_s`
and :math:`\phi'_{\rm hyd}` is made according to equation :eq:`split-phi-bo`.
This slightly different definition reflects the actual implementation in
the code and is valid for both linear and non-linear free-surface
formulation, in both r-coordinate and r\*-coordinate.

Because the linear free-surface approximation ignores the tracer
content of the fluid parcel between :math:`R_o` and
:math:`r_{\rm surf}=R_o+\eta`, for consistency reasons, this part is also
neglected in :math:`\phi'_{\rm hyd}` :

.. math:: \phi'_{\rm hyd} = \int^{r_{\rm surf}}_r (b - b_o) dr \simeq \int^{R_o}_r (b - b_o) dr

Note that in this case, the two definitions of :math:`\phi_s` and
:math:`\phi'_{\rm hyd}` from equations :eq:`split-phi-Ro` and
:eq:`split-phi-bo` converge toward the same (approximated) expressions:
:math:`\phi_s = \int^{r_{\rm surf}}_{R_o} b_o dr` and
:math:`\phi'_{\rm hyd}=\int^{R_o}_r b' dr`.
On the contrary, the unapproximated formulation
(see :numref:`free_surf_effect_col_thick`) retains the full expression:
:math:`\phi'_{\rm hyd} = \int^{r_{\rm surf}}_r (b - b_o) dr` . This is
obtained by selecting :varlink:`nonlinFreeSurf` =4 in parameter file ``data``.
Regarding the surface potential:

.. math::
    \phi_s = \int_{R_o}^{R_o+\eta} b_o dr = b_s \eta
     \hspace{5mm}\mathrm{with}\hspace{5mm}
     b_s = \frac{1}{\eta} \int_{R_o}^{R_o+\eta} b_o dr

:math:`b_s \simeq b_o(R_o)` is an excellent approximation (better
than the usual numerical truncation, since generally :math:`|\eta|` is
smaller than the vertical grid increment).

For the ocean, :math:`\phi_s = g \eta` and :math:`b_s = g` is uniform.
For the atmosphere, however, because of topographic effects, the
reference surface pressure :math:`R_o=p_o` has large spatial variations
that are responsible for significant :math:`b_s` variations (from 0.8 to
1.2 :math:`\rm [m^3/kg]`). For this reason, when :varlink:`uniformLin_PhiSurf`
=.FALSE. (parameter file ``data``, namelist ``PARAM01``) a non-uniform
linear coefficient :math:`b_s` is used and computed (:filelink:`INI_LINEAR_PHISURF <model/src/ini_linear_phisurf.F>`)
according to the reference surface pressure :math:`p_o`:
:math:`b_s = b_o(R_o) = c_p \kappa (p_o / P^o_{\rm SLP})^{(\kappa - 1)} \theta_{ref}(p_o)`,
with :math:`P^o_{\rm SLP}` the mean sea-level pressure.

.. _free_surf_effect_col_thick:

Free surface effect on column total thickness (Non-linear free-surface)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The total thickness of the fluid column is :math:`r_{\rm surf} - R_{\rm fixed} =
\eta + R_o - R_{\rm fixed}`. In most applications, the free surface
displacements are small compared to the total thickness
:math:`\eta \ll H_o = R_o - R_{\rm fixed}`. In the previous sections and in
older version of the model, the linearized free-surface approximation
was made, assuming :math:`r_{\rm surf} - R_{\rm fixed} \simeq H_o` when
computing horizontal transports, either in the continuity equation or in
tracer and momentum advection terms. This approximation is dropped when
using the non-linear free-surface formulation and the total thickness,
including the time varying part :math:`\eta`, is considered when
computing horizontal transports. Implications for the barotropic part
are presented hereafter. In section :numref:`tracer-cons-nonlinear-freesurface`
consequences for tracer conservation is briefly discussed (more details
can be found in Campin et al. (2004) :cite:`cam:04`) ; the general
time-stepping is presented in section :numref:`nonlin-freesurf-timestepping`
with some limitations regarding the vertical resolution in section
:numref:`nonlin-freesurf-dzsurf`.

In the non-linear formulation, the continuous form of the model
equations remains unchanged, except for the 2D continuity equation
:eq:`discrete-time-backward-free-surface` which is now integrated from
:math:`R_{\rm fixed}(x,y)` up to :math:`r_{\rm surf}=R_o+\eta` :

.. math::
   \epsilon_{\rm fs} \partial_t \eta =
   \left. \dot{r} \right|_{r=r_{\rm surf}} + \epsilon_{\rm fw} ({\mathcal{P-E}}) =
   - {\bf \nabla}_h \cdot \int_{R_{\rm fixed}}^{R_o+\eta} \vec{\bf v} dr
   + \epsilon_{fw} ({\mathcal{P-E}})

Since :math:`\eta` has a direct effect on the horizontal velocity
(through :math:`\nabla_h \Phi_{\rm surf}`), this adds a non-linear term to
the free surface equation. Several options for the time discretization
of this non-linear part can be considered, as detailed below.

If the column thickness is evaluated at time step :math:`n`, and with
implicit treatment of the surface potential gradient, equations
:eq:`eq-solve2D` and :eq:`eq-solve2D_rhs` become:

.. math::

   \begin{aligned}
   \epsilon_{\rm fs} {\eta}^{n+1} -
   {\bf \nabla}_h \cdot \Delta t^2 (\eta^{n}+R_o-R_{\rm fixed})
   {\bf \nabla}_h b_s {\eta}^{n+1}
   = {\eta}^*\end{aligned}

where

.. math::

   \begin{aligned}
   {\eta}^* = \epsilon_{\rm fs} \: {\eta}^{n} -
   \Delta t {\bf \nabla}_h \cdot \int_{R_{\rm fixed}}^{R_o+\eta^n} \vec{\bf v}^* dr
   \: + \: \epsilon_{\rm fw} \Delta_t ({\mathcal{P-E}})^{n}\end{aligned}

This method requires us to update the solver matrix at each time step.

Alternatively, the non-linear contribution can be evaluated fully
explicitly:

.. math::

   \begin{aligned}
   \epsilon_{\rm fs} {\eta}^{n+1} -
   {\bf \nabla}_h \cdot \Delta t^2 (R_o-R_{\rm fixed})
   {\bf \nabla}_h b_s {\eta}^{n+1}
   = {\eta}^*
   +{\bf \nabla}_h \cdot \Delta t^2 (\eta^{n})
   {\bf \nabla}_h b_s {\eta}^{n}\end{aligned}

This formulation allows one to keep the initial solver matrix unchanged
though throughout the integration, since the non-linear free surface
only affects the RHS.

Finally, another option is a “linearized” formulation where the total
column thickness appears only in the integral term of the RHS
:eq:`eq-solve2D_rhs` but not directly in the equation :eq:`eq-solve2D`.

Those different options (see :numref:`nonlinFreeSurf-flags`) have
been tested and show little differences. However, we recommend the use
of the most precise method (:varlink:`nonlinFreeSurf` =4) since the computation cost
involved in the solver matrix update is negligible.

.. table:: Non-linear free-surface flags
   :name: nonlinFreeSurf-flags

   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   | Parameter                 | Value   | Description                                                                             |
   +===========================+=========+=========================================================================================+
   | :varlink:`nonlinFreeSurf` | -1      | linear free-surface, restart from a pickup file                                         |
   |                           |         | produced with #undef :varlink:`EXACT_CONSERV` code                                      |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   |                           | 0       | linear free-surface (= default)                                                         |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   |                           | 4       | full non-linear free-surface                                                            |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   |                           | 3       | same as 4 but neglecting :math:`\int_{R_o}^{R_o+\eta} b' dr` in :math:`\Phi'_{\rm hyd}` |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   |                           | 2       | same as 3 but do not update cg2d solver matrix                                          |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   |                           | 1       | same as 2 but treat momentum as in linear free-surface                                  |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   | :varlink:`select_rStar`   | 0       | do not use :math:`r^*` vertical coordinate (= default)                                  |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   |                           | 2       | use :math:`r^*` vertical coordinate                                                     |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+
   |                           | 1       | same as 2 but without the contribution of the                                           |
   |                           |         | slope of the coordinate in :math:`\nabla \Phi`                                          |
   +---------------------------+---------+-----------------------------------------------------------------------------------------+


.. _tracer-cons-nonlinear-freesurface:

Tracer conservation with non-linear free-surface
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To ensure global tracer conservation (i.e., the total amount) as well as
local conservation, the change in the surface level thickness must be
consistent with the way the continuity equation is integrated, both in
the barotropic part (to find :math:`\eta`) and baroclinic part (to find
:math:`w = \dot{r}`).

To illustrate this, consider the shallow water model, with a source of
fresh water (:math:`\mathcal{P}`):

.. math:: \partial_t h +  \nabla  \cdot h \vec{\bf v} = \mathcal{P}

where :math:`h` is the total thickness of the water column. To conserve
the tracer :math:`\theta` we have to discretize:

.. math::
   \partial_t (h \theta) +  \nabla  \cdot ( h \theta \vec{\bf v})
     = \mathcal{P} \theta_{\rm rain}

Using the implicit (non-linear) free surface described above
(:numref:`press_meth_linear`) we have:

.. math::
   \begin{aligned}
   h^{n+1} = h^{n} - \Delta t  \nabla  \cdot (h^n \, \vec{\bf v}^{n+1} ) + \Delta t \mathcal{P} \\\end{aligned}

The discretized form of the tracer equation must adopt the same “form”
in the computation of tracer fluxes, that is, the same value of
:math:`h`, as used in the continuity equation:

.. math::
   \begin{aligned}
   h^{n+1} \, \theta^{n+1} = h^n \, \theta^n
           - \Delta t  \nabla  \cdot (h^n \, \theta^n \, \vec{\bf v}^{n+1})
           + \Delta t \mathcal{P} \theta_{\rm rain}\end{aligned}

The use of a 3 time-levels time-stepping scheme such as the
Adams-Bashforth make the conservation sightly tricky. The current
implementation with the Adams-Bashforth time-stepping provides an exact
local conservation and prevents any drift in the global tracer content
(Campin et al. (2004) :cite:`cam:04`). Compared to the linear free-surface
method, an additional step is required: the variation of the water
column thickness (from :math:`h^n` to :math:`h^{n+1}`) is not
incorporated directly into the tracer equation. Instead, the model uses
the :math:`G_\theta` terms (first step) as in the linear free surface
formulation (with the “*surface correction*” turned “on”, see tracer
section):

.. math::
   G_\theta^n = \left(-  \nabla  \cdot (h^n \, \theta^n \, \vec{\bf v}^{n+1})
            - \dot{r}_{\rm surf}^{n+1} \theta^n \right) / h^n

Then, in a second step, the thickness variation (expansion/reduction)
is taken into account:

.. math::
   \theta^{n+1} = \theta^n + \Delta t \frac{h^n}{h^{n+1}}
      \left( G_\theta^{(n+1/2)} + \mathcal{P} (\theta_{\mathrm{rain}} - \theta^n )/h^n \right)

Note that with a simple forward time step (no Adams-Bashforth), these
two formulations are equivalent, since
:math:`(h^{n+1} - h^{n})/ \Delta t = \mathcal{P} -  \nabla  \cdot (h^n \, \vec{\bf v}^{n+1} ) = P + \dot{r}_{\rm surf}^{n+1}`

.. _nonlin-freesurf-timestepping:

Time stepping implementation of the non-linear free-surface
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The grid cell thickness was hold constant with the linear free-surface;
with the non-linear free-surface, it is now varying in time, at least at
the surface level. This implies some modifications of the general
algorithm described earlier in sections :numref:`adams-bashforth-sync` and
:numref:`adams-bashforth-staggered`.

A simplified version of the staggered in time, non-linear free-surface
algorithm is detailed hereafter, and can be compared to the equivalent
linear free-surface case (eq. :eq:`Gv-n-staggered` to
:eq:`t-n+1-staggered`) and can also be easily transposed to the
synchronous time-stepping case. Among the simplifications, salinity
equation, implicit operator and detailed elliptic equation are
omitted. Surface forcing is explicitly written as fluxes of
temperature, fresh water and momentum,
:math:`\mathcal{Q}^{n+1/2}, \mathcal{P}^{n+1/2}, \mathcal{F}_{\bf v}^n` respectively. :math:`h^n`
and :math:`dh^n` are the column and grid box thickness in
r-coordinate.

  .. math::
     \phi^{n}_{\rm hyd} = \int b(\theta^{n},S^{n},r) dr
     :label: phi-hyd-nlfs

  .. math::
     \vec{\bf G}_{\vec{\bf v}}^{n-1/2}\hspace{-2mm} =
     \vec{\bf G}_{\vec{\bf v}} (dh^{n-1},\vec{\bf v}^{n-1/2})
     \hspace{+2mm};\hspace{+2mm}
     \vec{\bf G}_{\vec{\bf v}}^{(n)} =
        \frac{3}{2} \vec{\bf G}_{\vec{\bf v}}^{n-1/2}
     -  \frac{1}{2} \vec{\bf G}_{\vec{\bf v}}^{n-3/2}
     :label: Gv-n-nlfs

  .. math::
     \vec{\bf v}^{*} = \vec{\bf v}^{n-1/2} + \Delta t \frac{dh^{n-1}}{dh^{n}} \left(
     \vec{\bf G}_{\vec{\bf v}}^{(n)} + F_{\vec{\bf v}}^{n}/dh^{n-1} \right)
     - \Delta t \nabla \phi_{\rm hyd}^{n}
     :label: vstar-nlfs

  .. math::
     \longrightarrow \rm update \phantom{x} \rm model \phantom{x} \rm geometry : {\bf hFac}(dh^n)

  .. math::
     \begin{aligned}
     \eta^{n+1/2} \hspace{-1mm} & =
     \eta^{n-1/2} + \Delta t P^{n+1/2} - \Delta t
      \nabla  \cdot \int \vec{\bf v}^{n+1/2} dh^{n} \\
     & = \eta^{n-1/2} + \Delta t P^{n+1/2} - \Delta t
      \nabla  \cdot \int \!\!\! \left( \vec{\bf v}^* - g \Delta t \nabla \eta^{n+1/2} \right) dh^{n}\end{aligned}
     :label: nstar-nlfs

  .. math::
     \vec{\bf v}^{n+1/2}\hspace{-2mm} =
     \vec{\bf v}^{*} - g \Delta t \nabla \eta^{n+1/2}
     :label: v-n+1-nlfs

  .. math::
     h^{n+1} = h^{n} + \Delta t P^{n+1/2} - \Delta t
        \nabla  \cdot \int \vec{\bf v}^{n+1/2} dh^{n}
     :label: h-n+1-nlfs

  .. math::
     G_{\theta}^{n} = G_{\theta} ( dh^{n}, u^{n+1/2}, \theta^{n} )
     \hspace{+2mm};\hspace{+2mm}
     G_{\theta}^{(n+1/2)} = \frac{3}{2} G_{\theta}^{n} - \frac{1}{2} G_{\theta}^{n-1}
     :label: Gt-n-nlfs

  .. math::
     \theta^{n+1} =\theta^{n} + \Delta t \frac{dh^n}{dh^{n+1}} \left(
     G_{\theta}^{(n+1/2)}
     +( P^{n+1/2} (\theta_{\mathrm{rain}}-\theta^n) + \mathcal{Q}^{n+1/2})/dh^n \right)
     :label: t-n+1-nlfs

Two steps have been added to linear free-surface algorithm (eq.
:eq:`Gv-n-staggered` to :eq:`t-n+1-staggered`): Firstly, the model
“geometry” (here the **hFacC,W,S**) is updated just before entering
:filelink:`SOLVE_FOR_PRESSURE <model/src/solve_for_pressure.F>`,
using the current :math:`dh^{n}` field.
Secondly, the vertically integrated continuity equation
:eq:`h-n+1-nlfs` has been added (:varlink:`exactConserv` =.TRUE., in
parameter file ``data``, namelist ``PARM01``) just before computing the
vertical velocity, in subroutine :filelink:`INTEGR_CONTINUITY <model/src/integr_continuity.F>`. Although this
equation might appear redundant with :eq:`nstar-nlfs`, the
integrated column thickness :math:`h^{n+1}` will be different from
:math:`\eta^{n+1/2} + H`  in the following cases:

-  when Crank-Nicolson time-stepping is used (see :numref:`crank-nicolson_baro`).

-  when filters are applied to the flow field, after :eq:`v-n+1-nlfs`,
   and alter the divergence of the flow.

-  when the solver does not iterate until convergence; for example,
   because a too large residual target was set (:varlink:`cg2dTargetResidual`,
   parameter file ``data``, namelist ``PARM02``).

In this staggered time-stepping algorithm, the momentum tendencies are
computed using :math:`dh^{n-1}` geometry factors :eq:`Gv-n-nlfs`
and then rescaled in subroutine :filelink:`TIMESTEP <model/src/timestep.F>`, :eq:`vstar-nlfs`,
similarly to tracer tendencies (see :numref:`tracer-cons-nonlinear-freesurface`).
The tracers are stepped forward later,
using the recently updated flow field :math:`{\bf v}^{n+1/2}` and the
corresponding model geometry :math:`dh^{n}` to compute the tendencies
:eq:`Gt-n-nlfs`; then the tendencies are rescaled by
:math:`dh^n/dh^{n+1}` to derive the new tracers values
:math:`(\theta,S)^{n+1}` (:eq:`t-n+1-nlfs`, in subroutines :filelink:`CALC_GT <model/src/calc_gt.F>`,
:filelink:`CALC_GS <model/src/calc_gs.F>`).

Note that the fresh-water input is added in a consistent way in the
continuity equation and in the tracer equation, taking into account the
fresh-water temperature :math:`\theta_{\mathrm{rain}}`.

Regarding the restart procedure, two 2D fields :math:`h^{n-1}` and
:math:`(h^n-h^{n-1})/\Delta t` in addition to the standard state
variables and tendencies (:math:`\eta^{n-1/2}`,
:math:`{\bf v}^{n-1/2}`, :math:`\theta^n`, :math:`S^n`,
:math:`{\bf G}_{\bf v}^{n-3/2}`, :math:`G_{\theta,S}^{n-1}`) are
stored in a “*pickup*” file. The model restarts reading this
pickup file, then updates the model geometry according to
:math:`h^{n-1}`, and compute :math:`h^n` and the vertical velocity
before starting the main calling sequence (eq. :eq:`phi-hyd-nlfs` to
:eq:`t-n+1-nlfs`, :filelink:`FORWARD_STEP <model/src/forward_step.F>`).

.. admonition:: S/R  :filelink:`INTEGR_CONTINUITY <model/src/integr_continuity.F>`
  :class: note

    | :math:`h^{n+1} - H_o` : :varlink:`etaH` ( :filelink:`DYNVARS.h <model/inc/DYNVARS.h>` )
    | :math:`h^n - H_o` : :varlink:`etaHnm1` ( :filelink:`SURFACE.h <model/inc/SURFACE.h>` )
    | :math:`(h^{n+1} - h^n ) / \Delta t` : :varlink:`dEtaHdt` ( :filelink:`SURFACE.h <model/inc/SURFACE.h>` )


.. _nonlin-freesurf-dzsurf:

Non-linear free-surface and vertical resolution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When the amplitude of the free-surface variations becomes as large as
the vertical resolution near the surface, the surface layer thickness
can decrease to nearly zero or can even vanish completely. This later
possibility has not been implemented, and a minimum relative thickness
is imposed (:varlink:`hFacInf`, parameter file ``data``, namelist ``PARM01``) to
prevent numerical instabilities caused by very thin surface level.

A better alternative to the vanishing level problem relies on a different vertical coordinate
:math:`r^*` : The time variation of the total column thickness becomes
part of the :math:`r^*` coordinate motion, as in a :math:`\sigma_{z},\sigma_{p}`
model, but the fixed part related to topography is treated as in a
height or pressure coordinate model. A complete description is given in
Adcroft and Campin (2004) :cite:`adcroft:04a`.

The time-stepping implementation of the :math:`r^*` coordinate is
identical to the non-linear free-surface in :math:`r` coordinate, and
differences appear only in the spacial discretization.

