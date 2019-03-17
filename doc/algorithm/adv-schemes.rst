Linear advection schemes
========================

The advection schemes known as centered second order, centered fourth
order, first order upwind and upwind biased third order are known as
linear advection schemes because the coefficient for interpolation of
the advected tracer are linear and a function only of the flow, not the
tracer field it self. We discuss these first since they are most
commonly used in the field and most familiar.

Centered second order advection-diffusion
-----------------------------------------

The basic discretization, centered second order, is the default. It is
designed to be consistent with the continuity equation to facilitate
conservation properties analogous to the continuum. However, centered
second order advection is notoriously noisy and must be used in
conjunction with some finite amount of diffusion to produce a sensible
solution.

The advection operator is discretized:

.. math::
   {\cal A}_c \Delta r_f h_c G_{adv}^\tau = 
   \delta_i F_x + \delta_j F_y + \delta_k F_r
   :label: cent_2nd_ord

where the area integrated fluxes are given by:

.. math::

   \begin{aligned}
   F_x & = & U \overline{ \tau }^i \\
   F_y & = & V \overline{ \tau }^j \\
   F_r & = & W \overline{ \tau }^k\end{aligned}

The quantities :math:`U`, :math:`V` and :math:`W` are volume fluxes.
defined as:

.. math::

   \begin{aligned}
   U & = & \Delta y_g \Delta r_f h_w u \\
   V & = & \Delta x_g \Delta r_f h_s v \\
   W & = & {\cal A}_c w\end{aligned}

For non-divergent flow, this discretization can be shown to conserve the
tracer both locally and globally and to globally conserve tracer
variance, :math:`\tau^2`. The proof is given in
Adcroft (1995) :cite:`adcroft:95` and Adcroft et al. (1997) :cite:`adcroft:97` .

.. admonition:: S/R  :filelink:`GAD_C2_ADV_X <pkg/generic_advdiff/gad_c2_adv_x.F>`
  :class: note

    | :math:`F_x` : :varlink:`uT` ( argument )
    | :math:`U` : :varlink:`uTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_C2_ADV_Y <pkg/generic_advdiff/gad_c2_adv_y.F>`
  :class: note

    | :math:`F_y` : :varlink:`vT` ( argument )
    | :math:`V` : :varlink:`vTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_C2_ADV_R <pkg/generic_advdiff/gad_c2_adv_r.F>`
  :class: note

    | :math:`F_r` : :varlink:`wT` ( argument )
    | :math:`W` : :varlink:`rTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

Third order upwind bias advection
---------------------------------

Upwind biased third order advection offers a relatively good compromise
between accuracy and smoothness. It is not a “positive” scheme meaning
false extrema are permitted but the amplitude of such are significantly
reduced over the centered second order method.

The third order upwind fluxes are discretized:

.. math::

   \begin{aligned}
   F_x & = & U \overline{\tau - \frac{1}{6} \delta_{ii} \tau}^i
            + \frac{1}{2} |U| \delta_i \frac{1}{6} \delta_{ii} \tau \\
   F_y & = & V \overline{\tau - \frac{1}{6} \delta_{ii} \tau}^j
            + \frac{1}{2} |V| \delta_j \frac{1}{6} \delta_{jj} \tau \\
   F_r & = & W \overline{\tau - \frac{1}{6} \delta_{ii} \tau}^k
            + \frac{1}{2} |W| \delta_k \frac{1}{6} \delta_{kk} \tau \end{aligned}

At boundaries, :math:`\delta_{\hat{n}} \tau` is set to zero allowing
:math:`\delta_{nn}` to be evaluated. We are currently examine the
accuracy of this boundary condition and the effect on the solution.

.. admonition:: S/R  :filelink:`GAD_U3_ADV_X <pkg/generic_advdiff/gad_u3_adv_x.F>`
  :class: note

    | :math:`F_x` : :varlink:`uT` ( argument )
    | :math:`U` : :varlink:`uTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_U3_ADV_Y <pkg/generic_advdiff/gad_u3_adv_y.F>`
  :class: note

    | :math:`F_y` : :varlink:`vT` ( argument )
    | :math:`V` : :varlink:`vTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_U3_ADV_R <pkg/generic_advdiff/gad_u3_adv_r.F>`
  :class: note

    | :math:`F_r` : :varlink:`wT` ( argument )
    | :math:`W` : :varlink:`rTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )


Centered fourth order advection
-------------------------------

Centered fourth order advection is formally the most accurate scheme we
have implemented and can be used to great effect in high resolution
simulations where dynamical scales are well resolved. However, the scheme
is noisy, like the centered second order method, and so must be used with
some finite amount of diffusion. Bi-harmonic is recommended since it is
more scale selective and less likely to diffuse away the well resolved
gradient the fourth order scheme worked so hard to create.

The centered fourth order fluxes are discretized:

.. math::

   \begin{aligned}
   F_x & = & U \overline{\tau - \frac{1}{6} \delta_{ii} \tau}^i \\
   F_y & = & V \overline{\tau - \frac{1}{6} \delta_{ii} \tau}^j \\
   F_r & = & W \overline{\tau - \frac{1}{6} \delta_{ii} \tau}^k\end{aligned}

As for the third order scheme, the best discretization near boundaries
is under investigation but currently :math:`\delta_i \tau=0` on a
boundary.

.. admonition:: S/R  :filelink:`GAD_C4_ADV_X <pkg/generic_advdiff/gad_c4_adv_x.F>`
  :class: note

    | :math:`F_x` : :varlink:`uT` ( argument )
    | :math:`U` : :varlink:`uTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_C4_ADV_Y <pkg/generic_advdiff/gad_c4_adv_y.F>`
  :class: note

    | :math:`F_y` : :varlink:`vT` ( argument )
    | :math:`V` : :varlink:`vTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_C4_ADV_R <pkg/generic_advdiff/gad_c4_adv_r.F>`
  :class: note

    | :math:`F_r` : :varlink:`wT` ( argument )
    | :math:`W` : :varlink:`rTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

First order upwind advection
----------------------------

Although the upwind scheme is the underlying scheme for the robust or
non-linear methods given in :numref:`nonlinear_adv`, we haven’t actually implemented this method
for general use. It would be very diffusive and it is unlikely that it
could ever produce more useful results than the positive higher order
schemes.

Upwind bias is introduced into many schemes using the *abs* function and
it allows the first order upwind flux to be written:

.. math::

   \begin{aligned}
   F_x & = & U \overline{ \tau }^i - \frac{1}{2} |U| \delta_i \tau \\
   F_y & = & V \overline{ \tau }^j - \frac{1}{2} |V| \delta_j \tau \\
   F_r & = & W \overline{ \tau }^k - \frac{1}{2} |W| \delta_k \tau\end{aligned}

If for some reason the above method is desired, the second order
flux limiter scheme described in :numref:`secondord_FL` reduces to the above scheme if the
limiter is set to zero.

.. _nonlinear_adv:

Non-linear advection schemes
============================

Non-linear advection schemes invoke non-linear interpolation and are
widely used in computational fluid dynamics (non-linear does not refer
to the non-linearity of the advection operator). The flux limited
advection schemes belong to the class of finite volume methods which
neatly ties into the spatial discretization of the model.

When employing the flux limited schemes, first order upwind or
direct-space-time method, the time-stepping is switched to forward in
time.

.. _secondord_FL:

Second order flux limiters
--------------------------

The second order flux limiter method can be cast in several ways but is
generally expressed in terms of other flux approximations. For example,
in terms of a first order upwind flux and second order Lax-Wendroff
flux, the limited flux is given as:

.. math:: F = F_1 + \psi(r) F_{LW}
   :label: limited_flux

where :math:`\psi(r)` is the limiter function,

.. math:: F_1 = u \overline{\tau}^i - \frac{1}{2} |u| \delta_i \tau

is the upwind flux,

.. math:: F_{LW} = F_1 + \frac{|u|}{2} (1-c) \delta_i \tau

is the Lax-Wendroff flux and :math:`c = \frac{u \Delta t}{\Delta x}` is
the Courant (CFL) number.

The limiter function, :math:`\psi(r)`, takes the slope ratio

.. math::

   \begin{aligned}
   r = \frac{ \tau_{i-1} - \tau_{i-2} }{ \tau_{i} - \tau_{i-1} } & \forall & u > 0
   \\
   r = \frac{ \tau_{i+1} - \tau_{i} }{ \tau_{i} - \tau_{i-1} } & \forall & u < 0\end{aligned}

as its argument. There are many choices of limiter function but we
only provide the Superbee limiter (Roe 1995 :cite:`roe:85`):

.. math:: \psi(r) = \max[0,\min[1,2r],\min[2,r]]

.. admonition:: S/R  :filelink:`GAD_FLUXLIMIT_ADV_X <pkg/generic_advdiff/gad_fluxlimit_adv_x.F>`
  :class: note

    | :math:`F_x` : :varlink:`uT` ( argument )
    | :math:`U` : :varlink:`uTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_FLUXLIMIT_ADV_Y <pkg/generic_advdiff/gad_fluxlimit_adv_y.F>`
  :class: note

    | :math:`F_y` : :varlink:`vT` ( argument )
    | :math:`V` : :varlink:`vTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_FLUXLIMIT_ADV_R <pkg/generic_advdiff/gad_fluxlimit_adv_r.F>`
  :class: note

    | :math:`F_r` : :varlink:`wT` ( argument )
    | :math:`W` : :varlink:`rTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )


Third order direct space-time
-----------------------------

The direct space-time method deals with space and time discretization
together (other methods that treat space and time separately are known
collectively as the “Method of Lines”). The Lax-Wendroff scheme falls
into this category; it adds sufficient diffusion to a second order flux
that the forward-in-time method is stable. The upwind biased third order
DST scheme is:

.. math::
   \begin{aligned}F = u \left( \tau_{i-1}
           + d_0 (\tau_{i}-\tau_{i-1}) + d_1 (\tau_{i-1}-\tau_{i-2}) \right)
   \phantom{W} & \forall & u > 0 \\
   F = u \left( \tau_{i}
           - d_0 (\tau_{i}-\tau_{i-1}) - d_1 (\tau_{i+1}-\tau_{i}) \right)
   \phantom{W} & \forall & u < 0\end{aligned}
   :label: F_posneg-u

where

.. math::

   \begin{aligned}
   d_0 & = & \frac{1}{6} ( 2 - |c| ) ( 1 - |c| ) \\
   d_1 & = & \frac{1}{6} ( 1 - |c| ) ( 1 + |c| )\end{aligned}

The coefficients :math:`d_0` and :math:`d_1` approach :math:`1/3` and
:math:`1/6` respectively as the Courant number, :math:`c`, vanishes. In
this limit, the conventional third order upwind method is recovered. For
finite Courant number, the deviations from the linear method are
analogous to the diffusion added to centered second order advection in
the Lax-Wendroff scheme.

The DST3 method described above must be used in a forward-in-time manner
and is stable for :math:`0 \le |c| \le 1`. Although the scheme appears
to be forward-in-time, it is in fact third order in time and the
accuracy increases with the Courant number! For low Courant number, DST3
produces very similar results (indistinguishable in
:numref:`advect-1d-lo`) to the linear third order method but for large
Courant number, where the linear upwind third order method is unstable,
the scheme is extremely accurate (:numref:`advect-1d-hi`) with only
minor overshoots.

.. admonition:: S/R  :filelink:`GAD_DST3_ADV_X <pkg/generic_advdiff/gad_dst3_adv_x.F>`
  :class: note

    | :math:`F_x` : :varlink:`uT` ( argument )
    | :math:`U` : :varlink:`uTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_DST3_ADV_Y <pkg/generic_advdiff/gad_dst3_adv_y.F>`
  :class: note

    | :math:`F_y` : :varlink:`vT` ( argument )
    | :math:`V` : :varlink:`vTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_DST3_ADV_R <pkg/generic_advdiff/gad_dst3_adv_r.F>`
  :class: note

    | :math:`F_r` : :varlink:`wT` ( argument )
    | :math:`W` : :varlink:`rTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

Third order direct space-time with flux limiting
------------------------------------------------

The overshoots in the DST3 method can be controlled with a flux limiter.
The limited flux is written:

.. math::
   F = \frac{1}{2}(u+|u|)\left( \tau_{i-1} + \psi(r^+)(\tau_{i} - \tau_{i-1} )\right)
   + \frac{1}{2}(u-|u|)\left( \tau_{i-1} + \psi(r^-)(\tau_{i} - \tau_{i-1} )\right)
   :label: dst3_limiter

where

.. math::

   \begin{aligned}
   r^+ & = & \frac{\tau_{i-1} - \tau_{i-2}}{\tau_{i} - \tau_{i-1}} \\
   r^- & = & \frac{\tau_{i+1} - \tau_{i}}{\tau_{i} - \tau_{i-1}}\end{aligned}

and the limiter is the Sweby limiter:

.. math:: \psi(r) = \max[0, \min[\min(1,d_0+d_1r],\frac{1-c}{c}r ]]

.. admonition:: S/R  :filelink:`GAD_DST3FL_ADV_X <pkg/generic_advdiff/gad_dst3fl_adv_x.F>`
  :class: note

    | :math:`F_x` : :varlink:`uT` ( argument )
    | :math:`U` : :varlink:`uTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_DST3FL_ADV_Y <pkg/generic_advdiff/gad_dst3fl_adv_y.F>`
  :class: note

    | :math:`F_y` : :varlink:`vT` ( argument )
    | :math:`V` : :varlink:`vTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

.. admonition:: S/R  :filelink:`GAD_DST3FL_ADV_R <pkg/generic_advdiff/gad_dst3fl_adv_r.F>`
  :class: note

    | :math:`F_r` : :varlink:`wT` ( argument )
    | :math:`W` : :varlink:`rTrans` ( argument )
    | :math:`\tau` : :varlink:`tracer` ( argument )

Multi-dimensional advection
---------------------------

In many of the aforementioned advection schemes the behavior in multiple
dimensions is not necessarily as good as the one dimensional behavior.
For instance, a shape preserving monotonic scheme in one dimension can
have severe shape distortion in two dimensions if the two components of
horizontal fluxes are treated independently. There is a large body of
literature on the subject dealing with this problem and among the fixes
are operator and flux splitting methods, corner flux methods, and more.
We have adopted a variant on the standard splitting methods that allows
the flux calculations to be implemented as if in one dimension:

.. math::
   \begin{aligned}
   \tau^{n+1/3} & = & \tau^{n}
   - \Delta t \left( \frac{1}{\Delta x} \delta_i F^x(\tau^{n})
              - \tau^{n} \frac{1}{\Delta x} \delta_i u \right) \\
   \tau^{n+2/3} & = & \tau^{n+1/3}
   - \Delta t \left( \frac{1}{\Delta y} \delta_j F^y(\tau^{n+1/3})
              - \tau^{n} \frac{1}{\Delta y} \delta_i v \right) \\
   \tau^{n+3/3} & = & \tau^{n+2/3}
   - \Delta t \left( \frac{1}{\Delta r} \delta_k F^x(\tau^{n+2/3})
              - \tau^{n} \frac{1}{\Delta r} \delta_i w \right)\end{aligned}
   :label: tau_multiD

In order to incorporate this method into the general model algorithm, we
compute the effective tendency rather than update the tracer so that
other terms such as diffusion are using the :math:`n` time-level and not
the updated :math:`n+3/3` quantities:

.. math:: G^{n+1/2}_{adv} = \frac{1}{\Delta t} ( \tau^{n+3/3} - \tau^{n} )

So that the over all time-stepping looks likes:

.. math:: \tau^{n+1} = \tau^{n} + \Delta t \left( G^{n+1/2}_{adv} + G_{diff}(\tau^{n}) + G^{n}_{forcing} \right)

.. admonition:: S/R  :filelink:`GAD_ADVECTION <pkg/generic_advdiff/gad_advection.F>`
  :class: note

    | :math:`\tau` : :varlink:`tracer` ( argument )
    | :math:`G^{n+1/2}_{adv}` : :varlink:`gTracer` ( argument )
    | :math:`F_x, F_y, F_r` : :varlink:`aF` ( local )
    | :math:`U` : :varlink:`uTrans` ( local )
    | :math:`V` : :varlink:`vTrans` ( local )
    | :math:`W` : :varlink:`rTrans` ( local )

A schematic of multi-dimension time stepping for the cube sphere configuration is show in :numref:`multiDim_CS` .

  .. figure:: figs/multiDim_CS.*
    :width: 60%
    :align: center
    :alt: multiDim_CS
    :name: multiDim_CS

    Multi-dimensional advection time-stepping with cubed-sphere topology.

Comparison of advection schemes
===============================

:numref:`adv_scheme_summary` shows a summary of the different advection schemes available in MITgcm. 
“AB” stands for Adams-Bashforth and “DST” for direct space-time. The code corresponds to the number used
to select the corresponding advection scheme in the parameter file (e.g., ``tempAdvScheme=3`` in file
``data`` selects the 3rd order upwind advection scheme for temperature advection).

.. table:: MITgcm Advection Schemes
  :name: adv_scheme_summary


  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | Advection Scheme                                            | Code | Use | Use           | Stencil | Comments                                          |
  |                                                             |      | AB? | multi-dim?    | (1-D)   |                                                   |
  +=============================================================+======+=====+===============+=========+===================================================+
  | 1st order upwind                                            |  1   |  no | yes\ :sup:`*` |   3     | linear :math:`\tau`, non-linear :math:`\vec{v}`   |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | centered 2nd order                                          |  2   | yes |        no     |   3     | linear                                            |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | 3rd order upwind                                            |  3   | yes |        no     |   5     | linear :math:`\tau`                               |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | centered 4th order                                          |  4   | yes |        no     |   5     | linear                                            |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | 2nd order DST (Lax-Wendroff)                                |  20  |  no | yes\ :sup:`*` |   3     | linear :math:`\tau`, non-linear :math:`\vec{v}`   |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | 3rd order DST                                               |  30  |  no | yes\ :sup:`*` |   5     | linear :math:`\tau`, non-linear :math:`\vec{v}`   |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | 2nd order flux limiters                                     |  77  |  no | yes\ :sup:`*` |   5     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | 3rd order DST flux limiter                                  |  33  |  no | yes\ :sup:`*` |   5     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | piecewise parabolic w/“null” limiter                        |  40  |  no |        yes    |   7     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | piecewise parabolic w/“mono” limiter                        |  41  |  no |        yes    |   7     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | piecewise parabolic w/“weno” limiter                        |  42  |  no |        yes    |   7     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | piecewise quartic w/“null” limiter                          |  50  |  no |        yes    |   9     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | piecewise quartic w/“mono” limiter                          |  51  |  no |        yes    |   9     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | piecewise quartic w/“weno” limiter                          |  52  |  no |        yes    |   9     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | 7th order one-step method w/monotonicity preserving limiter |   7  |  no |        yes    |   9     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | second order-moment Prather                                 |  80  |  no |        yes    |   3     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
  | second order-moment Prather w/limiter                       |  81  |  no |        yes    |   3     | non-linear                                        |
  +-------------------------------------------------------------+------+-----+---------------+---------+---------------------------------------------------+
 

yes\ :sup:`*` indicates that either the multi-dim advection algorithm or standard approach can be utilized, controlled by a namelist parameter :varlink:`multiDimAdvection`
(in these cases, given that these schemes was designed to use multi-dim advection, using the standard approach is not recommended).
The minimum size of the required tile overlap region (:varlink:`OLx`, :varlink:`OLx`)
is (stencil size -1)/2.  The minimum overlap required by the model in general is 2,
so for some of the above choices the advection scheme will not cost anything in terms of an additional overlap requirement,
but especially given a small tile size, using scheme 7 for example would require costly additional overlap points
(note a cube sphere grid with a “wet-corner” requires doubling this overlap!)
In the ‘Comments’ column, :math:`\tau` refers to tracer advection, :math:`\vec{v}` momentum advection.

Shown in :numref:`advect-1d-lo` and :numref:`advect-1d-hi` is a 1-D comparison of advection schemes. Here we advect both a smooth hill and a hill with a more abrupt shock.
:numref:`advect-1d-lo` shown the result for a weak flow  (low Courant number) whereas  :numref:`advect-1d-hi` shows the result for a stronger flow (high Courant number).

  .. figure:: figs/advect-1d-lo.*
    :width: 100%
    :align: center
    :alt: advect-1d-lo
    :name: advect-1d-lo

    Comparison of 1-D advection schemes: Courant number is 0.05 with 60 points and solutions are shown for T=1 (one complete period). a) Shows the upwind biased schemes; first order upwind, DST3, third order upwind and second order upwind. b) Shows the centered schemes; Lax-Wendroff, DST4, centered second order, centered fourth order and finite volume fourth order. c) Shows the second order flux limiters: minmod, Superbee, MC limiter and the van Leer limiter. d) Shows the DST3 method with flux limiters due to Sweby with :math:`\mu =1` ,  :math:`\mu =c/(1-c)` and a fourth order DST method with Sweby limiter,  :math:`\mu =c/(1-c)` .

  .. figure:: figs/advect-1d-hi.*
    :width: 100%
    :align: center
    :alt: advect-1d-hi
    :name: advect-1d-hi

    Comparison of 1-D advection schemes: Courant number is 0.89 with 60 points and solutions are shown for T=1 (one complete period). a) Shows the upwind biased schemes; first order upwind and DST3. Third order upwind and second order upwind are unstable at this Courant number. b) Shows the centered schemes; Lax-Wendroff, DST4. Centered second order, centered fourth order and finite volume fourth order are unstable at this Courant number. c) Shows the second order flux limiters: minmod, Superbee, MC limiter and the van Leer limiter. d) Shows the DST3 method with flux limiters due to Sweby with :math:`\mu =1` ,  :math:`\mu =c/(1-c)` and a fourth order DST method with Sweby limiter,  :math:`\mu =c/(1-c)` .


:numref:`advect-2d-lo-diag`, :numref:`advect-2d-mid-diag` and
:numref:`advect-2d-hi-diag` show solutions to a simple diagonal advection
problem using a selection of schemes for low, moderate and high Courant
numbers, respectively. The top row shows the linear schemes, integrated
with the Adams-Bashforth method. Theses schemes are clearly unstable for
the high Courant number and weakly unstable for the moderate Courant
number. The presence of false extrema is very apparent for all Courant
numbers. The middle row shows solutions obtained with the unlimited but
multi-dimensional schemes. These solutions also exhibit false extrema
though the pattern now shows symmetry due to the multi-dimensional
scheme. Also, the schemes are stable at high Courant number where the
linear schemes weren’t. The bottom row (left and middle) shows the
limited schemes and most obvious is the absence of false extrema. The
accuracy and stability of the unlimited non-linear schemes is retained
at high Courant number but at low Courant number the tendency is to
lose amplitude in sharp peaks due to diffusion. The one dimensional
tests shown in :numref:`advect-1d-lo` and :numref:`advect-1d-hi` show
this phenomenon.

Finally, the bottom left and right panels use the same advection scheme
but the right does not use the multi-dimensional method. At low Courant
number this appears to not matter but for moderate Courant number severe
distortion of the feature is apparent. Moreover, the stability of the
multi-dimensional scheme is determined by the maximum Courant number
applied of each dimension while the stability of the method of lines is
determined by the sum. Hence, in the high Courant number plot, the
scheme is unstable.

  .. figure:: figs/advect-2d-lo-diag.*
    :width: 100%
    :align: center
    :alt: advect-2d-lo-diag
    :name: advect-2d-lo-diag

    Comparison of advection schemes in two dimensions; diagonal advection of a resolved Gaussian feature. Courant number is 0.01 with 30 :math:`\times` 30 points and solutions are shown for T=1/2. White lines indicate zero crossing (ie. the presence of false minima). The left column shows the second order schemes; top) centered second order with Adams-Bashforth, middle) Lax-Wendroff and bottom) Superbee flux limited. The middle column shows the third order schemes; top) upwind biased third order with Adams-Bashforth, middle) third order direct space-time method and bottom) the same with flux limiting. The top right panel shows the centered fourth order scheme with Adams-Bashforth and right middle panel shows a fourth order variant on the DST method. Bottom right panel shows the Superbee flux limiter (second order) applied independently in each direction (method of lines).

  .. figure:: figs/advect-2d-mid-diag.*
    :width: 100%
    :align: center
    :alt: advect-2d-mid-diag
    :name: advect-2d-mid-diag

    Comparison of advection schemes in two dimensions; diagonal advection of a resolved Gaussian feature. Courant number is 0.27 with 30 :math:`\times` 30 points and solutions are shown for T=1/2. White lines indicate zero crossing (ie. the presence of false minima). The left column shows the second order schemes; top) centered second order with Adams-Bashforth, middle) Lax-Wendroff and bottom) Superbee flux limited. The middle column shows the third order schemes; top) upwind biased third order with Adams-Bashforth, middle) third order direct space-time method and bottom) the same with flux limiting. The top right panel shows the centered fourth order scheme with Adams-Bashforth and right middle panel shows a fourth order variant on the DST method. Bottom right panel shows the Superbee flux limiter (second order) applied independently in each direction (method of lines).

  .. figure:: figs/advect-2d-hi-diag.*
    :width: 100%
    :align: center
    :alt: advect-2d-hi-diag
    :name: advect-2d-hi-diag

    Comparison of advection schemes in two dimensions; diagonal advection of a resolved Gaussian feature. Courant number is 0.47 with 30 :math:`\times` 30 points and solutions are shown for T=1/2. White lines indicate zero crossings and initial maximum values (ie. the presence of false extrema). The left column shows the second order schemes; top) centered second order with Adams-Bashforth, middle) Lax-Wendroff and bottom) Superbee flux limited. The middle column shows the third order schemes; top) upwind biased third order with Adams-Bashforth, middle) third order direct space-time method and bottom) the same with flux limiting. The top right panel shows the centered fourth order scheme with Adams-Bashforth and right middle panel shows a fourth order variant on the DST method. Bottom right panel shows the Superbee flux limiter (second order) applied independently in each direction (method of lines).

With many advection schemes implemented in the code two questions arise:
“Which scheme is best?” and “Why don’t you just offer the best advection
scheme?”. Unfortunately, no one advection scheme is “the best” for all
particular applications and for new applications it is often a matter of
trial to determine which is most suitable. Here are some guidelines but
these are not the rule;

-  If you have a coarsely resolved model, using a positive or upwind
   biased scheme will introduce significant diffusion to the solution
   and using a centered higher order scheme will introduce more noise.
   In this case, simplest may be best.

-  If you have a high resolution model, using a higher order scheme will
   give a more accurate solution but scale-selective diffusion might
   need to be employed. The flux limited methods offer similar accuracy
   in this regime.

-  If your solution has shocks or propagating fronts then a flux limited
   scheme is almost essential.

-  If your time-step is limited by advection, the multi-dimensional
   non-linear schemes have the most stability (up to Courant number 1).

-  If you need to know how much diffusion/dissipation has occurred you
   will have a lot of trouble figuring it out with a non-linear method.

-  The presence of false extrema is non-physical and this alone is the
   strongest argument for using a positive scheme.

.. _shapiro_filter:

Shapiro Filter
==============

The Shapiro filter (Shapiro 1970) :cite:`shapiro:70` is a high order
horizontal filter that efficiently remove small scale grid noise without
affecting the physical structures of a field. It is applied at the end
of the time step on both velocity and tracer fields.

Three different space operators are considered here (S1,S2 and S4). They
differ essentially by the sequence of derivative in both X and Y
directions. Consequently they show different damping response function
specially in the diagonal directions X+Y and X-Y.

Space derivatives can be computed in the real space, taking into account
the grid spacing. Alternatively, a pure computational filter can be
defined, using pure numerical differences and ignoring grid spacing.
This later form is stable whatever the grid is, and therefore specially
useful for highly anisotropic grid such as spherical coordinate grid. A
damping time-scale parameter :math:`\tau_{shap}` defines the strength of
the filter damping.

The three computational filter operators are :

.. math::

   \mathrm{S1c:}\hspace{2cm}
   [1 - 1/2 \frac{\Delta t}{\tau_{shap}}
      \{ (\frac{1}{4}\delta_{ii})^n 
       + (\frac{1}{4}\delta_{jj})^n \} ]

.. math::

   \mathrm{S2c:}\hspace{2cm}
   [1 - \frac{\Delta t}{\tau_{shap}} 
   \{ \frac{1}{8} (\delta_{ii} + \delta_{jj}) \}^n]

.. math::

   \mathrm{S4c:}\hspace{2cm}
   [1 - \frac{\Delta t}{\tau_{shap}} (\frac{1}{4}\delta_{ii})^n]
   [1 - \frac{\Delta t}{\tau_{shap}} (\frac{1}{4}\delta_{jj})^n]

In addition, the S2 operator can easily be extended to a physical space
filter:

.. math::

   \mathrm{S2g:}\hspace{2cm}
   [1 - \frac{\Delta t}{\tau_{shap}} 
   \{ \frac{L_{shap}^2}{8} \overline{\nabla}^2 \}^n]

with the Laplacian operator :math:`\overline{\nabla}^2` and a length
scale parameter :math:`L_{shap}`. The stability of this S2g filter
requires :math:`L_{shap} < \mathrm{Min}^{(Global)}(\Delta x,\Delta y)`.

.. _shapiro_diagnostics:

SHAP Diagnostics
----------------

::

    --------------------------------------------------------------
    <-Name->|Levs|parsing code|<-Units->|<- Tile (max=80c) 
    --------------------------------------------------------------
    SHAP_dT |  5 |SM      MR  |K/s      |Temperature Tendency due to Shapiro Filter
    SHAP_dS |  5 |SM      MR  |g/kg/s   |Specific Humidity Tendency due to Shapiro Filter
    SHAP_dU |  5 |UU   148MR  |m/s^2    |Zonal Wind Tendency due to Shapiro Filter
    SHAP_dV |  5 |VV   147MR  |m/s^2    |Meridional Wind Tendency due to Shapiro Filter


Nonlinear Viscosities for Large Eddy Simulation
===============================================

In Large Eddy Simulations (LES), a turbulent closure needs to be
provided that accounts for the effects of subgridscale motions on the
large scale. With sufficiently powerful computers, we could resolve the
entire flow down to the molecular viscosity scales
(:math:`L_{\nu}\approx 1 \rm cm`). Current computation allows perhaps
four decades to be resolved, so the largest problem computationally
feasible would be about 10m. Most oceanographic problems are much larger
in scale, so some form of LES is required, where only the largest scales
of motion are resolved, and the subgridscale effects on the
large-scale are parameterized.

To formalize this process, we can introduce a filter over the
subgridscale L: :math:`u_\alpha\rightarrow \overline{u_\alpha}` and L:
:math:`b\rightarrow \overline{b}`. This filter has some intrinsic length and time
scales, and we assume that the flow at that scale can be characterized
with a single velocity scale (:math:`V`) and vertical buoyancy gradient
(:math:`N^2`). The filtered equations of motion in a local Mercator
projection about the gridpoint in question (see Appendix for notation
and details of approximation) are:

.. math::
   {\frac{{ \overline{D} {{\tilde {\overline{u}}}}}} {{\overline{Dt}}}}  - \frac{{{\tilde {\overline{v}}}}
     \sin\theta}{{\rm Ro}\sin\theta_0} + \frac{{M_{Ro}}}{{\rm Ro}} \frac{\partial{\overline{\pi}}}{\partial{x}}
   = -\left({\overline{\frac{D{\tilde u}}{Dt} }} - {\frac{{\overline{D} {{\tilde {\overline{u}}}}}}{{\overline{Dt}}} }\right)
   +\frac{\nabla^2{{\tilde {\overline{u}}}}}{{\rm Re}}
   :label: mercat

.. math::
   {\frac{{ \overline{D} {{\tilde {\overline{v}}}}}} {{\overline{Dt}}}}  - \frac{{{\tilde {\overline{u}}}}
     \sin\theta}{{\rm Ro}\sin\theta_0} + \frac{{M_{Ro}}}{{\rm Ro}} \frac{\partial{\overline{\pi}}}{\partial{y}}
   = -\left({\overline{\frac{D{\tilde v}}{Dt} }} - {\frac{{\overline{D} {{\tilde {\overline{v}}}}}}{{\overline{Dt}}} }\right)
   +\frac{\nabla^2{{\tilde {\overline{v}}}}}{{\rm Re}}
   :label: mercat_v

.. math::
   \frac{{\overline{D} \overline w}}{{\overline{Dt}}} + \frac{ \frac{\partial{\overline{\pi}}}{\partial{z}} - \overline b}{{\rm Fr}^2\lambda^2}
   = -\left(\overline{\frac{D{w}}{Dt}} - \frac{{\overline{D} \overline w}}{{\overline{Dt}}}\right)
   +\frac{\nabla^2 \overline w}{{\rm Re}}\nonumber
   :label: mercat_w

.. math::
   \frac{{\overline{D} \bar b}}{{\overline{Dt}}} + \overline w =
    -\left(\overline{\frac{D{b}}{Dt}} - \frac{{\overline{D} \bar b}}{{\overline{Dt}}}\right)
   +\frac{\nabla^2 \overline b}{\Pr{\rm Re}}\nonumber
   :label: mercat_b

.. math::
   \mu^2\left({\frac{\partial{\tilde {\overline{u}}}}{\partial{x}}}   + {\frac{\partial{\tilde {\overline{v}}}}{\partial{y}}} \right)
   + {\frac{\partial{\overline w}}{\partial{z}}} = 0
   :label: cont_bfk

Tildes denote multiplication by :math:`\cos\theta/\cos\theta_0` to
account for converging meridians.

The ocean is usually turbulent, and an operational definition of
turbulence is that the terms in parentheses (the ’eddy’ terms) on the
right of :eq:`mercat` - :eq:`mercat_b`) are of comparable magnitude to the terms on the
left-hand side. The terms proportional to the inverse of , instead, are
many orders of magnitude smaller than all of the other terms in
virtually every oceanic application.

Eddy Viscosity
--------------

A turbulent closure provides an approximation to the ’eddy’ terms on the
right of the preceding equations. The simplest form of LES is just to
increase the viscosity and diffusivity until the viscous and diffusive
scales are resolved. That is, we approximate :eq:`mercat` - :eq:`mercat_b`:

.. math::
   \left({\overline{\frac{D{\tilde u}}{Dt} }} - {\frac{{\overline{D} {{\tilde {\overline{u}}}}}}{{\overline{Dt}}} }\right)
   \approx\frac{\nabla^2_h{{\tilde {\overline{u}}}}}{{\rm Re}_h}
   +\frac{{\frac{\partial^2{{\tilde {\overline{u}}}}}{{\partial{z}}^2}}}{{\rm Re}_v}
   :label: eddyvisc_u

.. math::
   \left({\overline{\frac{D{\tilde v}}{Dt} }} - {\frac{{\overline{D} {{\tilde {\overline{v}}}}}}{{\overline{Dt}}} }\right)
   \approx\frac{\nabla^2_h{{\tilde {\overline{v}}}}}{{\rm Re}_h}
   +\frac{{\frac{\partial^2{{\tilde {\overline{v}}}}}{{\partial{z}}^2}}}{{\rm Re}_v}
   :label: eddyvisc_v

.. math::
   \left(\overline{\frac{D{w}}{Dt}} - \frac{{\overline{D} \overline w}}{{\overline{Dt}}}\right)
   \approx\frac{\nabla^2_h \overline w}{{\rm Re}_h}
   +\frac{{\frac{\partial^2{\overline w}}{{\partial{z}}^2}}}{{\rm Re}_v}
   :label: eddyvisc_w

.. math::
   \left(\overline{\frac{D{b}}{Dt}} - \frac{{\overline{D} \bar b}}{{\overline{Dt}}}\right)
   \approx\frac{\nabla^2_h \overline b}{\Pr{\rm Re}_h}
   +\frac{{\frac{\partial^2{\overline b}}{{\partial{z}}^2}}}{\Pr{\rm Re}_v}\nonumber
   :label: eddyvisc_b

Reynolds-Number Limited Eddy Viscosity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One way of ensuring that the gridscale is sufficiently viscous (i.e.,
resolved) is to choose the eddy viscosity :math:`A_h` so that the
gridscale horizontal Reynolds number based on this eddy viscosity,
:math:`{\rm Re}_h`, is O(1). That is, if the gridscale is to be
viscous, then the viscosity should be chosen to make the viscous terms
as large as the advective ones. Bryan et al. (1975)
:cite:`bryan:75` notes that a computational mode is
squelched by using :math:`{\rm Re}_h<`\ 2.

MITgcm users can select horizontal eddy viscosities based on
:math:`{\rm Re}_h` using two methods. 1) The user may estimate the
velocity scale expected from the calculation and grid spacing and set
:varlink:`viscAh` to satisfy :math:`{\rm Re}_h<2`. 2) The user may use
:varlink:`viscAhReMax`, which ensures that the viscosity is always chosen so that
:math:`{\rm Re}_h<` :varlink:`viscAhReMax`. This last option should be used with
caution, however, since it effectively implies that viscous terms are
fixed in magnitude relative to advective terms. While it may be a useful
method for specifying a minimum viscosity with little effort, tests
Bryan et al. (1975) :cite:`bryan:75` have shown that setting :varlink:`viscAhReMax` =2
often tends to increase the viscosity substantially over other more
’physical’ parameterizations below, especially in regions where
gradients of velocity are small (and thus turbulence may be weak), so
perhaps a more liberal value should be used, e.g. :varlink:`viscAhReMax` =10.

While it is certainly necessary that viscosity be active at the
gridscale, the wavelength where dissipation of energy or enstrophy
occurs is not necessarily :math:`L=A_h/U`. In fact, it is by ensuring
that either the dissipation of energy in a 3-d turbulent cascade
(Smagorinsky) or dissipation of enstrophy in a 2-d turbulent cascade
(Leith) is resolved that these parameterizations derive their physical
meaning.

Vertical Eddy Viscosities
~~~~~~~~~~~~~~~~~~~~~~~~~

Vertical eddy viscosities are often chosen in a more subjective way, as
model stability is not usually as sensitive to vertical viscosity.
Usually the ’observed’ value from finescale measurements is used
(e.g. :varlink:`viscAr`\ :math:`\approx1\times10^{-4} m^2/s`). However,
Smagorinsky (1993) :cite:`smag:93` notes that the Smagorinsky
parameterization of isotropic turbulence implies a value of the vertical
viscosity as well as the horizontal viscosity (see below).

Smagorinsky Viscosity
~~~~~~~~~~~~~~~~~~~~~

Some suggest (see Smagorinsky 1963 :cite:`smag:63`; Smagorinsky 1993 :cite:`smag:93`) choosing a viscosity
that *depends on the resolved motions*. Thus, the overall viscous
operator has a nonlinear dependence on velocity. Smagorinsky chose his
form of viscosity by considering Kolmogorov’s ideas about the energy
spectrum of 3-d isotropic turbulence.

Kolmogorov supposed that energy is injected into the flow at
large scales (small :math:`k`) and is ’cascaded’ or transferred
conservatively by nonlinear processes to smaller and smaller scales
until it is dissipated near the viscous scale. By setting the energy
flux through a particular wavenumber :math:`k`, :math:`\epsilon`, to be
a constant in :math:`k`, there is only one combination of viscosity and
energy flux that has the units of length, the Kolmogorov wavelength. It
is :math:`L_\epsilon(\nu)\propto\pi\epsilon^{-1/4}\nu^{3/4}` (the
:math:`\pi` stems from conversion from wavenumber to wavelength). To
ensure that this viscous scale is resolved in a numerical model, the
gridscale should be decreased until :math:`L_\epsilon(\nu)>L` (so-called
Direct Numerical Simulation, or DNS). Alternatively, an eddy viscosity
can be used and the corresponding Kolmogorov length can be made larger
than the gridscale,
:math:`L_\epsilon(A_h)\propto\pi\epsilon^{-1/4}A_h^{3/4}` (for Large
Eddy Simulation or LES).

There are two methods of ensuring that the Kolmogorov length is resolved
in MITgcm. 1) The user can estimate the flux of energy through spectral
space for a given simulation and adjust grid spacing or :varlink:`viscAh` to ensure
that :math:`L_\epsilon(A_h)>L`; 2) The user may use the approach of
Smagorinsky with :varlink:`viscC2Smag`, which estimates the energy flux at every
grid point, and adjusts the viscosity accordingly.

Smagorinsky formed the energy equation from the momentum equations by
dotting them with velocity. There are some complications when using the
hydrostatic approximation as described by Smagorinsky (1993)
:cite:`smag:93`. The positive definite energy
dissipation by horizontal viscosity in a hydrostatic flow is
:math:`\nu D^2`, where D is the deformation rate at the viscous scale.
According to Kolmogorov’s theory, this should be a good approximation to
the energy flux at any wavenumber :math:`\epsilon\approx\nu D^2`.
Kolmogorov and Smagorinsky noted that using an eddy viscosity that
exceeds the molecular value :math:`\nu` should ensure that the energy
flux through viscous scale set by the eddy viscosity is the same as it
would have been had we resolved all the way to the true viscous scale.
That is, :math:`\epsilon\approx
A_{hSmag} \overline D^2`. If we use this approximation to estimate the
Kolmogorov viscous length, then

.. math::
   L_\epsilon(A_{hSmag})\propto\pi\epsilon^{-1/4}A_{hSmag}^{3/4}\approx\pi(A_{hSmag}
   \overline D^2)^{-1/4}A_{hSmag}^{3/4} = \pi A_{hSmag}^{1/2}\overline D^{-1/2}
   :label: kolm_visc_len

To make :math:`L_\epsilon(A_{hSmag})` scale with the gridscale, then

.. math:: A_{hSmag} = \left(\frac{{\sf viscC2Smag}}{\pi}\right)^2L^2|\overline D|
   :label: AhSmag

Where the deformation rate appropriate for hydrostatic flows with
shallow-water scaling is

.. math::
   |\overline D|=\sqrt{\left({\frac{\partial{\overline {\tilde u}}}{\partial{x}}} - {\frac{\partial{\overline {\tilde v}}}{\partial{y}}}\right)^2
   + \left({\frac{\partial{\overline {\tilde u}}}{\partial{y}}} + {\frac{\partial{\overline {\tilde v}}}{\partial{x}}}\right)^2}
   :label: Dbar_mag

The coefficient :varlink:`viscC2Smag` is what an MITgcm user sets, and it replaces
the proportionality in the Kolmogorov length with an equality. Others
(Griffies and Hallberg, 2000 :cite:`griffies:00`) suggest values of :varlink:`viscC2Smag` from 2.2 to
4 for oceanic problems. Smagorinsky (1993) :cite:`smag:93`
shows that values from 0.2 to 0.9 have been used in atmospheric modeling.

Smagorinsky (1993) :cite:`smag:93` shows that a corresponding
vertical viscosity should be used:

.. math::
   A_{vSmag} = \left(\frac{{\sf viscC2Smag}}{\pi}\right)^2H^2
   \sqrt{\left({\frac{\partial{\overline {\tilde u}}}{\partial{z}}}\right)^2
   + \left({\frac{\partial{\overline {\tilde v}}}{\partial{z}}}\right)^2}
   :label: A_vsmag

This vertical viscosity is currently not implemented in MITgcm.

Leith Viscosity
~~~~~~~~~~~~~~~

Leith (1968, 1996) :cite:`leith:68` :cite:`leith:96` notes that 2-d turbulence is
quite different from 3-d. In two-dimensional turbulence, energy cascades
to larger scales, so there is no concern about resolving the scales of
energy dissipation. Instead, another quantity, enstrophy, (which is the
vertical component of vorticity squared) is conserved in 2-d turbulence,
and it cascades to smaller scales where it is dissipated.

Following a similar argument to that above about energy flux, the
enstrophy flux is estimated to be equal to the positive-definite
gridscale dissipation rate of enstrophy :math:`\eta\approx A_{hLeith}
|\nabla\overline \omega_3|^2`. By dimensional analysis, the
enstrophy-dissipation scale is :math:`L_\eta(A_{hLeith})\propto\pi
A_{hLeith}^{1/2}\eta^{-1/6}`. Thus, the Leith-estimated length scale of
enstrophy-dissipation and the resulting eddy viscosity are

.. math::
   L_\eta(A_{hLeith})\propto\pi A_{hLeith}^{1/2}\eta^{-1/6}
   = \pi A_{hLeith}^{1/3}|\nabla \overline \omega_3|^{-1/3}
   :label: L_eta

.. math::
   A_{hLeith} = \left(\frac{{\sf viscC2Leith}}{\pi}\right)^3L^3|\nabla \overline\omega_3|
   :label: Ah_Leith

.. math::
   |\nabla\omega_3| \equiv \sqrt{\left[{\frac{\partial{\ }}{\partial{x}}}
   \left({\frac{\partial{\overline {\tilde v}}}{\partial{x}}} - {\frac{\partial{\overline {\tilde u}}}{\partial{y}}}\right)\right]^2
   + \left[{\frac{\partial{\ }}{\partial{y}}}\left({\frac{\partial{\overline {\tilde v}}}{\partial{x}}}
   - {\frac{\partial{\overline {\tilde u}}}{\partial{y}}}\right)\right]^2}
   :label: Leith3

Modified Leith Viscosity
~~~~~~~~~~~~~~~~~~~~~~~~

The argument above for the Leith viscosity parameterization uses
concepts from purely 2-dimensional turbulence, where the horizontal flow
field is assumed to be non-divergent. However, oceanic flows are only
quasi-two dimensional. While the barotropic flow, or the flow within
isopycnal layers may behave nearly as two-dimensional turbulence, there
is a possibility that these flows will be divergent. In a
high-resolution numerical model, these flows may be substantially
divergent near the grid scale, and in fact, numerical instabilities
exist which are only horizontally divergent and have little vertical
vorticity. This causes a difficulty with the Leith viscosity, which can
only respond to buildup of vorticity at the grid scale.

MITgcm offers two options for dealing with this problem. 1) The
Smagorinsky viscosity can be used instead of Leith, or in conjunction
with Leith -- a purely divergent flow does cause an increase in Smagorinsky
viscosity; 2) The :varlink:`viscC2LeithD` parameter can be set. This is a damping
specifically targeting purely divergent instabilities near the
gridscale. The combined viscosity has the form:

.. math::
   A_{hLeith} = L^3\sqrt{\left(\frac{{\sf viscC2Leith}}{\pi}\right)^6
   |\nabla \overline \omega_3|^2 + \left(\frac{{\sf viscC2LeithD}}{\pi}\right)^6
   |\nabla \nabla\cdot \overline {\tilde u}_h|^2}
   :label: Ah_Leithcomb

.. math::
   |\nabla \nabla\cdot \overline {\tilde u}_h| \equiv
   \sqrt{\left[{\frac{\partial{\ }}{\partial{x}}}\left({\frac{\partial{\overline {\tilde u}}}{\partial{x}}}
   + {\frac{\partial{\overline {\tilde v}}}{\partial{y}}}\right)\right]^2
   + \left[{\frac{\partial{\ }}{\partial{y}}}\left({\frac{\partial{\overline {\tilde u}}}{\partial{x}}}
   + {\frac{\partial{\overline {\tilde v}}}{\partial{y}}}\right)\right]^2}
   :label: Leith_mod

Whether there is any physical rationale for this correction is unclear,
but the numerical consequences are good. The divergence
in flows with the grid scale larger or comparable to the Rossby radius
is typically much smaller than the vorticity, so this adjustment only
rarely adjusts the viscosity if :varlink:`viscC2LeithD` = :varlink:`viscC2Leith`.
However, the rare regions where this
viscosity acts are often the locations for the largest vales of vertical
velocity in the domain. Since the CFL condition on vertical velocity is
often what sets the maximum timestep, this viscosity may substantially
increase the allowable timestep without severely compromising the verity
of the simulation. Tests have shown that in some calculations, a
timestep three times larger was allowed when :varlink:`viscC2LeithD` = :varlink:`viscC2Leith`.

Courant–Freidrichs–Lewy Constraint on Viscosity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Whatever viscosities are used in the model, the choice is constrained by
gridscale and timestep by the Courant–Freidrichs–Lewy (CFL) constraint
on stability:

.. math::
   \begin{aligned}
   A_h & < \frac{L^2}{4\Delta t} \\
   A_4 & \le \frac{L^4}{32\Delta t}\end{aligned}

The viscosities may be automatically limited to be no greater than
these values in MITgcm by specifying :varlink:`viscAhGridMax` :math:`<1` and
:varlink:`viscA4GridMax` :math:`<1`. Similarly-scaled minimum values of
viscosities are provided by :varlink:`viscAhGridMin` and :varlink:`viscA4GridMin`, which if
used, should be set to values :math:`\ll 1`. :math:`L` is roughly the
gridscale (see below).

Following Griffies and Hallberg (2000) :cite:`griffies:00`, we note that there is a
factor of :math:`\Delta x^2/8` difference between the harmonic and biharmonic viscosities. Thus,
whenever a non-dimensional harmonic coefficient is used in the MITgcm
(e.g. :varlink:`viscAhGridMax` :math:`<1`), the biharmonic equivalent is scaled
so that the same non-dimensional value can be used (e.g. :varlink:`viscA4GridMax` :math:`<1`).

Biharmonic Viscosity
~~~~~~~~~~~~~~~~~~~~

Holland (1978) :cite:`holland:78` suggested that eddy viscosities ought to be
focused on the dynamics at the grid scale, as larger motions would be
’resolved’. To enhance the scale selectivity of the viscous operator, he
suggested a biharmonic eddy viscosity instead of a harmonic (or Laplacian) viscosity:

.. math::
   \left({\overline{\frac{D{\tilde u}}{Dt} }} - {\frac{{\overline{D} {{\tilde {\overline{u}}}}}}{{\overline{Dt}}} }\right)
   \approx \frac{-\nabla^4_h{{\tilde {\overline{u}}}}}{{\rm Re}_4}
   + \frac{{\frac{\partial^2{{\tilde {\overline{u}}}}}{{\partial{z}}^2}}}{{\rm Re}_v}
   :label: bieddyvisc_u


.. math::
   \left({\overline{\frac{D{\tilde v}}{Dt} }} - {\frac{{\overline{D} {{\tilde {\overline{v}}}}}}{{\overline{Dt}}} }\right)
   \approx \frac{-\nabla^4_h{{\tilde {\overline{v}}}}}{{\rm Re}_4}
   + \frac{{\frac{\partial^2{{\tilde {\overline{v}}}}}{{\partial{z}}^2}}}{{\rm Re}_v}\nonumber
   :label: bieddyvisc_v


.. math::
   \left(\overline{\frac{D{w}}{Dt}} - \frac{{\overline{D} \overline w}}{{\overline{Dt}}}\right)
   \approx\frac{-\nabla^4_h\overline w}{{\rm Re}_4} + \frac{{\frac{\partial^2{\overline w}}{{\partial{z}}^2}}}{{\rm Re}_v}\nonumber
   :label: bieddyvisc_w

.. math::
   \left(\overline{\frac{D{b}}{Dt}} - \frac{{\overline{D} \bar b}}{{\overline{Dt}}}\right)
   \approx \frac{-\nabla^4_h \overline b}{\Pr{\rm Re}_4}
   +\frac{{\frac{\partial^2{\overline b}}{{\partial{z}}^2}}}{\Pr{\rm Re}_v}\nonumber
   :label: bieddyvisc_b


Griffies and Hallberg (2000) :cite:`griffies:00` propose that if one scales the
biharmonic viscosity by stability considerations, then the biharmonic
viscous terms will be similarly active to harmonic viscous terms at the
gridscale of the model, but much less active on larger scale motions.
Similarly, a biharmonic diffusivity can be used for less diffusive
flows.

In practice, biharmonic viscosity and diffusivity allow a less viscous,
yet numerically stable, simulation than harmonic viscosity and
diffusivity. However, there is no physical rationale for such operators
being of leading order, and more boundary conditions must be specified
than for the harmonic operators. If one considers the approximations of
:eq:`eddyvisc_u` - :eq:`eddyvisc_b` and :eq:`bieddyvisc_u` - :eq:`bieddyvisc_b`
to be terms in the Taylor series
expansions of the eddy terms as functions of the large-scale gradient,
then one can argue that both harmonic and biharmonic terms would occur
in the series, and the only question is the choice of coefficients.
Using biharmonic viscosity alone implies that one zeros the first
non-vanishing term in the Taylor series, which is unsupported by any
fluid theory or observation.

Nonetheless, MITgcm supports a plethora of biharmonic viscosities and
diffusivities, which are controlled with parameters named similarly to
the harmonic viscosities and diffusivities with the substitution
h :math:`\rightarrow 4` in the MITgcm parameter name. MITgcm also supports biharmonic Leith and
Smagorinsky viscosities:

.. math::
   A_{4Smag} = \left(\frac{{\sf viscC4Smag}}{\pi}\right)^2\frac{L^4}{8}|D|
   :label: A4_Smag

.. math::
   A_{4Leith} = \frac{L^5}{8}\sqrt{\left(\frac{{\sf viscC4Leith}}{\pi}\right)^6
   |\nabla \overline \omega_3|^2 + \left(\frac{{\sf viscC4LeithD}}{\pi}\right)^6
   |\nabla \nabla\cdot \overline {\bf {\tilde u}}_h|^2}
   :label: A4_Leith

However, it should be noted that unlike the harmonic forms, the
biharmonic scaling does not easily relate to whether energy-dissipation
or enstrophy-dissipation scales are resolved. If similar arguments are
used to estimate these scales and scale them to the gridscale, the
resulting biharmonic viscosities should be:

.. math::
   A_{4Smag} = \left(\frac{{\sf viscC4Smag}}{\pi}\right)^5L^5
   |\nabla^2\overline {\bf {\tilde u}}_h|
   :label: A4_Smag_alt

.. math::
   A_{4Leith} = L^6\sqrt{\left(\frac{{\sf viscC4Leith}}{\pi}\right)^{12}
   |\nabla^2 \overline \omega_3|^2 + \left(\frac{{\sf viscC4LeithD}}{\pi}\right)^{12}
   |\nabla^2 \nabla\cdot \overline {\bf {\tilde u}}_h|^2}
   :label: A4_Leith_alt

Thus, the biharmonic scaling suggested by Griffies and Hallberg (2000)
:cite:`griffies:00` implies:

.. math::
   \begin{aligned}
   |D| & \propto  L|\nabla^2\overline {\bf {\tilde u}}_h|\\
   |\nabla \overline \omega_3| & \propto L|\nabla^2 \overline \omega_3|\end{aligned}

It is not at all clear that these assumptions ought to hold. Only the
Griffies and Hallberg (2000) :cite:`griffies:00` forms are currently implemented in
MITgcm.

Selection of Length Scale
~~~~~~~~~~~~~~~~~~~~~~~~~

Above, the length scale of the grid has been denoted :math:`L`. However,
in strongly anisotropic grids, :math:`L_x` and :math:`L_y` will be quite
different in some locations. In that case, the CFL condition suggests
that the minimum of :math:`L_x` and :math:`L_y` be used. On the other
hand, other viscosities which involve whether a particular wavelength is
’resolved’ might be better suited to use the maximum of :math:`L_x` and
:math:`L_y`. Currently, MITgcm uses :varlink:`useAreaViscLength` to select between
two options. If false, the square root of the `harmonic mean <https://en.wikipedia.org/wiki/Harmonic_mean>`_
of :math:`L^2_x` and
:math:`L^2_y` is used for all viscosities, which is closer to the
minimum and occurs naturally in the CFL constraint. If :varlink:`useAreaViscLength`
is true, then the square root of the area of the grid cell is used.

Mercator, Nondimensional Equations
----------------------------------

The rotating, incompressible, Boussinesq equations of motion
(Gill, 1982) :cite:`gill:82` on a sphere can be written in Mercator
projection about a latitude :math:`\theta_0` and geopotential height
:math:`z=r-r_0`. The nondimensional form of these equations is:

.. math::
   {\rm Ro} \frac{D{\tilde u}}{Dt} - \frac{{\tilde v}
    \sin\theta}{\sin\theta_0}+M_{Ro}{\frac{\partial{\pi}}{\partial{x}}}
   + \frac{\lambda{\rm Fr}^2 M_{Ro}\cos \theta}{\mu\sin\theta_0} w
   = -\frac{{\rm Fr}^2 M_{Ro} {\tilde u} w}{r/H}
   + \frac{{\rm Ro} {\bf \hat x}\cdot\nabla^2{\bf u}}{{\rm Re}}
   :label: gill_u

.. math::
   {\rm Ro} \frac{D{\tilde v}}{Dt} +
   \frac{{\tilde u}\sin\theta}{\sin\theta_0} + M_{Ro}{\frac{\partial{\pi}}{\partial{y}}}
   = -\frac{\mu{\rm Ro} \tan\theta({\tilde u}^2 + {\tilde v}^2)}{r/L} 
   - \frac{{\rm Fr}^2M_{Ro} {\tilde v} w}{r/H}
   + \frac{{\rm Ro} {\bf \hat y}\cdot\nabla^2{\bf u}}{{\rm Re}}
   :label: gill_v

.. math::
   {\rm Fr}^2\lambda^2\frac{D{w}}{Dt}  - b + {\frac{\partial{\pi}}{\partial{z}}}
   -\frac{\lambda\cot \theta_0 {\tilde u}}{M_{Ro}}
   = \frac{\lambda\mu^2({\tilde u}^2+{\tilde v}^2)}{M_{Ro}(r/L)}
   + \frac{{\rm Fr}^2\lambda^2{\bf \hat z}\cdot\nabla^2{\bf u}}{{\rm Re}}
   :label: gill_w

.. math::
   \frac{D{b}}{Dt} + w = \frac{\nabla^2 b}{\Pr{\rm Re}}\nonumber
   :label: gill_b

.. math::
   \mu^2\left({\frac{\partial{\tilde u}}{\partial{x}}} + {\frac{\partial{\tilde v}}{\partial{y}}} \right)+{\frac{\partial{w}}{\partial{z}}}  = 0
   :label: gill_cont

Where

.. math::
   \mu\equiv\frac{\cos\theta_0}{\cos\theta},\ \ \
   {\tilde u}=\frac{u^*}{V\mu},\ \ \  {\tilde v}=\frac{v^*}{V\mu}

.. math::
   f_0\equiv2\Omega\sin\theta_0,\ \ \  
   %,\ \ \  \BFKDt\  \equiv \mu^2\left({\tilde u}\BFKpd x\  
   %+{\tilde v} \BFKpd y\  \right)+\frac{\rm Fr^2M_{Ro}}{\rm Ro} w\BFKpd z\  
   \frac{D}{Dt}  \equiv \mu^2\left({\tilde u}\frac{\partial}{\partial x}  
   +{\tilde v} \frac{\partial}{\partial y}  \right)
   +\frac{\rm Fr^2M_{Ro}}{\rm Ro} w\frac{\partial}{\partial z}

.. math::
   x\equiv \frac{r}{L} \phi \cos \theta_0, \ \ \   
   y\equiv \frac{r}{L} \int_{\theta_0}^\theta
   \frac{\cos \theta_0 {\,\rm d\theta}'}{\cos\theta'}, \ \ \   
   z\equiv \lambda\frac{r-r_0}{L}

.. math:: t^*=t \frac{L}{V},\ \ \  b^*= b\frac{V f_0M_{Ro}}{\lambda}

.. math::
  \pi^* = \pi V f_0 LM_{Ro},\ \ \  
   w^* = w V \frac{{\rm Fr}^2 \lambda M_{Ro}}{\rm Ro}

.. math:: {\rm Ro} \equiv \frac{V}{f_0 L},\ \ \  M_{Ro}\equiv \max[1,\rm Ro]

.. math::
   {\rm Fr} \equiv \frac{V}{N \lambda L}, \ \ \   
   {\rm Re} \equiv \frac{VL}{\nu}, \ \ \   
   {\rm Pr} \equiv \frac{\nu}{\kappa}

Dimensional variables are denoted by an asterisk where necessary. If we
filter over a grid scale typical for ocean models:

| 1m :math:`< L <` 100km
| 0.0001 :math:`< \lambda <` 1
| 0.001m/s :math:`< V <` 1 m/s
| :math:`f_0 <` 0.0001 s :sup:`-1`
| 0.01 s :sup:`-1` :math:`< N <` 0.0001 s :sup:`-1`
| 

these equations are very well approximated by

.. math::
   {\rm Ro}\frac{D{\tilde u}}{Dt} - \frac{{\tilde v}
     \sin\theta}{\sin\theta_0}+M_{Ro}{\frac{\partial{\pi}}{\partial{x}}}
   = -\frac{\lambda{\rm Fr}^2M_{Ro}\cos \theta}{\mu\sin\theta_0} w
   + \frac{{\rm Ro}\nabla^2{{\tilde u}}}{{\rm Re}} \\
   :label: gill_u_filt

.. math::
   {\rm Ro}\frac{D{\tilde v}}{Dt} +
   \frac{{\tilde u}\sin\theta}{\sin\theta_0}+M_{Ro}{\frac{\partial{\pi}}{\partial{y}}}
   = \frac{{\rm Ro}\nabla^2{{\tilde v}}}{{\rm Re}} \\
   :label: gill_v_filt

.. math::
   {\rm Fr}^2\lambda^2\frac{D{w}}{Dt} - b + {\frac{\partial{\pi}}{\partial{z}}}
   = \frac{\lambda\cot \theta_0 {\tilde u}}{M_{Ro}}
   +\frac{{\rm Fr}^2\lambda^2\nabla^2w}{{\rm Re}} 
   :label: gill_w_filt

.. math::
   \frac{D{b}}{Dt} + w = \frac{\nabla^2 b}{\Pr{\rm Re}} 
   :label: gill_b_filt


.. math::
   \mu^2\left({\frac{\partial{\tilde u}}{\partial{x}}} + {\frac{\partial{\tilde v}}{\partial{y}}} \right)+{\frac{\partial{w}}{\partial{z}}} = 0
   :label: gill_cont_filt

.. math::
   \nabla^2 \approx \left(\frac{\partial^2}{\partial x^2}
   +\frac{\partial^2}{\partial y^2}
   +\frac{\partial^2}{\lambda^2\partial z^2}\right)

Neglecting the non-frictional terms on the right-hand side is usually
called the ’traditional’ approximation. It is appropriate, with either
large aspect ratio or far from the tropics. This approximation is used
here, as it does not affect the form of the eddy stresses which is the
main topic. The frictional terms are preserved in this approximate form
for later comparison with eddy stresses.
