.. _hydrostatic_quasihydrostatic_forms:

Hydrostatic, Quasi-hydrostatic, Quasi-nonhydrostatic and Non-hydrostatic forms
------------------------------------------------------------------------------

Let us separate :math:`\phi` in to surface, hydrostatic and
non-hydrostatic terms:

.. math::
   \phi (x,y,r)=\phi _{s}(x,y)+\phi _{\rm hyd}(x,y,r)+\phi _{\rm nh}(x,y,r)
   :label: phi-split

and write :eq:`horiz-mtm` in the form:

.. math::
   \frac{\partial \vec{\mathbf{v}}_{h}}{\partial t}+ \nabla _{h}\phi
   _{s}+ \nabla _{h}\phi _{\rm hyd}+\epsilon _{\rm nh} \nabla _{h}\phi
   _{\rm nh}=\vec{\mathbf{G}}_{\vec{v}_{h}}  
   :label: mom-h

.. math:: 
   \frac{\partial \phi _{\rm hyd}}{\partial r}=-b
   :label: hydrostatic

.. math::
   \epsilon _{\rm nh}\frac{\partial \dot{r}}{\partial t}+\frac{\partial \phi _{\rm nh}}{
   \partial r}=G_{\dot{r}}
   :label: mom-w

Here :math:`\epsilon _{\rm nh}` is a non-hydrostatic parameter.

The :math:`\left( \vec{\mathbf{G}}_{\vec{v}},G_{\dot{r}}\right)` in
:eq:`mom-h` and :eq:`mom-w` represent advective, metric and Coriolis
terms in the momentum equations. In spherical coordinates they take the
form  [#]_ - see Marshall et al. (1997a) :cite:`marshall:97a` for a full discussion:

.. math::
   :label: gu-spherical

   G_{u} = & -\vec{\mathbf{v}} \cdot  \nabla  u && \qquad \text{advection} 

   & -\left\{ \underline{\frac{u\dot{r}}{{r}}}-\frac{uv\tan \varphi}{{r}}\right\} && \qquad \text{metric}    

   & -\left\{ -2\Omega v\sin \varphi+\underline{2\Omega \dot{r}\cos \varphi}\right\} && \qquad \text{Coriolis}  

   & +\mathcal{F}_{u} && \qquad \text{forcing/dissipation}

.. math::
   :label: gv-spherical

   G_{v} = & -\vec{\mathbf{v}} \cdot  \nabla  v && \qquad \text{advection}
 
   & -\left\{ \underline{\frac{v\dot{r}}{{r}}}-\frac{u^{2}\tan \varphi}{{r}}\right\} && \qquad \text{metric}    

   & -\left\{ 2\Omega u\sin \varphi\right\} && \qquad \text{Coriolis}  

   & +\mathcal{F}_{v} && \qquad \text{forcing/dissipation}

.. math::
   :label: gw-spherical

   G_{\dot{r}} = & -\underline{\underline{\vec{\mathbf{v}} \cdot  \nabla  \dot{r}}} && \qquad \text{advection}
 
   & -\left\{ \underline{\frac{u^{_{^{2}}}+v^{2}}{{r}}}\right\} && \qquad \text{metric}    

   & +\underline{2\Omega u\cos \varphi} && \qquad \text{Coriolis}  

   & +\underline{\underline{\mathcal{F}_{\dot{r}}}} && \qquad \text{forcing/dissipation}


In the above ‘:math:`{r}`’ is the distance from the center of the earth
and ‘:math:`\varphi` ’ is latitude (see :numref:`sphere_coor`).

Grad and div operators in spherical coordinates are defined in :ref:`operators`.


Shallow atmosphere approximation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Most models are based on the ‘hydrostatic primitive equations’ (**HPE**’s)
in which the vertical momentum equation is reduced to a statement of
hydrostatic balance and the ‘traditional approximation’ is made in which
the Coriolis force is treated approximately and the shallow atmosphere
approximation is made. MITgcm need not make the ‘traditional
approximation’. To be able to support consistent non-hydrostatic forms
the shallow atmosphere approximation can be relaxed - when dividing
through by :math:`r` in, for example, :eq:`gu-spherical`, we do not
replace :math:`r` by :math:`a`, the radius of the earth.

.. _hydro_and_quasihydro:

Hydrostatic and quasi-hydrostatic forms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These are discussed at length in Marshall et al. (1997a) :cite:`marshall:97a`.

In the ‘hydrostatic primitive equations’ (**HPE**) all the underlined
terms in Eqs. :eq:`gu-spherical`
:math:`\rightarrow` :eq:`gw-spherical` are neglected and ‘:math:`{r}`’
is replaced by ‘:math:`a`’, the mean radius of the earth. Once the
pressure is found at one level - e.g. by inverting a 2-D Elliptic
equation for :math:`\phi _{s}` at :math:`r=R_{\rm moving}` - the pressure
can be computed at all other levels by integration of the hydrostatic
relation, eq :eq:`hydrostatic`.

In the ‘quasi-hydrostatic’ equations (**QH**) strict balance between
gravity and vertical pressure gradients is not imposed. The
:math:`2\Omega u\cos\varphi` Coriolis term are not neglected and are balanced by a
non-hydrostatic contribution to the pressure field: only the terms
underlined twice in Eqs. :eq:`gu-spherical` :math:`\rightarrow` :eq:`gw-spherical` are set to
zero and, simultaneously, the shallow atmosphere approximation is
relaxed. In **QH** *all* the metric terms are retained and the full
variation of the radial position of a particle monitored. The **QH** 
vertical momentum equation :eq:`mom-w` becomes:

.. math:: \frac{\partial \phi _{\rm nh}}{\partial r}=2\Omega u\cos \varphi

making a small correction to the hydrostatic pressure.

**QH** has good energetic credentials - they are the same as for
**HPE**. Importantly, however, it has the same angular momentum
principle as the full non-hydrostatic model (**NH**) - see Marshall
et.al. (1997a) :cite:`marshall:97a`. As in **HPE** only a 2-D elliptic problem need be solved.

Non-hydrostatic and quasi-nonhydrostatic forms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MITgcm presently supports a full non-hydrostatic ocean isomorph, but
only a quasi-non-hydrostatic atmospheric isomorph.

Non-hydrostatic Ocean
^^^^^^^^^^^^^^^^^^^^^

In the non-hydrostatic ocean model all terms in equations
Eqs. :eq:`gu-spherical` :math:`\rightarrow` :eq:`gw-spherical` are
retained. A three dimensional elliptic equation must be solved subject
to Neumann boundary conditions (see below). It is important to note that
use of the full **NH** does not admit any new ‘fast’ waves in to the
system - the incompressible condition :eq:`continuity` has already
filtered out acoustic modes. It does, however, ensure that the gravity
waves are treated accurately with an exact dispersion relation. The
**NH** set has a complete angular momentum principle and consistent
energetics - see White and Bromley (1995) :cite:`white:95`; Marshall et al. (1997a) :cite:`marshall:97a`.

Quasi-nonhydrostatic Atmosphere
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the non-hydrostatic version of our atmospheric model we approximate
:math:`\dot{r}` in the vertical momentum eqs. :eq:`mom-w` and :eq:`gv-spherical` (but only here) by:

.. math:: \dot{r}=\frac{Dp}{Dt}=\frac{1}{g}\frac{D\phi }{Dt}
   :label: quasi-nh-w

where :math:`p_{\rm hy}` is the hydrostatic pressure.

Summary of equation sets supported by model
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Atmosphere
^^^^^^^^^^

Hydrostatic, and quasi-hydrostatic and quasi non-hydrostatic forms of
the compressible non-Boussinesq equations in :math:`p-`\ coordinates are
supported.

Hydrostatic and quasi-hydrostatic
'''''''''''''''''''''''''''''''''



The hydrostatic set is written out in :math:`p-`\ coordinates in
:ref:`atmos_appendix` - see eqs. :eq:`atmos-prime` to :eq:`atmos-prime5`.

Quasi-nonhydrostatic
''''''''''''''''''''

A quasi-nonhydrostatic form is also supported.

Ocean
^^^^^

Hydrostatic and quasi-hydrostatic
'''''''''''''''''''''''''''''''''

Hydrostatic, and quasi-hydrostatic forms of the incompressible
Boussinesq equations in :math:`z-`\ coordinates are supported.

Non-hydrostatic
'''''''''''''''

Non-hydrostatic forms of the incompressible Boussinesq equations in
:math:`z-` coordinates are supported - see eqs. :eq:`eq-ocean-mom` to :eq:`eq-ocean-salt`.



.. [#] In the hydrostatic primitive equations (**HPE**) all underlined terms in :eq:`gu-spherical`, :eq:`gv-spherical` and :eq:`gw-spherical` are omitted; the singly-underlined terms are included in the quasi-hydrostatic model (**QH**). The fully non-hydrostatic model (**NH**) includes all terms.

