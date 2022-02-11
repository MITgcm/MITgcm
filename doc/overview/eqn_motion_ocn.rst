.. _ocean_appendix:

Equations of Motion for the Ocean
---------------------------------

We review here the method by which the standard (Boussinesq,
incompressible) HPE’s for the ocean written in :math:`z-`\coordinates are
obtained. The non-Boussinesq equations for oceanic motion are:

.. math::
   \frac{D\vec{\mathbf{v}}_{h}}{Dt}+f\hat{\boldsymbol{k}}\times \vec{\mathbf{v}}
   _{h}+\frac{1}{\rho }  \nabla _{z}p  = \vec{\boldsymbol{\mathcal{F}}}_h 
   :label: non-boussinesq_horizmom

.. math::
   \epsilon _{\rm nh}\frac{Dw}{Dt}+g+\frac{1}{\rho }\frac{\partial p}{\partial z} = \epsilon _{\rm nh}\mathcal{F}_{w}
   :label: non-boussinesq_vertmom

.. math::
   \frac{1}{\rho }\frac{D\rho }{Dt}+ \nabla _{z}\cdot \vec{\mathbf{v}}
   _{h}+\frac{\partial w}{\partial z}  = 0
   :label: eq-zns-cont

.. math::
   \rho  = \rho (\theta ,S,p)
   :label: eq-zns-eos

.. math::
   \frac{D\theta }{Dt}  = \mathcal{Q}_{\theta }
   :label: eq-zns-heat

.. math::
   \frac{DS}{Dt} = \mathcal{Q}_{s}  
   :label: eq-zns-salt

These equations permit acoustics modes, inertia-gravity waves,
non-hydrostatic motions, a geostrophic (Rossby) mode and a thermohaline
mode. As written, they cannot be integrated forward consistently - if we
step :math:`\rho` forward in :eq:`eq-zns-cont`, the answer will not be
consistent with that obtained by stepping :eq:`eq-zns-heat` and
:eq:`eq-zns-salt` and then using :eq:`eq-zns-eos` to yield :math:`\rho`. It
is therefore necessary to manipulate the system as follows.
Differentiating the EOS (equation of state) gives:

.. math::
   \frac{D\rho }{Dt}=\left. \frac{\partial \rho }{\partial \theta }\right|
   _{S,p}\frac{D\theta }{Dt}+\left. \frac{\partial \rho }{\partial S}\right|
   _{\theta ,p}\frac{DS}{Dt}+\left. \frac{\partial \rho }{\partial p}\right|
   _{\theta ,S}\frac{Dp}{Dt}
   :label: EOSexpansion

Note that :math:`\frac{\partial \rho }{\partial p}=\frac{1}{c_{s}^{2}}`
is the reciprocal of the sound speed (:math:`c_{s}`) squared.
Substituting into :eq:`eq-zns-cont` gives:

.. math::
   \frac{1}{\rho c_{s}^{2}}\frac{Dp}{Dt}+ \nabla _{z}\cdot \vec{\mathbf{
   v}}+\partial _{z}w\approx 0  
   :label: eq-zns-pressure

where we have used an approximation sign to indicate that we have
assumed adiabatic motion, dropping the :math:`\frac{D\theta }{Dt}` and
:math:`\frac{DS}{Dt}`. Replacing :eq:`eq-zns-cont` with :eq:`eq-zns-pressure`
yields a system that can be explicitly integrated forward:

.. math::
   \frac{D\vec{\mathbf{v}}_{h}}{Dt}+f\hat{\boldsymbol{k}}\times \vec{\mathbf{v}}
   _{h}+\frac{1}{\rho } \nabla _{z}p = \vec{\boldsymbol{\mathcal{F}}}_h 
   :label: eq-cns-hmom 

.. math::
   \epsilon _{\rm nh}\frac{Dw}{Dt}+g+\frac{1}{\rho }\frac{\partial p}{\partial z} = \epsilon _{\rm nh}\mathcal{F}_{w}
   :label: eq-cns-hydro

.. math::
   \frac{1}{\rho c_{s}^{2}}\frac{Dp}{Dt}+ \nabla _{z}\cdot \vec{\mathbf{v}}_{h}+\frac{\partial w}{\partial z} = 0
   :label: eq-cns-cont

.. math::
   \rho  = \rho (\theta ,S,p)  
   :label: eq-cns-eos

.. math::
   \frac{D\theta }{Dt}  = \mathcal{Q}_{\theta }  
   :label: eq-cns-heat

.. math::
   \frac{DS}{Dt}  = \mathcal{Q}_{s}
   :label: eq-cns-salt

Compressible z-coordinate equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here we linearize the acoustic modes by replacing :math:`\rho` with
:math:`\rho _{o}(z)` wherever it appears in a product (ie. non-linear
term) - this is the ‘Boussinesq assumption’. The only term that then
retains the full variation in :math:`\rho` is the gravitational
acceleration:

.. math::
   \frac{D\vec{\mathbf{v}}_{h}}{Dt}+ f \hat{\boldsymbol{k}} \times \vec{\mathbf{v}}
   _{h}+\frac{1}{\rho _{o}} \nabla _{z}p = \vec{\boldsymbol{\mathcal{F}}}_h 
   :label: eq-zcb-hmom

.. math::
   \epsilon _{\rm nh}\frac{Dw}{Dt}+\frac{g\rho }{\rho _{o}}+\frac{1}{\rho _{o}}
   \frac{\partial p}{\partial z}  = \epsilon _{\rm nh}\mathcal{F}_{w}
   :label: eq-zcb-hydro

.. math::
   \frac{1}{\rho _{o}c_{s}^{2}}\frac{Dp}{Dt}+ \nabla _{z}\cdot \vec{
   \mathbf{v}}_{h}+\frac{\partial w}{\partial z}  = 0  
   :label: eq-zcb-cont

.. math::
   \rho = \rho (\theta ,S,p)
   :label: eq-zcb-eos

.. math::
   \frac{D\theta }{Dt} = \mathcal{Q}_{\theta }
   :label: eq-zcb-heat

.. math::
   \frac{DS}{Dt} = \mathcal{Q}_{s}
   :label: eq-zcb-salt

These equations still retain acoustic modes. But, because the
“compressible” terms are linearized, the pressure equation :eq:`eq-zcb-cont`
can be integrated implicitly with ease (the time-dependent term appears
as a Helmholtz term in the non-hydrostatic pressure equation). These are
the *truly* compressible Boussinesq equations. Note that the EOS must
have the same pressure dependency as the linearized pressure term, ie.
:math:`\left. \frac{\partial \rho }{\partial p}\right| _{\theta ,S}=\frac{1}{c_{s}^{2}}`, for consistency.

‘Anelastic’ z-coordinate equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The anelastic approximation filters the acoustic mode by removing the
time-dependency in the continuity (now pressure) equation
:eq:`eq-zcb-cont`. This could be done simply by noting that
:math:`\frac{Dp}{Dt}\approx -g\rho _{o} \frac{Dz}{Dt}=-g\rho _{o}w`, 
but this leads to an inconsistency between
continuity and EOS. A better solution is to change the dependency on
pressure in the EOS by splitting the pressure into a reference function
of height and a perturbation:

.. math:: \rho =\rho \left(\theta ,S,p_{o}(z)+\epsilon _{s}p^{\prime } \right)

Remembering that the term :math:`\frac{Dp}{Dt}` in continuity comes
from differentiating the EOS, the continuity equation then becomes:

.. math::

   \frac{1}{\rho _{o}c_{s}^{2}}\left( \frac{Dp_{o}}{Dt}+\epsilon _{s}\frac{
   Dp^{\prime }}{Dt}\right) + \nabla _{z}\cdot \vec{\mathbf{v}}_{h}+
   \frac{\partial w}{\partial z}=0

If the time- and space-scales of the motions of interest are longer
than those of acoustic modes, then
:math:`\frac{Dp^{\prime }}{Dt}\ll \frac{Dp_{o}}{Dt},  \nabla \cdot \vec{\mathbf{v}}_{h}`
in the continuity equations and :math:`\left. \frac{\partial \rho }{\partial p}\right| _{\theta ,S}\frac{
Dp^{\prime }}{Dt}\ll \left. \frac{\partial \rho }{\partial p}\right| _{\theta
,S}\frac{Dp_{o}}{Dt}` in the EOS :eq:`EOSexpansion`. Thus we set :math:`\epsilon_{s}=0`, removing the
dependency on :math:`p^{\prime }` in the continuity equation and EOS. Expanding
:math:`\frac{Dp_{o}(z)}{Dt}=-g\rho _{o}w` then leads to the anelastic continuity equation:

.. math::
   \nabla _{z}\cdot \vec{\mathbf{v}}_{h}+\frac{\partial w}{\partial z}-
   \frac{g}{c_{s}^{2}}w = 0
   :label: eq-za-cont1

A slightly different route leads to the quasi-Boussinesq continuity
equation where we use the scaling
:math:`\frac{\partial \rho ^{\prime }}{\partial t}+
\nabla _{3}\cdot \rho ^{\prime }\vec{\mathbf{v}}\ll \nabla 
_{3}\cdot \rho _{o}\vec{\mathbf{v}}` yielding:

.. math::
   \nabla _{z}\cdot \vec{\mathbf{v}}_{h}+\frac{1}{\rho _{o}}\frac{
   \partial \left( \rho _{o}w\right) }{\partial z} = 0
   :label: eq-za-cont2

Equations :eq:`eq-za-cont1` and :eq:`eq-za-cont2` are in fact the same equation
if:

.. math:: \frac{1}{\rho _{o}}\frac{\partial \rho _{o}}{\partial z} = -\frac{g}{c_{s}^{2}}

Again, note that if :math:`\rho _{o}` is evaluated from prescribed
:math:`\theta _{o}` and :math:`S_{o}` profiles, then the EOS dependency
on :math:`p_{o}` and the term :math:`\frac{g}{c_{s}^{2}}` in continuity should
be referred to those same profiles. The full set of ‘quasi-Boussinesq’ or ‘anelastic’ 
equations for the ocean are then:

.. math::
   \frac{D\vec{\mathbf{v}}_{h}}{Dt}+f\hat{\boldsymbol{k}}\times \vec{\mathbf{v}}
   _{h}+\frac{1}{\rho _{o}} \nabla _{z}p = \vec{\boldsymbol{\mathcal{F}}}_h
   :label: eq-zab-hmom

.. math::
   \epsilon _{\rm nh}\frac{Dw}{Dt}+\frac{g\rho }{\rho _{o}}+\frac{1}{\rho _{o}}
   \frac{\partial p}{\partial z} = \epsilon _{\rm nh}\mathcal{F}_{w}
   :label: eq-zab-hydro

.. math::
    \nabla _{z}\cdot \vec{\mathbf{v}}_{h}+\frac{1}{\rho _{o}}\frac{
   \partial \left( \rho _{o}w\right) }{\partial z} = 0
   :label: eq-zab-cont

.. math::
   \rho = \rho \left(\theta ,S,p_{o}(z) \right)
   :label: eq-zab-eos

.. math::
   \frac{D\theta }{Dt} = \mathcal{Q}_{\theta }
   :label: eq-zab-heat

.. math::
   \frac{DS}{Dt} = \mathcal{Q}_{s}
   :label: eq-zab-salt

Incompressible z-coordinate equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, the objective is to drop the depth dependence of :math:`\rho _{o}`
and so, technically, to also remove the dependence of :math:`\rho` on
:math:`p_{o}`. This would yield the “truly” incompressible Boussinesq
equations:

.. math::
   \frac{D\vec{\mathbf{v}}_{h}}{Dt}+f\hat{\boldsymbol{k}}\times \vec{\mathbf{v}}
   _{h}+\frac{1}{\rho _{c}} \nabla _{z}p = \vec{\boldsymbol{\mathcal{F}}}_h 
   :label: eq-ztb-hmom

.. math::
   \epsilon _{\rm nh}\frac{Dw}{Dt}+\frac{g\rho }{\rho _{c}}+\frac{1}{\rho _{c}}
   \frac{\partial p}{\partial z} = \epsilon _{\rm nh}\mathcal{F}_{w}
   :label: eq-ztb-hydro

.. math::
    \nabla _{z}\cdot \vec{\mathbf{v}}_{h}+\frac{\partial w}{\partial z} = 0
   :label: eq-ztb-cont

.. math::
   \rho = \rho (\theta ,S)
   :label: eq-ztb-eos

.. math::
   \frac{D\theta }{Dt} = \mathcal{Q}_{\theta }
   :label: eq-ztb-heat

.. math::
   \frac{DS}{Dt} = \mathcal{Q}_{s}
   :label: eq-ztb-salt

where :math:`\rho _{c}` is a constant reference density of water.

Compressible non-divergent equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The above “incompressible” equations are incompressible in both the flow
and the density. In many oceanic applications, however, it is important
to retain compressibility effects in the density. To do this we must
split the density thus:

.. math:: \rho =\rho _{o}+\rho ^{\prime }

We then assert that variations with depth of :math:`\rho _{o}` are
unimportant while the compressible effects in :math:`\rho ^{\prime }`
are:

.. math:: \rho _{o}=\rho _{c}

.. math:: \rho ^{\prime }=\rho (\theta ,S,p_{o}(z))-\rho _{o}

This then yields what we can call the semi-compressible Boussinesq
equations:

.. math::
   \frac{D\vec{\mathbf{v}}_{h}}{Dt}+f\hat{\boldsymbol{k}}\times \vec{\mathbf{v}}
   _{h}+\frac{1}{\rho _{c}} \nabla _{z}p^{\prime } = \vec{\boldsymbol{\mathcal{F}}}_h 
   :label: eq-ocean-mom

.. math::
   \epsilon _{\rm nh}\frac{Dw}{Dt}+\frac{g\rho ^{\prime }}{\rho _{c}}+\frac{1}{\rho
   _{c}}\frac{\partial p^{\prime }}{\partial z} = \epsilon _{\rm nh}\mathcal{F}_{w}
   :label: eq-ocean-wmom

.. math::
    \nabla _{z}\cdot \vec{\mathbf{v}}_{h}+\frac{\partial w}{\partial z} = 0
   :label: eq-ocean-cont

.. math::
   \rho ^{\prime } = \rho (\theta ,S,p_{o}(z))-\rho _{c}
   :label: eq-ocean-eos

.. math::
   \frac{D\theta }{Dt} = \mathcal{Q}_{\theta }
   :label: eq-ocean-theta

.. math::
   \frac{DS}{Dt} = \mathcal{Q}_{s}
   :label: eq-ocean-salt

Note that the hydrostatic pressure of the resting fluid, including that
associated with :math:`\rho _{c}`, is subtracted out since it has no
effect on the dynamics.

Though necessary, the assumptions that go into these equations are messy
since we essentially assume a different EOS for the reference density
and the perturbation density. Nevertheless, it is the hydrostatic
(:math:`\epsilon_{\rm nh}=0`) form of these equations that are used throughout the ocean
modeling community and referred to as the primitive equations (**HPE**’s).
