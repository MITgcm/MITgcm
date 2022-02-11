.. _finding_the_pressure_field:

Finding the pressure field
--------------------------

Unlike the prognostic variables :math:`u`, :math:`v`, :math:`w`,
:math:`\theta` and :math:`S`, the pressure field must be obtained
diagnostically. We proceed, as before, by dividing the total
(pressure/geo) potential in to three parts, a surface part,
:math:`\phi _{s}(x,y)`, a hydrostatic part :math:`\phi _{\rm hyd}(x,y,r)`
and a non-hydrostatic part :math:`\phi _{\rm nh}(x,y,r)`, as in
:eq:`phi-split`, and writing the momentum equation as in :eq:`mom-h`.

Hydrostatic pressure
~~~~~~~~~~~~~~~~~~~~

Hydrostatic pressure is obtained by integrating :eq:`hydrostatic` vertically from :math:`r=R_{o}` 
where :math:`\phi _{\rm hyd}(r=R_{o})=0`, to yield:

.. math::

   \int_{r}^{R_{o}}\frac{\partial \phi _{\rm hyd}}{\partial r}dr=\left[ \phi _{\rm hyd}
   \right] _{r}^{R_{o}}=\int_{r}^{R_{o}}-bdr

and so

.. math:: \phi _{\rm hyd}(x,y,r)=\int_{r}^{R_{o}}bdr
   :label: hydro-phi

The model can be easily modified to accommodate a loading term (e.g
atmospheric pressure pushing down on the ocean’s surface) by setting:

.. math:: \phi _{\rm hyd}(r=R_{o})= \text{loading}
   :label: loading

Surface pressure
~~~~~~~~~~~~~~~~

The surface pressure equation can be obtained by integrating continuity,
:eq:`continuity`, vertically from :math:`r=R_{\rm fixed}` to :math:`r=R_{\rm moving}`

.. math::
   \int_{R_{\rm fixed}}^{R_{\rm moving}}\left(  \nabla _{h}\cdot \vec{\mathbf{v}
   }_{h}+\partial _{r}\dot{r}\right) dr=0

Thus:

.. math::
   \frac{\partial \eta }{\partial t}+\vec{\mathbf{v}} \cdot  \nabla  \eta
   +\int_{R_{\rm fixed}}^{R_{\rm moving}} \nabla _{h}\cdot \vec{\mathbf{v}}
   _{h}dr=0

where :math:`\eta =R_{\rm moving}-R_{o}` is the free-surface
:math:`r`-anomaly in units of :math:`r`. The above can be rearranged to yield, using Leibnitz’s theorem:

.. math::
   \frac{\partial \eta }{\partial t}+ \nabla _{h}\cdot
   \int_{R_{\rm fixed}}^{R_{\rm moving}}\vec{\mathbf{v}}_{h}dr=\text{source}
   :label: free-surface

where we have incorporated a source term.

Whether :math:`\phi` is pressure (ocean model, :math:`p/\rho _{c}`) or
geopotential (atmospheric model), in :eq:`mom-h`, the horizontal gradient term can be written

.. math::
    \nabla _{h}\phi _{s}= \nabla _{h}\left( b_{s}\eta \right)
   :label: phi-surf

where :math:`b_{s}` is the buoyancy at the surface.

In the hydrostatic limit (:math:`\epsilon _{\rm nh}=0`), equations
:eq:`mom-h`, :eq:`free-surface` and :eq:`phi-surf` can be solved by
inverting a 2-D elliptic equation for :math:`\phi _{s}` as described in
Chapter 2. Both ‘free surface’ and ‘rigid lid’ approaches are available.

Non-hydrostatic pressure
~~~~~~~~~~~~~~~~~~~~~~~~

Taking the horizontal divergence of :eq:`mom-h` and adding
:math:`\frac{\partial }{\partial r}` of :eq:`mom-w`, invoking the
continuity equation :eq:`continuity`, we deduce that:

.. math::
   \nabla_{3}^{2}\phi _{\rm nh}=  \nabla  \cdot \vec{\mathbf{G}}_{\vec{v}}-\left(
   \nabla_{h}^{2}\phi _{s}+ \nabla^2 \phi _{\rm hyd}\right) = 
    \nabla  \cdot \vec{\mathbf{F}}
   :label: 3d-invert

For a given rhs this 3-D elliptic equation must be inverted for
:math:`\phi _{\rm nh}` subject to appropriate choice of boundary conditions.
This method is usually called *The Pressure Method* [Harlow and Welch
(1965) :cite:`harlow:65`; Williams (1969) :cite:`williams:69`; Potter (1973) :cite:`potter:73`. In the hydrostatic primitive
equations case (**HPE**), the 3-D problem does not need to be solved.

Boundary Conditions
^^^^^^^^^^^^^^^^^^^

We apply the condition of no normal flow through all solid boundaries -
the coasts (in the ocean) and the bottom:

.. math:: \vec{\mathbf{v}} \cdot \hat{\boldsymbol{n}} =0
   :label: nonormalflow

where :math:`\widehat{n}` is a vector of unit length normal to the
boundary. The kinematic condition :eq:`nonormalflow` is also applied to
the vertical velocity at :math:`r=R_{\rm moving}`. No-slip
:math:`\left( v_{T}=0\right) \ `\ or slip :math:`\left( \partial v_{T}/\partial n=0\right) \ `\ conditions are employed
on the tangential component of velocity, :math:`v_{T}`, at all solid
boundaries, depending on the form chosen for the dissipative terms in
the momentum equations - see below.

Eq. :eq:`nonormalflow` implies, making use of :eq:`mom-h`, that:

.. math::
   \hat{\boldsymbol{n}} \cdot  \nabla  \phi _{\rm nh}= \hat{\boldsymbol{n}} \cdot \vec{\mathbf{F}}
   :label: inhom-neumann-nh

where

.. math::
   \vec{\mathbf{F}}=\vec{\mathbf{G}}_{\vec{v}}-\left(  \nabla _{h}\phi_{s}+ \nabla \phi _{\rm hyd}\right)

presenting inhomogeneous Neumann boundary conditions to the Elliptic
problem :eq:`3d-invert`. As shown, for example, by Williams (1969) :cite:`williams:69`, one
can exploit classical 3D potential theory and, by introducing an
appropriately chosen :math:`\delta`-function sheet of ‘source-charge’,
replace the inhomogeneous boundary condition on pressure by a
homogeneous one. The source term :math:`rhs` in :eq:`3d-invert` is the
divergence of the vector :math:`\vec{\mathbf{F}}`. By simultaneously setting :math:`\hat{\boldsymbol{n}} \cdot \vec{\mathbf{F}}=0` 
and :math:`\hat{\boldsymbol{n}} \cdot  \nabla  \phi_{\rm nh}=0\ `\ on the boundary the
following self-consistent but simpler homogenized Elliptic problem is obtained:

.. math:: \nabla ^{2}\phi _{\rm nh}= \nabla  \cdot \widetilde{\vec{\mathbf{F}}}\qquad

where :math:`\widetilde{\vec{\mathbf{F}}}` is a modified :math:`\vec{\mathbf{F}}` 
such that :math:`\widetilde{\vec{\mathbf{F}}} \cdot \hat{\boldsymbol{n}} =0`. As is implied by
:eq:`inhom-neumann-nh` the modified boundary condition becomes:

.. math:: \hat{\boldsymbol{n}} \cdot  \nabla  \phi _{\rm nh}=0
   :label: hom-neumann-nh

If the flow is ‘close’ to hydrostatic balance then the 3-d inversion
converges rapidly because :math:`\phi _{\rm nh}\ `\ is then only a small
correction to the hydrostatic pressure field (see the discussion in
Marshall et al. (1997a,b) :cite:`marshall:97a` :cite:`marshall:97b`.

The solution :math:`\phi _{\rm nh}\ `\ to :eq:`3d-invert` and
:eq:`inhom-neumann-nh` does not vanish at :math:`r=R_{\rm moving}`, and so
refines the pressure there.

