.. _atmos_appendix:

Hydrostatic Primitive Equations for the Atmosphere in Pressure Coordinates
--------------------------------------------------------------------------

The hydrostatic primitive equations (**HPE**’s) in :math:`p-`\coordinates are:

.. math::
   \frac{D\vec{\mathbf{v}}_{h}}{Dt}+f\hat{\boldsymbol{k}}\times \vec{\mathbf{v}}_{h}+ \nabla _{p}\phi = \vec{\boldsymbol{\mathcal{F}}}
   :label: atmos-mom
 
.. math::
   \frac{\partial \phi }{\partial p}+\alpha = 0
   :label: eq-p-hydro-start

.. math::
    \nabla _{p}\cdot \vec{\mathbf{v}}_{h}+\frac{\partial \omega }{\partial p} = 0
   :label: atmos-cont

.. math::
   p\alpha = RT  
   :label: atmos-eos

.. math::
   c_{v}\frac{DT}{Dt}+p\frac{D\alpha }{Dt} = \mathcal{Q}
   :label: atmos-heat

where :math:`\vec{\mathbf{v}}_{h}=(u,v,0)` is the ‘horizontal’ (on pressure surfaces) component of velocity,
:math:`\frac{D}{Dt}=\frac{\partial}{\partial t}+\vec{\mathbf{v}}_{h}\cdot  \nabla _{p}+\omega \frac{\partial }{\partial p}`
is the total derivative, :math:`f=2\Omega \sin \varphi` is the Coriolis
parameter, :math:`\phi =gz` is the geopotential, :math:`\alpha =1/\rho`
is the specific volume, :math:`\omega =\frac{Dp }{Dt}` is the vertical
velocity in the :math:`p-`\ coordinate. Equation :eq:`atmos-heat` is the
first law of thermodynamics where internal energy :math:`e=c_{v}T`,
:math:`T` is temperature, :math:`Q` is the rate of heating per unit mass
and :math:`p\frac{D\alpha }{Dt}` is the work done by the fluid in
compressing.

It is convenient to cast the heat equation in terms of potential
temperature :math:`\theta` so that it looks more like a generic
conservation law. Differentiating :eq:`atmos-eos` we get:

.. math:: p\frac{D\alpha }{Dt}+\alpha \frac{Dp}{Dt}=R\frac{DT}{Dt}

which, when added to the heat equation :eq:`atmos-heat` and using
:math:`c_{p}=c_{v}+R`, gives:

.. math::
   c_{p}\frac{DT}{Dt}-\alpha \frac{Dp}{Dt}=\mathcal{Q}
   :label: eq-p-heat-interim

Potential temperature is defined:

.. math:: \theta =T(\frac{p_{c}}{p})^{\kappa }
   :label: potential-temp

where :math:`p_{c}` is a reference pressure and
:math:`\kappa =R/c_{p}`. For convenience we will make use of the Exner
function :math:`\Pi (p)` which is defined by:

.. math:: \Pi (p)=c_{p}(\frac{p}{p_{c}})^{\kappa }
   :label: Exner

The following relations will be useful and are easily expressed in
terms of the Exner function:

.. math::
   c_{p}T=\Pi \theta \;\;;\;\;\frac{\partial \Pi }{\partial p}=\frac{\kappa \Pi 
   }{p}\;\;;\;\;\alpha =\frac{\kappa \Pi \theta }{p}=\frac{\partial \ \Pi }{
   \partial p}\theta \;\;;\;\;\frac{D\Pi }{Dt}=\frac{\partial \Pi }{\partial p}
   \frac{Dp}{Dt}

where :math:`b=\frac{\partial \ \Pi }{\partial p}\theta` is the buoyancy.

The heat equation is obtained by noting that

.. math::
   c_{p}\frac{DT}{Dt}=\frac{D(\Pi \theta )}{Dt}=\Pi \frac{D\theta }{Dt}+\theta 
   \frac{D\Pi }{Dt}=\Pi \frac{D\theta }{Dt}+\alpha \frac{Dp}{Dt}

and on substituting into :eq:`eq-p-heat-interim` gives:

.. math::
   \Pi \frac{D\theta }{Dt}=\mathcal{Q}
   :label: potential-temperature-equation

which is in conservative form.

For convenience in the model we prefer to step forward
:eq:`potential-temperature-equation` rather than :eq:`atmos-heat`.

Boundary conditions
~~~~~~~~~~~~~~~~~~~

The upper and lower boundary conditions are:

.. math::
   \begin{aligned}\mbox{at the top:}\;\;p=0 &\text{,  }\omega =\frac{Dp}{Dt}=0\end{aligned}
   :label: boundary-condition-atmosphere-top

.. math::
   \begin{aligned}\mbox{at the surface:}\;\;p=p_{s} &\text{,  }\phi =\phi _{\rm topo}=g~Z_{\rm topo}\end{aligned}
   :label: boundary-condition-atmosphere-bot

In :math:`p-`\coordinates, the upper boundary acts like a solid boundary
(:math:`\omega=0` ); in :math:`z-`\coordinates the lower boundary is analogous to a
free surface (:math:`\phi` is imposed and :math:`\omega \neq 0`).

.. _hpe-p-geo-potential-split:

Splitting the geopotential
~~~~~~~~~~~~~~~~~~~~~~~~~~

For the purposes of initialization and reducing round-off errors, the
model deals with perturbations from reference (or ‘standard’) profiles.
For example, the hydrostatic geopotential associated with the resting
atmosphere is not dynamically relevant and can therefore be subtracted
from the equations. The equations written in terms of perturbations are
obtained by substituting the following definitions into the previous
model equations:

.. math::
   \theta = \theta _{o}+\theta ^{\prime }
   :label: atmos-ref-prof-theta 

.. math::
   \alpha = \alpha _{o}+\alpha ^{\prime }
   :label: atmos-ref-prof-alpha

.. math::
   \phi  = \phi _{o}+\phi ^{\prime }
   :label: atmos-ref-prof-phi

The reference state (indicated by subscript ‘*o*’) corresponds to
horizontally homogeneous atmosphere at rest
(:math:`\theta _{o},\alpha _{o},\phi_{o}`) with surface pressure :math:`p_{o}(x,y)` that satisfies
:math:`\phi_{o}(p_{o})=g~Z_{\rm topo}`, defined:

.. math:: \theta _{o}(p) = f^{n}(p) \\
.. math:: \alpha _{o}(p)  = \Pi _{p}\theta _{o} \\
.. math:: \phi _{o}(p)  = \phi _{\rm topo}-\int_{p_{0}}^{p}\alpha _{o}dp

The final form of the **HPE**’s in :math:`p-`\coordinates is then:

.. math::
   \frac{D\vec{\mathbf{v}}_{h}}{Dt}+f\hat{\boldsymbol{k}}\times \vec{\mathbf{v}}
   _{h}+ \nabla _{p}\phi ^{\prime } = \vec{\boldsymbol{\mathcal{F}}} 
   :label: atmos-prime

.. math::
   \frac{\partial \phi ^{\prime }}{\partial p}+\alpha ^{\prime }  = 0
   :label: atmos-prime2
 
.. math::
    \nabla _{p}\cdot \vec{\mathbf{v}}_{h}+\frac{\partial \omega }{
   \partial p} = 0
   :label: atmos-prime3
 
.. math::
   \frac{\partial \Pi }{\partial p}\theta ^{\prime } = \alpha ^{\prime }
   :label: atmos-prime4

.. math::
   \frac{D\theta }{Dt} = \frac{\mathcal{Q}}{\Pi }
   :label: atmos-prime5

