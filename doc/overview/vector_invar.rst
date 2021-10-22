Vector invariant form
---------------------

For some purposes it is advantageous to write momentum advection in
eq :eq:`horiz-mtm` and :eq:`vert-mtm` in the (so-called) ‘vector invariant’ form:

.. math::
   \frac{D\vec{\mathbf{v}}}{Dt}=\frac{\partial \vec{\mathbf{v}}}{\partial t}
   +\left(  \nabla  \times \vec{\mathbf{v}}\right) \times \vec{\mathbf{v}} +  \nabla 
   \left[ \frac{1}{2}(\vec{\mathbf{v}}\cdot \vec{\mathbf{v}})\right]
   :label: vi-identity

This permits alternative numerical treatments of the non-linear terms
based on their representation as a vorticity flux. Because gradients of
coordinate vectors no longer appear on the rhs of :eq:`vi-identity`,
explicit representation of the metric terms in :eq:`gu-spherical`,
:eq:`gv-spherical` and :eq:`gw-spherical`, can be avoided: information
about the geometry is contained in the areas and lengths of the volumes
used to discretize the model.

