Kinematic Boundary conditions
-----------------------------

Vertical
~~~~~~~~

at fixed and moving :math:`r` surfaces we set (see :numref:`zandp-vert-coord`):

.. math::
   \dot{r}=0 \text{ at } r=R_{\rm fixed}(x, y)\text{  (ocean bottom, top of the atmosphere)}
   :label: fixedbc

.. math::
   \dot{r}=\frac{Dr}{Dt} \text{ at } r=R_{\rm moving}(x, y)\text{  (ocean surface, bottom of the atmosphere)}  
   :label: movingbc

Here

.. math:: R_{\rm moving}=R_{o} + \eta

where :math:`R_{o}(x,y)` is the ‘:math:`r-`\ value’ (height or pressure,
depending on whether we are in the atmosphere or ocean) of the ‘moving
surface’ in the resting fluid and :math:`\eta` is the departure from
:math:`R_{o}(x,y)` in the presence of motion.

Horizontal
~~~~~~~~~~

.. math:: \vec{\mathbf{v}}\cdot \vec{\mathbf{n}}=0
   :label: noflow

where :math:`\vec{\mathbf{n}}` is the normal to a solid boundary.

