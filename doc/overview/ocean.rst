Ocean
-----

In the ocean we interpret:

.. math::
   r=z\text{  is the height}  
   :label: ocean-z 

.. math::
   \dot{r}=\frac{Dz}{Dt}=w\text{  is the vertical velocity} 
   :label: ocean-w
 
.. math::
   \phi=\frac{p}{\rho _{c}}\text{  is the pressure}
   :label: ocean-p

.. math::
   b(\theta ,S,r)=\frac{g}{\rho _{c}} \left( \vphantom{\dot{W}} \rho (\theta,S,r) - \rho_{c}\right) 
   \text{  is the buoyancy}
   :label: ocean-b

where :math:`\rho_{c}` is a fixed reference density of water and
:math:`g` is the acceleration due to gravity.

In the above:

At the bottom of the ocean: :math:`R_{\rm fixed}(x,y)=-H(x,y)`.

The surface of the ocean is given by: :math:`R_{\rm moving}=\eta`

The position of the resting free surface of the ocean is given by
:math:`R_{o}=Z_{o}=0`.

Boundary conditions are:

.. math::
   w=0~\text{at }r=R_{\rm fixed}\text{  (ocean bottom)}
   :label: fixed-bc-ocean

.. math::
   w=\frac{D\eta }{Dt}\text{ at }r=R_{\rm moving}=\eta \text{  (ocean surface)}
   :label: moving-bc-ocean

where :math:`\eta` is the elevation of the free surface.

Then equations :eq:`horiz-mtm`- :eq:`humidity-salt` yield a
consistent set of oceanic equations which, for convenience, are written
out in :math:`z-`\coordinates in :numref:`ocean_appendix` - see eqs. :eq:`eq-ocean-mom`
to :eq:`eq-ocean-salt`.

