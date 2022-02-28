Atmosphere
----------

In the atmosphere, (see :numref:`zandp-vert-coord`), we interpret:

.. math:: r=p\text{  is the pressure}
   :label: atmos-r

.. math::
   \dot{r}=\frac{Dp}{Dt}=\omega \text{  is the vertical velocity in p coordinates}
   :label: atmos-omega

.. math:: \phi =g\,z\text{  is the geopotential height}  
   :label: atmos-phi

.. math::
   b=\frac{\partial \Pi }{\partial p}\theta \text{  is the buoyancy}
   :label: atmos-b

.. math::
   \theta =T \left( \frac{p_{c}}{p} \right)^{\kappa} \text{  is potential temperature}
   :label: atmos-theta

.. math:: S=q \text{  is the specific humidity}
   :label: atmos-s

where

.. math:: T\text{ is absolute temperature}

.. math:: p\text{ is the pressure}

.. math::
   \begin{aligned}
   &&z\text{ is the height of the pressure surface} \\
   &&g\text{ is the acceleration due to gravity}\end{aligned}

In the above the ideal gas law, :math:`p=\rho RT`, has been expressed in
terms of the Exner function :math:`\Pi (p)` given by :eq:`exner`
(see also :numref:`atmos_appendix`)

.. math:: \Pi (p)=c_{p} \left( \frac{p}{p_{c}} \right)^{\kappa},
   :label: exner

where :math:`p_{c}` is a reference pressure and :math:`\kappa = R/c_{p}`
with :math:`R` the gas constant and :math:`c_{p}` the specific heat of
air at constant pressure.

At the top of the atmosphere (which is ‘fixed’ in our :math:`r`
coordinate):

.. math:: R_{\rm fixed}=p_{\rm top}=0.

In a resting atmosphere the elevation of the mountains at the bottom is
given by

.. math:: R_{\rm moving}=R_{o}(x,y)=p_{o}(x,y) ,

i.e. the (hydrostatic) pressure at the top of the mountains in a
resting atmosphere.

The boundary conditions at top and bottom are given by:

.. math::
   \omega =0~\text{at }r=R_{\rm fixed} \text{ (top of the atmosphere)}
   :label: fixed-bc-atmos

.. math::
   \omega =~\frac{Dp_{s}}{Dt}\text{ at }r=R_{\rm moving}\text{ (bottom of the atmosphere)}
   :label: moving-bc-atmos

Then the (hydrostatic form of) equations
:eq:`horiz-mtm`-:eq:`humidity-salt` yields a consistent set of
atmospheric equations which, for convenience, are written out in
:math:`p-`\coordinates in :numref:`atmos_appendix` - see
eqs. :eq:`atmos-prime`-:eq:`atmos-prime5`.

