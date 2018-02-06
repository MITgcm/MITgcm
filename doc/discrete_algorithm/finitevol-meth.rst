The finite volume method: finite volumes versus finite difference
-----------------------------------------------------------------

The finite volume method is used to discretize the equations in space.
The expression “finite volume” actually has two meanings; one is the
method of embedded or intersecting boundaries (shaved or lopped cells in
our terminology) and the other is non-linear interpolation methods that
can deal with non-smooth solutions such as shocks (i.e. flux limiters
for advection). Both make use of the integral form of the conservation
laws to which the *weak solution* is a solution on each finite volume of
(sub-domain). The weak solution can be constructed out of piece-wise
constant elements or be differentiable. The differentiable equations can
not be satisfied by piece-wise constant functions.

As an example, the 1-D constant coefficient advection-diffusion
equation:

.. math:: \partial_t \theta + \partial_x ( u \theta - \kappa \partial_x \theta ) = 0

can be discretized by integrating over finite sub-domains, i.e. the
lengths :math:`\Delta x_i`:

.. math:: \Delta x \partial_t \theta + \delta_i ( F ) = 0

is exact if :math:`\theta(x)` is piece-wise constant over the interval
:math:`\Delta x_i` or more generally if :math:`\theta_i` is defined as
the average over the interval :math:`\Delta x_i`.

The flux, :math:`F_{i-1/2}`, must be approximated:

.. math:: F = u \overline{\theta} - \frac{\kappa}{\Delta x_c} \partial_i \theta

and this is where truncation errors can enter the solution. The method
for obtaining :math:`\overline{\theta}` is unspecified and a wide range
of possibilities exist including centered and upwind interpolation,
polynomial fits based on the the volume average definitions of
quantities and non-linear interpolation such as flux-limiters.

Choosing simple centered second-order interpolation and differencing
recovers the same ODE’s resulting from finite differencing for the
interior of a fluid. Differences arise at boundaries where a boundary is
not positioned on a regular or smoothly varying grid. This method is
used to represent the topography using lopped cell, see Adcroft et al. (1997)
:cite:`adcroft:97`. Subtle difference also appear in more
than one dimension away from boundaries. This happens because each
direction is discretized independently in the finite difference method
while the integrating over finite volume implicitly treats all
directions simultaneously. 
