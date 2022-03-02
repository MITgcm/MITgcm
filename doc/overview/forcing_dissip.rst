Forcing/dissipation
-------------------

Forcing
~~~~~~~

The forcing terms :math:`\mathcal{F}` on the rhs of the equations are
provided by ‘physics packages’ and forcing packages. These are described
later on.

Dissipation
~~~~~~~~~~~

Momentum
^^^^^^^^

Many forms of momentum dissipation are available in the model. Laplacian
and biharmonic frictions are commonly used:

.. math::
   D_{V}=A_{h}\nabla _{h}^{2}v+A_{v}\frac{\partial ^{2}v}{\partial z^{2}}
   +A_{4}\nabla _{h}^{4}v
   :label: dissipation

where :math:`A_{h}` and :math:`A_{v}\ `\ are (constant) horizontal and
vertical viscosity coefficients and :math:`A_{4}\ `\ is the horizontal
coefficient for biharmonic friction. These coefficients are the same for
all velocity components.

Tracers
^^^^^^^

The mixing terms for the temperature and salinity equations have a
similar form to that of momentum except that the diffusion tensor can be
non-diagonal and have varying coefficients.

.. math::
   D_{T,S} =  \nabla  \cdot \left[ \boldsymbol{K}  \nabla  (T,S) \right] + K_{4} \nabla
   _{h}^{4}(T,S),
   :label: diffusion

where :math:`\boldsymbol{K}` is the diffusion tensor and
:math:`K_{4}\ ` the horizontal coefficient for biharmonic diffusion. In
the simplest case where the subgrid-scale fluxes of heat and salt are
parameterized with constant horizontal and vertical diffusion
coefficients, :math:`\boldsymbol{K}`, reduces to a diagonal
matrix with constant coefficients:

.. math::
   \qquad \qquad \qquad \qquad \boldsymbol{K} = \left( 
   \begin{array}{ccc}
   K_{h} & 0 & 0 \\ 
   0 & K_{h} & 0 \\ 
   0 & 0 & K_{v}
   \end{array}
   \right) \qquad \qquad \qquad
   :label: diagonal-diffusion-tensor

where :math:`K_{h}\ `\ and :math:`K_{v}\ `\ are the horizontal and
vertical diffusion coefficients. These coefficients are the same for all
tracers (temperature, salinity ... ).

