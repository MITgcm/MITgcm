Solution strategy
-----------------

The method of solution employed in the **HPE**, **QH** and **NH** models
is summarized in :numref:`soln_strat`. Under all dynamics, a
2-d elliptic equation is first solved to find the surface pressure and
the hydrostatic pressure at any level computed from the weight of fluid
above. Under **HPE** and **QH** dynamics, the horizontal momentum
equations are then stepped forward and :math:`\dot{r}` found from
continuity. Under **NH** dynamics a 3-d elliptic equation must be solved
for the non-hydrostatic pressure before stepping forward the horizontal
momentum equations; :math:`\dot{r}` is found by stepping forward the
vertical momentum equation.

There is no penalty in implementing **QH** over **HPE** except, of
course, some complication that goes with the inclusion of
:math:`\cos \varphi \ ` Coriolis terms and the relaxation of the shallow
atmosphere approximation. But this leads to negligible increase in
computation. In **NH**, in contrast, one additional elliptic equation -
a three-dimensional one - must be inverted for :math:`p_{\rm nh}`. However
the ‘overhead’ of the **NH** model is essentially negligible in the
hydrostatic limit (see detailed discussion in Marshall et al. (1997) :cite:`marshall:97a`
resulting in a non-hydrostatic algorithm that, in the hydrostatic limit,
is as computationally economic as the **HPEs**.



  .. figure:: figs/solution_strategy.*
    :width: 100%
    :align: center
    :alt: diagram of basic solution strategy in MITgcm
    :name: soln_strat
    
    Basic solution strategy in MITgcm. **HPE** and **QH** forms diagnose the vertical velocity, in **NH** a prognostic equation for the vertical velocity is integrated.
