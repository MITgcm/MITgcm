.. _crank-nicolson_baro:

Crank-Nicolson barotropic time stepping
---------------------------------------

The full implicit time stepping described previously is
unconditionally stable but damps the fast gravity waves, resulting in
a loss of potential energy. The modification presented now allows one
to combine an implicit part (:math:`\beta,\gamma`) and an explicit
part (:math:`1-\beta,1-\gamma`) for the surface pressure gradient
(:math:`\beta`) and for the barotropic flow divergence
(:math:`\gamma`). For instance, :math:`\beta=\gamma=1` is the previous fully implicit
scheme; :math:`\beta=\gamma=1/2` is the non damping (energy
conserving), unconditionally stable, Crank-Nicolson scheme;
:math:`(\beta,\gamma)=(1,0)` or :math:`=(0,1)` corresponds to the
forward - backward scheme that conserves energy but is only stable for
small time steps. In the code, :math:`\beta,\gamma` are defined as parameters,
respectively :varlink:`implicSurfPress`, :varlink:`implicDiv2DFlow`. They are read
from the main parameter file ``data`` (namelist ``PARM01``) and are set
by default to 1,1.

Equations :eq:`ustar-backward-free-surface` –
:eq:`vn+1-backward-free-surface` are modified as follows:

.. math::
   \frac{ \vec{\bf v}^{n+1} }{ \Delta t }
   + {\bf \nabla}_h b_s [ \beta {\eta}^{n+1} + (1-\beta) {\eta}^{n} ]
   + \epsilon_{nh} {\bf \nabla}_h {\phi'_{nh}}^{n+1}
   = \frac{ \vec{\bf v}^{n} }{ \Delta t }
   + \vec{\bf G}_{\vec{\bf v}} ^{(n+1/2)}
   + {\bf \nabla}_h {\phi'_{hyd}}^{(n+1/2)}

.. math::
   \epsilon_{fs} \frac{ {\eta}^{n+1} - {\eta}^{n} }{ \Delta t}
   + {\bf \nabla}_h \cdot \int_{R_{fixed}}^{R_o}
   [ \gamma \vec{\bf v}^{n+1} + (1-\gamma) \vec{\bf v}^{n}] dr
   = \epsilon_{fw} (P-E)
   :label: eta-n+1-CrankNick

We set

.. math::
     \begin{aligned}
     \vec{\bf v}^* & = &
     \vec{\bf v} ^{n} + \Delta t \vec{\bf G}_{\vec{\bf v}} ^{(n+1/2)}
     + (\beta-1) \Delta t {\bf \nabla}_h b_s {\eta}^{n}
     + \Delta t {\bf \nabla}_h {\phi'_{hyd}}^{(n+1/2)}
     \\
     {\eta}^* & = &
     \epsilon_{fs} {\eta}^{n} + \epsilon_{fw} \Delta t (P-E)
     - \Delta t {\bf \nabla}_h \cdot \int_{R_{fixed}}^{R_o}
     [ \gamma \vec{\bf v}^* + (1-\gamma) \vec{\bf v}^{n}] dr\end{aligned}

In the hydrostatic case :math:`\epsilon_{nh}=0`, allowing us to find
:math:`{\eta}^{n+1}`, thus:

.. math::
     \epsilon_{fs} {\eta}^{n+1} -
     {\bf \nabla}_h \cdot \beta\gamma \Delta t^2 b_s (R_o - R_{fixed})
     {\bf \nabla}_h {\eta}^{n+1}
     = {\eta}^*

and then to compute (:filelink:`CORRECTION_STEP <model/src/correction_step.F>`):

.. math::
     \vec{\bf v}^{n+1} = \vec{\bf v}^{*}
     - \beta \Delta t {\bf \nabla}_h b_s {\eta}^{n+1}

Notes:

#. The RHS term of equation :eq:`eta-n+1-CrankNick` corresponds the
   contribution of fresh water flux (P-E) to the free-surface variations
   (:math:`\epsilon_{fw}=1`, :varlink:`useRealFreshWaterFlux` =.TRUE. in parameter
   file ``data``). In order to remain consistent with the tracer equation,
   specially in the non-linear free-surface formulation, this term is
   also affected by the Crank-Nicolson time stepping. The RHS reads:
   :math:`\epsilon_{fw} ( \gamma (P-E)^{n+1/2} + (1-\gamma) (P-E)^{n-1/2} )`
 

#. The stability criteria with Crank-Nicolson time stepping for the pure
   linear gravity wave problem in cartesian coordinates is:

   -  :math:`\beta + \gamma < 1` : unstable

   -  :math:`\beta \geq 1/2` and :math:`\gamma \geq 1/2` : stable

   -  :math:`\beta + \gamma \geq 1` : stable if :math:`c_{max}^2 (\beta - 1/2)(\gamma - 1/2) + 1 \geq 0`
      with :math:`c_{max} = 2 \Delta t \sqrt{gH} \sqrt{ \frac{1}{\Delta x^2} + \frac{1}{\Delta y^2} }`


#. A similar mixed forward/backward time-stepping is also available for
   the non-hydrostatic algorithm, with a fraction :math:`\beta_{nh}`
   (:math:`0 < \beta_{nh} \leq 1`) of the non-hydrostatic pressure
   gradient being evaluated at time step :math:`n+1` (backward in time)
   and the remaining part (:math:`1 - \beta_{nh}`) being evaluated at
   time step :math:`n` (forward in time). The run-time parameter
   :varlink:`implicitNHPress` corresponding to the implicit fraction
   :math:`\beta_{nh}` of the non-hydrostatic pressure is set by default
   to the implicit fraction :math:`\beta` of surface pressure
   (:varlink:`implicSurfPress`), but can also be specified independently (in
   main parameter file ``data``, namelist ``PARM01``).
