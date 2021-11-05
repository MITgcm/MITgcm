.. _chap_autodiff:

Automatic Differentiation
*************************

Author: Patrick Heimbach

*Automatic differentiation* (AD), also referred to as algorithmic (or,
more loosely, computational) differentiation, involves automatically
deriving code to calculate partial derivatives from an existing fully
non-linear prognostic code (see Griewank and Walther, 2008 :cite:`griewank:08`).
A software
tool is used that parses and transforms source files according to a set
of linguistic and mathematical rules. AD tools are like source-to-source
translators in that they parse a program code as input and produce a new
program code as output (we restrict our discussion to source-to-source
tools, ignoring operator-overloading tools). However, unlike a pure
source-to-source translation, the output program represents a new
algorithm, such as the evaluation of the Jacobian, the Hessian, or
higher derivative operators. In principle, a variety of derived
algorithms can be generated automatically in this way.

MITgcm has been adapted for use with the Tangent linear and Adjoint
Model Compiler (TAMC) and its successor TAF (Transformation of
Algorithms in Fortran), developed by Ralf Giering
(Giering and Kaminski, 1998 :cite:`giering:98`, Giering, 2000
:cite:`giering:00`). The
first application of the adjoint of MITgcm for sensitivity studies was
published by Marotzke et al. (1999) :cite:`maro-eta:99`.
Stammer et al. (1997, 2002) :cite:`stammer:97` :cite:`stammer:02` use MITgcm and its adjoint
for ocean state estimation studies. In the following we shall refer to
TAMC and TAF synonymously, except were explicitly stated otherwise.

As of mid-2007 we are also able to generate fairly efficient adjoint
code of the MITgcm using a new, open-source AD tool, called OpenAD (see
Naumann, 2006 :cite:`naumann:06` and Utke et al., 2008 :cite:`utke:08`).
This enables us for the
first time to compare adjoint models generated from different AD tools,
providing an additional accuracy check, complementary to
finite-difference gradient checks. OpenAD and its application to MITgcm
is described in detail in :numref:`ad_openad`.

The AD tool exploits the chain rule for computing the first derivative
of a function with respect to a set of input variables. Treating a given
forward code as a composition of operations – each line representing a
compositional element, the chain rule is rigorously applied to the code,
line by line. The resulting tangent linear or adjoint code, then, may be
thought of as the composition in forward or reverse order, respectively,
of the Jacobian matrices of the forward code’s compositional elements.

Some basic algebra
==================

Let :math:`\cal{M}` be a general nonlinear, model, i.e., a mapping from
the :math:`m`-dimensional space :math:`U \subset \mathbb{R}^m` of input
variables :math:`\vec{u}=(u_1,\ldots,u_m)` (model parameters, initial
conditions, boundary conditions such as forcing functions) to the
:math:`n`-dimensional space :math:`V \subset \mathbb{R}^n` of model output
variable :math:`\vec{v}=(v_1,\ldots,v_n)` (model state, model
diagnostics, objective function, ...) under consideration:

.. math::
   \begin{aligned}
   {\cal M} \, : & \, U \,\, \longrightarrow \, V \\
   ~      & \, \vec{u} \,\, \longmapsto \, \vec{v} \, = \, 
   {\cal M}(\vec{u})\end{aligned}
   :label: fulloperator
 
The vectors :math:`\vec{u} \in U` and :math:`\vec{v} \in V` may be
represented with respect to some given basis vectors
:math:`{\rm span} (U) = \{ {\vec{e}_i} \}_{i = 1, \ldots , m}` and
:math:`{\rm span} (V) = \{ {\vec{f}_j} \}_{j = 1, \ldots , n}` as

.. math::
   \vec{u} \, = \, \sum_{i=1}^{m} u_i \, {\vec{e}_i},
   \qquad
   \vec{v} \, = \, \sum_{j=1}^{n} v_j \, {\vec{f}_j}

Two routes may be followed to determine the sensitivity of the output
variable :math:`\vec{v}` to its input :math:`\vec{u}`.

Forward or direct sensitivity
-----------------------------

Consider a perturbation to the input variables :math:`\delta \vec{u}`
(typically a single component
:math:`\delta \vec{u} = \delta u_{i} \, {\vec{e}_{i}}`). Their effect on
the output may be obtained via the linear approximation of the model
:math:`{\cal M}` in terms of its Jacobian matrix :math:`M`, evaluated
in the point :math:`u^{(0)}` according to

.. math::
   \delta \vec{v} \, = \, M |_{\vec{u}^{(0)}} \, \delta \vec{u}
   :label: tangent_linear

with resulting output perturbation :math:`\delta \vec{v}`. In
components
:math:`M_{j i} \, = \, \partial {\cal M}_{j} / \partial u_{i}`, it
reads

.. math::
   \delta v_{j} \, = \, \sum_{i} 
   \left. \frac{\partial {\cal M}_{j}}{\partial u_{i}} \right|_{u^{(0)}} \, 
   \delta u_{i}
   :label: jacobi_matrix

:eq:`tangent_linear` is the tangent linear model (TLM). In contrast
to the full nonlinear model :math:`{\cal M}`, the operator :math:`M`
is just a matrix which can readily be used to find the forward
sensitivity of :math:`\vec{v}` to perturbations in :math:`u`, but if
there are very many input variables :math:`(\gg O(10^{6})` for
large-scale oceanographic application), it quickly becomes prohibitive
to proceed directly as in :eq:`tangent_linear`, if the impact of each
component :math:`{\bf e_{i}}` is to be assessed.

Reverse or adjoint sensitivity
------------------------------

Let us consider the special case of a scalar objective function
:math:`{\cal J}(\vec{v})` of the model output (e.g., the total meridional
heat transport, the total uptake of CO\ :sub:`2` in the Southern Ocean
over a time interval, or a measure of some model-to-data misfit)

.. math::
   \begin{aligned}
   \begin{array}{cccccc}
   {\cal J}  \, : &  U & 
   \longrightarrow & V &   
   \longrightarrow & \mathbb{R} \\
   ~       &  \vec{u} & \longmapsto     & \vec{v}={\cal M}(\vec{u}) & 
   \longmapsto     & {\cal J}(\vec{u}) = {\cal J}({\cal M}(\vec{u}))
   \end{array}\end{aligned}
   :label: compo

The perturbation of :math:`{\cal J}` around a fixed point
:math:`{\cal J}_0`,

.. math:: {\cal J} \, = \, {\cal J}_0 \, + \, \delta {\cal J}

can be expressed in both bases of :math:`\vec{u}` and
:math:`\vec{v}` with respect to their corresponding inner product
:math:`\left\langle \,\, , \,\, \right\rangle`

.. math::
   \begin{aligned}
   {\cal J} & = \,
   {\cal J} |_{\vec{u}^{(0)}} \, + \, 
   \left\langle \, \nabla _{u}{\cal J}^T |_{\vec{u}^{(0)}} \, , \, \delta \vec{u} \, \right\rangle 
   \, + \, O(\delta \vec{u}^2) \\
   ~ & = \,
   {\cal J} |_{\vec{v}^{(0)}} \, + \, 
   \left\langle \, \nabla _{v}{\cal J}^T |_{\vec{v}^{(0)}} \, , \, \delta \vec{v} \, \right\rangle
   \, + \, O(\delta \vec{v}^2)
   \end{aligned}
   :label: deljidentity

(note, that the gradient :math:`\nabla f` is a co-vector, therefore
its transpose is required in the above inner product). Then, using the
representation of :math:`\delta {\cal J} =
\left\langle \, \nabla _{v}{\cal J}^T \, , \, \delta \vec{v} \, \right\rangle`,
the definition of an adjoint operator :math:`A^{\ast}` of a given
operator :math:`A`,

.. math::
   \left\langle \, A^{\ast} \vec{x} \, , \, \vec{y} \, \right\rangle =
   \left\langle \, \vec{x} \, , \,  A \vec{y} \, \right\rangle

which for finite-dimensional vector spaces is just the transpose of
:math:`A`,

.. math:: A^{\ast} \, = \, A^T

and from :eq:`tangent_linear`, :eq:`deljidentity`, we note that
(omitting :math:`|`\ ’s):

.. math::
   \delta {\cal J}
   \, = \,
   \left\langle \, \nabla _{v}{\cal J}^T \, , \, \delta \vec{v} \, \right\rangle
   \, = \,
   \left\langle \, \nabla _{v}{\cal J}^T \, , \, M \, \delta \vec{u} \, \right\rangle
   \, = \, 
   \left\langle \, M^T \, \nabla _{v}{\cal J}^T \, , \, 
   \delta \vec{u} \, \right\rangle
   :label: inner

With the identity :eq:`deljidentity`, we then find that the gradient
:math:`\nabla _{u}{\cal J}` can be readily inferred by invoking the
adjoint :math:`M^{\ast }` of the tangent linear model :math:`M`

.. math::
   \begin{aligned}
   \nabla _{u}{\cal J}^T |_{\vec{u}} & 
   = \, M^T |_{\vec{u}} \cdot \nabla _{v}{\cal J}^T |_{\vec{v}}  \\
   ~ & = \, M^T |_{\vec{u}} \cdot \delta \vec{v}^{\ast} \\
   ~ & = \, \delta \vec{u}^{\ast}
   \end{aligned}
   :label: adjoint

:eq:`adjoint` is the adjoint model (ADM), in which :math:`M^T` is the
adjoint (here, the transpose) of the tangent linear operator :math:`M`,
:math:`\,\delta \vec{v}^{\ast}` the adjoint variable of the model state
:math:`\vec{v}`, and :math:`\delta \vec{u}^{\ast}` the adjoint
variable of the control variable :math:`\vec{u}`.

The reverse nature of the adjoint calculation can be readily seen as
follows. Consider a model integration which consists of
:math:`\Lambda` consecutive operations
:math:`{\cal M}_{\Lambda} (  {\cal M}_{\Lambda-1} ( ...... ( {\cal M}_{\lambda} (......
( {\cal M}_{1} ( {\cal M}_{0}(\vec{u}) ))))`, where the
:math:`{\cal M}`\ ’s could be the elementary steps, i.e., single lines in
the code of the model, or successive time steps of the model
integration, starting at step 0 and moving up to step :math:`\Lambda`,
with intermediate
:math:`{\cal M}_{\lambda} (\vec{u}) = \vec{v}^{(\lambda+1)}` and final
:math:`{\cal M}_{\Lambda} (\vec{u}) = \vec{v}^{(\Lambda+1)} = \vec{v}`.
Let :math:`{\cal J}` be a cost function which explicitly depends on the
final state :math:`\vec{v}` only (this restriction is for clarity
reasons only). :math:`{\cal J}(u)` may be decomposed according to:

.. math::
   {\cal J}({\cal M}(\vec{u})) \, = \, 
   {\cal J} ( {\cal M}_{\Lambda} (  {\cal M}_{\Lambda-1} ( 
   ...... ( {\cal M}_{\lambda} (......
   ( {\cal M}_{1} ( {\cal M}_{0}(\vec{u}) )))))
   :label: compos

Then, according to the chain rule, the forward calculation reads, in
terms of the Jacobi matrices (we’ve omitted the :math:`|`\ ’s which,
nevertheless are important to the aspect of *tangent* linearity; note
also that by definition
:math:`\langle \, \nabla _{v}{\cal J}^T \, , \, \delta \vec{v} \, \rangle
= \nabla_v {\cal J} \cdot \delta \vec{v}` )

.. math::
   \begin{aligned}
   \nabla_v {\cal J} (M(\delta \vec{u})) & = \,
   \nabla_v {\cal J} \cdot M_{\Lambda}
   \cdot ...... \cdot M_{\lambda} \cdot ...... \cdot
   M_{1} \cdot M_{0} \cdot \delta \vec{u} \\
   ~ & = \, \nabla_v {\cal J} \cdot \delta \vec{v} \\
   \end{aligned}
   :label: forward

whereas in reverse mode we have

.. math::
   \boxed{
   \begin{aligned}
   M^T ( \nabla_v {\cal J}^T) & = \,
   M_{0}^T \cdot M_{1}^T
   \cdot ...... \cdot M_{\lambda}^T \cdot ...... \cdot 
   M_{\Lambda}^T \cdot \nabla_v {\cal J}^T \\
   ~ & = \, M_{0}^T \cdot M_{1}^T
   \cdot ...... \cdot 
   \nabla_{v^{(\lambda)}} {\cal J}^T \\
   ~ & = \, \nabla_u {\cal J}^T
   \end{aligned}}
   :label: reverse

clearly expressing the reverse nature of the calculation.
:eq:`reverse` is at the heart of automatic adjoint compilers. If the
intermediate steps :math:`\lambda` in :eq:`compos` – :eq:`reverse`
represent the model state (forward or adjoint) at each intermediate time
step as noted above, then correspondingly,
:math:`M^T (\delta \vec{v}^{(\lambda) \, \ast}) =
\delta \vec{v}^{(\lambda-1) \, \ast}` for the adjoint variables. It
thus becomes evident that the adjoint calculation also yields the
adjoint of each model state component :math:`\vec{v}^{(\lambda)}` at
each intermediate step :math:`\lambda`, namely

.. math::
   \boxed{
   \begin{aligned}
   \nabla_{v^{(\lambda)}} {\cal J}^T |_{\vec{v}^{(\lambda)}}
   & = \,
   M_{\lambda}^T |_{\vec{v}^{(\lambda)}} \cdot ...... \cdot 
   M_{\Lambda}^T |_{\vec{v}^{(\lambda)}} \cdot \delta \vec{v}^{\ast} \\
   ~ & = \, \delta \vec{v}^{(\lambda) \, \ast}
   \end{aligned}}

in close analogy to :eq:`adjoint` we note in passing that the
:math:`\delta \vec{v}^{(\lambda) \, \ast}` are the Lagrange multipliers
of the model equations which determine :math:`\vec{v}^{(\lambda)}`.

In components, :eq:`adjoint` reads as follows. Let

.. math::
   \begin{array}{rclcrcl}
   \delta \vec{u} & = &
   \left( \delta u_1,\ldots, \delta u_m \right)^T , & \qquad &
   \delta \vec{u}^{\ast} \,\, = \,\, \nabla_u {\cal J}^T & = &
   \left( 
   \frac{\partial {\cal J}}{\partial u_1},\ldots, 
   \frac{\partial {\cal J}}{\partial u_m}
   \right)^T \\
   \delta \vec{v} & = &
   \left( \delta v_1,\ldots, \delta u_n \right)^T , & \qquad &
   \delta \vec{v}^{\ast} \,\, = \,\, \nabla_v {\cal J}^T & = &
   \left( 
   \frac{\partial {\cal J}}{\partial v_1},\ldots, 
   \frac{\partial {\cal J}}{\partial v_n}
   \right)^T \\
   \end{array}

denote the perturbations in :math:`\vec{u}` and :math:`\vec{v}`,
respectively, and their adjoint variables; further

.. math::
   M \, = \, \left(
   \begin{array}{ccc}
   \frac{\partial {\cal M}_1}{\partial u_1} & \ldots &
   \frac{\partial {\cal M}_1}{\partial u_m} \\
   \vdots & ~ & \vdots \\
   \frac{\partial {\cal M}_n}{\partial u_1} & \ldots &
   \frac{\partial {\cal M}_n}{\partial u_m} \\
   \end{array}
   \right)

is the Jacobi matrix of :math:`{\cal M}` (an :math:`n \times m`
matrix) such that :math:`\delta \vec{v} = M \cdot \delta \vec{u}`, or

.. math::
   \delta v_{j} 
   \, = \, \sum_{i=1}^m M_{ji} \, \delta u_{i}
   \, = \, \sum_{i=1}^m \, \frac{\partial {\cal M}_{j}}{\partial u_{i}} 
   \delta u_{i}

Then :eq:`adjoint` takes the form

.. math::
   \delta u_{i}^{\ast} 
   \, = \, \sum_{j=1}^n M_{ji} \, \delta v_{j}^{\ast}
   \, = \, \sum_{j=1}^n \, \frac{\partial {\cal M}_{j}}{\partial u_{i}} 
   \delta v_{j}^{\ast}

or

.. math::
   \left(
   \begin{array}{c}
   \left. \frac{\partial}{\partial u_1} {\cal J} \right|_{\vec{u}^{(0)}} \\
   \vdots \\
   \left. \frac{\partial}{\partial u_m} {\cal J} \right|_{\vec{u}^{(0)}} \\
   \end{array}
   \right)
   \, = \,
   \left(
   \begin{array}{ccc}
   \left. \frac{\partial {\cal M}_1}{\partial u_1} \right|_{\vec{u}^{(0)}} 
   & \ldots &
   \left. \frac{\partial {\cal M}_n}{\partial u_1} \right|_{\vec{u}^{(0)}} \\
   \vdots & ~ & \vdots \\
   \left. \frac{\partial {\cal M}_1}{\partial u_m} \right|_{\vec{u}^{(0)}} 
   & \ldots &
   \left. \frac{\partial {\cal M}_n}{\partial u_m} \right|_{\vec{u}^{(0)}} \\
   \end{array}
   \right)
   \cdot
   \left(
   \begin{array}{c}
   \left. \frac{\partial}{\partial v_1} {\cal J} \right|_{\vec{v}} \\
   \vdots \\
   \left. \frac{\partial}{\partial v_n} {\cal J} \right|_{\vec{v}} \\
   \end{array}
   \right)

Furthermore, the adjoint :math:`\delta v^{(\lambda) \, \ast}` of any
intermediate state :math:`v^{(\lambda)}` may be obtained, using the
intermediate Jacobian (an :math:`n_{\lambda+1} \times n_{\lambda}`
matrix)

.. math::
   M_{\lambda} \, = \,
   \left(
   \begin{array}{ccc}
   \frac{\partial ({\cal M}_{\lambda})_1}{\partial v^{(\lambda)}_1}
   & \ldots &
   \frac{\partial ({\cal M}_{\lambda})_1}{\partial v^{(\lambda)}_{n_{\lambda}}} \\
   \vdots & ~ & \vdots \\
   \frac{\partial ({\cal M}_{\lambda})_{n_{\lambda+1}}}{\partial v^{(\lambda)}_1}
   & \ldots &
   \frac{\partial ({\cal M}_{\lambda})_{n_{\lambda+1}}}{\partial v^{(\lambda)}_{n_{\lambda}}} \\
   \end{array}
   \right)

and the shorthand notation for the adjoint variables
:math:`\delta v^{(\lambda) \, \ast}_{j} = \frac{\partial}{\partial v^{(\lambda)}_{j}}
{\cal J}^T`, :math:`j = 1, \ldots , n_{\lambda}`, for intermediate
components, yielding

.. math::
   \begin{aligned}
   \left(
   \begin{array}{c}
   \delta v^{(\lambda) \, \ast}_1 \\
   \vdots \\
   \delta v^{(\lambda) \, \ast}_{n_{\lambda}} \\
   \end{array}
   \right)
   \, = &
   \left(
   \begin{array}{ccc}
   \frac{\partial ({\cal M}_{\lambda})_1}{\partial v^{(\lambda)}_1}
   & \ldots \,\, \ldots &
   \frac{\partial ({\cal M}_{\lambda})_{n_{\lambda+1}}}{\partial v^{(\lambda)}_1} \\
   \vdots & ~ & \vdots \\
   \frac{\partial ({\cal M}_{\lambda})_1}{\partial v^{(\lambda)}_{n_{\lambda}}}
   & \ldots \,\, \ldots  &
   \frac{\partial ({\cal M}_{\lambda})_{n_{\lambda+1}}}{\partial v^{(\lambda)}_{n_{\lambda}}} \\
   \end{array}
   \right)
   \cdot
   %
   \\ ~ & ~
   \\ ~ &
   %
   \left(
   \begin{array}{ccc}
   \frac{\partial ({\cal M}_{\lambda+1})_1}{\partial v^{(\lambda+1)}_1}
   & \ldots &
   \frac{\partial ({\cal M}_{\lambda+1})_{n_{\lambda+2}}}{\partial v^{(\lambda+1)}_1} \\
   \vdots & ~ & \vdots \\
   \vdots & ~ & \vdots \\
   \frac{\partial ({\cal M}_{\lambda+1})_1}{\partial v^{(\lambda+1)}_{n_{\lambda+1}}}
   & \ldots  &
   \frac{\partial ({\cal M}_{\lambda+1})_{n_{\lambda+2}}}{\partial v^{(\lambda+1)}_{n_{\lambda+1}}} \\
   \end{array}
   \right)
   \cdot \, \ldots \, \cdot
   \left(
   \begin{array}{c}
   \delta v^{\ast}_1 \\
   \vdots \\
   \delta v^{\ast}_{n} \\
   \end{array}
   \right)
   \end{aligned}

:eq:`forward` and :eq:`reverse` are perhaps clearest in showing the
advantage of the reverse over the forward mode if the gradient
:math:`\nabla _{u}{\cal J}`, i.e., the sensitivity of the cost function
:math:`{\cal J}` with respect to *all* input variables :math:`u` (or
the sensitivity of the cost function with respect to *all* intermediate
states :math:`\vec{v}^{(\lambda)}`) are sought. In order to be able to
solve for each component of the gradient
:math:`\partial {\cal J} / \partial u_{i}` in :eq:`forward` a forward
calculation has to be performed for each component separately, i.e.,
:math:`\delta \vec{u} = \delta u_{i} {\vec{e}_{i}}` for the
:math:`i`-th forward calculation. Then, :eq:`forward` represents the
projection of :math:`\nabla_u {\cal J}` onto the :math:`i`-th
component. The full gradient is retrieved from the :math:`m` forward
calculations. In contrast, :eq:`reverse` yields the full gradient
:math:`\nabla _{u}{\cal J}` (and all intermediate gradients
:math:`\nabla _{v^{(\lambda)}}{\cal J}`) within a single reverse
calculation.

Note, that if :math:`{\cal J}` is a vector-valued function of
dimension :math:`l > 1`, :eq:`reverse` has to be modified according
to

.. math::
   M^T \left( \nabla_v {\cal J}^T \left(\delta \vec{J}\right) \right) 
   \, = \,
   \nabla_u {\cal J}^T \cdot \delta \vec{J}

where now :math:`\delta \vec{J} \in \mathbb{R}^l` is a vector of
dimension :math:`l`. In this case :math:`l` reverse simulations have
to be performed for each :math:`\delta J_{k}, \,\, k = 1, \ldots, l`.
Then, the reverse mode is more efficient as long as :math:`l < n`,
otherwise the forward mode is preferable. Strictly, the reverse mode is
called adjoint mode only for :math:`l = 1`.

A detailed analysis of the underlying numerical operations shows that
the computation of :math:`\nabla _{u}{\cal J}` in this way requires
about two to five times the computation of the cost function. Alternatively,
the gradient vector could be approximated by finite differences,
requiring :math:`m` computations of the perturbed cost function.

To conclude, we give two examples of commonly used types of cost
functions:

Example 1: :math:`{\cal J} = v_{j} (T)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The cost function consists of the :math:`j`-th component of the model
state :math:`\vec{v}` at time :math:`T`. Then
:math:`\nabla_v {\cal J}^T = {\vec{f}_{j}}` is just the :math:`j`-th
unit vector. The :math:`\nabla_u {\cal J}^T` is the projection of
the adjoint operator onto the :math:`j`-th component
:math:`{\bf f_{j}}`,

.. math::
     \nabla_u {\cal J}^T 
     \, = \, M^T \cdot \nabla_v {\cal J}^T
     \, = \,  \sum_{i} M^T_{ji} \, {\vec{e}_{i}}

Example 2: :math:`{\cal J} = \langle \, {\cal H}(\vec{v}) - \vec{d} \, , \, {\cal H}(\vec{v}) - \vec{d} \, \rangle`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The cost function represents the quadratic model vs. data misfit.
Here, :math:`\vec{d}` is the data vector and :math:`{\cal H}`
represents the operator which maps the model state space onto the data
space. Then, :math:`\nabla_v {\cal J}` takes the form

.. math::
     \begin{aligned}
     \nabla_v {\cal J}^T & = \, 2 \, \, H \cdot 
     \left( \, {\cal H}(\vec{v}) - \vec{d} \, \right) \\
     ~          & = \, 2 \sum_{j} \left\{ \sum_k
     \frac{\partial {\cal H}_k}{\partial v_{j}} 
     \left( {\cal H}_k (\vec{v}) - d_k \right)
     \right\} \, {\vec{f}_{j}} \\
     \end{aligned}

where :math:`H_{kj} = \partial {\cal H}_k / \partial v_{j}` is the
Jacobi matrix of the data projection operator. Thus, the gradient
:math:`\nabla_u {\cal J}` is given by the adjoint operator, driven
by the model vs. data misfit:

.. math::
    \nabla_u {\cal J}^T \, = \, 2 \, M^T \cdot 
     H \cdot \left( {\cal H}(\vec{v}) - \vec{d} \, \right)

.. _sec_autodiff_storage_v_recompute:

Storing vs. recomputation in reverse mode
-----------------------------------------

We note an important aspect of the forward vs. reverse mode calculation.
Because of the local character of the derivative (a derivative is
defined with respect to a point along the trajectory), the intermediate results
of the model trajectory
:math:`\vec{v}^{(\lambda+1)}={\cal M}_{\lambda}(v^{(\lambda)})` may be
required to evaluate the intermediate Jacobian
:math:`M_{\lambda}|_{\vec{v}^{(\lambda)}} \, \delta \vec{v}^{(\lambda)}`.
This is the case for example for nonlinear expressions (momentum advection,
nonlinear equation of state), and state-dependent conditional statements
(parameterization schemes). In the forward mode, the intermediate
results are required in the same order as computed by the full forward
model :math:`{\cal M}`, but in the reverse mode they are required in the
reverse order. Thus, in the reverse mode the trajectory of the forward
model integration :math:`{\cal M}` has to be stored to be available in
the reverse calculation. Alternatively, the complete model state up to
the point of evaluation has to be recomputed whenever its value is
required.

A method to balance the amount of recomputations vs. storage
requirements is called checkpointing (e.g., Griewank, 1992 :cite:`griewank:92`,
Restrepo et al., 1998 :cite:`restrepo:98`). It is depicted in :numref:`checkpointing` for
a 3-level checkpointing (as an example, we give explicit numbers for a
3-day integration with a 1-hourly timestep in square brackets).


 .. figure:: figs/checkpointing.png
    :width: 100%
    :align: center
    :alt: 3-lvl checkpointing schematic figure
    :name: checkpointing

    Schematic view of intermediate dump and restart for 3-level checkpointing.

-  In a first step, the model trajectory is subdivided into
   :math:`{n}^{lev3}` subsections [:math:`{n}^{lev3}`\ =3 1-day
   intervals], with the label :math:`lev3` for this outermost loop. The
   model is then integrated along the full trajectory, and the model
   state stored to disk only at every :math:`k_{i}^{lev3}`-th timestep
   [i.e. 3 times, at :math:`i = 0,1,2` corresponding to
   :math:`k_{i}^{lev3} = 0, 24, 48`]. In addition, the cost function
   is computed, if needed.

-  In a second step each subsection itself is divided into
   :math:`{n}^{lev2}` subsections [:math:`{n}^{lev2}`\ =4 6-hour
   intervals per subsection]. The model picks up at the last outermost
   dumped state :math:`v_{k_{n}^{lev3}}` and is integrated forward in
   time along the last subsection, with the label :math:`lev2` for this
   intermediate loop. The model state is now stored to disk at every
   :math:`k_{i}^{lev2}`-th timestep [i.e. 4 times, at
   :math:`i = 0,1,2,3` corresponding to
   :math:`k_{i}^{lev2} = 48, 54, 60, 66`].

-  Finally, the model picks up at the last intermediate dump state
   :math:`v_{k_{n}^{lev2}}` and is integrated forward in time along
   the last subsection, with the label :math:`lev1` for this
   intermediate loop. Within this sub-subsection only, parts of the
   model state are stored to memory at every timestep [i.e. every hour
   :math:`i=0,...,5` corresponding to
   :math:`k_{i}^{lev1} = 66, 67, \ldots, 71`]. The final state
   :math:`v_n = v_{k_{n}^{lev1}}` is reached and the model state of
   all preceding timesteps along the last innermost subsection are
   available, enabling integration backwards in time along the last
   subsection. The adjoint can thus be computed along this last
   subsection :math:`k_{n}^{lev2}`.

This procedure is repeated consecutively for each previous subsection
:math:`k_{n-1}^{lev2}, \ldots, k_{1}^{lev2}` carrying the adjoint
computation to the initial time of the subsection :math:`k_{n}^{lev3}`.
Then, the procedure is repeated for the previous subsection
:math:`k_{n-1}^{lev3}` carrying the adjoint computation to the initial
time :math:`k_{1}^{lev3}`.

For the full model trajectory of
:math:`n^{lev3} \cdot n^{lev2} \cdot n^{lev1}` timesteps the required
storing of the model state was significantly reduced to
:math:`n^{lev2} + n^{lev3}` to disk and roughly :math:`n^{lev1}` to
memory (i.e., for the 3-day integration with a total of 72 timesteps the
model state was stored 7 times to disk and roughly 6 times to memory).
This saving in memory comes at a cost of a required 3 full forward
integrations of the model (one for each checkpointing level). The
optimal balance of storage vs. recomputation certainly depends on the
computing resources available and may be adjusted by adjusting the
partitioning among the :math:`n^{lev3}, \,\, n^{lev2}, \,\, n^{lev1}`.

.. _sec_ad_tlm_and_adm:

TLM and ADM generation in general
=================================

In this section we describe in a general fashion the parts of the code
that are relevant for automatic differentiation using the software tool
TAF. Modifications to use OpenAD are described in :numref:`ad_openad`.

The basic flow is as follows: 

::

       the_model_main
       |
       |--- initialise_fixed
       |
       |--- #ifdef ALLOW_ADJOINT_RUN
       |           |    
       |           |--- ctrl_unpack
       |           |    
       |           |--- adthe_main_loop
       |           |    |
       |           |    |--- initialise_varia
       |           |    |--- ctrl_map_forcing
       |           |    |--- do iloop = 1, nTimeSteps
       |           |    |       |--- forward_step
       |           |    |       |--- cost_tile
       |           |    |    end do
       |           |    |--- cost_final
       |           |    |
       |           |    |--- adcost_final
       |           |    |--- do iloop = nTimeSteps, 1, -1
       |           |    |       |--- adcost_tile
       |           |    |       |--- adforward_step
       |           |    |    end do
       |           |    |--- adctrl_map_forcing
       |           |    |--- adinitialise_varia
       |           |    o
       |           |
       |           |--- ctrl_pack
       |           |
       |--- #else
       |           |
       |           |--- the_main_loop
       |           |
       |    #endif
       |
       |--- #ifdef ALLOW_GRADIENT_CHECK
       |           |
       |           |--- grdchk_main
       |           o
       |    #endif
       o


If CPP option
:varlink:`ALLOW_AUTODIFF_TAMC` is defined, the driver routine
:filelink:`the_model_main.F <model/src/the_model_main.F>`,
instead of calling :filelink:`the_model_loop.F <model/src/the_main_loop.F>`, invokes the
adjoint of this routine, ``adthe_main_loop.F`` (case
#define :varlink:`ALLOW_ADJOINT_RUN`, or the tangent linear of this routine
``g_the_main_loop.F`` (case #define :varlink:`ALLOW_TANGENTLINEAR_RUN`), which
are the toplevel routines in terms of automatic differentiation. The
routines ``adthe_main_loop.F`` or ``g_the_main_loop.F`` are generated by
TAF. It contains both the forward integration of the full model, the
cost function calculation, any additional storing that is required for
efficient checkpointing, and the reverse integration of the adjoint
model.

[DESCRIBE IN A SEPARATE SECTION THE WORKING OF THE TLM]

The above structure of ``adthe_main_loop.F`` has been
strongly simplified to focus on the essentials; in particular, no
checkpointing procedures are shown here. Prior to the call of
``adthe_main_loop.F``, the routine :filelink:`ctrl_unpack.F <pkg/ctrl/ctrl_unpack.F>`
is invoked to unpack the
control vector or initialize the control variables. Following the call
of ``adthe_main_loop.F``, the routine :filelink:`ctrl_pack.F <pkg/ctrl/ctrl_pack.F>`
is invoked to pack the
control vector (cf. :numref:`the_ctrl_vars`). If gradient checks are to
be performed, the option #define :varlink:`ALLOW_GRDCHK` is chosen. In this case
the driver routine :filelink:`grdchk_main.F <pkg/grdchk/grdchk_main.F>`
is called after the gradient has been
computed via the adjoint (cf. :numref:`ad_gradient_check`).

General setup
-------------

In order to configure AD-related setups the following packages need to
be enabled:

- :filelink:`pkg/autodiff`
- :filelink:`pkg/ctrl`
- :filelink:`pkg/cost`
- :filelink:`pkg/grdchk`

The packages are enabled by adding them to your experiment-specific
configuration file ``packages.conf`` (see Section ???).

The following AD-specific CPP option files need to be customized:

- :filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>`
  This header file collects CPP options for :filelink:`pkg/autodiff`,
  :filelink:`pkg/cost`, :filelink:`pkg/ctrl` as well as AD-unrelated options for the external
  forcing package :filelink:`pkg/exf`. (NOTE: These options are not set in their
  package-specific headers such as :filelink:`COST_OPTIONS.h <pkg/cost/COST_OPTIONS.h>`,
  but are instead collected in the single header file 
  :filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>`.
  The package-specific header files serve as simple placeholders at this point.) 

- :filelink:`tamc.h <pkg/autodiff/tamc.h>`
  This header configures the splitting of the time stepping loop
  with respect to the 3-level checkpointing (see section ???).

.. _building_adcode_using_taf:

Building the AD code using TAF
------------------------------

The build process of an AD code is very similar to building the forward
model. However, depending on which AD code one wishes to generate, and
on which AD tool is available (TAF or TAMC), the following make targets
are available:

+------------------+------------------------+----------------------------------------------------------------------------------+
| *AD-target*      | *output*               | *description*                                                                    |
+==================+========================+==================================================================================+
| «MODE»«TOOL»only | «MODE»_«TOOL»_output.f | generates code for «MODE» using «TOOL»                                           |
+------------------+------------------------+----------------------------------------------------------------------------------+
|                  |                        | no make dependencies on .F .h                                                    |
+------------------+------------------------+----------------------------------------------------------------------------------+
|                  |                        | useful for compiling on remote platforms                                         |
+------------------+------------------------+----------------------------------------------------------------------------------+
| «MODE»«TOOL»     | «MODE»_«TOOL»_output.f | generates code for «MODE» using «TOOL»                                           |
+------------------+------------------------+----------------------------------------------------------------------------------+
|                  |                        | includes make dependencies on .F .h                                              |
+------------------+------------------------+----------------------------------------------------------------------------------+
|                  |                        | i.e. input for «TOOL» may be re-generated                                        |
+------------------+------------------------+----------------------------------------------------------------------------------+
| «MODE»all        | mitgcmuv\_«MODE»       | generates code for «MODE» using «TOOL»                                           |
+------------------+------------------------+----------------------------------------------------------------------------------+
|                  |                        | and compiles all code                                                            |
+------------------+------------------------+----------------------------------------------------------------------------------+
|                  |                        | (use of TAF is set as default)                                                   |
+------------------+------------------------+----------------------------------------------------------------------------------+

Here, the following placeholders are used:

-  «TOOL»

   -  TAF

   -  TAMC

-  «MODE»

   -  ad generates the adjoint model (ADM)

   -  ftl generates the tangent linear model (TLM)

   -  svd generates both ADM and TLM for
      singular value decomposition (SVD) type calculations

For example, to generate the adjoint model using TAF after routines (``.F``)
or headers (``.h``) have been modified, but without compilation,
type ``make adtaf``; or, to generate the tangent linear model using TAMC without
re-generating the input code, type ``make ftltamconly``.

A typical full build process to generate the ADM via TAF would look like
follows:

::

    % mkdir build
    % cd build
    % ../../../tools/genmake2 -mods=../code_ad
    % make depend
    % make adall


The AD build process in detail
------------------------------

The ``make «MODE»all`` target consists of the following procedures:

#. A header file ``AD_CONFIG.h`` is generated which contains a CPP option
   on which code ought to be generated. Depending on the ``make`` target,
   the contents is one of the following:

   -  #define :varlink:`ALLOW_ADJOINT_RUN`

   -  #define :varlink:`ALLOW_TANGENTLINEAR_RUN`

   -  #define :varlink:`ALLOW_ECCO_OPTIMIZATION`

#. A single file ``«MODE»_input_code.f`` is concatenated consisting of all ``.f``
   files that are part of the list ``AD_FILES`` and all ``.flow`` files
   that are part of the list ``AD_FLOW_FILES``.

#. The AD tool is invoked with the ``«MODE»_«TOOL»_FLAGS``. The default AD tool
   flags in :filelink:`genmake2 <tools/genmake2>` can be overwritten by a :filelink:`tools/adjoint_options` file
   (similar to the platform-specific :filelink:`tools/build_options`, see :numref:`genmake2_optfiles`).
   The AD tool writes the resulting AD code into the file
   ``«MODE»_input_code_ad.f``.

#. A short sed script :filelink:`tools/adjoint_sed <tools/adjoint_sed>` is applied to ``«MODE»_input_code_ad.f`` to
   reinstate :varlink:`myThid` into the CALL argument list of active file I/O.
   The result is written to file ``«MODE»_«TOOL»_output.f``.

#. All routines are compiled and an executable is generated.

The list ``AD_FILES`` and ``.list`` files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Not all routines are presented to the AD tool. Routines typically hidden
are diagnostics routines which do not influence the cost function, but
may create artificial flow dependencies such as I/O of active variables.

:filelink:`genmake2 <tools/genmake2>` generates a list (or variable) ``AD_FILES`` which contains all
routines that are shown to the AD tool. This list is put together from
all files with suffix ``.list`` that :filelink:`genmake2 <tools/genmake2>` finds in its search
directories. The list file for the core MITgcm routines is :filelink:`model/src/model_ad_diff.list`
Note that no wrapper routine is shown to
TAF. These are either not visible at all to the AD code, or hand-written
AD code is available (see next section).

Each package directory contains its package-specific list file
``«PKG»_ad_diff.list``. For example, :filelink:`pkg/ptracers` contains the file
:filelink:`ptracers_ad_diff.list <pkg/ptracers_ad_diff.list>`.
Thus, enabling a package will automatically
extend the ``AD_FILES`` list of :filelink:`genmake2 <tools/genmake2>` to incorporate the
package-specific routines. Note that you will need to regenerate the
makefile if you enable a package (e.g., by adding it to ``packages.conf``)
and a ``Makefile`` already exists.

The list ``AD_FLOW_FILES`` and ``.flow`` files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TAMC and TAF can evaluate user-specified directives that start with a
specific syntax (``CADJ``, ``C$TAF``, ``!$TAF``). The main categories of directives
are ``STORE`` directives and ``FLOW`` directives. Here, we are concerned with
flow directives, store directives are treated elsewhere.

Flow directives enable the AD tool to evaluate how it should treat
routines that are ’hidden’ by the user, i.e. routines which are not
contained in the ``AD_FILES`` list (see previous section), but which
are called in part of the code that the AD tool does see. The flow
directive tell the AD tool:

-  which subroutine arguments are input/output

-  which subroutine arguments are active

-  which subroutine arguments are required to compute the cost

-  which subroutine arguments are dependent

The syntax for the flow directives can be found in the AD tool manuals.

:filelink:`genmake2 <tools/genmake2>` generates a list (or variable) ``AD_FLOW_FILES`` which
contains all files with ``suffix.flow`` that it finds in its search
directories. The flow directives for the core MITgcm routines of
:filelink:`eesupp/src/` and :filelink:`model/src/` reside in :filelink:`pkg/autodiff/`. This directory also
contains hand-written adjoint code for the MITgcm WRAPPER (:numref:`wrapper`).

Flow directives for package-specific routines are contained in the
corresponding package directories in the file ``«PKG»_ad.flow``, e.g.,
ptracers-specific directives are in :filelink:`ptracers_ad.flow <pkg/ptracers/ptracers_ad.flow>`.

Store directives for 3-level checkpointing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The storing that is required at each period of the 3-level checkpointing
is controlled by three top-level headers.

::

    do ilev_3 = 1, nchklev_3
    #  include ``checkpoint_lev3.h''
       do ilev_2 = 1, nchklev_2
    #     include ``checkpoint_lev2.h''
          do ilev_1 = 1, nchklev_1
    #        include ``checkpoint_lev1.h''

    ...

          end do
       end do
    end do

All files ``checkpoint_lev?.h`` are contained in directory :filelink:`pkg/autodiff/`.

.. _adoptfile:

Changing the default AD tool flags: ad_options files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Hand-written adjoint code
~~~~~~~~~~~~~~~~~~~~~~~~~

.. _pkg_cost_description:

The cost function (dependent variable)
--------------------------------------

The cost function :math:`{\cal J}` is referred to as the *dependent
variable*. It is a function of the input variables :math:`\vec{u}` via
the composition
:math:`{\cal J}(\vec{u}) \, = \, {\cal J}(M(\vec{u}))`. The input are
referred to as the *independent variables* or *control variables*. All
aspects relevant to the treatment of the cost function
:math:`{\cal J}` (parameter setting, initialization, accumulation,
final evaluation), are controlled by the package :filelink:`pkg/cost`. The aspects
relevant to the treatment of the independent variables are controlled by
the package :filelink:`pkg/ctrl` and will be treated in the next section.

::

          the_model_main
          |
          |-- initialise_fixed
          |   |
          |   |-- packages_readparms
          |       |
          |       |-- cost_readparms
          |       o
          |
          |-- the_main_loop
         ...  |
              |-- initialise_varia
              |   |
              |   |-- packages_init_variables
              |       |
              |       |-- cost_init
              |       o
              |
              |-- do iloop = 1,nTimeSteps
              |      |-- forward_step
              |      |-- cost_tile
              |      |   |
              |      |   |-- cost_tracer
              |   end do
              |
              |-- cost_final
              o

Enabling the package
~~~~~~~~~~~~~~~~~~~~

:filelink:`pkg/cost <pkg/cost>` is enabled by adding the line ``cost`` to your file ``packages.conf`` (see Section ???).

In general the following packages ought to be enabled
simultaneously: :filelink:`pkg/autodiff <pkg/autodiff>`, :filelink:`pkg/ctrl <pkg/ctrl>`,
and :filelink:`pkg/cost`. The basic CPP option to enable
the cost function is :varlink:`ALLOW_COST`. Each specific cost function
contribution has its own option. For the present example the option is
:varlink:`ALLOW_COST_TRACER`. All cost-specific options are set in
:filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>` Since the cost function is usually used in
conjunction with automatic differentiation, the CPP option
:varlink:`ALLOW_ADJOINT_RUN` (file :filelink:`CPP_OPTIONS.h <model/inc/CPP_OPTIONS.h>`) and
:varlink:`ALLOW_AUTODIFF_TAMC` (file :filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>`) should be defined.

Initialization
~~~~~~~~~~~~~~

The initialization of :filelink:`pkg/cost` is readily enabled as soon as
the CPP option :varlink:`ALLOW_COST` is defined.

-  The S/R :filelink:`cost_readparms.F </pkg/cost/cost_readparms.F>`
   reads runtime flags and parameters from file ``data.cost``.
   For the present example the only relevant parameter read is
   :varlink:`mult_tracer`. This multiplier enables different cost function
   contributions to be switched on (``= 1.``) or off (``= 0.``) at runtime.
   For more complex cost functions which involve model vs. data
   misfits, the corresponding data filenames and data specifications
   (start date and time, period, ...) are read in this S/R.

-  The S/R :filelink:`cost_init_varia.F </pkg/cost/cost_init_varia.F>`
   initializes the different cost function contributions. The
   contribution for the present example is :varlink:`objf_tracer` which is
   defined on each tile (bi,bj).

Accumulation
~~~~~~~~~~~~

The ’driver’ routine :filelink:`cost_tile.F </pkg/cost/cost_tile.F>`
is called at the end of each time
step. Within this ’driver’ routine, S/R are called for each of the
chosen cost function contributions. In the present example
(:varlink:`ALLOW_COST_TRACER`), S/R :filelink:`cost_tracer.F </pkg/cost/cost_tracer.F>` is called. It accumulates
:varlink:`objf_tracer` according to eqn. (ref:ask-the-author).

.. _sec_ad_finalize_contribtuions:

Finalize all contributions
~~~~~~~~~~~~~~~~~~~~~~~~~~

At the end of the forward integration S/R :filelink:`cost_final.F </pkg/cost/cost_final.F>` is called. It
accumulates the total cost function :varlink:`fc` from each contribution and
sums over all tiles:

.. math::
   {\cal J} \, = \, 
   {\rm fc} \, = \, 
   {\rm mult\_tracer} \sum_{\text{global sum}} \sum_{bi,\,bj}^{nSx,\,nSy}
   {\rm objf\_tracer}(bi,bj) \, + \, ...

The total cost function :varlink:`fc` will be the ’dependent’ variable in the
argument list for TAF, i.e.,

::

    taf -output 'fc' ...

::

       *************
       the_main_loop
       *************
       |
       |--- initialise_varia
       |    |
       |   ...
       |    |--- packages_init_varia
       |    |    |
       |    |   ...
       |    |    |--- #ifdef ALLOW_ADJOINT_RUN
       |    |    |          call ctrl_map_ini
       |    |    |          call cost_ini
       |    |    |    #endif
       |    |   ...
       |    |    o
       |   ...
       |    o
      ...
       |--- #ifdef ALLOW_ADJOINT_RUN
       |          call ctrl_map_forcing
       |    #endif
      ...
       |--- #ifdef ALLOW_TAMC_CHECKPOINTING
                  do ilev_3 = 1,nchklev_3
       |            do ilev_2 = 1,nchklev_2
       |              do ilev_1 = 1,nchklev_1
       |                iloop = (ilev_3-1)*nchklev_2*nchklev_1 +
       |                        (ilev_2-1)*nchklev_1           + ilev_1
       |    #else
       |          do iloop = 1, nTimeSteps
       |    #endif
       |    |
       |    |---       call forward_step
       |    |
       |    |--- #ifdef ALLOW_COST
       |    |          call cost_tile
       |    |    #endif
       |    |
       |    |    enddo
       |    o
       |
       |--- #ifdef ALLOW_COST
       |          call cost_final
       |    #endif
       o

.. _the_ctrl_vars:

The control variables (independent variables)
---------------------------------------------

The control variables are a subset of the model input (initial
conditions, boundary conditions, model parameters). Here we identify
them with the variable :math:`\vec{u}`. All intermediate variables
whose derivative with respect to control variables do not vanish are called
active variables. All subroutines whose derivative with respect to the control
variables don’t vanish are called active routines. Read and write
operations from and to file can be viewed as variable assignments.
Therefore, files to which active variables are written and from which
active variables are read are called active files. All aspects relevant
to the treatment of the control variables (parameter setting,
initialization, perturbation) are controlled by the package :filelink:`pkg/ctrl`.

::

          the_model_main
          |
          |-- initialise_fixed
          |   |
          |   |-- packages_readparms
          |       |
          |       |-- cost_readparms
          |       o
          |
          |-- the_main_loop
         ...  |
              |-- initialise_varia
              |   |
              |   |-- packages_init_variables
              |       |
              |       |-- cost_init
              |       o
              |
              |-- do iloop = 1,nTimeSteps
              |      |-- forward_step
              |      |-- cost_tile
              |      |   |
              |      |   |-- cost_tracer
              |   end do
              |
              |-- cost_final
              o


:filelink:`genmake2 <tools/genmake2>` and CPP options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Package :filelink:`pkg/ctrl` is enabled by adding the line ``ctrl`` to your file ``packages.conf``.
Each control variable is enabled via its own CPP option in
:filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>`.

Initialization
~~~~~~~~~~~~~~

- The S/R :filelink:`ctrl_readparms.F </pkg/ctrl/ctrl_readparms.F>`
  reads runtime flags and parameters from file ``data.ctrl``.
  For the present example the file contains the file names of each
  control variable that is used. In addition, the number of wet
  points for each control variable and the net dimension of the space
  of control variables (counting wet points only) :varlink:`nvarlength` is
  determined. Masks for wet points for each tile (bi,bj) and
  vertical layer k are generated for the three relevant
  categories on the C-grid: :varlink:`nWetCtile` for tracer fields,
  :varlink:`nWetWtile` for zonal velocity fields, :varlink:`nWetStile` for
  meridional velocity fields.

- Two important issues related to the handling of the control
  variables in MITgcm need to be addressed. First, in order to save
  memory, the control variable arrays are not kept in memory, but
  rather read from file and added to the initial fields during the
  model initialization phase. Similarly, the corresponding adjoint
  fields which represent the gradient of the cost function with respect to the
  control variables are written to file at the end of the adjoint
  integration. Second, in addition to the files holding the 2-D
  and 3-D control variables and the corresponding cost gradients,
  a 1-D control vector and gradient vector are written to file.
  They contain only the wet points of the control variables and the
  corresponding gradient. This leads to a significant data
  compression. Furthermore, an option is available
  (:varlink:`ALLOW_NONDIMENSIONAL_CONTROL_IO`) to non-dimensionalize the
  control and gradient vector, which otherwise would contain
  different pieces of different magnitudes and units. Finally, the
  control and gradient vector can be passed to a minimization routine
  if an update of the control variables is sought as part of a
  minimization exercise.

The files holding fields and vectors of the control variables and
gradient are generated and initialized in S/R :filelink:`ctrl_unpack.F </pkg/ctrl/ctrl_unpack.F>`.

Perturbation of the independent variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The dependency flow for differentiation with respect to the controls starts with
adding a perturbation onto the input variable, thus defining the
independent or control variables for TAF. Three types of controls may be
considered:

- Consider as an example the initial tracer distribution :varlink:`pTracer` as
  control variable. After :varlink:`pTracer` has been initialized in
  :filelink:`ptracers_init_varia.F <pkg/ptracers/ptracers_init_varia.F>`
  (dynamical variables such as temperature and salinity are
  initialized in :filelink:`ini_fields.F <>model/src/ini_fields.F>`), a perturbation anomaly is added to
  the field in S/R :filelink:`ctrl_map_ini.F </pkg/ctrl/ctrl_map_ini.F>`:

  .. math::
        \begin{aligned}
        u         & = \, u_{[0]} \, + \, \Delta u \\
        {\bf tr1}(...) & = \, {\bf tr1_{ini}}(...) \, + \, {\bf xx\_tr1}(...)
        \end{aligned}
        :label: perturb

  :varlink:`xx_tr1` is a 3-D global array holding the perturbation. In
  the case of a simple sensitivity study this array is identical to
  zero. However, it’s specification is essential in the context of
  automatic differentiation since TAF treats the corresponding line
  in the code symbolically when determining the differentiation chain
  and its origin. Thus, the variable names are part of the argument
  list when calling TAF:

  ::

       taf -input 'xx_tr1 ...' ...

  Now, as mentioned above, MITgcm avoids maintaining an array for each
  control variable by reading the perturbation to a temporary array
  from file. To ensure the symbolic link to be recognized by TAF, a
  scalar dummy variable ``xx_tr1_dummy`` is introduced and an ’active
  read’ routine of the adjoint support package :filelink:`pkg/autodiff` is
  invoked. The read-procedure is tagged with the variable
  ``xx_tr1_dummy`` enabling TAF to recognize the initialization of
  the perturbation. The modified call of TAF thus reads

  ::

       taf -input 'xx_tr1_dummy ...' ...

  and the modified operation (to perturb) in the code takes on the
  form

  ::

              call active_read_xyz( 
            &      ..., tmpfld3d, ..., xx_tr1_dummy, ... )

              tr1(...) = tr1(...) + tmpfld3d(...)

  Note that reading an active variable corresponds to a variable
  assignment. Its derivative corresponds to a write statement of the
  adjoint variable, followed by a reset. The ’active file’ routines
  have been designed to support active read and corresponding adjoint
  active write operations (and vice versa).

- The handling of boundary values as control variables proceeds
  exactly analogous to the initial values with the symbolic
  perturbation taking place in S/R
  :filelink:`ctrl_map_forcing.F </pkg/ctrl/ctrl_map_forcing.F>`.
  Note however
  an important difference: Since the boundary values are time
  dependent with a new forcing field applied at each time step, the
  general problem may be thought of as a new control variable at each
  time step (or, if the perturbation is averaged over a certain
  period, at each :math:`N` timesteps), i.e.,

  .. math::
        u_{\rm forcing} \, = \,
        \{ \, u_{\rm forcing} ( t_n ) \, \}_{
        n \, = \, 1, \ldots , {\rm nTimeSteps} }

  In the current example an equilibrium state is considered, and
  only an initial perturbation to surface forcing is applied with
  respect to the equilibrium state. A time dependent treatment of the
  surface forcing is implemented in the ECCO environment, involving
  the calendar (:filelink:`pkg/cal`) and external forcing (:filelink:`pkg/exf`) packages.

- This routine is not yet implemented, but would proceed proceed
  along the same lines as the initial value sensitivity. The mixing
  parameters :varlink:`diffkr` and :varlink:`kapgm` are currently added as controls
  in :filelink:`ctrl_map_ini.F </pkg/ctrl/ctrl_map_ini.F>`.

.. _sec_autodiff_output_adj_vars:  

Output of adjoint variables and gradient
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Several ways exist to generate output of adjoint fields.

-  In :filelink:`ctrl_map_ini.F </pkg/ctrl/ctrl_map_ini.F>`, :filelink:`ctrl_map_forcing.F </pkg/ctrl/ctrl_map_forcing.F>`:

   -  The control variable fields ``xx\_«...»``: before the forward integration, the control variables are read
      from file ``«xx\_ ...»`` and added to the model field.

   -  The adjoint variable fields ``adxx\_«...»``, i.e., the gradient
      :math:`\nabla _{u}{\cal J}` for each control variable:
      after the adjoint integration the corresponding adjoint
      variables are written to ``adxx\_«...»``.

-  In :filelink:`ctrl_unpack.F </pkg/ctrl/ctrl_unpack.F>`, :filelink:`ctrl_pack.F </pkg/ctrl/ctrl_pack.F>`: 

   -  The control vector ``vector_ctrl``:
      at the very beginning of the model initialization, the updated
      compressed control vector is read (or initialized) and
      distributed to 2-D and 3-D control variable fields.

   -  The gradient vector ``vector_grad``:
      at the very end of the adjoint integration, the 2-D and
      3-D adjoint variables are read, compressed to a single vector
      and written to file.

-  In addition to writing the gradient at the end of the
   forward/adjoint integration, many more adjoint variables of the
   model state at intermediate times can be written using S/R
   :filelink:`addummy_in_stepping.F </pkg/autodiff/addummy_in_stepping.F>`.
   The procedure is
   enabled using via the CPP-option :varlink:`ALLOW_AUTODIFF_MONITOR` (file
   :filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>`).
   To be part of the adjoint code, the
   corresponding S/R :filelink:`dummy_in_stepping.F <pkg/autodiff/dummy_in_stepping.F>`
   has to be called in the
   forward model (S/R :filelink:`the_main_loop.F <model/src/the_main_loop.F>`) at the appropriate place. The
   adjoint common blocks are extracted from the adjoint code via the
   header file :filelink:`adcommon.h </pkg/autodiff/adcommon.h>`.

   :filelink:`dummy_in_stepping.F <pkg/autodiff/dummy_in_stepping.F>` is essentially empty, the corresponding adjoint
   routine is hand-written rather than generated automatically.
   Appropriate flow directives
   (:filelink:`dummy_in_stepping.flow <pkg/autodiff/dummy_in_stepping.flow>`)
   ensure that
   TAMC does not automatically generate :filelink:`addummy_in_stepping.F <pkg/autodiff/addummy_in_stepping.F>` by
   trying to differentiate :filelink:`dummy_in_stepping.F <pkg/autodiff/dummy_in_stepping.F>`, but instead refers to
   the hand-written routine.

   :filelink:`dummy_in_stepping.F <pkg/autodiff/dummy_in_stepping.F>` is called in the forward code at the beginning
   of each timestep, before the call to :filelink:`model/src/dynamics.F`, thus ensuring that
   :filelink:`addummy_in_stepping.F <pkg/autodiff/addummy_in_stepping.F>` is called at the end of each timestep in the
   adjoint calculation, after the call to :filelink:`addummy_in_dynamics.F <pkg/autodiff/addummy_in_dynamics.F>`.

   :filelink:`addummy_in_stepping.F <pkg/autodiff/addummy_in_stepping.F>`
   includes the header files :filelink:`adcommon.h </pkg/autodiff/adcommon.h>`. This
   header file is also hand-written. It contains the common blocks
   :varlink:`addynvars_r`, :varlink:`addynvars_cd`, :varlink:`addynvars_diffkr`,
   :varlink:`addynvars_kapgm`, :varlink:`adtr1_r`, :varlink:`adffields`, which have
   been extracted from the adjoint code to enable access to the adjoint
   variables.

   **WARNING:** If the structure of the common blocks :varlink:`dynvars_r`,
   :varlink:`dynvars_cd`, etc., changes similar changes will occur in the
   adjoint common blocks. Therefore, consistency between the
   TAMC-generated common blocks and those in 
   :filelink:`adcommon.h </pkg/autodiff/adcommon.h>` have to be
   checked.

Control variable handling for optimization applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In optimization mode the cost function :math:`{\cal J}(u)` is sought
to be minimized with respect to a set of control variables
:math:`\delta {\cal J} \, = \, 0`, in an iterative manner. The
gradient :math:`\nabla _{u}{\cal J} |_{u_{[k]}}` together with the
value of the cost function itself :math:`{\cal J}(u_{[k]})` at
iteration step :math:`k` serve as input to a minimization routine
(e.g. quasi-Newton method, conjugate gradient, ... (Gilbert and Lemaréchal, 1989 
:cite:`gil-lem:89`) to compute an update in the control
variable for iteration step :math:`k+1`:

.. math::
   u_{[k+1]} \, = \,  u_{[0]} \, + \, \Delta u_{[k+1]}
   \quad \mbox{satisfying} \quad
    {\cal J} \left( u_{[k+1]} \right) \, < \, {\cal J} \left( u_{[k]} \right)

:math:`u_{[k+1]}` then serves as input for a forward/adjoint run to
determine :math:`{\cal J}` and :math:`\nabla _{u}{\cal J}` at
iteration step :math:`k+1`. :numref:`forward-adj_flow` sketches the flow
between forward/adjoint model and the minimization routine.

 .. figure:: figs/forward-adj_flow.*
    :width: 100%
    :align: center
    :alt: flow between forward/adjoint model and the minimization
    :name: forward-adj_flow

    Flow between the forward/adjoint model and the minimization routine.

The routines :filelink:`ctrl_unpack.F </pkg/ctrl/ctrl_unpack.F>` and 
:filelink:`ctrl_pack.F </pkg/ctrl/ctrl_pack.F>` provide the link between
the model and the minimization routine. As described in Section
ref:ask-the-author the :filelink:`ctrl_unpack.F </pkg/ctrl/ctrl_unpack.F>`
and :filelink:`ctrl_pack.F </pkg/ctrl/ctrl_pack.F>` routines read and write
control and gradient vectors which are compressed to contain only wet
points, in addition to the full 2-D and 3-D fields. The
corresponding I/O flow is shown in :numref:`forward-adj_io`:

 .. figure:: figs/forward-adj_io.*
    :width: 100%
    :align: center
    :alt: forward/adjoint model I/O
    :name: forward-adj_io

    Flow chart showing I/O in the forward/adjoint model.


:filelink:`ctrl_unpack.F </pkg/ctrl/ctrl_unpack.F>` reads the updated control vector
``vector_ctrl_<k>``. It distributes the
different control variables to 2-D and 3-D files
``xx_«...»<k>``. At the start of the forward
integration the control variables are read from
``xx_«...»<k>`` and added to the field.
Correspondingly, at the end of the adjoint integration the adjoint
fields are written to ``adxx_«...»<k>``, again via
the active file routines. Finally,
:filelink:`ctrl_pack.F </pkg/ctrl/ctrl_pack.F>` collects all adjoint
files and writes them to the compressed vector file
``vector_grad_<k>``.


NOTE: These options are not set in their package-specific headers
such as :filelink:`COST_OPTIONS.h <pkg/cost/COST_OPTIONS.h>`,
but are instead collected in the single
header file :filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>`. The package-specific header files
serve as simple placeholders at this point.

.. _ad_gradient_check:

The gradient check package
==========================

An indispensable test to validate the gradient computed via the adjoint
is a comparison against finite difference gradients. The gradient check
package :filelink:`pkg/grdchk` enables such tests in a straightforward and easy
manner. The driver routine :filelink:`grdchk_main.F <pkg/grdchk/grdchk_main.F>` is called from
:filelink:`the_model_main.F <model/src/the_model_main.F>` after
the gradient has been computed via the adjoint
model (cf. flow chart ???).

The gradient check proceeds as follows: The :math:`i-`\ th component of
the gradient :math:`(\nabla _{u}{\cal J}^T)_i` is compared with the
following finite-difference gradient:

.. math::
   \left(\nabla _{u}{\cal J}^T  \right)_i \quad \text{ vs. } \quad
   \frac{\partial {\cal J}}{\partial u_i} \, = \,
   \frac{ {\cal J}(u_i + \epsilon) - {\cal J}(u_i)}{\epsilon}

A gradient check at point :math:`u_i` may generally considered to be
successful if the deviation of the ratio between the adjoint and the
finite difference gradient from unity is less than 1 percent,

.. math::
   1 \, - \, 
   \frac{({\rm grad}{\cal J})_i (\text{adjoint})}
   {({\rm grad}{\cal J})_i (\text{finite difference})} \, < 1 \%

Code description
----------------


Code configuration
------------------

The relevant CPP precompile options are set in the following files:

- :filelink:`CPP_OPTIONS.h <model/inc/CPP_OPTIONS.h>`
  - Together with the flag :varlink:`ALLOW_ADJOINT_RUN`, define the flag :varlink:`ALLOW_GRADIENT_CHECK`.

The relevant runtime flags are set in the files:

- ``data.pkg``
  - Set :varlink:`useGrdchk` ``= .TRUE.``

-  ``data.grdchk``

   -  :varlink:`grdchk_eps`  

   -  :varlink:`nbeg`

   -  :varlink:`nstep`

   -  :varlink:`nend`

   -  :varlink:`grdchkvarindex`

::

       the_model_main
       |
       |-- ctrl_unpack
       |-- adthe_main_loop            - unperturbed cost function and
       |-- ctrl_pack                    adjoint gradient are computed here
       |
       |-- grdchk_main
           |
           |-- grdchk_init
           |-- do icomp=...           - loop over control vector elements
               |
               |-- grdchk_loc         - determine location of icomp on grid
               |
               |-- grdchk_getxx       - get control vector component from file
               |                        perturb it and write back to file
               |-- grdchk_getadxx     - get gradient component calculated 
               |                        via adjoint
               |-- the_main_loop      - forward run and cost evaluation
               |                        with perturbed control vector element
               |-- calculate ratio of adj. vs. finite difference gradient
               |
               |-- grdchk_setxx       - Reset control vector element
               |
               |-- grdchk_print       - print results

.. _sec_autodiff_diva:

Adjoint dump & restart – divided adjoint (DIVA)
===============================================

Authors: Patrick Heimbach & Geoffrey Gebbie, 07-Mar-2003*

***NOTE:THIS SECTION IS SUBJECT TO CHANGE. IT REFERS TO TAF-1.4.26.**

Previous TAF versions are incomplete and have problems with both TAF
options ``-pure`` and ``-mpi``.

The code which is tuned to the DIVA implementation of this TAF version
is ``checkpoint50`` (MITgcm) and ``ecco_c50_e28`` (ECCO).

Introduction
------------

Most high performance computing (HPC) centers require the use of batch
jobs for code execution. Limits in maximum available CPU time and memory
may prevent the adjoint code execution from fitting into any of the
available queues. This presents a serious limit for large scale / long
time adjoint ocean and climate model integrations. The MITgcm itself
enables the split of the total model integration into sub-intervals
through standard dump/restart of/from the full model state. For a
similar procedure to run in reverse mode, the adjoint model requires, in
addition to the model state, the adjoint model state, i.e., all variables
with derivative information which are needed in an adjoint restart. This
adjoint dump & restart is also termed ’divided adjoint (DIVA)’.

For this to work in conjunction with automatic differentiation, an AD
tool needs to perform the following tasks:

#. identify an adjoint state, i.e., those sensitivities whose
   accumulation is interrupted by a dump/restart and which influence the
   outcome of the gradient. Ideally, this state consists of

   -  the adjoint of the model state,

   -  the adjoint of other intermediate results (such as control
      variables, cost function contributions, etc.)

   -  bookkeeping indices (such as loop indices, etc.)

#. generate code for storing and reading adjoint state variables

#. generate code for bookkeeping , i.e., maintaining a file with index
   information

#. generate a suitable adjoint loop to propagate adjoint values for
   dump/restart with a minimum overhead of adjoint intermediate values.

TAF (but not TAMC!) generates adjoint code which performs the above
specified tasks. It is closely tied to the adjoint multi-level
checkpointing. The adjoint state is dumped (and restarted) at each step
of the outermost checkpointing level and adjoint integration is
performed over one outermost checkpointing interval. Prior to the
adjoint computations, a full forward sweep is performed to generate the
outermost (forward state) tapes and to calculate the cost function. In
the current implementation, the forward sweep is immediately followed by
the first adjoint leg. Thus, in theory, the following steps are
performed (automatically)

-  **1st model call:** This is the case if file ``costfinal`` does *not* exist. S/R
   ``mdthe_main_loop.f`` (generated by TAF) is called.

   #. calculate forward trajectory and dump model state after each
      outermost checkpointing interval to files ``tapelev3``

   #. calculate cost function ``fc`` and write it to file ``costfinal``

-  **2nd and all remaining model calls:**
   This is the case if file costfinal *does* exist. S/R
   ``adthe_main_loop.f`` (generated by TAF) is called.

   #. (forward run and cost function call is avoided since all values
      are known)

      -  if 1st adjoint leg:
         create index file ``divided.ctrl`` which contains info on current
         checkpointing index :math:`ilev3`

      -  if not :math:`i`-th adjoint leg:
         adjoint picks up at :math:`ilev3 = nlev3-i+1` and runs to
         :math:`nlev3 - i`

   #. perform adjoint leg from :math:`nlev3-i+1` to :math:`nlev3 - i`

   #. dump adjoint state to file ``snapshot``

   #. dump index file ``divided.ctrl`` for next adjoint leg

   #. in the last step the gradient is written.

A few modifications were performed in the forward code, obvious ones
such as adding the corresponding TAF-directive at the appropriate place,
and less obvious ones (avoid some re-initializations, when in an
intermediate adjoint integration interval).

[For TAF-1.4.20 a number of hand-modifications were necessary to
compensate for TAF bugs. Since we refer to TAF-1.4.26 onwards, these
modifications are not documented here].

.. _recipe1:

Recipe 1: single processor
--------------------------

#. In :filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>` set:

   - #define :varlink:`ALLOW_DIVIDED_ADJOINT`
   - #undef  :varlink:`ALLOW_DIVIDED_ADJOINT_MPI`

#. Generate adjoint code. Using the TAF option ``-pure``, two codes are
   generated:

   -  ``mdthe_main_loop.f``:
      Is responsible for the forward trajectory, storing of outermost
      checkpoint levels to file, computation of cost function, and
      storing of cost function to file (1st step).

   -  ``adthe_main_loop.f``:
      Is responsible for computing one adjoint leg, dump adjoint state
      to file and write index info to file (2nd and consecutive
      steps).

      for adjoint code generation, e.g., add ``-pure`` to TAF option list

      ::

              make adtaf

   -  One modification needs to be made to adjoint codes in S/R
      ``adecco_the_main_loop.f`` (generated by TAF):

      There’s a remaining issue with the ``-pure`` option. The ``call
      ad...`` between ``call ad...`` and the read of the ``snapshot`` file
      should be called only in the first adjoint leg between
      :math:`nlev3` and :math:`nlev3-1`. In the ecco-branch, the
      following lines should be bracketed by an ``if (idivbeg .GE.
      nchklev_3) then``, thus:

      ::


          ...
                xx_psbar_mean_dummy = onetape_xx_psbar_mean_dummy_3h(1)
                xx_tbar_mean_dummy = onetape_xx_tbar_mean_dummy_4h(1)
                xx_sbar_mean_dummy = onetape_xx_sbar_mean_dummy_5h(1)
                call barrier( mythid )
          cAdd(
                if (idivbeg .GE. nchklev_3) then
          cAdd)

                call adcost_final( mythid )
                call barrier( mythid )
                call adcost_sst( mythid )
                call adcost_ssh( mythid )
                call adcost_hyd( mythid )
                call adcost_averagesfields( mytime,myiter,mythid )
                call barrier( mythid )
          cAdd(
                endif
          cAdd)

          C----------------------------------------------
          C read snapshot
          C----------------------------------------------
                if (idivbeg .lt. nchklev_3) then
                  open(unit=77,file='snapshot',status='old',form='unformatted',
               $iostat=iers)
          ...

      For the main code, in all likelihood the block which needs to be
      bracketed consists of ``adcost_final.f`` (generated by TAF) only.

   -  Now the code can be copied as usual to ``adjoint_model.F`` and then
      be compiled:

      ::

              make adchange

      then compile

Recipe 2: multi processor (MPI)
-------------------------------

#. On the machine where you execute the code (most likely not the
   machine where you run TAF) find the includes directory for MPI
   containing ``mpif.h``. Either copy ``mpif.h`` to the machine where you
   generate the ``.f`` files before TAF-ing, or add the path to the includes
   directory to your :filelink:`genmake2 <tools/genmake2>` platform setup, TAF needs some MPI parameter
   settings (essentially ``mpi_comm_world`` and ``mpi_integer``) to
   incorporate those in the adjoint code.

#. In :filelink:`ECCO_CPPOPTIONS.h <pkg/autodiff/ECCO_CPPOPTIONS.h>` set

   - #define :varlink:`ALLOW_DIVIDED_ADJOINT`
   - #define  :varlink:`ALLOW_DIVIDED_ADJOINT_MPI`

   This will include the header file ``mpif.h`` into the top level routine
   for TAF.

#. Add the TAF option ``-mpi`` to the TAF argument list in the makefile.

#. Follow the same steps as in :ref:`Recipe 1 <recipe1>`.


.. _ad_openad:

Adjoint code generation using OpenAD
====================================

Authors: Jean Utke, Patrick Heimbach and Chris Hill

Introduction
------------

The development of OpenAD was initiated as part of the ACTS (Adjoint
Compiler Technology & Standards) project funded by the NSF Information
Technology Research (ITR) program. The main goals for OpenAD initially
defined for the ACTS project are:

#. develop a flexible, modular, open source tool that can generate
   adjoint codes of numerical simulation programs,

#. establish a platform for easy implementation and testing of source
   transformation algorithms via a language-independent abstract
   intermediate representation,

#. support for source code written in C and Fortan, and

#. generate efficient tangent linear and adjoint for the MIT general
   circulation model.

OpenAD’s homepage is at http://www-unix.mcs.anl.gov/OpenAD. A
development WIKI is at
http://wiki.mcs.anl.gov/OpenAD/index.php/Main_Page. From the WIKI’s
main page, click on `Handling GCM <https://wiki.mcs.anl.gov/OpenAD/index.php/Handling_GCM>`_
for various aspects pertaining to
differentiating the MITgcm with OpenAD.

Downloading and installing OpenAD
---------------------------------

The OpenAD webpage has a detailed description on how to download and
build OpenAD. From its homepage, please click on
`Binaries <http://www.mcs.anl.gov/OpenAD/binaries.shtml>`_. You may either download pre-built binaries
for quick trial, or follow the detailed build process described at
http://www.mcs.anl.gov/OpenAD/access.shtml.

Building MITgcm adjoint with OpenAD
-----------------------------------

**17-January-2008**

OpenAD was successfully built on head node of ``itrda.acesgrid.org``,
for following system:

::

    > uname -a
    Linux itrda 2.6.22.2-42.fc6 #1 SMP Wed Aug 15 12:34:26 EDT 2007 i686 i686 i386 GNU/Linux

    > cat /proc/version 
    Linux version 2.6.22.2-42.fc6 (brewbuilder@hs20-bc2-4.build.redhat.com) 
    (gcc version 4.1.2 20070626 (Red Hat 4.1.2-13)) #1 SMP Wed Aug 15 12:34:26 EDT 2007

    > module load ifc/9.1.036 icc/9.1.042

Head of MITgcm branch (``checkpoint59m`` with some modifications) was used for
building adjoint code. Following routing needed special care (revert
to revision 1.1): http://wwwcvs.mitgcm.org/viewvc/MITgcm/MITgcm_contrib/heimbach/OpenAD/OAD_support/active_module.f90?hideattic=0&view=markup.

Building the MITgcm adjoint using an OpenAD Singularity container
-----------------------------------------------------------------

The MITgcm adjoint can also be built using a Singularity container.  You will
need `Singularity <https://singularity.hpcng.org/>`_, version 3.X.  A container
with OpenAD can be downloaded from the Sylabs Cloud: [#thanks-Dan]_

::

   singularity pull library://jahn/default/openad:latest

To use it, supply the path to the downloaded container to genmake2,

::

   ../../../tools/genmake2 -oad -oadsingularity /path/to/openad_latest.sif ...
   make adAll

If your build directory is on a remotely mounted file system (mounted at
/mountpoint), you may have to add an option for mounting it in the container:

::

   ../../../tools/genmake2 -oad -oadsngl "-B /mountpoint /path/to/openad_latest.sif" ...

The ``-oadsingularity`` option is also supported by testreport,
:numref:`testreport_utility`.  Note that the path to the container has to be
either absolute or relative to the build directory.

.. rubric:: Footnotes

.. [#thanks-Dan] A big thank you to Dan Goldberg for supplying the definition
   file for the Singularity container!

