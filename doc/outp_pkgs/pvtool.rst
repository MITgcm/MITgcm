Potential Vorticity Matlab Toolbox
==================================

Author: Guillaume Maze

Introduction
------------

This section of the documentation describes a `MATLAB <https://www.mathworks.com/>`_  package that aims
to provide useful routines to compute vorticity fields (relative,
potential and planetary) and its related components. This is an offline
computation. It was developed to be used in mode water studies, so that
it comes with other related routines, in particular ones computing
surface vertical potential vorticity fluxes.

.. note::

    This toolbox was developed in 2006 for the `CLIMODE project <https://www.nsf.gov/awardsearch/showAward?AWD_ID=0425150>`_.
    The toolbox routines are available on this
    `archived repository <https://github.com/gmaze/gmaze_legacy/tree/master/matlab/MIT>`_.

Equations
---------

Potential vorticity
~~~~~~~~~~~~~~~~~~~

The package computes the three components of the relative vorticity,
:math:`\boldsymbol{\omega}`, defined by:

.. math::
   \begin{aligned}
     \boldsymbol{\omega} &=  \nabla  \times {\bf U}
     = \begin{pmatrix}
         \omega_x\\
         \omega_y\\
         \zeta
     \end{pmatrix}
     \simeq
     \begin{pmatrix}
         -\partial_z v\\
         -\partial_z u\\
         \partial_x v - \partial_y u
     \end{pmatrix}
   \end{aligned}
   :label: pv_eq1

where we omitted the vertical velocity component (as done throughout the package).

The package then computes the potential vorticity as:

.. math::
   \begin{aligned}
   Q &= -\frac{1}{\rho} \boldsymbol{\omega} \cdot  \nabla \sigma_\theta\\
     &= -\frac{1}{\rho}\left(\omega_x \frac{\partial \sigma_\theta}{\partial x} +
   \omega_y \frac{\partial \sigma_\theta}{\partial y} +
   \left(f + \zeta\right) \frac{\partial \sigma_\theta}{\partial z}\right)
   \end{aligned}
   :label: pv_eq2

where :math:`\rho` is the density, :math:`\sigma_\theta` is the
potential density (both eventually computed by the package) and
:math:`f` is the Coriolis parameter.

The package is also able to compute the simpler planetary vorticity as:

.. math::
   \begin{aligned}
   Q_{\rm spl} & = -\frac{f}{\rho}\frac{\sigma_\theta}{\partial z}
   \end{aligned}
   :label: pv_eq3

Surface vertical potential vorticity fluxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These quantities are useful in mode water studies because of the
impermeability theorem which states that for a given potential density
layer (embedding a mode water), the integrated PV only changes through
surface input/output.

Vertical PV fluxes due to diabatic and frictional processes are given by:

.. math::
   J^B_z = -\frac{f}{h}\left( \frac{\alpha Q_{\rm net}}{\text{C}_p}-\rho_0 \beta S_{\rm net}\right)
   :label: pv_eq14a

.. math::
   J^F_z = \frac{1}{\rho\delta_e} (\hat{\boldsymbol{k}} \times \boldsymbol{\tau}) \cdot  \nabla \sigma_m
  :label: pv_eq15a

These components can be computed with the package. Details on the
variables definition and the way these fluxes are derived can be found
in :numref:`notes_flux_form`.

We now give some simple explanations about these fluxes and how they can
reduce the PV value of an oceanic potential density layer.

Diabatic process
^^^^^^^^^^^^^^^^

Let’s take the PV flux due to surface buoyancy forcing from
:eq:`pv_eq14a` and simplify it as:

.. math::

   \begin{aligned}
     J^B_z \simeq -\frac{\alpha f}{h \text{C}_p} Q_{\rm net}\end{aligned}

When the net surface heat flux :math:`Q_{\rm net}` is upward, i.e., negative
and cooling the ocean (buoyancy loss), surface density will increase,
triggering mixing which reduces the stratification and then the PV.

.. math::
   \begin{aligned}
     Q_{\rm net} &< 0 \phantom{WWW}\text{(upward, cooling)} \\
     J^B_z   &> 0 \phantom{WWW}\text{(upward)} \\
     -\rho^{-1} \nabla  \cdot J^B_z &< 0 \phantom{WWW}\text{(PV flux divergence)} \\
     PV &\searrow \phantom{WWWi}\text{where } Q_{\rm net}<0 \end{aligned}


Frictional process: “Down-front” wind-stress
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now let’s take the PV flux due to the “wind-driven buoyancy flux” from
:eq:`pv_eq15a` and simplify it as:

.. math::
   \begin{aligned}
     J^F_z &= \frac{1}{\rho\delta_e} \left( \tau_x\frac{\partial \sigma}{\partial y} - \tau_y\frac{\partial \sigma}{\partial x} \right) \\
     &\simeq \frac{1}{\rho\delta_e} \tau_x\frac{\partial \sigma}{\partial y} \end{aligned}

When the wind is blowing from the east above the Gulf Stream (a region
of high meridional density gradient), it induces an advection of dense
water from the northern side of the Gulf Stream to the southern side through
Ekman currents. Then, it induces a “wind-driven” buoyancy lost and
mixing which reduces the stratification and the PV.

.. math::
   \begin{aligned}
    (\hat{\boldsymbol{k}} \times \boldsymbol{\tau}) \cdot  \nabla  \sigma &> 0 \phantom{WWW}\text{("Down-front" wind)} \\
    J^F_z &> 0 \phantom{WWW}\text{(upward)} \\
     -\rho^{-1}  \nabla  \cdot J^F_z &< 0 \phantom{WWW}\text{(PV flux divergence)} \\
     PV &\searrow \phantom{WWW}\text{where } (\hat{\boldsymbol{k}} \times \boldsymbol{\tau}) \cdot  \nabla \sigma>0
   \end{aligned}


Diabatic versus frictional processes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A recent debate in the community arose about the relative role of these
processes. Taking the ratio of :eq:`pv_eq14a` and
:eq:`pv_eq15a` leads to:

.. math::

   \begin{aligned}
     \frac{J^F_z}{J^B_z} & = \frac{ \dfrac{1}{\rho\delta_e} (\hat{\boldsymbol{k}} \times \boldsymbol{\tau}) \cdot  \nabla \sigma }
     {-\dfrac{f}{h}\left( \dfrac{\alpha Q_{\rm net}}{\text{C}_p} - \rho_0 \beta S_{\rm net}\right)} \\
     &\simeq \frac{Q_{\rm Ek}/\delta_e}{Q_{\rm net}/h} \nonumber
   \end{aligned}

where appears the lateral heat flux induced by Ekman currents:

.. math::

   \begin{aligned}
     Q_{\rm Ek} & = -\frac{\text{C}_p}{\alpha\rho f} (\hat{\boldsymbol{k}} \times \boldsymbol{\tau}) \cdot  \nabla \sigma
     \nonumber \\
     & = \frac{\text{C}_p}{\alpha}\delta_e \vec{\bf u}_{\rm Ek} \cdot  \nabla \sigma\end{aligned}

which can be computed with the package. In the aim of comparing both
processes, it will be useful to plot surface net and lateral
Ekman-induced heat fluxes together with PV fluxes.

Key routines
------------

-  **A_compute_potential_density.m**: Compute the potential density
   field. Requires the potential temperature and salinity (either total
   or anomalous) and produces one output file with the potential density
   field (file prefix is ``SIGMATHETA``). The routine uses :filelink:`utils/matlab/densjmd95.m`,
   a Matlab counterpart of the MITgcm built-in function to compute the
   density.

-  **B_compute_relative_vorticity.m**: Compute the three components
   of the relative vorticity defined in :eq:`pv_eq1`.
   Requires the two horizontal velocity components and produces three
   output files with the three components (files prefix are ``OMEGAX``,
   ``OMEGAY`` and ``ZETA``).

-  **C_compute_potential_vorticity.m**: Compute the potential
   vorticity without the negative ratio by the density. Two options are
   possible in order to compute either the full component (term into
   parenthesis in :eq:`pv_eq2` or the planetary component
   (:math:`f\partial_z\sigma_\theta` in :eq:`pv_eq3`). Requires
   the relative vorticity components and the potential density, and
   produces one output file with the potential vorticity (file prefix is
   ``PV`` for the full term and ``splPV`` for the planetary component).

-  **D_compute_potential_vorticity.m**: Load the field computed with
   and divide it by :math:`-\rho` to obtain the correct potential
   vorticity. Require the density field and after loading, overwrite the
   file with prefix ``PV`` or ``splPV``.

-  **compute_density.m**: Compute the density :math:`\rho` from the
   potential temperature and the salinity fields.

-  **compute_JFz.m**: Compute the surface vertical PV flux due to
   frictional processes. Requires the wind stress components, density,
   potential density and Ekman layer depth (all of them, except the wind
   stress, may be computed with the package), and produces one output
   file with the PV flux :math:`J^F_z` (see :eq:`pv_eq15a` and
   with ``JFz`` as a prefix.

-  **compute_JBz.m**: Compute the surface vertical PV flux due to
   diabatic processes as:

   .. math::
      \begin{aligned}
        J^B_z & = -\frac{f}{h}\frac{\alpha Q_{\rm net}}{\text{C}_p} \end{aligned}

   which is a simplified version of the full expression given in
   :eq:`pv_eq14a`. Requires the net surface heat flux and the
   mixed layer depth (of which an estimation can be computed with the
   package), and produces one output file with the PV flux :math:`J^B_z`
   and with JBz as a prefix.

-  **compute\_QEk.m**: Compute the horizontal heat flux due to Ekman
   currents from the PV flux induced by frictional forces as:

   .. math::
      \begin{aligned}
       Q_{\rm Ek} & = - \frac{\text{C}_p \delta_e}{\alpha f}J^F_z\end{aligned}

   Requires the PV flux due to frictional forces and the Ekman layer
   depth, and produces one output with the heat flux and with QEk as a
   prefix.

-  **eg\_main\_getPV**: A complete example of how to set up a master
   routine able to compute everything from the package.

Technical details
-----------------

File name
~~~~~~~~~

A file name is formed by three parameters which need to be set up as
global variables in `MATLAB <https://www.mathworks.com/>`_ before running any routines. They are:

-  the prefix, i.e., the variable name (``netcdf_UVEL`` for example). This
   parameter is specified in the help section of all diagnostic
   routines.

-  ``netcdf_domain``: the geographical domain.

-  ``netcdf_suff``: the netcdf extension (nc or cdf for example).

Then, for example, if the calling `MATLAB <https://www.mathworks.com/>`_ routine had set up:

::

    global netcdf_THETA netcdf_SALTanom netcdf_domain netcdf_suff
    netcdf_THETA    = 'THETA';
    netcdf_SALTanom = 'SALT';
    netcdf_domain   = 'north_atlantic';
    netcdf_suff     = 'nc';

the routine A_compute_potential_density.m to compute the potential
density field, will look for the files:

::

    THETA.north_atlantic.nc
    SALT.north_atlantic.nc

and the output file will automatically be:
``SIGMATHETA.north_atlantic.nc``.

Otherwise indicated, output file prefix cannot be changed.

Path to file
~~~~~~~~~~~~

All diagnostic routines look for input files in a subdirectory (relative
to the `MATLAB <https://www.mathworks.com/>`_ routine directory)
called ``./netcdf-files``, which in turn is
supposed to contain subdirectories for each set of fields. For example,
computing the potential density for the timestep 12H00 02/03/2005 will
require a subdirectory with the potential temperature and salinity files
like:

::

    ./netcdf-files/200501031200/THETA.north_atlantic.nc
    ./netcdf-files/200501031200/SALT.north_atlantic.nc

The output file ``SIGMATHETA.north\_atlantic.nc`` will be created in
``./netcdf-files/200501031200/``. All diagnostic routines take as argument
the name of the timestep subdirectory into ``./netcdf-files``.

Grids
~~~~~

With MITgcm numerical outputs, velocity and tracer fields may not be
defined on the same grid. Usually, ``UVEL`` and ``VVEL`` are defined on a C-grid
but when interpolated from a cube-sphere simulation they are defined on
a A-grid. When it is needed, routines allow to set up a global variable
which define the grid to use.

.. _notes_flux_form:

Notes on the flux form of the PV equation and vertical PV fluxes
----------------------------------------------------------------

Flux form of the PV equation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The conservative flux form of the potential vorticity equation is:

.. math::
   \begin{aligned}
   \frac{\partial \rho Q}{\partial t} +  \nabla  \cdot \vec{\bf J} & = 0
   \end{aligned}
   :label: pv_eq4

where the potential vorticity :math:`Q` is given by :eq:`pv_eq2`.

The generalized flux vector of potential vorticity is:

.. math::
   \begin{aligned}
    \vec{\bf J} & = \rho Q \vec{\bf u} + \vec{\bf N}_Q
   \end{aligned}

which allows to rewrite :eq:`pv_eq4` as:

.. math::
   \begin{aligned}
   \frac{DQ}{dt} & = - \frac{1}{\rho} \nabla  \cdot \vec{\bf N}_Q
   \end{aligned}
   :label: pv_eq5

where the non-advective PV flux :math:`\vec{\bf N}_Q` is given by:

.. math::
   \begin{aligned}
   \vec{\bf N}_Q & = -\frac{\rho_0}{g}B \vec{\boldsymbol{\omega}}_a + \vec{\bf F} \times  \nabla  \sigma_\theta
   \end{aligned}
   :label: pv_eq6

Its first component is linked to the buoyancy forcing:

.. math::
   \begin{aligned}
    B & = -\frac{g}{\rho_o}\frac{D \sigma_\theta}{dt}
   \end{aligned}

and the second one to the non-conservative body forces per unit mass:

.. math::
   \begin{aligned}
    \vec{\bf F} & = \frac{D \vec{\bf u}}{Dt} + 2 \vec{\boldsymbol{\Omega}} \times \vec{\bf u} +  \nabla  p
   \end{aligned}

Note that introducing :math:`B` into :eq:`pv_eq6` yields:

   .. math::
      \begin{aligned}
        \vec{\bf N}_Q & = \boldsymbol{\omega}_a \frac{D \sigma_\theta}{dt} + \vec{\bf F} \times  \nabla  \sigma_\theta
      \end{aligned}


Determining the PV flux at the ocean’s surface
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the context of mode water study, we are particularly interested in how
the PV may be reduced by surface PV fluxes because a mode water is
characterized by a low PV value. Considering the volume limited by two
:math:`iso-\sigma_\theta`, PV flux is limited to surface processes and
then vertical component of :math:`\vec{\bf N}_Q`. It is supposed that
:math:`B` and :math:`\vec{\bf F}` will only be non-zero in the mixed layer
(of depth :math:`h` and variable density :math:`\sigma_m`) exposed to
mechanical forcing by the wind and buoyancy fluxes through the ocean’s
surface.

Given the assumption of a mechanical forcing confined to a thin surface
Ekman layer (of depth :math:`\delta_e`, eventually computed by the
package) and of hydrostatic and geostrophic balances, we can write:

.. math::
   \begin{aligned}
     \vec{\bf u}_g & = \frac{1}{\rho f} \hat{\boldsymbol{k}} \times  \nabla  p \\
     \frac{\partial p_m}{\partial z} & = -\sigma_m g \\
     \frac{\partial \sigma_m}{\partial t} + \vec{\bf u}_m \cdot  \nabla  \sigma_m & = -\frac{\rho_0}{g}B
   \end{aligned}
   :label: pv_eq7

where:

.. math::
   \begin{aligned}
     \vec{\bf u}_m & = \vec{\bf u}_g + \vec{\bf u}_{\rm Ek} + o(R_o)
   \end{aligned}
   :label: pv_eq8

is the full velocity field composed of the geostrophic current
:math:`\vec{\bf u}_g` and the Ekman drift:

.. math::
  \begin{aligned}
     \vec{\bf u}_{\rm Ek} & = -\frac{1}{\rho f} \hat{\boldsymbol{k}} \times \frac{\partial \boldsymbol{\tau}}{\partial z}
   \end{aligned}
  :label: pv_eq9

(where :math:`\boldsymbol{\tau}` is the wind stress) and last by other ageostrophic
components of :math:`o(R_o)` which are neglected.

Partitioning the buoyancy forcing as:

.. math::
   \begin{aligned}
     B & = B_g + B_{\rm Ek}
   \end{aligned}
   :label: pv_eq10

and using :eq:`pv_eq8` and :eq:`pv_eq9`, :eq:`pv_eq7` becomes:

.. math::
   \begin{aligned}
    \frac{\partial \sigma_m}{\partial t} + \vec{\bf u}_g \cdot  \nabla  \sigma_m & = -\frac{\rho_0}{g} B_g
   \end{aligned}

revealing the “wind-driven buoyancy forcing”:

.. math::
   \begin{aligned}
     B_{\rm Ek} & = \frac{g}{\rho_0}\frac{1}{\rho f}\left(\hat{\boldsymbol{k}} \times \frac{\partial \boldsymbol{\tau}}{\partial z}\right)\cdot  \nabla \sigma_m
   \end{aligned}

Note that since:

.. math::
   \begin{aligned}
     \frac{\partial B_g}{\partial z} & = \frac{\partial}{\partial z}\left(-\frac{g}{\rho_0} \vec{\bf u}_g \cdot  \nabla \sigma_m\right)
     = -\frac{g}{\rho_0}\frac{\partial \vec{\bf u}_g}{\partial z} \cdot  \nabla  \sigma_m
     = 0
   \end{aligned}

:math:`B_g` must be uniform throughout the depth of the mixed layer and
then being related to the surface buoyancy flux by integrating
:eq:`pv_eq10` through the mixed layer:

.. math::
   \begin{aligned}
     \int_{-h}^0 B\,dz &= h B_g + \int_{-h}^0 B_{\rm Ek}\,dz = \mathcal{B}_{\rm in}
   \end{aligned}
   :label: pv_eq11

where :math:`\mathcal{B}_{\rm in}` is the vertically integrated surface buoyancy (in)flux:

.. math::
   \begin{aligned}
     \mathcal{B}_{\rm in} & = \frac{g}{\rho_o}\left( \frac{\alpha Q_{\rm net}}{\text{C}_p} - \rho_0\beta S_{\rm net}\right)
   \end{aligned}
   :label: pv_eq12

with :math:`\alpha\simeq 2.5\times10^{-4}\, \text{K}^{-1}` the thermal
expansion coefficient (computed by the package otherwise),
:math:`\text{C}_p=4187 \text{ J kg}^{-1}\text{K}^{-1}` the specific heat of
seawater, :math:`Q_{\rm net}\text{ (W m$^{-2}$)}` the net heat surface
flux (positive downward, warming the ocean), :math:`\beta\text{
((g/kg)$^{-1}$)}` the saline contraction coefficient, and
:math:`S_{\rm net}=S*(E-P)\text{ ((g/kg) m s$^{-1}$)}` the net freshwater
surface flux with :math:`S\text{ (g/kg)}` the surface salinity and
:math:`(E-P)\text{ (m s$^{-1}$)}` the fresh water flux.

Introducing the body force in the Ekman layer:

.. math::
   \begin{aligned}
     F_z & = \frac{1}{\rho}\frac{\partial \boldsymbol{\tau}}{\partial z}
   \end{aligned}

the vertical component of :eq:`pv_eq6` is:

.. math::
   \begin{aligned}
     \vec{\bf N}_Q \cdot \hat{\boldsymbol{k}} &= -\frac{\rho_0}{g}(B_g+B_{\rm Ek}) \omega_z
     + \frac{1}{\rho}
     \left( \frac{\partial \boldsymbol{\tau}}{\partial z} \times  \nabla  \sigma_\theta \right) \cdot \hat{\boldsymbol{k}} \\
     &= -\frac{\rho_0}{g}B_g\omega_z
     -\frac{\rho_0}{g}
     \left[ \frac{g}{\rho_0}\frac{1}{\rho f} \left( \hat{\boldsymbol{k}} \times \frac{\partial \boldsymbol{\tau}}{\partial z} \right)
       \cdot  \nabla \sigma_m \right]\omega_z
     + \frac{1}{\rho}
     \left( \frac{\partial \boldsymbol{\tau}}{\partial z}\times \nabla  \sigma_\theta \right)\cdot\hat{\boldsymbol{k}}\\
     &= -\frac{\rho_0}{g}B_g\omega_z
     + \left(1-\frac{\omega_z}{f}\right)\left(\frac{1}{\rho}\frac{\partial \boldsymbol{\tau}}{\partial z}
                   \times \nabla \sigma_\theta \right)\cdot\hat{\boldsymbol{k}}\end{aligned}

and given the assumption that :math:`\omega_z\simeq f`, the second term
vanishes and we obtain:

.. math::
   \begin{aligned}
     \vec{\bf N}_Q \cdot \hat{\boldsymbol{k}} & = -\frac{\rho_0}{g}f B_g .
   \end{aligned}
   :label: pv_eq13

Note that the wind-stress forcing does not appear explicitly here but
is implicit in :math:`B_g` through :eq:`pv_eq11`: the buoyancy
forcing :math:`B_g` is determined by the difference between the
integrated surface buoyancy flux :math:`\mathcal{B}_{\rm in}` and the
integrated “wind-driven buoyancy forcing”:

.. math::

   \begin{aligned}
     B_g &= \frac{1}{h}\left( \mathcal{B}_{\rm in} - \int_{-h}^0B_{\rm Ek}dz \right)  \\
     &= \frac{1}{h}\frac{g}{\rho_0}\left( \frac{\alpha Q_{\rm net}}{\text{C}_p} - \rho_0 \beta S_{\rm net}\right)
     - \frac{1}{h}\int_{-h}^0
     \frac{g}{\rho_0}\frac{1}{\rho f}\left (\hat{\boldsymbol{k}}\times \frac{\partial \boldsymbol{\tau}}{\partial z} \right) \cdot  \nabla \sigma_m dz \\
     &= \frac{1}{h}\frac{g}{\rho_0}\left( \frac{\alpha Q_{\rm net}}{\text{C}_p} - \rho_0 \beta S_{\rm net}\right)
     - \frac{g}{\rho_0}\frac{1}{\rho f \delta_e}\left (\hat{\boldsymbol{k}}\times \boldsymbol{\tau} \right) \cdot  \nabla \sigma_m\end{aligned}

Finally, from :eq:`pv_eq6`, the vertical surface flux of PV may
be written as:

.. math::
   \begin{aligned}
     \vec{\bf N}_Q \cdot \hat{\boldsymbol{k}} &= J^B_z + J^F_z  \\
     J^B_z &= -\frac{f}{h}\left( \frac{\alpha Q_{\rm net}}{\text{C}_p}-\rho_0 \beta S_{\rm net}\right) \\
     J^F_z &= \frac{1}{\rho\delta_e} (\hat{\boldsymbol{k}}\times \boldsymbol{\tau}) \cdot  \nabla \sigma_m \end{aligned}

