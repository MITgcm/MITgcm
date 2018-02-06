
Equations Solved
----------------

The model is configured in hydrostatic form. The implicit free surface form of the
pressure equation described in :cite:`marshall:97a` is
employed.
A horizontal Laplacian operator :math:`\nabla_{h}^2` provides viscous
dissipation. The wind-stress momentum input is added to the momentum equation
for the 'zonal flow', :math:`u`. Other terms in the model
are explicitly switched off for this experiment configuration (see section
:numref:`sec_eg_baro_code_config` ), yielding an active set of equations solved 
in this configuration as follows 

.. math::
    :label: eq_eg_baro_model_equations

    \frac{Du}{Dt} - fv + g\frac{\partial \eta}{\partial x} - A_{h}\nabla_{h}^2u
     = & \frac{\tau_{x}}{\rho_{0}\Delta z}

    \frac{Dv}{Dt} + fu + g\frac{\partial \eta}{\partial y} - A_{h}\nabla_{h}^2v
     = & 0

    \frac{\partial \eta}{\partial t} + \nabla_{h}\cdot \vec{u}
     = & 0


where :math:`u` and :math:`v` and the :math:`x` and :math:`y` components of the
flow vector :math:`\vec{u}`.


Discrete Numerical Configuration
--------------------------------

The domain is discretised with a uniform grid spacing in the horizontal set to :math:`\Delta x=\Delta y=20` km, so that there are sixty grid cells in the :math:`x` and :math:`y` directions. Vertically the model is configured with a single layer with depth, :math:`\Delta z`, of :math:`5000` m. 


Numerical Stability Criteria
++++++++++++++++++++++++++++

The Laplacian dissipation coefficient, :math:`A_{h}`, is set to :math:`400 m s^{-1}`. This value is chosen to yield a Munk layer width :cite:`adcroft:95`,

.. math::
    :label: eq_eg_baro_munk_layer
    
    M_{w} = \pi ( \frac { A_{h} }{ \beta } )^{\frac{1}{3}}


of :math:`\approx` 100km. This is greater than the model
resolution :math:`\Delta x`, ensuring that the frictional boundary
layer is well resolved.

The model is stepped forward with a 
time step :math:`\delta t=1200` secs. With this time step the stability 
parameter to the horizontal Laplacian friction :cite:`adcroft:95`

.. math::
    :label: eq_eg_baro_laplacian_stability

    S_{l} = 4 \frac{A_{h} \delta t}{{\Delta x}^2}


evaluates to 0.012, which is well below the 0.3 upper limit for stability.

The numerical stability for inertial oscillations :cite:`adcroft:95`

.. math::
    :label: eq_eg_baro_inertial_stability

    S_{i} = f^{2} {\delta t}^2


evaluates to :math:`0.0144` , which is well below the 0.5 upper limit for stability.


The advective CFL :cite:`adcroft:95` for an extreme maximum horizontal flow speed of :math:`{|\vec{u}|} = 2 ms^{-1}`

.. math::
    :label: eq_eg_baro_cfl_stability

    S_{a} = \frac{| \vec{u} | \delta t}{ \Delta x}


evaluates to 0.12. This is approaching the stability limit of 0.5 and limits :math:`\delta t` to 1200 s.


.. _sec_eg_baro_code_config:

Code Configuration
------------------

The model configuration for this experiment resides under the directory :code:`verification/tutorial_barotropic_gyre/`.

The experiment files

 - `input/data`
 - `input/data.pkg`
 - `input/eedata`
 - `input/windx.sin_y`
 - `input/topog.box`
 - `code/CPP_EEOPTIONS.h`
 - `code/CPP_OPTIONS.h`
 - `code/SIZE.h`

contain the code customizations and parameter settings for this 
experiments. Below we describe the customizations
to these files associated with this experiment.

File `input/data`
+++++++++++++++++


This file, reproduced completely below, specifies the main parameters 
for the experiment. The parameters that are significant for this configuration
are

 - Line 7

   - `viscAh=4.E2,`
   - this line sets the Laplacian friction coefficient to :math:`400 m^2s^{-1}`

 - Line 10

   - `beta=1.E-11,`
   - this line sets :math:`\beta` (the gradient of the coriolis parameter, :math:`f`) to :math:`10^{-11} s^{-1}m^{-1}`

 - Lines 15 and 16

   - `rigidLid=.FALSE.,`
   - `implicitFreeSurface=.TRUE.,`
   - these lines suppress the rigid lid formulation of the surface pressure inverter and activate the implicit free surface form of the pressure inverter.

 - Line 27

   - `startTime=0,`
   - this line indicates that the experiment should start from :math:`t=0` and implicitly suppresses searching for checkpoint files associated with restarting an numerical integration from a previously saved state.

 - Line 29

   - `endTime=12000,`
   - this line indicates that the experiment should start finish at :math:`t=12000s`. A restart file will be written at this time that will enable the simulation to be continued from this point.

 - Line 30

    - `deltaTmom=1200,`
    - This line sets the momentum equation timestep to :math:`1200s`.

 - Line 39

   - `usingCartesianGrid=.TRUE.,`
   - This line requests that the simulation be performed in a Cartesian coordinate system.

 - Line 41

   - `delX=60*20E3,`
   - This line sets the horizontal grid spacing between each x-coordinate line in the discrete grid. The syntax indicates that the discrete grid should be comprise of $60$ grid lines each separated by :math:`20 \times 10^{3}m` (20 km).

 - Line 42

   - `delY=60*20E3,`
   - This line sets the horizontal grid spacing between each y-coordinate line in the discrete grid to :math:`20 \times 10^{3}m` (20 km).

 - Line 43

   - `delZ=5000,`
   - This line sets the vertical grid spacing between each z-coordinate line in the discrete grid to 5000m (5 km).

 - Line 46

   - `bathyFile='topog.box'`
   - This line specifies the name of the file from which the domain bathymetry is read. This file is a two-dimensional (:math:`x,y`) map of depths. This file is assumed to contain 64-bit binary numbers giving the depth of the model at each grid cell, ordered with the x coordinate varying fastest. The points are ordered from low coordinate to high coordinate for both axes. The units and orientation of the depths in this file are the same as used in the MITgcm code. In this experiment, a depth of 0 m indicates a solid wall and a depth of -5000 m indicates open ocean. The matlab program `input/gendata.m` shows an example of how to generate a bathymetry file.

 - Line 49

   - `zonalWindFile='windx.sin_y'`
   - This line specifies the name of the file from which the x-direction surface wind stress is read. This file is also a two-dimensional (:math:`x,y`) map and is enumerated and formatted in the same manner as the bathymetry file. The matlab program `input/gendata.m` includes example code to generate a valid `zonalWindFile` file.

other lines in the file `input/data` are standard values that are described in the MITgcm Getting Started and MITgcm Parameters notes.

.. literalinclude:: ../../../verification/tutorial_barotropic_gyre/input/data
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/input/data


File `input/data.pkg`
+++++++++++++++++++++

This file uses standard default values and does not contain
customizations for this experiment.

File `input/eedata`
+++++++++++++++++++

This file uses standard default values and does not contain
customizations for this experiment.

File `input/windx.sin_y`
++++++++++++++++++++++++

The `input/windx.sin_y` file specifies a two-dimensional (:math:`x,y`) 
map of wind stress, :math:`\tau_{x}`, values. The units used are :math:`Nm^{-2}`. Although :math:`\tau_{x}` is only a function of :math:`y` in this experiment
this file must still define a complete two-dimensional map in order
to be compatible with the standard code for loading forcing fields 
in MITgcm. The included matlab program `input/gendata.m` gives a complete
code for creating the `input/windx.sin_y` file.

File `input/topog.box`
++++++++++++++++++++++

The `input/topog.box` file specifies a two-dimensional (:math:`x,y`) map of depth values. For this experiment values are either 0 m or :math:`-delZ` m, corresponding respectively to a wall or to deep ocean. The file contains a raw binary stream of data that is enumerated in the same way as standard MITgcm two-dimensional, horizontal arrays. The included matlab program `input/gendata.m` gives a completecode for creating the `input/topog.box` file.


File `code/SIZE.h`
++++++++++++++++++

Two lines are customized in this file for the current experiment


 - Line 39
   
   - `sNx=60,`
   - this line sets the lateral domain extent in grid points for the axis aligned with the x-coordinate.

 - Line 40

   - `sNy=60,`
   - this line sets the lateral domain extent in grid points for the axis aligned with the y-coordinate.

.. literalinclude:: ../../../verification/tutorial_barotropic_gyre/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_barotropic_gyre/code/SIZE.h


File `code/CPP_OPTIONS.h`
+++++++++++++++++++++++++

This file uses standard default values and does not contain
customizations for this experiment.


File `code/CPP_EEOPTIONS.h`
+++++++++++++++++++++++++++

This file uses standard default values and does not contain
customizations for this experiment.
