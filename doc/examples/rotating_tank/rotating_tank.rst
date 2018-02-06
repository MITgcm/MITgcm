
Equations Solved
----------------


Discrete Numerical Configuration
--------------------------------

The domain is discretised with a uniform cylindrical grid spacing in
the horizontal set to :math:`\Delta a=1`~cm and :math:`\Delta \phi=3^{\circ}`, so
that there are 120 grid cells in the azimuthal direction and
thirty-one grid cells in the radial, representing a tank 62cm in
diameter.  The bathymetry file sets the depth=0 in the nine lowest
radial rows to represent the central of the annulus.  Vertically the
model is configured with twenty-nine layers of uniform 0.5cm
thickness.


something about heat flux


.. _sec_eg_tank_code_config:

Code Configuration
------------------

The model configuration for this experiment resides under the
directory :code:`verification/rotatingi_tank/`.  The experiment files

 - :code:`input/data`
 - :code:`input/data.pkg`
 - :code:`input/eedata`
 - :code:`input/bathyPol.bin`
 - :code:`input/thetaPol.bin`
 - :code:`code/CPP\_EEOPTIONS.h`
 - :code:`code/CPP\_OPTIONS.h`
 - :code:`code/SIZE.h`

contain the code customizations and parameter settings for this 
experiments. Below we describe the customizations
to these files associated with this experiment.

File `input/data`
+++++++++++++++++

This file, reproduced completely below, specifies the main parameters 
for the experiment. The parameters that are significant for this configuration
are

 - Lines 9-10, 

   - `viscAh=5.0E-6,`
   - `viscAz=5.0E-6,`



These lines set the Laplacian friction coefficient in the horizontal
and vertical, respectively.  Note that they are several orders of
magnitude smaller than the other examples due to the small scale of
this example.

 - Lines 13-16,

   - diffKhT=2.5E-6,
   - diffKzT=2.5E-6,
   - diffKhS=1.0E-6,
   - diffKzS=1.0E-6,


These lines set horizontal and vertical diffusion coefficients for
temperature and salinity.  Similarly to the friction coefficients, the
values are a couple of orders of magnitude less than most
configurations.


 - Line 17, `f0=0.5`, this line sets the coriolis term, and represents a tank spinning at about 2.4 rpm.

 - Lines 23 and 24

   - `rigidLid=.TRUE.,`
   - `implicitFreeSurface=.FALSE.,`


These lines activate  the rigid lid formulation of the surface
pressure inverter and suppress the implicit free surface form
of the pressure inverter.

 - Line 40,

   - `nIter=0,`

This line indicates that the experiment should start from $t=0$ and
implicitly suppresses searching for checkpoint files associated with
restarting an numerical integration from a previously saved state.
Instead, the file thetaPol.bin will be loaded to initialized the
temperature fields as indicated below, and other variables will be
initialized to their defaults.


 - Line 43,

   - `deltaT=0.1,`

This line sets the integration timestep to $0.1s$.  This is an
unsually small value among the examples due to the small physical
scale of the experiment.  Using the ensemble Kalman filter to produce
input fields can necessitate even shorter timesteps.

 - Line 56,

   - `usingCylindricalGrid=.TRUE.,`

This line requests that the simulation be performed in a 
cylindrical coordinate system.

 - Line 57,

   - `dXspacing=3,`

This line sets the azimuthal grid spacing between each $x$-coordinate line
in the discrete grid. The syntax indicates that the discrete grid
should be comprised of $120$ grid lines each separated by $3^{\circ}$.
                                                                               

 - Line 58,

   - `dYspacing=0.01,`


This line sets the radial cylindrical grid spacing between each
:math:`a`-coordinate line in the discrete grid to :math:`1cm`.

 - Line 59,

   - `delZ=29*0.005,`

This line sets the vertical grid spacing between each of 29
z-coordinate lines in the discrete grid to $0.005m$ ($5$~mm).

 - Line 64,

   - `bathyFile='bathyPol.bin',`


This line specifies the name of the file from which the domain
'bathymetry' (tank depth) is read. This file is a two-dimensional 
(:math:`a,\phi`) map of
depths. This file is assumed to contain 64-bit binary numbers 
giving the depth of the model at each grid cell, ordered with the $\phi$ 
coordinate varying fastest. The points are ordered from low coordinate
to high coordinate for both axes.  The units and orientation of the
depths in this file are the same as used in the MITgcm code. In this
experiment, a depth of $0m$ indicates an area outside of the tank
and a depth
f :math:`-0.145m` indicates the tank itself. 

 - Line 65,

   - `hydrogThetaFile='thetaPol.bin',`

This line specifies the name of the file from which the initial values 
of temperature
are read. This file is a three-dimensional
(:math:`x,y,z`) map and is enumerated and formatted in the same manner as the 
bathymetry file. 

 - Lines 66 and 67

   - `tCylIn  = 0`
   - `tCylOut  = 20`

These line specify the temperatures in degrees Celsius of the interior
and exterior walls of the tank -- typically taken to be icewater on
the inside and room temperature on the outside.



Other lines in the file `input/data` are standard values
that are described in the MITgcm Getting Started and MITgcm Parameters
notes.

.. literalinclude:: ../../../verification/rotating_tank/input/data
    :linenos:
    :caption: `verification/rotating_tank/input/data`


File `input/data.pkg`
++++++++++++++++++++++++

This file uses standard default values and does not contain
customizations for this experiment.

File `input/eedata`
+++++++++++++++++++

This file uses standard default values and does not contain
customizations for this experiment.

File `input/thetaPol.bin`
+++++++++++++++++++++++++

The {\it input/thetaPol.bin} file specifies a three-dimensional ($x,y,z$) 
map of initial values of $\theta$ in degrees Celsius.  This particular 
experiment is set to random values x around 20C to provide initial 
perturbations.

File `input/bathyPol.bin`
+++++++++++++++++++++++++

The {\it input/bathyPol.bin} file specifies a two-dimensional ($x,y$) 
map of depth values. For this experiment values are either
$0m$ or {\bf -delZ}m, corresponding respectively to outside or inside of
the tank. The file contains a raw binary stream of data that is enumerated
in the same way as standard MITgcm two-dimensional, horizontal arrays.

File `code/SIZE.h`
++++++++++++++++++

Two lines are customized in this file for the current experiment


 - Line 39, 
   - `sNx=120,`

this line sets
the lateral domain extent in grid points for the
axis aligned with the x-coordinate.

 - Line 40, 
   - `sNy=31,`

this line sets
the lateral domain extent in grid points for the
axis aligned with the y-coordinate.

.. literalinclude:: ../../../verification/rotating_tank/code/SIZE.h
    :linenos:
    :caption: `verification/rotating_tank/code/SIZE.h`


File `code/CPP_OPTIONS.h`
+++++++++++++++++++++++++

This file uses standard default values and does not contain
customizations for this experiment.


File `code/CPP_EEOPTIONS.h`
+++++++++++++++++++++++++++

This file uses standard default values and does not contain
customizations for this experiment.

