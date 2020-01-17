.. _sec_eg_tank:

Rotating Tank
=============

  (in directory: :filelink:`verification/tutorial_rotating_tank/`)

This example configuration demonstrates using the MITgcm to simulate a
laboratory demonstration using a differentially heated rotating
annulus of water.  The simulation is configured for a laboratory scale
on a :math:`3^{\circ}\times1` cm cylindrical grid with 29
vertical levels of 0.5 cm each.  This is a typical laboratory setup for
illustrating principles of GFD, as well as for a laboratory data
assimilation project.

example illustration from GFD lab here

Equations Solved
----------------

Discrete Numerical Configuration
--------------------------------

The domain is discretized with a uniform cylindrical grid spacing in
the horizontal set to :math:`\Delta a=1` cm and :math:`\Delta \phi=3^{\circ}`, so
that there are 120 grid cells in the azimuthal direction and
31 grid cells in the radial, representing a tank 62 cm in
diameter.  The bathymetry file sets the depth=0 in the nine lowest
radial rows to represent the central of the annulus.  Vertically the
model is configured with 29 layers of uniform 0.5 cm
thickness.

something about heat flux

.. _sec_eg_tank_code_config:

Code Configuration
------------------

The model configuration for this experiment resides under the
directory :filelink:`verification/tutorial_rotating_tank/`.  The experiment files

 - :filelink:`verification/tutorial_rotating_tank/input/data`
 - :filelink:`verification/tutorial_rotating_tank/input/data.pkg`
 - :filelink:`verification/tutorial_rotating_tank/input/eedata`
 - ``verification/tutorial_rotating_tank/input/bathyPolR.bin``
 - ``verification/tutorial_rotating_tank/input/thetaPolR.bin``
 - :filelink:`verification/tutorial_rotating_tank/code/CPP_OPTIONS.h`
 - :filelink:`verification/tutorial_rotating_tank/code/SIZE.h`

contain the code customizations and parameter settings for this
experiments. Below we describe the customizations
to these files associated with this experiment.

File :filelink:`input/data <verification/tutorial_rotating_tank/input/data>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_rotating_tank/input/data
    :linenos:
    :caption: verification/tutorial_rotating_tank/input/data

This file specifies the main parameters
for the experiment. The parameters that are significant for this configuration
are

- Lines 9-10,

  ::

      viscAh=5.0E-6,
      viscAz=5.0E-6,

  These lines set the Laplacian friction coefficient in the horizontal
  and vertical, respectively.  Note that they are several orders of
  magnitude smaller than the other examples due to the small scale of
  this example.

- Lines 13-16,

  ::

      diffKhT=2.5E-6,
      diffKzT=2.5E-6,
      diffKhS=1.0E-6,
      diffKzS=1.0E-6,

  These lines set horizontal and vertical diffusion coefficients for
  temperature and salinity.  Similar to the friction coefficients, the
  values are a couple of orders of magnitude less than most
  configurations.

- Line 17,

  ::

      f0=0.5,

  this line sets the Coriolis term, and represents a tank spinning at about 2.4 rpm.

- Lines 24 and 25,

  ::

      rigidLid=.TRUE.,
      implicitFreeSurface=.FALSE.,

  These lines activate  the rigid lid formulation of the surface
  pressure inverter and suppress the implicit free surface form
  of the pressure inverter.

- Line 40,

  ::

     nIter=0,

  This line indicates that the experiment should start from :math:`t=0` and
  implicitly suppresses searching for checkpoint files associated with
  restarting an numerical integration from a previously saved state.
  Instead, the file ``thetaPolR.bin`` will be loaded to initialized the
  temperature fields as indicated below, and other variables will be
  initialized to their defaults.

- Line 43,

  ::

     deltaT=0.1,

  This line sets the integration timestep to 0.1 s.  This is an
  unusually small value among the examples due to the small physical
  scale of the experiment.  Using the ensemble Kalman filter to produce
  input fields can necessitate even shorter timesteps.

- Line 54,

  ::

     usingCylindricalGrid=.TRUE.,

  This line requests that the simulation be performed in a
  cylindrical coordinate system.

- Line 55,

  ::

     dXspacing=3,

  This line sets the azimuthal grid spacing between each
  :math:`x`-coordinate line
  in the discrete grid. The syntax indicates that the discrete grid
  should be comprised of 120 grid lines each separated by 3\ :sup:`o`.

- Line 56,

  ::

     dYspacing=0.01,

  This line sets the radial cylindrical grid spacing between each
  :math:`a`-coordinate line in the discrete grid to 1 cm.

- Line 57,

  ::

     delZ=29*0.005,

  This line sets the vertical grid spacing between each of 29
  :math:`z`-coordinate lines in the discrete grid to 0.005 m (= 5 mm).

- Line 64,

  ::

     bathyFile='bathyPolR.bin',

  This line specifies the name of the file from which the domain
  'bathymetry' (i.e., tank depth) is read. This file is a 2-D
  (:math:`a,\phi`) map of
  depths. This file is assumed to contain 64-bit binary numbers
  giving the depth of the model at each grid cell, ordered with the :math:`\phi`
  coordinate varying fastest. The points are ordered from low coordinate
  to high coordinate for both axes.  The units and orientation of the
  depths in this file are the same as used in the MITgcm code. In this
  experiment, a depth of 0 m indicates an area outside of the tank
  and a depth of -0.145 m indicates the tank itself.

- Line 63,

  ::

     hydrogThetaFile='thetaPol.bin',

  This line specifies the name of the file from which the initial values
  of temperature
  are read. This file is a 3-D
  (:math:`x,y,z`) map and is enumerated and formatted in the same manner as the
  bathymetry file.

- Lines 65 and 66

  ::

     tCylIn  = 0.,
     tCylOut  = 20.,

  These line specify the temperatures in degrees Celsius of the interior
  and exterior walls of the tank -- typically taken to be icewater on
  the inside and room temperature on the outside.

Other lines in the file :filelink:`verification/tutorial_rotating_tank/input/data` are
standard values that are described in :numref:`customize_model`.

File - :filelink:`input/data.pkg <verification/tutorial_rotating_tank/input/data.pkg>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File - :filelink:`input/eedata <verification/tutorial_rotating_tank/input/eedata>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

File ``input/thetaPolR.bin``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file specifies a 3-D :math:`(x,y,z)`
map of initial values of :math:`\theta` in degrees Celsius.  This particular
experiment is set to random values around 20 :sup:`o`\ C to provide initial
perturbations.

File ``input/bathyPolR.bin``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file specifies a 2-D :math:`(x,y)`
map of depth values. For this experiment values are either
0 m  or -delZ m, corresponding respectively to outside or inside of
the tank. The file contains a raw binary stream of data that is enumerated
in the same way as standard MITgcm 2-D, horizontal arrays.

File :filelink:`code/SIZE.h <verification/tutorial_rotating_tank/code/SIZE.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../../../verification/tutorial_rotating_tank/code/SIZE.h
    :linenos:
    :caption: verification/tutorial_rotating_tank/code/SIZE.h

Two lines are customized in this file for the current experiment

- Line 45,

  ::

      sNx=120,

  this line sets
  the lateral domain extent in grid points for the
  axis aligned with the :math:`x`-coordinate.

- Line 46,

  ::

      sNy=31,

  this line sets
  the lateral domain extent in grid points for the
  axis aligned with the :math:`y`-coordinate.

File :filelink:`code/CPP_OPTIONS.h <verification/tutorial_rotating_tank/code/CPP_OPTIONS.h>`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file uses standard default values and does not contain
customizations for this experiment.

