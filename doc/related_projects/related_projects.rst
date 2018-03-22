Related Projects and Highlighted Papers
***************************************


Projects Related to MITgcm
==========================

Estimating the Circulation and Climate of the Ocean (ECCO)
----------------------------------------------------------

ECCO is a community of MITgcm users who create and analyze ocean state estimates. ECCO typically optimizes initial conditions, surface forcing fields, and internal parameters to fit a multi-decadal model solution to various data constraints using MITgcm's adjoint capabilities. Unlike other data assimilation products, ECCO solutions are dynamically self-consistent, have closed budgets, and can easily be re-run by users.

websites: https://ecco.jpl.nasa.gov/, http://eccov4.readthedocs.io/en/latest/

Gcmfaces: Gridded Earth Variables In Matlab And Octave
------------------------------------------------------

The gcmfaces toolbox handles gridded Earth variables as sets of connected arrays. This object-oriented approach allows users to write generic, compact analysis codes that readily become applicable to a wide variety of grids. gcmfaces notably allows for analysis of MITgcm output on any of its familiar grids.

website: http://gcmfaces.readthedocs.io/en/latest/

MITprof: In-Situ Ocean Data In Matlab And Octave
------------------------------------------------

The MITprof toolbox handles unevenly distributed in-situ ocean observations. It is notably used, along with gcmfaces, to generate input files for MITgcm's profiles package (MITgcm/pkg/profiles).

website: https://github.com/gaelforget/MITprof

OceanParcels - Lagrangian Particle Tracker
------------------------------------------

Parcels provides a set of Python classses and methods to create customizable particle tracking simulations, focussing on tracking of both passive water parcels as well as active plankton, plastic and fish.

website: http://oceanparcels.org/

Southern Ocean State Estimation (SOSE)
--------------------------------------

SOSE uses the same techniques as ECCO to produce an eddy-permitting state estimate of the Southern Ocean.

website: http://sose.ucsd.edu/

Xgcm: General Circulation Model Postprocessing with xarray
----------------------------------------------------------

Xgcm is a python packge for working with the datasets produced by numerical General Circulation Models
(GCMs) and similar gridded datasets that are amenable to finite volume analysis. In these datasets, different
variables are located at different positions with respect to a volume or area element (e.g. cell center, cell face,
etc.) xgcm solves the problem of how to interpolate and difference these variables from one position to another.

website: http://xgcm.readthedocs.io/en/latest/


Xmitgcm
-------

Xmitgcm is a Python module that loads MITgcm MDS output files as `xarray <http://xarray.pydata.org/en/stable/>`_ datasets with the associated grid information. These can be easily exported as NetCDF files.

website: http://xmitgcm.readthedocs.io/en/latest/



Highlighted Papers
==================

