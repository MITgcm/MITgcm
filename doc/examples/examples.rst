.. _chap_modelExamples:

MITgcm Tutorial Example Experiments
***********************************

The full MITgcm distribution comes with a set of pre-configured
numerical experiments.  Some of these example experiments are tests of
individual parts of the model code, but many are fully fledged
numerical simulations. Full tutorials exist for a few of the examples,
and are documented in sections :numref:`sec_eg_baro` -
:numref:`sec_eg_tank`. The other examples follow the same general
structure as the tutorial examples. However, they only include brief
instructions in text file README.  The examples are
located in subdirectories under the directory `verification <https://github.com/altMITgcm/MITgcm66h/tree/master/>`_.
Each example is briefly described below.

.. _sec_eg_baro:

Barotropic Gyre MITgcm Example
==============================

  (in directory  :filelink:`verification/tutorial_barotropic_gyre/`)


This example experiment demonstrates using the MITgcm to simulate a Barotropic, wind-forced, ocean gyre circulation. The files for this experiment can be found in the verification directory `verification/tutorial_barotropic_gyre`. The experiment is a numerical rendition of the gyre circulation problem similar to the problems described analytically by Stommel in 1966  :cite:`Stommel66` and numerically in Holland et. al :cite:`Holland75`.

In this experiment the model is configured to represent a rectangular enclosed box of fluid, :math:`1200 \times 1200` km in lateral extent. The fluid is 5 km deep and is forced by a constant in time zonal wind stress, :math:`\tau_x`, that varies sinusoidally in the 'north-south' direction. Topologically the grid is Cartesian and the coriolis parameter :math:`f` is defined according to a mid-latitude beta-plane equation

.. math::
    :label: eq_eg_baro_fcori

    f(y) = f_{0}+\beta y

where :math:`y` is the distance along the 'north-south' axis of the simulated domain. For this experiment :math:`f_{0}` is set to :math:`10^{-4}s^{-1}` in :eq:`eq_eg_baro_fcori` and :math:`\beta = 10^{-11}s^{-1}m^{-1}`.


The sinusoidal wind-stress variations are defined according to 

.. math:: 
   :label: eq_eg_baro_taux

   \tau_x(y) = \tau_{0}\sin(\pi \frac{y}{L_y})

 
where :math:`L_{y}` is the lateral domain extent (1200~km) and 
:math:`\tau_0` is set to :math:`0.1N m^{-2}`. 


:numref:`fig_eg_baro_simulation_config` summarizes the configuration simulated.


  .. figure:: barotropic_gyre/figs/barogyre_simulation_config.*
      :width: 70%
      :align: center
      :alt: barotropic gyre configuration
      :name: fig_eg_baro_simulation_config

      Schematic of simulation domain and wind-stress forcing function for barotropic gyre numerical experiment. The domain is enclosed by solid walls at :math:`x=` 0, 1200 km and at :math:`y=` 0, 1200 km.


.. toctree::
   :maxdepth: 1

   barotropic_gyre/barotropic_gyre.rst


.. _sec_eg_tank:

A Rotating Tank in Cylindrical Coordinates
==========================================

  (in directory: :filelink:`verification/rotating_tank/`)

This example configuration demonstrates using the MITgcm to simulate a
laboratory demonstration using a differentially heated rotating
annulus of water.  The simulation is configured for a laboratory scale
on a :math:`3^{\circ}\times1\mathrm{cm}` cyclindrical grid with twenty-nine
vertical levels of 0.5cm each.  This is a typical laboratory setup for
illustration principles of GFD, as well as for a laboratory data
assimilation project. The files for this experiment can be found in
the verification directory under :code:`rotating_tank`.


example illustration from GFD lab here
 

.. toctree::
   :maxdepth: 1

   rotating_tank/rotating_tank.rst


