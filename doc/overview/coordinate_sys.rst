.. _operators:

Coordinate systems
------------------

Spherical coordinates
~~~~~~~~~~~~~~~~~~~~~

In spherical coordinates, the velocity components in the zonal,
meridional and vertical direction respectively, are given by:

.. math:: u=r\cos \varphi \frac{D\lambda }{Dt}

.. math:: v=r\frac{D\varphi }{Dt}

.. math:: \dot{r}=\frac{Dr}{Dt}

(see :numref:`sphere_coor`) Here :math:`\varphi` is the latitude, :math:`\lambda` the longitude,
:math:`r` the radial distance of the particle from the center of the
earth, :math:`\Omega` is the angular speed of rotation of the Earth and
:math:`D/Dt` is the total derivative.

The ‘grad’ (:math:`\nabla`) and ‘div’ (:math:`\nabla  \cdot`) operators
are defined by, in spherical coordinates:

.. math::
    \nabla  \equiv \left( \frac{1}{r\cos \varphi }\frac{\partial }{\partial \lambda }
   ,\frac{1}{r}\frac{\partial }{\partial \varphi },\frac{\partial }{\partial r}
   \right)

.. math::
    \nabla  \cdot v\equiv \frac{1}{r\cos \varphi }\left\{ \frac{\partial u}{\partial
   \lambda }+\frac{\partial }{\partial \varphi }\left( v\cos \varphi \right) \right\}
   +\frac{1}{r^{2}}\frac{\partial \left( r^{2}\dot{r}\right) }{\partial r}

|

  .. figure:: figs/sphere.png
    :width: 70%
    :align: center
    :alt: diagram of spherical polar coordinates
    :name: sphere_coor
    
    Spherical polar coordinates: longitude :math:`\lambda`, latitude :math:`\varphi` and :math:`r` the distance from the center.
