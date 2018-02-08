.. _sub_phys_pkg_land:

Land package
------------


Introduction
############

This package provides a simple land model based on Rong Zhang
[e-mail:roz@gfdl.noaa.gov] 2 layers model (see documentation below).

It is primarily implemented for AIM (\_v23) atmospheric physics but
could be adapted to work with a different atmospheric physics. Two
subroutines (*aim\_aim2land.F* *aim\_land2aim.F* in *pkg/aim\_v23*) are
used as interface with AIM physics.

Number of layers is a parameter (*land\_nLev* in *LAND\_SIZE.h*) and can
be changed.

**Note on Land Model**
date: June 1999
author: Rong Zhang


Equations and Key Parameters
############################

This is a simple 2-layer land model. The top layer depth
:math:`z1=0.1m`, the second layer depth :math:`z2=4m`.

Let :math:`T_{g1},T_{g2}` be the temperature of each layer,
:math:`W_{1,}W_{2}` be the soil moisture of each layer. The field
capacity :math:`f_{1,}` :math:`f_{2}` are the maximum water amount in
each layer, so :math:`W_{i}` is the ratio of available water to field
capacity. :math:`f_{i}=\gamma z_{i},\gamma =0.24` is the field capapcity
per meter soil\ :math:`,` so :math:`f_{1}=0.024m,` :math:`f_{2}=0.96m.`

The land temperature is determined by total surface downward heat flux
:math:`F,`

.. math:: z_{1}C_{1}\frac{dT_{g1}}{dt}=F-\lambda \frac{T_{g1}-T_{g2}}{(z_{1}+z_{2})/2}

.. math:: z_{2}C_{2}\frac{dT_{g2}}{dt}=\lambda \frac{T_{g1}-T_{g2}}{(z_{1}+z_{2})/2}

here :math:`C_{1},C_{2}` are the heat capacity of each layer ,
:math:`\lambda ` is the thermal conductivity,
:math:`\lambda =0.42Wm^{-1}K^{-1}.`

.. math:: C_{1}=C_{w}W_{1}\gamma +C_{s}

.. math:: C_{2}=C_{w}W_{2}\gamma +C_{s}

:math:`C_{w},C_{s}` are the heat capacity of water and dry soil
respectively. :math:`%
C_{w}=4.2\times 10^{6}Jm^{-3}K^{-1},C_{s}=1.13\times 10^{6}Jm^{-3}K^{-1}.`

The soil moisture is determined by precipitation :math:`P(m/s)`,surface
evaporation :math:`E(m/s)` and runoff :math:`R(m/s).`

.. math:: \frac{dW_{1}}{dt}=\frac{P-E-R}{f_{1}}+\frac{W_{2}-W_{1}}{\tau }

:math:`\tau =2` :math:`days` is the time constant for diffusion of
moisture between layers.

.. math:: \frac{dW_{2}}{dt}=\frac{f_{1}}{f_{2}}\frac{W_{1}-W_{2}}{\tau }

In the code, :math:`R=0` gives better result, :math:`W_{1},W_{2}` are
set to be within [0, 1]. If :math:`W_{1}` is greater than 1, then let
:math:`\delta W_{1}=W_{1}-1,W_{1}=1` and
:math:`W_{2}=W_{2}+p\delta W_{1}\frac{f_{1}}{f_{2}}`, i.e. the runoff of
top layer is put into second layer. :math:`p=0.5` is the fraction of top
layer runoff that is put into second layer.

The time step is 1 hour, it takes several years to reach equalibrium
offline.

Land diagnostics
################

::


    ------------------------------------------------------------------------
    <-Name->|Levs|<-parsing code->|<--  Units   -->|<- Tile (max=80c) 
    ------------------------------------------------------------------------
    GrdSurfT|  1 |SM      Lg      |degC            |Surface Temperature over land
    GrdTemp |  2 |SM      MG      |degC            |Ground Temperature at each level
    GrdEnth |  2 |SM      MG      |J/m3            |Ground Enthalpy at each level
    GrdWater|  2 |SM P    MG      |0-1             |Ground Water (vs Field Capacity) Fraction at each level
    LdSnowH |  1 |SM P    Lg      |m               |Snow Thickness over land
    LdSnwAge|  1 |SM P    Lg      |s               |Snow Age over land
    RUNOFF  |  1 |SM      L1      |m/s             |Run-Off per surface unit
    EnRunOff|  1 |SM      L1      |W/m^2           |Energy flux associated with run-Off
    landHFlx|  1 |SM      Lg      |W/m^2           |net surface downward Heat flux over land
    landPmE |  1 |SM      Lg      |kg/m^2/s        |Precipitation minus Evaporation over land
    ldEnFxPr|  1 |SM      Lg      |W/m^2           |Energy flux (over land) associated with Precip (snow,rain)

References
##########

Hansen J. et al. Efficient three-dimensional global models for climate
studies: models I and II. *Monthly Weather Review*, vol.111, no.4, pp.
609-62, 1983

Experiments and tutorials that use land
#######################################

-  Global atmosphere experiment in aim.5l_cs verification directory.


