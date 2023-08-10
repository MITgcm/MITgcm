.. include:: ../defs.hrst

.. _sub_phys_pkg_fizhi:

Fizhi: High-end Atmospheric Physics
-----------------------------------


Introduction
++++++++++++

The fizhi (high-end atmospheric physics) package includes a collection
of state-of-the-art physical parameterizations for atmospheric
radiation, cumulus convection, atmospheric boundary layer turbulence,
and land surface processes. The collection of atmospheric physics
parameterizations were originally used together as part of the GEOS-3
(Goddard Earth Observing System-3) GCM developed at the NASA/Goddard
Global Modelling and Assimilation Office (GMAO).

Equations
+++++++++

Moist Convective Processes:


.. _para_phys_pkg_fizhi_mc:

Sub-grid and Large-scale Convection
###################################

Sub-grid scale cumulus convection is parameterized using the Relaxed
Arakawa Schubert (RAS) scheme of :cite:`moorsz:92`, which is a linearized Arakawa
Schubert type scheme. RAS predicts the mass flux from an ensemble of
clouds. Each subensemble is identified by its entrainment rate and level
of neutral bouyancy which are determined by the grid-scale properties.

The thermodynamic variables that are used in RAS to describe the grid
scale vertical profile are the dry static energy, :math:`s=c_pT +gz`,
and the moist static energy, :math:`h=c_p T + gz + Lq`. The conceptual
model behind RAS depicts each subensemble as a rising plume cloud,
entraining mass from the environment during ascent, and detraining all
cloud air at the level of neutral buoyancy. RAS assumes that the
normalized cloud mass flux, :math:`\eta`, normalized by the cloud base
mass flux, is a linear function of height, expressed as:

.. math::

   \pp{\eta(z)}{z} = \lambda \hspace{0.4cm} \text{or} \hspace{0.4cm} \pp{\eta(P^{\kappa})}{P^{\kappa}} = 
   -\frac{c_p}{g} \theta \lambda

where we have used the hydrostatic equation written in the form:

.. math:: \pp{z}{P^{\kappa}} = -\frac{c_p}{g} \theta

The entrainment parameter, :math:`\lambda`, characterizes a particular
subensemble based on its detrainment level, and is obtained by assuming
that the level of detrainment is the level of neutral buoyancy, ie., the
level at which the moist static energy of the cloud, :math:`h_c`, is
equal to the saturation moist static energy of the environment,
:math:`h^*`. Following :cite:`moorsz:92`, :math:`\lambda` may be written as

.. math:: \lambda = \frac{h_B - h^*_D}{\frac{c_p}{g} \int_{P_D}^{P_B}\theta(h^*_D-h)dP^{\kappa}}

where the subscript :math:`B` refers to cloud base, and the subscript
:math:`D` refers to the detrainment level.

The convective instability is measured in terms of the cloud work
function :math:`A`, defined as the rate of change of cumulus kinetic
energy. The cloud work function is related to the buoyancy, or the
difference between the moist static energy in the cloud and in the
environment:

.. math::

   A = \int_{P_D}^{P_B} \frac{\eta}{1 + \gamma}
   \left[ \frac{h_c-h^*}{P^{\kappa}} \right] dP^{\kappa}

where :math:`\gamma` is :math:`\frac{L}{c_p}\pp{q^*}{T}` obtained from
the Claussius Clapeyron equation, and the subscript :math:`c` refers to
the value inside the cloud.

To determine the cloud base mass flux, the rate of change of :math:`A`
in time *due to dissipation by the clouds* is assumed to approximately
balance the rate of change of :math:`A` *due to the generation by the
large scale*. This is the quasi-equilibrium assumption, and results in
an expression for :math:`m_B`:

.. math:: m_B = \dfrac{- \left. \frac{dA}{dt} \right|_{\rm ls}}{K}

where :math:`K` is the cloud kernel, defined as the rate of change of
the cloud work function per unit cloud base mass flux, and is currently
obtained by analytically differentiating the expression for :math:`A` in
time. The rate of change of :math:`A` due to the generation by the large
scale can be written as the difference between the current
:math:`A(t+\Delta t)` and its equilibrated value after the previous
convective time step :math:`A(t)`, divided by the time step.
:math:`A(t)` is approximated as some critical :math:`A_{\rm crit}`, computed
by Lord (1982) from in situ observations.

The predicted convective mass fluxes are used to solve grid-scale
temperature and moisture budget equations to determine the impact of
convection on the large scale fields of temperature (through latent
heating and compensating subsidence) and moisture (through precipitation
and detrainment):

.. math:: \left.{\pp{\theta}{t}}\right|_{c} = \alpha \frac{ m_B}{c_p P^{\kappa}} \eta \pp{s}{p}

and

.. math:: \left.{\pp{q}{t}}\right|_{c} = \alpha \frac{m_B}{L} \eta \left( \pp{h}{p}-\pp{s}{p} \right)

where :math:`\theta = \frac{T}{P^{\kappa}}`, :math:`P = (p/p_0)`, and
:math:`\alpha` is the relaxation parameter.

As an approximation to a full interaction between the different
allowable subensembles, many clouds are simulated frequently, each
modifying the large scale environment some fraction :math:`\alpha` of
the total adjustment. The parameterization thereby “relaxes” the large
scale environment towards equilibrium.

In addition to the RAS cumulus convection scheme, the fizhi package
employs a Kessler-type scheme for the re-evaporation of falling rain :cite:`sudm:88`,
which correspondingly adjusts the temperature assuming :math:`h` is
conserved. RAS in its current formulation assumes that all cloud water
is deposited into the detrainment level as rain. All of the rain is
available for re-evaporation, which begins in the level below
detrainment. The scheme accounts for some microphysics such as the
rainfall intensity, the drop size distribution, as well as the
temperature, pressure and relative humidity of the surrounding air. The
fraction of the moisture deficit in any model layer into which the rain
may re-evaporate is controlled by a free parameter, which allows for a
relatively efficient re-evaporation of liquid precipitate and larger
rainout for frozen precipitation.

Due to the increased vertical resolution near the surface, the lowest
model layers are averaged to provide a 50 mb thick sub-cloud layer for
RAS. Each time RAS is invoked (every ten simulated minutes), a number of
randomly chosen subensembles are checked for the possibility of
convection, from just above cloud base to 10 mb.

Supersaturation or large-scale precipitation is initiated in the fizhi
package whenever the relative humidity in any grid-box exceeds a
critical value, currently 100%. The large-scale precipitation
re-evaporates during descent to partially saturate lower layers in a
process identical to the re-evaporation of convective rain.

.. _fizhi_clouds:

Cloud Formation
###############

Convective and large-scale cloud fractons which are used for
cloud-radiative interactions are determined diagnostically as part of
the cumulus and large-scale parameterizations. Convective cloud
fractions produced by RAS are proportional to the detrained liquid water
amount given by

.. math:: F_{\rm RAS} = \min\left[ \frac{l_{\rm RAS}}{l_c}, 1 \right]

where :math:`l_c` is an assigned critical value equal to :math:`1.25`
g/kg. A memory is associated with convective clouds defined by:

.. math:: F_{\rm RAS}^n = \min\left[ F_{\rm RAS} + \left(1-\frac{\Delta t_{\rm RAS}}{\tau}\right) F_{\rm RAS}^{n-1} \, , \, 1 \right],

where :math:`F_{\rm RAS}` is the instantaneous cloud fraction and
:math:`F_{\rm RAS}^{n-1}` is the cloud fraction from the previous RAS
timestep. The memory coefficient is computed using a RAS cloud
timescale, :math:`\tau`, equal to 1 hour. RAS cloud fractions are
cleared when they fall below 5%.

Large-scale cloudiness is defined, following Slingo and Ritter (1985),
as a function of relative humidity:

.. math:: F_{\rm ls} = \min\left[ { \left( \frac{\textrm{RH}-\textrm{RH}_c}{1-\textrm{RH}_c} \right) }^2 \, , \, 1 \right]

where

.. math::
   \begin{aligned}
   \textrm{RH}_c & = 1-s(1-s)(2-\sqrt{3}+2\sqrt{3}s)r \\
   s & = p/p_{\rm surf} \\
   r & = \left(\frac{1.0-\textrm{RH}_{\rm min}}{\alpha}\right) \\
   \textrm{RH}_{\rm min} & = 0.75 \\
   \alpha & = 0.573285 \end{aligned}

These cloud fractions are suppressed, however, in regions where the
convective sub-cloud layer is conditionally unstable. The functional
form of :math:`\textrm{RH}_c` is shown in :numref:`rhcrit`



 .. figure:: figs/rhcrit.*
    :width: 70%
    :align: center
    :alt: critical relative humidity for clouds
    :name: rhcrit

    Critical Relative Humidity for Clouds.



The total cloud fraction in a grid box is determined by the larger of
the two cloud fractions:

.. math:: F_{\rm cld} = \max \left[ F_{\rm RAS} \, , \, F_{\rm ls} \right]

Finally, cloud fractions are time-averaged between calls to the
radiation packages.

Radiation:

The parameterization of radiative heating in the fizhi package includes
effects from both shortwave and longwave processes. Radiative fluxes are
calculated at each model edge-level in both up and down directions. The
heating rates/cooling rates are then obtained from the vertical
divergence of the net radiative fluxes.

The net flux is

.. math:: F = F^\uparrow - F^\downarrow

where :math:`F` is the net flux, :math:`F^\uparrow` is the upward flux
and :math:`F^\downarrow` is the downward flux.

The heating rate due to the divergence of the radiative flux is given by

.. math:: \pp{\rho c_p T}{t} = - \pp{F}{z}

or

.. math:: \pp{T}{t} = \frac{g}{c_p \pi} \pp{F}{\sigma}

where :math:`g` is the accelation due to gravity and :math:`c_p` is the
heat capacity of air at constant pressure.

The time tendency for Longwave Radiation is updated every 3 hours. The
time tendency for Shortwave Radiation is updated once every three hours
assuming a normalized incident solar radiation, and subsequently
modified at every model time step by the true incident radiation. The
solar constant value used in the package is equal to 1365 W m\ :sup:`--2`
and a CO\ :sub:`2` mixing ratio of 330 ppm. For the ozone mixing ratio,
monthly mean zonally averaged climatological values specified as a
function of latitude and height :cite:`rosen:87` are linearly interpolated to the
current time.

Shortwave Radiation
###################

The shortwave radiation package used in the package computes solar
radiative heating due to the absoption by water vapor, ozone, carbon
dioxide, oxygen, clouds, and aerosols and due to the scattering by
clouds, aerosols, and gases. The shortwave radiative processes are
described by :cite:`chou:90,chou:92`. This shortwave package uses the Delta-Eddington
approximation to compute the bulk scattering properties of a single
layer following King and Harshvardhan (JAS, 1986). The transmittance and
reflectance of diffuse radiation follow the procedures of Sagan and
Pollock (JGR, 1967) and :cite:`lhans:74`.

Highly accurate heating rate calculations are obtained through the use
of an optimal grouping strategy of spectral bands. By grouping the UV
and visible regions as indicated in :numref:`tab_phys_pkg_fizhi_solar1`, the
Rayleigh scattering and the ozone absorption of solar radiation can be
accurately computed in the ultraviolet region and the photosynthetically
active radiation (PAR) region. The computation of solar flux in the
infrared region is performed with a broadband parameterization using the
spectrum regions shown in :numref:`tab_phys_pkg_fizhi_solar2`. The solar radiation
algorithm used in the fizhi package can be applied not only for climate
studies but also for studies on the photolysis in the upper atmosphere
and the photosynthesis in the biosphere.


.. table:: UV and visible spectral regions used in shortwave radiation package. 
  :name: tab_phys_pkg_fizhi_solar1

  +----------+--------+-----------------------+
  |     **UV and Visible Spectral Regions**   |
  +----------+--------+-----------------------+
  | Region   | Band   | Wavelength (micron)   |
  +==========+========+=======================+
  | UV-C     | 1.     | .175 - .225           |
  +----------+--------+-----------------------+
  |          | 2.     | .225 - .245           |
  +----------+--------+-----------------------+
  |          |        | .260 - .280           |
  +----------+--------+-----------------------+
  |          | 3.     | .245 - .260           |
  +----------+--------+-----------------------+
  | UV-B     | 4.     | .280 - .295           |
  +----------+--------+-----------------------+
  |          | 5.     | .295 - .310           |
  +----------+--------+-----------------------+
  |          | 6.     | .310 - .320           |
  +----------+--------+-----------------------+
  | UV-A     | 7.     | .320 - .400           |
  +----------+--------+-----------------------+
  | PAR      | 8.     | .400 - .700           |
  +----------+--------+-----------------------+




.. table:: Infrared spectral regions used in shortwave radiation package.
  :name: tab_phys_pkg_fizhi_solar2

  +--------+---------------------------------+-----------------------+
  |            **Infrared Spectral Regions**                         |
  +--------+---------------------------------+-----------------------+
  | Band   | Wavenumber (cm\ :sup:`--1`)     | Wavelength (micron)   |
  +========+=================================+=======================+
  | 1      | 1000-4400                       | 2.27-10.0             |
  +--------+---------------------------------+-----------------------+
  | 2      | 4400-8200                       | 1.22-2.27             |
  +--------+---------------------------------+-----------------------+
  | 3      | 8200-14300                      | 0.70-1.22             |
  +--------+---------------------------------+-----------------------+


Within the shortwave radiation package, both ice and liquid cloud
particles are allowed to co-exist in any of the model layers. Two sets
of cloud parameters are used, one for ice paticles and the other for
liquid particles. Cloud parameters are defined as the cloud optical
thickness and the effective cloud particle size. In the fizhi package,
the effective radius for water droplets is given as 10 microns, while 65
microns is used for ice particles. The absorption due to aerosols is
currently set to zero.

To simplify calculations in a cloudy atmosphere, clouds are grouped into
low (:math:`p>700` mb), middle (700 mb :math:`\ge p > 400` mb), and high
(:math:`p < 400` mb) cloud regions. Within each of the three regions,
clouds are assumed maximally overlapped, and the cloud cover of the
group is the maximum cloud cover of all the layers in the group. The
optical thickness of a given layer is then scaled for both the direct
(as a function of the solar zenith angle) and diffuse beam radiation so
that the grouped layer reflectance is the same as the original
reflectance. The solar flux is computed for each of eight cloud
realizations possible within this low/middle/high classification, and
appropriately averaged to produce the net solar flux.

Longwave Radiation
##################

The longwave radiation package used in the fizhi package is thoroughly
described by :cite:`chsz:94`. As described in that document, IR fluxes are
computed due to absorption by water vapor, carbon dioxide, and ozone. The
spectral bands together with their absorbers and parameterization methods,
configured for the fizhi package, are shown in
:numref:`tab_phys_pkg_fizhi_longwave`.

.. table:: IR spectral bands, absorbers, and parameterization method
  :name: tab_phys_pkg_fizhi_longwave

  +----------------+------------------------------------+------------------------------+----------+
  |                  **IR Spectral Bands**                                                        |
  +----------------+------------------------------------+------------------------------+----------+
  | Band           | Spectral Range (cm\ :sup:`--1`)    | Absorber                     | Method   |
  +================+====================================+==============================+==========+
  | 1              | 0-340                              | H\ :sub:`2`\ O line          | T        |
  +----------------+------------------------------------+------------------------------+----------+
  | 2              | 340-540                            | H\ :sub:`2`\ O line          | T        |
  +----------------+------------------------------------+------------------------------+----------+
  | 3a             | 540-620                            | H\ :sub:`2`\ O line          | K        |
  +----------------+------------------------------------+------------------------------+----------+
  | 3b             | 620-720                            | H\ :sub:`2`\ O continuum     | S        |
  +----------------+------------------------------------+------------------------------+----------+
  | 3b             | 720-800                            | CO\ :sub:`2`                 | T        |
  +----------------+------------------------------------+------------------------------+----------+
  | 4              | 800-980                            | H\ :sub:`2`\ O line          | K        |
  +----------------+------------------------------------+------------------------------+----------+
  |                |                                    | H\ :sub:`2`\ O continuum     | S        |
  +----------------+------------------------------------+------------------------------+----------+
  |                |                                    | H\ :sub:`2`\ O line          | K        |
  +----------------+------------------------------------+------------------------------+----------+
  | 5              | 980-1100                           | H\ :sub:`2`\ O continuum     | S        |
  +----------------+------------------------------------+------------------------------+----------+
  |                |                                    | O\ :sub:`3`                  | T        |
  +----------------+------------------------------------+------------------------------+----------+
  | 6              | 1100-1380                          | H\ :sub:`2`\ O line          | K        |
  +----------------+------------------------------------+------------------------------+----------+
  |                |                                    | H\ :sub:`2`\ O continuum     | S        |
  +----------------+------------------------------------+------------------------------+----------+
  | 7              | 1380-1900                          | H\ :sub:`2`\ O line          | T        |
  +----------------+------------------------------------+------------------------------+----------+
  | 8              | 1900-3000                          | H\ :sub:`2`\ O line          | K        |
  +----------------+------------------------------------+------------------------------+----------+
  | K: :math:`k`-distribution method with linear pressure scaling                                 |
  +----------------+------------------------------------+------------------------------+----------+
  | T: Table look-up with temperature and pressure scaling                                        |
  +----------------+------------------------------------+------------------------------+----------+
  | S: One-parameter temperature scaling                                                          |
  +----------------+------------------------------------+------------------------------+----------+


The longwave radiation package accurately computes cooling rates for the
middle and lower atmosphere from 0.01 mb to the surface. Errors are
< 0.4 C day\ :sup:`--1` in cooling rates and < 1% in
fluxes. From Chou and Suarez, it is estimated that the total effect of
neglecting all minor absorption bands and the effects of minor infrared
absorbers such as nitrous oxide (N\ :sub:`2`\ O), methane
(CH\ :sub:`4`), and the chlorofluorocarbons (CFCs), is an underestimate
of :math:`\approx 5` W m\ :sup:`--2` in the downward flux at the surface
and an overestimate of :math:`\approx 3` W m\ :sup:`--2` in the upward
flux at the top of the atmosphere.

Similar to the procedure used in the shortwave radiation package, clouds
are grouped into three regions catagorized as low/middle/high. The net
clear line-of-site probability :math:`(P)` between any two levels,
:math:`p_1` and :math:`p_2 \quad (p_2 > p_1)`, assuming randomly
overlapped cloud groups, is simply the product of the probabilities
within each group:

.. math:: P_{\rm net} = P_{\rm low} \times P_{\rm mid} \times P_{\rm hi}

Since all clouds within a group are assumed maximally overlapped, the
clear line-of-site probability within a group is given by:

.. math:: P_{\rm group} = 1 - F_{\rm max}

where :math:`F_{\rm max}` is the maximum cloud fraction encountered between
:math:`p_1` and :math:`p_2` within that group. For groups and/or levels
outside the range of :math:`p_1` and :math:`p_2`, a clear line-of-site
probability equal to 1 is assigned.

Cloud-Radiation Interaction
###########################

The cloud fractions and diagnosed cloud liquid water produced by moist
processes within the fizhi package are used in the radiation packages to
produce cloud-radiative forcing. The cloud optical thickness associated
with large-scale cloudiness is made proportional to the diagnosed
large-scale liquid water, :math:`\ell`, detrained due to
super-saturation. Two values are used corresponding to cloud ice
particles and water droplets. The range of optical thickness for these
clouds is given as

.. math:: 0.0002 \le \tau_{\rm ice} (\text{mb}^{-1}) \le 0.002  \quad\mbox{for}\quad  0 \le \ell \le 2 \; \text{mg/kg}

.. math:: 0.02 \le \tau_{\rm H_2O} (\text{mb}^{-1}) \le 0.2  \quad\mbox{for}\quad  0 \le \ell \le 10 \; \text{mg/kg}

The partitioning, :math:`\alpha`, between ice particles and water
droplets is achieved through a linear scaling in temperature:

.. math:: 0 \le \alpha \le 1 \quad\mbox{for}\quad  233.15 \le T \le 253.15

The resulting optical depth associated with large-scale cloudiness is
given as

.. math:: \tau_{\rm ls} = \alpha \tau_{\rm H_2O} + (1-\alpha) \tau_{\rm ice}

The optical thickness associated with sub-grid scale convective clouds
produced by RAS is given as

.. math:: \tau_{\rm RAS} = 0.16 \; \text{mb}^{-1}

The total optical depth in a given model layer is computed as a weighted
average between the large-scale and sub-grid scale optical depths,
normalized by the total cloud fraction in the layer:

.. math:: \tau = \left( \frac{F_{\rm RAS} \,\,\, \tau_{\rm RAS} + F_{\rm ls} \,\,\, \tau_{\rm ls} }{ F_{\rm RAS}+F_{\rm ls} } \right) \Delta p

where :math:`F_{\rm RAS}` and :math:`F_{\rm ls}` are the time-averaged cloud
fractions associated with RAS and large-scale processes described in
:numref:`fizhi_clouds`. The optical thickness for the longwave
radiative feedback is assumed to be 75%  of these values.

The entire Moist Convective Processes Module is called with a frequency
of 10 minutes. The cloud fraction values are time-averaged over the
period between Radiation calls (every 3 hours). Therefore, in a
time-averaged sense, both convective and large-scale cloudiness can
exist in a given grid-box.

Turbulence
##########

Turbulence is parameterized in the fizhi package to account for its
contribution to the vertical exchange of heat, moisture, and momentum.
The turbulence scheme is invoked every 30 minutes, and employs a
backward-implicit iterative time scheme with an internal time step of 5
minutes. The tendencies of atmospheric state variables due to turbulent
diffusion are calculated using the diffusion equations:

.. math::
   \begin{aligned}
   {\pp{u}{t}}_{\rm turb} &= {\pp{}{z} }{(- \overline{u^{\prime}w^{\prime}})}
    = {\pp{}{z} }{\left(K_m \pp{u}{z}\right)} \nonumber \\
   {\pp{v}{t}}_{\rm turb} &= {\pp{}{z} }{(- \overline{v^{\prime}w^{\prime}})}
    = {\pp{}{z} }{\left(K_m \pp{v}{z}\right)} \nonumber \\
   {\pp{T}{t}} = P^{\kappa}{\pp{\theta}{t}}_{\rm turb} &= 
   P^{\kappa}{\pp{}{z} }{(- \overline{w^{\prime}\theta^{\prime}})}
    = P^{\kappa}{\pp{}{z} }{\left(K_h \pp{\theta_v}{z}\right)} \nonumber \\
   {\pp{q}{t}}_{\rm turb} &= {\pp{}{z} }{(- \overline{w^{\prime}q^{\prime}})}
    = {\pp{}{z} }{\left(K_h \pp{q}{z}\right)}
   \end{aligned}

Within the atmosphere, the time evolution of second turbulent moments is
explicitly modeled by representing the third moments in terms of the
first and second moments. This approach is known as a second-order
closure modeling. To simplify and streamline the computation of the
second moments, the level 2.5 assumption of Mellor and Yamada (1974) and :cite:`yam:77`
is employed, in which only the turbulent kinetic energy (TKE),

.. math:: {\h}{q^2}={\overline{{u^{\prime}}^2}}+{\overline{{v^{\prime}}^2}}+{\overline{{w^{\prime}}^2}}

is solved prognostically and the other second moments are solved
diagnostically. The prognostic equation for TKE allows the scheme to
simulate some of the transient and diffusive effects in the turbulence.
The TKE budget equation is solved numerically using an implicit backward
computation of the terms linear in :math:`q^2` and is written:

.. math::

   {\dd{}{t} \left({{\h} q^2}\right)} - { \pp{}{z} \left[{ \frac{5}{3} {{\lambda}_1} q { \pp {}{z} 
   \left({\h}q^2\right)} }\right]} =
   {- \overline{{u^{\prime}}{w^{\prime}}} { \pp{U}{z} }} - {\overline{{v^{\prime}}{w^{\prime}}} 
   { \pp{V}{z} }} + {\frac{g}{\Theta_0}{\overline{{w^{\prime}}{{{\theta}_v}^{\prime}}}}
   - \frac{ q^3}{{\Lambda}_1} }

where :math:`q` is the turbulent velocity, :math:`{u^{\prime}}`,
:math:`{v^{\prime}}`, :math:`{w^{\prime}}` and
:math:`{{\theta}^{\prime}}` are the fluctuating parts of the velocity
components and potential temperature, :math:`U` and :math:`V` are the
mean velocity components, :math:`{\Theta_0}^{-1}` is the coefficient of
thermal expansion, and :math:`{{\lambda}_1}` and :math:`{{\Lambda} _1}`
are constant multiples of the master length scale, :math:`\ell`, which
is designed to be a characteristic measure of the vertical structure of
the turbulent layers.

The first term on the left-hand side represents the time rate of change
of TKE, and the second term is a representation of the triple
correlation, or turbulent transport term. The first three terms on the
right-hand side represent the sources of TKE due to shear and bouyancy,
and the last term on the right hand side is the dissipation of TKE.

In the level 2.5 approach, the vertical fluxes of the scalars
:math:`\theta_v` and :math:`q` and the wind components :math:`u` and
:math:`v` are expressed in terms of the diffusion coefficients
:math:`K_h` and :math:`K_m`, respectively. In the statisically
realizable level 2.5 turbulence scheme of :cite:`helflab:88`, these diffusion coefficients
are expressed as

.. math::

   K_h 
    = \left\{ \begin{array}{l@{\quad\mbox{for}\quad}l} q \, \ell \, S_H(G_M,G_H) \, & \mbox{decaying turbulence}
   \\ \frac{ q^2 }{ q_{\rm eq} } \, \ell \, S_{H}(G_{M_e},G_{H_e}) \, & \mbox{growing turbulence} \end{array} \right.

and

.. math::

   K_m
    = \left\{ \begin{array}{l@{\quad\mbox{for}\quad}l} q \, \ell \, S_M(G_M,G_H) \, & \mbox{decaying turbulence}                
   \\ \frac{ q^2 }{ q_{\rm eq} } \, \ell \, S_{M}(G_{M_e},G_{H_e}) \, & \mbox{growing turbulence} \end{array} \right.

where the subscript 'eq' refers to the value under conditions of
local equilibrium (obtained from the Level 2.0 Model), :math:`\ell` is
the master length scale related to the vertical structure of the
atmosphere, and :math:`S_M` and :math:`S_H` are functions of :math:`G_H`
and :math:`G_M`, the dimensionless buoyancy and wind shear parameters,
respectively. Both :math:`G_H` and :math:`G_M`, and their equilibrium
values :math:`G_{H_e}` and :math:`G_{M_e}`, are functions of the
Richardson number:

.. math::
   \textrm{RI} = \frac{ \frac{g}{\theta_v} \pp{\theta_v}{z} }{ (\pp{u}{z})^2 + (\pp{v}{z})^2 }
   =  \frac{c_p \pp{\theta_v}{z} \pp{P^ \kappa}{z} }{ (\pp{u}{z})^2 + (\pp{v}{z})^2 }

Negative values indicate unstable buoyancy and shear, small positive
values (<0.2) indicate dominantly unstable shear, and large
positive values indicate dominantly stable stratification.

Turbulent eddy diffusion coefficients of momentum, heat and moisture in
the surface layer, which corresponds to the lowest GCM level (see *—
missing table —*), are calculated using stability-dependant functions
based on Monin-Obukhov theory:

.. math:: {K_m} ({\rm surface}) = C_u \times u_* = C_D W_s

and

.. math:: {K_h} ({\rm surface}) =  C_t \times u_* = C_H W_s

where :math:`u_*=C_uW_s` is the surface friction velocity, :math:`C_D`
is termed the surface drag coefficient, :math:`C_H` the heat transfer
coefficient, and :math:`W_s` is the magnitude of the surface layer wind.

:math:`C_u` is the dimensionless exchange coefficient for momentum from
the surface layer similarity functions:

.. math:: {C_u} = \frac{u_* }{ W_s} = \frac{ k }{ \psi_{m} }

where k is the Von Karman constant and :math:`\psi_m` is the surface
layer non-dimensional wind shear given by

.. math:: \psi_{m} = {\int_{\zeta_{0}}^{\zeta} \frac{\phi_{m} }{ \zeta} d \zeta}

Here :math:`\zeta` is the non-dimensional stability parameter, and
:math:`\phi_m` is the similarity function of :math:`\zeta` which
expresses the stability dependance of the momentum gradient. The
functional form of :math:`\phi_m` is specified differently for stable
and unstable layers.

:math:`C_t` is the dimensionless exchange coefficient for heat and
moisture from the surface layer similarity functions:

.. math::

   {C_t} = -\frac{( \overline{w^{\prime}\theta^{\prime}}) }{ u_* \Delta \theta } =
   -\frac{( \overline{w^{\prime}q^{\prime}}) }{ u_* \Delta q } =
   \frac{ k }{ (\psi_{h} + \psi_{g}) }

where :math:`\psi_h` is the surface layer non-dimensional temperature
gradient given by

.. math:: \psi_{h} = {\int_{\zeta_{0}}^{\zeta} \frac{\phi_{h} }{ \zeta} d \zeta}

Here :math:`\phi_h` is the similarity function of :math:`\zeta`, which
expresses the stability dependance of the temperature and moisture
gradients, and is specified differently for stable and unstable layers
according to :cite:`helfschu:95`.

:math:`\psi_g` is the non-dimensional temperature or moisture gradient
in the viscous sublayer, which is the mosstly laminar region between the
surface and the tops of the roughness elements, in which temperature and
moisture gradients can be quite large. Based on :cite:`yagkad:74`:

.. math::

   \psi_{g} = \frac{ 0.55 ({\rm Pr}^{2/3} - 0.2) }{ \nu^{1/2} }
   (h_{0}u_{*} - h_{0_{\rm ref}}u_{*_{\rm ref}})^{1/2}

where Pr is the Prandtl number for air, :math:`\nu` is the molecular
viscosity, :math:`z_{0}` is the surface roughness length, and the
subscript 'ref' refers to a reference value. :math:`h_{0} = 30z_{0}`
with a maximum value over land of 0.01.

The surface roughness length over oceans is is a function of the
surface-stress velocity,

.. math:: {z_0} = c_1u^3_* + c_2u^2_* + c_3u_* + c_4 + \frac{c_5 }{ u_*}

where the constants are chosen to interpolate between the reciprocal
relation of :cite:`kondo:75` for weak winds, and the piecewise linear relation of :cite:`larpond:81` for
moderate to large winds. Roughness lengths over land are specified from
the climatology of :cite:`dorsell:89`.

For an unstable surface layer, the stability functions, chosen to
interpolate between the condition of small values of :math:`\beta` and
the convective limit, are the KEYPS function :cite:`pano:73` for momentum, and its
generalization for heat and moisture:

.. math::

   {\phi_m}^4 - 18 \zeta {\phi_m}^3 = 1 \hspace{1cm} ; \hspace{1cm} 
   {\phi_h}^2 - 18 \zeta {\phi_h}^3 = 1 \hspace{1cm}

The function for heat and moisture assures non-vanishing heat and
moisture fluxes as the wind speed approaches zero.

For a stable surface layer, the stability functions are the
observationally based functions of :cite:`clarke:70`, slightly modified for the momemtum
flux:

.. math::

   {\phi_m} = \frac{ 1 + 5 {{\zeta}_1} }{ 1 + 0.00794 {\zeta}_1
   (1+ 5 {\zeta}_1) } \hspace{1cm} ; \hspace{1cm}
   {\phi_h} = \frac{ 1 + 5 {{\zeta}_1} }{ 1 + 0.00794 {\zeta}
   (1+ 5 {{\zeta}_1}) }

The moisture flux also depends on a specified evapotranspiration
coefficient, set to unity over oceans and dependant on the
climatological ground wetness over land.

Once all the diffusion coefficients are calculated, the diffusion
equations are solved numerically using an implicit backward operator.

Atmospheric Boundary Layer
##########################

The depth of the atmospheric boundary layer (ABL) is diagnosed by the
parameterization as the level at which the turbulent kinetic energy is
reduced to a tenth of its maximum near surface value. The vertical
structure of the ABL is explicitly resolved by the lowest few (3-8)
model layers.

Surface Energy Budget
#####################

The ground temperature equation is solved as part of the turbulence
package using a backward implicit time differencing scheme:

.. math:: C_g\pp{T_g}{t} = R_{\rm sw} - R_{\rm lw} + Q_{\rm ice} - H - LE

where :math:`R_{\rm sw}` is the net surface downward shortwave radiative
flux and :math:`R_{\rm lw}` is the net surface upward longwave radiative
flux.

:math:`H` is the upward sensible heat flux, given by:

.. math::
   {H} = P^{\kappa}\rho c_{p} C_{H} W_s (\theta_{\rm surface} - \theta_{\rm NLAY})
   \hspace{1cm}\text{where}: \hspace{.2cm}C_H = C_u C_t

where :math:`\rho` = the atmospheric density at the surface,
:math:`c_{p}` is the specific heat of air at constant pressure, and
:math:`\theta` represents the potential temperature of the surface and
of the lowest :math:`\sigma`-level, respectively.

The upward latent heat flux, :math:`\textrm{LE}`, is given by

.. math::

   \textrm{LE} =  \rho \beta L C_{H} W_s (q_{\rm surface} - q_{\rm NLAY})
   \hspace{1cm}\text{where}: \hspace{.2cm}C_H = C_u C_t

where :math:`\beta` is the fraction of the potential evapotranspiration
actually evaporated, L is the latent heat of evaporation, and
:math:`q_{\rm surface}` and :math:`q_{\rm NLAY}` are the specific humidity of
the surface and of the lowest :math:`\sigma`-level, respectively.

The heat conduction through sea ice, :math:`Q_{\rm ice}`, is given by

.. math:: {Q_{\rm ice}} = \frac{C_{\rm ti} }{ H_i} (T_i-T_g)

where :math:`C_{\rm ti}` is the thermal conductivity of ice, :math:`H_i` is
the ice thickness, assumed to be 3 m where sea ice
is present, :math:`T_i` is 273 degrees Kelvin, and :math:`T_g` is the
surface temperature of the ice.

:math:`C_g` is the total heat capacity of the ground, obtained by
solving a heat diffusion equation for the penetration of the diurnal
cycle into the ground (Blackadar 1977), and is given by:

.. math::

   C_g = \sqrt{ \frac{\lambda C_s }{ 2\omega} } = \sqrt{(0.386 + 0.536W + 0.15W^2)2\times10^{-3}
   \frac{86400}{2\pi} }

Here, the thermal conductivity, :math:`\lambda`, is equal to
:math:`2\times10^{-3}` :math:`\frac{\text{ly}}{\text{sec}}\frac{\text{cm}}{\text{K}}`,
the angular velocity of the earth, :math:`\omega`, is
written as 86400 sec day\ :sup:`--1` divided by :math:`2 \pi`
radians day\ :sup:`--1`, and the expression for :math:`C_s`, the heat capacity per unit
volume at the surface, is a function of the ground wetness, :math:`W`.

Land Surface Processes:

Surface Type
############

The fizhi package surface Types are designated using the Koster-Suarez
:cite:`ks:91,ks:92` Land Surface Model (LSM) mosaic philosophy which allows multiple
“tiles”, or multiple surface types, in any one grid cell. The
Koster-Suarez LSM surface type classifications are shown in :numref:`tab_phys_pkg_fizhi_surface_type_designation`. The surface types and the percent of the grid cell
occupied by any surface type were derived from the surface
classification of :cite:`deftow:94`, and information about the location of permanent ice
was obtained from the classifications of :cite:`dorsell:89`. The surface type map for a
:math:`1^\circ` grid is shown in :numref:`fig_phys_pkg_fizhi_surftype`. The
determination of the land or sea category of surface type was made from
NCAR’s 10 minute by 10 minute Navy topography dataset, which includes
information about the percentage of water-cover at any point. The data
were averaged to the model’s grid resolutions, and any grid-box whose
averaged water percentage was :math:`\geq 60 \%` was defined as a water
point. The Land-Water designation was further modified subjectively to
ensure sufficient representation from small but isolated land and water
regions.

.. table:: Surface Type Designation
    :name: tab_phys_pkg_fizhi_surface_type_designation

    +--------+-----------------------------+
    | Type   | Vegetation Designation      |
    +========+=============================+
    | 1      | Broadleaf Evergreen Trees   |
    +--------+-----------------------------+
    | 2      | Broadleaf Deciduous Trees   |
    +--------+-----------------------------+
    | 3      | Needleleaf Trees            |
    +--------+-----------------------------+
    | 4      | Ground Cover                |
    +--------+-----------------------------+
    | 5      | Broadleaf Shrubs            |
    +--------+-----------------------------+
    | 6      | Dwarf Trees (Tundra)        |
    +--------+-----------------------------+
    | 7      | Bare Soil                   |
    +--------+-----------------------------+
    | 8      | Desert (Bright)             |
    +--------+-----------------------------+
    | 9      | Glacier                     |
    +--------+-----------------------------+
    | 10     | Desert (Dark)               |
    +--------+-----------------------------+
    | 100    | Ocean                       |
    +--------+-----------------------------+



.. figure:: figs/surftype.*
    :width: 70%
    :align: center
    :alt: surface type combinations
    :name: fig_phys_pkg_fizhi_surftype

    Surface type combinations 



Surface Roughness
#################

The surface roughness length over oceans is computed iteratively with
the wind stress by the surface layer parameterization :cite:`helfschu:95`. It employs an
interpolation between the functions of :cite:`larpond:81` for high winds and of :cite:`kondo:75` for weak
winds.


Albedo
######

The surface albedo computation, described in , employs the “two stream”
approximation used in Sellers’ (1987) Simple Biosphere (SiB) Model which
distinguishes between the direct and diffuse albedos in the visible and
in the near infra-red spectral ranges. The albedos are functions of the
observed leaf area index (a description of the relative orientation of
the leaves to the sun), the greenness fraction, the vegetation type, and
the solar zenith angle. Modifications are made to account for the
presence of snow, and its depth relative to the height of the vegetation
elements.

Gravity Wave Drag
#################

The fizhi package employs the gravity wave drag scheme of :cite:`zhouetal:95`. This scheme
is a modified version of Vernekar et al. (1992), which was based on
Alpert et al. (1988) and Helfand et al. (1987). In this version, the
gravity wave stress at the surface is based on that derived by
Pierrehumbert (1986) and is given by:

.. math:: 
  |\vec{\tau}_{\rm sfc}| = \frac{\rho U^3}{N \ell^*} \left( \frac{F_r^2}{1+F_r^2}\right)


where :math:`F_r = N h /U` is the Froude number, :math:`N` is the *Brunt
- Väisälä* frequency, :math:`U` is the surface wind speed, :math:`h` is
the standard deviation of the sub-grid scale orography, and
:math:`\ell^*` is the wavelength of the monochromatic gravity wave in
the direction of the low-level wind. A modification introduced by Zhou
et al. allows for the momentum flux to escape through the top of the
model, although this effect is small for the current 70-level model. The
subgrid scale standard deviation is defined by :math:`h`, and is not
allowed to exceed 400 m.

The effects of using this scheme within a GCM are shown in :cite:`taksz:96`. Experiments
using the gravity wave drag parameterization yielded significant and
beneficial impacts on both the time-mean flow and the transient
statistics of the a GCM climatology, and have eliminated most of the
worst dynamically driven biases in the a GCM simulation. An examination
of the angular momentum budget during climate runs indicates that the
resulting gravity wave torque is similar to the data-driven torque
produced by a data assimilation which was performed without gravity wave
drag. It was shown that the inclusion of gravity wave drag results in
large changes in both the mean flow and in eddy fluxes. The result is a
more accurate simulation of surface stress (through a reduction in the
surface wind strength), of mountain torque (through a redistribution of
mean sea-level pressure), and of momentum convergence (through a
reduction in the flux of westerly momentum by transient flow eddies).

Boundary Conditions and other Input Data
########################################

Required fields which are not explicitly predicted or diagnosed during
model execution must either be prescribed internally or obtained from
external data sets. In the fizhi package these fields include: sea
surface temperature, sea ice estent, surface geopotential variance,
vegetation index, and the radiation-related background levels of: ozone,
carbon dioxide, and stratospheric moisture.

Boundary condition data sets are available at the model’s resolutions
for either climatological or yearly varying conditions. Any frequency of
boundary condition data can be used in the fizhi package; however, the
current selection of data is summarized in :numref:`tab_phys_pkg_fizhi_inputs`. The
time mean values are interpolated during each model timestep to the
current time.

.. table:: Boundary conditions and other input data used in the fizhi package. Also noted are the current years and frequencies available.
    :name: tab_phys_pkg_fizhi_inputs

    +-----------------------------------------+-----------+-----------------------------+
    | **Fizhi Input Datasets**                                                          |
    +-----------------------------------------+-----------+-----------------------------+
    | Sea Ice Extent                          | monthly   | 1979-current, climatology   |
    +-----------------------------------------+-----------+-----------------------------+
    | Sea Ice Extent                          | weekly    | 1982-current, climatology   |
    +-----------------------------------------+-----------+-----------------------------+
    | Sea Surface Temperature                 | monthly   | 1979-current, climatology   |
    +-----------------------------------------+-----------+-----------------------------+
    | Sea Surface Temperature                 | weekly    | 1982-current, climatology   |
    +-----------------------------------------+-----------+-----------------------------+
    | Zonally Averaged Upper-Level Moisture   | monthly   | climatology                 |
    +-----------------------------------------+-----------+-----------------------------+
    | Zonally Averaged Ozone Concentration    | monthly   | climatology                 |
    +-----------------------------------------+-----------+-----------------------------+


Topography and Topography Variance
##################################

Surface geopotential heights are provided from an averaging of the Navy
10 minute by 10 minute dataset supplied by the National Center for
Atmospheric Research (NCAR) to the model’s grid resolution. The original
topography is first rotated to the proper grid-orientation which is
being run, and then averages the data to the model resolution.

The standard deviation of the subgrid-scale topography is computed by
interpolating the 10 minute data to the model’s resolution and
re-interpolating back to the 10 minute by 10 minute resolution. The
sub-grid scale variance is constructed based on this smoothed dataset.


Upper Level Moisture
####################

The fizhi package uses climatological water vapor data above 100 mb from
the Stratospheric Aerosol and Gas Experiment (SAGE) as input into the
model’s radiation packages. The SAGE data is archived as monthly zonal
means at :math:`5^\circ` latitudinal resolution. The data is
interpolated to the model’s grid location and current time, and blended
with the GCM’s moisture data. Below 300 mb, the model’s moisture data is
used. Above 100 mb, the SAGE data is used. Between 100 and 300 mb, a
linear interpolation (in pressure) is performed using the data from SAGE
and the GCM.


.. _fizhi_diagnostics:

Fizhi Diagnostics
+++++++++++++++++

Fizhi Diagnostic Menu:
    
+--------+----------------------------------+---------+--------------------------------------------------+
| NAME   |  UNITS                           |  LEVELS | DESCRIPTION                                      |
+--------+----------------------------------+---------+--------------------------------------------------+
| UFLUX  |  N m\ :sup:`--2`                 |  1      | Surface U-Wind Stress on the atmosphere          |
+--------+----------------------------------+---------+--------------------------------------------------+
| VFLUX  |  N m\ :sup:`--2`                 |  1      | Surface V-Wind Stress on the atmosphere          |
+--------+----------------------------------+---------+--------------------------------------------------+
| HFLUX  |  W m\ :sup:`--2`                 |  1      | Surface Flux of Sensible Heat                    |
+--------+----------------------------------+---------+--------------------------------------------------+
| EFLUX  |  W m\ :sup:`--2`                 |  1      | Surface Flux of Latent Heat                      |
+--------+----------------------------------+---------+--------------------------------------------------+
| QICE   |  W m\ :sup:`--2`                 |  1      | Heat Conduction through Sea-Ice                  |
+--------+----------------------------------+---------+--------------------------------------------------+
| RADLWG |  W m\ :sup:`--2`                 |  1      | Net upward LW flux at the ground                 |
+--------+----------------------------------+---------+--------------------------------------------------+
| RADSWG |  W m\ :sup:`--2`                 |  1      | Net downward SW flux at the ground               |
+--------+----------------------------------+---------+--------------------------------------------------+
| RI     |  dimensionless                   |  Nrphys | Richardson Number                                |
+--------+----------------------------------+---------+--------------------------------------------------+
| CT     |  dimensionless                   |  1      | Surface Drag coefficient for T and Q             |
+--------+----------------------------------+---------+--------------------------------------------------+
| CU     |  dimensionless                   |  1      | Surface Drag coefficient for U and V             |
+--------+----------------------------------+---------+--------------------------------------------------+
| ET     |  m\ :sup:`2` s\ :sup:`--1`       |  Nrphys | Diffusivity coefficient for T and Q              |
+--------+----------------------------------+---------+--------------------------------------------------+
| EU     |  m\ :sup:`2` s\ :sup:`--1`       |  Nrphys | Diffusivity coefficient for U and V              |
+--------+----------------------------------+---------+--------------------------------------------------+
| TURBU  |  m s\ :sup:`--1` day\ :sup:`--1` |  Nrphys | U-Momentum Changes due to Turbulence             |
+--------+----------------------------------+---------+--------------------------------------------------+
| TURBV  |  m s\ :sup:`--1` day\ :sup:`--1` |  Nrphys | V-Momentum Changes due to Turbulence             |
+--------+----------------------------------+---------+--------------------------------------------------+
| TURBT  |  deg day\ :sup:`--1`             |  Nrphys | Temperature Changes due to Turbulence            |
+--------+----------------------------------+---------+--------------------------------------------------+
| TURBQ  |  g/kg/day                        |  Nrphys | Specific Humidity Changes due to Turbulence      |
+--------+----------------------------------+---------+--------------------------------------------------+
| MOISTT |  deg day\ :sup:`--1`             |  Nrphys | Temperature Changes due to Moist Processes       |
+--------+----------------------------------+---------+--------------------------------------------------+
| MOISTQ |  g/kg/day                        |  Nrphys | Specific Humidity Changes due to Moist Processes |  
+--------+----------------------------------+---------+--------------------------------------------------+
| RADLW  |  deg day\ :sup:`--1`             |  Nrphys | Net Longwave heating rate for each level         |
+--------+----------------------------------+---------+--------------------------------------------------+
| RADSW  |  deg day\ :sup:`--1`             |  Nrphys | Net Shortwave heating rate for each level        |
+--------+----------------------------------+---------+--------------------------------------------------+
| PREACC |  mm/day                          |  1      | Total Precipitation                              |
+--------+----------------------------------+---------+--------------------------------------------------+
| PRECON |  mm/day                          |  1      | Convective Precipitation                         |
+--------+----------------------------------+---------+--------------------------------------------------+
| TUFLUX |  N m\ :sup:`--2`                 |  Nrphys | Turbulent Flux of U-Momentum                     |
+--------+----------------------------------+---------+--------------------------------------------------+
| TVFLUX |  N m\ :sup:`--2`                 |  Nrphys | Turbulent Flux of V-Momentum                     |
+--------+----------------------------------+---------+--------------------------------------------------+
| TTFLUX |  W m\ :sup:`--2`                 |  Nrphys | Turbulent Flux of Sensible Heat                  |
+--------+----------------------------------+---------+--------------------------------------------------+
            

+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| NAME   |  UNITS              |  LEVELS | DESCRIPTION                                                                         |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| TQFLUX | W m\ :sup:`--2`     | Nrphys  | Turbulent Flux of Latent Heat                                                       |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| CN     | dimensionless       | 1       | Neutral Drag Coefficient                                                            |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| WINDS  | m s\ :sup:`--1`     | 1       | Surface Wind Speed                                                                  |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| DTSRF  | deg                 | 1       | Air/Surface virtual temperature difference                                          |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| TG     | deg                 | 1       | Ground temperature                                                                  |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| TS     | deg                 | 1       | Surface air temperature (Adiabatic from lowest model layer)                         |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| DTG    | deg                 | 1       | Ground temperature adjustment                                                       |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| QG     | g kg\ :sup:`--1`    | 1       | Ground specific humidity                                                            |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| QS     | g kg\ :sup:`--1`    | 1       | Saturation surface specific humidity                                                |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| TGRLW  | deg                 | 1       | Instantaneous ground temperature used as input to the Longwave radiation subroutine |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| ST4    | W m\ :sup:`--2`     | 1       | Upward Longwave flux at the ground (:math:`\sigma T^4`)                             |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| OLR    | W m\ :sup:`--2`     | 1       | Net upward Longwave flux at the top of the model                                    |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| OLRCLR | W m\ :sup:`--2`     | 1       | Net upward clearsky Longwave flux at the top of the model                           |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| LWGCLR | W m\ :sup:`--2`     | 1       | Net upward clearsky Longwave flux at the ground                                     |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| LWCLR  | deg day\ :sup:`--1` | Nrphys  | Net clearsky Longwave heating rate for each level                                   |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| TLW    | deg                 | Nrphys  | Instantaneous temperature used as input to the Longwave radiation subroutine        |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| SHLW   | g g\ :sup:`--1`     | Nrphys  | Instantaneous specific humidity used as input to the Longwave radiation subroutine  |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| OZLW   | g g\ :sup:`--1`     | Nrphys  | Instantaneous ozone used as input to the Longwave radiation subroutine              |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| CLMOLW | :math:`0-1`         | Nrphys  | Maximum overlap cloud fraction used in the Longwave radiation subroutine            |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| CLDTOT | :math:`0-1`         | Nrphys  | Total cloud fraction used in the Longwave and Shortwave radiation subroutines       |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| LWGDOWN| W m\ :sup:`--2`     | 1       | Downwelling Longwave radiation at the ground                                        |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| GWDT   | deg day\ :sup:`--1` | Nrphys  | Temperature tendency due to Gravity Wave Drag                                       |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| RADSWT | W m\ :sup:`--2`     | 1       | Incident Shortwave radiation at the top of the atmosphere                           |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| TAUCLD | per 100 mb          | Nrphys  | Counted Cloud Optical Depth (non-dimensional) per 100 mb                            |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+
| TAUCLDC| Number              | Nrphys  | Cloud Optical Depth Counter                                                         |
+--------+---------------------+---------+-------------------------------------------------------------------------------------+

+--------+-----------------+----------+---------------------------------------------------------------+
| NAME   | UNITS           | LEVELS   | Description                                                   |
+--------+-----------------+----------+---------------------------------------------------------------+
| CLDLOW | 0-1             | Nrphys   | Low-Level ( 1000-700 hPa) Cloud Fraction (0-1)                |
+--------+-----------------+----------+---------------------------------------------------------------+
| EVAP   | mm/day          | 1        | Surface evaporation                                           |
+--------+-----------------+----------+---------------------------------------------------------------+
| DPDT   | hPa/day         | 1        | Surface Pressure time-tendency                                |
+--------+-----------------+----------+---------------------------------------------------------------+
| UAVE   | m/sec           | Nrphys   | Average U-Wind                                                |
+--------+-----------------+----------+---------------------------------------------------------------+
| VAVE   | m/sec           | Nrphys   | Average V-Wind                                                |
+--------+-----------------+----------+---------------------------------------------------------------+
| TAVE   | deg             | Nrphys   | Average Temperature                                           |
+--------+-----------------+----------+---------------------------------------------------------------+
| QAVE   | g/kg            | Nrphys   | Average Specific Humidity                                     |
+--------+-----------------+----------+---------------------------------------------------------------+
| OMEGA  | hPa/day         | Nrphys   | Vertical Velocity                                             |
+--------+-----------------+----------+---------------------------------------------------------------+
| DUDT   | m/sec/day       | Nrphys   | Total U-Wind tendency                                         |
+--------+-----------------+----------+---------------------------------------------------------------+
| DVDT   | m/sec/day       | Nrphys   | Total V-Wind tendency                                         |
+--------+-----------------+----------+---------------------------------------------------------------+
| DTDT   | deg/day         | Nrphys   | Total Temperature tendency                                    |
+--------+-----------------+----------+---------------------------------------------------------------+
| DQDT   | g/kg/day        | Nrphys   | Total Specific Humidity tendency                              |
+--------+-----------------+----------+---------------------------------------------------------------+
| VORT   | 10^{-4}/sec     | Nrphys   | Relative Vorticity                                            |
+--------+-----------------+----------+---------------------------------------------------------------+
| DTLS   | deg/day         | Nrphys   | Temperature tendency due to Stratiform Cloud Formation        |
+--------+-----------------+----------+---------------------------------------------------------------+
| DQLS   | g/kg/day        | Nrphys   | Specific Humidity tendency due to Stratiform Cloud Formation  |
+--------+-----------------+----------+---------------------------------------------------------------+
| USTAR  | m/sec           | 1        | Surface USTAR wind                                            |
+--------+-----------------+----------+---------------------------------------------------------------+
| Z0     | m               | 1        | Surface roughness                                             |
+--------+-----------------+----------+---------------------------------------------------------------+
| FRQTRB | 0-1             | Nrphys-1 | Frequency of Turbulence                                       |
+--------+-----------------+----------+---------------------------------------------------------------+
| PBL    | mb              | 1        | Planetary Boundary Layer depth                                |
+--------+-----------------+----------+---------------------------------------------------------------+
| SWCLR  | deg/day         | Nrphys   | Net clearsky Shortwave heating rate for each level            |
+--------+-----------------+----------+---------------------------------------------------------------+
| OSR    | W m\ :sup:`--2` | 1        | Net downward Shortwave flux at the top of the model           |
+--------+-----------------+----------+---------------------------------------------------------------+
| OSRCLR | W m\ :sup:`--2` | 1        | Net downward clearsky Shortwave flux at the top of the model  |
+--------+-----------------+----------+---------------------------------------------------------------+
| CLDMAS | kg / m^2        | Nrphys   | Convective cloud mass flux                                    |
+--------+-----------------+----------+---------------------------------------------------------------+
| UAVE   | m/sec           | Nrphys   | Time-averaged :math:`u`-Wind                                  |
+--------+-----------------+----------+---------------------------------------------------------------+



+--------+-------------------+--------+---------------------------------------------------------------+
| NAME   | UNITS             | LEVELS | DESCRIPTION                                                   |
+--------+-------------------+--------+---------------------------------------------------------------+
| VAVE   | m/sec             | Nrphys | Time-averaged :math:`v`-Wind                                  |
+--------+-------------------+--------+---------------------------------------------------------------+
| TAVE   | deg               | Nrphys | Time-averaged Temperature`                                    |
+--------+-------------------+--------+---------------------------------------------------------------+
| QAVE   | g/g               | Nrphys | Time-averaged Specific Humidity                               |
+--------+-------------------+--------+---------------------------------------------------------------+
| RFT    | deg/day           | Nrphys | Temperature tendency due Rayleigh Friction                    |
+--------+-------------------+--------+---------------------------------------------------------------+
| PS     | mb                | 1      | Surface Pressure                                              |
+--------+-------------------+--------+---------------------------------------------------------------+
| QQAVE  | (m/sec)\ :sup:`2` | Nrphys | Time-averaged Turbulent Kinetic Energy                        |
+--------+-------------------+--------+---------------------------------------------------------------+
| SWGCLR | W m\ :sup:`--2`   | 1      | Net downward clearsky Shortwave flux at the ground            |
+--------+-------------------+--------+---------------------------------------------------------------+
| PAVE   | mb                | 1      | Time-averaged Surface Pressure                                |
+--------+-------------------+--------+---------------------------------------------------------------+
| DIABU  | m/sec/day         | Nrphys | Total Diabatic forcing on :math:`u`-Wind                      |
+--------+-------------------+--------+---------------------------------------------------------------+
| DIABV  | m/sec/day         | Nrphys | Total Diabatic forcing on :math:`v`-Wind                      |
+--------+-------------------+--------+---------------------------------------------------------------+
| DIABT  | deg/day           | Nrphys | Total Diabatic forcing on Temperature                         |
+--------+-------------------+--------+---------------------------------------------------------------+
| DIABQ  | g/kg/day          | Nrphys | Total Diabatic forcing on Specific Humidity                   |
+--------+-------------------+--------+---------------------------------------------------------------+
| RFU    | m/sec/day         | Nrphys | U-Wind tendency due to Rayleigh Friction                      |
+--------+-------------------+--------+---------------------------------------------------------------+
| RFV    | m/sec/day         | Nrphys | V-Wind tendency due to Rayleigh Friction                      |
+--------+-------------------+--------+---------------------------------------------------------------+
| GWDU   | m/sec/day         | Nrphys | U-Wind tendency due to Gravity Wave Drag                      |
+--------+-------------------+--------+---------------------------------------------------------------+
| GWDU   | m/sec/day         | Nrphys | V-Wind tendency due to Gravity Wave Drag                      |
+--------+-------------------+--------+---------------------------------------------------------------+
| GWDUS  | N m\ :sup:`--2`   | 1      | U-Wind Gravity Wave Drag Stress at Surface                    |
+--------+-------------------+--------+---------------------------------------------------------------+
| GWDVS  | N m\ :sup:`--2`   | 1      | V-Wind Gravity Wave Drag Stress at Surface                    |
+--------+-------------------+--------+---------------------------------------------------------------+
| GWDUT  | N m\ :sup:`--2`   | 1      | U-Wind Gravity Wave Drag Stress at Top                        |
+--------+-------------------+--------+---------------------------------------------------------------+
| GWDVT  | N m\ :sup:`--2`   | 1      | V-Wind Gravity Wave Drag Stress at Top                        |
+--------+-------------------+--------+---------------------------------------------------------------+
| LZRAD  | mg/kg             | Nrphys | Estimated Cloud Liquid Water used in Radiation                |
+--------+-------------------+--------+---------------------------------------------------------------+

+--------+-------------------+--------+-----------------------------------------------------+
| NAME   | UNITS             | LEVELS | DESCRIPTION                                         |
+--------+-------------------+--------+-----------------------------------------------------+
| SLP    | mb                | 1      | Time-averaged Sea-level Pressure                    |
+--------+-------------------+--------+-----------------------------------------------------+
| CLDFRC | 0-1               | 1      | Total Cloud Fraction                                |
+--------+-------------------+--------+-----------------------------------------------------+
| TPW    | gm cm\ :sup:`--2` | 1      | Precipitable water                                  |
+--------+-------------------+--------+-----------------------------------------------------+
| U2M    | m/sec             | 1      | U-Wind at 2 meters                                  |
+--------+-------------------+--------+-----------------------------------------------------+
| V2M    | m/sec             | 1      | V-Wind at 2 meters                                  |
+--------+-------------------+--------+-----------------------------------------------------+
| T2M    | deg               | 1      | Temperature at 2 meters                             |
+--------+-------------------+--------+-----------------------------------------------------+
| Q2M    | g/kg              | 1      | Specific Humidity at 2 meters                       |
+--------+-------------------+--------+-----------------------------------------------------+
| U10M   | m/sec             | 1      | U-Wind at 10 meters                                 |
+--------+-------------------+--------+-----------------------------------------------------+
| V10M   | m/sec             | 1      | V-Wind at 10 meters                                 |
+--------+-------------------+--------+-----------------------------------------------------+
| T10M   | deg               | 1      | Temperature at 10 meters                            |
+--------+-------------------+--------+-----------------------------------------------------+
| Q10M   | g/kg              | 1      | Specific Humidity at 10 meters                      |
+--------+-------------------+--------+-----------------------------------------------------+
| DTRAIN | kg m\ :sup:`--2`  | Nrphys | Detrainment Cloud Mass Flux                         |
+--------+-------------------+--------+-----------------------------------------------------+
| QFILL  | g/kg/day          | Nrphys | Filling of negative specific humidity               |
+--------+-------------------+--------+-----------------------------------------------------+
| DTCONV | deg/sec           | Nr     | Temp Change due to Convection                       |
+--------+-------------------+--------+-----------------------------------------------------+
| DQCONV | g/kg/sec          | Nr     | Specific Humidity Change due to Convection          |
+--------+-------------------+--------+-----------------------------------------------------+
| RELHUM | percent           | Nr     | Relative Humidity                                   |
+--------+-------------------+--------+-----------------------------------------------------+
| PRECLS | g/m^2/sec         | 1      | Large Scale Precipitation                           |
+--------+-------------------+--------+-----------------------------------------------------+
| ENPREC | J/g               | 1      | Energy of Precipitation (snow, rain Temp)           |
+--------+-------------------+--------+-----------------------------------------------------+


Fizhi Diagnostic Description
++++++++++++++++++++++++++++

In this section we list and describe the diagnostic quantities available
within the GCM. The diagnostics are listed in the order that they appear
in the Diagnostic Menu, Section [sec:pkg:fizhi:diagnostics]. In all
cases, each diagnostic as currently archived on the output datasets is
time-averaged over its diagnostic output frequency:

.. math:: {\bf DIAGNOSTIC} = \frac{1}{TTOT} \sum_{t=1}^{t=TTOT} diag(t)

where :math:`TTOT = \frac{ {\bf NQDIAG} }{\Delta t}`, **NQDIAG** is the
output frequency of the diagnostic, and :math:`\Delta t` is the timestep
over which the diagnostic is updated.

Surface Zonal Wind Stress on the Atmosphere (:math:`Newton/m^2`)
################################################################

The zonal wind stress is the turbulent flux of zonal momentum from the
surface.

.. math:: {\bf UFLUX} =  - \rho C_D W_s u \hspace{1cm}where: \hspace{.2cm}C_D = C^2_u

where :math:`\rho` = the atmospheric density at the surface,
:math:`C_{D}` is the surface drag coefficient, :math:`C_u` is the
dimensionless surface exchange coefficient for momentum (see diagnostic
number 10), :math:`W_s` is the magnitude of the surface layer wind, and
:math:`u` is the zonal wind in the lowest model layer.

Surface Meridional Wind Stress on the Atmosphere (:math:`Newton/m^2`)
######################################################################

The meridional wind stress is the turbulent flux of meridional
momentum from the surface.

.. math:: {\bf VFLUX} =  - \rho C_D W_s v \hspace{1cm}where: \hspace{.2cm}C_D = C^2_u

where :math:`\rho` = the atmospheric density at the surface,
:math:`C_{D}` is the surface drag coefficient, :math:`C_u` is the
dimensionless surface exchange coefficient for momentum (see diagnostic
number 10), :math:`W_s` is the magnitude of the surface layer wind, and
:math:`v` is the meridional wind in the lowest model layer.

Surface Flux of Sensible Heat (W m\ :sup:`--2`)
################################################

The turbulent flux of sensible heat from the surface to the atmosphere
is a function of the gradient of virtual potential temperature and the
eddy exchange coefficient:

.. math::

   {\bf HFLUX} =  P^{\kappa}\rho c_{p} C_{H} W_s (\theta_{\rm surface} - \theta_{Nrphys})
   \hspace{1cm}where: \hspace{.2cm}C_H = C_u C_t

where :math:`\rho` = the atmospheric density at the surface,
:math:`c_{p}` is the specific heat of air, :math:`C_{H}` is the
dimensionless surface heat transfer coefficient, :math:`W_s` is the
magnitude of the surface layer wind, :math:`C_u` is the dimensionless
surface exchange coefficient for momentum (see diagnostic number 10),
:math:`C_t` is the dimensionless surface exchange coefficient for heat
and moisture (see diagnostic number 9), and :math:`\theta` is the
potential temperature at the surface and at the bottom model level.

Surface Flux of Latent Heat (:math:`Watts/m^2`)
###############################################

The turbulent flux of latent heat from the surface to the atmosphere
is a function of the gradient of moisture, the potential
evapotranspiration fraction and the eddy exchange coefficient:

.. math::

   {\bf EFLUX} =  \rho \beta L C_{H} W_s (q_{\rm surface} - q_{Nrphys})
   \hspace{1cm}where: \hspace{.2cm}C_H = C_u C_t

where :math:`\rho` = the atmospheric density at the surface,
:math:`\beta` is the fraction of the potential evapotranspiration
actually evaporated, L is the latent heat of evaporation, :math:`C_{H}`
is the dimensionless surface heat transfer coefficient, :math:`W_s` is
the magnitude of the surface layer wind, :math:`C_u` is the
dimensionless surface exchange coefficient for momentum (see diagnostic
number 10), :math:`C_t` is the dimensionless surface exchange
coefficient for heat and moisture (see diagnostic number 9), and
:math:`q_{\rm surface}` and :math:`q_{Nrphys}` are the specific humidity at
the surface and at the bottom model level, respectively.

Heat Conduction Through Sea Ice (:math:`Watts/m^2`)
###################################################

Over sea ice there is an additional source of energy at the surface due
to the heat conduction from the relatively warm ocean through the sea
ice. The heat conduction through sea ice represents an additional energy
source term for the ground temperature equation.

.. math:: {\bf QICE} = \frac{C_{ti}}{H_i} (T_i-T_g)

where :math:`C_{ti}` is the thermal conductivity of ice, :math:`H_i` is
the ice thickness, assumed to be :math:`3 \hspace{.1cm} m` where sea ice
is present, :math:`T_i` is 273 degrees Kelvin, and :math:`T_g` is the
temperature of the sea ice.

NOTE: QICE is not available through model version 5.3, but is
available in subsequent versions.


Net upward Longwave Flux at the surface (:math:`Watts/m^2`)
###########################################################

.. math::

   \begin{aligned}
   {\bf RADLWG} & =  & F_{LW,Nrphys+1}^{Net} \\
                & =  & F_{LW,Nrphys+1}^\uparrow - F_{LW,Nrphys+1}^\downarrow\end{aligned}

where Nrphys+1 indicates the lowest model edge-level, or
:math:`p = p_{surf}`. :math:`F_{LW}^\uparrow` is the upward Longwave
flux and :math:`F_{LW}^\downarrow` is the downward Longwave flux.


Net downard shortwave Flux at the surface (:math:`Watts/m^2`)
#############################################################

.. math::

   \begin{aligned}
   {\bf RADSWG} & =  & F_{SW,Nrphys+1}^{Net} \\
                & =  & F_{SW,Nrphys+1}^\downarrow - F_{SW,Nrphys+1}^\uparrow\end{aligned}

where Nrphys+1 indicates the lowest model edge-level, or
:math:`p = p_{surf}`. :math:`F_{SW}^\downarrow` is the downward
Shortwave flux and :math:`F_{SW}^\uparrow` is the upward Shortwave flux.

Richardson number (:math:`dimensionless`)
#########################################

The non-dimensional stability indicator is the ratio of the buoyancy
to the shear:

.. math::

   {\bf RI} = \frac{ \frac{g}{\theta_v} \pp {\theta_v}{z} }{ (\pp{u}{z})^2 + (\pp{v}{z})^2 }
    =  \frac{c_p \pp{\theta_v}{z} \pp{P^ \kappa}{z} }{ (\pp{u}{z})^2 + (\pp{v}{z})^2 }

where we used the hydrostatic equation:

.. math:: {\pp{\Phi}{P^ \kappa}} = c_p \theta_v

Negative values indicate unstable buoyancy **AND** shear, small positive
values (:math:`<0.4`) indicate dominantly unstable shear, and large
positive values indicate dominantly stable stratification.

CT - Surface Exchange Coefficient for Temperature and Moisture (dimensionless)
###############################################################################

The surface exchange coefficient is obtained from the similarity
functions for the stability dependant flux profile relationships:

.. math::

   {\bf CT} = -\frac{( \overline{w^{\prime}\theta^{\prime}} ) }{ u_* \Delta \theta } = 
   -\frac{( \overline{w^{\prime}q^{\prime}} ) }{ u_* \Delta q } = 
   \frac{ k }{ (\psi_{h} + \psi_{g}) }

where :math:`\psi_h` is the surface layer non-dimensional temperature
change and :math:`\psi_g` is the viscous sublayer non-dimensional
temperature or moisture change:

.. math::

   \psi_{h} = \int_{\zeta_{0}}^{\zeta} \frac{\phi_{h} }{ \zeta} d \zeta \hspace{1cm} and 
   \hspace{1cm} \psi_{g} = \frac{ 0.55 (Pr^{2/3} - 0.2) }{ \nu^{1/2} } 
   (h_{0}u_{*} - h_{0_{ref}}u_{*_{ref}})^{1/2}

and: :math:`h_{0} = 30z_{0}` with a maximum value over land of 0.01

:math:`\phi_h` is the similarity function of :math:`\zeta`, which
expresses the stability dependance of the temperature and moisture
gradients, specified differently for stable and unstable layers
according to . k is the Von Karman constant, :math:`\zeta` is the
non-dimensional stability parameter, Pr is the Prandtl number for air,
:math:`\nu` is the molecular viscosity, :math:`z_{0}` is the surface
roughness length, :math:`u_*` is the surface stress velocity (see
diagnostic number 67), and the subscript ref refers to a reference
value.

CU - Surface Exchange Coefficient for Momentum (dimensionless)
##############################################################

The surface exchange coefficient is obtained from the similarity
functions for the stability dependant flux profile relationships:

.. math:: {\bf CU} = \frac{u_* }{ W_s} = \frac{ k }{ \psi_{m} }

where :math:`\psi_m` is the surface layer non-dimensional wind shear:

.. math:: \psi_{m} = {\int_{\zeta_{0}}^{\zeta} \frac{\phi_{m} }{ \zeta} d \zeta}

:math:`\phi_m` is the similarity function of :math:`\zeta`, which
expresses the stability dependance of the temperature and moisture
gradients, specified differently for stable and unstable layers
according to . k is the Von Karman constant, :math:`\zeta` is the
non-dimensional stability parameter, :math:`u_*` is the surface stress
velocity (see diagnostic number 67), and :math:`W_s` is the magnitude of
the surface layer wind.

ET - Diffusivity Coefficient for Temperature and Moisture (m^2/sec) 
###################################################################

In the level 2.5 version of the Mellor-Yamada (1974) hierarchy, the
turbulent heat or moisture flux for the atmosphere above the surface
layer can be expressed as a turbulent diffusion coefficient :math:`K_h`
times the negative of the gradient of potential temperature or moisture.
In the :cite:`helflab:88` adaptation of this closure, :math:`K_h` takes the form:

.. math::

   {\bf ET} = K_h = -\frac{( \overline{w^{\prime}\theta_v^{\prime}}) }{ \pp{\theta_v}{z} } 
    = \left\{ \begin{array}{l@{\quad\mbox{for}\quad}l} q \, \ell \, S_H(G_M,G_H) & \mbox{decaying turbulence}
   \\ \frac{ q^2 }{ q_e } \, \ell \, S_{H}(G_{M_e},G_{H_e}) & \mbox{growing turbulence} \end{array} \right.

where :math:`q` is the turbulent velocity, or
:math:`\sqrt{2*turbulent \hspace{.2cm} kinetic \hspace{.2cm} 
energy}`, :math:`q_e` is the turbulence velocity derived from the more
simple level 2.0 model, which describes equilibrium turbulence,
:math:`\ell` is the master length scale related to the layer depth,
:math:`S_H` is a function of :math:`G_H` and :math:`G_M`, the
dimensionless buoyancy and wind shear parameters, respectively, or a
function of :math:`G_{H_e}` and :math:`G_{M_e}`, the equilibrium
dimensionless buoyancy and wind shear parameters. Both :math:`G_H` and
:math:`G_M`, and their equilibrium values :math:`G_{H_e}` and
:math:`G_{M_e}`, are functions of the Richardson number.

For the detailed equations and derivations of the modified level 2.5
closure scheme, see :cite:`helflab:88`.

In the surface layer, :math:`{\bf {ET}}` is the exchange coefficient
for heat and moisture, in units of :math:`m/sec`, given by:

.. math:: {\bf ET_{Nrphys}} =  C_t * u_* = C_H W_s

where :math:`C_t` is the dimensionless exchange coefficient for heat and
moisture from the surface layer similarity functions (see diagnostic
number 9), :math:`u_*` is the surface friction velocity (see diagnostic
number 67), :math:`C_H` is the heat transfer coefficient, and
:math:`W_s` is the magnitude of the surface layer wind.


EU - Diffusivity Coefficient for Momentum (m^2/sec)
###################################################

In the level 2.5 version of the Mellor-Yamada (1974) hierarchy, the
turbulent heat momentum flux for the atmosphere above the surface layer
can be expressed as a turbulent diffusion coefficient :math:`K_m` times
the negative of the gradient of the u-wind. In the :cite:`helflab:88` adaptation of this
closure, :math:`K_m` takes the form:

.. math::

   {\bf EU} = K_m = -\frac{( \overline{u^{\prime}w^{\prime}} ) }{ \pp{U}{z} }
    = \left\{ \begin{array}{l@{\quad\mbox{for}\quad}l} q \, \ell \, S_M(G_M,G_H) & \mbox{decaying turbulence}
   \\ \frac{ q^2 }{ q_e } \, \ell \, S_{M}(G_{M_e},G_{H_e}) & \mbox{growing turbulence} \end{array} \right.

where :math:`q` is the turbulent velocity, or
:math:`\sqrt{2*turbulent \hspace{.2cm} kinetic \hspace{.2cm}
energy}`, :math:`q_e` is the turbulence velocity derived from the more
simple level 2.0 model, which describes equilibrium turbulence,
:math:`\ell` is the master length scale related to the layer depth,
:math:`S_M` is a function of :math:`G_H` and :math:`G_M`, the
dimensionless buoyancy and wind shear parameters, respectively, or a
function of :math:`G_{H_e}` and :math:`G_{M_e}`, the equilibrium
dimensionless buoyancy and wind shear parameters. Both :math:`G_H` and
:math:`G_M`, and their equilibrium values :math:`G_{H_e}` and
:math:`G_{M_e}`, are functions of the Richardson number.

For the detailed equations and derivations of the modified level 2.5
closure scheme, see :cite:`helflab:88`.

In the surface layer, :math:`{\bf {EU}}` is the exchange coefficient
for momentum, in units of :math:`m/sec`, given by:

.. math:: {\bf EU_{Nrphys}} = C_u * u_* = C_D W_s

where :math:`C_u` is the dimensionless exchange coefficient for momentum
from the surface layer similarity functions (see diagnostic number 10),
:math:`u_*` is the surface friction velocity (see diagnostic number 67),
:math:`C_D` is the surface drag coefficient, and :math:`W_s` is the
magnitude of the surface layer wind.



TURBU - Zonal U-Momentum changes due to Turbulence (m/sec/day) 
##############################################################

The tendency of U-Momentum due to turbulence is written:

.. math::

   {\bf TURBU} = {\pp{u}{t}}_{\rm turb} = {\pp{}{z} }{(- \overline{u^{\prime}w^{\prime}})}
    = {\pp{}{z} }{(K_m \pp{u}{z})}

The Helfand and Labraga level 2.5 scheme models the turbulent flux of
u-momentum in terms of :math:`K_m`, and the equation has the form of a
diffusion equation.

TURBV - Meridional V-Momentum changes due to Turbulence (m/sec/day) 
###################################################################

The tendency of V-Momentum due to turbulence is written:

.. math::

   {\bf TURBV} = {\pp{v}{t}}_{\rm turb} = {\pp{}{z} }{(- \overline{v^{\prime}w^{\prime}})}
    = {\pp{}{z} }{(K_m \pp{v}{z})}

The Helfand and Labraga level 2.5 scheme models the turbulent flux of
v-momentum in terms of :math:`K_m`, and the equation has the form of a
diffusion equation.


TURBT - Temperature changes due to Turbulence (deg/day) 
#######################################################

The tendency of temperature due to turbulence is written:

.. math::

   {\bf TURBT} = {\pp{T}{t}} = P^{\kappa}{\pp{\theta}{t}}_{\rm turb} = 
   P^{\kappa}{\pp{}{z} }{(- \overline{w^{\prime}\theta^{\prime}})}
    = P^{\kappa}{\pp{}{z} }{(K_h \pp{\theta_v}{z})}

The Helfand and Labraga level 2.5 scheme models the turbulent flux of
temperature in terms of :math:`K_h`, and the equation has the form of a
diffusion equation.


TURBQ - Specific Humidity changes due to Turbulence (g/kg/day) 
###############################################################

The tendency of specific humidity due to turbulence is written:

.. math::

   {\bf TURBQ} = {\pp{q}{t}}_{\rm turb} = {\pp{}{z} }{(- \overline{w^{\prime}q^{\prime}})}
    = {\pp{}{z} }{(K_h \pp{q}{z})}

The Helfand and Labraga level 2.5 scheme models the turbulent flux of
temperature in terms of :math:`K_h`, and the equation has the form of a
diffusion equation.


MOISTT - Temperature Changes Due to Moist Processes (deg/day) 
#############################################################

.. math:: {\bf MOISTT} = \left. {\pp{T}{t}}\right|_{c} + \left. {\pp{T}{t}} \right|_{ls}

where:

.. math::

   \left.{\pp{T}{t}}\right|_{c} = R \sum_i \left( \alpha \frac{m_B}{c_p} \Gamma_s \right)_i 
   \hspace{.4cm} and 
   \hspace{.4cm} \left.{\pp{T}{t}}\right|_{ls} = \frac{L}{c_p} (q^*-q)

 and

.. math:: \Gamma_s = g \eta \pp{s}{p}

The subscript :math:`c` refers to convective processes, while the
subscript :math:`ls` refers to large scale precipitation processes, or
supersaturation rain. The summation refers to contributions from each
cloud type called by RAS. The dry static energy is given as :math:`s`,
the convective cloud base mass flux is given as :math:`m_B`, and the
cloud entrainment is given as :math:`\eta`, which are explicitly defined
in :numref:`para_phys_pkg_fizhi_mc`, the description of the convective
parameterization. The fractional adjustment, or relaxation parameter,
for each cloud type is given as :math:`\alpha`, while :math:`R` is the
rain re-evaporation adjustment.

MOISTQ - Specific Humidity Changes Due to Moist Processes (g/kg/day)
####################################################################

.. math:: {\bf MOISTQ} = \left. {\pp{q}{t}}\right|_{c} + \left. {\pp{q}{t}} \right|_{ls}

where:

.. math::

   \left.{\pp{q}{t}}\right|_{c} = R \sum_i \left( \alpha \frac{m_B}{L}(\Gamma_h-\Gamma_s) \right)_i 
   \hspace{.4cm} and 
   \hspace{.4cm} \left.{\pp{q}{t}}\right|_{ls} = (q^*-q)

and

.. math:: \Gamma_s = g \eta \pp{s}{p}\hspace{.4cm} and \hspace{.4cm}\Gamma_h = g \eta \pp{h}{p}

The subscript :math:`c` refers to convective processes, while the
subscript :math:`ls` refers to large scale precipitation processes, or
supersaturation rain. The summation refers to contributions from each
cloud type called by RAS. The dry static energy is given as :math:`s`,
the moist static energy is given as :math:`h`, the convective cloud base
mass flux is given as :math:`m_B`, and the cloud entrainment is given as
:math:`\eta`, which are explicitly defined in :numref:`para_phys_pkg_fizhi_mc`,
the description of the convective parameterization. The fractional
adjustment, or relaxation parameter, for each cloud type is given as
:math:`\alpha`, while :math:`R` is the rain re-evaporation adjustment.


RADLW - Heating Rate due to Longwave Radiation (deg/day)
########################################################

The net longwave heating rate is calculated as the vertical divergence
of the net terrestrial radiative fluxes. Both the clear-sky and
cloudy-sky longwave fluxes are computed within the longwave routine. The
subroutine calculates the clear-sky flux, :math:`F^{clearsky}_{LW}`,
first. For a given cloud fraction, the clear line-of-sight probability
:math:`C(p,p^{\prime})` is computed from the current level pressure
:math:`p` to the model top pressure, :math:`p^{\prime} = p_{top}`, and
the model surface pressure, :math:`p^{\prime} = p_{surf}`, for the
upward and downward radiative fluxes. (see Section
[sec:fizhi:radcloud]). The cloudy-sky flux is then obtained as:

.. math:: F_{LW} = C(p,p') \cdot F^{clearsky}_{LW}

Finally, the net longwave heating rate is calculated as the vertical
divergence of the net terrestrial radiative fluxes:

.. math:: \pp{\rho c_p T}{t} = - \p{z} F_{LW}^{NET}

or

.. math:: {\bf RADLW} = \frac{g}{c_p \pi} \p{\sigma} F_{LW}^{NET}

where :math:`g` is the accelation due to gravity, :math:`c_p` is the
heat capacity of air at constant pressure, and

.. math:: F_{LW}^{NET} = F_{LW}^\uparrow - F_{LW}^\downarrow


RADSW - Heating Rate due to Shortwave Radiation (deg/day) 
#########################################################

The net Shortwave heating rate is calculated as the vertical divergence
of the net solar radiative fluxes. The clear-sky and cloudy-sky
shortwave fluxes are calculated separately. For the clear-sky case, the
shortwave fluxes and heating rates are computed with both CLMO (maximum
overlap cloud fraction) and CLRO (random overlap cloud fraction) set to
zero (see Section [sec:fizhi:radcloud]). The shortwave routine is then
called a second time, for the cloudy-sky case, with the true
time-averaged cloud fractions CLMO and CLRO being used. In all cases, a
normalized incident shortwave flux is used as input at the top of the
atmosphere.

The heating rate due to Shortwave Radiation under cloudy skies is
defined as:

.. math:: \pp{\rho c_p T}{t} = - \p{z} F(cloudy)_{SW}^{NET} \cdot {\rm RADSWT}

or

.. math:: {\bf RADSW} = \frac{g}{c_p \pi} \p{\sigma} F(cloudy)_{SW}^{NET}\cdot {\rm RADSWT}

where :math:`g` is the accelation due to gravity, :math:`c_p` is the
heat capacity of air at constant pressure, RADSWT is the true incident
shortwave radiation at the top of the atmosphere (See Diagnostic #48),
and

.. math:: F(cloudy)_{SW}^{Net} = F(cloudy)_{SW}^\uparrow - F(cloudy)_{SW}^\downarrow


PREACC - Total (Large-scale + Convective) Accumulated Precipition (mm/day) 
###########################################################################

For a change in specific humidity due to moist processes,
:math:`\Delta q_{moist}`, the vertical integral or total precipitable
amount is given by:

.. math::

   {\bf PREACC} = \int_{surf}^{top} \rho \Delta q_{moist} dz = - \int_{surf}^{top} \Delta  q_{moist}
   \frac{dp}{g} = \frac{1}{g} \int_0^1 \Delta q_{moist} dp

A precipitation rate is defined as the vertically integrated moisture
adjustment per Moist Processes time step, scaled to :math:`mm/day`.


PRECON - Convective Precipition (mm/day)
########################################

For a change in specific humidity due to sub-grid scale cumulus
convective processes, :math:`\Delta q_{cum}`, the vertical integral or
total precipitable amount is given by:

.. math::

   {\bf PRECON} = \int_{surf}^{top} \rho \Delta q_{cum} dz = - \int_{surf}^{top} \Delta  q_{cum}
   \frac{dp}{g} = \frac{1}{g} \int_0^1 \Delta q_{cum} dp

A precipitation rate is defined as the vertically integrated moisture
adjustment per Moist Processes time step, scaled to :math:`mm/day`.

TUFLUX - Turbulent Flux of U-Momentum (Newton/m^2)
##################################################

The turbulent flux of u-momentum is calculated for
:math:`diagnostic \hspace{.2cm} purposes 
\hspace{.2cm} only` from the eddy coefficient for momentum:

.. math::

   {\bf TUFLUX} =  {\rho } {(\overline{u^{\prime}w^{\prime}})} =  
   {\rho } {(- K_m \pp{U}{z})}

where :math:`\rho` is the air density, and :math:`K_m` is the eddy
coefficient.

TVFLUX - Turbulent Flux of V-Momentum (Newton/m^2)
###################################################

The turbulent flux of v-momentum is calculated for
:math:`diagnostic \hspace{.2cm} purposes 
\hspace{.2cm} only` from the eddy coefficient for momentum:

.. math::

   {\bf TVFLUX} =  {\rho } {(\overline{v^{\prime}w^{\prime}})} = 
    {\rho } {(- K_m \pp{V}{z})}

where :math:`\rho` is the air density, and :math:`K_m` is the eddy
coefficient.


TTFLUX - Turbulent Flux of Sensible Heat (Watts/m^2) 
####################################################

The turbulent flux of sensible heat is calculated for
:math:`diagnostic \hspace{.2cm} purposes 
\hspace{.2cm} only` from the eddy coefficient for heat and moisture:

.. math::

   {\bf TTFLUX} = c_p {\rho }  
   P^{\kappa}{(\overline{w^{\prime}\theta^{\prime}})}
    = c_p  {\rho } P^{\kappa}{(- K_h \pp{\theta_v}{z})}

where :math:`\rho` is the air density, and :math:`K_h` is the eddy
coefficient.


TQFLUX - Turbulent Flux of Latent Heat (Watts/m^2)
###################################################

The turbulent flux of latent heat is calculated for
:math:`diagnostic \hspace{.2cm} purposes 
\hspace{.2cm} only` from the eddy coefficient for heat and moisture:

.. math::

   {\bf TQFLUX} = {L {\rho } (\overline{w^{\prime}q^{\prime}})} = 
   {L {\rho }(- K_h \pp{q}{z})}

where :math:`\rho` is the air density, and :math:`K_h` is the eddy
coefficient.


CN - Neutral Drag Coefficient (dimensionless)
#############################################

The drag coefficient for momentum obtained by assuming a neutrally
stable surface layer:

.. math:: {\bf CN} = \frac{ k }{ \ln(\frac{h }{z_0}) }

where :math:`k` is the Von Karman constant, :math:`h` is the height of
the surface layer, and :math:`z_0` is the surface roughness.

WINDS - Surface Wind Speed (meter/sec)
######################################

The surface wind speed is calculated for the last internal turbulence
time step:

.. math:: {\bf WINDS} = \sqrt{u_{Nrphys}^2 + v_{Nrphys}^2}

where the subscript :math:`Nrphys` refers to the lowest model level.

The air/surface virtual temperature difference measures the stability of
the surface layer:

.. math:: {\bf DTSRF} = (\theta_{v{Nrphys+1}} - \theta{v_{Nrphys}}) P^{\kappa}_{surf}

where

.. math::

   \theta_{v{Nrphys+1}} = \frac{ T_g }{ P^{\kappa}_{surf} } (1 + .609 q_{Nrphys+1}) \hspace{1cm}
   and \hspace{1cm} q_{Nrphys+1} = q_{Nrphys} + \beta(q^*(T_g,P_s) - q_{Nrphys})

:math:`\beta` is the surface potential evapotranspiration coefficient
(:math:`\beta=1` over oceans), :math:`q^*(T_g,P_s)` is the saturation
specific humidity at the ground temperature and surface pressure, level
:math:`Nrphys` refers to the lowest model level and level
:math:`Nrphys+1` refers to the surface.


TG - Ground Temperature (deg K)
################################

The ground temperature equation is solved as part of the turbulence
package using a backward implicit time differencing scheme:

.. math::

   {\bf TG} \hspace{.1cm} is \hspace{.1cm} obtained \hspace{.1cm} from: \hspace{.1cm}
   C_g\pp{T_g}{t} = R_{sw} - R_{lw} + Q_{\rm ice} - H - LE

where :math:`R_{sw}` is the net surface downward shortwave radiative
flux, :math:`R_{lw}` is the net surface upward longwave radiative flux,
:math:`Q_{\rm ice}` is the heat conduction through sea ice, :math:`H` is the
upward sensible heat flux, :math:`LE` is the upward latent heat flux,
and :math:`C_g` is the total heat capacity of the ground. :math:`C_g` is
obtained by solving a heat diffusion equation for the penetration of the
diurnal cycle into the ground (), and is given by:

.. math::

   C_g = \sqrt{ \frac{\lambda C_s }{ 2 \omega } } = \sqrt{(0.386 + 0.536W + 0.15W^2)2x10^{-3}
   \frac{86400.}{2\pi} }

Here, the thermal conductivity, :math:`\lambda`, is equal to
:math:`2x10^{-3}` :math:`\frac{ly}{sec} 
\frac{cm}{K}`, the angular velocity of the earth, :math:`\omega`, is
written as :math:`86400` :math:`sec/day` divided by :math:`2 \pi`
:math:`radians/
day`, and the expression for :math:`C_s`, the heat capacity per unit
volume at the surface, is a function of the ground wetness, :math:`W`.


TS - Surface Temperature (deg K)
#################################

The surface temperature estimate is made by assuming that the model’s
lowest layer is well-mixed, and therefore that :math:`\theta` is
constant in that layer. The surface temperature is therefore:

.. math:: {\bf TS} = \theta_{Nrphys} P^{\kappa}_{surf}


DTG - Surface Temperature Adjustment (deg K)
############################################

The change in surface temperature from one turbulence time step to the
next, solved using the Ground Temperature Equation (see diagnostic
number 30) is calculated:

.. math:: {\bf DTG} = {T_g}^{n} - {T_g}^{n-1}

where superscript :math:`n` refers to the new, updated time level, and
the superscript :math:`n-1` refers to the value at the previous
turbulence time level.


QG - Ground Specific Humidity (g/kg)
#####################################

The ground specific humidity is obtained by interpolating between the
specific humidity at the lowest model level and the specific humidity of
a saturated ground. The interpolation is performed using the potential
evapotranspiration function:

.. math:: {\bf QG} = q_{Nrphys+1} = q_{Nrphys} + \beta(q^*(T_g,P_s) - q_{Nrphys})

where :math:`\beta` is the surface potential evapotranspiration
coefficient (:math:`\beta=1` over oceans), and :math:`q^*(T_g,P_s)` is
the saturation specific humidity at the ground temperature and surface
pressure.


QS - Saturation Surface Specific Humidity (g/kg)
################################################

The surface saturation specific humidity is the saturation specific
humidity at the ground temprature and surface pressure:

.. math:: {\bf QS} = q^*(T_g,P_s)

TGRLW - Instantaneous ground temperature used as input to the Longwave radiation subroutine (deg)
#################################################################################################

.. math:: {\bf TGRLW}  = T_g(\lambda , \phi ,n)

where :math:`T_g` is the model ground temperature at the current time
step :math:`n`.

ST4 - Upward Longwave flux at the surface (Watts/m^2)
#####################################################

.. math:: {\bf ST4} = \sigma T^4

where :math:`\sigma` is the Stefan-Boltzmann constant and T is the
temperature.


OLR - Net upward Longwave flux at :math:`p=p_{top}` (Watts/m^2)
################################################################

.. math:: {\bf OLR}  =  F_{LW,top}^{NET}

where top indicates the top of the first model layer. In the GCM,
:math:`p_{top}` = 0.0 mb.


OLRCLR - Net upward clearsky Longwave flux at :math:`p=p_{top}` (Watts/m^2)
###########################################################################

.. math:: {\bf OLRCLR}  =  F(clearsky)_{LW,top}^{NET}

where top indicates the top of the first model layer. In the GCM,
:math:`p_{top}` = 0.0 mb.


LWGCLR - Net upward clearsky Longwave flux at the surface (Watts/m^2)
######################################################################

.. math::

   \begin{aligned}
   {\bf LWGCLR} & =  & F(clearsky)_{LW,Nrphys+1}^{Net} \\
                & =  & F(clearsky)_{LW,Nrphys+1}^\uparrow - F(clearsky)_{LW,Nrphys+1}^\downarrow\end{aligned}

where Nrphys+1 indicates the lowest model edge-level, or
:math:`p = p_{surf}`. :math:`F(clearsky)_{LW}^\uparrow` is the upward
clearsky Longwave flux and the :math:`F(clearsky)_{LW}^\downarrow` is
the downward clearsky Longwave flux.


LWCLR - Heating Rate due to Clearsky Longwave Radiation (deg/day)
#################################################################

The net longwave heating rate is calculated as the vertical divergence
of the net terrestrial radiative fluxes. Both the clear-sky and
cloudy-sky longwave fluxes are computed within the longwave routine. The
subroutine calculates the clear-sky flux, :math:`F^{clearsky}_{LW}`,
first. For a given cloud fraction, the clear line-of-sight probability
:math:`C(p,p^{\prime})` is computed from the current level pressure
:math:`p` to the model top pressure, :math:`p^{\prime} = p_{top}`, and
the model surface pressure, :math:`p^{\prime} = p_{surf}`, for the
upward and downward radiative fluxes. (see Section
[sec:fizhi:radcloud]). The cloudy-sky flux is then obtained as:

.. math:: F_{LW} = C(p,p') \cdot F^{clearsky}_{LW}

Thus, **LWCLR** is defined as the net longwave heating rate due to the
vertical divergence of the clear-sky longwave radiative flux:

.. math:: \pp{\rho c_p T}{t}_{clearsky} = - \p{z} F(clearsky)_{LW}^{NET}

or

.. math:: {\bf LWCLR} = \frac{g}{c_p \pi} \p{\sigma} F(clearsky)_{LW}^{NET}

where :math:`g` is the accelation due to gravity, :math:`c_p` is the
heat capacity of air at constant pressure, and

.. math:: F(clearsky)_{LW}^{Net} = F(clearsky)_{LW}^\uparrow - F(clearsky)_{LW}^\downarrow


TLW - Instantaneous temperature used as input to the Longwave radiation subroutine (deg)
########################################################################################

.. math:: {\bf TLW}  = T(\lambda , \phi ,level, n)

where :math:`T` is the model temperature at the current time step
:math:`n`.


SHLW - Instantaneous specific humidity used as input to the Longwave radiation subroutine (kg/kg)
#################################################################################################

.. math:: {\bf SHLW}  = q(\lambda , \phi , level , n)

where :math:`q` is the model specific humidity at the current time step
:math:`n`.


OZLW - Instantaneous ozone used as input to the Longwave radiation subroutine (kg/kg)
#####################################################################################

.. math:: {\bf OZLW}  = {\rm OZ}(\lambda , \phi , level , n)

where :math:`\rm OZ` is the interpolated ozone data set from the
climatological monthly mean zonally averaged ozone data set.


CLMOLW - Maximum Overlap cloud fraction used in LW Radiation (0-1)
##################################################################

**CLMOLW** is the time-averaged maximum overlap cloud fraction that has been
filled by the Relaxed Arakawa/Schubert Convection scheme and will be
used in the Longwave Radiation algorithm. These are convective clouds
whose radiative characteristics are assumed to be correlated in the
vertical. For a complete description of cloud/radiative interactions,
see Section [sec:fizhi:radcloud].

.. math:: {\bf CLMOLW} = CLMO_{RAS,LW}(\lambda, \phi,  level )


CLDTOT - Total cloud fraction used in LW and SW Radiation (0-1)
###############################################################

**CLDTOT** is the time-averaged total cloud fraction that has been
filled by the Relaxed Arakawa/Schubert and Large-scale Convection
schemes and will be used in the Longwave and Shortwave Radiation
packages. For a complete description of cloud/radiative interactions,
see Section [sec:fizhi:radcloud].

.. math:: {\bf CLDTOT} = F_{RAS} + F_{LS}

where :math:`F_{RAS}` is the time-averaged cloud fraction due to
sub-grid scale convection, and :math:`F_{LS}` is the time-averaged cloud
fraction due to precipitating and non-precipitating large-scale moist
processes.


CLMOSW - Maximum Overlap cloud fraction used in SW Radiation (0-1)
##################################################################

**CLMOSW** is the time-averaged maximum overlap cloud fraction that has been
filled by the Relaxed Arakawa/Schubert Convection scheme and will be
used in the Shortwave Radiation algorithm. These are convective clouds
whose radiative characteristics are assumed to be correlated in the
vertical. For a complete description of cloud/radiative interactions,
see Section [sec:fizhi:radcloud].

.. math:: {\bf CLMOSW} = CLMO_{RAS,SW}(\lambda, \phi,  level )


CLROSW - Random Overlap cloud fraction used in SW Radiation (0-1)
#################################################################

**CLROSW** is the time-averaged random overlap cloud fraction that has been
filled by the Relaxed Arakawa/Schubert and Large-scale Convection
schemes and will be used in the Shortwave Radiation algorithm. These are
convective and large-scale clouds whose radiative characteristics are
not assumed to be correlated in the vertical. For a complete description
of cloud/radiative interactions, see Section [sec:fizhi:radcloud].

.. math:: {\bf CLROSW} = CLRO_{RAS,Large Scale,SW}(\lambda, \phi,  level )


RADSWT - Incident Shortwave radiation at the top of the atmosphere (Watts/m^2)
##############################################################################

.. math:: {\bf RADSWT} = {\frac{S_0}{R_a^2}} \cdot cos \phi_z

where :math:`S_0`, is the extra-terrestial solar contant, :math:`R_a` is
the earth-sun distance in Astronomical Units, and :math:`cos \phi_z` is
the cosine of the zenith angle. It should be noted that **RADSWT**, as
well as **OSR** and **OSRCLR**, are calculated at the top of the
atmosphere (p=0 mb). However, the **OLR** and **OLRCLR** diagnostics are
currently calculated at :math:`p= p_{top}` (0.0 mb for the GCM).


EVAP - Surface Evaporation (mm/day)
###################################

The surface evaporation is a function of the gradient of moisture, the
potential evapotranspiration fraction and the eddy exchange coefficient:

.. math:: {\bf EVAP} =  \rho \beta K_{h} (q_{\rm surface} - q_{Nrphys})

where :math:`\rho` = the atmospheric density at the surface,
:math:`\beta` is the fraction of the potential evapotranspiration
actually evaporated (:math:`\beta=1` over oceans), :math:`K_{h}` is the
turbulent eddy exchange coefficient for heat and moisture at the surface
in :math:`m/sec` and :math:`q{surface}` and :math:`q_{Nrphys}` are the
specific humidity at the surface (see diagnostic number 34) and at the
bottom model level, respectively.


DUDT - Total Zonal U-Wind Tendency  (m/sec/day)
###############################################

**DUDT** is the total time-tendency of the Zonal U-Wind due to Hydrodynamic,
Diabatic, and Analysis forcing.

.. math:: {\bf DUDT} = \pp{u}{t}_{Dynamics} + \pp{u}{t}_{Moist} + \pp{u}{t}_{Turbulence} + \pp{u}{t}_{Analysis}


DVDT - Total Zonal V-Wind Tendency  (m/sec/day)
###############################################

**DVDT** is the total time-tendency of the Meridional V-Wind due to
Hydrodynamic, Diabatic, and Analysis forcing.

.. math:: {\bf DVDT} = \pp{v}{t}_{Dynamics} + \pp{v}{t}_{Moist} + \pp{v}{t}_{Turbulence} + \pp{v}{t}_{Analysis}


DTDT - Total Temperature Tendency  (deg/day)
############################################

**DTDT** is the total time-tendency of Temperature due to Hydrodynamic, Diabatic,
and Analysis forcing.

.. math::

   \begin{aligned}
   {\bf DTDT} & = \pp{T}{t}_{Dynamics} + \pp{T}{t}_{Moist Processes} + \pp{T}{t}_{Shortwave Radiation} \\
              & + \pp{T}{t}_{Longwave Radiation} + \pp{T}{t}_{Turbulence} + \pp{T}{t}_{Analysis} \end{aligned}


DQDT - Total Specific Humidity Tendency  (g/kg/day)
###################################################

**DQDT** is the total time-tendency of Specific Humidity due to Hydrodynamic,
Diabatic, and Analysis forcing.

.. math::

   {\bf DQDT} = \pp{q}{t}_{Dynamics} + \pp{q}{t}_{Moist Processes} 
   + \pp{q}{t}_{Turbulence} + \pp{q}{t}_{Analysis}


USTAR -  Surface-Stress Velocity (m/sec)
########################################

The surface stress velocity, or the friction velocity, is the wind speed
at the surface layer top impeded by the surface drag:

.. math::

   {\bf USTAR} = C_uW_s \hspace{1cm}where: \hspace{.2cm} 
   C_u = \frac{k}{\psi_m}

:math:`C_u` is the non-dimensional surface drag coefficient (see
diagnostic number 10), and :math:`W_s` is the surface wind speed (see
diagnostic number 28).


Z0 - Surface Roughness Length (m)
#################################

Over the land surface, the surface roughness length is interpolated to
the local time from the monthly mean data of . Over the ocean, the
roughness length is a function of the surface-stress velocity,
:math:`u_*`.

.. math:: {\bf Z0} = c_1u^3_* + c_2u^2_* + c_3u_* + c_4 + {c_5}{u_*}

where the constants are chosen to interpolate between the reciprocal
relation of for weak winds, and the piecewise linear relation of for
moderate to large winds.


FRQTRB - Frequency of Turbulence (0-1)
######################################

The fraction of time when turbulence is present is defined as the
fraction of time when the turbulent kinetic energy exceeds some minimum
value, defined here to be :math:`0.005 \hspace{.1cm}m^2/sec^2`. When
this criterion is met, a counter is incremented. The fraction over the
averaging interval is reported.


PBL - Planetary Boundary Layer Depth (mb)
#########################################

The depth of the PBL is defined by the turbulence parameterization to be
the depth at which the turbulent kinetic energy reduces to ten percent
of its surface value.

.. math:: {\bf PBL} = P_{PBL} - P_{\rm surface}

where :math:`P_{PBL}` is the pressure in :math:`mb` at which the
turbulent kinetic energy reaches one tenth of its surface value, and
:math:`P_s` is the surface pressure.


SWCLR - Clear sky Heating Rate due to Shortwave Radiation (deg/day)
###################################################################

The net Shortwave heating rate is calculated as the vertical divergence
of the net solar radiative fluxes. The clear-sky and cloudy-sky
shortwave fluxes are calculated separately. For the clear-sky case, the
shortwave fluxes and heating rates are computed with both CLMO (maximum
overlap cloud fraction) and CLRO (random overlap cloud fraction) set to
zero (see Section [sec:fizhi:radcloud]). The shortwave routine is then
called a second time, for the cloudy-sky case, with the true
time-averaged cloud fractions CLMO and CLRO being used. In all cases, a
normalized incident shortwave flux is used as input at the top of the
atmosphere.

The heating rate due to Shortwave Radiation under clear skies is defined
as:

.. math:: \pp{\rho c_p T}{t} = - \p{z} F(clear)_{SW}^{NET} \cdot {\rm RADSWT}

or

.. math:: {\bf SWCLR} = \frac{g}{c_p } \p{p} F(clear)_{SW}^{NET}\cdot {\rm RADSWT}

where :math:`g` is the accelation due to gravity, :math:`c_p` is the
heat capacity of air at constant pressure, RADSWT is the true incident
shortwave radiation at the top of the atmosphere (See Diagnostic #48),
and

.. math:: F(clear)_{SW}^{Net} = F(clear)_{SW}^\uparrow - F(clear)_{SW}^\downarrow


OSR - Net upward Shortwave flux at the top of the model (Watts/m^2)
###################################################################

.. math:: {\bf OSR}  =  F_{SW,top}^{NET}

where top indicates the top of the first model layer used in the
shortwave radiation routine. In the GCM, :math:`p_{SW_{top}}` = 0 mb.


OSRCLR - Net upward clearsky Shortwave flux at the top of the model (Watts/m^2)
###############################################################################

.. math:: {\bf OSRCLR}  =  F(clearsky)_{SW,top}^{NET}

where top indicates the top of the first model layer used in the
shortwave radiation routine. In the GCM, :math:`p_{SW_{top}}` = 0 mb.


CLDMAS - Convective Cloud Mass Flux (kg/m^2) 
############################################

The amount of cloud mass moved per RAS timestep from all convective
clouds is written:

.. math:: {\bf CLDMAS} = \eta m_B

where :math:`\eta` is the entrainment, normalized by the cloud base mass
flux, and :math:`m_B` is the cloud base mass flux. :math:`m_B` and
:math:`\eta` are defined explicitly in :numref:`para_phys_pkg_fizhi_mc`, the
description of the convective parameterization.


UAVE - Time-Averaged Zonal U-Wind (m/sec)
#########################################

The diagnostic **UAVE** is simply the time-averaged Zonal U-Wind over
the **NUAVE** output frequency. This is contrasted to the instantaneous
Zonal U-Wind which is archived on the Prognostic Output data stream.

.. math:: {\bf UAVE} = u(\lambda, \phi, level , t)

Note, **UAVE** is computed and stored on the staggered C-grid.


VAVE - Time-Averaged Meridional V-Wind (m/sec)
##############################################

The diagnostic **VAVE** is simply the time-averaged Meridional V-Wind
over the **NVAVE** output frequency. This is contrasted to the
instantaneous Meridional V-Wind which is archived on the Prognostic
Output data stream.

.. math:: {\bf VAVE} = v(\lambda, \phi, level , t)

Note, **VAVE** is computed and stored on the staggered C-grid.


TAVE - Time-Averaged Temperature (Kelvin)
#########################################

The diagnostic **TAVE** is simply the time-averaged Temperature over
the **NTAVE** output frequency. This is contrasted to the instantaneous
Temperature which is archived on the Prognostic Output data stream.

.. math:: {\bf TAVE} = T(\lambda, \phi, level , t)


QAVE - Time-Averaged Specific Humidity (g/kg)
#############################################

The diagnostic **QAVE** is simply the time-averaged Specific Humidity
over the **NQAVE** output frequency. This is contrasted to the
instantaneous Specific Humidity which is archived on the Prognostic
Output data stream.

.. math:: {\bf QAVE} = q(\lambda, \phi, level , t)


PAVE - Time-Averaged Surface Pressure - PTOP (mb)
#################################################

The diagnostic **PAVE** is simply the time-averaged Surface Pressure -
PTOP over the **NPAVE** output frequency. This is contrasted to the
instantaneous Surface Pressure - PTOP which is archived on the
Prognostic Output data stream.

.. math::

   \begin{aligned}
   {\bf PAVE} & =  & \pi(\lambda, \phi, level , t) \\
              & =  & p_s(\lambda, \phi, level , t) - p_T\end{aligned}

QQAVE - Time-Averaged Turbulent Kinetic Energy (m/sec)^2
########################################################

The diagnostic **QQAVE** is simply the time-averaged prognostic
Turbulent Kinetic Energy produced by the GCM Turbulence parameterization
over the **NQQAVE** output frequency. This is contrasted to the
instantaneous Turbulent Kinetic Energy which is archived on the
Prognostic Output data stream.

.. math:: {\bf QQAVE} = qq(\lambda, \phi, level , t)

Note, **QQAVE** is computed and stored at the “mass-point” locations
on the staggered C-grid.


SWGCLR - Net downward clearsky Shortwave flux at the surface (Watts/m^2)
########################################################################

.. math::

   \begin{aligned}
   {\bf SWGCLR} & =  & F(clearsky)_{SW,Nrphys+1}^{Net} \\
                & =  & F(clearsky)_{SW,Nrphys+1}^\downarrow - F(clearsky)_{SW,Nrphys+1}^\uparrow\end{aligned}

 
where Nrphys+1 indicates the lowest model edge-level, or
:math:`p = p_{surf}`. :math:`F(clearsky){SW}^\downarrow` is the downward
clearsky Shortwave flux and :math:`F(clearsky)_{SW}^\uparrow` is the
upward clearsky Shortwave flux.


DIABU - Total Diabatic Zonal U-Wind Tendency  (m/sec/day)
#########################################################

**DIABU** is the total time-tendency of the Zonal U-Wind due to Diabatic
processes and the Analysis forcing.

.. math:: {\bf DIABU} = \pp{u}{t}_{Moist} + \pp{u}{t}_{Turbulence} + \pp{u}{t}_{Analysis}



DIABV - Total Diabatic Meridional V-Wind Tendency  (m/sec/day)
##############################################################

**DIABV** is the total time-tendency of the Meridional V-Wind due to Diabatic
processes and the Analysis forcing.

.. math:: {\bf DIABV} = \pp{v}{t}_{Moist} + \pp{v}{t}_{Turbulence} + \pp{v}{t}_{Analysis}


DIABT Total Diabatic Temperature Tendency (deg/day)
###################################################

**DIABT** is the total time-tendency of Temperature due to Diabatic processes and
the Analysis forcing.

.. math::

   \begin{aligned}
   {\bf DIABT} & = \pp{T}{t}_{Moist Processes} + \pp{T}{t}_{Shortwave Radiation} \\
              & + \pp{T}{t}_{Longwave Radiation} + \pp{T}{t}_{Turbulence} + \pp{T}{t}_{Analysis} \end{aligned}

If we define the time-tendency of Temperature due to Diabatic
processes as

.. math::

   \begin{aligned}
   \pp{T}{t}_{Diabatic} & = \pp{T}{t}_{Moist Processes} + \pp{T}{t}_{Shortwave Radiation} \\
                        & + \pp{T}{t}_{Longwave Radiation} + \pp{T}{t}_{Turbulence}\end{aligned}

then, since there are no surface pressure changes due to Diabatic
processes, we may write

.. math:: \pp{T}{t}_{Diabatic} = \frac{p^\kappa}{\pi}\pp{\pi \theta}{t}_{Diabatic}

where :math:`\theta = T/p^\kappa`. Thus, **DIABT** may be written as

.. math:: {\bf DIABT} = \frac{p^\kappa}{\pi} \left( \pp{\pi \theta}{t}_{Diabatic} + \pp{\pi \theta}{t}_{Analysis} \right)


DIABQ - Total Diabatic Specific Humidity Tendency (g/kg/day)
############################################################

**DIABQ** is the total time-tendency of Specific Humidity due to Diabatic
processes and the Analysis forcing.

.. math:: {\bf DIABQ} = \pp{q}{t}_{Moist Processes} + \pp{q}{t}_{Turbulence} + \pp{q}{t}_{Analysis}

If we define the time-tendency of Specific Humidity due to Diabatic
processes as

.. math:: \pp{q}{t}_{Diabatic} = \pp{q}{t}_{Moist Processes} + \pp{q}{t}_{Turbulence}

then, since there are no surface pressure changes due to Diabatic
processes, we may write

.. math:: \pp{q}{t}_{Diabatic} = \frac{1}{\pi}\pp{\pi q}{t}_{Diabatic}

 Thus, **DIABQ** may be written as

.. math:: {\bf DIABQ} = \frac{1}{\pi} \left( \pp{\pi q}{t}_{Diabatic} + \pp{\pi q}{t}_{Analysis} \right)


VINTUQ - Vertically Integrated Moisture Flux (m/sec  g/kg)
##########################################################

The vertically integrated moisture flux due to the zonal u-wind is
obtained by integrating :math:`u q` over the depth of the atmosphere at
each model timestep, and dividing by the total mass of the column.

.. math:: {\bf VINTUQ} = \frac{ \int_{surf}^{top} u q \rho dz  } { \int_{surf}^{top} \rho dz  }

Using
:math:`\rho \delta z = -\frac{\delta p}{g} = - \frac{1}{g} \delta p`, we
have

.. math:: {\bf VINTUQ} = { \int_0^1 u q dp  }


VINTVQ - Vertically Integrated Moisture Flux (m/sec g/kg)
#########################################################

The vertically integrated moisture flux due to the meridional v-wind
is obtained by integrating :math:`v q` over the depth of the atmosphere
at each model timestep, and dividing by the total mass of the column.

.. math:: {\bf VINTVQ} = \frac{ \int_{surf}^{top} v q \rho dz  } { \int_{surf}^{top} \rho dz  }

Using
:math:`\rho \delta z = -\frac{\delta p}{g} = - \frac{1}{g} \delta p`, we
have

.. math:: {\bf VINTVQ} = { \int_0^1 v q dp  }


VINTUT - Vertically Integrated Heat Flux (m/sec deg)
####################################################

The vertically integrated heat flux due to the zonal u-wind is
obtained by integrating :math:`u T` over the depth of the atmosphere at
each model timestep, and dividing by the total mass of the column.

.. math:: {\bf VINTUT} = \frac{ \int_{surf}^{top} u T \rho dz  } { \int_{surf}^{top} \rho dz  }

Or,

.. math:: {\bf VINTUT} = { \int_0^1 u T dp  }


VINTVT - Vertically Integrated Heat Flux (m/sec deg)
####################################################

The vertically integrated heat flux due to the meridional v-wind is
obtained by integrating :math:`v T` over the depth of the atmosphere at
each model timestep, and dividing by the total mass of the column.

.. math:: {\bf VINTVT} = \frac{ \int_{surf}^{top} v T \rho dz  } { \int_{surf}^{top} \rho dz  }

Using :math:`\rho \delta z = -\frac{\delta p}{g}`, we have

.. math:: {\bf VINTVT} = { \int_0^1 v T dp  }


CLDFRC - Total 2-Dimensional Cloud Fracton (0-1)
################################################

If we define the time-averaged random and maximum overlapped cloudiness
as CLRO and CLMO respectively, then the probability of clear sky
associated with random overlapped clouds at any level is (1-CLRO) while
the probability of clear sky associated with maximum overlapped clouds
at any level is (1-CLMO). The total clear sky probability is given by
(1-CLRO)\*(1-CLMO), thus the total cloud fraction at each level may be
obtained by 1-(1-CLRO)\*(1-CLMO).

At any given level, we may define the clear line-of-site probability by
appropriately accounting for the maximum and random overlap cloudiness.
The clear line-of-site probability is defined to be equal to the product
of the clear line-of-site probabilities associated with random and
maximum overlap cloudiness. The clear line-of-site probability
:math:`C(p,p^{\prime})` associated with maximum overlap clouds, from the
current pressure :math:`p` to the model top pressure,
:math:`p^{\prime} = p_{top}`, or the model surface pressure,
:math:`p^{\prime} = p_{surf}`, is simply 1.0 minus the largest maximum
overlap cloud value along the line-of-site, ie.

.. math:: 1-MAX_p^{p^{\prime}} \left( CLMO_p \right)

Thus, even in the time-averaged sense it is assumed that the maximum
overlap clouds are correlated in the vertical. The clear line-of-site
probability associated with random overlap clouds is defined to be the
product of the clear sky probabilities at each level along the
line-of-site, ie.

.. math:: \prod_{p}^{p^{\prime}} \left( 1-CLRO_p \right)

The total cloud fraction at a given level associated with a line-
of-site calculation is given by

.. math::

   1-\left( 1-MAX_p^{p^{\prime}} \left[ CLMO_p \right] \right)
       \prod_p^{p^{\prime}} \left( 1-CLRO_p \right)

The 2-dimensional net cloud fraction as seen from the top of the
atmosphere is given by

.. math::

   {\bf CLDFRC} = 1-\left( 1-MAX_{l=l_1}^{Nrphys} \left[ CLMO_l \right] \right)
       \prod_{l=l_1}^{Nrphys} \left( 1-CLRO_l \right)

For a complete description of cloud/radiative interactions, see
Section [sec:fizhi:radcloud].


QINT - Total Precipitable Water (gm/cm^2)
#########################################

The Total Precipitable Water is defined as the vertical integral of the
specific humidity, given by:

.. math::

   \begin{aligned}
   {\bf QINT} & = \int_{surf}^{top} \rho q dz \\
              & = \frac{\pi}{g} \int_0^1 q dp
   \end{aligned}

where we have used the hydrostatic relation
:math:`\rho \delta z = -\frac{\delta p}{g}`.


U2M  Zonal U-Wind at 2 Meter Depth (m/sec)
##########################################

The u-wind at the 2-meter depth is determined from the similarity
theory:

.. math::

   {\bf U2M} = \frac{u_*}{k} \psi_{m_{2m}} \frac{u_{sl}}{W_s} =
   \frac{ \psi_{m_{2m}} }{ \psi_{m_{sl}} }u_{sl}

where :math:`\psi_m(2m)` is the non-dimensional wind shear at two
meters, and the subscript :math:`sl` refers to the height of the top of
the surface layer. If the roughness height is above two meters,
:math:`{\bf U2M}` is undefined.


V2M - Meridional V-Wind at 2 Meter Depth (m/sec)
################################################

The v-wind at the 2-meter depth is a determined from the similarity
theory:

.. math::

   {\bf V2M} = \frac{u_*}{k} \psi_{m_{2m}} \frac{v_{sl}}{W_s} =
   \frac{ \psi_{m_{2m}} }{ \psi_{m_{sl}} }v_{sl}

where :math:`\psi_m(2m)` is the non-dimensional wind shear at two
meters, and the subscript :math:`sl` refers to the height of the top of
the surface layer. If the roughness height is above two meters,
:math:`{\bf V2M}` is undefined.


T2M - Temperature at 2 Meter Depth (deg K)
##########################################

The temperature at the 2-meter depth is a determined from the similarity
theory:

.. math::

   {\bf T2M} = P^{\kappa} (\frac{\theta*}{k} ({\psi_{h_{2m}}+\psi_g}) + \theta_{surf} ) = 
   P^{\kappa}(\theta_{surf} + \frac{ \psi_{h_{2m}}+\psi_g }{ \psi_{h_{sl}}+\psi_g }
   (\theta_{sl} - \theta_{surf}) )

where:

.. math:: \theta_* = - \frac{ (\overline{w^{\prime}\theta^{\prime}}) }{ u_* }

where :math:`\psi_h(2m)` is the non-dimensional temperature gradient
at two meters, :math:`\psi_g` is the non-dimensional temperature
gradient in the viscous sublayer, and the subscript :math:`sl` refers to
the height of the top of the surface layer. If the roughness height is
above two meters, :math:`{\bf T2M}` is undefined.


Q2M - Specific Humidity at 2 Meter Depth (g/kg)
###############################################

The specific humidity at the 2-meter depth is determined from the
similarity theory:

.. math::

   {\bf Q2M} = P^{\kappa} \frac({q_*}{k} ({\psi_{h_{2m}}+\psi_g}) + q_{surf} ) = 
   P^{\kappa}(q_{surf} + \frac{ \psi_{h_{2m}}+\psi_g }{ \psi_{h_{sl}}+\psi_g }
   (q_{sl} - q_{surf}))

where:

.. math:: q_* = - \frac{ (\overline{w^{\prime}q^{\prime}}) }{ u_* }

where :math:`\psi_h(2m)` is the non-dimensional temperature gradient
at two meters, :math:`\psi_g` is the non-dimensional temperature
gradient in the viscous sublayer, and the subscript :math:`sl` refers to
the height of the top of the surface layer. If the roughness height is
above two meters, :math:`{\bf Q2M}` is undefined.


U10M - Zonal U-Wind at 10 Meter Depth (m/sec)
#############################################

The u-wind at the 10-meter depth is an interpolation between the surface
wind and the model lowest level wind using the ratio of the
non-dimensional wind shear at the two levels:

.. math::

   {\bf U10M} = \frac{u_*}{k} \psi_{m_{10m}} \frac{u_{sl}}{W_s} =
   \frac{ \psi_{m_{10m}} }{ \psi_{m_{sl}} }u_{sl}

where :math:`\psi_m(10m)` is the non-dimensional wind shear at ten
meters, and the subscript :math:`sl` refers to the height of the top of
the surface layer.


V10M - Meridional V-Wind at 10 Meter Depth (m/sec)
##################################################

The v-wind at the 10-meter depth is an interpolation between the surface
wind and the model lowest level wind using the ratio of the
non-dimensional wind shear at the two levels:

.. math::

   {\bf V10M} = \frac{u_*}{k} \psi_{m_{10m}} \frac{v_{sl}}{W_s} =
   \frac{ \psi_{m_{10m}} }{ \psi_{m_{sl}} }v_{sl}

where :math:`\psi_m(10m)` is the non-dimensional wind shear at ten
meters, and the subscript :math:`sl` refers to the height of the top of
the surface layer.


T10M - Temperature at 10 Meter Depth (deg K)
############################################

The temperature at the 10-meter depth is an interpolation between the
surface potential temperature and the model lowest level potential
temperature using the ratio of the non-dimensional temperature gradient
at the two levels:

.. math::

   {\bf T10M} = P^{\kappa} (\frac{\theta*}{k} ({\psi_{h_{10m}}+\psi_g}) + \theta_{surf} ) = 
   P^{\kappa}(\theta_{surf} + \frac{\psi_{h_{10m}}+\psi_g}{\psi_{h_{sl}}+\psi_g}
   (\theta_{sl} - \theta_{surf}))

where:

.. math:: \theta_* = - \frac{ (\overline{w^{\prime}\theta^{\prime}}) }{ u_* }

where :math:`\psi_h(10m)` is the non-dimensional temperature gradient
at two meters, :math:`\psi_g` is the non-dimensional temperature
gradient in the viscous sublayer, and the subscript :math:`sl` refers to
the height of the top of the surface layer.


Q10M - Specific Humidity at 10 Meter Depth (g/kg)
#################################################

The specific humidity at the 10-meter depth is an interpolation between
the surface specific humidity and the model lowest level specific
humidity using the ratio of the non-dimensional temperature gradient at
the two levels:

.. math::

   {\bf Q10M} = P^{\kappa} (\frac{q_*}{k} ({\psi_{h_{10m}}+\psi_g}) + q_{surf} ) = 
   P^{\kappa}(q_{surf} + \frac{\psi_{h_{10m}}+\psi_g}{\psi_{h_{sl}}+\psi_g}
   (q_{sl} - q_{surf}))

where:

.. math:: q_* =  - \frac{ (\overline{w^{\prime}q^{\prime}}) }{ u_* }

where :math:`\psi_h(10m)` is the non-dimensional temperature gradient
at two meters, :math:`\psi_g` is the non-dimensional temperature
gradient in the viscous sublayer, and the subscript :math:`sl` refers to
the height of the top of the surface layer.


DTRAIN - Cloud Detrainment Mass Flux (kg/m^2)
#############################################

The amount of cloud mass moved per RAS timestep at the cloud
detrainment level is written:

.. math:: {\bf DTRAIN} = \eta_{r_D}m_B

where :math:`r_D` is the detrainment level, :math:`m_B` is the cloud
base mass flux, and :math:`\eta` is the entrainment, defined in :numref:`para_phys_pkg_fizhi_mc`.


QFILL - Filling of negative Specific Humidity (g/kg/day)
########################################################

Due to computational errors associated with the numerical scheme used
for the advection of moisture, negative values of specific humidity may
be generated. The specific humidity is checked for negative values after
every dynamics timestep. If negative values have been produced, a
filling algorithm is invoked which redistributes moisture from below.
Diagnostic **QFILL** is equal to the net filling needed to eliminate
negative specific humidity, scaled to a per-day rate:

.. math:: {\bf QFILL} = q^{n+1}_{final} - q^{n+1}_{initial}

where

.. math:: q^{n+1} = (\pi q)^{n+1} / \pi^{n+1}

Key subroutines, parameters and files
+++++++++++++++++++++++++++++++++++++


Dos and don'ts
++++++++++++++


Fizhi Reference
+++++++++++++++


Experiments and tutorials that use fizhi
++++++++++++++++++++++++++++++++++++++++

-  Global atmosphere experiment with realistic SST and topography in
   fizhi-cs-32x32x10 verification directory.

-  Global atmosphere aqua planet experiment in fizhi-cs-aqualev20
   verification directory.


