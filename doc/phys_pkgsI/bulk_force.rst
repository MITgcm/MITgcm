.. _sub_phys_pkg_bulk_force:

BULK_FORCE: Bulk Formula Package
---------------------------------


author: Stephanie Dutkiewicz


Instead of forcing the model with heat and fresh water flux data, this
package calculates these fluxes using the changing sea surface
temperature. We need to read in some atmospheric data: **air
temperature, air humidity, down shortwave radiation, down longwave
radiation, precipitation, wind speed**. The current setup also reads in
**wind stress**, but this can be changed so that the stresses are
calculated from the wind speed.

The current setup requires that there is the thermodynamic-seaice
package (*pkg/thsice*, also refered below as seaice) is also used. It
would be useful though to have it also setup to run with some very
simple parametrization of the sea ice.

The heat and fresh water fluxes are calculated in *bulkf\_forcing.F*
called from *forward\_step.F*. These fluxes are used over open water,
fluxes over seaice are recalculated in the sea-ice package. Before the
call to *bulkf\_forcing.F* we call *bulkf\_fields\_load.F* to find the
current atmospheric conditions. The only other changes to the model code
come from the initializing and writing diagnostics of these fluxes.


subroutine BULKF_FIELDS_LOAD
++++++++++++++++++++++++++++

Here we find the atmospheric data needed for the bulk formula
calculations. These are read in at periodic intervals and values are
interpolated to the current time. The data file names come from
**data.blk**. The values that can be read in are: air temperature, air
humidity, precipitation, down solar radiation, down long wave radiation,
zonal and meridional wind speeds, total wind speed, net heat flux, net
freshwater forcing, cloud cover, snow fall, zonal and meridional wind
stresses, and SST and SSS used for relaxation terms. Not all these files
are necessary or used. For instance cloud cover and snow fall are not
used in the current bulk formula calculation. If total wind speed is not
supplied, wind speed is calculate from the zonal and meridional
components. If wind stresses are not read in, then the stresses are
calculated from the wind speed. Net heat flux and net freshwater can be
read in and used over open ocean instead of the bulk formula
calculations (but over seaice the bulkf formula is always used). This is
“hardwired” into *bulkf\_forcing* and the “ch” in the variable names
suggests that this is “cheating”. SST and SSS need to be read in if
there is any relaxation used.

subroutine BULKF_FORCING
++++++++++++++++++++++++

In *bulkf\_forcing.F*, we calculate heat and fresh water fluxes (and
wind stress, if necessary) for each grid cell. First we determine if the
grid cell is open water or seaice and this information is carried by
**iceornot**. There is a provision here for a different designation if
there is snow cover (but currently this does not make any difference).
We then call *bulkf\_formula\_lanl.F* which provides values for: up long
wave radiation, latent and sensible heat fluxes, the derivative of these
three with respect to surface temperature, wind stress, evaporation. Net
long wave radiation is calculated from the combination of the down long
wave read in and the up long wave calculated.

We then find the albedo of the surface - with a call to *sfc\_albedo* if
there is sea-ice (see the seaice package for information on the
subroutine). If the grid cell is open ocean the albedo is set as 0.1.
Note that this is a parameter that can be used to tune the results. The
net short wave radiation is then the down shortwave radiation minus the
amount reflected.

If the wind stress needed to be calculated in *bulkf\_formula\_lanl.F*,
it was calculated to grid cell center points, so in *bulkf\_forcing.F*
we regrid to **u** and **v** points. We let the model know if it has
read in stresses or calculated stresses by the switch **readwindstress**
which is can be set in data.blk, and defaults to **.TRUE.**.

We then calculate **Qnet** and **EmPmR** that will be used as the fluxes
over the open ocean. There is a provision for using runoff. If we are
“cheating” and using observed fluxes over the open ocean, then there is
a provision here to use read in **Qnet** and **EmPmR**.

The final call is to calculate averages of the terms found in this
subroutine.

subroutine BULKF_FORMULA_LANL
+++++++++++++++++++++++++++++

This is the main program of the package where the heat fluxes and
freshwater fluxes over ice and open water are calculated. Note that this
subroutine is also called from the seaice package during the iterations
to find the ice surface temperature.

Latent heat (:math:`L`) used in this subroutine depends on the state of
the surface: vaporization for open water, fusion and vaporization for
ice surfaces. Air temperature is converted from Celsius to Kelvin. If
there is no wind speed (:math:`u_s`) given, then the wind speed is
calculated from the zonal and meridional components.

We calculate the virtual temperature:

.. math:: T_o = T_{air} (1+\gamma q_{air})

where :math:`T_{air}` is the air temperature at :math:`h_T`,
:math:`q_{air}` is humidity at :math:`h_q` and :math:`\gamma` is a
constant.

The saturated vapor pressure is calculate (QQ ref):

.. math:: q_{sat} = \frac{a}{p_o} e^{L (b-\frac{c}{T_{srf}})}

where :math:`a,b,c` are constants, :math:`T_{srf}` is surface
temperature and :math:`p_o` is the surface pressure.

The two values crucial for the bulk formula calculations are the
difference between air at sea surface and sea surface temperature:

.. math:: \Delta T = T_{air} - T_{srf} +\alpha h_T

where :math:`\alpha` is adiabatic lapse rate and :math:`h_T` is the
height where the air temperature was taken; and the difference between
the air humidity and the saturated humidity

.. math:: \Delta q = q_{air} - q_{sat}.

We then calculate the turbulent exchange coefficients following Bryan et
al (1996) and the numerical scheme of Hunke and Lipscombe (1998). We
estimate initial values for the exchange coefficients, :math:`c_u`,
:math:`c_T` and :math:`c_q` as

.. math:: \frac{\kappa}{ln(z_{ref}/z_{rou})}

where :math:`\kappa` is the Von Karman constant, :math:`z_{ref}` is a
reference height and :math:`z_{rou}` is a roughness length scale which
could be a function of type of surface, but is here set as a constant.
Turbulent scales are:

.. math::

   \begin{aligned}
   u^* & = & c_u u_s \nonumber\\
   T^* & = & c_T \Delta T \nonumber\\
   q^* & = & c_q \Delta q \nonumber\end{aligned}

We find the “integrated flux profile” for momentum and stability if
there are stable QQ conditions (:math:`\Upsilon>0`) :

.. math:: \psi_m = \psi_s = -5 \Upsilon

and for unstable QQ conditions (:math:`\Upsilon<0`):

.. math::

   \begin{aligned}
   \psi_m & = & 2 ln(0.5(1+\chi)) + ln(0.5(1+\chi^2)) - 2 \tan^{-1} \chi + \pi/2
   \nonumber \\
   \psi_s & = & 2 ln(0.5(1+\chi^2)) \nonumber\end{aligned}

where

.. math::

   \Upsilon = \frac{\kappa g z_{ref}}{u^{*2}} (\frac{T^*}{T_o} + 
   \frac{q^*}{1/\gamma + q_a})

and :math:`\chi=(1-16\Upsilon)^{1/2}`.

The coefficients are updated through 5 iterations as:

.. math::

   \begin{aligned}
   c_u & = & \frac {\hat{c_u}}{1+\hat{c_u}(\lambda - \psi_m)/\kappa} \nonumber \\
   c_T & = & \frac {\hat{c_T}}{1+\hat{c_T}(\lambda - \psi_s)/\kappa} \nonumber \\
   c_q & = & c'_T\end{aligned}

where :math:`\lambda =ln(h_T/z_{ref})`.

We can then find the bulk formula heat fluxes:

Sensible heat flux:

.. math:: Q_s=\rho_{air} c_{p_{air}} u_s c_u c_T \Delta T

Latent heat flux:

.. math:: Q_l=\rho_{air} L u_s c_u c_q \Delta q

Up long wave radiation

.. math:: Q_{lw}^{up}=\epsilon \sigma T_{srf}^4

where :math:`\epsilon` is emissivity (which can be different for open
ocean, ice and snow), :math:`\sigma` is Stefan-Boltzman constant.

We calculate the derivatives of the three above functions with respect
to surface temperature

.. math::

   \begin{aligned}
   \frac{dQ_s}{d_T} & = & \rho_{air} c_{p_{air}} u_s c_u c_T \nonumber \\
   \frac{dQ_l}{d_T} & = & \frac{\rho_{air} L^2 u_s c_u c_q c}{T_{srf}^2} \nonumber \\
   \frac{dQ_{]lw}^{up}}{d_T} & = &  4 \epsilon \sigma t_{srf}^3 \nonumber\end{aligned}

And total derivative :math:`\frac{dQ_o}{dT}= \frac{dQ_s}{dT} +
\frac{dQ_l}{dT} + \frac{dQ_{lw}^{up}}{dT}`.

If we do not read in the wind stress, it is calculated here.

Initializing subroutines
++++++++++++++++++++++++

:code:`bulkf_init.F`: Set bulkf variables to zero.

:code:`bulkf_readparms.F`: Reads **data.blk**


Diagnostic subroutines
++++++++++++++++++++++

:code:`bulkf_ave.F`: Keeps track of means of the bulkf variables

:code:`bulkf_diags.F`: Finds averages and writes out diagnostics


Common Blocks
+++++++++++++

:code:`BULKF.h`: BULKF Variables, data file names, and logicals **readwindstress** and
**readsurface**

:code:`BULKF_DIAGS.h`: matrices for diagnostics: averages of fields from *bulkf_diags.F*

:code:`BULKF_ICE_CONSTANTS.h`: all the parameters needed by the ice model and in the bulkf formula
calculations.


Input file DATA.ICE
+++++++++++++++++++

We read in the file names of atmospheric data used in the bulk formula
calculations. Here we can also set the logicals: **readwindstress** if
we read in the wind stress rather than calculate it from the wind speed;
and **readsurface** to read in the surface temperature and salinity if
these will be used as part of a relaxing term.

Important Notes
+++++++++++++++

#. heat fluxes have different signs in the ocean and ice models.

#. **StartIceModel** must be changed in **data.ice**: 1 (if starting from no ice), 0 (if using pickup.ic file).


References
++++++++++

Bryan F.O., B.G Kauffman, W.G. Large, P.R. Gent, 1996: The NCAR CSM flux
coupler. Technical note TN-425+STR, NCAR.

Hunke, E.C and W.H. Lipscomb, circa 2001: CICE: the Los Alamos Sea Ice
Model Documentation and Software User’s Manual. LACC-98-16v.2.
(note: this documentation is no longer available as CICE has
progressed to a very different version 3)


Experiments and tutorials that use bulk\_force
++++++++++++++++++++++++++++++++++++++++++++++

-  Global ocean experiment in global\_ocean.cs32x15 verification
   directory, input from input.thsice directory.


