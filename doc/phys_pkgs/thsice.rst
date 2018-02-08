.. _sub_phys_pkg_thsice:

THSICE: The Thermodynamic Sea Ice Package
-----------------------------------------


**Important note:** This document has been written by Stephanie
Dutkiewicz and describes an earlier implementation of the sea-ice
package. This needs to be updated to reflect the recent changes (JMC).

This thermodynamic ice model is based on the 3-layer model by Winton
(2000). and the energy-conserving LANL CICE model (Bitz and Lipscomb,
1999). The model considers two equally thick ice layers; the upper layer
has a variable specific heat resulting from brine pockets, the lower
layer has a fixed heat capacity. A zero heat capacity snow layer lies
above the ice. Heat fluxes at the top and bottom surfaces are used to
calculate the change in ice and snow layer thickness. Grid cells of the
ocean model are either fully covered in ice or are open water. There is
a provision to parametrize ice fraction (and leads) in this package.
Modifications are discussed in small font following the subroutine
descriptions.

Key parameters and Routines
+++++++++++++++++++++++++++

The ice model is called from *thermodynamics.F*, subroutine
*ice\_forcing.F* is called in place of *external\_forcing\_surf.F*.

In *ice\_forcing.F*, we calculate the freezing potential of the ocean
model surface layer of water:

.. math:: {\bf frzmlt} = (T_f - SST) \frac{c_{sw} \rho_{sw} \Delta z}{\Delta t}

where :math:`c_{sw}` is seawater heat capacity, :math:`\rho_{sw}` is the
seawater density, :math:`\Delta z` is the ocean model upper layer
thickness and :math:`\Delta t` is the model (tracer) timestep. The
freezing temperature, :math:`T_f=\mu S` is a function of the salinity.

#. Provided there is no ice present and **frzmlt** is less than 0, the surface tendencies of wind, heat and freshwater are calculated as usual (ie. as in *external\_forcing\_surf.F*).

#. If there is ice present in the grid cell we call the main ice model routine *ice\_therm.F* (see below). Output from this routine gives net heat and freshwater flux affecting the top of the ocean.

Subroutine *ice\_forcing.F* uses these values to find the sea surface
tendencies in grid cells. When there is ice present, the surface stress
tendencies are set to zero; the ice model is purely thermodynamic and
the effect of ice motion on the sea-surface is not examined.

Relaxation of surface :math:`T` and :math:`S` is only allowed
equatorward of **relaxlat** (see **DATA.ICE below**), and no relaxation
is allowed under the ice at any latitude.

(Note that there is provision for allowing grid cells to have both
open water and seaice; if **compact** is between  0 and 1)


subroutine ICE_FREEZE
#####################

This routine is called from *thermodynamics.F* after the new temperature
calculation, *calc\_gt.F*, but before *calc\_gs.F*. In *ice\_freeze.F*,
any ocean upper layer grid cell with no ice cover, but with temperature
below freezing, :math:`T_f=\mu S` has ice initialized. We calculate
**frzmlt** from all the grid cells in the water column that have a
temperature less than freezing. In this routine, any water below the
surface that is below freezing is set to :math:`T_f`. A call to
*ice\_start.F* is made if **frzmlt** :math:`>0`, and salinity tendancy
is updated for brine release.


(There is a provision for fractional ice: In the case where the grid cell has less ice coverage than **icemaskmax** we allow *ice_start.F* to be called)


subroutine ICE_START
####################

The energy available from freezing the sea surface is brought into this
routine as **esurp**. The enthalpy of the 2 layers of any new ice is
calculated as:

.. math::

   \begin{aligned}
   q_1 & = & -c_{i}*T_f + L_i \nonumber \\
   q_2 & = & -c_{f}T_{mlt}+ c_{i}(T_{mlt}-T{f}) + L_i(1-\frac{T_{mlt}}{T_f})
   \nonumber\end{aligned}

where :math:`c_f` is specific heat of liquid fresh water, :math:`c_i` is
the specific heat of fresh ice, :math:`L_i` is latent heat of freezing,
:math:`\rho_i` is density of ice and :math:`T_{mlt}` is melting
temperature of ice with salinity of 1. The height of a new layer of ice
is

.. math:: h_{i new} = \frac{{\bf esurp} \Delta t}{qi_{0av}}

where :math:`qi_{0av}=-\frac{\rho_i}{2} (q_1+q_2)`.

The surface skin temperature :math:`T_s` and ice temperatures
:math:`T_1`, :math:`T_2` and the sea surface temperature are set at
:math:`T_f`.

(There is provision for fractional ice: new ice is formed over open
water; the first freezing in the cell must have a height of **himin0**;
this determines the ice fraction **compact**. If there is already ice in
the grid cell, the new ice must have the same height and the new ice
fraction is

.. math:: i_f=(1-\hat{i_f}) \frac{h_{i new}}{h_i}

where :math:`\hat{i_f}` is ice fraction from previous timestep and
:math:`h_i` is current ice height. Snow is redistributed over the new
ice fraction. The ice fraction is not allowed to become larger than
**iceMaskmax** and if the ice height is above **hihig** then freezing
energy comes from the full grid cell, ice growth does not occur under
orginal ice due to freezing water.)


subroutine ICE_THERM
####################

The main subroutine of this package is *ice\_therm.F* where the ice
temperatures are calculated and the changes in ice and snow thicknesses
are determined. Output provides the net heat and fresh water fluxes that
force the top layer of the ocean model.

If the current ice height is less than **himin** then the ice layer is
set to zero and the ocean model upper layer temperature is allowed to
drop lower than its freezing temperature; and atmospheric fluxes are
allowed to effect the grid cell. If the ice height is greater than
**himin** we proceed with the ice model calculation.

We follow the procedure of Winton (1999) – see equations 3 to 21 – to
calculate the surface and internal ice temperatures. The surface
temperature is found from the balance of the flux at the surface
:math:`F_s`, the shortwave heat flux absorbed by the ice, **fswint**,
and the upward conduction of heat through the snow and/or ice,
:math:`F_u`. We linearize :math:`F_s` about the surface temperature,
:math:`\hat{T_s}`, at the previous timestep (where :math:`\hat{ }`
indicates the value at the previous timestep):

.. math::

   F_s (T_s) = F_s(\hat{T_s}) + \frac{\partial F_s(\hat{T_s)}}{\partial T_s}
   (T_s-\hat{T_s})

where,

.. math::

   F_s  =  F_{sensible}+F_{latent}+F_{longwave}^{down}+F_{longwave}^{up}+ (1-
   \alpha) F_{shortwave}

and

.. math::

   \frac{d F_s}{dT} = \frac{d F_{sensible}}{dT} + \frac{d F_{latent}}{dT}
   +\frac{d F_{longwave}^{up}}{dT}.

:math:`F_s` and :math:`\frac{d F_s}{dT}` are currently calculated from
the **BULKF** package described separately, but could also be provided
by an atmospheric model. The surface albedo is calculated from the ice
height and/or surface temperature (see below, *srf\_albedo.F*) and the
shortwave flux absorbed in the ice is

.. math:: {\bf fswint} = (1-e^{\kappa_i h_i})(1-\alpha) F_{shortwave}

where :math:`\kappa_i` is bulk extinction coefficient.

The conductive flux to the surface is

.. math:: F_u=K_{1/2}(T_1-T_s)

where :math:`K_{1/2}` is the effective conductive coupling of the
snow-ice layer between the surface and the mid-point of the upper layer
of ice :math:`
K_{1/2}=\frac{4 K_i K_s}{K_s h_i + 4 K_i h_s}
`. :math:`K_i` and :math:`K_s` are constant thermal conductivities of
seaice and snow.

From the above equations we can develop a system of equations to find
the skin surface temperature, :math:`T_s` and the two ice layer
temperatures (see Winton, 1999, for details). We solve these equations
iteratively until the change in :math:`T_s` is small. When the surface
temperature is greater then the melting temperature of the surface, the
temperatures are recalculated setting :math:`T_s` to 0. The enthalpy of
the ice layers are calculated in order to keep track of the energy in
the ice model. Enthalpy is defined, here, as the energy required to melt
a unit mass of seaice with temperature :math:`T`. For the upper layer
(1) with brine pockets and the lower fresh layer (2):

.. math::

   \begin{aligned}
   q_1 & = & - c_f T_f + c_i (T_f-T)+ L_{i}(1-\frac{T_f}{T})
   \nonumber \\
   q_2 & = & -c_i T+L_i \nonumber\end{aligned}

where :math:`c_f` is specific heat of liquid fresh water, :math:`c_i` is
the specific heat of fresh ice, and :math:`L_i` is latent heat of
melting fresh ice.

From the new ice temperatures, we can calculate the energy flux at the
surface available for melting (if :math:`T_s`\ =0) and the energy at the
ocean-ice interface for either melting or freezing.

.. math::

   \begin{aligned}
   E_{top} &  =  & (F_s- K_{1/2}(T_s-T_1) ) \Delta t
   \nonumber \\
   E_{bot} &= & (\frac{4K_i(T_2-T_f)}{h_i}-F_b) \Delta t
   \nonumber\end{aligned}

where :math:`F_b` is the heat flux at the ice bottom due to the sea
surface temperature variations from freezing. If :math:`T_{sst}` is
above freezing, :math:`F_b=c_{sw} \rho_{sw}
\gamma (T_{sst}-T_f)u^{*}`, :math:`\gamma` is the heat transfer
coefficient and :math:`u^{*}=QQ` is frictional velocity between ice and
water. If :math:`T_{sst}` is below freezing,
:math:`F_b=(T_f - T_{sst})c_f \rho_f \Delta z /\Delta t` and set
:math:`T_{sst}` to :math:`T_f`. We also include the energy from lower
layers that drop below freezing, and set those layers to :math:`T_f`.

If :math:`E_{top}>0` we melt snow from the surface, if all the snow is
melted and there is energy left, we melt the ice. If the ice is all gone
and there is still energy left, we apply the left over energy to heating
the ocean model upper layer (See Winton, 1999, equations 27-29).
Similarly if :math:`E_{bot}>0` we melt ice from the bottom. If all the
ice is melted, the snow is melted (with energy from the ocean model
upper layer if necessary). If :math:`E_{bot}<0` we grow ice at the
bottom

.. math:: \Delta h_i = \frac{-E_{bot}}{(q_{bot} \rho_i)}

where :math:`q_{bot}=-c_{i} T_f + L_i` is the enthalpy of the new ice,
The enthalpy of the second ice layer, :math:`q_2` needs to be modified:

.. math::

   q_2 = \frac{ \hat{h_i}/2 \hat{q_2} + \Delta h_i q_{bot} }
           {\hat{h_i}/{2}+\Delta h_i}

If there is a ice layer and the overlying air temperature is below
0\ :math:`^o`\ C then any precipitation, :math:`P` joins the snow layer:

.. math:: \Delta h_s  = -P \frac{\rho_f}{\rho_s} \Delta t,

:math:`\rho_f` and :math:`\rho_s` are the fresh water and snow
densities. Any evaporation, similarly, removes snow or ice from the
surface. We also calculate the snow age here, in case it is needed for
the surface albedo calculation (see *srf\_albedo.F* below).

For practical reasons we limit the ice growth to **hilim** and snow is
limited to **hslim**. We converts any ice and/or snow above these limits
back to water, maintaining the salt balance. Note however, that heat is
not conserved in this conversion; sea surface temperatures below the ice
are not recalculated.

If the snow/ice interface is below the waterline, snow is converted to
ice (see Winton, 1999, equations 35 and 36). The subroutine
*new\_layers\_winton.F*, described below, repartitions the ice into
equal thickness layers while conserving energy.

The subroutine *ice\_therm.F* now calculates the heat and fresh water
fluxes affecting the ocean model surface layer. The heat flux:

.. math:: q_{net}= {\bf fswocn} - F_{b} - \frac{{\bf esurp}}{\Delta t}

is composed of the shortwave flux that has passed through the ice layer
and is absorbed by the water, **fswocn**\ :math:`=QQ`, the ocean flux to
the ice :math:`F_b`, and the surplus energy left over from the melting,
**esurp**. The fresh water flux is determined from the amount of fresh
water and salt in the ice/snow system before and after the timestep.


(There is a provision for fractional ice: If ice height is above
**hihig** then all energy from freezing at sea surface is used only in
the open water aparts of the cell (ie. :math:`F_b` will only have the
conduction term). The melt energy is partitioned by **frac\_energy**
between melting ice height and ice extent. However, once ice height
drops below **himon0** then all energy melts ice extent.)


subroutine SFC_ALBEDO
#####################

The routine *ice_therm.F* calls this routine to determine the surface
albedo. There are two calculations provided here:

#.  from LANL CICE model
    
    .. math::

       \alpha = f_s \alpha_s + (1-f_s) (\alpha_{i_{min}}
                + (\alpha_{i_{max}}- \alpha_{i_{min}}) (1-e^{-h_i/h_{\alpha}}))

    where :math:`f_s` is 1 if there is snow, 0 if not; the snow albedo,
    :math:`\alpha_s` has two values depending on whether :math:`T_s<0` or
    not; :math:`\alpha_{i_{min}}` and :math:`\alpha_{i_{max}}` are ice
    albedos for thin melting ice, and thick bare ice respectively, and
    :math:`h_{\alpha}` is a scale height.


#.  From GISS model (Hansen et al 1983)

    .. math:: \alpha = \alpha_i e^{-h_s/h_a} + \alpha_s (1-e^{-h_s/h_a})

    where :math:`\alpha_i` is a constant albedo for bare ice, :math:`h_a` is
    a scale height and :math:`\alpha_s` is a variable snow albedo.

    .. math:: \alpha_s = \alpha_1 + \alpha_2 e^{-\lambda_a a_s}

    where :math:`\alpha_1` is a constant, :math:`\alpha_2` depends on
    :math:`T_s`, :math:`a_s` is the snow age, and :math:`\lambda_a` is a
    scale frequency. The snow age is calculated in *ice\_therm.F* and is
    given in equation 41 in Hansen et al (1983).


subroutine NEW_LAYERS_WINTON
############################

The subroutine *new\_layers\_winton.F* repartitions the ice into equal
thickness layers while conserving energy. We pass to this subroutine,
the ice layer enthalpies after melting/growth and the new height of the
ice layers. The ending layer height should be half the sum of the new
ice heights from *ice\_therm.F*. The enthalpies of the ice layers are
adjusted accordingly to maintain total energy in the ice model. If layer
2 height is greater than layer 1 height then layer 2 gives ice to layer
1 and:

.. math:: q_1=f_1 \hat{q_1} + (1-f1) \hat{q_2}

where :math:`f_1` is the fraction of the new to old upper layer heights.
:math:`T_1` will therefore also have changed. Similarly for when ice
layer height 2 is less than layer 1 height, except here we need to to be
careful that the new :math:`T_2` does not fall below the melting
temperature.


Initializing subroutines
########################

*ice_init.F*: Set ice variables to zero, or reads in pickup information from
**pickup.ic** (which was written out in *checkpoint.F*)

*ice_readparms.F*: Reads **data.ice**


Diagnostic subroutines
######################

*ice_ave.F*: Keeps track of means of the ice variables

*ice_diags.F*: Finds averages and writes out diagnostics


Common Blocks
#############

*ICE.h*: Ice Varibles, also **relaxlat** and **startIceModel**

*ICE_DIAGS.h*: matrices for diagnostics: averages of fields from *ice\_diags.F*

*BULKF_ICE_CONSTANTS.h* (in **BULKF** package): all the parameters need by the ice model


Input file DATA.ICE
###################

Here we need to set **StartIceModel**: which is 1 if the model starts
from no ice; and 0 if there is a pickup file with the ice matrices
(**pickup.ic**) which is read in *ice\_init.F* and written out in
*checkpoint.F*. The parameter **relaxlat** defines the latitude poleward
of which there is no relaxing of surface :math:`T` or :math:`S` to
observations. This avoids the relaxation forcing the ice model at these
high latitudes.

(Note: **hicemin** is set to 0 here. If the provision for allowing grid
cells to have both open water and seaice is ever implemented, this would
be greater than 0)

Important Notes
+++++++++++++++

#. heat fluxes have different signs in the ocean and ice models.

#. **StartIceModel** must be changed in **data.ice**: 1 (if starting from no ice), 0 (if using pickup.ic file).


THSICE Diagnostics
++++++++++++++++++

::


    ------------------------------------------------------------------------
    <-Name->|Levs|<-parsing code->|<--  Units   -->|<- Tile (max=80c)
    ------------------------------------------------------------------------
    SI_Fract|  1 |SM P    M1      |0-1             |Sea-Ice fraction  [0-1]
    SI_Thick|  1 |SM PC197M1      |m               |Sea-Ice thickness (area weighted average)
    SI_SnowH|  1 |SM PC197M1      |m               |Snow thickness over Sea-Ice (area weighted)
    SI_Tsrf |  1 |SM  C197M1      |degC            |Surface Temperature over Sea-Ice (area weighted)
    SI_Tice1|  1 |SM  C197M1      |degC            |Sea-Ice Temperature, 1srt layer (area weighted)
    SI_Tice2|  1 |SM  C197M1      |degC            |Sea-Ice Temperature, 2nd  layer (area weighted)
    SI_Qice1|  1 |SM  C198M1      |J/kg            |Sea-Ice enthalpy, 1srt layer (mass weighted)
    SI_Qice2|  1 |SM  C198M1      |J/kg            |Sea-Ice enthalpy, 2nd  layer (mass weighted)
    SIalbedo|  1 |SM PC197M1      |0-1             |Sea-Ice Albedo [0-1] (area weighted average)
    SIsnwAge|  1 |SM P    M1      |s               |snow age over Sea-Ice
    SIsnwPrc|  1 |SM  C197M1      |kg/m^2/s        |snow precip. (+=dw) over Sea-Ice (area weighted)
    SIflxAtm|  1 |SM      M1      |W/m^2           |net heat flux from the Atmosphere (+=dw)
    SIfrwAtm|  1 |SM      M1      |kg/m^2/s        |fresh-water flux to the Atmosphere (+=up)
    SIflx2oc|  1 |SM      M1      |W/m^2           |heat flux out of the ocean (+=up)
    SIfrw2oc|  1 |SM      M1      |m/s             |fresh-water flux out of the ocean (+=up)
    SIsaltFx|  1 |SM      M1      |psu.kg/m^2      |salt flux out of the ocean (+=up)
    SItOcMxL|  1 |SM      M1      |degC            |ocean mixed layer temperature
    SIsOcMxL|  1 |SM P    M1      |psu             |ocean mixed layer salinity


References
++++++++++

Bitz, C.M. and W.H. Lipscombe, 1999: An Energy-Conserving Thermodynamic Model of Sea Ice. *Journal of Geophysical Research*, 104, 15,669 – 15,677.

Hansen, J., G. Russell, D. Rind, P. Stone, A. Lacis, S. Lebedeff, R. Ruedy and L.Travis, 1983: Efficient Three-Dimensional Global Models for Climate Studies: Models I and II. *Monthly Weather Review*, 111, 609 – 662.

Hunke, E.C and W.H. Lipscomb, circa 2001: CICE: the Los Alamos Sea Ice Model Documentation and Software User’s Manual. LACC-98-16v.2. (note: this documentation is no longer available as CICE has progressed to a very different version 3)

Winton, M, 2000: A reformulated Three-layer Sea Ice Model. *Journal of Atmospheric and Ocean Technology*, 17, 525 – 531.


Experiments and tutorials that use thsice
+++++++++++++++++++++++++++++++++++++++++

-  Global atmosphere experiment in aim.5l\_cs verification directory,
   input from input.thsice directory.

-  Global ocean experiment in global\_ocean.cs32x15 verification
   directory, input from input.thsice directory.


