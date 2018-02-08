.. _sub_phys_pkg_aim:

Atmospheric Intermediate Physics: AIM
-------------------------------------


Note: The folowing document below describes the ``aim_v23`` package that
is based on the version v23 of the SPEEDY code ().

Key subroutines, parameters and files
#####################################


AIM Diagnostics
###############

::

    ------------------------------------------------------------------------
    <-Name->|Levs|<-parsing code->|<--  Units   -->|<- Tile (max=80c) 
    ------------------------------------------------------------------------
    DIABT   |  5 |SM      ML      |K/s             |Pot. Temp.  Tendency (Mass-Weighted) from Diabatic Processes
    DIABQ   |  5 |SM      ML      |g/kg/s          |Spec.Humid. Tendency (Mass-Weighted) from Diabatic Processes
    RADSW   |  5 |SM      ML      |K/s             |Temperature Tendency due to Shortwave Radiation (TT_RSW)
    RADLW   |  5 |SM      ML      |K/s             |Temperature Tendency due to Longwave  Radiation (TT_RLW)
    DTCONV  |  5 |SM      MR      |K/s             |Temperature Tendency due to Convection (TT_CNV)
    TURBT   |  5 |SM      ML      |K/s             |Temperature Tendency due to Turbulence in PBL (TT_PBL)
    DTLS    |  5 |SM      ML      |K/s             |Temperature Tendency due to Large-scale condens. (TT_LSC)
    DQCONV  |  5 |SM      MR      |g/kg/s          |Spec. Humidity Tendency due to Convection (QT_CNV)
    TURBQ   |  5 |SM      ML      |g/kg/s          |Spec. Humidity Tendency due to Turbulence in PBL (QT_PBL)
    DQLS    |  5 |SM      ML      |g/kg/s          |Spec. Humidity Tendency due to Large-Scale Condens. (QT_LSC)
    TSR     |  1 |SM P    U1      |W/m^2           |Top-of-atm. net Shortwave Radiation (+=dw)
    OLR     |  1 |SM P    U1      |W/m^2           |Outgoing Longwave  Radiation (+=up)
    RADSWG  |  1 |SM P    L1      |W/m^2           |Net Shortwave Radiation at the Ground (+=dw)
    RADLWG  |  1 |SM      L1      |W/m^2           |Net Longwave  Radiation at the Ground (+=up)
    HFLUX   |  1 |SM      L1      |W/m^2           |Sensible Heat Flux (+=up)
    EVAP    |  1 |SM      L1      |g/m^2/s         |Surface Evaporation (g/m2/s)
    PRECON  |  1 |SM P    L1      |g/m^2/s         |Convective  Precipitation (g/m2/s)
    PRECLS  |  1 |SM      M1      |g/m^2/s         |Large Scale Precipitation (g/m2/s)
    CLDFRC  |  1 |SM P    M1      |0-1             |Total Cloud Fraction (0-1)
    CLDPRS  |  1 |SM PC167M1      |0-1             |Cloud Top Pressure (normalized)
    CLDMAS  |  5 |SM P    LL      |kg/m^2/s        |Cloud-base Mass Flux  (kg/m^2/s)
    DRAG    |  5 |SM P    LL      |kg/m^2/s        |Surface Drag Coefficient (kg/m^2/s)
    WINDS   |  1 |SM P    L1      |m/s             |Surface Wind Speed  (m/s)
    TS      |  1 |SM      L1      |K               |near Surface Air Temperature  (K)
    QS      |  1 |SM P    L1      |g/kg            |near Surface Specific Humidity  (g/kg)
    ENPREC  |  1 |SM      M1      |W/m^2           |Energy flux associated with precip. (snow, rain Temp)
    ALBVISDF|  1 |SM P    L1      |0-1             |Surface Albedo (Visible band) (0-1)
    DWNLWG  |  1 |SM P    L1      |W/m^2           |Downward Component of Longwave Flux at the Ground (+=dw)
    SWCLR   |  5 |SM      ML      |K/s             |Clear Sky Temp. Tendency due to Shortwave Radiation
    LWCLR   |  5 |SM      ML      |K/s             |Clear Sky Temp. Tendency due to Longwave  Radiation
    TSRCLR  |  1 |SM P    U1      |W/m^2           |Clear Sky Top-of-atm. net Shortwave Radiation (+=dw)
    OLRCLR  |  1 |SM P    U1      |W/m^2           |Clear Sky Outgoing Longwave  Radiation  (+=up)
    SWGCLR  |  1 |SM P    L1      |W/m^2           |Clear Sky Net Shortwave Radiation at the Ground (+=dw)
    LWGCLR  |  1 |SM      L1      |W/m^2           |Clear Sky Net Longwave  Radiation at the Ground (+=up)
    UFLUX   |  1 |UM   184L1      |N/m^2           |Zonal Wind Surface Stress  (N/m^2)
    VFLUX   |  1 |VM   183L1      |N/m^2           |Meridional Wind Surface Stress  (N/m^2)
    DTSIMPL |  1 |SM P    L1      |K               |Surf. Temp Change after 1 implicit time step

Experiments and tutorials that use aim
######################################

-  Global atmosphere experiment in aim.5l_cs verification directory.


