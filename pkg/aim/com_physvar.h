C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_physvar.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

C--
C--   /PHYGR1/ : Model variables on gaussian grid
C--    UG1    = u-wind
C--    VG1    = v-wind
C--    TG1    = abs. temperature
C--    QG1    = specific humidity (g/kg)
C--    PHIG1  = geopotential
C--    PSLG1  = log. of surface pressure

      COMMON /PHYGR1/ UG1(NGP,NLEV), VG1(NGP,NLEV), TG1(NGP,NLEV),
     *                QG1(NGP,NLEV), PHIG1(NGP,NLEV), PSLG1(NGP)

C--   
C--   /PHYGR2/ : Diagnosed upper-air variables
C--    SE     = dry static energy
C--    RH     = relative humidity
C--    QSAT   = saturation specific humidity (g/kg)

      COMMON /PHYGR2/ SE(NGP,NLEV), RH(NGP,NLEV), QSAT(NGP,NLEV)

C--
C--   /PHYGR3/ : Diagnosed surface variables 
C--    PSG    = surface pressure
C--    TS     = surface temperature
C--    TSKIN  = skin temperature (not yet implemented)
C--    CLOUDC = total cloud cover (fraction)
C --   PNLEVW = Surface pressure for the computation of the surface fluxes
C
      COMMON /PHYGR3/ PSG(NGP), TS(NGP), TSKIN(NGP), 
     *                CLOUDC(NGP), PNLEVW(NGP)

C--
C--   /PHYTEN/ : Physical param. tendencies
C--    TT_CNV  =  temperature tendency due to convection
C--    QT_CNV  = sp. humidity tendency due to convection
C--    TT_LSC  =  temperature tendency due to large-scale condensation
C--    QT_LSC  = sp. humidity tendency due to large-scale condensation
C--    TT_RSW  =  temperature tendency due to short-wave radiation
C--    TT_RLW  =  temperature tendency due to long-wave radiation
C--    UT_PBL  =       u-wind tendency due to PBL and diffusive processes
C--    VT_PBL  =       v-wind tendency due to PBL and diffusive processes
C--    TT_PBL  =  temperature tendency due to PBL and diffusive processes
C--    QT_PBL  = sp. humidity tendency due to PBL and diffusive processes

      COMMON /PHYTEN/ TT_CNV(NGP,NLEV), QT_CNV(NGP,NLEV),
     *                TT_LSC(NGP,NLEV), QT_LSC(NGP,NLEV),
     *                TT_RSW(NGP,NLEV), TT_RLW(NGP,NLEV),
     *                UT_PBL(NGP,NLEV), VT_PBL(NGP,NLEV),
     *                TT_PBL(NGP,NLEV), QT_PBL(NGP,NLEV)

C--
C--   /FLUXES/ : Surface and upper boundary fluxes
C--    PRECNV = convective precipitation  [g/(m^2 s)]
C--    PRECLS = large-scale precipitation [g/(m^2 s)]
C--    CBMF   = cloud-base mass flux 
C--    TSR    = top-of-atm. shortwave radiation (downward)
C--    SSR    = surface shortwave radiation (downward)
C--    SLR    = surface longwave radiation (upward) 
C--    OLR    = outgoing longwave radiation (upward)
C--    USTR   = u-stress (1: land, 2: sea, 3: weighted average)
C--    VSTR   = v-stress (1: land, 2: sea, 3: weighted average)
C--    SHF    = sensible heat flux (1: land, 2: sea, 3: w. average)
C--    EVAP   = evaporation [g/(m^2 s)] (1: land, 2: sea, 3: w. average)

      COMMON /FLUXES/ PRECNV(NGP), PRECLS(NGP), CBMF(NGP),
     *                TSR(NGP), SSR(NGP), SLR(NGP), OLR(NGP),
     *                USTR(NGP,3), VSTR(NGP,3), SHF(NGP,3), EVAP(NGP,3),
     *                T0(NGP,2), Q0(NGP), QSAT0(NGP,2), SLR_DOWN(NGP),
     *                ST4S(NGP), SPEED0(NGP)
