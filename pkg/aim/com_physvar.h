C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_physvar.h,v 1.3 2001/05/29 19:28:53 cnh Exp $
C $Name:  $

C--
C--   /PHYGR1/ : Model variables on gaussian grid
C--    UG1     = u-wind
C--    VG1     = v-wind
C--    TG1     = abs. temperature
C--    QG1     = specific humidity (g/kg)
C--    PHIG1   = geopotential
C--    PSLG1   = log. of surface pressure
C--    Vsurfsq = Square of surface wind speed (grid position = as T,Q)

      COMMON /PHYGR1/ UG1(NGP,NLEV,MAX_NO_THREADS), 
     &                VG1(NGP,NLEV,MAX_NO_THREADS), 
     &                TG1(NGP,NLEV,MAX_NO_THREADS),
     &                QG1(NGP,NLEV,MAX_NO_THREADS), 
     &                PHIG1(NGP,NLEV,MAX_NO_THREADS), 
     &                PSLG1(NGP,MAX_NO_THREADS),
     &                Vsurfsq(NGP,MAX_NO_THREADS) 

C--   
C--   /PHYGR2/ : Diagnosed upper-air variables
C--    SE     = dry static energy
C--    RH     = relative humidity
C--    QSAT   = saturation specific humidity (g/kg)

      COMMON /PHYGR2/ 
     &                SE(NGP,NLEV,MAX_NO_THREADS), 
     &                RH(NGP,NLEV,MAX_NO_THREADS), 
     &                QSAT(NGP,NLEV,MAX_NO_THREADS)

C--
C--   /PHYGR3/ : Diagnosed surface variables 
C--    PSG    = surface pressure
C--    TS     = surface temperature
C--    TSKIN  = skin temperature (not yet implemented)
C--    CLOUDC = total cloud cover (fraction)
C --   PNLEVW = Surface pressure for the computation of the surface fluxes
C
      COMMON /PHYGR3/ PSG(NGP,MAX_NO_THREADS), 
     &                TS(NGP,MAX_NO_THREADS), 
     &                TSKIN(NGP,MAX_NO_THREADS), 
     &                CLOUDC(NGP,MAX_NO_THREADS), 
     &                PNLEVW(NGP,MAX_NO_THREADS)

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

      COMMON /PHYTEN/ TT_CNV(NGP,NLEV,MAX_NO_THREADS), 
     &                QT_CNV(NGP,NLEV,MAX_NO_THREADS),
     &                TT_LSC(NGP,NLEV,MAX_NO_THREADS), 
     &                QT_LSC(NGP,NLEV,MAX_NO_THREADS),
     &                TT_RSW(NGP,NLEV,MAX_NO_THREADS), 
     &                TT_RLW(NGP,NLEV,MAX_NO_THREADS),
     &                UT_PBL(NGP,NLEV,MAX_NO_THREADS), 
     &                VT_PBL(NGP,NLEV,MAX_NO_THREADS),
     &                TT_PBL(NGP,NLEV,MAX_NO_THREADS), 
     &                QT_PBL(NGP,NLEV,MAX_NO_THREADS)

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
C--    DRAG   = surface Drag term (= Cd*|V|)

      COMMON /FLUXES/ PRECNV(NGP,MAX_NO_THREADS), 
     &                PRECLS(NGP,MAX_NO_THREADS), 
     &                CBMF(NGP,MAX_NO_THREADS),
     &                TSR(NGP,MAX_NO_THREADS), 
     &                SSR(NGP,MAX_NO_THREADS), 
     &                SLR(NGP,MAX_NO_THREADS), 
     &                OLR(NGP,MAX_NO_THREADS),
     &                USTR(NGP,3,MAX_NO_THREADS), 
     &                VSTR(NGP,3,MAX_NO_THREADS), 
     &                SHF(NGP,3,MAX_NO_THREADS), 
     &                EVAP(NGP,3,MAX_NO_THREADS),
     &                T0(NGP,2,MAX_NO_THREADS), 
     &                Q0(NGP,MAX_NO_THREADS), 
     &                QSAT0(NGP,2,MAX_NO_THREADS), 
     &                SLR_DOWN(NGP,MAX_NO_THREADS),
     &                ST4S(NGP,MAX_NO_THREADS),
     &                SPEED0(NGP,MAX_NO_THREADS),
     &                DRAG(NGP,MAX_NO_THREADS)
