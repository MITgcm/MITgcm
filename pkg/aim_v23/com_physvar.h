C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_physvar.h,v 1.1 2002/11/22 17:16:06 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C--   COMMON /PHYGR1/ : Model variables on gaussian grid (updated in PHYPAR)
C      UG1     = u-wind
C      VG1     = v-wind
C      TG1     = abs. temperature
C      QG1     = specific humidity (g/kg)
C      PHIG1   = geopotential
C      PSLG1   = log. of surface pressure
C      VsurfSq = Square of surface wind speed (grid position = as T,Q)
c     COMMON /PHYGR1/ TG1, QG1, Vsurfsq
c     _RL UG1    (NGP,NLEV)
c     _RL VG1    (NGP,NLEV)
      _RL TG1    (NGP,NLEV)
      _RL QG1    (NGP,NLEV)
c     _RL PHIG1  (NGP,NLEV)
c     _RL PSLG1  (NGP)
      _RL VsurfSq(NGP) 

C--   COMMON /PHYGR2/ : Diagnosed upper-air variables (updated in PHYPAR)
C      SE     = dry static energy <- replaced by Pot.Temp.
C      RH     = relative humidity
C      QSAT   = saturation specific humidity (g/kg)
c     COMMON /PHYGR2/ SE, RH, QSAT
      COMMON /PHYGR2/     RH
      _RL SE   (NGP,NLEV)
      _RL RH   (NGP,NLEV,MAX_NO_THREADS)
      _RL QSAT (NGP,NLEV)
      

C--   COMMON /PHYGR3/ : Diagnosed surface variables (updated in PHYPAR)
C      PSG    = surface pressure (normalized)
C      TS     = surface temperature
C      TSKIN  = skin temperature
C      U0     = near-surface u-wind
C      V0     = near-surface v-wind
C      T0     = near-surface air temperature
C      Q0     = near-surface specific humidity (g/kg)
C      CLOUDC = total cloud cover (fraction)
C      CLTOP  = norm. pressure at cloud top
c     COMMON /PHYGR3/ PSG, TS, TSKIN,
      COMMON /PHYGR3/      TS, TSKIN, T0, Q0, CLOUDC, CLTOP
      _RL PSG   (NGP)
      _RL TS    (NGP,MAX_NO_THREADS)
      _RL TSKIN (NGP,MAX_NO_THREADS)
c     _RL U0(NGP), V0(NGP)
      _RL T0    (NGP,MAX_NO_THREADS)
      _RL Q0    (NGP,MAX_NO_THREADS)
      _RL CLTOP (NGP,MAX_NO_THREADS)
      _RL CLOUDC(NGP,MAX_NO_THREADS)

C--   COMMON /PHYTEN/ : Physical param. tendencies (updated in PHYPAR)
C      TT_CNV  =  temperature tendency due to convection
C      QT_CNV  = sp. humidity tendency due to convection
C      TT_LSC  =  temperature tendency due to large-scale condensation
C      QT_LSC  = sp. humidity tendency due to large-scale condensation
C      TT_RSW  =  temperature tendency due to short-wave radiation
C      TT_RLW  =  temperature tendency due to long-wave radiation
C      UT_PBL  =       u-wind tendency due to PBL and diffusive processes
C      VT_PBL  =       v-wind tendency due to PBL and diffusive processes
C      TT_PBL  =  temperature tendency due to PBL and diffusive processes
C      QT_PBL  = sp. humidity tendency due to PBL and diffusive processes
      COMMON /PHYTEN/ TT_CNV, QT_CNV, TT_LSC, QT_LSC, 
     &        TT_RSW, TT_RLW, TT_PBL, QT_PBL
      _RL TT_CNV (NGP,NLEV,MAX_NO_THREADS)
      _RL QT_CNV (NGP,NLEV,MAX_NO_THREADS)
      _RL TT_LSC (NGP,NLEV,MAX_NO_THREADS)
      _RL QT_LSC (NGP,NLEV,MAX_NO_THREADS)
      _RL TT_RSW (NGP,NLEV,MAX_NO_THREADS)
      _RL TT_RLW (NGP,NLEV,MAX_NO_THREADS)
c     _RL UT_PBL (NGP,NLEV,MAX_NO_THREADS)
c     _RL VT_PBL (NGP,NLEV,MAX_NO_THREADS)
      _RL TT_PBL (NGP,NLEV,MAX_NO_THREADS)
      _RL QT_PBL (NGP,NLEV,MAX_NO_THREADS)

C--   COMMON /FLUXES/ : Surface and upper boundary fluxes (updated in PHYPAR)
C      PRECNV = convective precipitation  [g/(m^2 s)]
C      PRECLS = large-scale precipitation [g/(m^2 s)]
C      CBMF   = cloud-base mass flux 
C      TSR    = top-of-atm. shortwave radiation (downward)
C      SSR    = surface shortwave radiation (downward)
C      SLR    = surface longwave radiation (upward) 
C      OLR    = outgoing longwave radiation (upward)
C      USTR   = u-stress (1: land, 2: sea, 3: weighted average)
C      VSTR   = v-stress (1: land, 2: sea, 3: weighted average)
C      SHF    = sensible heat flux (1: land, 2: sea, 3: w. average)
C      EVAP   = evaporation [g/(m^2 s)] (1: land, 2: sea, 3: w. average)
C      DRAG   = surface Drag term (= Cd*Rho*|V|) (1:land, 2:sea, 3:w.aver)
      COMMON /FLUXES/ PRECNV, PRECLS, CBMF, TSR, SSR, SLR, OLR,
     &                SHF, EVAP, SPEED0, DRAG
      _RL PRECNV (NGP,MAX_NO_THREADS)
      _RL PRECLS (NGP,MAX_NO_THREADS)
      _RL CBMF   (NGP,MAX_NO_THREADS)
      _RL TSR    (NGP,MAX_NO_THREADS)
      _RL SSR    (NGP,MAX_NO_THREADS)
      _RL SLR    (NGP,MAX_NO_THREADS)
      _RL OLR    (NGP,MAX_NO_THREADS)
c     _RL USTR   (NGP,3)
c     _RL VSTR   (NGP,3)
      _RL SHF    (NGP,3,MAX_NO_THREADS)
      _RL EVAP   (NGP,3,MAX_NO_THREADS)
      _RL SPEED0 (NGP,MAX_NO_THREADS)
      _RL DRAG   (NGP,3,MAX_NO_THREADS)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
