C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_radcon.h,v 1.1 2002/11/22 17:16:06 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C--   COMMON /RADCON/: Radiation constants (initial. in INPHYS)
C      RHCL1  = relative hum. corresponding to cloud cover = 0
C      RHCL2  = relative hum. corresponding to cloud cover = 1
C      QACL1  = specific hum. threshold for cloud cover in the upper troposphere
C      QACL2  = specific hum. threshold for cloud cover in the upper troposphere
C      ALBCL  = cloud albedo (for cloud cover = 1)
C      EPSSW  = fraction of incoming solar radiation absorbed by ozone
C      EPSLW  = fraction of surface LW radiation emitted directly to space
C      EMISFC = longwave surface emissivity
C--:            shortwave absorptivities (for dp = 10^5 Pa) :
C      ABSDRY = abs. of dry air      (visible band)
C      ABSAER = abs. of aerosols     (visible band)
C      ABSWV1 = abs. of water vapour (visible band, for dq = 1 g/kg)
C      ABSWV2 = abs. of water vapour (near IR band, for dq = 1 g/kg)
C      ABSCL1 = abs. of clouds       (visible band, constant term)
C      ABSCL2 = abs. of clouds       (visible band, for dw = 1 g/kg)
C--:            longwave absorptivities (per dp = 10^5 Pa) :
C      ABLWIN = abs. of air in "window" band
C      ABLCO2 = abs. of air in CO2 band
C      ABLWV1 = abs. of water vapour in H2O band 1 (weak),   for dq = 1 g/kg
C      ABLWV2 = abs. of water vapour in H2O band 2 (strong), for dq = 1 g/kg
C      ABLCL1 = abs. of clouds       in "window" band,       constant term
C      ABLCL2 = abs. of clouds       in "window" band,       for dw = 1 g/kg

      COMMON /RADCON/ RHCL1,  RHCL2,  QACL1,  QACL2,  ALBCL,
     &                EPSSW,  EPSLW,  EMISFC, 
     &                ABSDRY, ABSAER, ABSWV1, ABSWV2, ABSCL1, ABSCL2, 
     &                ABLWIN, ABLCO2, ABLWV1, ABLWV2, ABLCL1, ABLCL2
      _RL  RHCL1,  RHCL2,  QACL1,  QACL2,  ALBCL
      _RL  EPSSW,  EPSLW,  EMISFC
      _RL  ABSDRY, ABSAER, ABSWV1, ABSWV2, ABSCL1, ABSCL2
      _RL  ABLWIN, ABLCO2, ABLWV1, ABLWV2, ABLCL1, ABLCL2

C--   COMMON /RADFIX/: Time-invariant fields (initial. in RADSET)
C      FBAND  = energy fraction emitted in each LW band = f(T)
      COMMON /RADFIX/ FBAND
      _RL FBAND(lwTemp1:lwTemp2,0:NBAND)

C--------------
C- Next 2 common blocks have been detach from "com_radcon.h"
C  and put in a sepated file (com_radvar.h)

C--   COMMON /RADZON/: Zonally-averaged fields for SW scheme
C               (updated in SOL_OZ)
C      FSOL   = flux of incoming solar radiation
C      OZONE  = flux absorbed by ozone (lower stratos.)
C      OZUPP  = flux absorbed by ozone (upper stratos.)
C      ZENIT  = optical depth ratio (function of solar zenith angle) 
c     COMMON /RADZON/ FSOL, OZONE, OZUPP, ZENIT, STRATZ
c     _RL FSOL(NGP), OZONE(NGP), OZUPP(NGP), ZENIT(NGP), STRATZ(NGP)

C--   COMMON /RADFLD/: Transmissivity and blackbody rad.
C               (updated in RADSW/RADLW)
C      TAU2   = transmissivity of atmospheric layers
C      ST4A   = blackbody emission from full and half atmospheric levels
C      STRATC = stratospheric correction term 
C      FLUX   = radiative flux in different spectral bands
c     COMMON /RADFLD/ TAU2, ST4A, STRATC, FLUX
c     _RL TAU2(NGP,NLEV,4), ST4A(NGP,NLEV,2)
c     _RL STRATC(NGP), FLUX(NGP,4)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
