C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_radvar.h,v 1.1 2002/11/22 17:16:06 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C  2nd part of original file "com_radcon.h":
C   contains temp. variables used within radiation scheme
C   and passed as arguments to SOL_OZ, RADSW & RADLW
C   (originally passed through common blocks)

C--   COMMON /RADZON/: Zonally-averaged fields for SW scheme
C               (updated in SOL_OZ)
C      FSOL   = flux of incoming solar radiation
C      OZONE  = flux absorbed by ozone (lower stratos.)
C      OZUPP  = flux absorbed by ozone (upper stratos.)
C      ZENIT  = optical depth ratio (function of solar zenith angle) 
c     COMMON /RADZON/ FSOL, OZONE, OZUPP, ZENIT, STRATZ
      _RL FSOL(NGP), OZONE(NGP), OZUPP(NGP), ZENIT(NGP), STRATZ(NGP)

C--   COMMON /RADFLD/: Transmissivity and blackbody rad.
C               (updated in RADSW/RADLW)
C      TAU2   = transmissivity of atmospheric layers
C      ST4A   = blackbody emission from full and half atmospheric levels
C      STRATC = stratospheric correction term 
C      FLUX   = radiative flux in different spectral bands
c     COMMON /RADFLD/ TAU2, ST4A, STRATC, FLUX
      _RL TAU2(NGP,NLEV,NBAND), ST4A(NGP,NLEV,2)
      _RL STRATC(NGP), FLUX(NGP,NBAND)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
