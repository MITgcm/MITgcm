C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_forcing1.h,v 1.5 2002/09/27 20:01:57 jmc Exp $
C $Name:  $
C $Namer: $

#ifdef ALLOW_AIM

C--   COMMON /LSMASK/ land-sea masks 
C     fmask1 - fractional land-sea mask
      COMMON /LSMASK/ fmask1
      _RL fmask1(ngp,MAX_NO_THREADS)

C--   COMMON /FORFIX/ Time invariant forcing fields 
C     phi0   - surface geopotential
C     alb0   - land-surface albedo
      COMMON /FORFIX/ phi0, alb0
      _RL phi0   (ngp,MAX_NO_THREADS)
      _RL alb0   (ngp,MAX_NO_THREADS)

C--   COMMON /FORCIN/ Forcing fields 
C     sst1   - SST
C     oice1  - sea ice fraction
C     stl1   - land-surface temperature
C     snow1  - snow depth (mm water)
C     soilq1 - soil wetness (mm water)
      COMMON /FORCIN/ sst1, oice1, stl1, snow1, soilq1
      _RL sst1   (ngp,MAX_NO_THREADS)
      _RL oice1  (ngp,MAX_NO_THREADS)
      _RL stl1   (ngp,MAX_NO_THREADS)
      _RL snow1  (ngp,MAX_NO_THREADS)
      _RL soilq1 (ngp,MAX_NO_THREADS)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
