C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_forcing.h,v 1.2 2002/12/10 02:35:27 jmc Exp $
C $Name:  $
C $Namer: $

#ifdef ALLOW_AIM

C--   COMMON /LSMASK/ land-sea masks (initial. in INFORC)
C     fmask1 - fractional land-sea mask
c_FM  COMMON /LSMASK/ fmask1, fmask0, fmasko1, fmaskl1
      COMMON /LSMASK/ fmask1
      _RL fmask1(NGP,MAX_NO_THREADS)
c     _RL fmask0(NGP,MAX_NO_THREADS)
c     _RL fmasko1(NGP,MAX_NO_THREADS)
c     _RL fmaskl1(NGP,MAX_NO_THREADS)

C--   COMMON /FORFIX/ Time invariant forcing fields 
C              (initial. in INFORC, except for phis0 initial. in INVARS)
C     phi0   - surface geopotential
C     phis0  - ?
C     alb0   - land-surface albedo
c_FM  COMMON /FORFIX/ phi0, phis0, alb0
      _RL phi0   (NGP)
c     _RL phis0  (NGP)
c     _RL alb0   (NGP,MAX_NO_THREADS)

C--   COMMON /FORMON/ Monthly-mean forcing fields (initial. in INFORC)
c     common /FORMON/ sst12(ix,il,12),
c    &                oice12(ix,il,12),
c    &                stl12(ix,il,12),
c    &                snow12(ix,il,12),
c    &                soilw12(ix,il,12)

C--   COMMON /FORDAY/ Daily forcing fields (updated in FORDATE)
C     sst1   - SST
C     oice1  - sea ice fraction
C     stl1   - land-surface temperature
C     snow1  - snow depth (mm water)
C     soilw1 - soil wetness (mm water)
C     alb1   - land-surface albedo
c     COMMON /FORDAY/ sst1, oice1, stl1, snow1, soilw1, alb1
      COMMON /FORDAY/ sst1, stl1, soilw1, alb1
      _RL sst1   (NGP,MAX_NO_THREADS)
c     _RL oice1  (NGP,MAX_NO_THREADS)
      _RL stl1   (NGP,MAX_NO_THREADS)
c     _RL snow1  (NGP,MAX_NO_THREADS)
      _RL soilw1 (NGP,MAX_NO_THREADS)
      _RL alb1   (NGP,MAX_NO_THREADS)
      _RL oice1(NGP), snow1(NGP)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
