C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_forcing.h,v 1.6 2010/08/24 13:29:00 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C-- Note: Variables which do not need to stay in common block (local var)
C         are declare locally in each S/R that use them (commented with "cL");
C         Some variables are not needed at all (commented with single "c").

C--   COMMON /LSMASK/ land-sea masks (initial. in INFORC)
C     fmask1 - fractional land / sea / sea-ice mask
C     1:land fraction ; 2:ice-free ocean ; 3:sea-ice frac.; (1)+(2)+(3)=1.
c_FM  COMMON /LSMASK/ fmask1, fmask0, fmasko1, fmaskl1
      COMMON /LSMASK/ fmask1
      _RL fmask1(NGP,3,MAX_NO_THREADS)
c     _RL fmask0(NGP,MAX_NO_THREADS)
c     _RL fmasko1(NGP,MAX_NO_THREADS)
c     _RL fmaskl1(NGP,MAX_NO_THREADS)

C--   COMMON /FORFIX/ Time invariant forcing fields
C              (initial. in INFORC, except for phis0 initial. in INVARS)
C     phi0   - surface geopotential
C     phis0  - ?
C     alb0   - land-surface albedo
c_FM  COMMON /FORFIX/ phi0, phis0, alb0
cL    _RL phi0   (NGP)
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
C     sti1   - sea-ice surface temperature [K]
C     stl1   - land-surface temperature    [K]
C     snow1  - snow depth (mm water)
C     soilw1 - soil wetness (mm water)
C     alb1   - surface albedo (1:land, 2:ocean, 3:sea-ice, 0:average)
C     dTsurf - surface temperature changes form 1 it to the next one
c     COMMON /FORDAY/ sst1, oice1, stl1, snow1, soilw1, alb1
      COMMON /FORDAY/ sst1, sti1, stl1, soilw1, alb1, dTsurf
      _RL sst1   (NGP,MAX_NO_THREADS)
cL    _RL oice1  (NGP,MAX_NO_THREADS)
      _RL sti1   (NGP,MAX_NO_THREADS)
      _RL stl1   (NGP,MAX_NO_THREADS)
cL    _RL snow1  (NGP,MAX_NO_THREADS)
      _RL soilw1 (NGP,MAX_NO_THREADS)
      _RL alb1   (NGP,0:3,MAX_NO_THREADS)
      _RL dTsurf (NGP,3,MAX_NO_THREADS)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */
