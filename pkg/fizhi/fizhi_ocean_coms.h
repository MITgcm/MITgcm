C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_ocean_coms.h,v 1.5 2007/03/12 21:46:24 molod Exp $
C $Name:  $

c Ocean Parameters
c -------------------
      common /ocean_params/sstclim,siceclim,ksst,kice
      logical sstclim,siceclim
      integer ksst, kice

c Ocean Exports
c -------------------
      common /ocean_exports/ sst, sice
      _RL sst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nsx,Nsy)
      _RL sice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nsx,Nsy)

