C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_ocean_coms.h,v 1.2 2004/07/28 01:25:07 molod Exp $
C $Name:  $

c Ocean Exports
c -------------------
      common /ocean_exports/ sst, sice
      _RL sst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nsx,Nsy)
      _RL sice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nsx,Nsy)

