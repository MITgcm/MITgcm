C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_ocean_coms.h,v 1.1 2004/06/07 16:57:39 molod Exp $
C $Name:  $

c Ocean Exports
c -------------------
      common /ocean_exports/ sst, sice
      _RL sst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,Nsy)
      _RL sice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,Nsy)

