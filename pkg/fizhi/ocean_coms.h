C $Header: /u/gcmpack/MITgcm/pkg/fizhi/Attic/ocean_coms.h,v 1.2 2004/05/05 00:39:21 edhill Exp $
C $Name:  $

c Ocean Exports
c -------------------
      common /ocean_exports/ sst, sice
      _RL sst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,Nsy)
      _RL sice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,Nsy)

