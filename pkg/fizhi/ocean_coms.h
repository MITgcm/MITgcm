c Ocean Exports
c -------------------
      common /ocean_exports/ sst, sice
      _RL sst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,Nsy)
      _RL sice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,Nsy)

