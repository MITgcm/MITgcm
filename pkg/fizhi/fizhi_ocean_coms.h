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

