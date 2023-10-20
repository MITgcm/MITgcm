C     Define some local weights specific to this experiment
C     whfluxm     :: weight for heat flux
C     wtheta      :: dummy weight for temperature
      COMMON /COST_LOCAL_WEIGHTS/ whfluxm, wtheta
      _RL whfluxm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wtheta ( Nr,nSx,nSy )
