C
C--   98/06/12 - elk
C
C     /========================================================================\
C     | AVER.h                                                                 |
C     | o Time averages of Dynamical model variables (common block DYNVARS_A) |
C     |========================================================================|
C
C     TimeAve   - time of temporal integration (s) *** for each thread ***
C     uVeltave  - zonal velocity (m/s, i=1 held at western face)
C     vVeltave  - meridional velocity (m/s, j=1 held at southern face)
C     thetatave - potential temperature (oC, held at pressure/tracer point)
C     salttave  - salinity (ppt, held at pressure/tracer point)
C     uttave    - uVel * theta
C     vttave    - vVel * theta
C
      COMMON /TAVE_0/ TimeAve,InterTimeAve
      _RL TimeAve(Nr,nSx,nSy)
      _RL InterTimeAve(Nr,nSx,nSy)
C
      COMMON /DYNVARS_A/ 
     &                  uVeltave,vVeltave,thetatave,salttave,
     &                  uttave,vttave,k13tave,k23tave,wVeltave,
     &                  KapGMtave,ConvectCountTave
      _RL  uVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  thetatave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  salttave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  uttave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vttave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  k13tave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  k23tave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  KapGMtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ConvectCountTave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
