C $Header: /u/gcmpack/MITgcm/pkg/timeave/TIMEAVE_STATV.h,v 1.2 2001/05/29 14:01:40 adcroft Exp $
C $Name:  $
C
C--   98/06/12 - elk
C
C     /========================================================================\
C     | TIMEAVE_STATV.h                                                        |
C     | o Time averages of model state-variables (common block TAVE_STATEVARS) |
C     |========================================================================|
C
C     TimeAve_* - time of temporal integration (s) *** for each thread ***
C     TimeAve_half - half time_step multiple (used for state variables)
C     TimeAve_full - full time_step multiple (used for for intermediate var.) 
C     etaTave   - surface displacement (r unit, i.e. ocean:z, atmos:p)
C     uVeltave  - zonal velocity (m/s, i=1 held at western face)
C     vVeltave  - meridional velocity (m/s, j=1 held at southern face)
C     thetatave - potential temperature (oC, held at pressure/tracer point)
C     salttave  - salinity (ppt, held at pressure/tracer point)
C     uttave    - uVel * theta
C     vttave    - vVel * theta

      COMMON /TAVE_TIME/ TimeAve_half,TimeAve_full
      _RL TimeAve_half(Nr,nSx,nSy)
      _RL TimeAve_full(Nr,nSx,nSy)

      COMMON /TAVE_STATEVARS/ 
     &                  etaTave,uVeltave,vVeltave,wVeltave,
     &                  thetatave,salttave,uttave,vttave,
     &                  TTtave,UUtave,VVtave,
     &                  phiHydtave,ConvectCountTave
      _RL  etaTave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  thetatave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  salttave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  TTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  UUtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  VVtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  uttave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vttave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wVeltave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phiHydtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL ConvectCountTave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
