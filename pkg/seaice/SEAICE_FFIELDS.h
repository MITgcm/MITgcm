C $Header:

#ifdef ALLOW_SEAICE

C     /==========================================================\
C     | SEAICE_FFIELDS.h                                         |
C     | o Sea ice model forcing fields                           |
C     |==========================================================|
C     \==========================================================/
C
C     uwind  - Surface (10-m) zonal wind velocity in m/s
C              at North-East B-grid U point
C              >0 from West to East
C     vwind  - Surface (10-m) meridional wind velocity in m/s
C              at North-East B-grid V point
C              >0 from South to North
C     atemp  - Surface (2-m) air temperature in deg K
C              at North-East B-grid tracer point
C     aqh    - Surface (2m) specific humidity in kg/kg
C              at North-East B-grid tracer point
C     lwflux - Downward longwave radiation in W/m^2
C              at North-East B-grid tracer point
C              >0 for ocean warming
C     swflux - Downward shortwave radiation in W/m^2
C              at North-East B-grid tracer point
C              >0 for ocean warming
C     precip - Precipitation in m/s
C              at North-East B-grid tracer point
C              >0 decreases salinity
C     evap   - Evaporation in m/s
C              at North-East B-grid tracer point
C              >0 increases salinity
C     runoff - River and glacier runoff in m/s
C              at North-East B-grid tracer point
C              >0 decreases salinity
C

#ifdef SEAICE_EXTERNAL_FORCING

C--   Define forcing fields outside pkg/seaice.
#include "exf_fields.h"

#else SEAICE_EXTERNAL_FORCING

C--   Define forcing fields internally.
      COMMON /SEAICE_FFIELDS/ uwind, vwind, atemp,
     &     aqh, lwflux, swflux, precip, evap, runoff
      _RL  uwind    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vwind    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  atemp    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aqh      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  lwflux   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  swflux   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  precip   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  evap     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  runoff   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /SEAICE_TDFIELDS/ uwind0, uwind1, vwind0, vwind1,
     &     atemp0, atemp1, aqh0, aqh1, lwflux0, lwflux1,
     &     swflux0, swflux1, precip0, precip1, evap0, evap1,
     &     runoff0, runoff1, SSSsi0, SSSsi1, SSTsi0, SSTsi1
      _RS  uwind0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  uwind1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  atemp0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  atemp1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aqh0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aqh1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  lwflux0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  lwflux1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  swflux0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  swflux1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  precip0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  precip1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  evap0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  evap1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSSsi0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSSsi1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSTsi0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSTsi1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif SEAICE_EXTERNAL_FORCING

#endif ALLOW_SEAICE
