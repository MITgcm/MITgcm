C $Header:

#ifdef ALLOW_SEAICE

C     /==========================================================\
C     | SEAICE_FFIELDS.h                                         |
C     | o Sea ice model forcing fields                           |
C     |==========================================================|
C     \==========================================================/
C
C
C     uwind     :: Surface (10-m) zonal wind velocity in m/s
C                  > 0 for increase in uVel, which is west to
C                      east for cartesian and spherical polar grids
C                  Typical range: -10 < uwind < 10
C                  Northeast B-grid U point
C
C     vwind     :: Surface (10-m) meridional wind velocity in m/s
C                  > 0 for increase in vVel, which is south to
C                      north for cartesian and spherical polar grids
C                  Typical range: -10 < vwind < 10
C                  Northeast B-grid V point
C
C     evap      :: Evaporation in m/s
C                  > 0 for increase in salt (ocean salinity)
C                  Typical range: 0 < evap < 2.5e-7
C                  Northeast B-grid tracer point
C
C     precip    :: Precipitation in m/s
C                  > 0 for decrease in salt (ocean salinity)
C                  Typical range: 0 < precip < 5e-7
C                  Northeast B-grid tracer point
C
C     runoff    :: River and glacier runoff in m/s
C                  > 0 for decrease in salt (ocean salinity)
C                  Typical range: 0 < runoff < ????
C                  Northeast B-grid tracer point
C
C     atemp     :: Surface (2-m) air temperature in deg K
C                  Typical range: 200 < atemp < 300
C                  Northeast B-grid tracer point
C
C     aqh       :: Surface (2m) specific humidity in kg/kg
C                  Typical range: 0 < aqh < 0.02
C                  Northeast B-grid tracer point
C
C     swdown    :: Downward shortwave radiation in W/m^2
C                  > 0 for increase in theta (ocean warming)
C                  Typical range: 0 < swdown < 450
C                  Northeast B-grid tracer point
C
C     lwdown    :: Downward longwave radiation in W/m^2
C                  > 0 for increase in theta (ocean warming)
C                  Typical range: 50 < lwdown < 450
C                  Northeast B-grid tracer point
C
C
C     NOTES:
C     ======
C
C     #ifdef SEAICE_EXTERNAL_FORCING, sea-ice forcing fields
C     are defined in exf_fields.h
C
C     #ifdef SEAICE_EXTERNAL_FLUXES, additional forcing fields, fu,
C     fv, Qnet, Qsw, and EmPmR, as defined in FFIELDS.h, are required.
C
C     If (SEAICEwindOnCgrid .EQ. .TRUE.), uwind and vwind are
C     defined on southwest C-grid U and V points, respectively.
C
C     Downward and net radiative fluxes are not the same !!!
C     Downward radiative fluxes, swdown and lwdown, are used by
C     radiation bulk formulae to compute net radiative fluxes.
C

#ifdef SEAICE_EXTERNAL_FORCING

C--   Define forcing fields outside pkg/seaice.
#include "exf_fields.h"

#else /* SEAICE_EXTERNAL_FORCING undefined */

C--   Define forcing fields internally.
      COMMON /SEAICE_FFIELDS/ uwind, vwind, atemp,
     &     aqh, lwdown, swdown, precip, evap, runoff
      _RL  uwind    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vwind    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  atemp    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aqh      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  lwdown   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  swdown   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  precip   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  evap     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  runoff   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /SEAICE_TDFIELDS/ uwind0, uwind1, vwind0, vwind1,
     &     atemp0, atemp1, aqh0, aqh1, lwdown0, lwdown1,
     &     swdown0, swdown1, precip0, precip1, evap0, evap1,
     &     runoff0, runoff1, SSSsi0, SSSsi1, SSTsi0, SSTsi1
      _RS  uwind0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  uwind1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  atemp0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  atemp1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aqh0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aqh1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  lwdown0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  lwdown1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  swdown0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  swdown1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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

#endif /* SEAICE_EXTERNAL_FORCING */

#endif /* ALLOW_SEAICE */
