C $Header:

#ifdef ALLOW_SEAICE

C     /==========================================================\
C     | SEAICE_FFIELDS.h                                         |
C     | o Sea ice model forcing fields                           |
C     |==========================================================|
C     \==========================================================/
C
C     gairx  - Surface (10-m) zonal wind velocity in m/s
C                 (>0 from West to East)
C     gairy  - Surface (10-m) meridional wind velocity in m/s
C                 (>0 from South to North)
C     tair   - Surface (2-m) air temperature in deg K
C
C     qa     - Surface (2m) specific humidity
C
C     flo    - Downward longwave radiation in W/m^2
C                 (>0 for ocean warming)
C     fsh    - Downward shortwave radiation in W/m^2
C                 (>0 for ocean warming)
C     rain   - Precipitation in m/s (>0 decreases salinity)
C
C     evap   - Evaporation in m/s (>0 increases salinity)
C
C     runoff - River and glacier runoff (>0 decreases salinity)
C
C
      COMMON /SEAICE_FFIELDS/
     &        gairx, gairy, tair, qa, flo, fsh, rain, evap, runoff
      _RS  gairx    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  gairy    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tair     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qa       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  flo      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fsh      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  rain     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  evap     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /SEAICE_TDFIELDS/
     &     gairx0, gairx1, gairy0, gairy1, tair0, tair1, qa0, qa1,
     &     flo0, flo1, fsh0, fsh1, rain0, rain1, evap0, evap1,
     &     runoff0, runoff1, SSSsi0, SSSsi1, SSTsi0, SSTsi1
      _RS  gairx0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  gairx1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  gairy0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  gairy1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qa0      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qa1      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  flo0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  flo1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fsh0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fsh1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  rain0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  rain1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  evap0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  evap1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSSsi0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSSsi1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSTsi0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSTsi1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif ALLOW_SEAICE
