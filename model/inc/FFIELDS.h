C $Header: /u/gcmpack/MITgcm/model/inc/FFIELDS.h,v 1.4 1998/06/17 21:07:01 adcroft Exp $
C
C     /==========================================================\
C     | FFIELDS.h                                                |
C     | o Model forcing fields                                   |
C     |==========================================================|
C     | The arrays here will need changing and customising for a |
C     | particular experiment.                                   |
C     \==========================================================/
C
C--   For a classical "gyre" type experiment just one term is needed.
C     fu     - Zonal velocity tendency term ( m/s^2 )
C     fv     - Meridional velocity tendency term ( m/s^2 )
C     SST    - Sea surface temperature (degrees) for forcing
C     SSS    - Sea surface salinity (psu) for forcing
C     Qnet   - Surface heat flux (degrees/second)
C     EmPpR  - Evaporation - Precipitation + Runoff (psu/second)
      COMMON /FFIELDS/
     &                 fu, fv,
     &                 SST, SSS,
     &                 Qnet, EmPpR
      _RS  fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPpR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /FFDOT/
     &                 fudot, fvdot,
     &                 SSTdot, SSSdot,
     &                 Qnetdot, EmPpRdot
      _RS  fudot    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fvdot    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSTdot   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSSdot   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnetdot  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPpRdot (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
