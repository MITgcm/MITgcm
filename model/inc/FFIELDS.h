C $Header: /u/gcmpack/MITgcm/model/inc/FFIELDS.h,v 1.6 1999/05/05 18:32:34 adcroft Exp $
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
C     Qnet   - Surface heat flux (converted to degrees/second)
C     EmPmR  - Evaporation - Precipitation - Runoff (converted to psu/second)
C     SST    - Sea surface temperature (degrees) for relaxation
C     SSS    - Sea surface salinity (psu) for relaxation
C     Qsw    - Short-wave surface heat flux (converted to degrees/second)
      COMMON /FFIELDS/
     &                 fu,
     &                 fv,
     &                 Qnet,
     &                 EmPmR,
     &                 SST,
     &                 SSS,
     &                 Qsw
      _RS  fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
