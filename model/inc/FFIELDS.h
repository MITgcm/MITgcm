C $Header: /u/gcmpack/MITgcm/model/inc/FFIELDS.h,v 1.3 1998/06/15 05:13:55 cnh Exp $
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
C     thetaClim - Temperature climatology. Used in relaxation
C                 surface bc.
C     saltClim  - Salinty climatology
      COMMON /FFIELDS/ fu, fv, thetaClim, saltClim
      _RS  fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  thetaClim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RS  saltClim (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
