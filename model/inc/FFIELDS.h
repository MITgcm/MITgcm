C $Header: /u/gcmpack/MITgcm/model/inc/FFIELDS.h,v 1.2 1998/04/24 02:11:36 cnh Exp $
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
      COMMON /FFIELDS/ fu, fv
      _RS  fu     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fv     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
