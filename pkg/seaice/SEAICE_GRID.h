C $Header:

#ifdef ALLOW_SEAICE

C     /==========================================================\
C     | SEAICE_GRID.h                                            |
C     | o Sea ice model grid parameters.                         |
C     \==========================================================/

      _RL TNGTICE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL TNGICE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL CSTICE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL CSUICE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL SINEICE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DXTICE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DXUICE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DYTICE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DYUICE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

      COMMON/SEAICE_GRID/ TNGTICE, TNGICE, CSTICE, CSUICE, SINEICE,
     &                    DXTICE, DXUICE, DYTICE, DYUICE

#endif ALLOW_SEAICE
