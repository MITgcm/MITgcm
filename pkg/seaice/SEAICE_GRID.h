C $Header:

#ifdef ALLOW_SEAICE

C     /==========================================================\
C     | SEAICE_GRID.h                                            |
C     | o Sea ice model grid parameters.                         |
C     \==========================================================/
C
C     TNGTICE   - tan(phi) at the B-grid cell center
C     TNGICE    - tan(phi) at the north-east corner of the B-grid cell
C     CSTICE    - cos(phi) at the B-grid cell center
C     CSUICE    - cos(phi) at north-east corner of the B-grid cell
C     SINEICE   - sin(phi) at north-east corner of the B-grid cell
C     DXTICE    - Cell face separation in X thru B-grid cell center
C     DYTICE    - Cell face separation in Y thru B-grid cell center
C     DXUICE    - Separation in X with the north-east corner at the center
C     DYUICE    - Separation in Y with the north-east corner at the center
C
C     |       e                       e 
C     |       t                       t
C     |       c                       c
C     |                  |<------dxuice(i=1)--------->|
C     |       uv---------------------uv--------------etc...
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |    B-grid cell (h)    |
C     |       |                       |
C     |       |<--dxtice(i=1)-------->|          (h)
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       uv---------------------uv--------------etc...
C     |       |                       |
C
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

#endif /* ALLOW_SEAICE */
