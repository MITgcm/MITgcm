C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/SEAICE_GRID.h,v 1.7 2004/04/28 12:00:53 mlosch Exp $
C $Name:  $

#ifdef ALLOW_SEAICE

C     /==========================================================\
C     | SEAICE_GRID.h                                            |
C     | o Sea ice model grid parameters.                         |
C     \==========================================================/
C
C     TNGTICE   - tan(phi) at the B-grid cell center
C     TNGICE    - tan(phi) at the south-west corner of the B-grid cell
C     CSTICE    - cos(phi) at the B-grid cell center
C     CSUICE    - cos(phi) at south-west corner of the B-grid cell
C     SINEICE   - sin(phi) at south-west corner of the B-grid cell
C     DXTICE    - Cell face separation in X thru B-grid cell center
C     DYTICE    - Cell face separation in Y thru B-grid cell center
C     DXUICE    - Separation in X with the south-west corner at the center
C     DYUICE    - Separation in Y with the south-west corner at the center
C
C     |       e                       e 
C     |       t                       t
C     |       c                       c
C     |                  |<------dxuice(i=2)--------->|
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
      _RL TNGTICE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL TNGICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL CSTICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL CSUICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL RECIP_CSTICE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL RECIP_CSUICE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL SINEICE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DXTICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DXUICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DYTICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DYUICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

      COMMON/SEAICE_GRID/ TNGTICE, TNGICE, CSTICE, CSUICE, SINEICE,
     &                    RECIP_CSTICE, RECIP_CSUICE,
     &                    DXTICE, DXUICE, DYTICE, DYUICE

#endif /* ALLOW_SEAICE */
