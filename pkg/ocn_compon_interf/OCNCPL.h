C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/OCNCPL.h,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $
C
C     /==========================================================\
C     | OCNCPL.h                                                 |
C     | o Variables shared between coupling layer and ocean      |
C     |   component.                                             |
C     |==========================================================|
C     | These variables are used in the ocean component. Grid    |
C     | variables have already been mapped/interpolated to the   |
C     | ocean grid.                                              |
C     | Which variables are exported will depend on the specific |
C     | model coupling being utilised. The variables carried here|
C     | will need to be customised accordingly.                  |
C     \==========================================================/
C
C     COMMON /OCN_CPL2OCN_R/ 
C     Hatm  - Atmosphere model orography i.e. height of mountains (in Pa)
C             Used in checking consistency of land/sea regions.
C     HeatFlux - Surface heat flux (W/m^2). Positive flux is out
C                of ocean.
C     tauX     - Zonal      surface wind-stress (N/m^2). Same sign as the 
C                wind : Zonal      wind is positive for westward flow.
C     tauY     - Meridional surface wind-stress (N/m^2). Same sign as the 
C                wind : Meridional wind is positive for northward flow.
C     FWFlux   - Surface flux of fresh water. Positive flux is out of
C                ocean.
C
      COMMON /OCN_CPL2OCN_R/ 
     &                   Hatm, HeatFlux, tauX, tauY, 
     &                   FWFlux,
     &                   uVelGround, vVelGround,
     &                   qLatent, qSensible, qLongwave,
     &                   qShortwave
      _RL Hatm    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HeatFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauX    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauY    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FWFlux  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uVelGround(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vVelGround(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL qLatent   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL qSensible (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL qLongwave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL qShortwave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     COMMON /OCN_OCN2CPL_R/ 
C     SSTocn2cpl - Ocean sea-surface temperature map exported to
C                  coupling layer (oC).
      COMMON /OCN_OCN2CPL_R/ 
     &                   SSTocn2cpl
      _RL SSTocn2cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
