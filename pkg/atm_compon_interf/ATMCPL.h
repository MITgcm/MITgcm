C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/ATMCPL.h,v 1.1 2003/12/15 02:44:47 jmc Exp $
C $Name:  $
C
C
C     /==========================================================\
C     | ATMCPL.h                                                 |
C     | o Variables shared between atmos. component to coupler   |
C     |   layer.                                                 |
C     |==========================================================|
C     | These variables are used in the atmos component. Grid    |
C     | variables have already been mapped/interpolated to the   |
C     | atmos grid.                                              |
C     | Which variables are exported will depend on the specific |
C     | ocean coupling being utilised. The variables             |
C     | carried here will need to be customised accordingly.     |
C     \==========================================================/
C
C     COMMON /ATM_ATM2CPL_R/ 
C     HeatFlux   - Atmospheric heat flux at lower boundary (W/m^2). Positive flux 
C                  is into atmosphere.
C     tauX       - Atmospheric zonal momentum flux at lower boundary (N/m^2).
C                  Same sign as wind. Positive zonal wind is westward.
C     tauY       - Atmospheric meridional momentum flux at lower boundary (N/m^2).
C                  Same sign as wind. Positive meridional wind is northward.
C     uVelGround - Zonal wind speed at the ground (m/s).
C     vVelGround - Meridional wind speed at the ground (m/s).
C     EvMPrFlux  - Fresh water flux (=Evap-Precip) on atmos. grid
C                      ( m/s, positive into atmosphere).
C     RunOffFlux - Fresh water flux (=RunOff) on atmos. grid
C                      ( m/s, positive is leaving the land bucket)
C     Qsensible  - Sensible heatflux (W/m^2).
C     Qlatent    - Latent heatflux (W/m^2).
C     Qlongwave  - Downward longwave (W/m^2).
C     Qshortwave - Upward shortwave (W/m^2).
C     HeatFluxTime   - Time period over which flux field has been integrated.
C     tauXTime       - Time period over which flux field has been integrated.
C     tauYTime       - Time period over which flux field has been integrated.
C     uVelGroundTime - Time period over which term has been integrated.
C     vVelGroundTime - Time period over which term has been integrated.
C     EvMPrTime      - Time period over which flux field has been integrated.
C     RunOffTime     - Time period over which flux field has been integrated.
C     QsensibleTime  - Time period over which term has been integrated.
C     QlatentTime    - Time period over which term has been integrated.
C     QlongwaveTime  - Time period over which term has been integrated.
C     QshortwaveTime - Time period over which term has been integrated.
      COMMON /ATM_ATM2CPL_R/ 
     &                   HeatFlux, tauX, tauY, EvMPrFlux, RunOffFlux,
     &                   uVelGround, vVelGround,
     &                   Qsensible, Qlatent, Qlongwave, Qshortwave, 
     &                   HeatFluxTime, tauXtime, tauYtime, 
     &                   uVelGroundTime, vVelGroundTime,
     &                   EvMPrTime, RunOffTime,
     &                   QsensibleTime, QlatentTime,
     &                   QlongwaveTime, QshortwaveTime
      _RL  HeatFlux  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  tauX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  tauY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uVelGround(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vVelGround(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  EvMPrFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  RunOffFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Qsensible (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Qlatent   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Qlongwave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Qshortwave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  HeatFluxTime(nSx,nSy)
      _RL  tauXTime(nSx,nSy)
      _RL  tauYTime(nSx,nSy)
      _RL  uVelGroundTime(nSx,nSy)
      _RL  vVelGroundTime(nSx,nSy)
      _RL  EvMPrTime (nSx,nSy)
      _RL  RunOffTime(nSx,nSy)
      _RL  QsensibleTime(nSx,nSy)
      _RL  QlatentTime(nSx,nSy)
      _RL  QlongwaveTime(nSx,nSy)
      _RL  QshortwaveTime(nSx,nSy)

C
C     COMMON /ATM_CPL2ATM_R/
C     Hocn   - Ocean depths (m). Hocn==0. => land.
C     SSTocn - Ocean surface temperature (oC). Undefined data for land points.
      COMMON /ATM_CPL2ATM_R/ 
     &                   Hocn, SSTocn
      _RL  Hocn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSTocn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
