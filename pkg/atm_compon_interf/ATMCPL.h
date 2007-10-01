C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/ATMCPL.h,v 1.4 2007/10/01 14:51:38 jmc Exp $
C $Name:  $
C
C
C     *==========================================================*
C     | ATMCPL.h
C     | o Variables shared between atmos. component to coupler
C     |   layer.
C     *==========================================================*
C     | These variables are used in the atmos component. Grid
C     | variables have already been mapped/interpolated to the
C     | atmos grid.
C     | Which variables are exported will depend on the specific
C     | ocean coupling being utilised. The variables
C     | carried here will need to be customised accordingly.
C     *==========================================================*
C
C     COMMON /ATM_ATM2CPL_R/
C     atmSLPr    :: Atmospheric Sea-Level pressure anomaly (Pa=N/m2)
C     HeatFlux   :: Atmospheric net surface heat flux (W/m^2) (+=upward).
C     qShortWave :: net shortwave radiation heat flux (W/m^2) (+=upward).
C     qSensible  :: Sensible heatflux (W/m^2).
C     qLatent    :: Latent heatflux (W/m^2).
C     qLongWave  :: net longwave radiation heat flux  (W/m^2) (+=upward).
C     uVelGround :: Zonal wind speed at the ground (m/s).
C     vVelGround :: Meridional wind speed at the ground (m/s).
C     tauX       :: Atmospheric zonal momentum flux at lower boundary (N/m^2).
C                  Same sign as wind. Positive zonal wind is westward.
C     tauY       :: Atmospheric meridional momentum flux at lower boundary (N/m^2).
C                  Same sign as wind. Positive meridional wind is northward.
C     EvMPrFlux  :: Fresh water flux (=Evap-Precip) on atmos. grid
C                      ( kg/m2/s, positive into atmosphere).
C     RunOffFlux :: Fresh water flux (=RunOff) on atmos. grid
C                      ( kg/m2/s, positive is leaving the land bucket)
C     RunOffEnFx :: Energy carried by RunOff (W/m2) (+=leaving land bucket)
C     iceSaltFlx :: salt flux from sea-ice compon. (+=upward=leaving the ocean)
C     seaIceMass :: sea-ice mass (kg/m2)
C     airCO2     :: atmospheric CO2 (parts by volume)
C     sWSpeed    :: surface wind speed (m/s)
C     fracIce    :: seaice fraction
C     atmSLPrTime    :: Time period over which term has been integrated.
C     HeatFluxTime   :: Time period over which flux field has been integrated.
C     qShortWaveTime :: Time period over which term has been integrated.
C     qSensibleTime  :: Time period over which term has been integrated.
C     qLatentTime    :: Time period over which term has been integrated.
C     qLongwaveTime  :: Time period over which term has been integrated.
C     uVelGroundTime :: Time period over which term has been integrated.
C     vVelGroundTime :: Time period over which term has been integrated.
C     tauXTime       :: Time period over which flux field has been integrated.
C     tauYTime       :: Time period over which flux field has been integrated.
C     EvMPrTime      :: Time period over which flux field has been integrated.
C     RunOffTime     :: Time period over which flux field has been integrated.
C     ROEnFxTime     :: Time period over which term has been integrated.
C     saltFxTime     :: Time period over which term has been integrated.
C     seaIceTime     :: Time period over which term has been integrated.
C     airCO2Time     :: Time period over which term has been integrated.
C     sWSpeedTime    :: Time period over which term has been integrated.
C     fracIceTime    :: Time period over which term has been integrated.
      COMMON /ATM_ATM2CPL_R/
     &                   atmSLPr, HeatFlux, qShortWave,
c    &                   qSensible, qLatent, qLongWave,
c    &                   uVelGround, vVelGround,
     &                   tauX, tauY,
     &                   EvMPrFlux, RunOffFlux, RunOffEnFx,
     &                   iceSaltFlx, seaIceMass,
     &                   airCO2, sWSpeed, fracIce,
     &                   atmSLPrTime, HeatFluxTime, qShortWaveTime,
c    &                   qSensibleTime, qLatentTime, qLongWaveTime,
c    &                   uVelGroundTime, vVelGroundTime,
     &                   tauXtime, tauYtime,
     &                   EvMPrTime, RunOffTime, ROEnFxTime,
     &                   saltFxTime, seaIceTime,
     &                   airCO2Time, sWSpeedTime, fracIceTime
      _RL  atmSLPr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  HeatFlux  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  qShortWave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  qSensible (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  qLatent   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  qLongWave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  uVelGround(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  vVelGround(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  tauX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  tauY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  EvMPrFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  RunOffFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  RunOffEnFx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  iceSaltFlx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  seaIceMass(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  airCO2    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sWSpeed   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  fracIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  atmSLPrTime   (nSx,nSy)
      _RL  HeatFluxTime  (nSx,nSy)
      _RL  qShortWaveTime(nSx,nSy)
c     _RL  qSensibleTime (nSx,nSy)
c     _RL  qLatentTime   (nSx,nSy)
c     _RL  qLongWaveTime (nSx,nSy)
c     _RL  uVelGroundTime(nSx,nSy)
c     _RL  vVelGroundTime(nSx,nSy)
      _RL  tauXTime      (nSx,nSy)
      _RL  tauYTime      (nSx,nSy)
      _RL  EvMPrTime     (nSx,nSy)
      _RL  RunOffTime    (nSx,nSy)
      _RL  ROEnFxTime    (nSx,nSy)
      _RL  saltFxTime    (nSx,nSy)
      _RL  seaIceTime    (nSx,nSy)
      _RL  airCO2Time    (nSx,nSy)
      _RL  sWSpeedTime   (nSx,nSy)
      _RL  fracIceTime   (nSx,nSy)


C     COMMON /ATM_CPL2ATM_R/
C     ocMxlD    :: Ocean mixed-layer depth (m). ocMxlD==0. => land.
C     SSTocn    :: Ocean surface temperature (oC). Undefined data for land points.
C     SSSocn    :: Ocean surface salinity (psu). Undefined data for land points.
C     vSqocn    :: Ocean surface velocity square (m2/s2).
C     flxCO2ocn :: Ocean flux of CO2 (mol/m2/s).
      COMMON /ATM_CPL2ATM_R/
     &                   ocMxlD, SSTocn, SSSocn, vSqocn, flxCO2ocn
      _RL  ocMxlD   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSTocn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSSocn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vSqocn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  flxCO2ocn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
