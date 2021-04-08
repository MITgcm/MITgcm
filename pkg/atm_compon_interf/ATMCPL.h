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

C--   fields sent from ATM to OCN:

C-    COMMON /ATM_ATM2CPL_R/
C     atmSLPr    :: Atmospheric Sea-Level pressure anomaly (Pa=N/m2)
C     HeatFlux   :: Atmospheric net surface heat flux (W/m^2) (+=upward).
C     qShortWave :: net shortwave radiation heat flux (W/m^2) (+=upward).
C     tauX       :: Atmospheric zonal momentum flux at lower boundary (N/m^2).
C                  Same sign as wind. Positive zonal wind is westward.
C     tauY       :: Atmospheric meridional momentum flux at lower boundary (N/m^2).
C                  Same sign as wind. Positive meridional wind is northward.
C     EvMPrFlux  :: Fresh water flux (=Evap-Precip) on atmos. grid
C                      ( kg/m2/s, positive into atmosphere).
C     atmSLPrTime    :: Time fraction over which term has been integrated.
C     HeatFluxTime   :: Time fraction over which term has been integrated.
C     qShortWaveTime :: Time fraction over which term has been integrated.
C     tauXTime       :: Time fraction over which term has been integrated.
C     tauYTime       :: Time fraction over which term has been integrated.
C     EvMPrTime      :: Time fraction over which term has been integrated.
      COMMON /ATM_ATM2CPL_R/
     &                   atmSLPr, HeatFlux, qShortWave,
     &                   tauX, tauY, EvMPrFlux,
     &                   atmSLPrTime, HeatFluxTime, qShortWaveTime,
     &                   tauXtime, tauYtime, EvMPrTime
      _RL  atmSLPr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  HeatFlux  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  qShortWave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  tauX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  tauY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  EvMPrFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  atmSLPrTime   (nSx,nSy)
      _RL  HeatFluxTime  (nSx,nSy)
      _RL  qShortWaveTime(nSx,nSy)
      _RL  tauXTime      (nSx,nSy)
      _RL  tauYTime      (nSx,nSy)
      _RL  EvMPrTime     (nSx,nSy)

#ifdef ALLOW_LAND
C-    COMMON /ATM_LAND2CPL_R/
C     RunOffFlux :: Fresh water flux (=RunOff) on atmos. grid
C                      ( kg/m2/s, positive is leaving the land bucket)
C     RunOffEnFx :: Energy carried by RunOff (W/m2) (+=leaving land bucket)
C     RunOffTime :: Time fraction over which term has been integrated.
C     ROEnFxTime :: Time fraction over which term has been integrated.
      COMMON /ATM_LAND2CPL_R/
     &                   RunOffFlux, RunOffEnFx,
     &                   RunOffTime, ROEnFxTime
      _RL  RunOffFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  RunOffEnFx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  RunOffTime (nSx,nSy)
      _RL  ROEnFxTime (nSx,nSy)
#endif /* ALLOW_LAND */

#ifdef ALLOW_THSICE
C-    COMMON /ATM_SICE2CPL_R/
C     iceSaltFlx     :: salt flux from sea-ice compon. (+=upward)
C     seaIceMass     :: sea-ice mass (kg/m2)
C     saltPlmFlx_cpl :: salt-plume flux for salt_plume pkg
C     saltFxTime     :: Time fraction over which term has been integrated.
C     sIceMassTime   :: Time fraction over which term has been integrated.
C     saltPlmFlxTime :: Time fraction over which term has been integrated
      COMMON /ATM_SICE2CPL_R/
     &                   iceSaltFlx, seaIceMass,   saltPlmFlx_cpl,
     &                   saltFxTime, sIceMassTime, saltPlmFlxTime
      _RL  iceSaltFlx    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  seaIceMass    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  saltPlmFlx_cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  saltFxTime    (nSx,nSy)
      _RL  sIceMassTime  (nSx,nSy)
      _RL  saltPlmFlxTime(nSx,nSy)
#endif /* ALLOW_THSICE */

#ifdef ALLOW_AIM
C-    COMMON /ATM_AIM2CPL_R/
C     airCO2     :: atmospheric CO2 (parts by volume)
C     sWSpeed    :: surface wind speed (m/s)
C     airCO2Time     :: Time fraction over which term has been integrated.
C     sWSpeedTime    :: Time fraction over which term has been integrated.
      COMMON /ATM_AIM2CPL_R/
     &                   airCO2,     sWSpeed,
     &                   airCO2Time, sWSpeedTime
      _RL  airCO2     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sWSpeed    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  airCO2Time (nSx,nSy)
      _RL  sWSpeedTime(nSx,nSy)
#endif /* ALLOW_AIM */

#ifdef ALLOW_THSICE
C--   fields used for multiple purpose:
C-    COMMON /ATM_CPL_ICE_R/
C     sIceFrac_cpl   :: seaice fraction
C     sIceFracTime   :: Time fraction over which term has been integrated.
      COMMON /ATM_CPL_ICE_R/
     &                   sIceFrac_cpl, sIceFracTime
      _RL  sIceFrac_cpl (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sIceFracTime (nSx,nSy)

C--   fields sent in both direction:
C-    COMMON /ATM_CPL_2WAYS_R/
C     sIceThick_cpl :: seaice thickness [m]
C     sIceSnowH_cpl :: snow thickness over seaice  [m]
C     sIceQ1_cpl    :: seaice enthalpy of ice layer 1 [J/kg]
C     sIceQ2_cpl    :: seaice enthalpy of ice layer 2 [J/kg]
C     sIceThickTime ::  Time fraction over which term has been integrated.
C     sIceSnowHTime ::  Time fraction over which term has been integrated.
C     sIceQ1Time    ::  Time fraction over which term has been integrated.
C     sIceQ2Time    ::  Time fraction over which term has been integrated.
      COMMON /ATM_CPL_2WAYS_R/
     &            sIceThick_cpl, sIceSnowH_cpl, sIceQ1_cpl, sIceQ2_cpl,
     &            sIceThickTime, sIceSnowHTime, sIceQ1Time, sIceQ2Time
      _RL  sIceThick_cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sIceSnowH_cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sIceQ1_cpl   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sIceQ2_cpl   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sIceThickTime(nSx,nSy)
      _RL  sIceSnowHTime(nSx,nSy)
      _RL  sIceQ1Time   (nSx,nSy)
      _RL  sIceQ2Time   (nSx,nSy)
#endif /* ALLOW_THSICE */

C--   fields sent from OCN to ATM:

C-    COMMON /ATM_CPL2ATM_R/
C     ocMxlD    :: Ocean mixed-layer depth (m). ocMxlD==0. => land.
C     SSTocn    :: Ocean surface temperature (oC). Undefined data for land points.
C     SSSocn    :: Ocean surface salinity (g/kg). Undefined data for land points.
C     vSqocn    :: Ocean surface velocity square (m2/s2).
      COMMON /ATM_CPL2ATM_R/
     &            ocMxlD, SSTocn, SSSocn, vSqocn
      _RL  ocMxlD   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSTocn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSSocn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vSqocn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_AIM
C-    COMMON /ATM_CPL2AIM_R/
C     flxCO2ocn :: Ocean flux of CO2 (mol/m2/s).
      COMMON /ATM_CPL2AIM_R/
     &            flxCO2ocn
      _RL  flxCO2ocn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_AIM */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
