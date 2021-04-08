C     *==========================================================*
C     | OCNCPL.h
C     | o Variables shared between coupling layer and ocean
C     |   component.
C     *==========================================================*
C     | These variables are used in the ocean component. Grid
C     | variables have already been mapped/interpolated to the
C     | ocean grid.
C     | Which variables are exported will depend on the specific
C     | model coupling being utilised. The variables carried here
C     | will need to be customised accordingly.
C     *==========================================================*

C--   fields sent from ATM to OCN:

C     COMMON /OCN_CPL2OCN_R/
C     landMask   :: Atmosphere land mask (=1 : full land grid-point;
C                   =0 : full ocean grid-point);
C                   Used in checking consistency of land/sea regions.
C     atmSLPr    :: Atmospheric Sea-Level pressure anomaly (Pa=N/m2)
C     HeatFlux   :: Surface heat flux (W/m2). Positive flux is out of ocean
C     qShortWave :: net shortwave radiation heat flux (W/m^2) (+=upward).
C     tauX       :: Zonal      surface wind-stress (N/m^2). Same sign as the
C                   wind : Zonal  wind is positive for westward flow.
C     tauY       :: Meridional surface wind-stress (N/m^2). Same sign as the
C                   wind : Merid. wind is positive for northward flow.
C     FWFlux     :: Surface flux of fresh water (kg/m2/s). Positive flux
C                   is out of ocean.
C     iceSaltFlx :: salt flux from seaice compon. (+=upward=leaving the ocean)
C     seaIceMass :: seaice mass (kg/m2)

      COMMON /OCN_CPL2OCN_R/
     &            landMask,
     &            atmSLPr, HeatFlux, qShortWave,
     &            tauX, tauY,
     &            FWFlux,
     &            iceSaltFlx, seaIceMass
      _RL landMask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL atmSLPr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HeatFlux  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL qShortWave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FWFlux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceSaltFlx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaIceMass(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_SALT_PLUME
C     COMMON /OCN_CPL2SALTPLM_R/
C     saltPlmFlx_cpl :: salt-plume flux for salt_plume pkg
      COMMON /OCN_CPL2SALTPLM_R/
     &            saltPlmFlx_cpl
      _RL saltPlmFlx_cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_DIC
C     COMMON /OCN_CPL2DIC_R/
C     RunOff_cpl :: Run-Off (kg/m2/s) used for DIC pkg calculations
C     airCO2     :: atmospheric CO2 (parts by volume)
C     surfWSpeed :: atmospheric surface wind speed (m/s)
      COMMON /OCN_CPL2DIC_R/
     &                   RunOff_cpl,
     &                   airCO2, surfWSpeed
      _RL RunOff_cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL airCO2    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL surfWSpeed(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DIC */

C--   fields used for multiple purpose:
#if (defined ALLOW_DIC) || (defined ALLOW_THSICE)
C     COMMON /OCN_CPL_ICE_R/
C     sIceFrac_cpl   :: seaice fraction (from external model)
      COMMON /OCN_CPL_ICE_R/
     &            sIceFrac_cpl
      _RL sIceFrac_cpl  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DIC or ALLOW_THSICE */

C--   fields sent in both direction:
#ifdef ALLOW_THSICE
C     COMMON /OCN_CPL_2WAYS_R/
C     sIceThick_cpl  :: seaice thickness [m]
C     sIceSnowH_cpl  :: snow thickness over seaice  [m]
C     sIceQ1_cpl     :: seaice enthalpy of ice layer 1 [J/kg]
C     sIceQ2_cpl     :: seaice enthalpy of ice layer 2 [J/kg]
      COMMON /OCN_CPL_2WAYS_R/
     &            sIceThick_cpl, sIceSnowH_cpl, sIceQ1_cpl, sIceQ2_cpl
      _RL sIceThick_cpl (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sIceSnowH_cpl (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sIceQ1_cpl    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sIceQ2_cpl    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_THSICE */

C--   fields sent from OCN to ATM:

C     COMMON /OCN_OCN2CPL_R/
C     ocMxlD2cpl :: Ocean mixed-layer depth exported to coupler (m)
C     SSTocn2cpl :: Ocean surface temperature map exported to
C                   coupling layer (oC).
C     SSSocn2cpl :: Ocean surface salinity exported to coupler (g/kg)
C     vSqocn2cpl :: Ocean surface velocity square exported to
C                   coupler (m2/s2)

      COMMON /OCN_OCN2CPL_R/
     &            ocMxlD2cpl, SSTocn2cpl, SSSocn2cpl, vSqocn2cpl
      _RL ocMxlD2cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SSTocn2cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SSSocn2cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vSqocn2cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_DIC
C     COMMON /OCN_DIC2CPL_R/
C     fluxCO2cpl :: ocean flux of CO2 exported to atm (mol/m2/s)
      COMMON /OCN_DIC2CPL_R/
     &            fluxCO2cpl
      _RL fluxCO2cpl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DIC */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
