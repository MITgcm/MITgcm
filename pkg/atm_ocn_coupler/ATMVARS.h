C     *==========================================================*
C     | ATMVARS.h Declare arrays for holding data on the atmos.
C     |           grid. Arrays may need adding or removing
C     |           different couplings.
C     *==========================================================*

C     landMask_atm   :: Atmosphere land mask (=1 : full land grid-point;
C                       =0 : full ocean grid-point);

C--   fields send to ATM:
C     OcMxlD_atm     :: Ocean mixed-layer depths on atmos. grid (m)
C     SST_atm        :: Sea surface temperature on atmos. grid ( oC).
C     SSS_atm        :: Sea surface salinity on atmos. grid (g/kg).
C     vSq_atm        :: Sea surface velocity square on atmos. grid (m2/s2)
C-    optionally sent:
C     fluxCO2_atm    :: flux of CO2 from ocn->Atm on atmos. grid (mol/m2/s)

C--   fields received from ATM:
C     atmSLPr_atm    :: Sea Level atmos. pressure on atmos. grid (Pa)
C     HeatFlux_atm   :: net Heat flux on atmos. grid (W/m2, +=upward)
C     qShortWave_atm :: net shortwave radiation on atmos. grid (W/m2, +=upward)
C     TauX_atm       :: Zonal momentum flux on atmos. grid ( N/m^2, same
C                       sign as the wind ; positive wind == westward flow)
C     TauY_atm       :: Meridional momentum flux on atmos. grid ( N/m^2, same
C                       sign as the wind ; positive wind == northward flow)
C     EvMPr_atm      :: Fresh water flux (=Evap-Precip) on atmos. grid
C                       ( kg/m2/s, positive into atmosphere).
C-    optionally received:
C     RunOff_atm     :: Fresh water flux (=RunOff) on atmos. grid
C                       ( kg/m2/s, positive is leaving the land bucket)
C     ROEnFx_atm     :: Energy carried by RunOff on atmos. grid
C                       ( W/m2, +=leaving land bucket)
C     SaltFlx_atm    :: salt flux from seaice compon. on atmos. grid
C                       ( g/m2/s, +=upward=leaving the ocean)
C     sIceMass_atm   :: seaice mass  on atmos. grid (kg/m2)
C     saltPlmFlx_atm :: salt-plume flux on atmos. grid, for salt_plume pkg
C     aCO2_atm       :: atmos CO2 on atmos. grid (parts by volume)
C     wSpeed_atm     :: surface windspeed on atmos. grid (m/s)

C--   fields used for multiple purpose:
C     sIceFrac_atm   :: seaice fraction on atmos. grid

C--   fields sent to & received from ATM:
C     sIceThick_atm  :: seaice thickness [m]                  on atmos. grid
C     sIceSnowH_atm  :: snow thickness over seaice  [m]       on atmos. grid
C     sIceQ1_atm     :: seaice enthalpy of ice layer 1 [J/kg] on atmos. grid
C     sIceQ2_atm     :: seaice enthalpy of ice layer 2 [J/kg] on atmos. grid

      COMMON /ATMVARS_R/
     &       landMask_atm,
     &       OcMxlD_atm, SST_atm, SSS_atm, vSq_atm,
     &       fluxCO2_atm,
     &       atmSLPr_atm, HeatFlux_atm, qShortWave_atm,
     &       TauX_atm, TauY_atm, EvMPr_atm,
     &       RunOff_atm, ROEnFx_atm,
     &       SaltFlx_atm, sIceMass_atm, saltPlmFlx_atm,
     &       aCO2_atm, wSpeed_atm,
     &       sIceFrac_atm,
     &       sIceThick_atm, sIceSnowH_atm, sIceQ1_atm, sIceQ2_atm

      _RL landMask_atm   (Nx_atm,Ny_atm)

      _RL OcMxlD_atm     (Nx_atm,Ny_atm)
      _RL SST_atm        (Nx_atm,Ny_atm)
      _RL SSS_atm        (Nx_atm,Ny_atm)
      _RL vSq_atm        (Nx_atm,Ny_atm)
      _RL fluxCO2_atm    (Nx_atm,Ny_atm)

      _RL atmSLPr_atm    (Nx_atm,Ny_atm)
      _RL HeatFlux_atm   (Nx_atm,Ny_atm)
      _RL qShortWave_atm (Nx_atm,Ny_atm)
      _RL TauX_atm       (Nx_atm,Ny_atm)
      _RL TauY_atm       (Nx_atm,Ny_atm)
      _RL EvMPr_atm      (Nx_atm,Ny_atm)

      _RL RunOff_atm     (Nx_atm,Ny_atm)
      _RL ROEnFx_atm     (Nx_atm,Ny_atm)
      _RL SaltFlx_atm    (Nx_atm,Ny_atm)
      _RL sIceMass_atm   (Nx_atm,Ny_atm)
      _RL saltPlmFlx_atm (Nx_atm,Ny_atm)
      _RL aCO2_atm       (Nx_atm,Ny_atm)
      _RL wSpeed_atm     (Nx_atm,Ny_atm)

      _RL sIceFrac_atm   (Nx_atm,Ny_atm)
      _RL sIceThick_atm  (Nx_atm,Ny_atm)
      _RL sIceSnowH_atm  (Nx_atm,Ny_atm)
      _RL sIceQ1_atm     (Nx_atm,Ny_atm)
      _RL sIceQ2_atm     (Nx_atm,Ny_atm)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
