C     *==========================================================*
C     | OCNVARS.h Declare arrays for holding data on the ocean
C     |           grid. Arrays may need adding or removing
C     |           different couplings.
C     *==========================================================*

C     landMask_ocn   :: Atmosphere land mask (=1 : full land grid-point;
C                       =0 : full ocean grid-point);

C--   fields received from OCN:
C     OcMxlD_ocn     ::- Ocean mixed-layer depths on ocean grid (m)
C     SST_ocn        :: Sea surface temperature on ocean grid ( oC).
C     SSS_ocn        :: Sea surface salinity on ocean grid (g/kg).
C     vSq_ocn        :: Sea surface velocity square on ocean grid (m2/s2)
C-    optionally received:
C     fluxCO2_ocn    :: flux of CO2 from ocn->Atm on ocean grid (mol/m2/s)

C--   fields send to OCN:
C     atmSLPr_ocn    :: Sea Level atmos. pressure on ocean grid (Pa)
C     HeatFlux_ocn   :: net Heat flux on ocean grid (W/m2, +=upward)
C     qShortWave_ocn :: net shortwave radiation on ocean grid (W/m2, +=upward)
C     TauX_ocn       :: Zonal momentum flux on ocean grid ( N/m^2, same
C                       sign as the wind ; positive wind == westward flow)
C     TauY_ocn       :: Meridional momentum flux on ocean grid ( N/m^2, same
C                       sign as the wind ; positive wind == northward flow)
C     FWFlux_ocn     :: Fresh water flux on ocean grid
C                       ( kg/m2/s, positive out of ocean).
C-    optionally sent:
C     SaltFlx_ocn    :: salt flux from seaice compon. on ocean grid
C                       ( g/m2/s, +=upward=leaving the ocean)
C     sIceMass_ocn   :: seaice mass  on ocean grid (kg/m2)
C     saltPlmFlx_ocn :: salt-plume flux for salt_plume pkg
C     RunOff_ocn     :: Run-Off (kg/m2/s) used for DIC pkg calculations
C     aCO2_ocn       :: atmos CO2 on ocean grid (parts by volume)
C     wSpeed_ocn     :: surface windspeed on ocean grid (m/s)

C--   fields used for multiple purpose:
C     sIceFrac_ocn   :: seaice fraction  on ocean grid

C--   fields sent to & received from OCN:
C     sIceThick_ocn  :: seaice thickness [m]
C     sIceSnowH_ocn  :: snow thickness over seaice  [m]
C     sIceQ1_ocn     :: seaice enthalpy of ice layer 1 [J/kg]
C     sIceQ2_ocn     :: seaice enthalpy of ice layer 2 [J/kg]

      COMMON /OCNVARS_R/
     &       landMask_ocn,
     &       OcMxlD_ocn, SST_ocn, SSS_ocn, vSq_ocn,
     &       fluxCO2_ocn,
     &       atmSLPr_ocn, HeatFlux_ocn, qShortWave_ocn,
     &       TauX_ocn, TauY_ocn, FWFlux_ocn,
     &       SaltFlx_ocn, sIceMass_ocn, saltPlmFlx_ocn,
     &       RunOff_ocn, aCO2_ocn, wSpeed_ocn,
     &       sIceFrac_ocn,
     &       sIceThick_ocn, sIceSnowH_ocn, sIceQ1_ocn, sIceQ2_ocn

      _RL landMask_ocn   (Nx_ocn,Ny_ocn)

      _RL OcMxlD_ocn     (Nx_ocn,Ny_ocn)
      _RL SST_ocn        (Nx_ocn,Ny_ocn)
      _RL SSS_ocn        (Nx_ocn,Ny_ocn)
      _RL vSq_ocn        (Nx_ocn,Ny_ocn)
      _RL fluxCO2_ocn    (Nx_ocn,Ny_ocn)

      _RL atmSLPr_ocn    (Nx_ocn,Ny_ocn)
      _RL HeatFlux_ocn   (Nx_ocn,Ny_ocn)
      _RL qShortWave_ocn (Nx_ocn,Ny_ocn)
      _RL TauX_ocn       (Nx_ocn,Ny_ocn)
      _RL TauY_ocn       (Nx_ocn,Ny_ocn)
      _RL FWFlux_ocn     (Nx_ocn,Ny_ocn)

      _RL SaltFlx_ocn    (Nx_ocn,Ny_ocn)
      _RL sIceMass_ocn   (Nx_ocn,Ny_ocn)
      _RL saltPlmFlx_ocn (Nx_ocn,Ny_ocn)

      _RL RunOff_ocn     (Nx_ocn,Ny_ocn)
      _RL aCO2_ocn       (Nx_ocn,Ny_ocn)
      _RL wSpeed_ocn     (Nx_ocn,Ny_ocn)

      _RL sIceFrac_ocn   (Nx_ocn,Ny_ocn)
      _RL sIceThick_ocn  (Nx_ocn,Ny_ocn)
      _RL sIceSnowH_ocn  (Nx_ocn,Ny_ocn)
      _RL sIceQ1_ocn     (Nx_ocn,Ny_ocn)
      _RL sIceQ2_ocn     (Nx_ocn,Ny_ocn)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
