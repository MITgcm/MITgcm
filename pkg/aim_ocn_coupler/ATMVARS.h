C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/ATMVARS.h,v 1.1 2003/12/15 02:28:00 jmc Exp $
C $Name:  $

C     /==========================================================\
C     | ATMVARS.h Declare arrays for holding data on the atmos.  |
C     |           grid. Arrays may need adding or removing       |
C     |           different couplings.                           |
C     \==========================================================/
C     AtmDepths_atm  - Atmos. depths on atmos. grid
C     OcnDepths_ocn  - Ocean depth on atmos. grid.
C     SST_atm        - Sea surface temperature on atmos. grid ( oC ).
C     HeatFlux_atm   - Heat flux on atmos. grid ( Wm^-2, positive into
C                      atmosphere).
C     EvMPr_atm      - Fresh water flux (=Evap-Precip) on atmos. grid 
C                      ( m/s, positive into atmosphere).
C     RunOff_atm     - Fresh water flux (=RunOff) on atmos. grid 
C                      ( m/s, positive is leaving the land bucket)
C     TauX_atm       - Zonal momentum flux on atmos. grid ( N/m^2, same
C                      sign as the wind ; positive wind == westward flow)
C     TauY_atm       - Meridional momentum flux on atmos. grid ( N/m^2, same
C                      sign as the wind ; positive wind == northward flow)
      COMMON /ATMVARS_R/
     &       AtmDepths_atm, OcnDepths_atm,
     &       SST_atm, HeatFlux_atm, EvMPr_atm, RunOff_atm,
     &       TauX_atm, TauY_atm,
     &       uVelGround_atm, vVelGround_atm,
     &       qLatent_atm, qSensible_atm,
     &       qLongwave_atm, qShortwave_atm
      _RS AtmDepths_atm  (Nx_atm,Ny_atm)
      _RS OcnDepths_atm  (Nx_atm,Ny_atm)
      _RL SST_atm        (Nx_atm,Ny_atm)
      _RL HeatFlux_atm   (Nx_atm,Ny_atm)
      _RL EvMPr_atm      (Nx_atm,Ny_atm)
      _RL RunOff_atm     (Nx_atm,Ny_atm)
      _RL TauX_atm       (Nx_atm,Ny_atm)
      _RL TauY_atm       (Nx_atm,Ny_atm)
      _RL uVelGround_atm (Nx_atm,Ny_atm)
      _RL vVelGround_atm (Nx_atm,Ny_atm)
      _RL qLatent_atm    (Nx_atm,Ny_atm)
      _RL qSensible_atm  (Nx_atm,Ny_atm)
      _RL qShortwave_atm (Nx_atm,Ny_atm)
      _RL qLongwave_atm  (Nx_atm,Ny_atm)
