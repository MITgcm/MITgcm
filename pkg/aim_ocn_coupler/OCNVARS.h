C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/OCNVARS.h,v 1.1 2003/12/15 02:28:00 jmc Exp $
C $Name:  $

C     /==========================================================\
C     | OCNVARS.h Declare arrays for holding data on the ocean   |
C     |           grid. Arrays may need adding or removing       |
C     |           different couplings.                           |
C     \==========================================================/
C     OcnDepths_ocn  - Ocean bathymetry on ocean grid ( m  )
C     AtmDepths_ocn  - Atmos. depth on ocean grid     ( Pa )
C     SST_ocn        - Sea surface temperature on ocean grid ( oC ).
C     HeatFlux_ocn   - Heat flux on ocean grid ( Wm^-2, positive out
C                      of ocean).
C     FWFlux_ocn     - Fresh water flux on ocean grid ( m/s, positive out
C                      of ocean).
C     TauX_ocn       - Zonal momentum flux on ocean grid ( N/m^2, same
C                      sign as the wind ; positive wind == westward
C                      flow.)
C     TauY_ocn       - Meridional momentum flux on ocean grid ( N/m^2, same
C                      sign as the wind ; positive wind == northward
C                      flow.)
      COMMON /OCNVARS_R/
     &       OcnDepths_ocn, AtmDepths_ocn,
     &       SST_ocn, HeatFlux_ocn, FWFlux_ocn,
     &       TauX_ocn, TauY_ocn,
     &       uVelGround_ocn, vVelGround_ocn,
     &       qLatent_ocn, qSensible_ocn,
     &       qLongwave_ocn, qShortwave_ocn
      _RS OcnDepths_ocn  (Nx_ocn,Ny_ocn)
      _RS AtmDepths_ocn  (Nx_ocn,Ny_ocn)
      _RL SST_ocn        (Nx_ocn,Ny_ocn)
      _RL HeatFlux_ocn   (Nx_ocn,Ny_ocn)
      _RL FWFlux_ocn     (Nx_ocn,Ny_ocn)
      _RL TauX_ocn       (Nx_ocn,Ny_ocn)
      _RL TauY_ocn       (Nx_ocn,Ny_ocn)
      _RL uVelGround_ocn (Nx_ocn,Ny_ocn)
      _RL vVelGround_ocn (Nx_ocn,Ny_ocn)
      _RL qLatent_ocn    (Nx_ocn,Ny_ocn)
      _RL qSensible_ocn  (Nx_ocn,Ny_ocn)
      _RL qLongwave_ocn  (Nx_ocn,Ny_ocn)
      _RL qShortwave_ocn (Nx_ocn,Ny_ocn)
