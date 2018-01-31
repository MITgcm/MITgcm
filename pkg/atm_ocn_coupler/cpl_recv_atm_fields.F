#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_RECV_ATM_FIELDS
C     !INTERFACE:
      SUBROUTINE CPL_RECV_ATM_FIELDS( msgUnit, iter )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_RECV_ATM_FIELDS
C     | o Routine for receiving fluxes from atmos. component
C     *==========================================================*
C     | This version talks to the MITgcm general circulation
C     | model.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "CPL_PARAMS.h"
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     msgUnit   :: log-file I/O unit
C     iter      :: current iteration number
      INTEGER msgUnit, iter

C     !LOCAL VARIABLES:
CEOP

C-    Receive Sea Level atmos. pressure from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmSLPrName,
     I                       Nx_atm, Ny_atm,
     O                       atmSLPr_atm )

C-    Receive (net) Heat Flux from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmHeatFluxName,
     I                       Nx_atm, Ny_atm,
     O                       HeatFlux_atm )

C-    Receive net shortwave radiation from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmQshortWaveName,
     I                       Nx_atm, Ny_atm,
     O                       qShortwave_atm   )

C-    Receive Zonal momentum flux from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmTauXName,
     I                       Nx_atm, Ny_atm,
     O                       TauX_atm )

C-    Receive Meridional momentum flux from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmTauYName,
     I                       Nx_atm, Ny_atm,
     O                       TauY_atm )

C-    Receive fresh water flux (=Evap-Precip) from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmEvMPrName,
     I                       Nx_atm, Ny_atm,
     O                       EvMPr_atm )

      IF ( cpl_exchange_RunOff.GE.2 ) THEN
C-    Receive fresh water flux (=RunOff) from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmRunOffName,
     I                         Nx_atm, Ny_atm,
     O                         RunOff_atm )

C-    Receive Energy carried by RunOff from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmROEnFxName,
     I                         Nx_atm, Ny_atm,
     O                         ROEnFx_atm )
      ENDIF

      IF ( cpl_exchange1W_sIce.GE.2 ) THEN
C-    Receive salt flux (<- sea-ice) from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmSaltFxName,
     I                         Nx_atm, Ny_atm,
     O                         SaltFlx_atm )

C-    Receive sea-ice mass from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmSIceMassName,
     I                         Nx_atm, Ny_atm,
     O                         sIceMass_atm )
      ENDIF

      IF ( cpl_exchange_SaltPl.GE.2 ) THEN
C-    Receive Salt-Plume flux from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmSaltPlmFlxName,
     I                         Nx_atm, Ny_atm,
     O                         saltPlmFlx_atm )
      ENDIF

      IF ( cpl_exchange_DIC.GE.2 ) THEN
C-    Receive atmos CO2 from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmAirCO2Name,
     I                         Nx_atm, Ny_atm,
     O                         aCO2_atm )

C-    Receive surface wind speed from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmWindSpdName,
     I                         Nx_atm, Ny_atm,
     O                         wSpeed_atm )
      ENDIF

      IF ( ( cpl_exchange1W_sIce.GE.2
     &     .AND.cpl_exchange_DIC.GE.2 )
     &  .OR. cpl_exchange2W_sIce.GE.2 ) THEN
C-    Receive seaice fraction from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmSIceFracName,
     I                         Nx_atm, Ny_atm,
     O                         sIceFrac_atm )
      ENDIF

      IF ( cpl_exchange2W_sIce.GE.2 ) THEN
C-    Receive seaice thickness from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmSIceThickName,
     I                         Nx_atm, Ny_atm,
     O                         sIceThick_atm )

C-    Receive snow thickness from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmSIceSnowName,
     I                         Nx_atm, Ny_atm,
     O                         sIceSnowH_atm )

C-    Receive seaice enthalpy (layer 1) from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmSIceQ1Name,
     I                         Nx_atm, Ny_atm,
     O                         sIceQ1_atm )

C-    Receive seaice enthalpy (layer 2) from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmSIceQ2Name,
     I                         Nx_atm, Ny_atm,
     O                         sIceQ2_atm )
      ENDIF

      RETURN
      END
