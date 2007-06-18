C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_recv_atm_fields.F,v 1.4 2007/06/18 21:20:35 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_RECV_ATM_FIELDS
C     !INTERFACE:
      SUBROUTINE CPL_RECV_ATM_FIELDS

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
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"

C     !INPUT/OUTPUT PARAMETERS:

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

C-    Receive fresh water flux (=RunOff) from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmRunOffName,
     I                       Nx_atm, Ny_atm,
     O                       RunOff_atm )

C-    Receive Energy carried by RunOff from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmROEnFxName,
     I                       Nx_atm, Ny_atm,
     O                       ROEnFx_atm )

C-    Receive salt flux (<- sea-ice) from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmSaltFxName,
     I                       Nx_atm, Ny_atm,
     O                       SaltFlx_atm )

C-    Receive sea-ice mass from atmos. component
      CALL COUPRECV_R8TILES( atmCompName, atmSeaIceName,
     I                       Nx_atm, Ny_atm,
     O                       sIceMass_atm )

      IF ( atmCpl_exchange_DIC ) THEN

C-    Receive atmos CO2 from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmAirCO2Name,
     I                         Nx_atm, Ny_atm,
     O                         aCO2_atm )

C-    Receive surface wind speed from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmWSpdName,
     I                         Nx_atm, Ny_atm,
     O                         wSpeed_atm )

C-    Receive seaice fraction from atmos. component
        CALL COUPRECV_R8TILES( atmCompName, atmFIceName,
     I                         Nx_atm, Ny_atm,
     O                         sIceFrac_atm )

      ENDIF

      RETURN
      END
