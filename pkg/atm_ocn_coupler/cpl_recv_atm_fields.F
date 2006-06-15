C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_recv_atm_fields.F,v 1.3 2006/06/15 23:29:17 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_RECV_ATM_FIELDS
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_ATM_FIELDS                           |
C     | o Routine for receiving fluxes from atmos. component     |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model.                                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

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

      RETURN
      END
