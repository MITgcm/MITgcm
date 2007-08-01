C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_send_atm_fields.F,v 1.5 2007/08/01 00:53:37 dfer Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_SEND_ATM_FIELDS
C     !INTERFACE:
      SUBROUTINE CPL_SEND_ATM_FIELDS

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_SEND_ATM_FIELDS
C     | o Routine for sending ocean surface fields to atmos.
C     |   component.
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
#include "OCNSIZE.h"
#include "OCNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:

C     !LOCAL VARIABLES:

CEOP

C--   Map ocean mixed-layer depth onto atmos. grid
      CALL OCN_TO_ATM_MAPXYR8(
     I         Nx_ocn, Ny_ocn, OcMxlD_ocn, Nx_atm, Ny_atm,
     O                         OcMxlD_atm )
C     Send ocean mixed-layer depth to atmos. component
      CALL COUPSEND_R8TILES( atmCompName, atmMxlDName,
     I                       Nx_atm, Ny_atm, OcMxlD_atm )

C--   Map ocean surface temperature onto atmos. grid
      CALL OCN_TO_ATM_MAPXYR8(
     I         Nx_ocn, Ny_ocn, SST_ocn, Nx_atm, Ny_atm,
     O                         SST_atm )
C     Send SST to atmos. component
      CALL COUPSEND_R8TILES( atmCompName, atmSSTName,
     I                       Nx_atm, Ny_atm, SST_atm )

C--   Map ocean surface salinity onto atmos. grid
      CALL OCN_TO_ATM_MAPXYR8(
     I         Nx_ocn, Ny_ocn, SSS_ocn, Nx_atm, Ny_atm,
     O                         SSS_atm )
C     Send SSS to atmos. component
      CALL COUPSEND_R8TILES( atmCompName, atmSSSName,
     I                       Nx_atm, Ny_atm, SSS_atm )

C--   Map ocean surface velocity square onto atmos. grid
      CALL OCN_TO_ATM_MAPXYR8(
     I         Nx_ocn, Ny_ocn, vSq_ocn, Nx_atm, Ny_atm,
     O                         vSq_atm )
C     Send ocean surface velocity square to atmos. component
      CALL COUPSEND_R8TILES( atmCompName, atmSSVsqName,
     I                       Nx_atm, Ny_atm, vSq_atm )

      IF ( atmCpl_exchange_DIC ) THEN
C--   Map ocn->atm flux of CO2 onto atmos. grid
        CALL OCN_TO_ATM_MAPXYR8(
     I           Nx_ocn, Ny_ocn, fluxCO2_ocn, Nx_atm, Ny_atm,
     O                           fluxCO2_atm )
C     Send ocn->atm flux of CO2 to atmos. component
        CALL COUPSEND_R8TILES( atmCompName, atmFCO2Name,
     I                         Nx_atm, Ny_atm, fluxCO2_atm )
      ENDIF

      RETURN
      END
