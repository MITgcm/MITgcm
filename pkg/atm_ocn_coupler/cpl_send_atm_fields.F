C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_send_atm_fields.F,v 1.1 2006/06/15 23:05:26 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_SEND_ATM_FIELDS
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_ATM_FIELDS                           |
C     | o Routine for sending ocean surface fields to atmos.     |
C     |   component.                                             |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model.                                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

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

      RETURN
      END
