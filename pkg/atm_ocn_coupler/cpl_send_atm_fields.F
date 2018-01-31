#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_SEND_ATM_FIELDS
C     !INTERFACE:
      SUBROUTINE CPL_SEND_ATM_FIELDS( msgUnit, iter )

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
#include "CPL_PARAMS.h"
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     msgUnit   :: log-file I/O unit
C     iter      :: current iteration number
      INTEGER msgUnit, iter

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

      IF ( cpl_exchange_DIC.GE.2 ) THEN
C--   Map ocn->atm flux of CO2 onto atmos. grid
        CALL OCN_TO_ATM_MAPXYR8(
     I           Nx_ocn, Ny_ocn, fluxCO2_ocn, Nx_atm, Ny_atm,
     O                           fluxCO2_atm )
C     Send ocn->atm flux of CO2 to atmos. component
        CALL COUPSEND_R8TILES( atmCompName, atmCO2FluxName,
     I                         Nx_atm, Ny_atm, fluxCO2_atm )
      ENDIF

      IF ( cpl_exchange2W_sIce.EQ.3 ) THEN
C--   Map pkg/thsice fields onto atmos. grid:
C     Map seaice fraction from ocean compon.
        CALL OCN_TO_ATM_MAPXYR8(
     I           Nx_ocn, Ny_ocn, SIceFrac_ocn, Nx_atm, Ny_atm,
     O                           SIceFrac_atm )
C     Map seaice thickness
        CALL OCN_TO_ATM_MAPXYR8(
     I           Nx_ocn, Ny_ocn, sIceThick_ocn, Nx_atm, Ny_atm,
     O                           sIceThick_atm )
C     Map snow thickness
        CALL OCN_TO_ATM_MAPXYR8(
     I           Nx_ocn, Ny_ocn, sIceSnowH_ocn, Nx_atm, Ny_atm,
     O                           sIceSnowH_atm )
C     Map seaice enthalpy (layer 1)
        CALL OCN_TO_ATM_MAPXYR8(
     I           Nx_ocn, Ny_ocn, sIceQ1_ocn, Nx_atm, Ny_atm,
     O                           sIceQ1_atm )
C     Map seaice enthalpy (layer 2)
        CALL OCN_TO_ATM_MAPXYR8(
     I           Nx_ocn, Ny_ocn, sIceQ2_ocn, Nx_atm, Ny_atm,
     O                           sIceQ2_atm )
      ENDIF
      IF ( cpl_exchange2W_sIce.GE.2 ) THEN
C--   Send pkg/thsice fields to atmos. component:
C     Send sea-ice fraction
        CALL COUPSEND_R8TILES( atmCompName, atmSIceFracName,
     I                         Nx_atm, Ny_atm, SIceFrac_atm )
C     Send seaice thickness
        CALL COUPSEND_R8TILES( atmCompName, atmSIceThickName,
     I                         Nx_atm, Ny_atm, sIceThick_atm )
C     Send snow thickness
        CALL COUPSEND_R8TILES( atmCompName, atmSIceSnowName,
     I                         Nx_atm, Ny_atm, sIceSnowH_atm )
C     Send seaice enthalpy (layer 1)
        CALL COUPSEND_R8TILES( atmCompName, atmSIceQ1Name,
     I                         Nx_atm, Ny_atm, sIceQ1_atm )
C     Send seaice enthalpy (layer 2)
        CALL COUPSEND_R8TILES( atmCompName, atmSIceQ2Name,
     I                         Nx_atm, Ny_atm, sIceQ2_atm )
      ENDIF

      RETURN
      END
