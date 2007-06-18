C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_send_ocn_fields.F,v 1.5 2007/06/18 21:20:35 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_SEND_OCN_FIELDS
C     !INTERFACE:
      SUBROUTINE CPL_SEND_OCN_FIELDS

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_SEND_OCN_FIELDS
C     | o Routine for sending surface flux to ocean component
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
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     !INPUT/OUTPUT PARAMETERS:

C     !LOCAL VARIABLES:

CEOP

C--   Map Sea Level atmos. pressure onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I         Nx_atm, Ny_atm, atmSLPr_atm, Nx_ocn, Ny_ocn,
     O                         atmSLPr_ocn )
C     Send Sea Level atmos. pressure to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnSLPrName,
     I                       Nx_ocn, Ny_ocn, atmSLPr_ocn )

C--   Map heatflux onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I         Nx_atm, Ny_atm, HeatFlux_atm, Nx_ocn, Ny_ocn,
     O                         HeatFlux_ocn )
C     Map Energy flux carried by RunOff onto ocean grid and substract it
C       from previous (direct) FWFlux => total Heat-Flux = Qnet - EnRunOff
      CALL ATM_TO_OCN_MAPRUNOFF(
     I     ROEnFx_atm,
     U     HeatFlux_ocn)
C     Send (net) heatflux. to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnHeatFluxName,
     I                       Nx_ocn, Ny_ocn, HeatFlux_ocn )

C--   Map net shortwave radiation onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I         Nx_atm, Ny_atm, qShortWave_atm, Nx_ocn, Ny_ocn,
     O                         qShortWave_ocn )
C     Send net shortwave radiation to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnQshortWaveName,
     I                       Nx_ocn, Ny_ocn, qShortWave_ocn )

C--   Map Zonal momentum flux onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I         Nx_atm, Ny_atm, TauX_atm, Nx_ocn, Ny_ocn,
     O                         TauX_ocn )
C     Send Zonal momentum flux to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnTauXName,
     I                       Nx_ocn, Ny_ocn, TauX_ocn )

C--   Map Meridional momentum flux onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I         Nx_atm, Ny_atm, TauY_atm, Nx_ocn, Ny_ocn,
     O                         TauY_ocn )
C     Send Meridional momentum flux to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnTauYName,
     I                       Nx_ocn, Ny_ocn, TauY_ocn )

C--   Map atmos. fresh water flux onto ocean grid : Evap - Precip
      CALL ATM_TO_OCN_MAPXYR8(
     I         Nx_atm, Ny_atm, EvMPr_atm, Nx_ocn, Ny_ocn,
     O                         FWFlux_ocn )
C     Map atmos. runOff from land onto ocean grid and substract it
C       from previous (direct) FWFlux => total FWFlux = E-P-R
      CALL ATM_TO_OCN_MAPRUNOFF(
     I     RunOff_atm,
     U     FWFlux_ocn)
C     Send E-P-R to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnFWFluxName,
     I                       Nx_ocn, Ny_ocn, FWFlux_ocn )

C--   Map salt flux from sea-ice compon. onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I         Nx_atm, Ny_atm, SaltFlx_atm, Nx_ocn, Ny_ocn,
     O                         SaltFlx_ocn )
C     Send salt flux to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnSaltFxName,
     I                       Nx_ocn, Ny_ocn, SaltFlx_ocn )

C--   Map sea-ice mass onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I         Nx_atm, Ny_atm, sIceMass_atm, Nx_ocn, Ny_ocn,
     O                         sIceMass_ocn )
C     Send sea-ice mass to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnSeaIceName,
     I                       Nx_ocn, Ny_ocn, sIceMass_ocn )

      IF ( ocnCpl_exchange_DIC ) THEN

C--   Map atmos CO2 onto ocean grid
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, aCO2_atm, Nx_ocn, Ny_ocn,
     O                           aCO2_ocn )
C     Send atmos CO2 to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnAirCO2Name,
     I                         Nx_ocn, Ny_ocn, aCO2_ocn )

C--   Map surf wind speed onto ocean grid
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, wSpeed_atm, Nx_ocn, Ny_ocn,
     O                           wSpeed_ocn )
C     Send surface windspeed to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnWSpdName,
     I                         Nx_ocn, Ny_ocn, wSpeed_ocn )

C--   Map seaice fraction from sea-ice compon. onto ocean grid
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, SIceFrac_atm, Nx_ocn, Ny_ocn,
     O                           SIceFrac_ocn )
C     Send sea-ice fraction to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnFIceName,
     I                         Nx_ocn, Ny_ocn, SIceFrac_ocn )

      ENDIF

      RETURN
      END
