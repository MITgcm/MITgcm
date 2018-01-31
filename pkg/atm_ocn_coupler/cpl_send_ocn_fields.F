#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_SEND_OCN_FIELDS
C     !INTERFACE:
      SUBROUTINE CPL_SEND_OCN_FIELDS( msgUnit, iter )

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
#include "CPL_PARAMS.h"
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     msgUnit   :: log-file I/O unit
C     iter      :: current iteration number
      INTEGER msgUnit, iter

C     !LOCAL VARIABLES:
      _RL tmpFld(Nx_ocn,Ny_ocn)
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
      IF ( cpl_exchange_RunOff.GE.2 ) THEN
C     Map Energy flux carried by RunOff onto ocean grid and substract it
C       from previous (direct) FWFlux => total Heat-Flux = Qnet - EnRunOff
        CALL ATM_TO_OCN_MAPRUNOFF(
     I           ROEnFx_atm,
     O           tmpFld,
     U           HeatFlux_ocn )
      ENDIF
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
      IF ( cpl_exchange_RunOff.GE.2 ) THEN
C     Map atmos. runOff from land onto ocean grid and substract it
C       from previous (direct) FWFlux => total FWFlux = E-P-R
        CALL ATM_TO_OCN_MAPRUNOFF(
     I           RunOff_atm,
     O           RunOff_ocn,
     U           FWFlux_ocn )
      ENDIF
C     Send E-P-R to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnFWFluxName,
     I                       Nx_ocn, Ny_ocn, FWFlux_ocn )

      IF ( MOD(cpl_exchange1W_sIce,2).EQ.1 ) THEN
C--   Map salt flux from sea-ice compon. onto ocean grid
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, SaltFlx_atm, Nx_ocn, Ny_ocn,
     O                         SaltFlx_ocn )
C     Send salt flux to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnSaltFxName,
     I                         Nx_ocn, Ny_ocn, SaltFlx_ocn )

C--   Map sea-ice mass onto ocean grid
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, sIceMass_atm, Nx_ocn, Ny_ocn,
     O                           sIceMass_ocn )
C     Send sea-ice mass to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnSIceMassName,
     I                         Nx_ocn, Ny_ocn, sIceMass_ocn )
      ENDIF

      IF ( MOD(cpl_exchange_SaltPl,2).EQ.1 ) THEN
C--   Map Salt-Plume flux onto ocean grid
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, saltPlmFlx_atm, Nx_ocn, Ny_ocn,
     O                           saltPlmFlx_ocn )
C     Send Salt-Plume flux to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnSaltPlmFlxName,
     I                         Nx_ocn, Ny_ocn, saltPlmFlx_ocn  )
      ENDIF

      IF ( MOD(cpl_exchange_RunOff,2).EQ.1 ) THEN
C     Send Run-Off flux to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnRunOffName,
     I                         Nx_ocn, Ny_ocn, RunOff_ocn  )
      ENDIF

      IF ( MOD(cpl_exchange_DIC,2).EQ.1 ) THEN
C--   Map atmos CO2 onto ocean grid
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, aCO2_atm, Nx_ocn, Ny_ocn,
     O                           aCO2_ocn )
C     Send atmos CO2 to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnAirCO2Name,
     I                         Nx_ocn, Ny_ocn, aCO2_ocn )

C--   Map surface wind speed onto ocean grid
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, wSpeed_atm, Nx_ocn, Ny_ocn,
     O                           wSpeed_ocn )
C     Send surface windspeed to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnWindSpdName,
     I                         Nx_ocn, Ny_ocn, wSpeed_ocn )
      ENDIF

      IF ( ( MOD(cpl_exchange1W_sIce,2).EQ.1
     &     .AND.MOD(cpl_exchange_DIC,2).EQ.1 )
     &  .OR. MOD(cpl_exchange2W_sIce,2).EQ.1 ) THEN
C--   Map seaice fraction from atmos. compon. onto ocean grid
       IF ( ( cpl_exchange1W_sIce.GE.2
     &      .AND.cpl_exchange_DIC.GE.2 )
     &   .OR. cpl_exchange2W_sIce.GE.2 ) THEN
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, SIceFrac_atm, Nx_ocn, Ny_ocn,
     O                           SIceFrac_ocn )
       ENDIF
C     Send sea-ice fraction to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnSIceFracName,
     I                         Nx_ocn, Ny_ocn, SIceFrac_ocn )
      ENDIF

      IF ( cpl_exchange2W_sIce.EQ.3 ) THEN
C--   Map other pkg/thsice fields onto ocean grid:
C     Map seaice thickness
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, sIceThick_atm, Nx_ocn, Ny_ocn,
     O                           sIceThick_ocn )
C     Map snow thickness
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, sIceSnowH_atm, Nx_ocn, Ny_ocn,
     O                           sIceSnowH_ocn )
C     Map seaice enthalpy (layer 1)
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, sIceQ1_atm, Nx_ocn, Ny_ocn,
     O                           sIceQ1_ocn )
C     Map seaice enthalpy (layer 2)
        CALL ATM_TO_OCN_MAPXYR8(
     I           Nx_atm, Ny_atm, sIceQ2_atm, Nx_ocn, Ny_ocn,
     O                           sIceQ2_ocn )
      ENDIF
      IF ( MOD(cpl_exchange2W_sIce,2).EQ.1 ) THEN
C--   Map other pkg/thsice fields to ocean component:
C     Send seaice thickness
        CALL COUPSEND_R8TILES( ocnCompName, ocnSIceThickName,
     I                         Nx_ocn, Ny_ocn, sIceThick_ocn )
C     Send snow thickness
        CALL COUPSEND_R8TILES( ocnCompName, ocnSIceSnowName,
     I                         Nx_ocn, Ny_ocn, sIceSnowH_ocn )
C     Send seaice enthalpy (layer 1)
        CALL COUPSEND_R8TILES( ocnCompName, ocnSIceQ1Name,
     I                         Nx_ocn, Ny_ocn, sIceQ1_ocn )
C     Send seaice enthalpy (layer 2)
        CALL COUPSEND_R8TILES( ocnCompName, ocnSIceQ2Name,
     I                         Nx_ocn, Ny_ocn, sIceQ2_ocn )
      ENDIF

      RETURN
      END
