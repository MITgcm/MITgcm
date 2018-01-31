#include "ATM_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_EXPORT_FIELDS
C     !INTERFACE:
      SUBROUTINE ATM_EXPORT_FIELDS( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_EXPORT_FIELDS
C     | o Routine for exporting atmos. surface coupling fields
C     |   to coupling layer.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CPL_PARAMS.h"
#include "ATMCPL.h"
#include "ATMIDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter  :: Current timestep number.
C     myThid  :: Thread number for this instance of the routine.
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf  :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Send Atmospheric fields to coupling layer
C  Note: 1) Information is sent on the atmos. model grid.
C        2) MIT Coupler checkpoint1 does not allow asynchronous posting of
C          data, so ordering has to be consistent with coupling layer ordering

C-    export Sea-Level pressure:
      CALL ATM_EXPORT_FLD( atmSLPrName,
     U                     atmSLPr, atmSLPrTime, myThid )
C-    export Net surface heat flux:
      CALL ATM_EXPORT_FLD( atmHeatFluxName,
     U                     heatFlux, HeatFluxTime, myThid )
C-    export Net Short-Wave surface heat flux:
      CALL ATM_EXPORT_FLD( atmQshortWaveName,
     U                     qShortWave, qShortWaveTime, myThid )
C-    export surface wind stress, Zonal & Meridional components:
      CALL ATM_EXPORT_FLD( atmTauXName,
     U                     tauX, tauXTime, myThid )
      CALL ATM_EXPORT_FLD( atmTauYName,
     U                     tauY, tauYTime, myThid )
C-    export Net fresh water flux (Evap - Precip):
      CALL ATM_EXPORT_FLD( atmEvMPrName,
     U                     EvMPrFlux, EvMPrTime, myThid )
#ifdef ALLOW_LAND
      IF ( atm_cplExch_RunOff ) THEN
C-    export Run Off fresh water flux:
       CALL ATM_EXPORT_FLD( atmRunOffName,
     U                      RunOffFlux, RunOffTime, myThid )
C-    export Energy flux associated with Run Off:
       CALL ATM_EXPORT_FLD( atmROEnFxName,
     U                      RunOffEnFx, ROEnFxTime, myThid )
      ENDIF
#endif /* ALLOW_LAND */
#ifdef ALLOW_THSICE
      IF ( atm_cplExch1W_sIce ) THEN
C-    export Salt flux (related to sea-ice melting/freezing):
       CALL ATM_EXPORT_FLD( atmSaltFxName,
     U                      iceSaltFlx, saltFxTime, myThid )
C-    export Sea-Ice Mass:
       CALL ATM_EXPORT_FLD( atmSIceMassName,
     U                      seaIceMass, sIceMassTime, myThid )
      ENDIF
      IF ( atm_cplExch_SaltPl ) THEN
C-    export Salt-Plume flux:
       CALL ATM_EXPORT_FLD( atmSaltPlmFlxName,
     U                      saltPlmFlx_cpl, saltPlmFlxTime, myThid )
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_AIM
      IF ( atm_cplExch_DIC ) THEN
C-    export atmospheric CO2:
       CALL ATM_EXPORT_FLD( atmAirCO2Name,
     U                      airCO2, airCO2Time, myThid )

C-    export surface wind speed:
       CALL ATM_EXPORT_FLD( atmWindSpdName,
     U                      sWSpeed, sWSpeedTime, myThid )
      ENDIF
#endif /* ALLOW_AIM */

#ifdef ALLOW_THSICE
      IF ( ( atm_cplExch1W_sIce.AND.atm_cplExch_DIC )
     &  .OR. atm_cplExch2W_sIce ) THEN
C-    export seaice fraction:
       CALL ATM_EXPORT_FLD( atmSIceFracName,
     U                      sIceFrac_cpl, sIceFracTime, myThid )
      ENDIF
      IF ( atm_cplExch2W_sIce ) THEN
C-    export seaice thickness:
       CALL ATM_EXPORT_FLD( atmSIceThickName,
     U                      sIceThick_cpl, sIceThickTime, myThid )
C-    export snow thickness:
       CALL ATM_EXPORT_FLD( atmSIceSnowName,
     U                      sIceSnowH_cpl, sIceSnowHTime, myThid )
C-    export seaice enthalpy (layer 1):
       CALL ATM_EXPORT_FLD( atmSIceQ1Name,
     U                      sIceQ1_cpl, sIceQ1Time, myThid )
C-    export seaice enthalpy (layer 2):
       CALL ATM_EXPORT_FLD( atmSIceQ2Name,
     U                      sIceQ2_cpl, sIceQ2Time, myThid )
      ENDIF
#endif /* ALLOW_THSICE */

#endif /* COMPONENT_MODULE */

      RETURN
      END
