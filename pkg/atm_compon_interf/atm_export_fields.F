C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_export_fields.F,v 1.1 2015/12/31 21:20:25 jmc Exp $
C $Name:  $

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
C-    export Run Off fresh water flux:
      CALL ATM_EXPORT_FLD( atmRunOffName,
     U                     RunOffFlux, RunOffTime, myThid )
C-    export Energy flux associated with Run Off:
      CALL ATM_EXPORT_FLD( atmROEnFxName,
     U                     RunOffEnFx, ROEnFxTime, myThid )
C-    export Salt flux (related to sea-ice melting/freezing):
      CALL ATM_EXPORT_FLD( atmSaltFxName,
     U                     iceSaltFlx, saltFxTime, myThid )
C-    export Sea-Ice Mass:
      CALL ATM_EXPORT_FLD( atmSIceMassName,
     U                     seaIceMass, seaIceTime, myThid )

      IF ( atm_cplExch_DIC ) THEN
C-    export atmospheric CO2:
       CALL ATM_EXPORT_FLD( atmAirCO2Name,
     U                      airCO2, airCO2Time, myThid )

C-    export surface wind speed:
       CALL ATM_EXPORT_FLD( atmWindSpdName,
     U                      sWSpeed, sWSpeedTime, myThid )

C-    export seaice fraction:
       CALL ATM_EXPORT_FLD( atmSIceFracName,
     U                      fracIce, fracIceTime, myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
