C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_export_import_data.F,v 1.3 2015/12/25 04:37:19 jmc Exp $
C $Name:  $

#include "ATM_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_EXPORT_IMPORT_DATA
C     !INTERFACE:
      SUBROUTINE CPL_EXPORT_IMPORT_DATA(
     I                      myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_EXPORT_IMPORT_DATA
C     | o Routine for controlling
C     |   - export of coupling data to coupler layer AND
C     |   - import of coupling data from coupler layer.
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
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "ATMCPL.h"
#include "ATMIDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: Current time in simulation.
C     myIter  :: Current timestep number.
C     myThid  :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi, bj  :: Tile indices
C     msgBuf  :: Informational/error message buffer
      INTEGER bi, bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Note MIT Coupler checkpoint1 does not allow asynchronous posting of
C     data, so ordering has to be consistent with coupling layer ordering.
C--------------------------------------------------------------------------
C     Send data to coupling layer

      IF ( MOD(myIter,cplSendFrq_iter).EQ.0 ) THEN
       _BEGIN_MASTER( myThid )
        IF ( debugMode .OR. countPrtExp.LT.maxNumberPrint ) THEN
         WRITE(msgBuf,'(A,I10)')
     &    '  Exporting atmospheric surf fluxes at iter=', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         countPrtExp = countPrtExp + 1
        ENDIF
       _END_MASTER( myThid )

C--   Store state variables (without averaging over cplSendFrq_iter time-steps)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
c          CALL ATM_STORE_DYNVARS( bi, bj, myTime, myIter, myThid )
           IF ( useThSIce ) THEN
            CALL ATM_STORE_THSICE( bi, bj, myTime, myIter, myThid )
           ENDIF
         ENDDO
        ENDDO

C-    export Sea-Level pressure:
        CALL ATM_EXPORT_FLD( atmSLPrName,
     U                       atmSLPr, atmSLPrTime, myThid )
C-    export Net surface heat flux:
        CALL ATM_EXPORT_FLD( atmHeatFluxName,
     U                       heatFlux, HeatFluxTime, myThid )
C-    export Net Short-Wave surface heat flux:
        CALL ATM_EXPORT_FLD( atmQshortWaveName,
     U                       qShortWave, qShortWaveTime, myThid )
C-    export surface wind stress, Zonal & Meridional components:
        CALL ATM_EXPORT_FLD( atmTauXName,
     U                       tauX, tauXTime, myThid )
        CALL ATM_EXPORT_FLD( atmTauYName,
     U                       tauY, tauYTime, myThid )
C-    export Net fresh water flux (Evap - Precip):
        CALL ATM_EXPORT_FLD( atmEvMPrName,
     U                       EvMPrFlux, EvMPrTime, myThid )
C-    export Run Off fresh water flux:
        CALL ATM_EXPORT_FLD( atmRunOffName,
     U                       RunOffFlux, RunOffTime, myThid )
C-    export Energy flux associated with Run Off:
        CALL ATM_EXPORT_FLD( atmROEnFxName,
     U                       RunOffEnFx, ROEnFxTime, myThid )
C-    export Salt flux (related to sea-ice melting/freezing):
        CALL ATM_EXPORT_FLD( atmSaltFxName,
     U                       iceSaltFlx, saltFxTime, myThid )
C-    export Sea-Ice Mass:
        CALL ATM_EXPORT_FLD( atmSIceMassName,
     U                       seaIceMass, seaIceTime, myThid )

        IF ( atm_cplExch_DIC ) THEN
C-    export atmospheric CO2:
         CALL ATM_EXPORT_FLD( atmAirCO2Name,
     U                        airCO2, airCO2Time, myThid )

C-    export surface wind speed:
         CALL ATM_EXPORT_FLD( atmWindSpdName,
     U                        sWSpeed, sWSpeedTime, myThid )

C-    export seaice fraction:
         CALL ATM_EXPORT_FLD( atmSIceFracName,
     U                        fracIce, fracIceTime, myThid )
        ENDIF

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Fetch data from coupling layer.

      IF ( MOD(myIter,cplSendFrq_iter).EQ.0 ) THEN
       _BEGIN_MASTER( myThid )
        IF ( debugMode .OR. countPrtImp.LT.maxNumberPrint ) THEN
         WRITE(msgBuf,'(A,I10)')
     &    '  Importing  oceanic surface fields at iter=', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         countPrtImp = countPrtImp + 1
        ENDIF
       _END_MASTER( myThid )

        CALL ATM_IMPORT_FIELDS( myThid )

      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
