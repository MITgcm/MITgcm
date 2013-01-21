C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_do_co2.F,v 1.8 2013/01/21 21:50:55 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_DO_CO2
C     !INTERFACE:
      SUBROUTINE AIM_DO_CO2( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_DO_CO2
C     | o CO2 budget of the atmosphere
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "AIM_CO2.h"
C-- Coupled to the Ocean :
#ifdef COMPONENT_MODULE
#include "CPL_PARAMS.h"
#include "ATMCPL.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: Current time of simulation ( s )
C     myIter :: Current iteration number in simulation
C     myThid :: Number of this instance of the routine
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_AIM
#ifdef ALLOW_AIM_CO2
C     !FUNCTIONS:
      INTEGER  ILNBLNK, IFNBLNK
      EXTERNAL ILNBLNK, IFNBLNK
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     bi,bj  - Tile index
C     i,j    - loop counters
      INTEGER bi, bj, i, j
      _RL total_flux, atpco2_check
      _RL flxCO2tile(nSx,nSy)
      LOGICAL permCheckPoint
      INTEGER iUnit, iLo, iHi
      _RS dummyRS(1)
      _RL tmpco2(2)
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef COMPONENT_MODULE
      IF ( useCoupler .AND. useImportFlxCO2 ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           aimflxCo2(i,j,bi,bj) = flxCO2ocn(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* COMPONENT_MODULE */

      IF ( Aim_CO2_Flag .EQ. 1 .AND. myTime .EQ. startTime ) THEN
        _BEGIN_MASTER(myThid)
        atm_pCO2 = atmpCO2init
        _END_MASTER(myThid)
        _BARRIER

      ELSEIF ( Aim_CO2_Flag .EQ. 2 ) THEN

C- First compute global mole flux at air-sea interface
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          flxCO2tile(bi,bj) = 0. _d 0
          DO j=1,sNy
           DO i=1,sNx
            flxCO2tile(bi,bj)=flxCO2tile(bi,bj) + aimflxCo2(i,j,bi,bj)
     &                      * rA(i,j,bi,bj) * deltaT
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL(flxCO2tile,total_flux,myThid)

        _BARRIER
        _BEGIN_MASTER(myThid)
        IF ( startTime.EQ.baseTime .AND. myTime.EQ.0 ) THEN
C- If  first iteration, use atmpCO2init as initial condition
          atm_pCO2 = atmpCO2init
          Atm_CO2_Moles = atm_pCO2 * total_atmos_moles

        ELSEIF ( myTime .EQ. startTime ) THEN
C- If restart, read moles number from pickup
          WRITE(fn,'(A,I10.10)') 'pickup_aimCo2.',nIter0
          iUnit = 0
          CALL MDS_READVEC_LOC(  fn, precFloat64, iUnit, 'RL', 2,
     O                           tmpco2, dummyRS,
     I                           0, 0, 1, myThid )
          Atm_CO2_Moles = tmpco2(1)
          atpco2_check  = tmpco2(2)
          atm_pCO2 = Atm_CO2_Moles / total_atmos_moles

          iUnit = standardMessageUnit
          iLo = IFNBLNK(fn)
          iHi = ILNBLNK(fn)
          WRITE(msgBuf,'(A)') ' '
          CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
          WRITE(msgBuf,'(A)') '// ==================================='
          CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
          WRITE(msgBuf,'(2A)') '// AIM_DO_CO2: Read pickup ',fn(iLo:iHi)
          CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

          CALL WRITE_0D_RL( atpco2_check, INDEX_NONE, 'atpco2_check =',
     &                     ' /* pCo2 from pickup file */')
          CALL WRITE_0D_RL( atm_pCO2, INDEX_NONE, 'atm_pCO2 =',
     &                     ' /* pCo2 from Atm_CO2_Moles */')

c        ELSE
c          WRITE(msgBuf,'(A)') 'AIM_DO_CO2: How did you end up here?'
c          CALL PRINT_ERROR( msgBuf , myThid)
c          STOP 'ABNORMAL END: S/R AIM_DO_CO2'
        ENDIF

C- Calculate new atmos pCO2
        Atm_CO2_Moles = Atm_CO2_Moles - total_flux
        atm_pCO2 = Atm_CO2_Moles / total_atmos_moles

C- Write out if time for a new pickup
        permCheckPoint = myTime.NE.startTime
        permCheckPoint = permCheckPoint .AND.
     &    DIFFERENT_MULTIPLE(pChkptFreq,myTime+deltaTClock,deltaTClock)
        IF (permCheckPoint) THEN
          WRITE(fn,'(A,I10.10)') 'pickup_aimCo2.',myIter+1
C- write values to new pickup
          tmpco2(1)= Atm_CO2_Moles
          tmpco2(2)= atm_pCO2
          iUnit = 0
          CALL MDS_WRITEVEC_LOC( fn, precFloat64, iUnit, 'RL', 2,
     I                           tmpco2, dummyRS,
     I                           0, 0, -1, myIter, myThid )
        ENDIF
        _END_MASTER(myThid)
        _BARRIER

C--- end of Aim_CO2_Flag IF.
      ENDIF

#endif /* ALLOW_AIM_CO2 */
#endif /* ALLOW_AIM */

      RETURN
      END
