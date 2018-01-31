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
#include "RESTART.h"
#include "GRID.h"

#include "AIM_PARAMS.h"
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
      LOGICAL modelEnd
      LOGICAL permPickup, tempPickup
      INTEGER iUnit, iLo, iHi
      _RS dummyRS(1)
      _RL tmpco2(2)
      CHARACTER*(10) suff
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

      IF ( aim_select_pCO2 .GE. 2 ) THEN

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
        IF ( myIter.EQ.0 ) THEN
C- If  first iteration, use atmpCO2init as initial condition
          atm_CO2_Moles = atm_pCO2 * total_atmos_moles

        ELSEIF ( myIter.EQ.nIter0 ) THEN
C- If restart, read moles number from pickup
          IF ( pickupSuff.EQ.' ' ) THEN
            IF ( rwSuffixType.EQ.0 ) THEN
              WRITE(fn,'(A,I10.10)') 'pickup_aimCo2.', myIter
            ELSE
              CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
              WRITE(fn,'(A,A)') 'pickup_aimCo2.', suff
            ENDIF
          ELSE
            WRITE(fn,'(A,A10)') 'pickup_aimCo2.', pickupSuff
          ENDIF
          iUnit = 0
          CALL MDS_READVEC_LOC(  fn, precFloat64, iUnit, 'RL', 2,
     O                           tmpco2, dummyRS,
     I                           0, 0, 1, myThid )
          atm_CO2_Moles = tmpco2(1)
          atpco2_check  = tmpco2(2)
          atm_pCO2 = atm_CO2_Moles / total_atmos_moles

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
     &                     ' /* pCo2 from atm_CO2_Moles */')
        ENDIF

C- Calculate new atmos pCO2
        atm_CO2_Moles = atm_CO2_Moles - total_flux
        atm_pCO2 = atm_CO2_Moles / total_atmos_moles

C- Set pCO2 for AIM Radiation:
        IF ( aim_select_pCO2 .EQ. 3 ) THEN
          aim_pCO2 = atm_pCO2
        ENDIF

C- Write out if time for a new pickup
        modelEnd = (myTime+deltaTClock).EQ.endTime
     &        .OR. (myIter+1).EQ.nEndIter
        permPickup = .FALSE.
        tempPickup = .FALSE.
        permPickup =
     &    DIFFERENT_MULTIPLE(pChkptFreq,myTime+deltaTClock,deltaTClock)
        tempPickup =
     &    DIFFERENT_MULTIPLE( chkptFreq,myTime+deltaTClock,deltaTClock)
        IF ( (modelEnd.AND.writePickupAtEnd)
     &       .OR. permPickup .OR. tempPickup ) THEN
          IF ( permPickup ) THEN
            IF ( rwSuffixType.EQ.0 ) THEN
              WRITE(fn,'(A,I10.10)') 'pickup_aimCo2.', myIter+1
            ELSE
              CALL RW_GET_SUFFIX( suff,
     &                    myTime+deltaTClock, myIter+1, myThid )
              WRITE(fn,'(A,A)') 'pickup_aimCo2.', suff
            ENDIF
          ELSE
            WRITE(fn,'(A,A)') 'pickup_aimCo2.', checkPtSuff(nCheckLev)
          ENDIF
C- write values to new pickup
          tmpco2(1)= atm_CO2_Moles
          tmpco2(2)= atm_pCO2
          iUnit = 0
          CALL MDS_WRITEVEC_LOC( fn, precFloat64, iUnit, 'RL', 2,
     I                           tmpco2, dummyRS,
     I                           0, 0, -1, myIter, myThid )
        ENDIF
        _END_MASTER(myThid)
        _BARRIER

C--- end if aim_select_pCO2 >= 2
      ENDIF

#endif /* ALLOW_AIM_CO2 */
#endif /* ALLOW_AIM */

      RETURN
      END
