C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_do_co2.F,v 1.6 2009/09/28 16:09:36 dfer Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_DO_CO2( myTime, myIter, myThid)
C     *==========================================================*
C     | S/R AIM_DO_CO2                                           |
C     | o CO2 budget of the atmosphere                           |
C     *==========================================================*

      IMPLICIT NONE

C     == Global data ==
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

#include "AIM2DYN.h"
#include "AIM_CO2.h"

C-- Coupled to the Ocean :
#ifdef COMPONENT_MODULE
#include "CPL_PARAMS.h"
#include "ATMCPL.h"
#endif

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Current iteration number in simulation
C     myThid - Number of this instance of the routine
      INTEGER myIter, myThid
      _RL myTime
CEndOfInterface

#ifdef ALLOW_AIM
#ifdef ALLOW_AIM_CO2
      INTEGER  ILNBLNK, IFNBLNK, MDS_RECLEN
      EXTERNAL ILNBLNK, IFNBLNK, MDS_RECLEN

C     == Local variables ==
C     bi,bj  - Tile index
C     i,j    - loop counters
      INTEGER bi, bj, i, j
      INTEGER iUnit, length_of_rec
      _RL total_flux, atpco2_check
      Real*8 tmpco2(2)
      _RL flxCO2tile(nSx,nSy)
      LOGICAL permCheckPoint
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      CHARACTER*(MAX_LEN_FNAM) fn, msgBuf
      INTEGER ilo,ihi


#ifdef COMPONENT_MODULE
      IF ( useImportFlxCO2 ) THEN
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

      IF ( Aim_CO2_Flag .EQ. 1 .AND. myTime .EQ. starttime ) THEN
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
        IF ( startTime.EQ.baseTime .AND. nIter0.EQ.0 ) THEN
C- If  first iteration, use atmpCO2init as initial condition
          atm_pCO2 = atmpCO2init
          Atm_CO2_Moles = atm_pCO2 * total_atmos_moles

        ELSEIF ( myTime .EQ. starttime ) THEN
C- If restart, read moles number from pickup
          WRITE(fn,'(A,I10.10)') 'pickup_aimCo2.',nIter0
          ilo = IFNBLNK(fn)
          ihi = ILNBLNK(fn)
          CALL MDSFINDUNIT( iUnit, myThid )
          length_of_rec = MDS_RECLEN( precFloat64, 2, myThid )
          OPEN(UNIT=iUnit,FILE=fn(ilo:ihi),STATUS='old',
     &         FORM='UNFORMATTED',ACCESS='DIRECT',RECL=length_of_rec)
          READ(iUnit,rec=1) tmpco2
          CLOSE(iUnit)
#ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8( 2, tmpco2 )
#endif

          Atm_CO2_Moles = tmpco2(1)
          atpco2_check  = tmpco2(2)
          atm_pCO2 = Atm_CO2_Moles / total_atmos_moles

          iUnit = standardMessageUnit
          WRITE(msgBuf,'(A)') ' '
          CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
          WRITE(msgBuf,'(A)') '// ==================================='
          CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
          WRITE(msgBuf,'(2A)') '// AIM_DO_CO2: Read pickup ',fn(ilo:ihi)
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
        permCheckPoint = .FALSE.
        permCheckPoint =
     &  DIFFERENT_MULTIPLE(pChkptFreq,myTime+deltaTClock,deltaTClock)
     &  .AND. (myTime .NE. starttime)
        IF (permCheckPoint) THEN
          DO i = 1,MAX_LEN_FNAM
            fn(i:i) = ' '
          ENDDO
          WRITE(fn,'(A,I10.10)') 'pickup_aimCo2.',myIter+1
C- write values to new pickup
          CALL MDSFINDUNIT( iUnit, myThid )
          length_of_rec = MDS_RECLEN( precFloat64, 2, myThid )
          OPEN(UNIT=iUnit,FILE=fn,STATUS='unknown',
     &      FORM='UNFORMATTED',ACCESS='DIRECT',RECL=length_of_rec)
          tmpco2(1)= Atm_CO2_Moles
          tmpco2(2)= atm_pCO2
          WRITE(iUnit,rec=1) tmpco2
          CLOSE(iUnit)

        ENDIF
        _END_MASTER(myThid)
        _BARRIER

C--- end of Aim_CO2_Flag IF.
      ENDIF

#endif /* ALLOW_AIM_CO2 */
#endif /* ALLOW_AIM */

      RETURN
      END
