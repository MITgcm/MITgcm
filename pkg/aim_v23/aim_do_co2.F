C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_do_co2.F,v 1.1 2009/01/05 15:15:33 dfer Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_DO_CO2(
     I           bi, bj, myTime, myIter, myThid)
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

C     == Routine arguments ==
C     bi,bj  - Tile index
C     myTime - Current time of simulation ( s )
C     myIter - Current iteration number in simulation
C     myThid - Number of this instance of the routine
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEndOfInterface

#ifdef ALLOW_AIM
#ifdef ALLOW_AIM_CO2
C     == Local variables ==
C     i,j,k        :: loop counters
C     I2,Katm      :: loop counters
      INTEGER i,j,k
      INTEGER I2, Katm, iUnit
      INTEGER istate
      _RL total_flux, atpco2, atpco2_check
      _RL tmpco2(2)
      LOGICAL permCheckPoint
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      CHARACTER*(MAX_LEN_FNAM) fn, msgBuf
      INTEGER ilo,ihi
      INTEGER  ilnblnk,ifnblnk
      EXTERNAL ilnblnk,ifnblnk

#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

      IF ( Aim_CO2_Flag .EQ. 1 .AND. myTime .EQ. starttime ) THEN

        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          aim_CO2(I2,myThid)= atm_pCo2
         ENDDO
        ENDDO

      ELSEIF ( Aim_CO2_Flag .EQ. 2 ) THEN

        total_flux = 0. _d 0
C- First compute global mole flux at air-sea interface
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          total_flux = total_flux + aimflxCo2(I2,myThid)
     &               * rA(i,j,bi,bj) * deltaT
         ENDDO
        ENDDO
        _GLOBAL_SUM_R8(total_flux,myThid)


        IF ( startTime.EQ.baseTime .AND. nIter0.EQ.0 ) THEN
C- If  first iteration, use atm_pCo2 as initial condition
           DO j=1,sNy
            DO i=1,sNx
             I2 = i+(j-1)*sNx
             aim_CO2(I2,myThid)= atm_pCo2
            ENDDO
           ENDDO

        ELSEIF ( myTime .EQ. starttime ) THEN
C- If restart, read moles number from pickup
          _BEGIN_MASTER(myThid)
          WRITE(fn,'(A,I10.10)') 'pickup_aimCo2.',nIter0
          ilo = ifnblnk(fn)
          ihi = ilnblnk(fn)
          CALL MDSFINDUNIT( iUnit, myThid )
          OPEN(UNIT=iUnit,FILE=fn(ilo:ihi),STATUS='old',
     &      FORM='UNFORMATTED',ACCESS='DIRECT',RECL=2*WORDLENGTH*2)
          READ(iUnit,rec=1) tmpco2
          CLOSE(iUnit)
#ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8( 2, tmpco2 )
#endif
          _END_MASTER(myThid)
          _BARRIER

          Atm_CO2_Moles = tmpco2(1)
          atpco2_check  = tmpco2(2)
          atpco2 = Atm_CO2_Moles / total_atmos_moles

          iUnit = standardMessageUnit
          WRITE(msgBuf,'(A)') ' '
          CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
          WRITE(msgBuf,'(A)') '// ==================================='
          CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
          WRITE(msgBuf,'(2A)') '// AIM_DO_CO2: Read pickup ',fn(ilo:ihi)
          CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

          CALL WRITE_0D_R8( atpco2_check, INDEX_NONE, 'atpco2_check =',
     &     ' /* pCo2 from pickup file */')
          CALL WRITE_0D_R8( atpco2, INDEX_NONE, 'atpco2 =',
     &     ' /* pCo2 from Atm_CO2_Moles */')

c        ELSE
c          WRITE(msgBuf,'(A)') 'AIM_DO_CO2: How did you end up here?'
c          CALL PRINT_ERROR( msgBuf , myThid)
c          STOP 'ABNORMAL END: S/R AIM_DO_CO2'
        ENDIF

C- Calculate new atmos pCO2
        Atm_CO2_Moles = Atm_CO2_Moles - total_flux
        atpco2 = Atm_CO2_Moles / total_atmos_moles

        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          aim_CO2(I2,myThid)= atpCo2
         ENDDO
        ENDDO

C- Write out if time for a new pickup
        permCheckPoint = .FALSE.
        permCheckPoint =
     &  DIFFERENT_MULTIPLE(pChkptFreq,myTime+deltaTClock,deltaTClock)
     &  .AND. (myTime .NE. starttime)
        IF (permCheckPoint) THEN
          _BEGIN_MASTER(myThid)
          DO i = 1,MAX_LEN_FNAM
              fn(i:i) = ' '
          ENDDO
          WRITE(fn,'(A,I10.10)') 'pickup_aimCo2.',myIter+1
          _BARRIER
C- write values to new pickup
          CALL MDSFINDUNIT( iUnit, myThid )
          OPEN(UNIT=iUnit,FILE=fn,STATUS='new',
     &      FORM='UNFORMATTED',ACCESS='DIRECT',RECL=2*WORDLENGTH*2)
          tmpco2(1)= Atm_CO2_Moles
          tmpco2(2)= atpco2
          WRITE(iUnit,rec=1) tmpco2
          CLOSE(iUnit)
          _END_MASTER(myThid)
          _BARRIER

        ENDIF

C--- end of Aim_CO2_Flag IF.
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND.
     &       DIAGNOSTICS_IS_ON('aim_pCO2',myThid) ) THEN
         CALL DIAGNOSTICS_SCALE_FILL( aim_CO2, 1. _d 6, 1,
     &                  'aim_pCO2', 1, 1, 3,bi,bj, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */


#endif /* ALLOW_AIM_CO2 */
#endif /* ALLOW_AIM */

      RETURN
      END
