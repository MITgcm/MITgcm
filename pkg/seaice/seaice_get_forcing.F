C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE SEAICE_GET_FORCING( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_GET_FORCING                            |
C     | o Load atmospheric state and runoff.                     |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_FFIELDS.h"
 
C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
 
#ifdef ALLOW_SEAICE
#ifndef SEAICE_EXTERNAL_FORCING

C     === Local arrays ===
      COMMON /TDFIELDS_FLAGS/
     &                 wind0_is_first, flux0_is_first,
     &                 SSS0_is_first, SST0_is_first
      LOGICAL          wind0_is_first, flux0_is_first,
     &                 SSS0_is_first, SST0_is_first

C     === Local variables ===
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi,bj,i,j,iRec,iEnd
      _RL aWghtWind,bWghtWind,aWghtFlux,bWghtFlux,
     &     aWghtSSS,bWghtSSS,aWghtSST,bWghtSST
      _RS initValue
      _RL year, seconds, YearTime, four
      INTEGER CurrentYear, CurrentYear2
      logical done
      CHARACTER*(MAX_LEN_MBUF) fName

C--   Compute CurrentYear and YearTime
      four = 4.0
      YearTime = myTime
      done = .false.
      do year = StartingYear, EndingYear
         if( .not. done ) then
            if( mod(year,four) .eq. 0. ) then
               seconds = 366.*24.*60.*60.
            else
               seconds = 365.*24.*60.*60.
            endif
            if( YearTime-seconds .ge. 0. ) then
               YearTime = YearTime-seconds
            else
               CurrentYear = year
               done = .true.
            endif
         endif
      enddo
      if( CurrentYear.ge.2000 ) then
         CurrentYear2 = CurrentYear-2000
      else
         CurrentYear2 = CurrentYear-1900
      endif

C--   Check to see whether myTime is outside available forcing data
      IF(  CurrentYear.gt. EndingYear       .or.
     &     YearTime   .lt. WindForcingStart .or.
     &     YearTime   .gt. WindForcingEnd   .or.
     &     YearTime   .lt. FluxForcingStart .or.
     &     YearTime   .gt. FluxForcingEnd   .or.
     &     YearTime   .lt. SSTforcingStart  .or.
     &     YearTime   .gt. SSTforcingEnd    .or.
     &     YearTime   .lt. SSSforcingStart  .or.
     &     YearTime   .gt. SSSforcingEnd         ) THEN
         WRITE(msgBuf,'(A)') 'No Available Forcing Data'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SEAICE_GET_FORCING'
      ENDIF

C--   First call requires that we initialize everything for safety
      IF ( myIter .EQ. nIter0 ) THEN
       initValue = ZERO
       CALL INIT_ARRAY_RS( uwind0  , initValue, myThid )
       CALL INIT_ARRAY_RS( uwind1  , initValue, myThid )
       CALL INIT_ARRAY_RS( vwind0  , initValue, myThid )
       CALL INIT_ARRAY_RS( vwind1  , initValue, myThid )
       initValue = 283. _d 0
       CALL INIT_ARRAY_RS( atemp0  , initValue, myThid )
       CALL INIT_ARRAY_RS( atemp1  , initValue, myThid )
       initValue = 0.005 _d 0
       CALL INIT_ARRAY_RS( aqh0    , initValue, myThid )
       CALL INIT_ARRAY_RS( aqh1    , initValue, myThid )
       initValue = 300. _d 0
       CALL INIT_ARRAY_RS( lwdown0 , initValue, myThid )
       CALL INIT_ARRAY_RS( lwdown1 , initValue, myThid )
       initValue = 200. _d 0
       CALL INIT_ARRAY_RS( swdown0 , initValue, myThid )
       CALL INIT_ARRAY_RS( swdown1 , initValue, myThid )
       initValue = ZERO
       CALL INIT_ARRAY_RS( precip0 , initValue, myThid )
       CALL INIT_ARRAY_RS( precip1 , initValue, myThid )
       CALL INIT_ARRAY_RS( evap0   , initValue, myThid )
       CALL INIT_ARRAY_RS( evap1   , initValue, myThid )
       CALL INIT_ARRAY_RS( runoff0 , initValue, myThid )
       CALL INIT_ARRAY_RS( runoff1 , initValue, myThid )
       initValue = 35. _d 0
       CALL INIT_ARRAY_RS( SSSsi0  , initValue, myThid )
       CALL INIT_ARRAY_RS( SSSsi1  , initValue, myThid )
       initValue = 10. _d 0
       CALL INIT_ARRAY_RS( SSTsi0  , initValue, myThid )
       CALL INIT_ARRAY_RS( SSTsi1  , initValue, myThid )

       wind0_is_first = .TRUE.
       flux0_is_first = .TRUE.
       SSS0_is_first  = .TRUE.
       SST0_is_first  = .TRUE.

       _BEGIN_MASTER(myThid)
       write(0,*)
     &  'S/R SEAICE_GET_FORCING: initialize',myTime,myIter

       iRec = int((YearTime-WindForcingStart)/WindForcingPeriod) + 1
       IF ( uwindFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( uwindFile ) - 2
        WRITE(fName,'(A,I2.2)') uwindFile(1:iEnd), CurrentYear2
        CALL READ_REC_XY_RS( fName,uwind0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,uwind1,iRec+1,myIter,myThid )
       ENDIF
       IF ( vwindFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( vwindFile ) - 2
        WRITE(fName,'(A,I2.2)') vwindFile(1:iEnd), CurrentYear2
        CALL READ_REC_XY_RS( fName,vwind0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,vwind1,iRec+1,myIter,myThid )
       ENDIF

       iRec = int((YearTime-FluxForcingStart)/FluxForcingPeriod) + 1
       IF ( atempFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( atempFile ) - 4
        WRITE(fName,'(A,I4.4)') atempFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,atemp0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,atemp1,iRec+1,myIter,myThid )
       ENDIF
       IF ( aqhFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( aqhFile ) - 4
        WRITE(fName,'(A,I4.4)') aqhFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,aqh0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,aqh1,iRec+1,myIter,myThid )
       ENDIF
       IF ( lwdownFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( lwdownFile ) - 4
        WRITE(fName,'(A,I4.4)') lwdownFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,lwdown0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,lwdown1,iRec+1,myIter,myThid )
       ENDIF
       IF ( swdownFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( swdownFile ) - 4
        WRITE(fName,'(A,I4.4)') swdownFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,swdown0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,swdown1,iRec+1,myIter,myThid )
       ENDIF
       IF ( precipFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( precipFile ) - 4
        WRITE(fName,'(A,I4.4)') precipFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,precip0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,precip1,iRec+1,myIter,myThid )
       ENDIF
       IF ( evapFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( evapFile ) - 4
        WRITE(fName,'(A,I4.4)') evapFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,evap0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,evap1,iRec+1,myIter,myThid )
       ENDIF
       IF ( runoffFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( runoffFile ) - 4
        WRITE(fName,'(A,I4.4)') runoffFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,runoff0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,runoff1,iRec+1,myIter,myThid )
       ENDIF

       iRec = int((YearTime-SSTforcingStart)/SSTforcingPeriod) + 1
       IF ( thetaClimFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( thetaClimFile ) - 2
        WRITE(fName,'(A,I2.2)') thetaClimFile(1:iEnd), CurrentYear2
        CALL READ_REC_XY_RS( fName,SSTsi0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,SSTsi1,iRec+1,myIter,myThid )
       ENDIF

       iRec = int((YearTime-SSSforcingStart)/SSSforcingPeriod) + 1
       IF ( saltClimFile .NE. ' ' ) THEN
        CALL READ_REC_XY_RS( saltClimFile,SSSsi0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( saltClimFile,SSSsi1,iRec+1,myIter,myThid )
       ENDIF

       _END_MASTER(myThid)

       _EXCH_XY_R4( uwind0,  myThid )
       _EXCH_XY_R4( uwind1,  myThid )
       _EXCH_XY_R4( vwind0,  myThid )
       _EXCH_XY_R4( vwind1,  myThid )
       _EXCH_XY_R4( atemp0,  myThid )
       _EXCH_XY_R4( atemp1,  myThid )
       _EXCH_XY_R4( aqh0,    myThid )
       _EXCH_XY_R4( aqh1,    myThid )
       _EXCH_XY_R4( lwdown0, myThid )
       _EXCH_XY_R4( lwdown1, myThid )
       _EXCH_XY_R4( swdown0, myThid )
       _EXCH_XY_R4( swdown1, myThid )
       _EXCH_XY_R4( precip0, myThid )
       _EXCH_XY_R4( precip1, myThid )
       _EXCH_XY_R4( evap0,   myThid )
       _EXCH_XY_R4( evap1,   myThid )
       _EXCH_XY_R4( runoff0, myThid )
       _EXCH_XY_R4( runoff1, myThid )
       _EXCH_XY_R4( SSTsi0,  myThid )
       _EXCH_XY_R4( SSTsi1,  myThid )
       _EXCH_XY_R4( SSSsi0,  myThid )
       _EXCH_XY_R4( SSSsi1,  myThid )

      ENDIF

C--   Now calculate whether if it is time to update wind speed arrays
      iRec = int((YearTime-WindForcingStart)/WindForcingPeriod) + 2
      aWghtWind = mod(YearTime-WindForcingStart,WindForcingPeriod) /
     &        WindForcingPeriod
      bWghtWind=ONE-aWghtWind
      IF ( aWghtWind .EQ. 0 ) THEN
       _BEGIN_MASTER(myThid)
       write(0,*)
     &  'S/R SEAICE_GET_FORCING: reading winds',myTime,myIter
       IF ( uwindFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( uwindFile ) - 2
        WRITE(fName,'(A,I2.2)') uwindFile(1:iEnd), CurrentYear2
        IF (wind0_is_first) THEN
         CALL READ_REC_XY_RS( fName,uwind0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,uwind1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( vwindFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( vwindFile ) - 2
        WRITE(fName,'(A,I2.2)') vwindFile(1:iEnd), CurrentYear2
        IF (wind0_is_first) THEN
         CALL READ_REC_XY_RS( fName,vwind0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,vwind1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       _END_MASTER(myThid)
       IF (wind0_is_first) THEN
        _EXCH_XY_R4( uwind0, myThid )
        _EXCH_XY_R4( vwind0, myThid )
        wind0_is_first=.FALSE.
       ELSE
        _EXCH_XY_R4( uwind1, myThid )
        _EXCH_XY_R4( vwind1, myThid )
        wind0_is_first=.TRUE.
       ENDIF
      ENDIF

C--   Now calculate whether if it is time to update heat and freshwater flux
      iRec = int((YearTime-FluxForcingStart)/FluxForcingPeriod) + 2
      aWghtFlux = mod(YearTime-FluxForcingStart,FluxForcingPeriod) /
     &        FluxForcingPeriod
      bWghtFlux=ONE-aWghtFlux
      IF ( aWghtFlux .EQ. 0 ) THEN
       _BEGIN_MASTER(myThid)
       write(0,*)
     &  'S/R SEAICE_GET_FORCING: reading fluxes',myTime,myIter
       IF ( atempFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( atempFile ) - 4
        WRITE(fName,'(A,I4.4)') atempFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,atemp0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,atemp1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( aqhFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( aqhFile ) - 4
        WRITE(fName,'(A,I4.4)') aqhFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,aqh0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,aqh1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( lwdownFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( lwdownFile ) - 4
        WRITE(fName,'(A,I4.4)') lwdownFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,lwdown0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,lwdown1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( swdownFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( swdownFile ) - 4
        WRITE(fName,'(A,I4.4)') swdownFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,swdown0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,swdown1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( precipFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( precipFile ) - 4
        WRITE(fName,'(A,I4.4)') precipFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,precip0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,precip1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( evapFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( evapFile ) - 4
        WRITE(fName,'(A,I4.4)') evapFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,evap0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,evap1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( runoffFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( runoffFile ) - 4
        WRITE(fName,'(A,I4.4)') runoffFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,runoff0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,runoff1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       _END_MASTER(myThid)
       IF (flux0_is_first) THEN
        _EXCH_XY_R4(atemp0,  myThid )
        _EXCH_XY_R4(aqh0,    myThid )
        _EXCH_XY_R4(lwdown0, myThid )
        _EXCH_XY_R4(swdown0, myThid )
        _EXCH_XY_R4(precip0, myThid )
        _EXCH_XY_R4(evap0,   myThid )
        _EXCH_XY_R4(runoff0, myThid )
        flux0_is_first=.FALSE.
       ELSE
        _EXCH_XY_R4(atemp1,  myThid )
        _EXCH_XY_R4(aqh1,    myThid )
        _EXCH_XY_R4(lwdown1, myThid )
        _EXCH_XY_R4(swdown1, myThid )
        _EXCH_XY_R4(precip1, myThid )
        _EXCH_XY_R4(evap1,   myThid )
        _EXCH_XY_R4(runoff1, myThid )
        flux0_is_first=.TRUE.
       ENDIF
      ENDIF

C--   Now calculate whether if it is time to update SST array
      iRec = int((YearTime-SSTforcingStart)/SSTforcingPeriod) + 2
      aWghtSST = mod(YearTime-SSTforcingStart,SSTforcingPeriod) /
     &        SSTforcingPeriod
      bWghtSST=ONE-aWghtSST
      IF ( aWghtSST .EQ. 0 .AND. thetaClimFile .NE. ' ' ) THEN
       _BEGIN_MASTER(myThid)
       write(0,*) 'S/R SEAICE_GET_FORCING: reading SST',myTime,myIter
       iEnd = ILNBLNK( thetaClimFile ) - 2
       WRITE(fName,'(A,I2.2)') thetaClimFile(1:iEnd), CurrentYear2
       IF (SST0_is_first) THEN
        CALL READ_REC_XY_RS( fName,SSTsi0,iRec,myIter,myThid )
       ELSE
        CALL READ_REC_XY_RS( fName,SSTsi1,iRec,myIter,myThid )
       ENDIF
       _END_MASTER(myThid)
       IF (SST0_is_first) THEN
        _EXCH_XY_R4( SSTsi0, myThid )
        SST0_is_first=.FALSE.
       ELSE
        _EXCH_XY_R4( SSTsi1, myThid )
        SST0_is_first=.TRUE.
       ENDIF
      ENDIF

C--   Now calculate whether if it is time to update SSS array
      iRec = int((YearTime-SSSforcingStart)/SSSforcingPeriod) + 2
      aWghtSSS = mod(YearTime-SSSforcingStart,SSSforcingPeriod) /
     &        SSSforcingPeriod
      bWghtSSS=ONE-aWghtSSS
      IF ( aWghtSSS .EQ. 0 .AND. saltClimFile .NE. ' ') THEN
       _BEGIN_MASTER(myThid)
       write(0,*) 'S/R SEAICE_GET_FORCING: reading SSS',myTime,myIter
       IF (SSS0_is_first) THEN
        CALL READ_REC_XY_RS( saltClimFile,SSSsi0,iRec,myIter,myThid )
       ELSE
        CALL READ_REC_XY_RS( saltClimFile,SSSsi1,iRec,myIter,myThid )
       ENDIF
       _END_MASTER(myThid)
       IF (SSS0_is_first) THEN
        _EXCH_XY_R4( SSSsi0, myThid )
        SSS0_is_first=.FALSE.
       ELSE
        _EXCH_XY_R4( SSSsi1, myThid )
        SSS0_is_first=.TRUE.
       ENDIF
      ENDIF

C--   Time interpolation of wind forcing variables.
      IF (wind0_is_first) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           uwind(i,j,bi,bj) = bWghtWind *  uwind0(i,j,bi,bj) +
     &                        aWghtWind *  uwind1(i,j,bi,bj)
           vwind(i,j,bi,bj) = bWghtWind *  vwind0(i,j,bi,bj) +
     &                        aWghtWind *  vwind1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           uwind(i,j,bi,bj) = aWghtWind *  uwind0(i,j,bi,bj) +
     &                        bWghtWind *  uwind1(i,j,bi,bj)
           vwind(i,j,bi,bj) = aWghtWind *  vwind0(i,j,bi,bj) +
     &                        bWghtWind *  vwind1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Time interpolation of flux forcing variables.
      IF (flux0_is_first) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
          atemp(i,j,bi,bj)  = bWghtFlux *  atemp0(i,j,bi,bj) +
     &                        aWghtFlux *  atemp1(i,j,bi,bj)
          aqh(i,j,bi,bj)    = bWghtFlux *    aqh0(i,j,bi,bj) +
     &                        aWghtFlux *    aqh1(i,j,bi,bj)
          lwdown(i,j,bi,bj) = bWghtFlux * lwdown0(i,j,bi,bj) +
     &                        aWghtFlux * lwdown1(i,j,bi,bj)
          swdown(i,j,bi,bj) = bWghtFlux * swdown0(i,j,bi,bj) +
     &                        aWghtFlux * swdown1(i,j,bi,bj)
          precip(i,j,bi,bj) = bWghtFlux * precip0(i,j,bi,bj) +
     &                        aWghtFlux *   precip1(i,j,bi,bj)
          evap(i,j,bi,bj)   = bWghtFlux *   evap0(i,j,bi,bj) +
     &                        aWghtFlux *   evap1(i,j,bi,bj)
          runoff(i,j,bi,bj) = bWghtFlux * runoff0(i,j,bi,bj) +
     &                        aWghtFlux * runoff1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
          atemp(i,j,bi,bj)  = aWghtFlux *  atemp0(i,j,bi,bj) +
     &                        bWghtFlux *  atemp1(i,j,bi,bj)
          aqh(i,j,bi,bj)    = aWghtFlux *    aqh0(i,j,bi,bj) +
     &                        bWghtFlux *    aqh1(i,j,bi,bj)
          lwdown(i,j,bi,bj) = aWghtFlux * lwdown0(i,j,bi,bj) +
     &                        bWghtFlux * lwdown1(i,j,bi,bj)
          swdown(i,j,bi,bj) = aWghtFlux * swdown0(i,j,bi,bj) +
     &                        bWghtFlux * swdown1(i,j,bi,bj)
          precip(i,j,bi,bj) = aWghtFlux * precip0(i,j,bi,bj) +
     &                        bWghtFlux * precip1(i,j,bi,bj)
          evap(i,j,bi,bj)   = aWghtFlux *   evap0(i,j,bi,bj) +
     &                        bWghtFlux *   evap1(i,j,bi,bj)
          runoff(i,j,bi,bj) = aWghtFlux * runoff0(i,j,bi,bj) +
     &                        bWghtFlux * runoff1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Time interpolation of SSS forcing variables.
      IF (SSS0_is_first) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           SSS(i,j,bi,bj)   = bWghtSSS  *   SSSsi0(i,j,bi,bj) +
     &                        aWghtSSS  *   SSSsi1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           SSS(i,j,bi,bj)   = aWghtSSS  *   SSSsi0(i,j,bi,bj) +
     &                        bWghtSSS  *   SSSsi1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Time interpolation of SST forcing variables.
      IF (SST0_is_first) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           SST(i,j,bi,bj)   = bWghtSST  *   SSTsi0(i,j,bi,bj) +
     &                        aWghtSST  *   SSTsi1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           SST(i,j,bi,bj)   = aWghtSST  *   SSTsi0(i,j,bi,bj) +
     &                        bWghtSST  *   SSTsi1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* SEAICE_EXTERNAL_FORCING */
#endif /* ALLOW_SEAICE */

      RETURN
      END


C=======================================================================

      SUBROUTINE INIT_ARRAY_RS( arr, initValue, myThid )
C     This routine sets the RS array arr to initValue
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
C     === Arguments ===
      _RS arr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS initValue
      INTEGER myThid

#ifdef ALLOW_SEAICE
#ifndef SEAICE_EXTERNAL_FORCING

C     === Local variables ===
      INTEGER i,j,bi,bj
C
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO  j = 1-Oly,sNy+Oly
         DO  i = 1-Olx,sNx+Olx
          arr(i,j,bi,bj) = initValue
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_EXTERNAL_FORCING */
#endif /* ALLOW_SEAICE */

      RETURN
      END
