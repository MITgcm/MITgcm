C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE SEAICE_GET_FORCING( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_GET_FORCING                            |
C     | o Load wind, thermal, and evaporation minus              |
C     |   precipitation fields for sea ice model.                |
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
      _RL year, seconds, YearTime
      INTEGER CurrentYear, CurrentYear2
      logical done
      CHARACTER*(MAX_LEN_MBUF) fName

C--   Compute CurrentYear and YearTime
      YearTime = myTime
      done = .false.
      do year = StartingYear, EndingYear
         if( .not. done ) then
            if( mod(year,4.) .eq. 0. ) then
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
       initValue = 0.
       CALL INIT_ARRAY_RS( gairx0 , initValue, myThid )
       CALL INIT_ARRAY_RS( gairx1 , initValue, myThid )
       CALL INIT_ARRAY_RS( gairy0 , initValue, myThid )
       CALL INIT_ARRAY_RS( gairy1 , initValue, myThid )
       initValue = 283.
       CALL INIT_ARRAY_RS( tair0  , initValue, myThid )
       CALL INIT_ARRAY_RS( tair1  , initValue, myThid )
       initValue = 0.005
       CALL INIT_ARRAY_RS( qa0    , initValue, myThid )
       CALL INIT_ARRAY_RS( qa1    , initValue, myThid )
       initValue = 300.
       CALL INIT_ARRAY_RS( flo0   , initValue, myThid )
       CALL INIT_ARRAY_RS( flo1   , initValue, myThid )
       initValue = 200.
       CALL INIT_ARRAY_RS( fsh0   , initValue, myThid )
       CALL INIT_ARRAY_RS( fsh1   , initValue, myThid )
       initValue = 0.
       CALL INIT_ARRAY_RS( rain0  , initValue, myThid )
       CALL INIT_ARRAY_RS( rain1  , initValue, myThid )
       CALL INIT_ARRAY_RS( evap0  , initValue, myThid )
       CALL INIT_ARRAY_RS( evap1  , initValue, myThid )
       CALL INIT_ARRAY_RS( runoff0, initValue, myThid )
       CALL INIT_ARRAY_RS( runoff1, initValue, myThid )
       initValue = 35.
       CALL INIT_ARRAY_RS( SSSsi0   , initValue, myThid )
       CALL INIT_ARRAY_RS( SSSsi1   , initValue, myThid )
       initValue = 10.
       CALL INIT_ARRAY_RS( SSTsi0   , initValue, myThid )
       CALL INIT_ARRAY_RS( SSTsi1   , initValue, myThid )

       wind0_is_first = .TRUE.
       flux0_is_first = .TRUE.
       SSS0_is_first  = .TRUE.
       SST0_is_first  = .TRUE.

       _BEGIN_MASTER(myThid)
       write(0,*)
     &  'S/R SEAICE_GET_FORCING: initialize',myTime,myIter

       iRec = int((YearTime-WindForcingStart)/WindForcingPeriod) + 1
       IF ( gairxFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( gairxFile ) - 2
        WRITE(fName,'(A,I2.2)') gairxFile(1:iEnd), CurrentYear2
        CALL READ_REC_XY_RS( fName,gairx0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,gairx1,iRec+1,myIter,myThid )
       ENDIF
       IF ( gairyFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( gairyFile ) - 2
        WRITE(fName,'(A,I2.2)') gairyFile(1:iEnd), CurrentYear2
        CALL READ_REC_XY_RS( fName,gairy0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,gairy1,iRec+1,myIter,myThid )
       ENDIF

       iRec = int((YearTime-FluxForcingStart)/FluxForcingPeriod) + 1
       IF ( tairFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( tairFile ) - 4
        WRITE(fName,'(A,I4.4)') tairFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,tair0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,tair1,iRec+1,myIter,myThid )
       ENDIF
       IF ( qaFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( qaFile ) - 4
        WRITE(fName,'(A,I4.4)') qaFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,qa0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,qa1,iRec+1,myIter,myThid )
       ENDIF
       IF ( floFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( floFile ) - 4
        WRITE(fName,'(A,I4.4)') floFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,flo0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,flo1,iRec+1,myIter,myThid )
       ENDIF
       IF ( fshFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( fshFile ) - 4
        WRITE(fName,'(A,I4.4)') fshFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,fsh0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,fsh1,iRec+1,myIter,myThid )
       ENDIF
       IF ( rainFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( rainFile ) - 4
        WRITE(fName,'(A,I4.4)') rainFile(1:iEnd), CurrentYear
        CALL READ_REC_XY_RS( fName,rain0,iRec  ,myIter,myThid )
        CALL READ_REC_XY_RS( fName,rain1,iRec+1,myIter,myThid )
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

       _EXCH_XY_R4( gairx0, myThid )
       _EXCH_XY_R4( gairx1, myThid )
       _EXCH_XY_R4( gairy0, myThid )
       _EXCH_XY_R4( gairy1, myThid )
       _EXCH_XY_R4( tair0,  myThid )
       _EXCH_XY_R4( tair1,  myThid )
       _EXCH_XY_R4( qa0,    myThid )
       _EXCH_XY_R4( qa1,    myThid )
       _EXCH_XY_R4( flo0,   myThid )
       _EXCH_XY_R4( flo1,   myThid )
       _EXCH_XY_R4( fsh0,   myThid )
       _EXCH_XY_R4( fsh1,   myThid )
       _EXCH_XY_R4( rain0,  myThid )
       _EXCH_XY_R4( rain1,  myThid )
       _EXCH_XY_R4( evap0,  myThid )
       _EXCH_XY_R4( evap1,  myThid )
       _EXCH_XY_R4( runoff0,myThid )
       _EXCH_XY_R4( runoff1,myThid )
       _EXCH_XY_R4( SSTsi0,   myThid )
       _EXCH_XY_R4( SSTsi1,   myThid )
       _EXCH_XY_R4( SSSsi0,   myThid )
       _EXCH_XY_R4( SSSsi1,   myThid )

      ENDIF

C--   Now calculate whether if it is time to update wind speed arrays
      iRec = int((YearTime-WindForcingStart)/WindForcingPeriod) + 2
      aWghtWind = mod(YearTime-WindForcingStart,WindForcingPeriod) /
     &        WindForcingPeriod
      bWghtWind=1.-aWghtWind
      IF ( aWghtWind .EQ. 0 ) THEN
       _BEGIN_MASTER(myThid)
       write(0,*)
     &  'S/R SEAICE_GET_FORCING: reading winds',myTime,myIter
       IF ( gairxFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( gairxFile ) - 2
        WRITE(fName,'(A,I2.2)') gairxFile(1:iEnd), CurrentYear2
        IF (wind0_is_first) THEN
         CALL READ_REC_XY_RS( fName,gairx0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,gairx1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( gairyFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( gairyFile ) - 2
        WRITE(fName,'(A,I2.2)') gairyFile(1:iEnd), CurrentYear2
        IF (wind0_is_first) THEN
         CALL READ_REC_XY_RS( fName,gairy0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,gairy1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       _END_MASTER(myThid)
       IF (wind0_is_first) THEN
        _EXCH_XY_R4( gairx0, myThid )
        _EXCH_XY_R4( gairy0, myThid )
        wind0_is_first=.FALSE.
       ELSE
        _EXCH_XY_R4( gairx1, myThid )
        _EXCH_XY_R4( gairy1, myThid )
        wind0_is_first=.TRUE.
       ENDIF
      ENDIF

C--   Now calculate whether if it is time to update heat and freshwater flux
      iRec = int((YearTime-FluxForcingStart)/FluxForcingPeriod) + 2
      aWghtFlux = mod(YearTime-FluxForcingStart,FluxForcingPeriod) /
     &        FluxForcingPeriod
      bWghtFlux=1.-aWghtFlux
      IF ( aWghtFlux .EQ. 0 ) THEN
       _BEGIN_MASTER(myThid)
       write(0,*)
     &  'S/R SEAICE_GET_FORCING: reading fluxes',myTime,myIter
       IF ( tairFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( tairFile ) - 4
        WRITE(fName,'(A,I4.4)') tairFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,tair0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,tair1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( qaFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( qaFile ) - 4
        WRITE(fName,'(A,I4.4)') qaFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,qa0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,qa1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( floFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( floFile ) - 4
        WRITE(fName,'(A,I4.4)') floFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,flo0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,flo1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( fshFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( fshFile ) - 4
        WRITE(fName,'(A,I4.4)') fshFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,fsh0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,fsh1,iRec,myIter,myThid )
        ENDIF
       ENDIF
       IF ( rainFile .NE. ' ' ) THEN
        iEnd = ILNBLNK( rainFile ) - 4
        WRITE(fName,'(A,I4.4)') rainFile(1:iEnd), CurrentYear
        IF (flux0_is_first) THEN
         CALL READ_REC_XY_RS( fName,rain0,iRec,myIter,myThid )
        ELSE
         CALL READ_REC_XY_RS( fName,rain1,iRec,myIter,myThid )
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
        _EXCH_XY_R4(tair0,  myThid )
        _EXCH_XY_R4(qa0,    myThid )
        _EXCH_XY_R4(flo0,   myThid )
        _EXCH_XY_R4(fsh0,   myThid )
        _EXCH_XY_R4(rain0,  myThid )
        _EXCH_XY_R4(evap0,  myThid )
        _EXCH_XY_R4(runoff0,myThid )
        flux0_is_first=.FALSE.
       ELSE
        _EXCH_XY_R4(tair1,  myThid )
        _EXCH_XY_R4(qa1,    myThid )
        _EXCH_XY_R4(flo1,   myThid )
        _EXCH_XY_R4(fsh1,   myThid )
        _EXCH_XY_R4(rain1,  myThid )
        _EXCH_XY_R4(evap1,  myThid )
        _EXCH_XY_R4(runoff1,myThid )
        flux0_is_first=.TRUE.
       ENDIF
      ENDIF

C--   Now calculate whether if it is time to update SST array
      iRec = int((YearTime-SSTforcingStart)/SSTforcingPeriod) + 2
      aWghtSST = mod(YearTime-SSTforcingStart,SSTforcingPeriod) /
     &        SSTforcingPeriod
      bWghtSST=1.-aWghtSST
      IF ( aWghtSST .EQ. 0 .AND. thetaClimFile .NE. ' ' ) THEN
       _BEGIN_MASTER(myThid)
       write(0,*) 'S/R EXTERNAL_FIELDS_LOAD: reading SST',myTime,myIter
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
      bWghtSSS=1.-aWghtSSS
      IF ( aWghtSSS .EQ. 0 .AND. saltClimFile .NE. ' ') THEN
       _BEGIN_MASTER(myThid)
       write(0,*) 'S/R EXTERNAL_FIELDS_LOAD: reading SSS',myTime,myIter
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
           gairx(i,j,bi,bj) = bWghtWind *  gairx0(i,j,bi,bj) +
     &                        aWghtWind *  gairx1(i,j,bi,bj)
           gairy(i,j,bi,bj) = bWghtWind *  gairy0(i,j,bi,bj) +
     &                        aWghtWind *  gairy1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           gairx(i,j,bi,bj) = aWghtWind *  gairx0(i,j,bi,bj) +
     &                        bWghtWind *  gairx1(i,j,bi,bj)
           gairy(i,j,bi,bj) = aWghtWind *  gairy0(i,j,bi,bj) +
     &                        bWghtWind *  gairy1(i,j,bi,bj)
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
          tair(i,j,bi,bj)  = bWghtFlux *   tair0(i,j,bi,bj) +
     &                       aWghtFlux *   tair1(i,j,bi,bj)
          qa(i,j,bi,bj)    = bWghtFlux *     qa0(i,j,bi,bj) +
     &                       aWghtFlux *     qa1(i,j,bi,bj)
          flo(i,j,bi,bj)   = bWghtFlux *    flo0(i,j,bi,bj) +
     &                       aWghtFlux *    flo1(i,j,bi,bj)
          fsh(i,j,bi,bj)   = bWghtFlux *    fsh0(i,j,bi,bj) +
     &                       aWghtFlux *    fsh1(i,j,bi,bj)
          rain(i,j,bi,bj)  = bWghtFlux *   rain0(i,j,bi,bj) +
     &                       aWghtFlux *   rain1(i,j,bi,bj)
          evap(i,j,bi,bj)  = bWghtFlux *   evap0(i,j,bi,bj) +
     &                       aWghtFlux *   evap1(i,j,bi,bj)
          runoff(i,j,bi,bj)= bWghtFlux * runoff0(i,j,bi,bj) +
     &                       aWghtFlux * runoff1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
          tair(i,j,bi,bj)  = aWghtFlux *   tair0(i,j,bi,bj) +
     &                       bWghtFlux *   tair1(i,j,bi,bj)
          qa(i,j,bi,bj)    = aWghtFlux *     qa0(i,j,bi,bj) +
     &                       bWghtFlux *     qa1(i,j,bi,bj)
          flo(i,j,bi,bj)   = aWghtFlux *    flo0(i,j,bi,bj) +
     &                       bWghtFlux *    flo1(i,j,bi,bj)
          fsh(i,j,bi,bj)   = aWghtFlux *    fsh0(i,j,bi,bj) +
     &                       bWghtFlux *    fsh1(i,j,bi,bj)
          rain(i,j,bi,bj)  = aWghtFlux *   rain0(i,j,bi,bj) +
     &                       bWghtFlux *   rain1(i,j,bi,bj)
          evap(i,j,bi,bj)  = aWghtFlux *   evap0(i,j,bi,bj) +
     &                       bWghtFlux *   evap1(i,j,bi,bj)
          runoff(i,j,bi,bj)= aWghtFlux * runoff0(i,j,bi,bj) +
     &                       bWghtFlux * runoff1(i,j,bi,bj)
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

#endif ALLOW_SEAICE

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

#endif ALLOW_SEAICE

      RETURN
      END
