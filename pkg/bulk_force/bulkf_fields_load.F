C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_fields_load.F,v 1.17 2011/06/07 20:45:30 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

C     !ROUTINE: BULKF_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE BULKF_FIELDS_LOAD( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE BULKF_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*
C     | Bulk formula External source field loading routine.
C     | This routine is called every time we want to
C     | load a a set of external fields. The routine decides
C     | which fields to load and then reads them in.
C     | This routine needs to be customised for particular
C     | experiments.
C     | Notes
C     | =====
C     | Two-dimensional and three-dimensional I/O are handled in
C     | the following way under MITgcmUV. A master thread
C     | performs I/O using system calls. This threads reads data
C     | into a temporary buffer. At present the buffer is loaded
C     | with the entire model domain. This is probably OK for now
C     | Each thread then copies data from the buffer to the
C     | region of the proper array it is responsible for.
C     | =====
C     | Conversion of flux fields are described in FFIELDS.h
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_BULK_FORCE

C     !LOCAL VARIABLES:
C     === Local arrays in common block ===
C     bulkfRec   :: time-record currently loaded (in temp arrays *[1])
C     tair[01]   :: Temp. for air temperature
C     qair[01]   :: Temp. for air specific humidity
C     solar[01]  :: Temp. for incoming solar radition
C     flwdwn[01] :: Temp. for downward longwave flux
C     cloud[01]  :: Temp for cloud
C     wspeed[01] :: Temp. for wind speed
C     uwind[01]  :: Temp. for zonal wind speed
C     vwind[01]  :: Temp. for meridional wind speed
C     rain[01]   :: Temp. for rain
C     runoff[01] :: Temp. for runoff
C     snow[01]   :: Temp  for snow
C     thAir[01]  :: Temp for Air potential temp. in the BL [K]
c     qnetch[01] :: Temp for qnet (cheating)
C     empch[01]  :: Temp for empmr (cheating)
C     [01]       :: End points for interpolation

      COMMON /BULKF_FIELDS_LOAD_I/ bulkfRec
      COMMON /BULKF_FIELDS_LOAD_RS/
     &                 tair0, tair1, qair0, qair1,
     &                 solar0, solar1, flwdwn0, flwdwn1,
     &                 cloud0, cloud1, wspeed0, wspeed1,
     &                 uwind0, uwind1, vwind0, vwind1,
     &                 rain0, rain1, runoff0, runoff1,
     &                 snow0, snow1,
#ifdef ALLOW_FORMULA_AIM
     &                 thAir0, thAir1,
#endif
     &                 qnetch0, qnetch1, empch0, empch1

      INTEGER bulkfRec(nSx,nSy)
      _RS  tair0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tair1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qair0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qair1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  solar0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  solar1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  flwdwn0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  flwdwn1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  cloud0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  cloud1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  wspeed0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  wspeed1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  uwind0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  uwind1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  rain0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  rain1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  snow0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  snow1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_FORMULA_AIM
      _RS  thAir0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  thAir1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
      _RS  qnetch0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qnetch1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  empch0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  empch1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     === Local variables ===
C     aWght, bWght :: Interpolation weights
      _RL  aWght,bWght
      INTEGER intimeP, intime0, intime1
      INTEGER bi, bj, i, j
CEOP

      IF ( periodicExternalForcing ) THEN

C First call requires that we initialize everything to zero for safety
      IF ( myIter .EQ. nIter0 ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           bulkfRec(bi,bj) = 0
           DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx,sNx+Olx
              tair0(i,j,bi,bj)   = 0. _d 0
              tair1(i,j,bi,bj)   = 0. _d 0
              qair0(i,j,bi,bj)   = 0. _d 0
              qair1(i,j,bi,bj)   = 0. _d 0
              rain0(i,j,bi,bj)   = 0. _d 0
              rain1(i,j,bi,bj)   = 0. _d 0
              solar0(i,j,bi,bj)  = 0. _d 0
              solar1(i,j,bi,bj)  = 0. _d 0
              flwdwn0(i,j,bi,bj) = 0. _d 0
              flwdwn1(i,j,bi,bj) = 0. _d 0
              cloud0(i,j,bi,bj)  = 0. _d 0
              cloud1(i,j,bi,bj)  = 0. _d 0
              wspeed0(i,j,bi,bj) = 0. _d 0
              wspeed1(i,j,bi,bj) = 0. _d 0
              uwind0(i,j,bi,bj)  = 0. _d 0
              uwind1(i,j,bi,bj)  = 0. _d 0
              vwind0(i,j,bi,bj)  = 0. _d 0
              vwind1(i,j,bi,bj)  = 0. _d 0
              runoff0(i,j,bi,bj) = 0. _d 0
              runoff1(i,j,bi,bj) = 0. _d 0
              snow0(i,j,bi,bj)   = 0. _d 0
              snow1(i,j,bi,bj)   = 0. _d 0
#ifdef ALLOW_FORMULA_AIM
              thAir0(i,j,bi,bj)  = 0. _d 0
              thAir1(i,j,bi,bj)  = 0. _d 0
#endif
              qnetch0(i,j,bi,bj) = 0. _d 0
              qnetch1(i,j,bi,bj) = 0. _d 0
              empch0(i,j,bi,bj)  = 0. _d 0
              empch1(i,j,bi,bj)  = 0. _d 0
            ENDDO
           ENDDO
         ENDDO
        ENDDO
      ENDIF

C--   Now calculate whether it is time to update the forcing arrays
      CALL GET_PERIODIC_INTERVAL(
     O                  intimeP, intime0, intime1, bWght, aWght,
     I                  externForcingCycle, externForcingPeriod,
     I                  deltaTclock, myTime, myThid )

      bi = myBxLo(myThid)
      bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
      IF ( debugLevel.GE.debLevB ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &   ' BULKF_FIELDS_LOAD,', myIter,
     &   ' : iP,iLd,i0,i1=', intimeP,bulkfRec(bi,bj), intime0,intime1,
     &   ' ; Wght=', bWght, aWght
        _END_MASTER(myThid)
      ENDIF
#endif /* ALLOW_DEBUG */

C-    Make no assumption on sequence of calls to BULKF_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
      IF ( intime1.NE.bulkfRec(bi,bj) ) THEN

       _BARRIER

C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
       IF ( debugLevel.GE.debLevZero ) THEN
         _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &   ' BULKF_FIELDS_LOAD, it=', myIter,
     &   ' : Reading new data, i0,i1=', intime0, intime1,
     &    ' (prev=', intimeP, bulkfRec(bi,bj), ' )'
        _END_MASTER(myThid)
       ENDIF

      IF ( AirTempFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( AirTempFile,tair0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( AirTempFile,tair1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( AirHumidityFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( AirhumidityFile,qair0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( AirhumidityFile,qair1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( SolarFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( SolarFile,solar0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( SolarFile,solar1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( LongwaveFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( LongwaveFile,flwdwn0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( LongwaveFile,flwdwn1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( CloudFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( CloudFile,cloud0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( CloudFile,cloud1,intime1,
     &       myIter,myThid )
      ENDIF

      IF ( WSpeedFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( WSpeedFile,wspeed0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( WSpeedFile,wspeed1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( UwindFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( UWindFile,uwind0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( UWindFile,uwind1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( VwindFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( VWindFile,vwind0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( VWindFile,vwind1,intime1,
     &       myIter,myThid )
      ENDIF

      IF ( RainFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( RainFile,rain0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( RainFile,rain1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( RunoffFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( RunoffFile,runoff0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( RunoffFile,runoff1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( SnowFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( SnowFile,snow0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( SnowFile,snow1,intime1,
     &       myIter,myThid )
      ENDIF

#ifdef ALLOW_FORMULA_AIM
      IF ( airPotTempFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( airPotTempFile, thAir0, intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( airPotTempFile, thAir1, intime1,
     &       myIter,myThid )
      ENDIF
#endif

      IF ( QnetFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( QnetFile,qnetch0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( QnetFile,qnetch1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( EmPFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( EmpFile,empch0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( EmpFile,empch1,intime1,
     &       myIter,myThid )
c      IF ( convertEmP2rUnit.EQ.mass2rUnit ) THEN
C-     EmPmR is now (after c59h) expressed in kg/m2/s (fresh water mass flux)
        _BARRIER
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            empch0(i,j,bi,bj) = empch0(i,j,bi,bj)*rhoConstFresh
            empch1(i,j,bi,bj) = empch1(i,j,bi,bj)*rhoConstFresh
           ENDDO
          ENDDO
         ENDDO
        ENDDO
c      ENDIF
      ENDIF

C note: no need for extra barrier here since EXCH contains threads synchronization
c      _BARRIER

       _EXCH_XY_RS(tair0 , myThid )
       _EXCH_XY_RS(tair1 , myThid )
       _EXCH_XY_RS(qair0 , myThid )
       _EXCH_XY_RS(qair1 , myThid )
       _EXCH_XY_RS(solar0, myThid )
       _EXCH_XY_RS(solar1, myThid )
       _EXCH_XY_RS(flwdwn0, myThid )
       _EXCH_XY_RS(flwdwn1, myThid )
       _EXCH_XY_RS(cloud0, myThid )
       _EXCH_XY_RS(cloud1, myThid )
       _EXCH_XY_RS(wspeed0, myThid )
       _EXCH_XY_RS(wspeed1, myThid )
c      _EXCH_XY_RS(uwind0, myThid )
c      _EXCH_XY_RS(uwind1, myThid )
c      _EXCH_XY_RS(vwind0, myThid )
c      _EXCH_XY_RS(vwind1, myThid )
       CALL EXCH_UV_AGRID_3D_RS( uwind0, vwind0, .TRUE., 1, myThid )
       CALL EXCH_UV_AGRID_3D_RS( uwind1, vwind1, .TRUE., 1, myThid )
       _EXCH_XY_RS(rain0, myThid )
       _EXCH_XY_RS(rain1, myThid )
       _EXCH_XY_RS(runoff0, myThid )
       _EXCH_XY_RS(runoff1, myThid )
       _EXCH_XY_RS(snow0 , myThid )
       _EXCH_XY_RS(snow1 , myThid )
#ifdef ALLOW_FORMULA_AIM
       IF ( useFluxFormula_AIM ) THEN
         _EXCH_XY_RS( thAir0, myThid )
         _EXCH_XY_RS( thAir1, myThid )
       ENDIF
#endif
       _EXCH_XY_RS(qnetch0, myThid )
       _EXCH_XY_RS(qnetch1, myThid )
       _EXCH_XY_RS(empch0, myThid )
       _EXCH_XY_RS(empch1, myThid )

C-    save newly loaded time-record
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           bulkfRec(bi,bj) = intime1
         ENDDO
        ENDDO

C--   end if-block for loading new time-records
      ENDIF

C--   Interpolate TAIR, QAIR, RAIN, SOLAR
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
Cswdblkf -- QQQQQ check if tair is K or C ------
C        -- dasilva data in C, ncep data in K ---
          TAIR(i,j,bi,bj)   = bWght*tair0(i,j,bi,bj)
     &                       +aWght*tair1(i,j,bi,bj)
c                            +273.15 _d 0
Cswdblkf -- QQQQQ set to kg.kg??? ---
C       -- dasilva data in g, ncep in kg ---
          QAIR(i,j,bi,bj)    =(bWght*qair0(i,j,bi,bj)
     &                        +aWght*qair1(i,j,bi,bj) )
          SOLAR(i,j,bi,bj)   = bWght*solar0(i,j,bi,bj)
     &                        +aWght*solar1(i,j,bi,bj)
          FLWDWN(i,j,bi,bj)  = bWght*flwdwn0(i,j,bi,bj)
     &                        +aWght*flwdwn1(i,j,bi,bj)
          CLOUD(i,j,bi,bj)   = bWght*cloud0(i,j,bi,bj)
     &                        +aWght*cloud1(i,j,bi,bj)
          WSPEED(i,j,bi,bj)  = bWght*wspeed0(i,j,bi,bj)
     &                        +aWght*wspeed1(i,j,bi,bj)
          UWIND(i,j,bi,bj)   = bWght*uwind0(i,j,bi,bj)
     &                        +aWght*uwind1(i,j,bi,bj)
          VWIND(i,j,bi,bj)   = bWght*vwind0(i,j,bi,bj)
     &                        +aWght*vwind1(i,j,bi,bj)
          RAIN(i,j,bi,bj)    = bWght*rain0(i,j,bi,bj)
     &                        +aWght*rain1(i,j,bi,bj)
          RUNOFF(i,j,bi,bj)  = bWght*runoff0(i,j,bi,bj)
     &                        +aWght*runoff1(i,j,bi,bj)
c#ifdef ALLOW_THSICE
c         SNOW(i,j,bi,bj)    = bWght*snow0(i,j,bi,bj)
c    &                        +aWght*snow1(i,j,bi,bj)
c#endif
          Qnetch(i,j,bi,bj)  = bWght*qnetch0(i,j,bi,bj)
     &                        +aWght*qnetch1(i,j,bi,bj)
          EmPch(i,j,bi,bj)   = bWght*empch0(i,j,bi,bj)
     &                        +aWght*empch1(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_FORMULA_AIM
        IF ( useFluxFormula_AIM ) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            thAir(i,j,bi,bj)  = bWght*thAir0(i,j,bi,bj)
     &                        + aWght*thAir1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif
       ENDDO
      ENDDO

C- jmc: not needed since local fields have been exchanged.
c      _EXCH_XY_RL(tair , myThid )

C-- Diagnostics
c      IF (myThid.EQ.1 .AND. myTime.LT.62208000.) THEN
c        write(*,'(a,1p5e12.4,2i6,2e12.4)')
c     &   'time,TAIR,QAIR,RAIN,SOLAR,i0,i1,a,b = ',
c     &   myTime,
c     &   TAIR(1,sNy,1,1),QAIR(1,sNy,1,1),
c     &   RAIN(1,sNy,1,1),SOLAR(1,sNy,1,1),
c     &   intime0,intime1,aWght,bWght
c        write(*,'(a,1p4e12.4,2e12.4)')
c     &   'time,tair0,tair1,TAIR = ',
c     &   myTime,
c     &   tair0(1,sNy,1,1),tair1(1,sNy,1,1),TAIR(1,sNy,1,1),
c     &   aWght,bWght
c      ENDIF

C endif for periodicForcing
      ENDIF

#endif /*ALLOW_BULK_FORCE*/

      RETURN
      END
