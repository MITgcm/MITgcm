
#include "CPP_OPTIONS.h"
 
C     !ROUTINE: BULKF_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE BULKF_FIELDS_LOAD( myTime, myIter, myThid )
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
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
cswdblk -- add ---
#ifdef ALLOW_BULK_FORCE
#include "BULKF.h"
#endif
cswdblk -- end add ---
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"
#endif
      LOGICAL DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
      INTEGER myThid
      _RL     myTime
      INTEGER myIter
 

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     tair[01]  :: Temp. for air temperature
C     qair[01]  :: Temp. for air specific humidity
C     rain[01]  :: Temp. for rain
C     solar[01] :: Temp. for incoming solar radition
C     flw[01]   :: Temp. for downward longwave flux
C     uwind[01]  :: Temp. for zonal wind speed
C     vwind[01]  :: Temp. for meridional wind speed
C     wspeed[01] :: Temp. for wind speed
C     tauxLocal[01] :: Temp. for meridional wind stress
C     tauyLocal[01] : : Temp. for zonal wind stress
C     runoff[01]  :: Temp. for runoff
c     qnetch[01]  :: Temp for qnet (cheating)
c     empch[01]   :: Temp for empmr (cheating)
c     cloud[01]   :: Temp for cloud
c     snow[01]    :: Temp for snow
C     [01]      :: End points for interpolation
C     Above use static heap storage to allow exchange.
C     aWght, bWght :: Interpolation weights
      COMMON /BULKFFIELDS/
     &                 tair0, qair0, rain0, solar0,
     &                 flw0, uwind0, vwind0, runoff0,
     &                 taux0Local, tauy0Local, wspeed0,
     &                 qnetch0, empch0, cloud0, snow0,
     &                 tair1, qair1, rain1, solar1,
     &                 flw1, uwind1, vwind1, runoff1,
     &                 taux1Local, tauy1Local, wspeed1,
     &                 qnetch1,empch1, cloud1,snow1,
     &                 sss0Local, sss1Local, sst0Local, sst1Local
 
      _RS  tair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  rain0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  rain1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  solar0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  solar1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  flw0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  flw1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  uwind0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  uwind1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  runoff1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  taux0Local    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  taux1Local    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tauy0Local    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tauy1Local    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  wspeed0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  wspeed1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qnetch0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qnetch1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  empch0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  empch1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  cloud0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  cloud1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  snow0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  snow1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST0Local     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS0Local     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST1Local     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS1Local     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)



      INTEGER bi,bj,i,j,intime0,intime1
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm

#ifdef ALLOW_BULK_FORCE

      IF ( periodicExternalForcing ) THEN

C First call requires that we initialize everything to zero for safety
      IF ( myIter .EQ. nIter0 ) THEN
       CALL LEF_ZERO( tair0 ,myThid )
       CALL LEF_ZERO( qair0 ,myThid )
       CALL LEF_ZERO( rain0 ,myThid )
       CALL LEF_ZERO( solar0,myThid )
       CALL LEF_ZERO( flw0 ,myThid )
       CALL LEF_ZERO( uwind0,myThid )
       CALL LEF_ZERO( vwind0,myThid )
       CALL LEF_ZERO( runoff0,myThid )
       CALL LEF_ZERO( wspeed0,myThid )
       CALL LEF_ZERO( qnetch0,myThid )
       CALL LEF_ZERO( empch0,myThid )
       CALL LEF_ZERO( cloud0,myThid )
       CALL LEF_ZERO( snow0,myThid )
       CALL LEF_ZERO( tair1 ,myThid )
       CALL LEF_ZERO( qair1 ,myThid )
       CALL LEF_ZERO( rain1 ,myThid )
       CALL LEF_ZERO( solar1,myThid )
       CALL LEF_ZERO( flw1 ,myThid )
       CALL LEF_ZERO( uwind1,myThid )
       CALL LEF_ZERO( vwind0,myThid )
       CALL LEF_ZERO( runoff1,myThid )
       CALL LEF_ZERO( wspeed1,myThid )
       CALL LEF_ZERO( qnetch1,myThid )
       CALL LEF_ZERO( empch1,myThid )
       CALL LEF_ZERO( cloud1,myThid )
       CALL LEF_ZERO( snow1,myThid )
       if (readwindstress)  then
          CALL LEF_ZERO( taux0Local ,myThid )
          CALL LEF_ZERO( tauy0Local ,myThid )
          CALL LEF_ZERO( taux1Local ,myThid )
          CALL LEF_ZERO( tauy1Local ,myThid )
       endif
       if (readsurface)  then
          CALL LEF_ZERO( sst0Local ,myThid )
          CALL LEF_ZERO( sst0Local ,myThid )
          CALL LEF_ZERO( sss1Local ,myThid )
          CALL LEF_ZERO( sss1Local ,myThid )
       endif

      ENDIF

C Now calculate whether it is time to update the forcing arrays
      rdt=1. _d 0 / deltaTclock
      nForcingPeriods=
     &  int(externForcingCycle/externForcingPeriod+0.5)
      Imytm=int(myTime*rdt+0.5)
      Ifprd=int(externForcingPeriod*rdt+0.5)
      Ifcyc=int(externForcingCycle*rdt+0.5)
      Iftm=mod( Imytm+Ifcyc-Ifprd/2 ,Ifcyc)

      intime0=int(Iftm/Ifprd)
      intime1=mod(intime0+1,nForcingPeriods)
      aWght=float( Iftm-Ifprd*intime0 )/float( Ifprd )
      bWght=1.-aWght

      intime0=intime0+1
      intime1=intime1+1

      IF (
     &  Iftm-Ifprd*(intime0-1) .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

       _BEGIN_MASTER(myThid)

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       WRITE(*,*)
     &  'S/R BULKF_FIELDS_LOAD'
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
      IF ( RainFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( RainFile,rain0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( RainFile,rain1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( SolarFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( SolarFile,solar0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( SolarFile,solar1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( LongwaveFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( LongwaveFile,flw0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( LongwaveFile,flw1,intime1,
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
      IF ( RunoffFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( RunoffFile,runoff0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( RunoffFile,runoff1,intime1,
     &       myIter,myThid )
      ENDIF

      IF ( WSpeedFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( WSpeedFile,wspeed0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( WSpeedFile,wspeed1,intime1,
     &       myIter,myThid )
      ENDIF


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
      ENDIF

      IF ( CloudFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( CloudFile,cloud0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( CloudFile,cloud1,intime1,
     &       myIter,myThid )
      ENDIF

      IF ( SnowFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( SnowFile,snow0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( SnowFile,snow1,intime1,
     &       myIter,myThid )
      ENDIF


       if (readwindstress) then 
        IF ( zonalWindFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( zonalWindFile,taux0Local,intime0,
     &                 myIter,myThid )
         CALL READ_REC_XY_RS( zonalWindFile,taux1Local,intime1,
     &                 myIter,myThid )
        ENDIF
        IF ( meridWindFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( meridWindFile,tauy0Local,intime0,
     &                 myIter,myThid )
         CALL READ_REC_XY_RS( meridWindFile,tauy1Local,intime1,
     &                 myIter,myThid )
        ENDIF
       endif
       if (readsurface) then
         IF ( thetaClimFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( thetaClimFile,SST0Local,intime0,
     &                  myIter,myThid )
          CALL READ_REC_XY_RS( thetaClimFile,SST1Local,intime1,
     &                  myIter,myThid )
         ENDIF
         IF ( saltClimFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( saltClimFile,SSS0Local,intime0,
     &                  myIter,myThid )
          CALL READ_REC_XY_RS( saltClimFile,SSS1Local,intime1,
     &                  myIter,myThid )
         ENDIF
       endif

       _END_MASTER(myThid)
C
       _EXCH_XY_R4(tair0 , myThid )
       _EXCH_XY_R4(tair1 , myThid )
       _EXCH_XY_R4(qair0 , myThid )
       _EXCH_XY_R4(qair1 , myThid )
       _EXCH_XY_R4(rain0, myThid )
       _EXCH_XY_R4(rain1, myThid )
       _EXCH_XY_R4(solar0, myThid )
       _EXCH_XY_R4(solar1, myThid )
       _EXCH_XY_R4(flw0, myThid )
       _EXCH_XY_R4(flw1, myThid )
       _EXCH_XY_R4(uwind0, myThid )
       _EXCH_XY_R4(uwind1, myThid )
       _EXCH_XY_R4(vwind0, myThid )
       _EXCH_XY_R4(vwind1, myThid )
CXXXX       CALL EXCH_UV_XY_RS(uwind0, vwind0, .TRUE., myThid)
CXXXX       CALL EXCH_UV_XY_RS(uwind1, vwind1, .TRUE., myThid)
       _EXCH_XY_R4(runoff0, myThid )
       _EXCH_XY_R4(runoff1, myThid )
       _EXCH_XY_R4(wspeed0, myThid )
       _EXCH_XY_R4(wspeed1, myThid )
       _EXCH_XY_R4(qnetch0, myThid )
       _EXCH_XY_R4(qnetch1, myThid )
       _EXCH_XY_R4(empch0, myThid )
       _EXCH_XY_R4(empch1, myThid )
       _EXCH_XY_R4(cloud0, myThid )
       _EXCH_XY_R4(cloud1, myThid )
       _EXCH_XY_R4(snow0 , myThid )
       _EXCH_XY_R4(snow1 , myThid )
       if (readwindstress) then
c       _EXCH_XY_R4(taux0Local , myThid )
c       _EXCH_XY_R4(taux1Local , myThid )
c       _EXCH_XY_R4(tauy0Local , myThid )
c       _EXCH_XY_R4(tauy1Local , myThid )
       CALL EXCH_UV_XY_RS(taux0Local, tauy0Local, .TRUE., myThid)
       CALL EXCH_UV_XY_RS(taux1Local, tauy1Local, .TRUE., myThid)
       endif
       if (readsurface) then
       _EXCH_XY_R4(SST0Local  , myThid )
       _EXCH_XY_R4(SST1Local  , myThid )
       _EXCH_XY_R4(SSS0Local  , myThid )
       _EXCH_XY_R4(SSS1Local  , myThid )
       endif
C
      ENDIF

C--   Interpolate TAIR, QAIR, RAIN, SOLAR
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
cswdblkf -- QQQQQ check if tair is K or C ------
c        -- dasilva data in C, ncep data in K ---
          TAIR(i,j,bi,bj)   = bWght*tair0(i,j,bi,bj)  
     &                       +aWght*tair1(i,j,bi,bj)   !+273.15
cswdblkf -- QQQQQ set to kg.kg??? ---
c       -- dasilva data in g, ncep in kg ---
          QAIR(i,j,bi,bj)   =( bWght*qair0(i,j,bi,bj)  
     &                       +aWght*qair1(i,j,bi,bj) )
          RAIN(i,j,bi,bj)    = bWght*rain0(i,j,bi,bj) 
     &                       +aWght*rain1(i,j,bi,bj)
          SOLAR(i,j,bi,bj)    =  bWght*solar0(i,j,bi,bj)
     &                       +aWght*solar1(i,j,bi,bj)
          FLW(i,j,bi,bj)    =   bWght*flw0(i,j,bi,bj)
     &                       +aWght*flw1(i,j,bi,bj)
          UWIND(i,j,bi,bj)    = bWght*uwind0(i,j,bi,bj)
     &                       +aWght*uwind1(i,j,bi,bj)
          VWIND(i,j,bi,bj)    = bWght*vwind0(i,j,bi,bj)
     &                       +aWght*vwind1(i,j,bi,bj)
          RUNOFF(i,j,bi,bj)    = bWght*runoff0(i,j,bi,bj)
     &                       +aWght*runoff1(i,j,bi,bj)
          WSPEED(i,j,bi,bj)    = bWght*wspeed0(i,j,bi,bj)
     &                       +aWght*wspeed1(i,j,bi,bj)
          QNETCH(i,j,bi,bj)    = bWght*qnetch0(i,j,bi,bj)
     &                       +aWght*qnetch1(i,j,bi,bj)
          EMPCH(i,j,bi,bj)    = bWght*empch0(i,j,bi,bj)
     &                       +aWght*empch1(i,j,bi,bj)
          CLOUD(i,j,bi,bj)    = bWght*cloud0(i,j,bi,bj)
     &                       +aWght*cloud1(i,j,bi,bj)
#ifdef ALLOW_THERM_SEAICE
          SNOW(i,j,bi,bj)    = bWght*snow0(i,j,bi,bj)
     &                       +aWght*snow1(i,j,bi,bj)
#endif
          if (readwindstress) then
            fu(i,j,bi,bj)    = bWght*taux0Local(i,j,bi,bj)
     &                         +aWght*taux1Local(i,j,bi,bj)
            fv(i,j,bi,bj)    = bWght*tauy0Local(i,j,bi,bj)
     &                         +aWght*tauy1Local(i,j,bi,bj)
          endif
          if (readsurface) then
            SST(i,j,bi,bj)   = bWght*SST0Local(i,j,bi,bj)
     &                       +aWght*SST1Local(i,j,bi,bj)
            SSS(i,j,bi,bj)   = bWght*SSS0Local(i,j,bi,bj)
     &                       +aWght*SSS1Local(i,j,bi,bj)
          endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

       _EXCH_XY_R8(tair , myThid )
       _EXCH_XY_R8(qair , myThid )
       _EXCH_XY_R8(rain, myThid )
       _EXCH_XY_R8(solar, myThid )
       _EXCH_XY_R8(flw, myThid )
       _EXCH_XY_R8(uwind, myThid )
       _EXCH_XY_R8(vwind, myThid )
       _EXCH_XY_R8(runoff, myThid )
       _EXCH_XY_R8(wspeed, myThid )
       _EXCH_XY_R8(qnetch, myThid )
       _EXCH_XY_R8(empch, myThid )
       if (readwindstress) then
c       _EXCH_XY_R8(fu , myThid )
c       _EXCH_XY_R8(fv , myThid )
       CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
       endif
       if (readsurface) then
       _EXCH_XY_R8(SST  , myThid )
       _EXCH_XY_R8(SSS  , myThid )
       endif



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

#endif   
c end ifdef ALLOW_BULK_FORCE

      RETURN
      END

