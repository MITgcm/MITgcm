C $Header: /u/gcmpack/MITgcm/verification/aim.5l_LatLon/code/Attic/external_fields_load.F,v 1.1 2001/06/18 17:39:59 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE EXTERNAL_FIELDS_LOAD( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE EXTERNAL_FIELDS_LOAD                          |
C     | o Control reading of fields from external source.        |
C     |==========================================================|
C     | External source field loading routine.                   |
C     | This routine is called every time we want to             |
C     | load a a set of external fields. The routine decides     |
C     | which fields to load and then reads them in.             |
C     | This routine needs to be customised for particular       |
C     | experiments.                                             |
C     | Notes                                                    |
C     | =====                                                    |
C     | Two-dimensional and three-dimensional I/O are handled in |
C     | the following way under MITgcmUV. A master thread        |
C     | performs I/O using system calls. This threads reads data |
C     | into a temporary buffer. At present the buffer is loaded |
C     | with the entire model domain. This is probably OK for now|
C     | Each thread then copies data from the buffer to the      |
C     | region of the proper array it is responsible for.        |
C     | =====                                                    |
C     | Conversion of flux fields are described in FFIELDS.h     |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
      INTEGER myThid
      _RL     myTime
      INTEGER myIter
CEndOfInterface
 
C     === Functions ===
      LOGICAL DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     === Local arrays ===
      COMMON /TDFIELDS/
     &                 taux0, tauy0, Qnet0, EmPmR0, SST0, SSS0, Qsw0,
     &                 taux1, tauy1, Qnet1, EmPmR1, SST1, SSS1, Qsw1
      _RS  taux0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tauy0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  taux1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tauy1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     === Local variables ===
      INTEGER bi,bj,i,j,intime0,intime1

      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm

      IF ( periodicExternalForcing ) THEN

C First call requires that we initialize everything to zero for safety
      IF ( myIter .EQ. nIter0 ) THEN
       CALL LEF_ZERO( taux0 ,myThid )
       CALL LEF_ZERO( tauy0 ,myThid )
       CALL LEF_ZERO( Qnet0 ,myThid )
       CALL LEF_ZERO( EmPmR0 ,myThid )
       CALL LEF_ZERO( SST0 ,myThid )
       CALL LEF_ZERO( SSS0 ,myThid )
       CALL LEF_ZERO( Qsw0 ,myThid )
       CALL LEF_ZERO( taux1 ,myThid )
       CALL LEF_ZERO( tauy1 ,myThid )
       CALL LEF_ZERO( Qnet1 ,myThid )
       CALL LEF_ZERO( EmPmR1 ,myThid )
       CALL LEF_ZERO( SST1 ,myThid )
       CALL LEF_ZERO( SSS1 ,myThid )
       CALL LEF_ZERO( Qsw1 ,myThid )
      ENDIF

C Now calculate whether it is time to update the forcing arrays
      rdt=1. _d 0 / deltaTclock
      nForcingPeriods=int(externForcingCycle/externForcingPeriod+0.5)
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
       write(0,*)
     &  'S/R LOAD_INTERPOLATE_FORCING: Reading new data',myTime,myIter

      IF ( zonalWindFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( zonalWindFile,taux0,intime0,myIter,myThid )
       CALL READ_REC_XY_RS( zonalWindFile,taux1,intime1,myIter,myThid )
      ENDIF
      IF ( meridWindFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( meridWindFile,tauy0,intime0,myIter,myThid )
       CALL READ_REC_XY_RS( meridWindFile,tauy1,intime1,myIter,myThid )
      ENDIF
      IF ( surfQFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( surfQFile,Qnet0,intime0,myIter,myThid )
       CALL READ_REC_XY_RS( surfQFile,Qnet1,intime1,myIter,myThid )
      ENDIF
      IF ( EmPmRfile .NE. ' '  ) THEN
Cfixed CALL READ_REC_XY_RS( EmPmRfile,EmPmR0,intime0,myIter,myThid )
Cfixed CALL READ_REC_XY_RS( EmPmRfile,EmPmR1,intime1,myIter,myThid )
       CALL READ_REC_XY_RS( EmPmRfile,EmPmR0,1,myIter,myThid )
       CALL READ_REC_XY_RS( EmPmRfile,EmPmR1,1,myIter,myThid )
      ENDIF
      IF ( thetaClimFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( thetaClimFile,SST0,intime0,myIter,myThid )
       CALL READ_REC_XY_RS( thetaClimFile,SST1,intime1,myIter,myThid )
      ENDIF
      IF ( saltClimFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( saltClimFile,SSS0,intime0,myIter,myThid )
       CALL READ_REC_XY_RS( saltClimFile,SSS1,intime1,myIter,myThid )
      ENDIF
#ifdef SHORTWAVE_HEATING
      IF ( surfQswFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( surfQswFile,Qsw0,intime0,myIter,myThid )
       CALL READ_REC_XY_RS( surfQswFile,Qsw1,intime1,myIter,myThid )
      ENDIF
#endif

       _END_MASTER(myThid)
C
       _EXCH_XY_R4(SST0  , myThid )
       _EXCH_XY_R4(SST1  , myThid )
       _EXCH_XY_R4(SSS0  , myThid )
       _EXCH_XY_R4(SSS1  , myThid )
       _EXCH_XY_R4(taux0 , myThid )
       _EXCH_XY_R4(taux1 , myThid )
       _EXCH_XY_R4(tauy0 , myThid )
       _EXCH_XY_R4(tauy1 , myThid )
       _EXCH_XY_R4(Qnet0, myThid )
       _EXCH_XY_R4(Qnet1, myThid )
       _EXCH_XY_R4(EmPmR0, myThid )
       _EXCH_XY_R4(EmPmR1, myThid )
#ifdef SHORTWAVE_HEATING
       _EXCH_XY_R4(Qsw0, myThid )
       _EXCH_XY_R4(Qsw1, myThid )
#endif
C
      ENDIF

C--   Interpolate fu,fv,Qnet,EmPmR,SST,SSS,Qsw
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          SST(i,j,bi,bj)   = bWght*SST0(i,j,bi,bj)  
     &                       +aWght*SST1(i,j,bi,bj)
          SSS(i,j,bi,bj)   = bWght*SSS0(i,j,bi,bj)  
     &                       +aWght*SSS1(i,j,bi,bj)
          fu(i,j,bi,bj)    = bWght*taux0(i,j,bi,bj) 
     &                       +aWght*taux1(i,j,bi,bj)
          fv(i,j,bi,bj)    = bWght*tauy0(i,j,bi,bj) 
     &                       +aWght*tauy1(i,j,bi,bj)
          Qnet(i,j,bi,bj)  = bWght*Qnet0(i,j,bi,bj)
     &                       +aWght*Qnet1(i,j,bi,bj)
          EmPmR(i,j,bi,bj) = bWght*EmPmR0(i,j,bi,bj)
     &                       +aWght*EmPmR1(i,j,bi,bj)
#ifdef SHORTWAVE_HEATING
          Qsw(i,j,bi,bj)   = bWght*Qsw0(i,j,bi,bj)
     &                       +aWght*Qsw1(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-- Diagnostics
      IF (myThid.EQ.1 .AND. myTime.LT.62208000.) THEN
        write(0,'(a,1p7e12.4,2i6,2e12.4)')
     &   'time,SST,SSS,fu,fv,Q,E-P,i0,i1,a,b = ',
     &   myTime,
     &   SST(1,sNy,1,1),SSS(1,sNy,1,1),
     &   fu(1,sNy,1,1),fv(1,sNy,1,1),
     &   Qnet(1,sNy,1,1),EmPmR(1,sNy,1,1),
     &   intime0,intime1,aWght,bWght
        write(0,'(a,1p7e12.4)')
     &   'time,fu0,fu1,fu = ',
     &   myTime,
     &   taux0(1,sNy,1,1),taux1(1,sNy,1,1),fu(1,sNy,1,1),
     &   aWght,bWght
      ENDIF

C endif for periodicForcing
      ENDIF

#ifdef ALLOW_AIM
      IF ( useAIM ) THEN
C      Update AIM bottom boundary data
       CALL AIM_EXTERNAL_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

      RETURN
      END

      SUBROUTINE LEF_ZERO( arr ,myThid )
C     This routine simply sets the argument array to zero
C     Used only by EXTERNAL_FIELDS_LOAD
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
C     === Arguments ===
      _RS  arr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
C     === Local variables ===
      INTEGER i,j,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          arr(i,j,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
