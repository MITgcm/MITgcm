C $Header: /u/gcmpack/MITgcm/model/src/Attic/load_external_fields.F,v 1.3 1998/06/22 15:26:25 adcroft Exp $

#include "CPP_EEOPTIONS.h"
 
CStartOfInterface
      SUBROUTINE LOAD_EXTERNAL_FIELDS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE LOAD_EXTERNAL_FIELDS                          |
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
C     \==========================================================/
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation's time
C     myIter - Simulation's iteration number
      INTEGER myThid
      REAL    myTime
      INTEGER myIter
CEndOfInterface
 
C     === Local variables ===

C     Simple example of loading a climatology at the
C     first time step that is the used in a relaxation term
C     in the surface layer.
      IF ( myTime .EQ. startTime ) THEN
       CALL ZERO_CLIMATOLOGY( myThid )
       CALL LOAD_THETA_CLIMATOLOGY( myThid )
       CALL LOAD_SALT_CLIMATOLOGY( myThid )
       CALL LOAD_WIND_STRESS_CLIMATOLOGY( myThid )
      ENDIF

      IF ( periodicExternalForcing ) THEN
       CALL LOAD_INTERPOLATE_FORCING( myIter, myTime , myThid )
      ENDIF

      RETURN
      END

CStartOfInterface
      SUBROUTINE LOAD_SALT_CLIMATOLOGY( myThid )
C     /==========================================================\
C     | S/R LOAD_SALT_CLIMATOLOGY                                |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C     === Routine arguments ===
      INTEGER myThid

CEndOfInterface

C     === Local variables ===
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) msgBuf

      IF ( saltClimFile .EQ. ' ' ) THEN
C--    Check that relaxation term is off
C      IF ( doSaltClimRelax ) THEN
C       WRITE(msgBuf,'(A)')  'S/R LOAD_SALT_CLIMATOLOGY'
C       CALL PRINT_ERROR( msgBuf, 1 )
C       WRITE(msgBuf,'(A)')  'No salt climatology for relaxation specified.'
C       CALL PRINT_ERROR( msgBuf, 1 )
C       STOP 'ABNORMAL END: S/R LOAD_SALT_CLIMATOLOGY'
C      ENDIF
      ELSE
C--    Load value from file
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XY_RS( saltClimFile, ' ', SSS, 0, myThid )
       _END_MASTER(   myThid )
      ENDIF

      _EXCH_XY_R4( SSS, myThid )

C
      IF ( saltClimFile .NE. ' ' ) THEN
       CALL PLOT_FIELD_XYRS( SSS, 'Salt Climatology' , 1, myThid )
      ENDIF
C

      RETURN
      END

CStartOfInterface
      SUBROUTINE LOAD_THETA_CLIMATOLOGY( myThid )
C     /==========================================================\
C     | S/R LOAD_THETA_CLIMATOLOGY                               |
C     \==========================================================?

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C     === Routine arguments ===
      INTEGER myThid

CEndOfInterface

C     === Local variables ===
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) msgBuf

      IF ( thetaClimFile .EQ. ' ' ) THEN
C--    Check that relaxation term is off
C      IF ( doThetaClimRelax ) THEN
C       WRITE(msgBuf,'(A)')  'S/R LOAD_THETA_CLIMATOLOGY'
C       CALL PRINT_ERROR( msgBuf, 1 )
C       WRITE(msgBuf,'(A)')  'No theta climatology for relaxation specified.'
C       CALL PRINT_ERROR( msgBuf, 1 )
C       STOP 'ABNORMAL END: S/R LOAD_THETA_CLIMATOLOGY'
C      ENDIF
      ELSE
C--    Load value from file
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XY_RS( thetaClimFile, ' ', SST, 0, myThid )
       _END_MASTER(   myThid )
      ENDIF

      _EXCH_XY_R4( SST, myThid )
C
      IF ( thetaClimFile .NE. ' ' ) THEN
       CALL PLOT_FIELD_XYRS( SST, 'Theta Climatology' , 1, myThid )
      ENDIF
C
      RETURN
      END

CStartOfInterface
      SUBROUTINE LOAD_WIND_STRESS_CLIMATOLOGY( myThid )
C     /==========================================================\
C     | S/R LOAD_WIND_STRESS_CLIMATOLOGY                         |
C     \==========================================================?

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C     === Routine arguments ===
      INTEGER myThid

CEndOfInterface

C     === Local variables ===
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) msgBuf
      INTEGER bi,bj,i,j


      _BEGIN_MASTER(myThid)

      IF (.NOT. zonalWindFile .EQ. ' ' ) THEN
       CALL READ_FLD_XY_RS( zonalWindFile, ' ', fu, 0, myThid )
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fu(i,j,bi,bj) = fu(i,j,bi,bj)/(delZ(1)*rhonil)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      IF (.NOT. meridWindFile .EQ. ' ' ) THEN
       CALL READ_FLD_XY_RS( meridWindFile, ' ', fv, 0, myThid )
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fv(i,j,bi,bj) = fv(i,j,bi,bj)/(delZ(1)*rhonil)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      _END_MASTER(myThid)
C
      _EXCH_XY_R4(fu  , myThid )
      _EXCH_XY_R4(fv  , myThid )
C
      IF (.NOT. zonalWindFile .EQ. ' ' ) THEN
       CALL PLOT_FIELD_XYRS( fu, 'S/R LOAD_WIND_STRESS_CLIMATOLOGY FU',1,myThid)
      ENDIF
      IF (.NOT. meridWindFile .EQ. ' ' ) THEN
       CALL PLOT_FIELD_XYRS( fv, 'S/R LOAD_WIND_STRESS_CLIMATOLOGY FV',1,myThid)
      ENDIF
C
      RETURN
      END

CStartOfInterface
      SUBROUTINE LOAD_INTERPOLATE_FORCING( myIter, myTime, myThid )
C     /==========================================================\
C     | S/R LOAD_INTERPOLATE_FORCING                             |
C     \==========================================================?

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C     === Routine arguments ===
      REAL myTime
      INTEGER myIter,myThid

CEndOfInterface

C     === Functions ===
      LOGICAL DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     === Local variables ===
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) msgBuf
      INTEGER bi,bj,i,j,intime0,intime1
      CHARACTER*(40) filename
      REAL aWght,myForcingTime

      myForcingTime=myTime -externForcingPeriod*0.5 _d 0
     &              +externForcingCycle

      IF (
     &  DIFFERENT_MULTIPLE(externForcingPeriod,
     &                     myForcingTime,myForcingTime-deltaTClock)
     &  .OR. myTime .EQ. startTime
     & ) THEN

       _BEGIN_MASTER(myThid)

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       write(0,*) 'S/R LOAD_INTERPOLATE_FORCING: Reading new data',myTime,myIter
       intime0=int( (myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &              )/externForcingPeriod )
       intime1=int( (myForcingTime+externForcingPeriod-externForcingCycle*
     &              int((myForcingTime+externForcingPeriod)/externForcingCycle)
     &              )/externForcingPeriod )

C--    SST
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'levtemp.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', SST, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'levtemp.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', SSTdot, myIter, myThid )
    
C      Set SSTdot to the total change over the period
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           SSTdot(i,j,bi,bj) = SSTdot(i,j,bi,bj)-SST(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C      Interpolate SST to correct time (necessary only for myTime=startTime)
       aWght=(myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &       )/externForcingPeriod - float(intime0)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           SST(i,j,bi,bj) = SST(i,j,bi,bj)+aWght*SSTdot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      Convert SSTdot into the increment to SST per time-step (here after)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           SSTdot(i,j,bi,bj)=SSTdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    SSS
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'levsalt.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', SSS, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'levsalt.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', SSSdot, myIter, myThid )
    
C      Set SSSdot to the total change over the period
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           SSSdot(i,j,bi,bj) = SSSdot(i,j,bi,bj)-SSS(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C      Interpolate SSS to correct time (necessary only for myTime=startTime)
       aWght=(myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &       )/externForcingPeriod - float(intime0)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           SSS(i,j,bi,bj) = SSS(i,j,bi,bj)+aWght*SSSdot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      Convert SSSdot into the increment to SSS per time-step (here after)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           SSSdot(i,j,bi,bj)=SSSdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    TauX
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'windx.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', fu, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'windx.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', fudot, myIter, myThid )
    
C      Set fudot to the total change over the period
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fu(i,j,bi,bj) = fu(i,j,bi,bj)/(delZ(1)*rhonil)
           fudot(i,j,bi,bj) = fudot(i,j,bi,bj)/(delZ(1)*rhonil)
           fudot(i,j,bi,bj) = fudot(i,j,bi,bj)-fu(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C      Interpolate fu to correct time (necessary only for myTime=startTime)
       aWght=(myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &       )/externForcingPeriod - float(intime0)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fu(i,j,bi,bj) = fu(i,j,bi,bj)+aWght*fudot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      Convert fudot into the increment to fu per time-step (here after)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fudot(i,j,bi,bj)=fudot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    TauY
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'windy.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', fv, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'windy.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', fvdot, myIter, myThid )
    
C      Set fvdot to the total change over the period
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fv(i,j,bi,bj) = fv(i,j,bi,bj)/(delZ(1)*rhonil)
           fvdot(i,j,bi,bj) = fvdot(i,j,bi,bj)/(delZ(1)*rhonil)
           fvdot(i,j,bi,bj) = fvdot(i,j,bi,bj)-fv(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C      Interpolate fv to correct time (necessary only for myTime=startTime)
       aWght=(myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &       )/externForcingPeriod - float(intime0)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fv(i,j,bi,bj) = fv(i,j,bi,bj)+aWght*fvdot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      Convert fvdot into the increment to fv per time-step (here after)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fvdot(i,j,bi,bj)=fvdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    Qnet
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'Qnet.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', Qnet, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'Qnet.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', Qnetdot, myIter, myThid )
    
C      Set Qnetdot to the total change over the period
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)/(4200.*rhonil*delZ(1))
           Qnetdot(i,j,bi,bj) = Qnetdot(i,j,bi,bj)/(4200.*rhonil*delZ(1))
           Qnetdot(i,j,bi,bj) = Qnetdot(i,j,bi,bj)-Qnet(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C      Interpolate Qnet to correct time (necessary only for myTime=startTime)
       aWght=(myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &       )/externForcingPeriod - float(intime0)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)+aWght*Qnetdot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      Convert Qnetdot into the increment to Qnet per time-step (here after)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           Qnetdot(i,j,bi,bj)=Qnetdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    EmPpR
Cfix   WRITE(filename(1:40),'(A,I10.10,A)') 
Cfix &      'EmP_y.',intime0+1,'.bin'
       filename='EmP_y.bin'
       CALL READ_FLD_XY_RS( filename, ' ', EmPpR, myIter, myThid )
Cfix   WRITE(filename(1:40),'(A,I10.10,A)') 
Cfix &      'EmPpR.',intime1+1,'.bin'
Cfix   CALL READ_FLD_XY_RS( filename, ' ', EmPpRdot, myIter, myThid )
    
C      Set EmPpRdot to the total change over the period
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
Cfix       EmPpRdot(i,j,bi,bj) = EmPpRdot(i,j,bi,bj)-EmPpR(i,j,bi,bj)
           EmPpR(i,j,bi,bj) = EmPpR(i,j,bi,bj)/(3153600000.*delZ(1))*35.
           EmPpRdot(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C      Interpolate EmPpR to correct time (necessary only for myTime=startTime)
       aWght=(myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &       )/externForcingPeriod - float(intime0)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           EmPpR(i,j,bi,bj) = EmPpR(i,j,bi,bj)+aWght*EmPpRdot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      Convert EmPpRdot into the increment to EmPpR per time-step (here after)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           EmPpRdot(i,j,bi,bj)=EmPpRdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C
       _END_MASTER(myThid)
C
       _EXCH_XY_R4(SST  , myThid )
       _EXCH_XY_R4(SSS  , myThid )
       _EXCH_XY_R4(fu   , myThid )
       _EXCH_XY_R4(fv   , myThid )
       _EXCH_XY_R4(Qnet , myThid )
       _EXCH_XY_R4(EmPpR, myThid )
C
      ELSE

C      Increment SST,SSS,fu,fv,Qnet,EmPpR
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           SST(i,j,bi,bj) = SST(i,j,bi,bj)+SSTdot(i,j,bi,bj)
           SSS(i,j,bi,bj) = SSS(i,j,bi,bj)+SSSdot(i,j,bi,bj)
           fu(i,j,bi,bj) = fu(i,j,bi,bj)+fudot(i,j,bi,bj)
           fv(i,j,bi,bj) = fv(i,j,bi,bj)+fvdot(i,j,bi,bj)
           Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)+Qnetdot(i,j,bi,bj)
           EmPpR(i,j,bi,bj) = EmPpR(i,j,bi,bj)+EmPpRdot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDIF

      RETURN
      END

CStartOfInterface
      SUBROUTINE ZERO_CLIMATOLOGY( myThid )
C     /==========================================================\
C     | S/R ZERO_CLIMATOLOGY                                     |
C     \==========================================================?

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C     === Routine arguments ===
      INTEGER myThid

CEndOfInterface

C     === Local variables ===
      INTEGER i,j,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           SST(i,j,bi,bj) = 0.
           SSS(i,j,bi,bj) = 0.
           Qnet(i,j,bi,bj) = 0.
           EmPpR(i,j,bi,bj) = 0.
           fudot(i,j,bi,bj) = 0.
           fvdot(i,j,bi,bj) = 0.
           SSTdot(i,j,bi,bj) = 0.
           SSSdot(i,j,bi,bj) = 0.
           Qnetdot(i,j,bi,bj) = 0.
           EmPpRdot(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO


      RETURN
      END
