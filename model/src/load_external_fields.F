C $Header: /u/gcmpack/MITgcm/model/src/Attic/load_external_fields.F,v 1.9 1998/11/06 22:44:48 cnh Exp $

#include "CPP_OPTIONS.h"
 
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
C     myTime - Simulation time
C     myIter - Simulation timestep number
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
C     bi,bj,i,j - Loop counters
C     gamma     - Input data to acceleration conversion factor.
      CHARACTER*(MAX_LEN_FNAM) msgBuf
      INTEGER bi,bj,i,j
      _RL gamma



      IF (.NOT. zonalWindFile .EQ. ' ' ) THEN
C--    Set gamma to convert from N/m^2 to m/s^2
       IF     ( UsingZCoords ) THEN
        gamma = 1. _d 0 / (delR(1)*rhonil)
       ELSEIF ( UsingPCoords ) THEN
        gamma = Gravity / delR(1)
       ELSE
        WRITE(msgBuf,'(A)')  'S/R LOAD_WIND_STRESS_CLIMATOLOGY'
        CALL PRINT_ERROR( msgBuf, 1 )
        WRITE(msgBuf,'(A)')
     &   'No relation converting input to acceleration.'
        CALL PRINT_ERROR( msgBuf, 1 )
        STOP 'ABNORMAL END: S/R LOAD_WIND_STRESS_CLIMATOLOGY'
       ENDIF
       _BEGIN_MASTER(myThid)
       CALL READ_FLD_XY_RS( zonalWindFile, ' ', fu, 0, myThid )
       _END_MASTER(myThid)
       _BARRIER
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fu(i,j,bi,bj) = fu(i,j,bi,bj)*gamma
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      IF (.NOT. meridWindFile .EQ. ' ' ) THEN
C--    Set gamma to convert from N/m^2 to m/s^2
       IF     ( UsingZCoords ) THEN
        gamma = 1. _d 0 / (delR(1)*rhonil)
       ELSEIF ( UsingPCoords ) THEN
        gamma = Gravity / (delR(1))
       ELSE
        WRITE(msgBuf,'(A)')  'S/R LOAD_WIND_STRESS_CLIMATOLOGY'
        CALL PRINT_ERROR( msgBuf, 1 )
        WRITE(msgBuf,'(A)')
     &   'No relation converting input to acceleration.'
        CALL PRINT_ERROR( msgBuf, 1 )
        STOP 'ABNORMAL END: S/R LOAD_WIND_STRESS_CLIMATOLOGY'
       ENDIF
       _BEGIN_MASTER(myThid)
       CALL READ_FLD_XY_RS( meridWindFile, ' ', fv, 0, myThid )
       _END_MASTER(myThid)
       _BARRIER
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fv(i,j,bi,bj) = fv(i,j,bi,bj)*gamma
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C
      _EXCH_XY_R4(fu  , myThid )
      _EXCH_XY_R4(fv  , myThid )
C
      IF (.NOT. zonalWindFile .EQ. ' ' ) THEN
       CALL PLOT_FIELD_XYRS( fu, 
     &   'S/R LOAD_WIND_STRESS_CLIMATOLOGY FU',1,myThid)
      ENDIF
      IF (.NOT. meridWindFile .EQ. ' ' ) THEN
       CALL PLOT_FIELD_XYRS( fv, 
     &   'S/R LOAD_WIND_STRESS_CLIMATOLOGY FV',1,myThid)
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


C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       _BEGIN_MASTER(myThid)
       write(0,*) 
     &   'S/R LOAD_INTERPOLATE_FORCING: Reading new data',myTime,myIter
       _END_MASTER(myThid)
       intime0=int( (myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &              )/externForcingPeriod )
       intime1=
     &   int( (myForcingTime+externForcingPeriod-externForcingCycle*
     &              int((myForcingTime+externForcingPeriod)
     &                  /externForcingCycle)
     &              )/externForcingPeriod )

C--    SST
       _BEGIN_MASTER(myThid)
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'levtemp_m.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', SST, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'levtemp_m.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', SSTdot, myIter, myThid )
       _END_MASTER(myThid)
       _BARRIER
    
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
           SSTdot(i,j,bi,bj)=
     &     SSTdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    SSS
       _BEGIN_MASTER(myThid)
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'levsalt_m.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', SSS, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'levsalt_m.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', SSSdot, myIter, myThid )
       _END_MASTER(myThid)
       _BARRIER
    
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
           SSSdot(i,j,bi,bj)= 
     &     SSSdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    TauX
       _BEGIN_MASTER(myThid)
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'windx.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', fu, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'windx.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', fudot, myIter, myThid )
       _END_MASTER(myThid)
       _BARRIER
    
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
           fudot(i,j,bi,bj)=
     &     fudot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    TauY
       _BEGIN_MASTER(myThid)
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'windy.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', fv, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'windy.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', fvdot, myIter, myThid )
       _END_MASTER(myThid)
       _BARRIER
    
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
           fvdot(i,j,bi,bj)=
     &     fvdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C

C--    Qnet
       _BEGIN_MASTER(myThid)
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'Qnet.',intime0+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', Qnet, myIter, myThid )
       WRITE(filename(1:40),'(A,I10.10,A)') 
     &      'Qnet.',intime1+1,'.bin'
       CALL READ_FLD_XY_RS( filename, ' ', Qnetdot, myIter, myThid )
       _END_MASTER(myThid)
       _BARRIER
    
C      Set Qnetdot to the total change over the period
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)/(4200.*rhonil*delZ(1))
           Qnetdot(i,j,bi,bj) = 
     &     Qnetdot(i,j,bi,bj)/(4200.*rhonil*delZ(1))
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

C--    EmPmR
Cfix   WRITE(filename(1:40),'(A,I10.10,A)') 
Cfix &      'EmP_y.',intime0+1,'.bin'
       _BEGIN_MASTER(myThid)
       filename='EmP_y.bin'
       CALL READ_FLD_XY_RS( filename, ' ', EmPmR, myIter, myThid )
       _END_MASTER(myThid)
       _BARRIER
Cfix   WRITE(filename(1:40),'(A,I10.10,A)') 
Cfix &      'EmPmR.',intime1+1,'.bin'
Cfix   CALL READ_FLD_XY_RS( filename, ' ', EmPmRdot, myIter, myThid )
    
C      Set EmPmRdot to the total change over the period
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
Cfix       EmPmRdot(i,j,bi,bj) = EmPmRdot(i,j,bi,bj)-EmPmR(i,j,bi,bj)
C sign change is becuase data is infact P-E not E-P
           EmPmR(i,j,bi,bj) = -EmPmR(i,j,bi,bj)/(3153600000.*delZ(1))*35.
           EmPmRdot(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C      Interpolate EmPmR to correct time (necessary only for myTime=startTime)
       aWght=(myForcingTime-externForcingCycle*
     &               int(myForcingTime/externForcingCycle)
     &       )/externForcingPeriod - float(intime0)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)+aWght*EmPmRdot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      Convert EmPmRdot into the increment to EmPmR per time-step (here after)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           EmPmRdot(i,j,bi,bj)=EmPmRdot(i,j,bi,bj)*deltaTclock/externForcingPeriod 
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C
       _EXCH_XY_R4(SST  , myThid )
       _EXCH_XY_R4(SSS  , myThid )
       _EXCH_XY_R4(fu   , myThid )
       _EXCH_XY_R4(fv   , myThid )
       _EXCH_XY_R4(Qnet , myThid )
       _EXCH_XY_R4(EmPmR, myThid )
       _EXCH_XY_R4(SSTdot  , myThid )
       _EXCH_XY_R4(SSSdot  , myThid )
       _EXCH_XY_R4(fudot   , myThid )
       _EXCH_XY_R4(fvdot   , myThid )
       _EXCH_XY_R4(Qnetdot , myThid )
       _EXCH_XY_R4(EmPmRdot, myThid )
C
      ELSE

C      Increment SST,SSS,fu,fv,Qnet,EmPmR
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           SST(i,j,bi,bj) = SST(i,j,bi,bj)+SSTdot(i,j,bi,bj)
           SSS(i,j,bi,bj) = SSS(i,j,bi,bj)+SSSdot(i,j,bi,bj)
           fu(i,j,bi,bj) = fu(i,j,bi,bj)+fudot(i,j,bi,bj)
           fv(i,j,bi,bj) = fv(i,j,bi,bj)+fvdot(i,j,bi,bj)
           Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)+Qnetdot(i,j,bi,bj)
           EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)+EmPmRdot(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       IF (myThid.EQ.1 .AND. myForcingTime.LT.62208000.) THEN
        write(0,'(a,1p6e10.2)') 'SST,SS,fu,fv,Q,E-P = ',
     &   SST(1,12,1,1),SSS(1,12,1,1),fu(1,12,1,1),fv(1,12,1,1),
     &   Qnet(1,12,1,1),EmPmR(1,12,1,1)
       ENDIF

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
           EmPmR(i,j,bi,bj) = 0.
           fudot(i,j,bi,bj) = 0.
           fvdot(i,j,bi,bj) = 0.
           SSTdot(i,j,bi,bj) = 0.
           SSSdot(i,j,bi,bj) = 0.
           Qnetdot(i,j,bi,bj) = 0.
           EmPmRdot(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO


      RETURN
      END
