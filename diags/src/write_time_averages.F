#include "CPP_EEOPTIONS.h"

      SUBROUTINE WRITE_TIME_AVERAGES(myCurrentTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE WRITE_TIME_AVERAGES                           |
C     |                                                          |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AVER.h"
#include "DYNVARS.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      REAL    myCurrentTime
      INTEGER prevPrec

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     TimeAve - total time over average
      CHARACTER*(MAX_LEN_FNAM) suff
      REAL DDTT
      INTEGER bi,bj,k
 
C     Final Time Averages and Dump Files if needed
      IF ( myIter.NE.nIter0 .AND. 
     & DIFFERENT_MULTIPLE(taveFreq,myCurrentTime,myCurrentTime-deltaTClock)
     &   ) THEN

       WRITE(suff,'(I10.10)') myIter

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nz
          CALL TIMEAVER_FINAL_XYZ(thetatave,timeave,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(salttave,timeave,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(uVeltave,timeave,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(vVeltave,timeave,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(uttave,timeave,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(vttave,timeave,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(k13tave,intertimeave,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(k23tave,intertimeave,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(wVeltave,intertimeave,bi,bj,K,myThid)
         ENDDO
        ENDDO
       ENDDO

C      Write to files
       _BARRIER
       _BEGIN_MASTER( myThid )
        prevPrec = writeBinaryPrec
        writeBinaryPrec = precFloat32
        CALL WRITE_FLD_XYZ_RL('Ttave.',suff,thetatave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('Stave.',suff,salttave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('uVeltave.',suff,uVeltave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('vVeltave.',suff,vVeltave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('UTtave.',suff,uttave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('VTtave.',suff,vttave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('K13tave.',suff,k13tave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('K23tave.',suff,k23tave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('WTtave.',suff,wVeltave,myIter,myThid)
        writeBinaryPrec = prevPrec
       _END_MASTER( myThid )
       _BARRIER

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nz

C      Reset the averages to zero
          CALL TIMEAVER_INI_XYZ(thetatave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(salttave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(uVeltave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(vVeltave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(uttave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(vttave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(k13tave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(k23tave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(wVeltave, bi,bj,K,myThid)
          TimeAve(k,bi,bj)=0.
          InterTimeAve(k,bi,bj)=0.

C      Time Averages of single fields
          DDTT=0.5*deltaTclock
          CALL TIMEAVER_1FLD_XYZ(theta, thetatave, DDTT, bi,bj,K,myThid)
          CALL TIMEAVER_1FLD_XYZ(salt,  salttave,  DDTT, bi,bj,K,myThid)
          CALL TIMEAVER_1FLD_XYZ(uVel,  uVeltave,  DDTT, bi,bj,K,myThid)
          CALL TIMEAVER_1FLD_XYZ(vVel,  vVeltave,  DDTT, bi,bj,K,myThid)

C      Time Averages of "double" fields
          CALL TIMEAVER_2FLD_XYZ(theta, uVel, uttave, 1, DDTT, bi,bj,K,myThid)
          CALL TIMEAVER_2FLD_XYZ(theta, vVel, vttave, 2, DDTT, bi,bj,K,myThid)

C      Keep record of how much time has been integrated over
          TimeAve(k,bi,bj)=TimeAve(k,bi,bj)+DDTT

         ENDDO
        ENDDO
       ENDDO

      ENDIF
 
      RETURN
      END
