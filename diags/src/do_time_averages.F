#include "CPP_EEOPTIONS.h"

      SUBROUTINE DO_TIME_AVERAGES(
     I     myCurrentTime, myIter, bi, bj, K, kUp, kDown,
     I     K13,K23,KapGM,ConvectCount,
     I     myThid)
C     /==========================================================\
C     | SUBROUTINE DO_TIME_AVERAGES                              |
C     | o Time averaging routine for U, V, T, S, UT, VT in model |
C     |   main time-stepping                                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "AVER.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter, bi, bj, K, kUp, kDown
      _RL     myCurrentTime
      _RL K13   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL K23   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KapGM (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ConvectCount (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C     == Local variables ==
      _RL DDTT
      LOGICAL TOP_LEVEL
 
      TOP_LEVEL=K.EQ.1

C     Initialize fields for the first call ever
      IF ( myIter .EQ. nIter0 ) THEN
         CALL TIMEAVER_INI_XYZ(thetatave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(salttave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(uVeltave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(vVeltave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(wVeltave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(uttave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(vttave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(k13tave, bi, bj, K, myThid)
         CALL TIMEAVER_INI_XYZ(k23tave, bi, bj, K, myThid)
         IF (TOP_LEVEL) CALL TIMEAVER_INI_XY(KapGMtave, bi, bj, myThid)
         CALL TIMEAVER_INI_XYZ(ConvectCountTave, bi, bj, K, myThid)
         TimeAve(k,bi,bj)=0.
         InterTimeAve(k,bi,bj)=0.
      ENDIF
      
      IF ( myIter .EQ. nIter0 .OR.
     & DIFFERENT_MULTIPLE
     &     (taveFreq,myCurrentTime,myCurrentTime-deltaTClock) )
     &   THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF

C     Time Averages of single fields
      CALL TIMEAVER_1FLD_XYZ(theta, thetatave, DDTT, bi, bj, K, myThid)
      CALL TIMEAVER_1FLD_XYZ(salt,  salttave,  DDTT, bi, bj, K, myThid)
      CALL TIMEAVER_1FLD_XYZ(uVel,  uVeltave,  DDTT, bi, bj, K, myThid)
      CALL TIMEAVER_1FLD_XYZ(vVel,  vVeltave,  DDTT, bi, bj, K, myThid)
      CALL TIMEAVER_1FLD_XYZ(wVel,  wVeltave,  DDTT, bi, bj, K, myThid)

C     Time Averages of "double" fields
      CALL TIMEAVER_2FLD_XYZ(theta, uVel, uttave, 1, DDTT, 
     & bi, bj, K, myThid)
      CALL TIMEAVER_2FLD_XYZ(theta, vVel, vttave, 2, DDTT, 
     & bi, bj, K, myThid)

C     Keep record of how much time has been integrated over
      TimeAve(k,bi,bj)=TimeAve(k,bi,bj)+DDTT

C     Time Averages of "intermediate" fields
      DDTT=deltaTclock
      CALL TIMEAVER_INTER_XYZ(Nr, K13, k13tave, DDTT, 
     & bi, bj, K, K, myThid)
      CALL TIMEAVER_INTER_XYZ(Nr, K23, k23tave, DDTT, 
     & bi, bj, K, K, myThid)
      IF (TOP_LEVEL)
     &  CALL TIMEAVER_INTER_XY(KapGM, KapGMtave, DDTT, bi, bj, myThid)
      InterTimeAve(k,bi,bj)=InterTimeAve(k,bi,bj)+DDTT
      CALL TIMEAVER_INTER_XYZ(Nr, ConvectCount, ConvectCountTave, DDTT,
     & bi, bj, K, K, myThid)

      RETURN
      END
