#include "CPP_EEOPTIONS.h"

      SUBROUTINE BAR2_INIT( myThid )
      IMPLICIT NONE
C
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "BAR2.h"
C
      INTEGER myThid
      INTEGER I
C
      DO I = 1, lShare4
       BAR2_level(I,myThid) = 0
       BAR2_barrierCount(I,myThid) = 0
       BAR2_spinsCount(I,myThid) = 0
       BAR2_spinsCount(I,myThid) = 0
       BAR2_spinsMax  (I,myThid) = 0
       BAR2_spinsMin  (I,myThid) = 1000000000
      ENDDO
C
      bar2CollectStatistics = .TRUE.
C
      RETURN
      END
      SUBROUTINE BAR2( myThid )
      IMPLICIT NONE
C
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "BAR2.h"
C
      INTEGER myThid
C
      INTEGER myLevel
      INTEGER nDone
      INTEGER I
      INTEGER spinCount
C
      spinCount = 0
C
      IF ( myThid .NE. 1 ) THEN

       BAR2_level(1,myThid) = BAR2_level(1,myThid)+1
       myLevel              = BAR2_level(1,myThid)
   10  CONTINUE
        IF ( BAR2_level(1,1) .EQ. myLevel  ) GOTO 11
        spinCount = spinCount+1
        CALL FOOL_THE_COMPILER( BAR2_level )
       GOTO 10
   11  CONTINUE

      ELSE

       myLevel = BAR2_level(1,1)
   12  CONTINUE
        CALL FOOL_THE_COMPILER( BAR2_level )
        nDone   = 1
        DO I = 2, nThreads
         IF ( BAR2_level(1,1) .EQ. BAR2_level(1,I)-1 ) nDone = nDone+1
        ENDDO
        spinCount = spinCount+1
       IF ( nDone .LT. nThreads ) GOTO 12

       BAR2_level(1,1) = myLevel+1

      ENDIF
C
      BAR2_barrierCount(1,myThid) = BAR2_barrierCount(1,myThid)+1
      BAR2_spinsCount(1,myThid) = BAR2_spinsCount(1,myThid)+spinCount
      BAR2_spinsMax  (1,myThid) = MAX(BAR2_spinsMax(1,myThid),spinCount)
      BAR2_spinsMin  (1,myThid) = MIN(BAR2_spinsMin(1,myThid),spinCount)
C
      RETURN
      END

