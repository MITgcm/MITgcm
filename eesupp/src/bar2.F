C $Header: /u/gcmpack/MITgcm/eesupp/src/bar2.F,v 1.7 2009/08/04 18:01:37 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: BAR2_INIT

C     !INTERFACE:
      SUBROUTINE BAR2_INIT( myThid )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *=====================================================================*
C     | SUBROUTINE BAR2\_INIT
C     | o Setup global barrier data structures.
C     *=====================================================================*
C     | Initialise global barrier data structures that can be used in
C     | conjunction with MPI or that can also be used to create
C     *=====================================================================*

C     !USES:
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "BAR2.h"
C

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number of this instance of BAR2_INIT
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I :: Loop counter
      INTEGER I
CEOP

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

CBOP

C     !ROUTINE: BAR2

C     !INTERFACE:
      SUBROUTINE BAR2( myThid )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *=====================================================================*
C     | SUBROUTINE BAR2
C     | o Global barrier routine.
C     *=====================================================================*
C     | Implements a simple true shared memory barrier that uses a global
C     | heap array that all threads can access to synchronise. Each thread
C     | writes to a predefined location. One thread polls the locations. Other
C     | threads poll an all clear assertion location. Once the polling
C     | thread that is looping over locations sees writes for each thread is
C     | writes the all clear assertion location and everyone proceeds. A
C     | cyclic series of locations is used to ensure that race conditions do
C     | not occur. A few simple statistics are recorded giving number of
C     | barrier calls, max, min and aggregate polling loop counts.
C     *=====================================================================*

C     !USES:
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "BAR2.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number of this instance of BAR2
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     myLevel   :: Temp. to hold "active" barrier level
C     nDone     :: Temp. for counting number of threads that
C                  have reached the barrier.
C     I         :: Loop counter
C     spinCount :: Temp. for doing statistics on how many
C                  times barrier code looped.
      INTEGER myLevel
      INTEGER nDone
      INTEGER I
      INTEGER spinCount
CEOP

#ifdef USE_OMP_THREADING
C$OMP BARRIER
      BAR2_barrierCount(1,myThid) = BAR2_barrierCount(1,myThid)+1
Cdbg C$OMP BARRIER
Cdbg       DO I = 2, nThreads
Cdbg        IF ( BAR2_barrierCount(1,I) .NE. BAR2_barrierCount(1,1) ) THEN
Cdbg         PRINT *, BAR2_barrierCount(1,1:nThreads)
Cdbg         CALL SYSTEM('sleep 1')
Cdbg         PRINT *, BAR2_barrierCount(1,1:nThreads)
Cdbg         Stop ' bar2 OUT OF SYNC '
Cdbg        ENDIF
Cdbg       ENDDO
Cdbg C$OMP BARRIER
      RETURN
#endif

      spinCount = 0

      IF ( myThid .NE. 1 ) THEN

       BAR2_level(1,myThid) = BAR2_level(1,myThid)+1
       myLevel              = BAR2_level(1,myThid)
   10  CONTINUE
        IF ( BAR2_level(1,1) .EQ. myLevel  ) GOTO 11
        spinCount = spinCount+1
        CALL FOOL_THE_COMPILER( BAR2_level(1,1) )
       GOTO 10
   11  CONTINUE

      ELSE

       myLevel = BAR2_level(1,1)
   12  CONTINUE
        CALL FOOL_THE_COMPILER( BAR2_level(1,1) )
        nDone   = 1
        DO I = 2, nThreads
         IF ( BAR2_level(1,1) .EQ. BAR2_level(1,I)-1 ) nDone = nDone+1
        ENDDO
        spinCount = spinCount+1
       IF ( nDone .LT. nThreads ) GOTO 12

       BAR2_level(1,1) = myLevel+1

      ENDIF

      BAR2_barrierCount(1,myThid) = BAR2_barrierCount(1,myThid)+1
      BAR2_spinsCount(1,myThid) = BAR2_spinsCount(1,myThid)+spinCount
      BAR2_spinsMax  (1,myThid) = MAX(BAR2_spinsMax(1,myThid),spinCount)
      BAR2_spinsMin  (1,myThid) = MIN(BAR2_spinsMin(1,myThid),spinCount)

      RETURN
      END
