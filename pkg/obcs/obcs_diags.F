
#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_DIAGS(myCurrentTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE OBCS_DIAGS                               |
C     | o Controlling routine for IO in model main time-stepping |
C     |   loop.                                                  |
C     |==========================================================|
C     | Many systems do not have thread safe IO so it is easier  |
C     | to lump everything together and do dumping of fields     |
C     | and updating of forcing terms in a single place.         |
C     | The approach to IO used here is that writes are only     |
C     | performed by thread 1 and that a process only writes out |
C     | its data ( it does not know about anyone elses data!)    |
C     | Reading on the other hand is assumed to be from a file   |
C     | containing all the data for all the processes. Only the  |
C     | portion of data of interest to this process is actually  |
C     | loaded. To work well this assumes the existence of some  |
C     | reliable tool to join datasets together at the end of a  |
C     | run - see joinds.p                                       |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "OBCS.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      _RL    myCurrentTime

#ifdef ALLOW_OBCS
C     == Local variables ==

C--   Generaly only thread 1 does IO here. It can not start until 
C--   all threads fields are ready.
      IF (
     &  DIFFERENT_MULTIPLE(dumpFreq,myCurrentTime,deltaTClock)
     &  .AND. OBCSprintDiags
     & ) THEN

       _BARRIER

#ifdef ALLOW_OBCS_NORTH
        CALL PLOT_FIELD_XZRL( OBNu, 'OBNu' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBNv, 'OBNv' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBNt, 'OBNt' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBNs, 'OBNs' , Nr, myIter, mythid )
#endif

#ifdef ALLOW_OBCS_SOUTH
        CALL PLOT_FIELD_XZRL( OBSu, 'OBSu' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBSv, 'OBSv' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBSt, 'OBSt' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBSs, 'OBSs' , Nr, myIter, mythid )
#endif

#ifdef ALLOW_OBCS_EAST
        CALL PLOT_FIELD_YZRL( OBEu, 'OBEu' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBEv, 'OBEv' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBEt, 'OBEt' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBEs, 'OBEs' , Nr, myIter, mythid )
#endif

#ifdef ALLOW_OBCS_WEST
        CALL PLOT_FIELD_YZRL( OBWu, 'OBWu' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBWv, 'OBWv' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBWt, 'OBWt' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBWs, 'OBWs' , Nr, myIter, mythid )
#endif

      ENDIF

#endif

      RETURN
      END
 
