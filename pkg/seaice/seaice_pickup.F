C $Header:

#include "SEAICE_OPTIONS.h"

C--   File seaice_pickup.F: Routines to handle reading and writing
C--   sea ice pickup files.
C--   Contents
C--   o SEAICE_READ_PICKUP  - Read sea ice pickup file for restarting.
C--   o SEAICE_WRITE_PICKUP - Write out sea ice pickup file.

CStartofinterface
      SUBROUTINE SEAICE_READ_PICKUP ( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_READ_PICKUP                            |
C     | o Read in sea ice pickup file for restarting.            |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_SEAICE

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn

C--    Going to do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

C--    Read ice model fields
       WRITE(fn,'(A,I10.10)') 'pickup_seaice.',nIter0
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, TICE , 1, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, YNEG , 2, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, HSNOW, 3, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, UICE , 2, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, VICE , 3, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, HEFF , 4, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, AREA , 5, myThid )

       _END_MASTER( myThid )
       _BARRIER

C--    Update overlap regions 
       CALL SEAICE_EXCH ( UICE, myThid )
       CALL SEAICE_EXCH ( VICE, myThid )
       CALL SEAICE_EXCH ( HEFF, myThid )
       CALL SEAICE_EXCH ( AREA, myThid )
       _EXCH_XY_R8(TICE , myThid )
       _EXCH_XY_R8(YNEG , myThid )
       _EXCH_XY_R8(HSNOW, myThid )

#endif /* ALLOW_SEAICE */

      RETURN
      END

CStartofinterface
      SUBROUTINE SEAICE_WRITE_PICKUP ( modelEnd, myCurrentTime, 
     &                              myIt, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_WRITE_PICKUP                           |
C     | o Write sea ice pickup file for restarting.              |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     modelEnd - Checkpoint call at end of model run.
C     myCurrentTime - Current time of simulation ( s )
C     myIt     - Iteration number
C     myThid   - Thread number for this instance of the routine.
      LOGICAL modelEnd    
      INTEGER myThid
      INTEGER myIt
      _RL     myCurrentTime
CEndofinterface

#ifdef ALLOW_SEAICE

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     == Local variables ==
C     permCheckPoint - Flag indicating whether a permanent checkpoint
C                      will be written.
      LOGICAL permCheckPoint  
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      LOGICAL lgf

      permCheckPoint = .FALSE.
      permCheckPoint=
     & DIFFERENT_MULTIPLE(pChkptFreq,myCurrentTime,
     &                    myCurrentTime-deltaTClock)

      IF (
     &    (.NOT. modelEnd .AND. (
     &     permCheckPoint
     &     .OR.
     &     DIFFERENT_MULTIPLE(chkptFreq,
     &    myCurrentTime,myCurrentTime-deltaTClock)
     &     ) 
     &    ) 
     &     .OR.
     &    (
     &     modelEnd 
     &     .AND. .NOT.
     &     permCheckPoint 
     &     .AND. .NOT.
     &     DIFFERENT_MULTIPLE(chkptFreq,
     &    myCurrentTime,myCurrentTime-deltaTClock)
     &    )
     & ) THEN

C--    Going to do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

       prec = precFloat64
       lgf = globalFile

C--    Write model fields
       IF ( permCheckPoint ) THEN
        WRITE(fn,'(A,I10.10)') 'pickup_seaice.',myIt
       ELSE
        WRITE(fn,'(A,A)') 'pickup_seaice.',checkPtSuff(nCheckLev)
       ENDIF
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,  TICE, 1, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,  YNEG, 2, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, HSNOW, 3, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  UICE, 2, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  VICE, 3, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  HEFF, 4, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  AREA, 5, myIt,myThid)

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

#endif /* ALLOW_SEAICE */

      RETURN
      END
