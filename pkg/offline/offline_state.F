C $Header: /u/gcmpack/MITgcm/pkg/offline/Attic/offline_state.F,v 1.5 2005/08/24 23:23:34 jmc Exp $
C $Name:  $

#include "OFFLINE_OPTIONS.h"

#undef  MULTIPLE_RECORD_STATE_FILES

CBOP
C     !ROUTINE: OFFLINE_STATE 
C     !INTERFACE:
      SUBROUTINE OFFLINE_STATE ( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OFFLINE_STATE                                    
C     | o Controlling routine for IO to dump model state.         
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h" 
#include "EOS.h"
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      _RL     myTime
      INTEGER myThid
      INTEGER myIter

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER iRec
CEOP

      IF ( 
     &     DIFFERENT_MULTIPLE(dumpFreq,myTime,deltaTClock)
     &  .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                             myTime.EQ.startTime  )
     &   ) THEN
         IF ( dumpFreq .EQ. 0.0 ) THEN
            iRec = 1
         ELSE
            iRec = int ( (myTime-startTime) / dumpFreq +1.5 )
         ENDIF
         
C--   Going to really do some IO. Make everyone except master thread wait.
         _BARRIER
         _BEGIN_MASTER( myThid )

C--     Write model fields

#ifdef MULTIPLE_RECORD_STATE_FILES

C       Write each snap-shot as a new record in one file per variable
C       - creates relatively few files but these files can become huge

#else /* MULTIPLE_RECORD_STATE_FILES */

C       Write each snap-shot as a new file (original and default method)
C       - creates many files but for large configurations is easier to
C         transfer analyse a particular snap-shots
        WRITE(suff,'(I10.10)') myIter

#ifdef ALLOW_PTRACERS
       IF (usePTRACERS) CALL PTRACERS_WRITE_STATE(myIter,myTime,myThid)
#endif /* ALLOW_PTRACERS */
         _END_MASTER( myThid )
         _BARRIER

#endif

      ENDIF

      RETURN
      END
