#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#include "OFFLINE_OPTIONS.h"

CBOP
C     !ROUTINE: OFFLINE_MODEL_IO
C     !INTERFACE:
      SUBROUTINE OFFLINE_MODEL_IO( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OFFLINE_MODEL_IO                                
C     | o Controlling routine for IO in offline model  time-stepping  
C     |   loop.                                                   
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"


      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

CEOP

C--   Write model state to binary file
      CALL OFFLINE_STATE( myTime, myIter, myThid )

#ifdef ALLOW_TIMEAVE
C--   Do time averages
      IF (taveFreq.GT.0. .AND. myIter.NE.nIter0 ) THEN
c       CALL TIMEAVE_STATV_WRITE( myTime, myIter, myThid )
#ifdef ALLOW_PTRACERS
       IF ( usePTRACERS )
     & CALL PTRACERS_STATV_WRITE( myTime, myIter, myThid )
#endif
      ENDIF
#endif

#ifdef ALLOW_GCHEM
C--   Do gchem diagnostics.
      IF ( useGCHEM )
     & CALL GCHEM_DIAGS( myTime, myIter, myThid )
#endif


      RETURN
      END
