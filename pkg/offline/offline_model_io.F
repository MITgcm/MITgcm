C $Header: /u/gcmpack/MITgcm/pkg/offline/Attic/offline_model_io.F,v 1.7 2005/08/18 18:22:00 stephd Exp $
C $Name:  $

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
c      CALL OFFLINE_STATE( myTime, myIter, myThid )
c
c#ifdef ALLOW_TIMEAVE
cC--   Do time averages
c      IF (taveFreq.GT.0. .AND. myIter.NE.nIter0 ) THEN
cc       CALL TIMEAVE_STATV_WRITE( myTime, myIter, myThid )
c#ifdef ALLOW_PTRACERS
c       IF ( usePTRACERS )
c     & CALL PTRACERS_WRITE_TIMEAVE( myTime, myIter, myThid )
c#endif
c      ENDIF
c#endif

#ifdef ALLOW_GCHEM
C--   Do gchem diagnostics.
      IF ( useGCHEM )
     & CALL GCHEM_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_PTRACERS
C--   Do ptracer output.
      IF ( usePTRACERS )
     & CALL PTRACERS_OUTPUT( myTime, myIter, myThid )
#endif

      RETURN
      END
