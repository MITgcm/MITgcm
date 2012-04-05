C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_init_fixed.F,v 1.1 2012/04/05 21:59:03 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: CAL_INIT_FIXED

C !INTERFACE:
      SUBROUTINE CAL_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialise CAL variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('CAL_INIT_FIXED',myThid)
#endif

C     Check consistency of the date specifications with the MITgcm
C     startTime, endTime, and nTimeSteps, then set all the calendar
C     parameters.
      CALL CAL_SET(
     I              startTime, endTime, deltaTclock,
     I              nIter0, nEndIter, nTimeSteps,
     I              myThid )

      CALL CAL_SUMMARY( myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('CAL_INIT_FIXED',myThid)
#endif

      RETURN
      END
