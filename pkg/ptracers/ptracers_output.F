C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_output.F,v 1.5 2013/03/19 22:02:22 jahn Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C     !ROUTINE: PTRACERS_OUTPUT

C     !INTERFACE:
      SUBROUTINE PTRACERS_OUTPUT(
     I     myTime, myIter, myThid )

C     !DESCRIPTION:
C     This is the controlling routine for PTRACERS output. It is
C     called from DO_THE_MODEL_IO.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"

C     !INPUT PARAMETERS:
C     myIter    :: time-step number
C     myTime    :: model time
C     myThid    :: thread number
      INTEGER myIter
      INTEGER myThid
      _RL myTime
CEOP

#ifdef ALLOW_PTRACERS
C     !LOCAL VARIABLES:

C     Write snapshot output
      CALL PTRACERS_WRITE_STATE( myIter, myTime, myThid )
#ifdef ALLOW_TIMEAVE
C     Write time average output
      IF ( PTRACERS_taveFreq.GT.0. .AND. myIter.NE.nIter0 ) THEN
       CALL PTRACERS_WRITE_TIMEAVE( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_TIMEAVE */
#ifdef ALLOW_MONITOR
C     Compute and print monitor statistics
      CALL PTRACERS_MONITOR( myIter, myTime, myThid )
#endif /* ALLOW_MONITOR */

#endif /* ALLOW_PTRACERS */

      RETURN
      END
