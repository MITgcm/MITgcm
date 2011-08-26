C $Header: /u/gcmpack/MITgcm/pkg/matrix/matrix_output.F,v 1.3 2011/08/26 19:47:00 jmc Exp $
C $Name:  $

#include "MATRIX_OPTIONS.h"

CBOP
C     !ROUTINE: MATRIX_OUTPUT

C     !INTERFACE:
      SUBROUTINE MATRIX_OUTPUT(
     I                  myTime, myIter, myThid )

C     !DESCRIPTION:
C     This is the controlling routine for MATRIX output. It is
C     called from DO_THE_MODEL_IO.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
C     myTime    :: model time
C     myIter    :: time-step number
C     myThid    :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_MATRIX
C     !LOCAL VARIABLES:

      IF ( myIter.EQ.nIter0 ) THEN
C       Write grid info for pkg/matrix
        CALL MATRIX_WRITE_GRID( myThid )
      ELSE
C       Write matrix to file
        CALL MATRIX_WRITE_TENDENCY( myTime, myIter, myThid )
      ENDIF

#endif /* ALLOW_MATRIX */

      RETURN
      END
