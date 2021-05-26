#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_PRESCRIBE_READ
C     !INTERFACE:
      SUBROUTINE OBCS_PRESCRIBE_READ (
     I                          myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *============================================================*
C     | SUBROUTINE OBCS_PRESCRIBE_READ
C     *============================================================*
C     | read open boundary conditions from file
C     | N.B.: * uses exf and cal routines for file/record handling
C     |       * uses ctrl routines for control variable handling
C     *============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OBCS_PRESCRIBE

C     !LOCAL VARIABLES:
CEOP

# ifdef ALLOW_EXF
      IF ( useEXF ) THEN
        CALL OBCS_EXF_LOAD( myTime, myIter, myThid )
      ENDIF
# endif /* ALLOW_EXF */

      IF ( .NOT. useEXF ) THEN
cph#ifndef ALLOW_AUTODIFF_TAMC
       CALL OBCS_FIELDS_LOAD( myTime, myIter, myThid )
cph#else
cph       STOP 'PH HAS DISABLED THIS RUNTIME OPTION FOR ALLOW_EXF'
cph#endif
      ENDIF

#endif /* ALLOW_OBCS_PRESCRIBE */

      RETURN
      END
