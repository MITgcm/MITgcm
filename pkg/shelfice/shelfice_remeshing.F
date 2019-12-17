#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_REMESHING
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESHING(
     I                    myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESHING
C     | o When myTime is a multiple of SHELFICEremeshFrequency
C     |   checks if etaN under shelfice triggers a remesh
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     SHELFICE_REMESHING
C       |
C       |-- SHELFICE_REMESH_C_MASK
C       |    |
C       |    |-- SHELFICE_REMESH_UVMASKS
C       |    |-- CALC_SURF_DR
C       |    |-- UPDATE_SURF_DR
C       |    |-- CALC_SURF_DR
C       |
C       |-- SHELFICE_REMESH_RMINSURF
C       |
C       |-- UPDATE_CG2D
C       |
C       |-- SHELFICE_REMESH_CALC_W

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHELFICE_REMESHING
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
c     INTEGER k, bi, bj, i, j
CEOP

      IF ( SHELFICEremeshFrequency.GT.zeroRL
     &                        .AND. myTime.GT.baseTime ) THEN
       IF (  DIFFERENT_MULTIPLE( SHELFICEremeshFrequency,
     &                           myTime, deltaTClock ) ) THEN
c     IF ((myTime/ShelficeRemeshFrequency) .EQ.
c    &     INT (myTime/ShelficeRemeshFrequency) .AND. myTime
c    &      .NE. 0.0        ) THEN
         CALL SHELFICE_REMESH_C_MASK  ( myTime, myIter, myThid )
         CALL SHELFICE_REMESH_RMINSURF( myTime, myIter, myThid )
         CALL UPDATE_CG2D             ( myTime, myIter, myThid )
         CALL SHELFICE_REMESH_CALC_W( uVel, vVel, myThid )
       ENDIF
      ENDIF
#endif /* ALLOW_SHELFICE_REMESHING */

      RETURN
      END
