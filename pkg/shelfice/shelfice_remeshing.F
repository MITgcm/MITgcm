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
C       |-- SHELFICE_REMESH_UV_MASK
C       |
C       |-- CALC_SURF_DR
C       |-- UPDATE_SURF_DR
C       |-- CALC_SURF_DR
C       |
C       |-- SHELFICE_REMESH_STATE
C       |
C       |-- SHELFICE_REMESH_CALC_W

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SURFACE.h"
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
C     k1SurfC   :: surface level index (at cell center) before remeshing
C     k1SurfW   :: surface level index (at cell W.Edge) before remeshing
C     k1SurfS   :: surface level index (at cell S.Edge) before remeshing
C     mergeFacC :: merging weight for tracers (cell center)
C     mergeFacW :: merging weight for U component velocity
C     mergeFacS :: merging weight for V component velocity
      INTEGER k1SurfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER k1SurfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER k1SurfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mergeFacC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
      _RL mergeFacW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
      _RL mergeFacS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
CEOP

      IF ( SHELFICEremeshFrequency.GT.zeroRL
     &                        .AND. myTime.GT.baseTime ) THEN
       IF (  DIFFERENT_MULTIPLE( SHELFICEremeshFrequency,
     &                           myTime, deltaTClock ) ) THEN
         CALL SHELFICE_REMESH_C_MASK(
     O                 k1SurfC, mergeFacC,
     I                 myTime, myIter, myThid )
         CALL SHELFICE_REMESH_UV_MASK(
     O                 k1SurfW, k1SurfS, mergeFacW, mergeFacS,
     I                 myTime, myIter, myThid )

         CALL CALC_SURF_DR( etaHnm1, myTime, myIter, myThid )
         CALL UPDATE_SURF_DR(.TRUE., myTime, myIter, myThid )
         CALL CALC_SURF_DR( etaH, myTime, myIter, myThid )

         CALL SHELFICE_REMESH_STATE(
     I                 k1SurfC, k1SurfW, k1SurfS, mergeFacC,
     U                 mergeFacW, mergeFacS,
     I                 myTime, myIter, myThid )
         CALL SHELFICE_REMESH_CALC_W(
     I                 uVel, vVel,
     O                 wVel,
     I                 myIter, myThid )
       ENDIF
      ENDIF
#endif /* ALLOW_SHELFICE_REMESHING */

      RETURN
      END
