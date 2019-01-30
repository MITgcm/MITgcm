#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SHELFICE_REMESHING
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESHING(
     I    myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESHING
C     | o If SHELFICERemeshFrequencey is a multiple of mytime,
C     |   checks if etaN under shelfice triggers a remesh
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     SHELFICE_REMESHING
C       |
C       |-- SHELFICE_UPDATE_MASKS_REMESH
C       |    |
C       |    |-- SHELFICE_REMESH_UVMASKS
C       |    |-- OBCS_UPDATE_REMESH (called only if horiz remeshing)
C       |    |-- CALC_SURF_DR
C       |    |-- CALC_SURF_DR
C       |    |-- UPDATE_SURF_DR
C       |
C       |-- SHELFICE_REMESH_UPDATE_RMINSURF
C       |
C       |-- UPDATE_CG2D
C       |
C       |-- SHELFICE_INTEGR_CONT_REMESH

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#include "GRID.h"
#include "CG2D.h" 
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

      INTEGER myThid
      INTEGER myIter
      _RL myTime

#ifdef ALLOW_SHELFICE
#ifdef ALLOW_SHELFICE_REMESHING

      INTEGER k, bi, bj, i, j
      Real*8 addMass(1)
      Real*8 rStarDhDt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

        IF ((myTime/ShelficeRemeshFrequency) .EQ.
     &     INT (myTime/ShelficeRemeshFrequency) .AND. myTime
     &      .NE. 0.0        ) THEN
         CALL SHELFICE_REMESH_C_MASK (mytime, myiter, myThid)
         CALL SHELFICE_REMESH_RMINSURF(myTime, myIter, myThid)
C-- DNG: CALC_SURF_DR is called twice as the hfac_surfNm1 etc fields
C-- must be initialised since maskC has changed
         CALL UPDATE_CG2D(myTime,myIter, myThid)
         CALL SHELFICE_REMESH_CALC_W (uVel ,vVel, myThid)
        ENDIF 
#endif /* ALLOW_SHELFICE_REMESHING */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
