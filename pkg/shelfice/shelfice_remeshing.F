C $Header: /u/gcmpack/MITgcm_contrib/verification_other/shelfice_remeshing/code/shelfice_remeshing.F,v 1.8 2018/05/20 13:04:08 dgoldberg Exp $
C $Name:  $
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
          IF ((myTime/ShelficeRemeshFrequency) .EQ.
     &     INT (myTime/ShelficeRemeshFrequency) .AND. myTime
     &      .NE. 0.0        ) THEN
         CALL UPD_MASKS_REMESH (myThid)
         CALL SHELFICE_UPDATE_KSURF_REMESH( myThid )
#ifdef ALLOW_OBCS
         if (useobcs) then
         CALL OBCS_UPDATE_REMESH( myThid )
         endif
#endif
         CALL UPD_LINEAR_PHISURF(myThid)
C-- DNG: CALC_SURF_DR is called twice as the hfac_surfNm1 etc fields
C-- must be initialised since maskC has changed		
         CALL CALC_SURF_DR( etaH, myTime, myIter, myThid )
         CALL CALC_SURF_DR( etaH, myTime, myIter, myThid )
         CALL UPDATE_SURF_DR(.FALSE., myTime, myIter, myThid)
         CALL UPDATE_CG2D(myTime,myIter, myThid)
         CALL INTEGR_CONTINUITY (uVel ,vVel, myTime, niter0, myThid)
         ENDIF 
#endif /* ALLOW_SHELFICE_REMESHING */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
