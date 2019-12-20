#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SHELFICE_REMESH_CALC_W
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_CALC_W (
     I                             uFld, vFld,
     I                             myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_CALC_W
C     | o Integrate the continuity Eq : compute vertical velocity
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uFld     :: Zonal velocity ( m/s )
C     vFld     :: Meridional velocity ( m/s )
C     myThid   :: my Thread Id. number
      INTEGER myThid
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_SHELFICE
#ifdef ALLOW_SHELFICE_REMESHING

C     !LOCAL VARIABLES:
C     Local variables in common block

C     Local variables
C     bi,bj    :: tile index
C     i,j,k    :: Loop counters
      INTEGER k,bi,bj
#ifdef ALLOW_OBCS
      INTEGER i, j
#endif
#ifndef ALLOW_ADDFLUID
      _RL addMass(1)
#endif /* ndef ALLOW_ADDFLUID */
#if (defined NONLIN_FRSURF) && !(defined DISABLE_RSTAR_CODE)
      _RL rStarDhDt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#else
      _RL rStarDhDt(1)
#endif
CEOP

C--   Start bi,bj loops
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO k=Nr,1,-1
C--    Integrate continuity vertically for vertical velocity

       CALL INTEGRATE_FOR_W(
     I                       bi, bj, k, uFld, vFld,
     I                       addMass, rStarDhDt,
     O                       wVel,
     I                       myThid )

#ifdef ALLOW_OBCS
C--    reset W to zero outside the OB interior region
       IF ( useOBCS ) THEN
         DO j=1,sNy
          DO i=1,sNx
             wVel(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)*maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDIF
#endif /* ALLOW_OBCS */

C-    End DO k=Nr,1,-1
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   End bi,bj loops
       ENDDO
      ENDDO

      _EXCH_XYZ_RL( wVel , myThid )
#endif
#endif

      RETURN
      END
