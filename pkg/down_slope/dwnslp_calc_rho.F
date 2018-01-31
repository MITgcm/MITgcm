#include "DWNSLP_OPTIONS.h"

CBOP
C     !ROUTINE: DWNSLP_CALC_RHO
C     !INTERFACE:
      SUBROUTINE DWNSLP_CALC_RHO(
     I                tFld, sFld,
     O                rhoLoc,
     I                k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DWNSLP_CALC_RHO
C     | o Calculates [rho(S,T,z)-rhoConst] of a 2-D slice
C     |   filling land-points with bottom density
C     *==========================================================*
C     | Note: could move this S/R to model/src (if needed) since
C     |  it does not contain anything specific to Down-Slope pkg
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     tFld      :: Pot.Temperature (3-D array)
C     sFld      :: Salinity (3-D array)
C     rhoLoc    :: In-situ density [kg/m3] (2-D array) computed at z=rC ;
C     k         :: current vertical index
C     bi,bj     :: Tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current time-step number
C     myThid    :: my Thread Id number
      _RL     tFld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     sFld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     rhoLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     tLoc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     sLoc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j,kl

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   make a local copy of Temp & salt, copying bottom values further down
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
c        kl = k
c        IF ( kLowC(i,j,bi,bj).GE.1 ) THEN
c          IF ( k.GT.kLowC (i,j,bi,bj) ) k = kLowC (i,j,bi,bj)
c          IF ( k.LT.kSurfC(i,j,bi,bj) ) k = kSurfC(i,j,bi,bj)
c        ENDIF
C-    same as above, using min,max:
         kl = MIN( MAX(k,kSurfC(i,j,bi,bj)), MAX(kLowC(i,j,bi,bj),1) )
         tLoc(i,j) = tFld(i,j,kl,bi,bj)
         sLoc(i,j) = sFld(i,j,kl,bi,bj)
       ENDDO
      ENDDO

      CALL FIND_RHO_2D(
     I      1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I      tLoc, sLoc,
     O      rhoLoc,
     I      k, bi, bj, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
