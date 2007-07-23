C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_salt_plume_depth.F,v 1.2 2007/07/23 00:17:44 dimitri Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_SALT_PLUME_DEPTH
C     !INTERFACE:
      SUBROUTINE CALC_SALT_PLUME_DEPTH(
     I                       rhoSurf, sigmaR,
     I                       bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_SALT_PLUME_DEPTH
C     | o Compute depth of penetration of salt plumes rejected
C     |   during sea ice growth
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     rhoSurf   :: Surface density anomaly
C     sigmaR    :: Vertical gradient of potential density
C     bi,bj     :: tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: my Thread Id number
      _RL     rhoSurf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     sigmaR (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j,k
      _RL     rhoBigNb, tmpFac, SaltPlumeCriterion
      _RL     rhoMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     rhoLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C     find surface where the potential density (ref.lev=surface) is
C     larger than surface density plus SaltPlumeCriterion.  Initially
C     use the default value 0.4 kg/m^3 of Duffy et al, 1999.
      SaltPlumeCriterion = 0.4

      rhoBigNb  = rhoConst*1. _d 10
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        rhoKm1(i,j) = rhoSurf(i,j)
        rhoMxL(i,j) = rhoSurf(i,j) + SaltPlumeCriterion
        SaltPlumeDepth(i,j,bi,bj) = rF(1)-R_low(I,J,bi,bj)
       ENDDO
      ENDDO
      DO k = 2,Nr
C-    potential density (reference level = surface level)
       CALL FIND_RHO(
     I      bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, K, 1,
     I      theta, salt,
     O      rhoLoc, myThid )
       
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         IF ( k.LE.klowC(i,j,bi,bj) .AND.
     &        rhoLoc(i,j).GE.rhoMxL(i,j) ) THEN
          IF ( rhoLoc(i,j).GT.rhoKm1(i,j) ) THEN
           tmpFac = ( rhoMxL(i,j) - rhoKm1(i,j) )
     &          / ( rhoLoc(i,j) - rhoKm1(i,j) )
          ELSE
           tmpFac = 0.
          ENDIF
          SaltPlumeDepth(i,j,bi,bj) = rF(1)-rC(k-1)+tmpFac*drC(k)
          rhoMxL(i,j) = rhoBigNb
         ELSE
          rhoKm1(i,j) = rhoLoc(i,j)
         ENDIF
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
