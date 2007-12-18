C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_calc_depth.F,v 1.4 2007/12/18 05:34:41 dimitri Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_CALC_DEPTH
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_CALC_DEPTH(
     I                       rhoSurf, sigmaR,
     I                       bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SALT_PLUME_CALC_DEPTH
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
#include "SALT_PLUME.h"

C     !INPUT/OUTPUT PARAMETERS:
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
CEOP

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
C     i,j :: Loop counters
      INTEGER i,j,k
      _RL     rhoBigNb, tmpFac
      _RL     rhoMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     rhoLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     GG, GGm1
      _RL     SPIND (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        SaltPlumeDepth(i,j,bi,bj) = rF(1)-R_low(I,J,bi,bj)
        SPIND(i,j)=0.
       ENDDO
      ENDDO

      IF (CriterionType.EQ.1) THEN

       rhoBigNb  = rhoConst*1. _d 10
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         rhoKm1(i,j) = rhoSurf(i,j)
         rhoMxL(i,j) = rhoSurf(i,j) + SaltPlumeCriterion
        ENDDO
       ENDDO
       DO k = 2,Nr
C-     potential density (reference level = surface level)
        CALL FIND_RHO(
     I         bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, K, 1,
     I         theta, salt,
     O         rhoLoc, myThid )

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          IF ( k.LE.klowC(i,j,bi,bj) .AND.
     &         rhoLoc(i,j).GE.rhoMxL(i,j) ) THEN
           IF ( rhoLoc(i,j).GT.rhoKm1(i,j) ) THEN
              tmpFac = ( rhoMxL(i,j) - rhoKm1(i,j) )
     &             / ( rhoLoc(i,j) - rhoKm1(i,j) )
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

      ELSEIF ( CriterionType.EQ.2 ) THEN 

        DO k=2,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           GG  =-1.0*sigmaR(i,j,k)
           GGm1=-1.0*sigmaR(i,j,k-1)
           IF ( k.LE.klowC(i,j,bi,bj) .AND.
     &          GG.GE.SaltPlumeCriterion ) THEN
            IF (GGm1.LE.SaltPlumeCriterion) THEN
             tmpFac = (SaltPlumeCriterion - GGm1)
     &              / (GG                 - GGm1)
             IF(SPIND(i,j) .LT. 0.5) THEN
               SaltPlumeDepth(i,j,bi,bj) = rF(1)-rC(k-1)+tmpFac*drC(k)
               SPIND(i,j)=1.
             ENDIF
            ELSE
             tmpFac = 0.
            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          SaltPlumeDepth(i,j,bi,bj) =
     &       min( SaltPlumeDepth(i,j,bi,bj)*SPovershoot,
     &            rF(1)-R_low(i,j,bi,bj) )
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics )
     &      CALL SALT_PLUME_DIAGNOSTICS_FILL(bi,bj,myThid)
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
