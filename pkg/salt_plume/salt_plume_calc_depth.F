#include "SALT_PLUME_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

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
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey, kkey
#endif
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

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ikey = (act1 + 1) + act2*max1
     &                  + act3*max1*max2
     &                  + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C Initializing the saltplume depth to bottom topography
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        SaltPlumeDepth(i,j,bi,bj) = rF(1)-R_low(I,J,bi,bj)
        SPIND(i,j)  = 0. _d 0
        rhoKm1(i,j) = 0. _d 0
        rhoMxL(i,j) = 0. _d 0
       ENDDO
      ENDDO

C CriterionType 1 = use delta_rho to determine salt plume depth
      IF (CriterionType.EQ.1) THEN

       rhoBigNb  = rhoConst*1. _d 10
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         SaltPlumeDepth(i,j,bi,bj) = rF(1)-R_low(I,J,bi,bj)
         rhoKm1(i,j) = rhoSurf(i,j)
         rhoMxL(i,j) = rhoSurf(i,j) + SaltPlumeCriterion
        ENDDO
       ENDDO

       DO k = 2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = (ikey-1)*Nr + k
CADJ STORE rhoKm1(:,:) = comlev1_bibj_k,
CADJ &     key=kkey, byte=isbyte, kind = isbyte
CADJ STORE rhoMxL(:,:) = comlev1_bibj_k,
CADJ &     key=kkey, byte=isbyte, kind = isbyte
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj_k,
CADJ &     key=kkey, byte=isbyte, kind = isbyte
CADJ STORE salt(:,:,k,bi,bj) = comlev1_bibj_k,
CADJ &     key=kkey, byte=isbyte, kind = isbyte
#endif
C-     potential density (reference level = surface level)
        CALL FIND_RHO_2D(
     I       1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I       theta(1-OLx,1-OLy,K,bi,bj), salt(1-OLx,1-OLy,K,bi,bj),
     O       rhoLoc,
     I       K, bi, bj, myThid )

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

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          SaltPlumeDepth(i,j,bi,bj) = rF(1)-R_low(I,J,bi,bj)
          SPIND(i,j)  = 0. _d 0
         ENDDO
        ENDDO

        DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = (ikey-1)*Nr + k
CADJ STORE SPIND(:,:) = comlev1_bibj_k,
CADJ &     key=kkey, byte=isbyte, kind = isbyte
#endif
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

C Make sure that the deepest SaltPlumeDepth is bottom topography:
      DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          SaltPlumeDepth(i,j,bi,bj) =
     &       min( SaltPlumeDepth(i,j,bi,bj),
     &            rF(1)-R_low(i,j,bi,bj) )
         ENDDO
      ENDDO

C#ifdef ALLOW_DIAGNOSTICS
C      IF ( useDiagnostics )
C     &      CALL SALT_PLUME_DIAGNOSTICS_FILL(bi,bj,myThid)
C#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
