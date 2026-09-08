#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INTEGRATE_FOR_W
C     !INTERFACE:
      SUBROUTINE INTEGRATE_FOR_W(
     I                     bi, bj, hConv3d, rStarDhDt,
     O                     wFld,
     I                     myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INTEGRATE_FOR_W
C     | o Integrate for vertical velocity.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     hConv3d    :: horizontal transport convergence [wVel units]
C     rStarDhDt  :: relative time derivative of column thickness = d.eta/dt / H
C     wFld       :: Vertical velocity
C     myIter     :: Current iteration number in simulation
C     myThid     :: my Thread Id. number
      INTEGER bi, bj
      _RL hConv3d(1:sNx,1:sNy,Nr)
#if (defined NONLIN_FRSURF) && !(defined DISABLE_RSTAR_CODE)
      _RL rStarDhDt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#else
      _RL rStarDhDt(1)
#endif
      _RL  wFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER i, j, k
      _RL wFacKp, rFacKm
CEOP

      DO k=Nr,1,-1
C--   Integrate continuity vertically for vertical velocity

C--   Calculate vertical "volume transport" through face k
C     between tracer cell k-1 & k
       wFacKp = deepFac2F(k+1)*rhoFacF(k+1)
       rFacKm = recip_deepFac2F(k)*recip_rhoFacF(k)
       IF (rigidLid) THEN
C-  o Rigid-Lid case: zero at lower and upper boundaries
        IF (k.EQ.1) THEN
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) = 0.
           ENDDO
          ENDDO
        ELSEIF (k.EQ.Nr) THEN
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) =
     &          hConv3d(i,j,k)
     &         *maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
     &         *rFacKm
           ENDDO
          ENDDO
        ELSE
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) =
     &        ( wFld(i,j,k+1,bi,bj)*wFacKp
     &         +hConv3d(i,j,k)
     &        )*maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
     &         *rFacKm
           ENDDO
          ENDDO
        ENDIF
#ifdef NONLIN_FRSURF
# ifndef DISABLE_RSTAR_CODE
       ELSEIF ( select_rStar.NE.0 ) THEN
C-  o rStar case: zero under-ground and at r_lower boundary
C     can be non-zero at surface (useRealFreshWaterFlux).
        IF (k.EQ.Nr) THEN
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) =
     &        ( hConv3d(i,j,k)
     &         -rStarDhDt(i,j)*drF(k)*h0FacC(i,j,k,bi,bj)
     &        )*maskC(i,j,k,bi,bj)
     &         *rFacKm
           ENDDO
          ENDDO
        ELSE
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) =
     &        ( wFld(i,j,k+1,bi,bj)*wFacKp
     &         +hConv3d(i,j,k)
     &         -rStarDhDt(i,j)*drF(k)*h0FacC(i,j,k,bi,bj)
     &        )*maskC(i,j,k,bi,bj)
     &         *rFacKm
           ENDDO
          ENDDO
        ENDIF
# endif /* DISABLE_RSTAR_CODE */
# ifndef DISABLE_SIGMA_CODE
       ELSEIF ( selectSigmaCoord.NE.0 ) THEN
C-  o Hybrid Sigma coordinate:
        IF (k.EQ.Nr) THEN
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) =
     &        ( hConv3d(i,j,k)
     &         -dEtaHdt(i,j,bi,bj)*dBHybSigF(k)
     &        )*maskC(i,j,k,bi,bj)
           ENDDO
          ENDDO
        ELSE
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) =
     &        ( wFld(i,j,k+1,bi,bj)
     &         +hConv3d(i,j,k)
     &         -dEtaHdt(i,j,bi,bj)*dBHybSigF(k)
     &        )*maskC(i,j,k,bi,bj)
           ENDDO
          ENDDO
        ENDIF
# endif /* DISABLE_SIGMA_CODE */
#endif /* NONLIN_FRSURF */
       ELSE
C-  o Free Surface case (r-Coordinate):
C      non zero at surface ; zero under-ground and at r_lower boundary
        IF (k.EQ.Nr) THEN
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) =
     &          hConv3d(i,j,k)
     &         *maskC(i,j,k,bi,bj)
     &         *rFacKm
           ENDDO
          ENDDO
        ELSE
          DO j=1,sNy
           DO i=1,sNx
             wFld(i,j,k,bi,bj) =
     &        ( wFld(i,j,k+1,bi,bj)*wFacKp
     &         +hConv3d(i,j,k)
     &        )*maskC(i,j,k,bi,bj)
     &         *rFacKm
           ENDDO
          ENDDO
        ENDIF
C-  endif - rigid-lid / Free-Surf.
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( k.EQ.Nr .AND. myIter.NE.0 .AND. usingPCoords
     &      .AND. fluidIsWater .AND. useRealFreshWaterFlux ) THEN
         DO j=1,sNy
          DO i=1,sNx
            wFld(i,j,k,bi,bj) = wFld(i,j,k,bi,bj)
     &               +mass2rUnit*PmEpR(i,j,bi,bj)*maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ENDIF

C-    End DO k=Nr,1,-1
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
