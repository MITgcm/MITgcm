#include "LONGSTEP_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

CBOP
C     !ROUTINE:
C     !INTERFACE: LONGSTEP_RESIDUAL_FLOW
      SUBROUTINE LONGSTEP_RESIDUAL_FLOW(
     U                  uFld, vFld, wFld,
     I                  bi, bj, myIter, myThid )
C     !DESCRIPTION:
C     Add GM-bolus velocity to Eulerian velocity to get Residual Mean velocity.

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LONGSTEP.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uFld   :: zonal      velocity (updated)
C     vFld   :: meridional velocity (updated)
C     wFld   :: vertical volume transport (updated)
C     bi,bj  :: tile indices
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      INTEGER bi, bj, myIter, myThid
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k :: loop indices
      INTEGER i, j, k
      INTEGER kp1
      _RL maskp1, flipSign4LHCoord
      _RL delPsi
CEOP

C     Since bolus transport is computed as curl of stream-function, needs to
C     flip sign when using Left-Handed Coordinate system such as P-coordinate
      flipSign4LHCoord = -gravitySign

      IF ( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &     .AND. .NOT.GM_InMomAsStress ) THEN

       DO k=1,Nr
        kp1 = MIN(k+1,Nr)
        maskp1 = 1.
        IF (k.GE.Nr) maskp1 = 0.

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           delPsi = LS_PsiX(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &            - LS_PsiX(i,j, k, bi,bj)*deepFacF(k)
           uFld(i,j,k) = uFld(i,j,k)
     &                 + delPsi*recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
     &                         *recip_deepFacC(k)*flipSign4LHCoord
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           delPsi = LS_PsiY(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &            - LS_PsiY(i,j, k, bi,bj)*deepFacF(k)
           vFld(i,j,k) = vFld(i,j,k)
     &                 + delPsi*recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
     &                         *recip_deepFacC(k)*flipSign4LHCoord
         ENDDO
        ENDDO
C       deep-model: simplify FacF / Fac2F (from dx,y / rA) to: 1/FacF
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
           delPsi = ( dyG(i+1,j,bi,bj)*LS_PsiX(i+1,j,k,bi,bj)
     &               -dyG( i ,j,bi,bj)*LS_PsiX( i ,j,k,bi,bj)
     &               +dxG(i,j+1,bi,bj)*LS_PsiY(i,j+1,k,bi,bj)
     &               -dxG(i, j ,bi,bj)*LS_PsiY(i, j ,k,bi,bj)
     &              )
           wFld(i,j,k) = wFld(i,j,k)
     &                 + delPsi*recip_rA(i,j,bi,bj)
     &                         *recip_deepFacF(k)*flipSign4LHCoord
         ENDDO
        ENDDO
       ENDDO

      ENDIF
#endif /* ALLOW_GMREDI */

      RETURN
      END
