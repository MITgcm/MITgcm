#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_RESIDUAL_FLOW
C     !INTERFACE:
      SUBROUTINE GMREDI_RESIDUAL_FLOW(
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
#include "GMREDI.h"
#ifdef ALLOW_EDDYPSI
# include "DYNVARS.h"
# include "FFIELDS.h"
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
#ifdef GM_BOLUS_ADVEC

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k :: loop indices
      INTEGER i, j, k
      INTEGER kp1
      _RL maskp1
      _RL delPsi
#ifdef ALLOW_EDDYPSI
      _RL ustar, vstar
#endif
CEOP

      IF ( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &     .AND. .NOT.GM_InMomAsStress ) THEN

       DO k=1,Nr
        kp1 = MIN(k+1,Nr)
        maskp1 = 1.
        IF (k.GE.Nr) maskp1 = 0.

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           delPsi = GM_PsiX(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &            - GM_PsiX(i,j, k, bi,bj)*deepFacF(k)
           uFld(i,j,k) = uFld(i,j,k)
     &                 + delPsi*recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
     &                         *recip_deepFacC(k)
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           delPsi = GM_PsiY(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &            - GM_PsiY(i,j, k, bi,bj)*deepFacF(k)
           vFld(i,j,k) = vFld(i,j,k)
     &                 + delPsi*recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
     &                         *recip_deepFacC(k)
         ENDDO
        ENDDO
C       deep-model: simplify FacF / Fac2F (from dx,y / rA) to: 1/FacF
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
           delPsi = ( dyG(i+1,j,bi,bj)*GM_PsiX(i+1,j,k,bi,bj)
     &               -dyG( i ,j,bi,bj)*GM_PsiX( i ,j,k,bi,bj)
     &               +dxG(i,j+1,bi,bj)*GM_PsiY(i,j+1,k,bi,bj)
     &               -dxG(i, j ,bi,bj)*GM_PsiY(i, j ,k,bi,bj)
     &              )*maskC(i,j,k,bi,bj)
           wFld(i,j,k) = wFld(i,j,k) + delPsi*recip_rA(i,j,bi,bj)
     &                                       *recip_deepFacF(k)
         ENDDO
        ENDDO

       ENDDO

#ifdef ALLOW_EDDYPSI
      ELSEIF( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &        .AND. GM_InMomAsStress ) THEN

C     Calculate the mean velocity from the residual and bolus
       DO k=1,Nr
        kp1 = MIN(k+1,Nr)
        maskp1 = 1.
        IF (k.GE.Nr) maskp1 = 0.

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          delPsi = GM_PsiX(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &           - GM_PsiX(i,j, k, bi,bj)*deepFacF(k)
          ustar = delPsi*recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
     &                  *recip_deepFacC(k)
          uEulerMean(i,j,k,bi,bj) = uVel(i,j,k,bi,bj) - ustar
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          delPsi = GM_PsiY(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &           - GM_PsiY(i,j, k, bi,bj)*deepFacF(k)
          vstar  = delPsi*recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
     &                   *recip_deepFacC(k)
          vEulerMean(i,j,k,bi,bj) = vVel(i,j,k,bi,bj) - vstar
         ENDDO
        ENDDO

       ENDDO

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(uEulerMean,'U_EulerM',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(vEulerMean,'V_EulerM',0,Nr,1,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_EDDYPSI */

      ENDIF
#endif /* GM_BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
