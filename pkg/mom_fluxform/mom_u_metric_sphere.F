#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_METRIC_SPHERE

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_METRIC_SPHERE(
     I        bi, bj, k,
     I        uFld, vFld,
     O        uMetricTerms,
     I        myThid )

C !DESCRIPTION:
C Calculates the zonal metric term due to sphericity:
C \begin{equation}
C \frac{u}{a} \overline{v}^{ij} \tan{\phi}
C \end{equation}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj               :: tile indices
C  k                    :: vertical level
C  uFld                 :: zonal velocity
C  vFld                 :: meridional velocity
C  myThid               :: my Thread Id number
      INTEGER bi, bj, k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uMetricTerms         :: metric term
      _RL uMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i, j                 :: loop indices
      INTEGER i, j
CEOP

      IF ( selectMetricTerms.EQ.1 ) THEN
C-    Using analytical expression for tan(Phi) (stored in: tanPhiAtU)
        DO j=1-OLx,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx
           uMetricTerms(i,j) =
     &      uFld(i,j)*recip_rSphere*recip_deepFacC(k)*0.25 _d 0
     &          *( vFld(i,j  ) + vFld(i-1,j  )
     &           + vFld(i,j+1) + vFld(i-1,j+1)
     &           )*_tanPhiAtU(i,j,bi,bj)
         ENDDO
        ENDDO
      ELSE
C-    Using grid-spacing gradient: estimates tan(Phi)/rSphere as -del^j(dxC)/rAz
        DO j=2-OLx,sNy+OLy-1
         DO i=2-OLx,sNx+OLx
           uMetricTerms(i,j) = -halfRL*
     &     ( ( uFld(i,j-1) + uFld(i,j) )*halfRL
     &      *( dxC(i,j,bi,bj)-dxC(i,j-1,bi,bj) )*recip_rAz(i, j, bi,bj)
     &      *( vFld(i-1, j )*dxG(i-1, j, bi,bj)*hFacS(i-1, j, k,bi,bj)
     &       + vFld( i,  j )*dxG( i,  j, bi,bj)*hFacS( i,  j, k,bi,bj)
     &       )*halfRL
     &     + ( uFld(i,j) + uFld(i,j+1) )*halfRL
     &      *( dxC(i,j+1,bi,bj)-dxC(i,j,bi,bj) )*recip_rAz(i,j+1,bi,bj)
     &      *( vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*hFacS(i-1,j+1,k,bi,bj)
     &       + vFld( i, j+1)*dxG( i, j+1,bi,bj)*hFacS( i, j+1,k,bi,bj)
     &       )*halfRL
     &     )*recip_hFacW(i,j,k,bi,bj)
     &      *recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
