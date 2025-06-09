#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_METRIC_SPHERE

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_METRIC_SPHERE(
     I        bi, bj, k,
     I        uFld,
     O        vMetricTerms,
     I        myThid )

C !DESCRIPTION:
C Calculates the meridional metric term due to sphericity:
C \begin{equation}
C \frac{1}{a} \overline{u}^{ij} \overline{u}^{ij} \tan{\phi}
C \end{equation}

C !USES: ===============================================================`
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj               :: tile indices
C  k                    :: vertical level
C  uFld                 :: zonal velocity
C  myThid               :: my Thread Id number
      INTEGER bi, bj, k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vMetricTerms         :: metric term
      _RL vMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i, j                 :: loop indices
      INTEGER i, j
CEOP

      IF ( selectMetricTerms.EQ.1 ) THEN
C-    Use analytical expression for tan(Phi) (stored in: tanPhiAtV)
        DO j=1-OLx+1,sNy+OLy
         DO i=1-OLx,sNx+OLx-1
           vMetricTerms(i,j) = -recip_rSphere*recip_deepFacC(k)
     &      *0.25 _d 0*( uFld(i,j  ) + uFld(i+1,j  )
     &                 + uFld(i,j-1) + uFld(i+1,j-1)
     &                 )
     &      *0.25 _d 0*( uFld(i,j  ) + uFld(i+1,j  )
     &                 + uFld(i,j-1) + uFld(i+1,j-1)
     &                 )
     &      *_tanPhiAtV(i,j,bi,bj)
         ENDDO
        ENDDO
      ELSE
C-    Using grid-spacing gradient: estimates tan(Phi)/rSphere as -del^j(dxC)/rAz
        DO j=2-OLx,sNy+OLy
         DO i=1-OLx,sNx+OLx-1
           vMetricTerms(i,j) = +halfRL*
     &     ( ( uFld(i,j-1) + uFld(i,j) )*halfRL
     &      *( dxC( i,  j, bi,bj)
     &       - dxC( i, j-1,bi,bj) )*recip_rAz( i, j,bi,bj)
     &      *( uFld( i, j-1)*dyG( i, j-1,bi,bj)*hFacW( i, j-1,k,bi,bj)
     &       + uFld( i,  j )*dyG( i,  j, bi,bj)*hFacW( i,  j, k,bi,bj)
     &       )*halfRL
     &     + ( uFld(i+1,j-1) + uFld(i+1,j) )*halfRL
     &      *( dxC(i+1, j, bi,bj)
     &       - dxC(i+1,j-1,bi,bj) )*recip_rAz(i+1,j,bi,bj)
     &      *( uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacW(i+1,j-1,k,bi,bj)
     &       + uFld(i+1, j )*dyG(i+1, j, bi,bj)*hFacW(i+1, j, k,bi,bj)
     &       )*halfRL
     &     )*recip_hFacS(i,j,k,bi,bj)
     &      *recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
