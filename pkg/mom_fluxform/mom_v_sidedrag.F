C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_v_sidedrag.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_SIDEDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_SIDEDRAG(
     I        bi,bj,k,
     I        vFld, del2v, hFacZ,
     O        vDragTerms,
     I        myThid)

C !DESCRIPTION:
C Calculates the drag terms due to the no-slip condition on viscous stresses:
C \begin{equation*}
C G^v_{drag} = - \frac{2}{\Delta x_v} (A_h v - A_4 \nabla^2 v)
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uvld                 :: meridional flow
C  del2v                :: Laplacian of meridional flow
C  hFacZ                :: fractional open water at vorticity points
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vDragTerms           :: drag term
      _RL vDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  hFacZClosedE         :: fractional open water to east
C  hFacZClosedW         :: fractional open water to west
      INTEGER I,J
      _RS hFacZClosedE,hFacZClosedW
CEOP

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
        vDragTerms(i,j) =
     &   -_recip_hFacS(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAs(i,j,bi,bj)
     &   *( hFacZClosedW*_dyU( i ,j,bi,bj)
     &      *_recip_dxV( i ,j,bi,bj)
     &     +hFacZClosedE*_dyU(i+1,j,bi,bj)
     &      *_recip_dxV(i+1,j,bi,bj) )
     &   *drF(k)*2.*(
     &                viscAh*vFld(i,j)*cosFacV(J,bi,bj)
     &               -viscA4*del2v(i,j)*cosFacV(J,bi,bj)
#ifdef COSINEMETH_III
     &               -viscA4*del2v(i,j)*sqcosFacV(J,bi,bj)
#else
     &               -viscA4*del2v(i,j)*cosFacV(J,bi,bj)
#endif
     &              )
       ENDDO
      ENDDO

      RETURN
      END
