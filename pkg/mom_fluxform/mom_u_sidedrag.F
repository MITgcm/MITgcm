C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_u_sidedrag.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_SIDEDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_SIDEDRAG(
     I        bi,bj,k,
     I        uFld, del2u, hFacZ,
     O        uDragTerms,
     I        myThid)

C !DESCRIPTION:
C Calculates the drag terms due to the no-slip condition on viscous stresses:
C \begin{equation*}
C G^u_{drag} = - \frac{2}{\Delta y_u} (A_h u - A_4 \nabla^2 u)
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
C  uFld                 :: zonal flow
C  del2u                :: Laplacian of zonal flow
C  hFacZ                :: fractional open water at vorticity points
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uDragTerms           :: drag term
      _RL uDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  hFacZClosedN         :: fractional open water to north
C  hFacZClosedS         :: fractional open water to south
      INTEGER I,J
      _RS hFacZClosedS,hFacZClosedN
CEOP

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
        uDragTerms(i,j) =
     &   -_recip_hFacW(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAw(i,j,bi,bj)
     &   *( hFacZClosedS*_dxV(i, j ,bi,bj)
     &      *_recip_dyU(i, j ,bi,bj)
     &     +hFacZClosedN*_dxV(i,j+1,bi,bj)
     &      *_recip_dyU(i,j+1,bi,bj) )
     &   *drF(k)*2.*(
#ifdef ISOTROPIC_COS_SCALING
     &                viscAh*uFld(i,j)*cosFacU(J,bi,bj)
#ifdef COSINEMETH_III
     &               -viscA4*del2u(i,j)*sqcosFacU(J,bi,bj)
#else
     &               -viscA4*del2u(i,j)*cosFacU(J,bi,bj)
#endif
#else
     &                viscAh*uFld(i,j)
     &               -viscA4*del2u(i,j)
#endif
     &              )
       ENDDO
      ENDDO

      RETURN
      END
