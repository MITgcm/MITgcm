C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_v_sidedrag.F,v 1.7 2005/09/29 02:31:55 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"
#define SIDEDRAG_OLD_VERSION

CBOP
C !ROUTINE: MOM_V_SIDEDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_SIDEDRAG(
     I        bi,bj,k,
     I        vFld, del2v, hFacZ,
     I        viscAh_Z,viscA4_Z,
     I        harmonic,biharmonic,useVariableViscosity,
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
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL harmonic,biharmonic,useVariableViscosity
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

#ifdef SIDEDRAG_OLD_VERSION
      _RL Ahtmp,A4tmp

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
        Ahtmp=min(viscAh+viscAhGrid*rAs(i,j,bi,bj)/deltaTmom,
     &            viscAhMax)
        A4tmp=min(viscA4+viscA4Grid*(rAs(i,j,bi,bj)**2)/deltaTmom,
     &            viscA4Max)
        IF (viscA4GridMax.GT.0.) THEN
           A4tmp=min(A4tmp,viscA4GridMax*(rAs(i,j,bi,bj)**2)/deltaTmom)
        ENDIF
        A4tmp=max(A4tmp,viscA4GridMin*(rAs(i,j,bi,bj)**2)/deltaTmom)
        vDragTerms(i,j) =
     &   -_recip_hFacS(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAs(i,j,bi,bj)
     &   *( hFacZClosedW*_dyU( i ,j,bi,bj)
     &      *_recip_dxV( i ,j,bi,bj)
     &     +hFacZClosedE*_dyU(i+1,j,bi,bj)
     &      *_recip_dxV(i+1,j,bi,bj) )
     &   *drF(k)*2.*(
     &                Ahtmp*vFld(i,j)*cosFacV(J,bi,bj)
     &               -A4tmp*del2v(i,j)*cosFacV(J,bi,bj)
#ifdef COSINEMETH_III
     &               -viscA4*del2v(i,j)*sqcosFacV(J,bi,bj)
#else
     &               -viscA4*del2v(i,j)*cosFacV(J,bi,bj)
#endif
     &              )
       ENDDO
      ENDDO

#else /* SIDEDRAG_OLD_VERSION */

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
        vDragTerms(i,j) =
     &   -_recip_hFacS(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAs(i,j,bi,bj)
     &   *( hFacZClosedW*_dyU( i ,j,bi,bj)*_recip_dxV( i ,j,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
     &         *(viscAh_Z(i  ,j)*vFld(i,j)*cosFacV(J,bi,bj)
#ifdef COSINEMETH_III
     &           -viscA4_Z(i  ,j)*del2v(i,j)*sqcosFacV(J,bi,bj))
#else 
     &           -viscA4_Z(i  ,j)*del2v(i,j)*cosFacV(J,bi,bj))
#endif
#else
     &         *(viscAh_Z(i  ,j)*vFld(i,j)
     &           -viscA4_Z(i  ,j)*del2v(i,j))
#endif
     &     +hFacZClosedE*_dyU(i+1,j,bi,bj)*_recip_dxV(i+1,j,bi,bj) 
#ifdef ISOTROPIC_COS_SCALING
     &         *(viscAh_Z(i+1,j)*vFld(i,j)*cosFacV(J,bi,bj)
#ifdef COSINEMETH_III
     &           -viscA4_Z(i+1,j)*del2v(i,j)*sqcosFacV(J,bi,bj))
#else
     &           -viscA4_Z(i+1,j)*del2v(i,j)*cosFacV(J,bi,bj))
#endif
#else
     &         *(viscAh_Z(i+1,j)*vFld(i,j)
     &           -viscA4_Z(i+1,j)*del2v(i,j))
#endif
     &              )
     &   *drF(k)*2.
       ENDDO
      ENDDO

#endif /* (not) SIDEDRAG_OLD_VERSION */

      RETURN
      END
