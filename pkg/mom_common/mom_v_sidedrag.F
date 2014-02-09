C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_v_sidedrag.F,v 1.13 2014/02/09 18:56:19 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_SIDEDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_SIDEDRAG(
     I        bi, bj, k,
     I        vFld, del2v, hFacZ,
     I        viscAh_Z,viscA4_Z,
     I        harmonic, biharmonic, useVariableViscosity,
     O        vDragTerms,
     I        myThid )

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
#include "SURFACE.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  vfld                 :: meridional flow
C  del2v                :: Laplacian of meridional flow
C  hFacZ                :: fractional open water at vorticity points
C  myThid               :: my Thread Id number
      INTEGER bi, bj, k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL harmonic, biharmonic, useVariableViscosity
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vDragTerms           :: drag term
      _RL vDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  hFacZClosedE         :: fractional open water to east
C  hFacZClosedW         :: fractional open water to west
      INTEGER i, j
      _RL hFacZClosedE, hFacZClosedW
      _RL Ahtmp,A4tmp
CEOP

      IF ( sideDragFactor.LE.0. ) THEN
C--   Laplacian and bi-harmonic terms: variable-Viscosity coeff. from
C     MOM_CALC_VISC are not used here (corresponds to the "old" version).

      DO j=2-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx-1
#ifdef NONLIN_FRSURF
        hFacZClosedW = h0FacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = h0FacS(i,j,k,bi,bj) - hFacZ(i+1,j)
#else
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
#endif
        Ahtmp=MIN(viscAh+viscAhGrid*rAs(i,j,bi,bj)/deltaTmom,
     &            viscAhMax)
        A4tmp=MIN(viscA4+viscA4Grid*(rAs(i,j,bi,bj)**2)/deltaTmom,
     &            viscA4Max)
        IF (viscA4GridMax.GT.0.) THEN
           A4tmp=MIN(A4tmp,viscA4GridMax*(rAs(i,j,bi,bj)**2)/deltaTmom)
        ENDIF
        A4tmp=MAX(A4tmp,viscA4GridMin*(rAs(i,j,bi,bj)**2)/deltaTmom)
        vDragTerms(i,j) =
     &   -_recip_hFacS(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAs(i,j,bi,bj)
     &   *( hFacZClosedW*_dyU( i ,j,bi,bj)
     &      *_recip_dxV( i ,j,bi,bj)
     &     +hFacZClosedE*_dyU(i+1,j,bi,bj)
     &      *_recip_dxV(i+1,j,bi,bj) )
     &   *drF(k)*2.*(
     &                Ahtmp*vFld(i,j)*cosFacV(j,bi,bj)
#ifdef COSINEMETH_III
     &               -viscA4*del2v(i,j)*sqcosFacV(j,bi,bj)
#else
     &               -A4tmp*del2v(i,j)*cosFacV(j,bi,bj)
#endif
     &              )
       ENDDO
      ENDDO

      ELSE
C--   Laplacian and bi-harmonic terms: using variable-Viscosity coeff.
C     from MOM_CALC_VISC, consistent with dissipation in the interior

      DO j=2-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx-1
#ifdef NONLIN_FRSURF
        hFacZClosedW = h0FacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = h0FacS(i,j,k,bi,bj) - hFacZ(i+1,j)
#else
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
#endif
        vDragTerms(i,j) =
     &   -_recip_hFacS(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAs(i,j,bi,bj)
     &   *( hFacZClosedW*_dyU( i ,j,bi,bj)*_recip_dxV( i ,j,bi,bj)
     &         *( viscAh_Z(i  ,j)*vFld(i,j)*cosFacV(j,bi,bj)
#ifdef COSINEMETH_III
     &           -viscA4_Z(i  ,j)*del2v(i,j)*sqcosFacV(j,bi,bj) )
#else
     &           -viscA4_Z(i  ,j)*del2v(i,j)*cosFacV(j,bi,bj) )
#endif
     &     +hFacZClosedE*_dyU(i+1,j,bi,bj)*_recip_dxV(i+1,j,bi,bj)
     &         *( viscAh_Z(i+1,j)*vFld(i,j)*cosFacV(j,bi,bj)
#ifdef COSINEMETH_III
     &           -viscA4_Z(i+1,j)*del2v(i,j)*sqcosFacV(j,bi,bj) )
#else
     &           -viscA4_Z(i+1,j)*del2v(i,j)*cosFacV(j,bi,bj) )
#endif
     &    )*drF(k)*sideDragFactor
       ENDDO
      ENDDO

C--   end old-version / new-version IF blocks
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL(vDragTerms,'VSidDrag',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
