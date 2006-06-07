C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_u_sidedrag.F,v 1.12 2006/06/07 01:55:15 heimbach Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_SIDEDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_SIDEDRAG(
     I        bi,bj,k,
     I        uFld, del2u, hFacZ,
     I        viscAh_Z,viscA4_Z,
     I        harmonic,biharmonic,useVariableViscosity,
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
#include "SURFACE.h"

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
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL harmonic,biharmonic,useVariableViscosity
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uDragTerms           :: drag term
      _RL uDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  hFacZClosedN         :: fractional open water to north
C  hFacZClosedS         :: fractional open water to south
      INTEGER i,j
      _RL hFacZClosedS,hFacZClosedN
CEOP
      _RL Ahtmp,A4tmp


      IF ( sideDragFactor.LE.0. ) THEN
C--   Laplacian and bi-harmonic terms: variable-Viscosity coeff. from
C     MOM_CALC_VISC are not used here (corresponds to the "old" version).

      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
#ifdef NONLIN_FRSURF
C-      this will not give any side-drag along thin wall.
C       (but this might just be what we want ...)
        hFacZClosedS = h0FacW(i,j,k,bi,bj) - h0FacW(i,j-1,k,bi,bj)
        hFacZClosedN = h0FacW(i,j,k,bi,bj) - h0FacW(i,j+1,k,bi,bj)
        hFacZClosedS = MAX( 0. _d 0, hFacZClosedS )
        hFacZClosedN = MAX( 0. _d 0, hFacZClosedN )
#else
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
#endif
        Ahtmp=min(viscAh+viscAhGrid*rAw(i,j,bi,bj)/deltaTmom,
     &            viscAhMax)
        A4tmp=min(viscA4+viscA4Grid*(rAw(i,j,bi,bj)**2)/deltaTmom,
     &            viscA4Max)
        A4tmp=min(A4tmp,viscA4GridMax*(rAw(i,j,bi,bj)**2)/deltaTmom)
        A4tmp=max(A4tmp,viscA4GridMin*(rAw(i,j,bi,bj)**2)/deltaTmom)
        uDragTerms(i,j) =
     &   -_recip_hFacW(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAw(i,j,bi,bj)
     &   *( hFacZClosedS*_dxV(i, j ,bi,bj)
     &      *_recip_dyU(i, j ,bi,bj)
     &     +hFacZClosedN*_dxV(i,j+1,bi,bj)
     &      *_recip_dyU(i,j+1,bi,bj) )
     &   *drF(k)*2.*(
#ifdef ISOTROPIC_COS_SCALING
     &                viscAh*uFld(i,j)*cosFacU(j,bi,bj)
#ifdef COSINEMETH_III
     &               -viscA4*del2u(i,j)*sqcosFacU(j,bi,bj)
#else
     &               -viscA4*del2u(i,j)*cosFacU(j,bi,bj)
#endif
#else
     &                Ahtmp*uFld(i,j)
     &               -A4tmp*del2u(i,j)
#endif
     &              )
       ENDDO
      ENDDO

      ELSE
C--   Laplacian and bi-harmonic terms: using variable-Viscosity coeff.
C     from MOM_CALC_VISC, consistent with dissipation in the interior

      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
C-      this will not give any side-drag along thin wall.
C       (but this might just be what we want ...)
#ifdef NONLIN_FRSURF
        hFacZClosedS = h0FacW(i,j,k,bi,bj) - h0FacW(i,j-1,k,bi,bj)
        hFacZClosedN = h0FacW(i,j,k,bi,bj) - h0FacW(i,j+1,k,bi,bj)
#else
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - _hFacW(i,j-1,k,bi,bj)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - _hFacW(i,j+1,k,bi,bj)
#endif
        hFacZClosedS = MAX( 0. _d 0, hFacZClosedS )
        hFacZClosedN = MAX( 0. _d 0, hFacZClosedN )
        uDragTerms(i,j) =
     &   -_recip_hFacW(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAw(i,j,bi,bj)
     &   *( hFacZClosedS*_dxV(i, j ,bi,bj)*_recip_dyU(i, j ,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
     &         *( viscAh_Z(i,j  )*uFld(i,j)*cosFacU(j,bi,bj)
#ifdef COSINEMETH_III
     &           -viscA4_Z(i,j  )*del2u(i,j)*sqcosFacU(j,bi,bj) )
#else
     &           -viscA4_Z(i,j  )*del2u(i,j)*cosFacU(j,bi,bj) )
#endif
#else
     &         *( viscAh_Z(i,j  )*uFld(i,j)
     &           -viscA4_Z(i,j  )*del2u(i,j) )
#endif
     &     +hFacZClosedN*_dxV(i,j+1,bi,bj)*_recip_dyU(i,j+1,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
     &         *( viscAh_Z(i,j+1)*uFld(i,j)*cosFacU(j,bi,bj)
#ifdef COSINEMETH_III
     &           -viscA4_Z(i,j+1)*del2u(i,j)*sqcosFacU(j,bi,bj) )
#else
     &           -viscA4_Z(i,j+1)*del2u(i,j)*cosFacU(j,bi,bj) )
#endif
#else
     &         *( viscAh_Z(i,j+1)*uFld(i,j)
     &           -viscA4_Z(i,j+1)*del2u(i,j) )
#endif
     &    )*drF(k)*sideDragFactor
       ENDDO
      ENDDO

C--   end old-version / new-version IF blocks
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL(uDragTerms,'USidDrag',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
