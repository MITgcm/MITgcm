C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_u_sidedrag.F,v 1.13 2014/02/09 18:56:19 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_SIDEDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_SIDEDRAG(
     I        bi, bj, k,
     I        uFld, del2u, hFacZ,
     I        viscAh_Z, viscA4_Z,
     I        harmonic, biharmonic, useVariableViscosity,
     O        uDragTerms,
     I        myThid )

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
C  myThid               :: my Thread Id number
      INTEGER bi, bj, k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL harmonic, biharmonic, useVariableViscosity
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uDragTerms           :: drag term
      _RL uDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  hFacZClosedN         :: fractional open water to north
C  hFacZClosedS         :: fractional open water to south
      INTEGER i,j
      _RL hFacZClosedS, hFacZClosedN
      _RL Ahtmp, A4tmp
CEOP

      IF ( sideDragFactor.LE.0. ) THEN
C--   Laplacian and bi-harmonic terms: variable-Viscosity coeff. from
C     MOM_CALC_VISC are not used here (corresponds to the "old" version).

      DO j=2-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx-1
#ifdef NONLIN_FRSURF
        hFacZClosedS = h0FacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = h0FacW(i,j,k,bi,bj) - hFacZ(i,j+1)
#else
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
#endif
        Ahtmp=MIN(viscAh+viscAhGrid*rAw(i,j,bi,bj)/deltaTmom,
     &            viscAhMax)
        A4tmp=MIN(viscA4+viscA4Grid*(rAw(i,j,bi,bj)**2)/deltaTmom,
     &            viscA4Max)
        A4tmp=MIN(A4tmp,viscA4GridMax*(rAw(i,j,bi,bj)**2)/deltaTmom)
        A4tmp=MAX(A4tmp,viscA4GridMin*(rAw(i,j,bi,bj)**2)/deltaTmom)
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

      DO j=2-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx-1
#ifdef NONLIN_FRSURF
        hFacZClosedS = h0FacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = h0FacW(i,j,k,bi,bj) - hFacZ(i,j+1)
#else
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
#endif
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
