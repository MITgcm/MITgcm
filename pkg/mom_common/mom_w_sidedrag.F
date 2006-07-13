C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_w_sidedrag.F,v 1.1 2006/07/13 21:30:12 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_W_SIDEDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_W_SIDEDRAG(
     I        bi,bj,k,
     I        wFld, del2w, 
     I        rThickC, recip_rThickC,
     I        viscAh_W, viscA4_W,
     O        gwSideDrag,
     I        myThid)

C !DESCRIPTION:
C Calculates the drag terms due to the no-slip condition on viscous stresses:
C \begin{equation*}
C G^w_{drag} = - \frac{2}{\Delta x_w} (A_h w - A_4 \nabla^2 w)
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
C  wFld                 :: vertical velocity
C  del2w                :: Laplacian of vertical velocity
C  rThickC              :: thickness of W-cell
C  recip_rThickC        :: reciprol of W-cell thickness
C  viscAh_W             :: harmonic   horizontal viscosity (at W-Cell center)
C  viscA4_W             :: biharmonic horizontal viscosity (at W-Cell center)
C  myThid               :: my Thread Id number
      INTEGER bi,bj,k
      _RL  wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  del2w   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rThickC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_rThickC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  viscAh_W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  viscA4_W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  gwSideDrag           :: side drag tendency
      _RL gwSideDrag(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_NONHYDROSTATIC
C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  heightStepW          :: height of topographic step to West
C  heightStepE          :: height of topographic step to East
C  heightStepS          :: height of topographic step to South
C  heightStepN          :: height of topographic step to North
      INTEGER i,j
      _RL heightStepW, heightStepE
      _RL heightStepS, heightStepN
CEOP

C--   Laplacian and bi-harmonic terms: using variable-Viscosity coeff.
C     from MOM_CALC_VISC, consistent with dissipation in the interior

      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
C-      this will not give any side-drag along thin wall.
C       (but this might just be what we want ...)
        heightStepW = MAX( 0. _d 0, rThickC(i,j) - rThickC(i-1,j) )
        heightStepE = MAX( 0. _d 0, rThickC(i,j) - rThickC(i+1,j) )
        heightStepS = MAX( 0. _d 0, rThickC(i,j) - rThickC(i,j-1) )
        heightStepN = MAX( 0. _d 0, rThickC(i,j) - rThickC(i,j+1) )
C- jmc: take directly viscAh_W & viscA4_W (no bar_i, bar_j) since
C       they are already at the center (and not above u,v point).
        gwSideDrag(i,j) = 
     &   -sideDragFactor
     &    *( heightStepW*_dyG( i ,j,bi,bj)*_recip_dxC( i ,j,bi,bj)
     &         *( viscAh_W(i,j,k,bi,bj)*wFld(i,j,k,bi,bj)
     &                                 *cosFacV(j,bi,bj)
     &           -viscA4_W(i,j,k,bi,bj)*del2w(i,j)
#ifdef COSINEMETH_III
     &                                 *sqCosFacU(j,bi,bj)
#else
     &                                 *cosFacU(j,bi,bj)
#endif
     &          )
     &     +heightStepE*_dyG(i+1,j,bi,bj)*_recip_dxC(i+1,j,bi,bj)
     &         *( viscAh_W(i,j,k,bi,bj)*wFld(i,j,k,bi,bj)
     &                                 *cosFacV(j,bi,bj)
     &           -viscA4_W(i,j,k,bi,bj)*del2w(i,j)
#ifdef COSINEMETH_III
     &                                 *sqCosFacU(j,bi,bj)
#else
     &                                 *cosFacU(j,bi,bj)
#endif
     &          )
     &     +heightStepS*_dxG(i,j,bi,bj)*_recip_dyC(i,j,bi,bj)
     &         *( viscAh_W(i,j,k,bi,bj)*wFld(i,j,k,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
     &                                 *cosFacV(j,bi,bj)
#endif
     &           -viscA4_W(i,j,k,bi,bj)*del2w(i,j)
#ifdef ISOTROPIC_COS_SCALING
# ifdef COSINEMETH_III
     &                                 *sqCosFacV(j,bi,bj)
# else
     &                                 *cosFacV(j,bi,bj)
# endif
#endif
     &          )
     &     +heightStepN*_dxG(i,j+1,bi,bj)*_recip_dyC(i,j+1,bi,bj)
     &         *( viscAh_W(i,j,k,bi,bj)*wFld(i,j,k,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
     &                                 *cosFacV(j+1,bi,bj)
#endif
     &           -viscA4_W(i,j,k,bi,bj)*del2w(i,j)
#ifdef ISOTROPIC_COS_SCALING
# ifdef COSINEMETH_III
     &                                 *sqCosFacV(j+1,bi,bj)
# else
     &                                 *cosFacV(j+1,bi,bj)
# endif
#endif
     &          )
     &    )*recip_rThickC(i,j)*recip_rA(i,j,bi,bj)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL(gwSideDrag,'WSidDrag',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
