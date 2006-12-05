C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_yviscflux.F,v 1.7 2006/12/05 05:30:38 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_YVISCFLUX

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_YVISCFLUX(
     I        bi,bj,k,
     I        vFld, del2v,
     O        yViscFluxV,
     I        viscAh_D,viscA4_D,
     I        myThid )

C !DESCRIPTION:
C Calculates the area integrated meridional viscous fluxes of V:
C \begin{equation*}
C F^y = - \frac{ \Delta x_f \Delta r_f h_c }{\Delta y_f}
C  ( A_h \delta_j v - A_4 \delta_j \nabla^2 v )
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
C  vFld                 :: meridional flow
C  del2v                :: Laplacian of meridional flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  yViscFluxV           :: viscous fluxes
      _RL yViscFluxV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
CEOP

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        yViscFluxV(i,j) =
     &    _dxF(i,j,bi,bj)*drF(k)*_hFacC(i,j,k,bi,bj)
     &     *(
     &       -viscAh_D(i,j)*( vFld(i,j+1)-vFld(i,j) )
#ifdef ISOTROPIC_COS_SCALING
     &       *cosFacU(J,bi,bj)
#endif
     &       +viscA4_D(i,j)*(del2v(i,j+1)-del2v(i,j))
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &       *sqCosFacU(J,bi,bj)
#else
     &       *cosFacU(J,bi,bj)
#endif
#endif
     &      )*_recip_dyF(i,j,bi,bj)
c    &       *deepFacC(k)        ! dxF scaling factor
c    &       *recip_deepFacC(k)  ! recip_dyF scaling factor

       ENDDO
      ENDDO

      RETURN
      END
