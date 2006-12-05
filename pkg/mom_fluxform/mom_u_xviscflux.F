C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_xviscflux.F,v 1.7 2006/12/05 05:30:38 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_XVISCFLUX

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_XVISCFLUX(
     I        bi,bj,k,
     I        uFld, del2u,
     O        xViscFluxU,
     I        viscAh_D,viscA4_D,
     I        myThid )

C !DESCRIPTION:
C Calculates the area integrated zonal viscous fluxes of U:
C \begin{equation*}
C F^x = - \frac{ \Delta y_f \Delta r_f h_c }{\Delta x_f}
C  ( A_h \delta_i u - A_4 \delta_i \nabla^2 u )
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
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  xViscFluxU           :: viscous fluxes
      _RL xViscFluxU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
CEOP

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        xViscFluxU(i,j) =
     &    _dyF(i,j,bi,bj)*drF(k)*_hFacC(i,j,k,bi,bj)
     &     *(
     &       -viscAh_D(i,j)*( uFld(i+1,j)-uFld(i,j) )
     &       *cosFacU(J,bi,bj)
     &       +viscA4_D(i,j)*(del2u(i+1,j)-del2u(i,j))
#ifdef COSINEMETH_III
     &       *sqCosFacU(J,bi,bj)
#else
     &       *cosFacU(J,bi,bj)
#endif
     &      )*_recip_dxF(i,j,bi,bj)
c    &       *deepFacC(k)        ! dyF scaling factor
c    &       *recip_deepFacC(k)  ! recip_dxF scaling factor
       ENDDO
      ENDDO

      RETURN
      END
