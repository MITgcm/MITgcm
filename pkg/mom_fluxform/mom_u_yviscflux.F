C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_yviscflux.F,v 1.7 2006/12/05 05:30:38 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_YVISCFLUX

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_YVISCFLUX(
     I        bi,bj,k,
     I        uFld, del2u, hFacZ,
     O        yViscFluxU,
     I        viscAh_Z,viscA4_Z,
     I        myThid )

C !DESCRIPTION:
C Calculates the area integrated meridional viscous fluxes of U:
C \begin{equation*}
C F^y = - \frac{ \Delta y_v \Delta r_f h_z }{\Delta y_u}
C  ( A_h \delta_j u - A_4 \delta_j \nabla^2 u )
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
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  yViscFluxU           :: viscous fluxes
      _RL yViscFluxU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
CEOP

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        yViscFluxU(i,j) =
     &    _dxV(i,j,bi,bj)*drF(k)*hFacZ(i,j)
     &     *(
     &       -viscAh_Z(i,j)*( uFld(i,j)-uFld(i,j-1) )
#ifdef ISOTROPIC_COS_SCALING
     &       *cosFacV(J,bi,bj)
#endif
     &       +viscA4_Z(i,j)*(del2u(i,j)-del2u(i,j-1))
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &       *sqCosFacV(J,bi,bj)
#else
     &       *cosFacV(J,bi,bj)
#endif
#endif
     &      )*_recip_dyU(i,j,bi,bj)
c    &       *deepFacC(k)        ! dxV scaling factor
c    &       *recip_deepFacC(k)  ! recip_dyU scaling factor
       ENDDO
      ENDDO

      RETURN
      END
