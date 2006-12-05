C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_xviscflux.F,v 1.7 2006/12/05 05:30:38 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_XVISCFLUX

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_XVISCFLUX(
     I        bi,bj,k,
     I        vFld, del2v, hFacZ,
     O        xViscFluxV,
     I        viscAh_Z,viscA4_Z,
     I        myThid )

C !DESCRIPTION:
C Calculates the area integrated zonal viscous fluxes of V:
C \begin{equation*}
C F^x = - \frac{ \Delta x_u \Delta r_f h_z }{\Delta x_v}
C  ( A_h \delta_i v - A_4 \delta_i \nabla^2 v )
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
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  xViscFluxU           :: viscous fluxes
      _RL xViscFluxV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
CEOP

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        xViscFluxV(i,j) =
     &    _dyU(i,j,bi,bj)*drF(k)*hFacZ(i,j)
     &     *(
     &       -viscAh_Z(i,j)*( vFld(i,j)-vFld(i-1,j) )
     &       *cosFacV(J,bi,bj)
     &       +viscA4_Z(i,j)*(del2v(i,j)-del2v(i-1,j))
#ifdef COSINEMETH_III
     &       *sqCosFacV(J,bi,bj)
#else
     &       *cosFacV(J,bi,bj)
#endif
     &      )*_recip_dxV(i,j,bi,bj)
c    &       *deepFacC(k)        ! dyU scaling factor
c    &       *recip_deepFacC(k)  ! recip_dxV scaling factor
       ENDDO
      ENDDO

      RETURN
      END
