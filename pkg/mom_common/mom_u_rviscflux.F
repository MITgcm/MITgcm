C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_u_rviscflux.F,v 1.2 2005/06/22 00:29:02 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_RVISCFLUX

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_RVISCFLUX(
     I        bi,bj,k,
     I        uFld, KappaRU,
     O        rViscFluxU,
     I        myThid)

C !DESCRIPTION:
C Calculates the area integrated vertical viscous fluxes of U:
C \begin{equation*}
C F^r = - \frac{ {\cal A}_w }{\Delta r_c} A_r \delta_k u 
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
C  KappaRU              :: vertical viscosity
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  rViscFluxU           :: viscous fluxes
      _RL rViscFluxU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  Kp1                  :: =k+1 for k<Nr, k=Nr for k>=Nr
      INTEGER I,J,Kp1
CEOP

      Kp1=min(K+1,Nr)

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        rViscFluxU(i,j) =
     &   -KappaRU(i,j,kp1)
     &   *rAw(i,j,bi,bj)
     &   *( uFld(i,j,kp1,bi,bj)-uFld(i,j,k,bi,bj)
     &    )*rkSign*recip_drC(kp1)
     &    *_maskW(i,j,k,bi,bj)
     &    *_maskW(i,j,kp1,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
