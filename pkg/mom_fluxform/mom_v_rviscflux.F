C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_v_rviscflux.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_RVISCFLUX

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_RVISCFLUX(
     I        bi,bj,k,
     I        vFld, KappaRV,
     O        rViscFluxV,
     I        myThid)

C !DESCRIPTION:
C Calculates the area integrated vertical viscous fluxes of V:
C \begin{equation*}
C F^r = - \frac{ {\cal A}_s }{\Delta r_c} A_r \delta_k v
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
C  KappaRV              :: vertical viscosity
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  rViscFluxV           :: viscous fluxes
      _RL rViscFluxV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  Kp1                  :: =k+1 for k<Nr, k=Nr for k>=Nr
      INTEGER I,J,Kp1
CEOP

      Kp1=min(K+1,Nr)

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        rViscFluxV(i,j) =
     &   -KappaRV(i,j,kp1)
     &   *rAs(i,j,bi,bj)
     &   *( vFld(i,j,k,bi,bj)-vFld(i,j,kp1,bi,bj)
     &    )*rkFac*recip_drC(kp1)
     &    *_maskS(i,j,kp1,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
