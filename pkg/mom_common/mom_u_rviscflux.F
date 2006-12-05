C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_u_rviscflux.F,v 1.4 2006/12/05 05:27:38 jmc Exp $
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
C Calculates the area integrated vertical viscous fluxes of U
C  at vertical interface k (between level k & k-1):
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
      INTEGER i,j
CEOP

C     - Vertical viscous flux
      IF ( k.LE.1 .OR. k.GT.Nr ) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          rViscFluxU(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
          rViscFluxU(i,j) =
     &      -KappaRU(i,j,k)
     &      *rAw(i,j,bi,bj)*deepFac2F(k)*rhoFacF(k)
     &      *( uFld(i,j,k,bi,bj)-uFld(i,j,k-1,bi,bj)
     &       )*rkSign*recip_drC(k)
     &      *_maskW(i,j,k,bi,bj)
     &      *_maskW(i,j,k-1,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
