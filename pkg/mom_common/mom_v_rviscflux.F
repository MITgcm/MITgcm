C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_v_rviscflux.F,v 1.4 2006/12/05 05:27:38 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_RVISCFLUX

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_RVISCFLUX(
     I        bi,bj,k,
     I        vFld, KappaRV,
     O        rViscFluxV,
     I        myThid)

C !DESCRIPTION:
C Calculates the area integrated vertical viscous fluxes of V
C  at vertical interface k (between level k & k-1):
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
      INTEGER i,j
CEOP

C     - Vertical viscous flux
      IF ( k.LE.1 .OR. k.GT.Nr ) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          rViscFluxV(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
          rViscFluxV(i,j) =
     &     -KappaRV(i,j,k)
     &     *rAs(i,j,bi,bj)*deepFac2F(k)*rhoFacF(k)
     &     *( vFld(i,j,k,bi,bj)-vFld(i,j,k-1,bi,bj)
     &      )*rkSign*recip_drC(k)
     &     *_maskS(i,j,k,bi,bj)
     &     *_maskS(i,j,k-1,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
